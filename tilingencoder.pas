unit tilingencoder;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}
{$TYPEDADDRESS ON}

{$define ASM_DBMP}


interface

uses
  windows, Classes, SysUtils, strutils, types, Math, FileUtil, typinfo, zstream, IniFiles, Graphics,
  IntfGraphics, FPimage, FPCanvas, FPWritePNG, GraphType, fgl, MTProcs, extern, tbbmalloc, bufstream;

type
  TEncoderStep = (esAll = -1, esLoad = 0, esPreparePalettes, esCluster, esReconstruct, esSmooth, esReindex, esSave);
  TKeyFrameReason = (kfrNone, kfrManual, kfrLength, kfrDecorrelation);
  TRenderPage = (rpNone, rpInput, rpOutput, rpTilesPalette);

const
  // tweakable constants

  cClusterQWeighting = False;
  cReconstructQWeighting = False;
  cSmoothingQWeighting = True;
  cYakmoMaxIterations = 300;
  cDCTFeaturesMul = 1;

{$if false}
  cRedMul = 2126;
  cGreenMul = 7152;
  cBlueMul = 722;
{$else}
  cRedMul = 299;
  cGreenMul = 587;
  cBlueMul = 114;
{$endif}
  cRGBw = 13; // in 1 / 32th

  // don't change these

  cBitsPerComp = 8;
  cLumaDiv = cRedMul + cGreenMul + cBlueMul;
  cVecInvWidth = 16;
  cTileWidthBits = 3;
  cTileWidth = 1 shl cTileWidthBits;
  cColorCpns = 3;
  cTileDCTSize = cColorCpns * sqr(cTileWidth * cDCTFeaturesMul);
  cUnrolledDCTSize = sqr(sqr(cTileWidth * cDCTFeaturesMul));
  cPhi = (1 + sqrt(5)) / 2;
  cInvPhi = 1 / cPhi;

  cDitheringListLen = 256;
  cDitheringMap : array[0..8*8 - 1] of Byte = (
     0, 48, 12, 60,  3, 51, 15, 63,
    32, 16, 44, 28, 35, 19, 47, 31,
     8, 56,  4, 52, 11, 59,  7, 55,
    40, 24, 36, 20, 43, 27, 39, 23,
     2, 50, 14, 62,  1, 49, 13, 61,
    34, 18, 46, 30, 33, 17, 45, 29,
    10, 58,  6, 54,  9, 57,  5, 53,
    42, 26, 38, 22, 41, 25, 37, 21
  );
  cDitheringLen = length(cDitheringMap);

  cEncoderStepLen: array[TEncoderStep] of Integer = (0, 2, -1, 6, -1, -1, 2, 1);

  cQ = 16;
  cDCTQuantization: array[0..cColorCpns-1{YUV}, 0..7, 0..7] of TFloat = (
    (
      // Luma
      (cQ / 16, cQ /  11, cQ /  10, cQ /  16, cQ /  24, cQ /  40, cQ /  51, cQ /  61),
      (cQ / 12, cQ /  12, cQ /  14, cQ /  19, cQ /  26, cQ /  58, cQ /  60, cQ /  55),
      (cQ / 14, cQ /  13, cQ /  16, cQ /  24, cQ /  40, cQ /  57, cQ /  69, cQ /  56),
      (cQ / 14, cQ /  17, cQ /  22, cQ /  29, cQ /  51, cQ /  87, cQ /  80, cQ /  62),
      (cQ / 18, cQ /  22, cQ /  37, cQ /  56, cQ /  68, cQ / 109, cQ / 103, cQ /  77),
      (cQ / 24, cQ /  35, cQ /  55, cQ /  64, cQ /  81, cQ / 104, cQ / 113, cQ /  92),
      (cQ / 49, cQ /  64, cQ /  78, cQ /  87, cQ / 103, cQ / 121, cQ / 120, cQ / 101),
      (cQ / 72, cQ /  92, cQ /  95, cQ /  98, cQ / 112, cQ / 100, cQ / 103, cQ /  99)
    ),
    (
      // U, weighted by luma importance
      (cQ / 17, cQ /  18, cQ /  24, cQ /  47, cQ /  99, cQ /  99, cQ /  99, cQ /  99),
      (cQ / 18, cQ /  21, cQ /  26, cQ /  66, cQ /  99, cQ /  99, cQ /  99, cQ / 112),
      (cQ / 24, cQ /  26, cQ /  56, cQ /  99, cQ /  99, cQ /  99, cQ / 112, cQ / 128),
      (cQ / 47, cQ /  66, cQ /  99, cQ /  99, cQ /  99, cQ / 112, cQ / 128, cQ / 144),
      (cQ / 99, cQ /  99, cQ /  99, cQ /  99, cQ / 112, cQ / 128, cQ / 144, cQ / 160),
      (cQ / 99, cQ /  99, cQ /  99, cQ / 112, cQ / 128, cQ / 144, cQ / 160, cQ / 176),
      (cQ / 99, cQ /  99, cQ / 112, cQ / 128, cQ / 144, cQ / 160, cQ / 176, cQ / 192),
      (cQ / 99, cQ / 112, cQ / 128, cQ / 144, cQ / 160, cQ / 176, cQ / 192, cQ / 208)
    ),
    (
      // V, weighted by luma importance
      (cQ / 17, cQ /  18, cQ /  24, cQ /  47, cQ /  99, cQ /  99, cQ /  99, cQ /  99),
      (cQ / 18, cQ /  21, cQ /  26, cQ /  66, cQ /  99, cQ /  99, cQ /  99, cQ / 112),
      (cQ / 24, cQ /  26, cQ /  56, cQ /  99, cQ /  99, cQ /  99, cQ / 112, cQ / 128),
      (cQ / 47, cQ /  66, cQ /  99, cQ /  99, cQ /  99, cQ / 112, cQ / 128, cQ / 144),
      (cQ / 99, cQ /  99, cQ /  99, cQ /  99, cQ / 112, cQ / 128, cQ / 144, cQ / 160),
      (cQ / 99, cQ /  99, cQ /  99, cQ / 112, cQ / 128, cQ / 144, cQ / 160, cQ / 176),
      (cQ / 99, cQ /  99, cQ / 112, cQ / 128, cQ / 144, cQ / 160, cQ / 176, cQ / 192),
      (cQ / 99, cQ / 112, cQ / 128, cQ / 144, cQ / 160, cQ / 176, cQ / 192, cQ / 208)
    )
  );


  cDCTUVRatio: array[0..7,0..7] of TFloat = (
    (0.5, sqrt(0.5), sqrt(0.5), sqrt(0.5), sqrt(0.5), sqrt(0.5), sqrt(0.5), sqrt(0.5)),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1)
  );

type
  // GliGli's TileMotion header structs and commands

  TGTMHeader = packed record
    FourCC: array[0..3] of AnsiChar; // ASCII "GTMv"
    RIFFSize: Cardinal;
    WholeHeaderSize: Cardinal; // including TGTMKeyFrameInfo and all
    EncoderVersion: Cardinal;
    FramePixelWidth: Cardinal;
    FramePixelHeight: Cardinal;
    KFCount: Cardinal;
    FrameCount: Cardinal;
    AverageBytesPerSec: Cardinal;
    KFMaxBytesPerSec: Cardinal;
  end;

  TGTMKeyFrameInfo = packed record
    FourCC: array[0..3] of AnsiChar; // ASCII "GTMk"
    RIFFSize: Cardinal;
    KFIndex: Cardinal;
    FrameIndex: Cardinal;
    RawSize: Cardinal;
    CompressedSize: Cardinal;
    TimeCodeMillisecond: Cardinal;
  end;

  // Commands Description:
  // =====================
  //
  // SkipBlock:               data -> none; commandBits -> skip count - 1 (10 bits)
  // ShortTileIdx:            data -> tile index (16 bits); commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
  // LongTileIdx:             data -> tile index (32 bits); commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
  // LoadPalette:             data -> palette index (8 bits); palette format (8 bits) (0: RGBA32); RGBA bytes (32bits); commandBits -> none
  // ShortBlendTileIdx:       data -> tile index (16 bits); BlendY offset(4 bits); BlendX offset(4 bits); Previous frame factor (4 bits); Current frame factor (4 bits);
  //                            commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
  // LongLendTileIdx:         data -> tile index (32 bits); BlendY offset(4 bits); BlendX offset(4 bits); Previous frame factor (4 bits); Current frame factor (4 bits);
  //                            commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
  // ShortAddlBlendTileIdx:   data -> tile index (16 bits) * 2; blending ratio (8 bits); padding (8 bits); commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
  // LongAddlBlendTileIdx:    data -> tile index (32 bits) * 2; blending ratio (8 bits); padding (8 bits); commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
  // ShortAdditionalTileIdx:  data -> tile index (16 bits) * 2; commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
  // LongAdditionalTileIdx:   data -> tile index (32 bits) * 2; commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
  //
  // (insert new commands here...)
  //
  // FrameEnd:                data -> none; commandBits bit 0 -> keyframe end
  // TileSet:                 data -> start tile (32 bits); end tile (32 bits); { indexes per tile (64 bytes) } * count; commandBits -> indexes count per palette
  // SetDimensions:           data -> width in tiles (16 bits); height in tiles (16 bits); frame length in nanoseconds (32 bits) (2^32-1: still frame); tile count (32 bits); commandBits -> none
  // ExtendedCommand:         data -> following bytes count (32 bits); custom commands, proprietary extensions, ...; commandBits -> extended command index (10 bits)
  //
  // ReservedArea:            reserving the MSB for future use (do not use for new commands)

  TGTMCommand = (
    gtSkipBlock = 0,
    gtShortTileIdx = 1,
    gtLongTileIdx = 2,
    gtLoadPalette = 3,
    gtShortBlendTileIdx = 4,
    gtLongBlendTileIdx = 5,
    gtShortAddlBlendTileIdx = 6,
    gtLongAddlBlendTileIdx = 7,
    gtPrevFrameBlend = 8,
    gtShortAdditionalTileIdx = 9,
    gtLongAdditionalTileIdx = 10,


    gtFrameEnd = 28,
    gtTileSet = 29,
    gtSetDimensions = 30,
    gtExtendedCommand = 31,

    gtReservedAreaBegin = 32,
    gtReservedAreaEnd = 63
  );

  TSpinlock = LongInt;
  PSpinLock = ^TSpinlock;

  PIntegerDynArray = ^TIntegerDynArray;
  PBoolean = ^Boolean;
  PPBoolean = ^PBoolean;

  TIntegerList=specialize TFPGList<Integer>;
  TFloatFloatFunction = function(x: TFloat; Data: Pointer): TFloat;

  PTile = ^TTile;
  PPTile = ^PTile;

  PTileDynArray = array of PTile;
  PTileDynArray2 = array of PTileDynArray;

  TRGBPixels = array[0..(cTileWidth - 1),0..(cTileWidth - 1)] of Integer;
  TPalPixels = array[0..(cTileWidth - 1),0..(cTileWidth - 1)] of Byte;
  TCpnPixels = array[0..cColorCpns-1, 0..cTileWidth-1,0..cTileWidth-1] of TFloat;
  TCpnPixelsDouble = array[0..cColorCpns-1, 0..cTileWidth-1,0..cTileWidth-1] of Double;
  PRGBPixels = ^TRGBPixels;
  PPalPixels = ^TPalPixels;
  PCpnPixels = ^TCpnPixels;

  ETilingEncoderGTMReloadError = class(Exception);

  { TTile }

  TTile = packed record // /!\ update TTileHelper.CopyFrom each time this structure is changed /!\
    UseCount: Cardinal;
    TmpIndex: Integer;
    Flags: set of (tfActive, tfHasRGBPixels, tfHasPalPixels, tfHMirror_Initial, tfVMirror_Initial);
  end;

  { TTileHelper }

  TTileHelper = record helper for TTile
  private
    function GetActive: Boolean;
    function GetHasPalPixels: Boolean;
    function GetHasRGBPixels: Boolean;
    function GetHMirror_Initial: Boolean;
    function GetVMirror_Initial: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetHasPalPixels(AValue: Boolean);
    procedure SetHasRGBPixels(AValue: Boolean);
    procedure SetHMirror_Initial(AValue: Boolean);
    procedure SetVMirror_Initial(AValue: Boolean);
  public
    function GetRGBPixelsPtr: PRGBPixels;
    function GetPalPixelsPtr: PPalPixels;

    function GetRGBPixels(y, x: Integer): Integer;
    function GetPalPixels(y, x: Integer): Byte;
    procedure SetRGBPixels(y, x: Integer; value: Integer);
    procedure SetPalPixels(y, x: Integer; value: Byte);

    class function Array1DNew(x: Integer; ARGBPixels, APalPixels: Boolean): PTileDynArray; static;
    class procedure Array1DDispose(var AArray: PTileDynArray); static;
    class procedure Array1DRealloc(var AArray: PTileDynArray; ANewX: integer); static;
    class function New(ARGBPixels, APalPixels: Boolean): PTile; static;
    class procedure Dispose(var ATile: PTile); static;
    procedure CopyFrom(const ATile: TTile);
    procedure CopyPalPixelsFrom(const ATile: TTile);
    procedure CopyPalPixels(const APalPixels: TPalPixels); overload;
    procedure CopyPalPixels(const APalPixels: TByteDynArray); overload;
    procedure CopyRGBPixels(const ARGBPixels: TRGBPixels);
    procedure ClearPalPixels;
    procedure ClearRGBPixels;
    procedure ClearPixels;
    procedure ExtractPalPixels(AArray: PFloat);
    procedure LoadPalPixels(AArray: PFloat);
    function ComparePalPixelsTo(const ATile: TTile): Integer;
    function CompareRGBPixelsTo(const ATile: TTile): Integer;

    property RGBPixels[y, x: Integer]: Integer read GetRGBPixels write SetRGBPixels;
    property PalPixels[y, x: Integer]: Byte read GetPalPixels write SetPalPixels;

    property Active: Boolean read GetActive write SetActive;
    property HasRGBPixels: Boolean read GetHasRGBPixels write SetHasRGBPixels;
    property HasPalPixels: Boolean read GetHasPalPixels write SetHasPalPixels;
    property HMirror_Initial: Boolean read GetHMirror_Initial write SetHMirror_Initial;
    property VMirror_Initial: Boolean read GetVMirror_Initial write SetVMirror_Initial;
  end;

  { TTileMapItem }

  TTileMapItem = packed record
    TileIdx: Integer;
    ResidualErr: Single;
    PalIdx: SmallInt;
    Flags: set of (tmfHMirror, tmfVMirror, tmfSmoothed);
  end;

  PTileMapItem = ^TTileMapItem;

  TTileMapItems = array of TTileMapItem;

  { TTileMapItemHelper }

  TTileMapItemHelper = record helper for TTileMapItem
  private
    function GetHMirror: Boolean;
    function GetVMirror: Boolean;
    procedure SetHMirror(AValue: Boolean);
    procedure SetVMirror(AValue: Boolean);
    function GetSmoothed: Boolean;
    procedure SetSmoothed(AValue: Boolean);
  public
    property HMirror: Boolean read GetHMirror write SetHMirror;
    property VMirror: Boolean read GetVMirror write SetVMirror;
    property Smoothed: Boolean read GetSmoothed write SetSmoothed;
  end;

  { TTilingDataset }

  TTilingDataset = record
    KNNSize: Integer;
    Dataset: TSingleDynArray2;
    ANN: PANNkdtree;
  end;

  PTilingDataset = ^TTilingDataset;

  { TCountIndex }

  TCountIndex = record
    Index, Count: Integer;
    R, G, B: Byte;
    Hue, Sat, Val: Byte;
  end;

  PCountIndex = ^TCountIndex;
  TCountIndexList = specialize TFPGList<PCountIndex>;

  { TMixingPlan }

  TMixingPlan = record
    // static
    LumaPal: array of Integer;
    Y2Palette: array of array[0..3] of Integer;
    Y2MixedColors: Integer;
  end;

  { TKFTilingBest }

  TKFTilingBest = record
    ResidualErr: TFloat;
    TileIdx: Integer;
    BlendedX, BlendedY: ShortInt;
    BlendPrev, BlendOffset: Byte;
  end;

  TTilingEncoder = class;
  TKeyFrame = class;

  { TFrame }

  TFrame = class
    Encoder: TTilingEncoder;
    PKeyFrame: TKeyFrame;
    Index: Integer;

    FrameTiles: array of PTile;
    TileMap: array of array of TTileMapItem;

    PaletteRGB: TIntegerDynArray2;
    PaletteInfo: array of record
      UseCount: Integer;
      MixingPlan, BWMixingPlan: TMixingPlan;
      PalIdx_Initial: Integer;
    end;

    TileDS: array of PTilingDataset;
    ReconstructErrCml: Double;

    procedure LogResidualErr;

    // algorithms

    procedure DoPalettization(ADitheringGamma: Integer);
    procedure DoQuantization(APalIdx: Integer; UseYakmo: Boolean; DLv3BPC: Integer; ADitheringGamma: Integer);
    procedure OptimizePalettes;

    procedure PrepareTiling(APaletteIndex, AFTGamma: Integer);
    procedure FinishTiling(APaletteIndex: Integer);
    procedure DoTiling(APaletteIndex: Integer; AFTGamma: Integer);

    constructor Create(AParent: TTilingEncoder; AIndex: Integer);

    // processes

    procedure PreparePalettes;
    procedure Reconstruct;
  end;

  TFrameArray =  array of TFrame;

  { TKeyFrame }

  TKeyFrame = class
    Encoder: TTilingEncoder;
    Index, StartFrame, EndFrame, FrameCount: Integer;
    FramesLeft: Integer;
    Reason: TKeyFrameReason;

    FrameTilesRefCount: Integer;
    FrameTilesEvent: THandle;

    constructor Create(AParent: TTilingEncoder; AIndex, AStartFrame, AEndFrame: Integer);
    destructor Destroy; override;

    procedure AcquireFrameTiles;
    procedure ReleaseFrameTiles;

    // algorithms

    procedure DoTemporalSmoothing(AFrame, APrevFrame: TFrame);

    // processes

    procedure Smooth;
  end;

  TKeyFrameArray =  array of TKeyFrame;

  TTilingEncoderProgressEvent = procedure(ASender: TTilingEncoder; APosition, AMax: Integer; AHourGlass: Boolean) of object;

  { TTilingEncoder }

  TTilingEncoder = class
  private
    // encoder state variables

    FCS: TRTLCriticalSection;
    FFramesLeft: Integer;

    FGamma: array[0..1] of TFloat;
    FGammaCorLut: array[-1..1, 0..High(Byte)] of TFloat;
    FVecInv: array[0..256 * 4 - 1] of Cardinal;
    FDCTLut:array[0..cUnrolledDCTSize - 1] of TFloat;
    FDCTLutDouble:array[0..cUnrolledDCTSize - 1] of Double;
    FInvDCTLutDouble:array[0..cUnrolledDCTSize - 1] of Double;

    FTiles: PTileDynArray;
    FKeyFrames: TKeyFrameArray;
    FFrames: TFrameArray;

    // video properties

    FLoadedInputPath: String;
    FTileMapWidth: Integer;
    FTileMapHeight: Integer;
    FTileMapSize: Integer;
    FScreenWidth: Integer;
    FScreenHeight: Integer;
    FFramesPerSecond: Double;

    // settings

    FInputFileName: String;
    FOutputFileName: String;
    FStartFrame: Integer;
    FFrameCountSetting: Integer;
    FScaling: Double;
    FEncoderGammaValue: Double;
    FPaletteSize: Integer;
    FPaletteCount: Integer;
    FQuantizerUseYakmo: Boolean;
    FQuantizerDennisLeeBitsPerComponent: Integer;
    FQuantizerPosterize: Boolean;
    FQuantizerPosterizeBitsPerComponent: Integer;
    FDitheringUseGamma: Boolean;
    FDitheringUseThomasKnoll: Boolean;
    FDitheringYliluoma2MixedColors: Integer;
    FGlobalTilingUseGamma: Boolean;
    FGlobalTilingTileCount: Integer;
    FGlobalTilingQualityBasedTileCount: Double;
    FGlobalTilingLumaOnly: Boolean;
    FGlobalTilingRatio: Double;
    FFrameTilingFromPalette: Boolean;
    FFrameTilingUseGamma: Boolean;
    FSmoothingFactor: Double;
    FShotTransMaxSecondsPerKF: Double;
    FShotTransMinSecondsPerKF: Double;
    FShotTransCorrelLoThres: Double;

    // GUI state variables

    FRenderPrevKeyFrame: TKeyFrame;
    FRenderFrameIndex: Integer;
    FRenderGammaValue: Double;
    FRenderPage: TRenderPage;
    FRenderPsychoVisualQuality: Double;
    FRenderTitleText: String;
    FRenderUseGamma: Boolean;
    FRenderMirrored: Boolean;
    FRenderPaletteIndex: Integer;
    FRenderPlaying: Boolean;
    FRenderSmoothed: Boolean;
    FRenderDithered: Boolean;
    FRenderTilePage: Integer;
    FOutputBitmap: TBitmap;
    FInputBitmap: TBitmap;
    FPaletteBitmap: TBitmap;
    FTilesBitmap: TBitmap;
    FOnProgress: TTilingEncoderProgressEvent;
    FProgressStep: TEncoderStep;
    FProgressAllStartTime, FProgressProcessStartTime, FProgressPrevTime: Int64;

    FProgressSyncPos, FProgressSyncMax: Integer;
    FProgressSyncHG: Boolean;

    function GetFrameCount: Integer;
    function GetKeyFrameCount: Integer;
    function GetMaxThreadCount: Integer;
    function GetTiles: PTileDynArray;
    procedure SetDitheringYliluoma2MixedColors(AValue: Integer);
    procedure SetEncoderGammaValue(AValue: Double);
    procedure SetFrameCountSetting(AValue: Integer);
    procedure SetFramesPerSecond(AValue: Double);
    procedure SetGlobalTilingQualityBasedTileCount(AValue: Double);
    procedure SetGlobalTilingRatio(AValue: Double);
    procedure SetMaxThreadCount(AValue: Integer);
    procedure SetPaletteCount(AValue: Integer);
    procedure SetPaletteSize(AValue: Integer);
    procedure SetQuantizerDennisLeeBitsPerComponent(AValue: Integer);
    procedure SetQuantizerPosterizeBitsPerComponent(AValue: Integer);
    procedure SetRenderFrameIndex(AValue: Integer);
    procedure SetRenderGammaValue(AValue: Double);
    procedure SetRenderPaletteIndex(AValue: Integer);
    procedure SetRenderTilePage(AValue: Integer);
    procedure SetGlobalTilingTileCount(AValue: Integer);
    procedure SetScaling(AValue: Double);
    procedure SetShotTransCorrelLoThres(AValue: Double);
    procedure SetShotTransMaxSecondsPerKF(AValue: Double);
    procedure SetShotTransMinSecondsPerKF(AValue: Double);
    procedure SetSmoothingFactor(AValue: Double);
    procedure SetStartFrame(AValue: Integer);

    function PearsonCorrelation(const x: TFloatDynArray; const y: TFloatDynArray): TFloat;
    function PrepareInterFrameCorrelation(AFrame: TFrame): TFloatDynArray;

    function GetSettings: String;
    procedure ProgressRedraw(ASubStepIdx: Integer; AReason: String; AProgressStep: TEncoderStep = esAll; AThread: TThread = nil);
    procedure SyncProgress;

    function GammaCorrect(lut: Integer; x: Byte): TFloat; inline;
    function GammaUncorrect(lut: Integer; x: TFloat): Byte; inline;

    function HSVToRGB(h, s, v: Byte): Integer;
    procedure RGBToHSV(col: Integer; out h, s, v: Byte); overload;
    procedure RGBToHSV(col: Integer; out h, s, v: TFloat); overload;
    procedure RGBToYUV(col: Integer; GammaCor: Integer; out y, u, v: TFloat);
    procedure RGBToYUV(r, g, b: Byte; GammaCor: Integer; out y, u, v: TFloat);
    procedure RGBToLAB(r, g, b: TFloat; GammaCor: Integer; out ol, oa, ob: TFloat);
    procedure RGBToLAB(ir, ig, ib: Integer; GammaCor: Integer; out ol, oa, ob: TFloat);
    function LABToRGB(ll, aa, bb: TFloat; GammaCor: Integer): Integer;
    function YUVToRGB(y, u, v: TFloat; GammaCor: Integer): Integer;

    procedure ComputeTilePsyVisFeatures(const ATile: TTile; FromPal, UseLAB, QWeighting, HMirror, VMirror: Boolean;
     ColorCpns, GammaCor: Integer; const pal: TIntegerDynArray; DCT: PFloat); inline; overload;
    procedure ComputeTilePsyVisFeatures(const ATile: TTile; FromPal, UseLAB, QWeighting, HMirror, VMirror: Boolean;
     ColorCpns, GammaCor: Integer; const pal: TIntegerDynArray; DCT: PDouble); inline; overload;
    procedure ComputeInvTilePsyVisFeatures(DCT: PDouble; UseLAB, QWeighting: Boolean; ColorCpns, GammaCor: Integer;
      var ATile: TTile);

    // Dithering algorithms ported from http://bisqwit.iki.fi/story/howto/dither/jy/

    class function ColorCompare(r1, g1, b1, r2, g2, b2: Int64): Int64;
    procedure PreparePlan(var Plan: TMixingPlan; MixedColors: Integer; const pal: array of Integer);
    procedure TerminatePlan(var Plan: TMixingPlan);
    function DeviseBestMixingPlanYliluoma(var Plan: TMixingPlan; col: Integer; var List: array of Byte): Integer;
    procedure DeviseBestMixingPlanThomasKnoll(var Plan: TMixingPlan; col: Integer; var List: array of Byte);

    function GetTileCount(AActiveOnly: Boolean): Integer;
    function GetFrameTileCount(AFrame: TFrame): Integer;
    procedure DitherTile(var ATile: TTile; var Plan: TMixingPlan);
    class function GetTileZoneSum(const ATile: TTile; x, y, w, h: Integer): Integer;
    class procedure GetTileHVMirrorHeuristics(const ATile: TTile; out AHMirror, AVMirror: Boolean);
    class procedure HMirrorTile(var ATile: TTile; APalOnly: Boolean = False);
    class procedure VMirrorTile(var ATile: TTile; APalOnly: Boolean = False);

    procedure InitLuts;
    procedure ClearAll;
    procedure ReframeUI(AWidth, AHeight: Integer);
    procedure InitFrames(AFrameCount: Integer);
    procedure LoadFrameTilesFromImage(var AFrame: TFrame; AImageWidth, AImageHeight: Integer; AImage: PInteger);
    procedure FindKeyFrames(AManualMode: Boolean);

    procedure OptimizeGlobalPalettes;
    procedure DoGlobalKMeans(AClusterCount, AGamma, AColorCpns: Integer; ABIRCHRatio, ABICORatio: Double);

    procedure ReindexTiles(KeepRGBPixels: Boolean);

    procedure LoadStream(AStream: TStream);
    procedure SaveStream(AStream: TStream);

    // processes

    procedure Load;
    procedure PreparePalettes;
    procedure Cluster;
    procedure Reconstruct;
    procedure Smooth;
    procedure Reindex;
    procedure Save;
  public
    // constructor / destructor

    constructor Create;
    destructor Destroy; override;

    // functions

    procedure Run(AStep: TEncoderStep = esAll);

    procedure Render(AFast: Boolean);
    procedure GeneratePNGs;
    procedure GenerateY4M(AFileName: String);
    procedure SaveSettings(ASettingsFileName: String);
    procedure LoadSettings(ASettingsFileName: String);
    procedure LoadDefaultSettings;

    procedure ReloadGTM(AFileName: String);

    procedure Test;

    // encoder state variables

    property Tiles: PTileDynArray read GetTiles;
    property KeyFrames: TKeyFrameArray read FKeyFrames;
    property Frames: TFrameArray read FFrames;

    // video properties

    property ScreenWidth: Integer read FScreenWidth;
    property ScreenHeight: Integer read FScreenHeight;
    property FramesPerSecond: Double read FFramesPerSecond write SetFramesPerSecond;
    property TileMapWidth: Integer read FTileMapWidth;
    property TileMapHeight: Integer read FTileMapHeight;
    property TileMapSize: Integer read FTileMapSize;
    property FrameCount: Integer read GetFrameCount;
    property KeyFrameCount: Integer read GetKeyFrameCount;

    // settings

    property InputFileName: String read FInputFileName write FInputFileName;
    property OutputFileName: String read FOutputFileName write FOutputFileName;
    property StartFrame: Integer read FStartFrame write SetStartFrame;
    property FrameCountSetting: Integer read FFrameCountSetting write SetFrameCountSetting;
    property Scaling: Double read FScaling write SetScaling;
    property EncoderGammaValue: Double read FEncoderGammaValue write SetEncoderGammaValue;
    property PaletteSize: Integer read FPaletteSize write SetPaletteSize;
    property PaletteCount: Integer read FPaletteCount write SetPaletteCount;
    property QuantizerUseYakmo: Boolean read FQuantizerUseYakmo write FQuantizerUseYakmo;
    property QuantizerDennisLeeBitsPerComponent: Integer read FQuantizerDennisLeeBitsPerComponent write SetQuantizerDennisLeeBitsPerComponent;
    property QuantizerPosterizeBitsPerComponent: Integer read FQuantizerPosterizeBitsPerComponent write SetQuantizerPosterizeBitsPerComponent;
    property QuantizerPosterize: Boolean read FQuantizerPosterize write FQuantizerPosterize;
    property DitheringUseGamma: Boolean read FDitheringUseGamma write FDitheringUseGamma;
    property DitheringUseThomasKnoll: Boolean read FDitheringUseThomasKnoll write FDitheringUseThomasKnoll;
    property DitheringYliluoma2MixedColors: Integer read FDitheringYliluoma2MixedColors write SetDitheringYliluoma2MixedColors;
    property GlobalTilingUseGamma: Boolean read FGlobalTilingUseGamma write FGlobalTilingUseGamma;
    property GlobalTilingTileCount: Integer read FGlobalTilingTileCount write SetGlobalTilingTileCount;
    property GlobalTilingQualityBasedTileCount: Double read FGlobalTilingQualityBasedTileCount write SetGlobalTilingQualityBasedTileCount;
    property GlobalTilingLumaOnly: Boolean read FGlobalTilingLumaOnly write FGlobalTilingLumaOnly;
    property GlobalTilingRatio: Double read FGlobalTilingRatio write SetGlobalTilingRatio;
    property FrameTilingFromPalette: Boolean read FFrameTilingFromPalette write FFrameTilingFromPalette;
    property FrameTilingUseGamma: Boolean read FFrameTilingUseGamma write FFrameTilingUseGamma;
    property SmoothingFactor: Double read FSmoothingFactor write SetSmoothingFactor;
    property MaxThreadCount: Integer read GetMaxThreadCount write SetMaxThreadCount;
    property ShotTransMaxSecondsPerKF: Double read FShotTransMaxSecondsPerKF write SetShotTransMaxSecondsPerKF;
    property ShotTransMinSecondsPerKF: Double read FShotTransMinSecondsPerKF write SetShotTransMinSecondsPerKF;
    property ShotTransCorrelLoThres: Double read FShotTransCorrelLoThres write SetShotTransCorrelLoThres;


    // GUI state variables

    property RenderPlaying: Boolean read FRenderPlaying write FRenderPlaying;
    property RenderFrameIndex: Integer read FRenderFrameIndex write SetRenderFrameIndex;
    property RenderMirrored: Boolean read FRenderMirrored write FRenderMirrored;
    property RenderSmoothed: Boolean read FRenderSmoothed write FRenderSmoothed;
    property RenderDithered: Boolean read FRenderDithered write FRenderDithered;
    property RenderUseGamma: Boolean read FRenderUseGamma write FRenderUseGamma;
    property RenderPaletteIndex: Integer read FRenderPaletteIndex write SetRenderPaletteIndex;
    property RenderTilePage: Integer read FRenderTilePage write SetRenderTilePage;
    property RenderGammaValue: Double read FRenderGammaValue write SetRenderGammaValue;
    property RenderPage: TRenderPage read FRenderPage write FRenderPage;
    property RenderTitleText: String read FRenderTitleText;
    property RenderPsychoVisualQuality: Double read FRenderPsychoVisualQuality;
    property OutputBitmap: TBitmap read FOutputBitmap;
    property InputBitmap: TBitmap read FInputBitmap;
    property PaletteBitmap: TBitmap read FPaletteBitmap;
    property TilesBitmap: TBitmap read FTilesBitmap;
    property ProgressStep: TEncoderStep read FProgressStep;
    property OnProgress: TTilingEncoderProgressEvent read FOnProgress write FOnProgress;
  end;

  { TFastPortableNetworkGraphic }

  TFastPortableNetworkGraphic = class(TPortableNetworkGraphic)
    procedure InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter); override;
  end;


  function iDiv0(x, y: Integer): Integer;overload;inline;
  function iDiv0(x, y: Int64): Int64;overload;inline;

implementation

const
  CGTMCommandsCount = Ord(High(TGTMCommand)) + 1;
  CGTMCommandCodeBits = round(ln(CGTMCommandsCount) / ln(2));
  CGTMCommandBits = 16 - CGTMCommandCodeBits;

procedure SpinEnter(Lock: PSpinLock); assembler;
label spin_lock;
asm
spin_lock:
     mov     eax, 1          // Set the EAX register to 1.

     xchg    eax, [Lock]     // Atomically swap the EAX register with the lock variable.
                             // This will always store 1 to the lock, leaving the previous value in the EAX register.

     test    eax, eax        // Test EAX with itself. Among other things, this will set the processor's Zero Flag if EAX is 0.
                             // If EAX is 0, then the lock was unlocked and we just locked it.
                             // Otherwise, EAX is 1 and we didn't acquire the lock.

     jnz     spin_lock       // Jump back to the MOV instruction if the Zero Flag is not set;
                             // the lock was previously locked, and so we need to spin until it becomes unlocked.
end;

procedure SpinLeave(Lock: PSpinLock); assembler;
asm
    xor     eax, eax        // Set the EAX register to 0.

    xchg    eax, [Lock]     // Atomically swap the EAX register with the lock variable.
end;

procedure Exchange(var a, b: Integer);
var
  tmp: Integer;
begin
  tmp := b;
  b := a;
  a := tmp;
end;

function iDiv0(x, y: Integer): Integer;overload;inline;
begin
  Result := 0;
  if y <> 0 then
    Result := x div y;
end;

function iDiv0(x, y: Int64): Int64;overload;inline;
begin
  Result := 0;
  if y <> 0 then
    Result := x div y;
end;

function Div0(x, y: TFloat): TFloat;inline;
begin
  Result := 0;
  if y <> 0 then
    Result := x / y;
end;

procedure DivMod(Dividend: Int64; Divisor: LongInt; var Result, Remainder: LongInt);
begin
  Result := Dividend div Divisor;
  Remainder := Dividend mod Divisor;
end;

function NanDef(x, def: TFloat): TFloat; inline;
begin
  Result := x;
  if IsNan(Result) then
    Result := def;
end;

function SwapRB(c: Integer): Integer; inline;
begin
  Result := ((c and $ff) shl 16) or ((c shr 16) and $ff) or (c and $ff00);
end;

function ToRGB(r, g, b: Byte): Integer; inline;
begin
  Result := (b shl 16) or (g shl 8) or r;
end;

procedure FromRGB(col: Integer; out r, g, b: Integer); inline; overload;
begin
  r := col and $ff;
  g := (col shr 8) and $ff;
  b := (col shr 16) and $ff;
end;

procedure FromRGB(col: Integer; out r, g, b: Byte); inline; overload;
begin
  r := col and $ff;
  g := (col shr 8) and $ff;
  b := (col shr 16) and $ff;
end;

function ToLuma(r, g, b: Byte): Integer; inline;
begin
  Result := r * cRedMul + g * cGreenMul + b * cBlueMul;
end;

function ToBW(col: Integer): Integer;
var
  r, g, b: Byte;
begin
  FromRGB(col, r, g, b);
  Result := ToLuma(r, g, b);
  Result := Result div cLumaDiv;
  Result := ToRGB(Result, Result, Result);
end;

function CompareIntegers(const Item1, Item2: Integer): Integer;
begin
  Result := CompareValue(Item1, Item2);
end;

function lerp(x, y, alpha: TFloat): TFloat; inline;
begin
  Result := x + (y - x) * alpha;
end;

function ilerp(x, y, alpha, maxAlpha: Integer): Integer; inline;
begin
  Result := x + ((y - x) * alpha) div maxAlpha;
end;

function revlerp(x, r, alpha: TFloat): TFloat; inline;
begin
  Result := x + (r - x) / alpha;
end;

function Posterize(v, bpc: Byte): Byte; inline;
var
  cvt: Integer;
begin
  cvt := (1 shl bpc) - 1;

  Result := min(255, Round(Round((v * cvt) / 255.0) * 255.0 / cvt));
end;

function CompareEuclideanDCTPtr(pa, pb: PFloat): TFloat; overload;
var
  i: Integer;
begin
  Result := 0;
  for i := cTileDCTSize div 8 - 1 downto 0 do
  begin
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
  end;
end;

function CompareEuclideanDCTPtr_asm(pa_rcx, pb_rdx: PFloat): TFloat; register; assembler;
label loop;
asm
  push rax
  push rcx
  push rdx

  sub rsp, 16 * 12
  movdqu oword ptr [rsp],       xmm1
  movdqu oword ptr [rsp + $10], xmm2
  movdqu oword ptr [rsp + $20], xmm3
  movdqu oword ptr [rsp + $30], xmm4
  movdqu oword ptr [rsp + $40], xmm5
  movdqu oword ptr [rsp + $50], xmm6
  movdqu oword ptr [rsp + $60], xmm7
  movdqu oword ptr [rsp + $70], xmm8
  movdqu oword ptr [rsp + $80], xmm9
  movdqu oword ptr [rsp + $90], xmm10
  movdqu oword ptr [rsp + $a0], xmm11
  movdqu oword ptr [rsp + $b0], xmm12

  // unrolled for 48 = (cTileDCTSize / 4)

  pxor xmm0, xmm0
  mov al, (cTileDCTSize / 48)
  loop:

    // step 1

    movups xmm2,  oword ptr [rcx]
    movups xmm4,  oword ptr [rcx + $10]
    movups xmm6,  oword ptr [rcx + $20]
    movups xmm8,  oword ptr [rcx + $30]
    movups xmm10, oword ptr [rcx + $40]
    movups xmm12, oword ptr [rcx + $50]

    movups xmm1,  oword ptr [rdx]
    movups xmm3,  oword ptr [rdx + $10]
    movups xmm5,  oword ptr [rdx + $20]
    movups xmm7,  oword ptr [rdx + $30]
    movups xmm9,  oword ptr [rdx + $40]
    movups xmm11, oword ptr [rdx + $50]

    subps xmm1,  xmm2
    subps xmm3,  xmm4
    subps xmm5,  xmm6
    subps xmm7,  xmm8
    subps xmm9,  xmm10
    subps xmm11, xmm12

    mulps xmm1,  xmm1
    mulps xmm3,  xmm3
    mulps xmm5,  xmm5
    mulps xmm7,  xmm7
    mulps xmm9,  xmm9
    mulps xmm11, xmm11

    addps xmm1, xmm7
    addps xmm3, xmm9
    addps xmm5, xmm11

    addps xmm1, xmm3
    addps xmm0, xmm5
    addps xmm0, xmm1

    // step 2

    movups xmm2,  oword ptr [rcx + $60]
    movups xmm4,  oword ptr [rcx + $70]
    movups xmm6,  oword ptr [rcx + $80]
    movups xmm8,  oword ptr [rcx + $90]
    movups xmm10, oword ptr [rcx + $a0]
    movups xmm12, oword ptr [rcx + $b0]

    movups xmm1,  oword ptr [rdx + $60]
    movups xmm3,  oword ptr [rdx + $70]
    movups xmm5,  oword ptr [rdx + $80]
    movups xmm7,  oword ptr [rdx + $90]
    movups xmm9,  oword ptr [rdx + $a0]
    movups xmm11, oword ptr [rdx + $b0]

    subps xmm1,  xmm2
    subps xmm3,  xmm4
    subps xmm5,  xmm6
    subps xmm7,  xmm8
    subps xmm9,  xmm10
    subps xmm11, xmm12

    mulps xmm1,  xmm1
    mulps xmm3,  xmm3
    mulps xmm5,  xmm5
    mulps xmm7,  xmm7
    mulps xmm9,  xmm9
    mulps xmm11, xmm11

    addps xmm1, xmm7
    addps xmm3, xmm9
    addps xmm5, xmm11

    addps xmm1, xmm3
    addps xmm0, xmm5
    addps xmm0, xmm1

    // loop

    lea rcx, [rcx + $c0]
    lea rdx, [rdx + $c0]
    lea r8, [r8 + $c0]

    dec al
    jnz loop

  // end

  movdqu xmm1,  oword ptr [rsp]
  movdqu xmm2,  oword ptr [rsp + $10]
  movdqu xmm3,  oword ptr [rsp + $20]
  movdqu xmm4,  oword ptr [rsp + $30]
  movdqu xmm5,  oword ptr [rsp + $40]
  movdqu xmm6,  oword ptr [rsp + $50]
  movdqu xmm7,  oword ptr [rsp + $60]
  movdqu xmm8,  oword ptr [rsp + $70]
  movdqu xmm9,  oword ptr [rsp + $80]
  movdqu xmm10, oword ptr [rsp + $90]
  movdqu xmm11, oword ptr [rsp + $a0]
  movdqu xmm12, oword ptr [rsp + $b0]
  add rsp, 16 * 12

  haddps xmm0, xmm0
  haddps xmm0, xmm0

  pop rdx
  pop rcx
  pop rax
end;

function CompareEuclideanDCT(const a, b: TFloatDynArray): TFloat; inline; overload;
begin
  Result := CompareEuclideanDCTPtr_asm(@a[0], @b[0]);
end;

function CompareEuclidean(const a, b: TFloatDynArray): TFloat; inline;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(a) do
    Result += sqr(a[i] - b[i]);
end;

function CompareEuclidean(a, b: PDouble; size: Integer): Double; inline;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to size - 1 do
    Result += sqr(a[i] - b[i]);
end;

function CompareTilePixels(Item1, Item2:Pointer):Integer;
var
  t1, t2: PTile;
begin
  t1 := PTile(Item1);
  t2 := PTile(Item2);
  Result := t1^.CompareRGBPixelsTo(t2^);
end;

function CompareCMULHS(const Item1,Item2:PCountIndex):Integer;
begin
  Result := CompareValue(Item1^.Val, Item2^.Val);
  if Result = 0 then
    Result := CompareValue(Item1^.Sat, Item2^.Sat);
  if Result = 0 then
    Result := CompareValue(Item1^.Hue, Item2^.Hue);
end;

function ComparePaletteUseCount(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PInteger(Item2)^, PInteger(Item1)^);
end;

function CompareTileUseCountRev(Item1, Item2, UserParameter:Pointer):Integer;
var
  t1, t2: PTile;
begin
  t1 := PPTile(Item1)^;
  t2 := PPTile(Item2)^;

  Result := CompareValue(t2^.UseCount, t1^.UseCount);
  if Result = 0 then
    Result := t1^.ComparePalPixelsTo(t2^);
end;

procedure ComputeBlending(PRes, Px, Py: PFloat; bx, by: TFloat); inline;
var
  i: Integer;
begin
  for i := 0 to cTileDCTSize div 8 - 1 do // unroll by 8
  begin
    PRes^ := Px^ * bx + Py^ * by; Inc(PRes); Inc(Px); Inc(Py);
    PRes^ := Px^ * bx + Py^ * by; Inc(PRes); Inc(Px); Inc(Py);
    PRes^ := Px^ * bx + Py^ * by; Inc(PRes); Inc(Px); Inc(Py);
    PRes^ := Px^ * bx + Py^ * by; Inc(PRes); Inc(Px); Inc(Py);
    PRes^ := Px^ * bx + Py^ * by; Inc(PRes); Inc(Px); Inc(Py);
    PRes^ := Px^ * bx + Py^ * by; Inc(PRes); Inc(Px); Inc(Py);
    PRes^ := Px^ * bx + Py^ * by; Inc(PRes); Inc(Px); Inc(Py);
    PRes^ := Px^ * bx + Py^ * by; Inc(PRes); Inc(Px); Inc(Py);
  end;
end;

procedure ComputeBlending_Asm(PRes_rcx, Px_rdx, Py_r8: PFloat; bx_xmm3, by_stack: TFloat); register; assembler;
label loop;
asm
  push rax
  push rcx
  push rdx
  push r8

  sub rsp, 16 * 10
  movdqu oword ptr [rsp],       xmm1
  movdqu oword ptr [rsp + $10], xmm2
  movdqu oword ptr [rsp + $20], xmm3
  movdqu oword ptr [rsp + $30], xmm4
  movdqu oword ptr [rsp + $40], xmm5
  movdqu oword ptr [rsp + $50], xmm6
  movdqu oword ptr [rsp + $60], xmm7
  movdqu oword ptr [rsp + $70], xmm8
  movdqu oword ptr [rsp + $80], xmm9
  movdqu oword ptr [rsp + $90], xmm10

  pshufd xmm1, xmm3, 0
  pshufd xmm2, dword ptr [by_stack], 0

  // unrolled for 48 = (cTileDCTSize / 4)

  mov al, (cTileDCTSize / 48)
  loop:

    // step 1

    movups xmm4,  oword ptr [r8]
    movups xmm6,  oword ptr [r8 + $10]
    movups xmm8,  oword ptr [r8 + $20]
    movups xmm10, oword ptr [r8 + $30]

    movups xmm3,  oword ptr [rdx]
    movups xmm5,  oword ptr [rdx + $10]
    movups xmm7,  oword ptr [rdx + $20]
    movups xmm9,  oword ptr [rdx + $30]

    mulps xmm4,  xmm2
    mulps xmm6,  xmm2
    mulps xmm8,  xmm2
    mulps xmm10, xmm2

    mulps xmm3,  xmm1
    mulps xmm5,  xmm1
    mulps xmm7,  xmm1
    mulps xmm9,  xmm1

    addps xmm3,  xmm4
    addps xmm5,  xmm6
    addps xmm7,  xmm8
    addps xmm9,  xmm10

    movups oword ptr [rcx],       xmm3
    movups oword ptr [rcx + $10], xmm5
    movups oword ptr [rcx + $20], xmm7
    movups oword ptr [rcx + $30], xmm9

    // step 2

    movups xmm4,  oword ptr [r8 + $40]
    movups xmm6,  oword ptr [r8 + $50]
    movups xmm8,  oword ptr [r8 + $60]
    movups xmm10, oword ptr [r8 + $70]

    movups xmm3,  oword ptr [rdx + $40]
    movups xmm5,  oword ptr [rdx + $50]
    movups xmm7,  oword ptr [rdx + $60]
    movups xmm9,  oword ptr [rdx + $70]

    mulps xmm4,  xmm2
    mulps xmm6,  xmm2
    mulps xmm8,  xmm2
    mulps xmm10, xmm2

    mulps xmm3,  xmm1
    mulps xmm5,  xmm1
    mulps xmm7,  xmm1
    mulps xmm9,  xmm1

    addps xmm3,  xmm4
    addps xmm5,  xmm6
    addps xmm7,  xmm8
    addps xmm9,  xmm10

    movups oword ptr [rcx + $40], xmm3
    movups oword ptr [rcx + $50], xmm5
    movups oword ptr [rcx + $60], xmm7
    movups oword ptr [rcx + $70], xmm9

    // step 3

    movups xmm4,  oword ptr [r8 + $80]
    movups xmm6,  oword ptr [r8 + $90]
    movups xmm8,  oword ptr [r8 + $a0]
    movups xmm10, oword ptr [r8 + $b0]

    movups xmm3,  oword ptr [rdx + $80]
    movups xmm5,  oword ptr [rdx + $90]
    movups xmm7,  oword ptr [rdx + $a0]
    movups xmm9,  oword ptr [rdx + $b0]

    mulps xmm4,  xmm2
    mulps xmm6,  xmm2
    mulps xmm8,  xmm2
    mulps xmm10, xmm2

    mulps xmm3,  xmm1
    mulps xmm5,  xmm1
    mulps xmm7,  xmm1
    mulps xmm9,  xmm1

    addps xmm3,  xmm4
    addps xmm5,  xmm6
    addps xmm7,  xmm8
    addps xmm9,  xmm10

    movups oword ptr [rcx + $80], xmm3
    movups oword ptr [rcx + $90], xmm5
    movups oword ptr [rcx + $a0], xmm7
    movups oword ptr [rcx + $b0], xmm9

    // loop

    lea rcx, [rcx + $c0]
    lea rdx, [rdx + $c0]
    lea r8, [r8 + $c0]

    dec al
    jnz loop

  // end

  movdqu xmm1,  oword ptr [rsp]
  movdqu xmm2,  oword ptr [rsp + $10]
  movdqu xmm3,  oword ptr [rsp + $20]
  movdqu xmm4,  oword ptr [rsp + $30]
  movdqu xmm5,  oword ptr [rsp + $40]
  movdqu xmm6,  oword ptr [rsp + $50]
  movdqu xmm7,  oword ptr [rsp + $60]
  movdqu xmm8,  oword ptr [rsp + $70]
  movdqu xmm9,  oword ptr [rsp + $80]
  movdqu xmm10, oword ptr [rsp + $90]
  add rsp, 16 * 10

  pop r8
  pop rdx
  pop rcx
  pop rax
end;

function ComputeBlendingError(PPlain, PPrev, POff: PFloat; bp, bo: TFloat): TFloat; inline;
var
  i: Integer;
begin
  Result := 0.0;
  for i := 0 to cTileDCTSize div 8 - 1 do // unroll by 8
  begin
    Result += Sqr(PPlain^ - (PPrev^ * bp + POff^ * bo)); Inc(PPlain); Inc(PPrev); Inc(POff);
    Result += Sqr(PPlain^ - (PPrev^ * bp + POff^ * bo)); Inc(PPlain); Inc(PPrev); Inc(POff);
    Result += Sqr(PPlain^ - (PPrev^ * bp + POff^ * bo)); Inc(PPlain); Inc(PPrev); Inc(POff);
    Result += Sqr(PPlain^ - (PPrev^ * bp + POff^ * bo)); Inc(PPlain); Inc(PPrev); Inc(POff);
    Result += Sqr(PPlain^ - (PPrev^ * bp + POff^ * bo)); Inc(PPlain); Inc(PPrev); Inc(POff);
    Result += Sqr(PPlain^ - (PPrev^ * bp + POff^ * bo)); Inc(PPlain); Inc(PPrev); Inc(POff);
    Result += Sqr(PPlain^ - (PPrev^ * bp + POff^ * bo)); Inc(PPlain); Inc(PPrev); Inc(POff);
    Result += Sqr(PPlain^ - (PPrev^ * bp + POff^ * bo)); Inc(PPlain); Inc(PPrev); Inc(POff);
  end;
end;


function ComputeBlendingError_Asm(PPlain_rcx, PPrev_rdx, POff_r8: PFloat; bp_xmm3, bo_stack: TFloat): TFloat; register; assembler;
const
  cDCTSizeOffset = cTileDCTSize * SizeOf(TFloat);
label loop;
asm
  push rcx
  push rdx
  push r8

  sub rsp, 16 * 14
  movdqu oword ptr [rsp],       xmm1
  movdqu oword ptr [rsp + $10], xmm2
  movdqu oword ptr [rsp + $20], xmm3
  movdqu oword ptr [rsp + $30], xmm4
  movdqu oword ptr [rsp + $40], xmm5
  movdqu oword ptr [rsp + $50], xmm6
  movdqu oword ptr [rsp + $60], xmm7
  movdqu oword ptr [rsp + $70], xmm8
  movdqu oword ptr [rsp + $80], xmm9
  movdqu oword ptr [rsp + $90], xmm10
  movdqu oword ptr [rsp + $a0], xmm11
  movdqu oword ptr [rsp + $b0], xmm12
  movdqu oword ptr [rsp + $c0], xmm13
  movdqu oword ptr [rsp + $d0], xmm14

  pshufd xmm1, xmm3, 0
  pshufd xmm2, dword ptr [bo_stack], 0
  xorps xmm0, xmm0

  lea rax, byte ptr [rcx + cDCTSizeOffset]
  loop:
    movups xmm3,  oword ptr [rcx]
    movups xmm6,  oword ptr [rcx + $10]
    movups xmm9,  oword ptr [rcx + $20]
    movups xmm12, oword ptr [rcx + $30]

    movups xmm4,  oword ptr [rdx]
    movups xmm7,  oword ptr [rdx + $10]
    movups xmm10, oword ptr [rdx + $20]
    movups xmm13, oword ptr [rdx + $30]

    movups xmm5,  oword ptr [r8]
    movups xmm8,  oword ptr [r8 + $10]
    movups xmm11, oword ptr [r8 + $20]
    movups xmm14, oword ptr [r8 + $30]

    mulps xmm4, xmm1
    mulps xmm5, xmm2
    addps xmm4, xmm5
    subps xmm3, xmm4
    mulps xmm3, xmm3
    addps xmm0, xmm3

    mulps xmm7, xmm1
    mulps xmm8, xmm2
    addps xmm7, xmm8
    subps xmm6, xmm7
    mulps xmm6, xmm6
    addps xmm0, xmm6

    mulps xmm10, xmm1
    mulps xmm11, xmm2
    addps xmm10, xmm11
    subps xmm9, xmm10
    mulps xmm9, xmm9
    addps xmm0, xmm9

    mulps xmm13, xmm1
    mulps xmm14, xmm2
    addps xmm13, xmm14
    subps xmm12, xmm13
    mulps xmm12, xmm12
    addps xmm0, xmm12

    lea rcx, [rcx + $40]
    lea rdx, [rdx + $40]
    lea r8, [r8 + $40]

    cmp rcx, rax
    jne loop

  haddps xmm0, xmm0
  haddps xmm0, xmm0

  movdqu xmm1,  oword ptr [rsp]
  movdqu xmm2,  oword ptr [rsp + $10]
  movdqu xmm3,  oword ptr [rsp + $20]
  movdqu xmm4,  oword ptr [rsp + $30]
  movdqu xmm5,  oword ptr [rsp + $40]
  movdqu xmm6,  oword ptr [rsp + $50]
  movdqu xmm7,  oword ptr [rsp + $60]
  movdqu xmm8,  oword ptr [rsp + $70]
  movdqu xmm9,  oword ptr [rsp + $80]
  movdqu xmm10, oword ptr [rsp + $90]
  movdqu xmm11, oword ptr [rsp + $a0]
  movdqu xmm12, oword ptr [rsp + $b0]
  movdqu xmm13, oword ptr [rsp + $c0]
  movdqu xmm14, oword ptr [rsp + $d0]
  add rsp, 16 * 14

  pop r8
  pop rdx
  pop rcx
end;

function EqualQualityTileCount(tileCount: TFloat): Integer;
begin
  Result := round(sqrt(tileCount) * log2(1 + tileCount));
end;

{ TTileMapItemHelper }

function TTileMapItemHelper.GetHMirror: Boolean;
begin
  Result := tmfHMirror in Flags;
end;

function TTileMapItemHelper.GetVMirror: Boolean;
begin
  Result := tmfVMirror in Flags;
end;

procedure TTileMapItemHelper.SetHMirror(AValue: Boolean);
begin
  if AValue then
    Flags += [tmfHMirror]
  else
    Flags -= [tmfHMirror];
end;

procedure TTileMapItemHelper.SetVMirror(AValue: Boolean);
begin
  if AValue then
    Flags += [tmfVMirror]
  else
    Flags -= [tmfVMirror];
end;

function TTileMapItemHelper.GetSmoothed: Boolean;
begin
  Result := tmfSmoothed in Flags;
end;

procedure TTileMapItemHelper.SetSmoothed(AValue: Boolean);
begin
  if AValue then
    Flags += [tmfSmoothed]
  else
    Flags -= [tmfSmoothed];
end;

{ TFastPortableNetworkGraphic }

procedure TFastPortableNetworkGraphic.InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter);
var
  W: TFPWriterPNG absolute AWriter;
begin
  inherited InitializeWriter(AImage, AWriter);
  W.CompressionLevel := clfastest;
end;

{ TTileHelper }

function TTileHelper.GetActive: Boolean;
begin
  Result := tfActive in Flags;
end;

function TTileHelper.GetHasPalPixels: Boolean;
begin
  Result := tfHasPalPixels in Flags;
end;

function TTileHelper.GetHasRGBPixels: Boolean;
begin
  Result := tfHasRGBPixels in Flags;
end;

function TTileHelper.GetHMirror_Initial: Boolean;
begin
  Result := tfHMirror_Initial in Flags;
end;

function TTileHelper.GetVMirror_Initial: Boolean;
begin
  Result := tfVMirror_Initial in Flags;
end;

procedure TTileHelper.SetActive(AValue: Boolean);
begin
  if AValue then
    Flags += [tfActive]
  else
    Flags -= [tfActive];
end;

procedure TTileHelper.SetHasPalPixels(AValue: Boolean);
begin
  if AValue then
    Flags += [tfHasPalPixels]
  else
    Flags -= [tfHasPalPixels];
end;

procedure TTileHelper.SetHasRGBPixels(AValue: Boolean);
begin
  if AValue then
    Flags += [tfHasRGBPixels]
  else
    Flags -= [tfHasRGBPixels];
end;

procedure TTileHelper.SetHMirror_Initial(AValue: Boolean);
begin
  if AValue then
    Flags += [tfHMirror_Initial]
  else
    Flags -= [tfHMirror_Initial];
end;

procedure TTileHelper.SetVMirror_Initial(AValue: Boolean);
begin
  if AValue then
    Flags += [tfVMirror_Initial]
  else
    Flags -= [tfVMirror_Initial];
end;

function TTileHelper.GetRGBPixelsPtr: PRGBPixels;
begin
  Assert(HasRGBPixels, 'TTileHelper !HasRGBPixels');
  Result := PRGBPixels(PByte(@Self) + SizeOf(TTile) + IfThen(HasPalPixels, SizeOf(TPalPixels)));
end;

function TTileHelper.GetPalPixelsPtr: PPalPixels;
begin
  Assert(HasPalPixels, 'TTileHelper !HasPalPixels');
  Result := PPalPixels(PByte(@Self) + SizeOf(TTile));
end;

function TTileHelper.GetRGBPixels(y, x: Integer): Integer;
begin
  Result := GetRGBPixelsPtr^[y, x];
end;

function TTileHelper.GetPalPixels(y, x: Integer): Byte;
begin
  Result := GetPalPixelsPtr^[y, x];
end;

procedure TTileHelper.SetRGBPixels(y, x: Integer; value: Integer);
begin
  GetRGBPixelsPtr^[y, x] := value;
end;

procedure TTileHelper.SetPalPixels(y, x: Integer; value: Byte);
begin
  GetPalPixelsPtr^[y, x] := value;
end;

class function TTileHelper.Array1DNew(x: Integer; ARGBPixels, APalPixels: Boolean): PTileDynArray;
var
  i, size: Integer;
  data: PByte;
begin
  Result := nil;
  size := SizeOf(TTile) + IfThen(APalPixels, SizeOf(TPalPixels)) + IfThen(ARGBPixels, SizeOf(TRGBPixels));
  data := AllocMem(size * x);

  FillByte(data^, size * x, 0);
  SetLength(Result, x);

  for i := 0 to x - 1 do
  begin
    PTile(data)^.HasPalPixels := APalPixels;
    PTile(data)^.HasRGBPixels := ARGBPixels;
    Result[i] := PTile(data);
    Inc(data, size);
  end;
end;

class procedure TTileHelper.Array1DDispose(var AArray: PTileDynArray);
var
  i: Integer;
  smallest: Pointer;
begin
  if Length(AArray) > 0 then
  begin
    // account for the array having been sorted
    smallest := AArray[0];
    for i := 1 to High(AArray) do
      if AArray[i] < smallest then
        smallest := AArray[i];

    if Assigned(smallest) then
      Freemem(smallest);
    SetLength(AArray, 0);
  end;
end;

class procedure TTileHelper.Array1DRealloc(var AArray: PTileDynArray; ANewX: integer);
var
  prevLen, i, size: Integer;
  data: PByte;
  smallest: PTile;
  HasPalPx, HasRGBPx: Boolean;
begin
  Assert(Length(AArray) > 0);

  // account for the array having been sorted
  smallest := AArray[0];
  for i := 1 to High(AArray) do
    if AArray[i] < smallest then
      smallest := AArray[i];

  prevLen := Length(AArray);
  HasPalPx := smallest^.HasPalPixels;
  HasRGBPx := smallest^.HasRGBPixels;

  size := SizeOf(TTile) + IfThen(HasPalPx, SizeOf(TPalPixels)) + IfThen(HasRGBPx, SizeOf(TRGBPixels));
  data := PByte(smallest);

  data := ReAllocMem(data, size * ANewX);
  SetLength(AArray, ANewX);

  // recompute existing array pointers
  for i := 0 to min(ANewX, prevLen) - 1 do
    AArray[i] := PTile(PByte(AArray[i]) + (data - PByte(smallest)));

  // init new pointers
  Inc(data, size * prevLen);
  for i := prevLen to ANewX - 1 do
  begin
    AArray[i] := PTile(data);
    AArray[i]^.HasPalPixels := HasPalPx;
    AArray[i]^.HasRGBPixels := HasRGBPx;
    Inc(data, size);
  end;
end;

class function TTileHelper.New(ARGBPixels, APalPixels: Boolean): PTile;
begin
  Result := AllocMem(SizeOf(TTile) + IfThen(APalPixels, SizeOf(TPalPixels)) + IfThen(ARGBPixels, SizeOf(TRGBPixels)));
  FillByte(Result^, SizeOf(TTile), 0);

  Result^.HasPalPixels := APalPixels;
  Result^.HasRGBPixels := ARGBPixels;

  if APalPixels then
    FillByte(Result^.GetPalPixelsPtr^[0, 0], sqr(cTileWidth), 0);

  if ARGBPixels then
    FillDWord(Result^.GetRGBPixelsPtr^[0, 0], sqr(cTileWidth), 0);
end;

class procedure TTileHelper.Dispose(var ATile: PTile);
begin
  FreeMemAndNil(ATile);
end;

procedure TTileHelper.CopyPalPixelsFrom(const ATile: TTile);
begin
  Move(ATile.GetPalPixelsPtr^[0, 0], GetPalPixelsPtr^[0, 0], SizeOf(TPalPixels));
end;

procedure TTileHelper.CopyPalPixels(const APalPixels: TPalPixels);
begin
  Move(APalPixels[0, 0], GetPalPixelsPtr^[0, 0], SizeOf(TPalPixels));
end;

procedure TTileHelper.CopyPalPixels(const APalPixels: TByteDynArray);
begin
  Move(APalPixels[0], GetPalPixelsPtr^[0, 0], SizeOf(TPalPixels));
end;

procedure TTileHelper.CopyRGBPixels(const ARGBPixels: TRGBPixels);
begin
  Move(ARGBPixels[0, 0], GetRGBPixelsPtr^[0, 0], SizeOf(TRGBPixels));
end;

procedure TTileHelper.ClearPalPixels;
begin
  FillByte(GetPalPixelsPtr^[0, 0], sqr(cTileWidth), 0);
end;

procedure TTileHelper.ClearRGBPixels;
begin
  FillDWord(GetRGBPixelsPtr^[0, 0], sqr(cTileWidth), 0);
end;

procedure TTileHelper.ClearPixels;
begin
  ClearPalPixels;
  ClearRGBPixels;
end;

procedure TTileHelper.ExtractPalPixels(AArray: PFloat);
var
  i: Integer;
  PB: PByte;
  PF: PFloat;
begin
  Assert(HasPalPixels);
  PB := @GetPalPixelsPtr^[0, 0];
  PF := AArray;
  for i := 0 to Sqr(cTileWidth) - 1 do
  begin
    PF^ := PB^;
    Inc(PB);
    Inc(PF);
  end;
end;

procedure TTileHelper.LoadPalPixels(AArray: PFloat);
var
  i: Integer;
  PB: PByte;
  PF: PFloat;
begin
  Assert(HasPalPixels);
  PB := @GetPalPixelsPtr^[0, 0];
  PF := AArray;
  for i := 0 to Sqr(cTileWidth) - 1 do
  begin
    PB^ := Round(PF^);
    Inc(PB);
    Inc(PF);
  end;
end;

function TTileHelper.ComparePalPixelsTo(const ATile: TTile): Integer;
begin
  Result := CompareByte(GetPalPixelsPtr^[0, 0], ATile.GetPalPixelsPtr^[0, 0], sqr(cTileWidth));
end;

function TTileHelper.CompareRGBPixelsTo(const ATile: TTile): Integer;
begin
  Result := CompareDWord(GetRGBPixelsPtr^[0, 0], ATile.GetRGBPixelsPtr^[0, 0], sqr(cTileWidth));
end;

procedure TTileHelper.CopyFrom(const ATile: TTile);
begin
  UseCount := ATile.UseCount;
  TmpIndex := ATile.TmpIndex;
  Active := ATile.Active;
  HMirror_Initial := ATile.HMirror_Initial;
  VMirror_Initial := ATile.VMirror_Initial;

  if HasPalPixels and ATile.HasPalPixels then
    CopyPalPixels(ATile.GetPalPixelsPtr^);
  if HasRGBPixels and ATile.HasRGBPixels then
    CopyRGBPixels(ATile.GetRGBPixelsPtr^);
end;

{ TKeyFrame }

constructor TKeyFrame.Create(AParent: TTilingEncoder; AIndex, AStartFrame, AEndFrame: Integer);
begin
  Encoder := AParent;
  Index := AIndex;
  StartFrame := AStartFrame;
  EndFrame := AEndFrame;
  FrameCount := AEndFrame - AStartFrame + 1;
  FramesLeft := -1;

  FrameTilesEvent := CreateEvent(nil, True, False, nil);
end;

destructor TKeyFrame.Destroy;
begin
  CloseHandle(FrameTilesEvent);
  inherited Destroy;
end;

procedure DoLoadFFMPEGFrame(AIndex, AWidth, AHeight:Integer; AFrameData: PInteger; AUserParameter: Pointer);
var
  Encoder: TTilingEncoder;
  frmIdx: Integer;
begin
  Encoder := TTilingEncoder(AUserParameter);
  frmIdx := AIndex - Encoder.FStartFrame;

  Encoder.LoadFrameTilesFromImage(Encoder.FFrames[frmIdx], AWidth, AHeight, AFrameData);
end;

procedure TKeyFrame.AcquireFrameTiles;
var
  FFMPEG: TFFMPEG;
  PNG: TPortableNetworkGraphic;
  frmIdx: Integer;
begin
  if InterLockedIncrement(FrameTilesRefCount) = 1 then
  begin

    if FileExists(Encoder.FLoadedInputPath) then
    begin
      FFMPEG := FFMPEG_Open(Encoder.FLoadedInputPath, Encoder.FScaling, True);
      try
        FFMPEG_LoadFrames(FFMPEG, StartFrame + Encoder.FStartFrame, FrameCount, @DoLoadFFMPEGFrame, Encoder);
      finally
        FFMPEG_Close(FFMPEG);
      end;
    end
    else
    begin
      PNG := TPortableNetworkGraphic.Create;
      try
        PNG.PixelFormat:=pf32bit;
        for frmIdx := StartFrame to EndFrame do
        begin
          PNG.LoadFromFile(Format(Encoder.FLoadedInputPath, [frmIdx + Encoder.FStartFrame]));
          Encoder.LoadFrameTilesFromImage(Encoder.FFrames[frmIdx], PNG.RawImage.Description.Width, PNG.RawImage.Description.Height, PInteger(PNG.RawImage.Data));
        end;
      finally
        PNG.Free;
      end;
    end;

    // signal other threads decompression is done
    SetEvent(FrameTilesEvent);
  end
  else
  begin
    WaitForSingleObject(FrameTilesEvent, INFINITE);
  end;
end;

procedure TKeyFrame.ReleaseFrameTiles;
var
  frmIdx, ftrc: Integer;
begin
  ftrc := InterLockedDecrement(FrameTilesRefCount);
  if ftrc <= 0 then
  begin
    Assert(ftrc = 0);
    ResetEvent(FrameTilesEvent);
    for frmIdx := StartFrame to EndFrame do
      TTile.Array1DDispose(Encoder.FFrames[frmIdx].FrameTiles);
  end;
end;

procedure TKeyFrame.DoTemporalSmoothing(AFrame, APrevFrame: TFrame);
var
  sx, sy: Integer;
  cmp: TFloat;
  Tile, PrevTile: PTile;
  TMI, PrevTMI: PTileMapItem;
  CurDCT, PrevDCT: array[0 .. cTileDCTSize - 1] of TFloat;
begin
  Assert(Assigned(AFrame));
  Assert(AFrame.PKeyFrame = Self);
  Assert(Assigned(APrevFrame));
  Assert(APrevFrame.PKeyFrame = Self);

  for sy := 0 to Encoder.FTileMapHeight - 1 do
    for sx := 0 to Encoder.FTileMapWidth - 1 do
    begin
      TMI := @AFrame.TileMap[sy, sx];
      PrevTMI := @APrevFrame.TileMap[sy, sx];

      Tile := Encoder.FTiles[TMI^.TileIdx];
      PrevTile := Encoder.FTiles[PrevTMI^.TileIdx];

      Encoder.ComputeTilePsyVisFeatures(
          Tile^, True, False,
          TMI^.HMirror, TMI^.VMirror,
          cSmoothingQWeighting, cColorCpns, -1,
          AFrame.PaletteRGB[TMI^.PalIdx], PFloat(@CurDCT[0]));

      Encoder.ComputeTilePsyVisFeatures(
          PrevTile^, True, False,
          PrevTMI^.HMirror, PrevTMI^.VMirror,
          cSmoothingQWeighting, cColorCpns, -1,
          APrevFrame.PaletteRGB[PrevTMI^.PalIdx], PFloat(@PrevDCT[0]));

      // compare DCT of current tile with tile from prev frame tilemap

      cmp := CompareEuclideanDCT(CurDCT, PrevDCT);
      cmp := sqrt(cmp);

      // if difference is low enough, mark the tile as smoothed for tilemap compression use

      TMI^.Smoothed := cmp <= Encoder.FSmoothingFactor;
    end;
end;

procedure TKeyFrame.Smooth;
var
  frmIdx, sx, sy: Integer;
  TMI: PTileMapItem;
begin
  if FrameCount = 0 then
    Exit;

  for frmIdx := StartFrame to EndFrame do
    for sy := 0 to Encoder.FTileMapHeight - 1 do
      for sx := 0 to Encoder.FTileMapWidth - 1 do
      begin
        TMI := @Encoder.FFrames[frmIdx].TileMap[sy, sx];

        TMI^.Smoothed := False;
      end;

  // iteratively smooth frames

  for frmIdx := StartFrame + 1 to EndFrame do
    DoTemporalSmoothing(Encoder.FFrames[frmIdx], Encoder.FFrames[frmIdx - 1]);
end;

{ TFrame }

procedure TFrame.LogResidualErr;
var
  frmIdx: Integer;
  tileResd, errCml: Double;
begin
  tileResd := Sqrt(ReconstructErrCml / Encoder.FTileMapSize);
  WriteLn('Frame: ', Index:8, ' ResidualErr: ', tileResd:12:6, ' (by tile)');

  InterLockedDecrement(Encoder.FFramesLeft);
  if Encoder.FFramesLeft <= 0 then
  begin
    errCml := 0.0;
    for frmIdx := 0 to High(Encoder.FFrames) do
      errCml += Encoder.FFrames[frmIdx].ReconstructErrCml;

    tileResd := Sqrt(errCml / (Encoder.FTileMapSize * Length(Encoder.FFrames)));
    WriteLn('All:', Length(Encoder.FFrames):8, ' ResidualErr: ', tileResd:12:6, ' (by tile)');
  end;
end;

procedure TFrame.DoPalettization(ADitheringGamma: Integer);
var
  DCTs: TDoubleDynArray2;

  procedure DoDCTs;
  var
    ftIdx, ty, tx: Integer;
    rr, gg, bb: Byte;
    l, a, b: TFloat;
    Tile: PTile;
  begin
    SetLength(DCTs, Encoder.FTileMapSize, cTileDCTSize);

    for ftIdx := 0 to Encoder.FTileMapSize - 1 do
    begin
      Tile := FrameTiles[ftIdx];

      Encoder.ComputeTilePsyVisFeatures(Tile^, False, True, False, False, False, cColorCpns, ADitheringGamma, nil, @DCTs[ftIdx, 0]);

      DCTs[ftIdx, cTileDCTSize - 3] := 0.0;
      DCTs[ftIdx, cTileDCTSize - 2] := 0.0;
      DCTs[ftIdx, cTileDCTSize - 1] := 0.0;
      for ty := 0 to cTileWidth - 1 do
        for tx := 0 to cTileWidth - 1 do
        begin
          FromRGB(Tile^.RGBPixels[ty, tx], rr, gg, bb);

          Encoder.RGBToLAB(rr, gg, bb, ADitheringGamma, l, a, b);

          DCTs[ftIdx, cTileDCTSize - 3] += l;
          DCTs[ftIdx, cTileDCTSize - 2] += a;
          DCTs[ftIdx, cTileDCTSize - 1] += b;
        end;
    end;
  end;

var
  Yakmo: PYakmo;
  YakmoClusters: TIntegerDynArray;
  sy, sx, si, palIdx: Integer;
  PalIdxLUT: TIntegerDynArray;
begin
  DoDCTs;

  SetLength(YakmoClusters, Encoder.FTileMapSize);
  if Encoder.FPaletteCount > 1 then
  begin
    Yakmo := yakmo_create(Encoder.FPaletteCount, 1, cYakmoMaxIterations, 1, 0, 0, 0);
    yakmo_load_train_data(Yakmo, Length(DCTs), cTileDCTSize, PPDouble(@DCTs[0]));
    yakmo_train_on_data(Yakmo, @YakmoClusters[0]);
    yakmo_destroy(Yakmo);
  end;

  si := 0;
  for sy := 0 to Encoder.FTileMapHeight - 1 do
    for sx := 0 to Encoder.FTileMapWidth - 1 do
    begin
      TileMap[sy, sx].PalIdx := YakmoClusters[si];
      Inc(si);
    end;


  // sort entire palettes by use count

  SetLength(PaletteRGB, Encoder.FPaletteCount);
  SetLength(PaletteInfo, Encoder.FPaletteCount);
  SetLength(PalIdxLUT, Encoder.FPaletteCount);

  for palIdx := 0 to Encoder.FPaletteCount - 1 do
    PaletteInfo[palIdx].PalIdx_Initial := palIdx;

  for sy := 0 to Encoder.FTileMapHeight - 1 do
    for sx := 0 to Encoder.FTileMapWidth - 1 do
      Inc(PaletteInfo[TileMap[sy, sx].PalIdx].UseCount);

  QuickSort(PaletteInfo[0], 0, Encoder.FPaletteCount - 1, SizeOf(PaletteInfo[0]), @ComparePaletteUseCount, Self);
  for palIdx := 0 to Encoder.FPaletteCount - 1 do
    PalIdxLUT[PaletteInfo[palIdx].PalIdx_Initial] := palIdx;

  // assign final palette indexes

  for sy := 0 to Encoder.FTileMapHeight - 1 do
    for sx := 0 to Encoder.FTileMapWidth - 1 do
      TileMap[sy, sx].PalIdx := PalIdxLUT[TileMap[sy, sx].PalIdx];
end;

procedure TFrame.DoQuantization(APalIdx: Integer; UseYakmo: Boolean; DLv3BPC: Integer; ADitheringGamma: Integer);
var
  CMPal: TCountIndexList;

  procedure DoDennisLeeV3(AColorCount, APosterizeBpc: Integer);
  var
    dlCnt: Integer;
    dlInput: PByte;
    i, j, sy, sx, dx, dy, ty, tx, k, tileCnt, tileFx, tileFy, best: Integer;
    dlPal: TDLUserPal;
    Tile: PTile;
    CMItem: PCountIndex;
  begin
    dlCnt := Encoder.FScreenWidth * Encoder.FScreenHeight;
    dlInput := GetMem(dlCnt * 3);
    try
      FillChar(dlInput^, dlCnt * 3, 0);
      FillChar(dlPal[0, 0], SizeOf(dlPal), $ff);

      // find width and height of a rectangular area to arrange tiles

      tileCnt := 0;
      for sy := 0 to Encoder.FTileMapHeight - 1 do
        for sx := 0 to Encoder.FTileMapWidth - 1 do
        begin
          Tile := FrameTiles[sy * Encoder.FTileMapWidth + sx];
          if Tile^.Active and (Encoder.FFrames[i].TileMap[sy, sx].PalIdx = APalIdx) then
            Inc(tileCnt);
        end;

      best := MaxInt;
      tileFx := 0;
      tileFy := 0;
      j := 0;
      k := 0;
      for i := 1 to tileCnt do
      begin
        DivMod(tileCnt, i, j, k);
        if (k = 0) and (abs(i - j) < best) then
        begin
          best := abs(i - j);
          tileFx := i;
          tileFy := j;
        end;
      end;

      // copy tile data into area

      dx := 0;
      dy := 0;
      for sy := 0 to Encoder.FTileMapHeight - 1 do
        for sx := 0 to Encoder.FTileMapWidth - 1 do
        begin
          Tile := FrameTiles[sy * Encoder.FTileMapWidth + sx];

          if Tile^.Active and (Encoder.FFrames[i].TileMap[sy, sx].PalIdx = APalIdx) then
          begin
            j := ((dy * cTileWidth) * tileFx * cTileWidth + (dx * cTileWidth)) * 3;
            k := sy * Encoder.FTileMapWidth + sx;
            for ty := 0 to cTileWidth - 1 do
            begin
              for tx := 0 to cTileWidth - 1 do
              begin
                FromRGB(Tile^.RGBPixels[ty, tx], dlInput[j + 0], dlInput[j + 1], dlInput[j + 2]);
                Inc(j, 3);
              end;
              Inc(j, (tileFx - 1) * cTileWidth * 3);
            end;

            Inc(dx);
            if dx >= tileFx then
            begin
              dx := 0;
              Inc(dy);
            end;
          end;
        end;

      // call Dennis Lee v3 method

      dl3quant(dlInput, tileFx * cTileWidth, tileFy * cTileWidth, AColorCount, DLv3BPC - 1, @dlPal);
    finally
      Freemem(dlInput);
    end;

    // retrieve palette data

    for i := 0 to AColorCount - 1 do
    begin
      New(CMItem);
      CMItem^.Count := 0;
      CMItem^.R := Posterize(dlPal[0][i], APosterizeBpc); CMItem^.G := Posterize(dlPal[1][i], APosterizeBpc); CMItem^.B := Posterize(dlPal[2][i], APosterizeBpc);
      Encoder.RGBToHSV(ToRGB(CMItem^.R, CMItem^.G, CMItem^.B), CMItem^.Hue, CMItem^.Sat, CMItem^.Val);
      CMPal.Add(CMItem);
    end;
  end;


  procedure DoYakmo(AColorCount, APosterizeBpc: Integer);
  const
    cFeatureCount = 3;
  var
    i, di, ty, tx, sy, sx, DSLen: Integer;
    rr, gg, bb: Byte;
    Tile: PTile;
    Dataset, Centroids: TDoubleDynArray2;
    Clusters: TIntegerDynArray;
    Yakmo: PYakmo;
    CMItem: PCountIndex;
  begin
    DSLen := 0;
    for sy := 0 to Encoder.FTileMapHeight - 1 do
      for sx := 0 to Encoder.FTileMapWidth - 1 do
        if TileMap[sy, sx].PalIdx = APalIdx then
          Inc(DSLen);

    SetLength(Dataset, DSLen * Sqr(cTileWidth), cFeatureCount);
    SetLength(Clusters, Length(Dataset));
    SetLength(Centroids, AColorCount, cFeatureCount);

    // build a dataset of RGB pixels

    di := 0;
    for sy := 0 to Encoder.FTileMapHeight - 1 do
      for sx := 0 to Encoder.FTileMapWidth - 1 do
        if TileMap[sy, sx].PalIdx = APalIdx then
        begin
          Tile := FrameTiles[sy * Encoder.FTileMapWidth + sx];
          for ty := 0 to cTileWidth - 1 do
            for tx := 0 to cTileWidth - 1 do
            begin
              FromRGB(Tile^.RGBPixels[ty, tx], rr, gg, bb);
              Dataset[di, 0] := rr;
              Dataset[di, 1] := gg;
              Dataset[di, 2] := bb;
              Inc(di);
            end;
        end;
    Assert(di = Length(Dataset));

    // use KMeans to quantize to AColorCount elements

    if Length(Dataset) >= AColorCount then
    begin
      Yakmo := yakmo_create(AColorCount, 1, cYakmoMaxIterations, 1, 0, 0, 0);
      yakmo_load_train_data(Yakmo, Length(Dataset), cFeatureCount, PPDouble(@Dataset[0]));

      SetLength(Dataset, 0); // free up some memmory

      yakmo_train_on_data(Yakmo, @Clusters[0]);
      yakmo_get_centroids(Yakmo, PPDouble(@Centroids[0]));
      yakmo_destroy(Yakmo);
    end;

    // retrieve palette data

    for i := 0 to AColorCount - 1 do
    begin
      New(CMItem);

      if Length(Clusters) >= AColorCount then
      begin
        CMItem^.R := 0;
        CMItem^.G := 0;
        CMItem^.B := 0;
        if not IsNan(Centroids[i, 0]) and not IsNan(Centroids[i, 1]) and  not IsNan(Centroids[i, 2]) then
        begin
          CMItem^.R := Posterize(EnsureRange(Round(Centroids[i, 0]), 0, 255), APosterizeBpc);
          CMItem^.G := Posterize(EnsureRange(Round(Centroids[i, 1]), 0, 255), APosterizeBpc);
          CMItem^.B := Posterize(EnsureRange(Round(Centroids[i, 2]), 0, 255), APosterizeBpc);
        end;
      end;

      CMItem^.Count := 0;
      Encoder.RGBToHSV(ToRGB(CMItem^.R, CMItem^.G, CMItem^.B), CMItem^.Hue, CMItem^.Sat, CMItem^.Val);
      CMPal.Add(CMItem);
    end;
  end;

  function HasColor(AColor: Integer): Boolean;
  var i: Integer;
  begin
    Result := False;
    for i := 0 to CMPal.Count - 1 do
      if ToRGB(CMPal[i]^.R, CMPal[i]^.G, CMPal[i]^.B) = AColor then
        Exit(True);
  end;

  procedure DoPosterizerBased(APosterizeBpc: Integer);
  const
    cAddlColors : array[0..1] of Integer = ($ffffff, $000000);
  var
    i, j, prevCol, col, uniqueColCount, pbColCount, bestUniqueColCount, bestPbColCount: Integer;
    CMItem: PCountIndex;
  begin
    bestUniqueColCount := -1;
    bestPbColCount := -1;
    pbColCount := Encoder.FPaletteSize;

    repeat
      // call appropriate method

      if UseYakmo then
        DoYakmo(pbColCount, APosterizeBpc)
      else
        DoDennisLeeV3(pbColCount, APosterizeBpc);

      // count unique colors

      CMPal.Sort(@CompareCMULHS);
      uniqueColCount := CMPal.Count;
      prevCol := -1;
      for i := 0 to CMPal.Count - 1 do
      begin
        CMItem := CMPal[i];
        col := ToRGB(CMItem^.R, CMItem^.G, CMItem^.B);
        if col = prevCol then
          Dec(uniqueColCount);
        prevCol := col;
      end;

      Inc(pbColCount);
      New(CMItem);
      CMItem^.Count := 0;
      CMPal.Add(CMItem);

      if (uniqueColCount > bestUniqueColCount) and (uniqueColCount <= Encoder.FPaletteSize) then
      begin
        bestUniqueColCount := uniqueColCount;
        bestPbColCount := pbColCount;
      end;

      // clear palette

      for i := 0 to CMPal.Count - 1 do
        Dispose(CMPal[i]);
      CMPal.Clear;

    until (uniqueColCount >= Encoder.FPaletteSize) or (pbColCount >= High(Byte));

    // call appropriate method

    if UseYakmo then
      DoYakmo(bestPbColCount, APosterizeBpc)
    else
      DoDennisLeeV3(bestPbColCount, APosterizeBpc);

    // prune duplicate colors

    CMPal.Sort(@CompareCMULHS);
    prevCol := -1;
    for i := 0 to CMPal.Count - 1 do
    begin
      CMItem := CMPal[i];
      col := ToRGB(CMItem^.R, CMItem^.G, CMItem^.B);
      if col = prevCol then
      begin
        Dispose(CMPal[i]);
        CMPal[i] := nil;
      end;
      prevCol := col;
    end;
    CMPal.Pack;

    // ensure enough colors (stuff with commmon colors)

    j := 0;
    for i := CMPal.Count to Encoder.FPaletteSize - 1 do
    begin
      New(CMItem);
      FillChar(CMItem^, SizeOf(TCountIndex), 0);

      while HasColor(cAddlColors[j]) and (j < High(cAddlColors)) do
        Inc(j);

      FromRGB(cAddlColors[j], CMItem^.R, CMItem^.G, CMItem^.B);
      col := ToRGB(CMItem^.R, CMItem^.G, CMItem^.B);
      Encoder.RGBToHSV(col, CMItem^.Hue, CMItem^.Sat, CMItem^.Val);

      CMPal.Add(CMItem);
    end;
  end;

var
  i: Integer;
begin
  CMPal := TCountIndexList.Create;
  try
    // do quantize

    if Encoder.QuantizerPosterize then
    begin
      DoPosterizerBased(Encoder.QuantizerPosterizeBitsPerComponent);
    end
    else
    begin
      if UseYakmo then
        DoYakmo(Encoder.FPaletteSize, cBitsPerComp)
      else
        DoDennisLeeV3(Encoder.FPaletteSize, cBitsPerComp);
    end;

    // split most used colors into tile palettes

    CMPal.Sort(@CompareCMULHS);

    SetLength(PaletteRGB[APalIdx], Encoder.FPaletteSize);
    for i := 0 to Encoder.FPaletteSize - 1 do
      PaletteRGB[APalIdx, i] := ToRGB(CMPal[i]^.R, CMPal[i]^.G, CMPal[i]^.B);

    for i := 0 to CMPal.Count - 1 do
      Dispose(CMPal[i]);
  finally
    CMPal.Free;
  end;
end;

procedure TFrame.OptimizePalettes;
var
  i, j, palIdx, colIdx1, colIdx2, iteration, bestPalIdx, bestColIdx1, bestColIdx2, tmp, uc: Integer;
  r, g, b: byte;
  prevBest, best, v: Double;
  InnerPerm: TByteDynArray;
  PalR, PalG, PalB, InnerPalR, InnerPalG, InnerPalB: TDoubleDynArray;
begin
  SetLength(PalR, Encoder.FPaletteSize);
  SetLength(PalG, Encoder.FPaletteSize);
  SetLength(PalB, Encoder.FPaletteSize);
  SetLength(InnerPalR, Encoder.FPaletteSize);
  SetLength(InnerPalG, Encoder.FPaletteSize);
  SetLength(InnerPalB, Encoder.FPaletteSize);
  SetLength(InnerPerm, Encoder.FPaletteSize);

  // stepwise algorithm on palette colors permutations

  best := 0;
  iteration := 0;
  repeat
    prevBest := best;
    best := 0;

    bestPalIdx := -1;
    bestColIdx1 := -1;
    bestColIdx2 := -1;
    for palIdx := 0 to High(PaletteRGB) do
    begin
      // accumulate the whole palette except the one that will be permutated

      FillQWord(PalR[0], Length(PalR), 0);
      FillQWord(PalG[0], Length(PalG), 0);
      FillQWord(PalB[0], Length(PalB), 0);
      for i := 0 to High(PaletteRGB) do
      begin
        uc := PaletteInfo[i].UseCount;
        for j := 0 to Encoder.FPaletteSize - 1 do
          if i <> palIdx then
          begin
            FromRGB(PaletteRGB[i, j], r, g, b);
            PalR[j] += r * uc;
            PalG[j] += g * uc;
            PalB[j] += b * uc;
          end;
      end;

      // try all permutations in the current palette

      for colIdx1 := 0 to High(InnerPerm) do
        for colIdx2 := colIdx1 + 1 to High(InnerPerm) do
        begin
          for i := 0 to Encoder.FPaletteSize - 1 do
            InnerPerm[i] := i;

          tmp := InnerPerm[colIdx1];
          InnerPerm[colIdx1] := InnerPerm[colIdx2];
          InnerPerm[colIdx2] := tmp;

          Move(PalR[0], InnerPalR[0], Length(PalR) * SizeOf(Double));
          Move(PalG[0], InnerPalG[0], Length(PalG) * SizeOf(Double));
          Move(PalB[0], InnerPalB[0], Length(PalB) * SizeOf(Double));

          uc := PaletteInfo[palIdx].UseCount;
          for i := 0 to Encoder.FPaletteSize - 1 do
          begin
            FromRGB(PaletteRGB[palIdx, InnerPerm[i]], r, g, b);
            InnerPalR[i] += r * uc;
            InnerPalG[i] += g * uc;
            InnerPalB[i] += b * uc;
          end;

          // try to maximize accumulated palette standard deviation
          // rationale: the less samey it is, the better the colors pair with each other across palettes

          v := cRedMul * StdDev(InnerPalR) + cGreenMul * StdDev(InnerPalG) + cBlueMul * StdDev(InnerPalB);

          if v > best then
          begin
            best := v;
            bestPalIdx := palIdx;
            bestColIdx1 := colIdx1;
            bestColIdx2 := colIdx2;
          end;
        end;
    end;

    if (best > prevBest) and (bestPalIdx >= 0) and (bestColIdx1 >= 0) and (bestColIdx2 >= 0) then
    begin
      tmp := PaletteRGB[bestPalIdx, bestColIdx1];
      PaletteRGB[bestPalIdx, bestColIdx1] := PaletteRGB[bestPalIdx, bestColIdx2];
      PaletteRGB[bestPalIdx, bestColIdx2] := tmp;
    end;

    Inc(iteration);

    //WriteLn(iteration:3, bestPalIdx:3, bestColIdx1:3, bestColIdx2:3, best:12:0);

  until best <= prevBest;

  WriteLn('Frame: ', Index:8, ' OptimizePalettes: ', iteration, ' iterations');
end;

procedure TFrame.PrepareTiling(APaletteIndex, AFTGamma: Integer);
var
  DS: PTilingDataset;

  procedure DoPsyV;
  var
    T: PTile;
    tidx: Int64;
  begin
    for tidx := 0 to High(Encoder.Tiles) do
    begin
      T := Encoder.Tiles[tidx];

      Assert(T^.Active);
      Encoder.ComputeTilePsyVisFeatures(T^, True, False, cReconstructQWeighting, False, False, cColorCpns, AFTGamma, PaletteRGB[APaletteIndex], PSingle(@DS^.Dataset[tidx, 0]));
    end;
  end;

begin
  // Compute psycho visual model for all tiles (in curent palette)

  DS := New(PTilingDataset);
  FillChar(DS^, SizeOf(TTilingDataset), 0);

  DS^.KNNSize := Encoder.GetTileCount(False);
  SetLength(DS^.Dataset, DS^.KNNSize, cTileDCTSize);

  DoPsyV;

  // Build KNN

  DS^.ANN := ann_kdtree_single_create(PPSingle(@DS^.Dataset[0]), DS^.KNNSize, cTileDCTSize, 32, ANN_KD_STD);

  // Dataset is ready

  TileDS[APaletteIndex] := DS;
end;

procedure TFrame.FinishTiling(APaletteIndex: Integer);
begin
  if Length(TileDS[APaletteIndex]^.Dataset) > 0 then
    ann_kdtree_single_destroy(TileDS[APaletteIndex]^.ANN);
  TileDS[APaletteIndex]^.ANN := nil;
  SetLength(TileDS[APaletteIndex]^.Dataset, 0);
  Dispose(TileDS[APaletteIndex]);

  TileDS[APaletteIndex] := nil;
end;

procedure TFrame.DoTiling(APaletteIndex: Integer; AFTGamma: Integer);
var
  sx, sy, dsIdx: Integer;
  errCml: Double;
  dsErr: Single;

  T: PTile;
  TMI: PTileMapItem;

  DS: PTilingDataset;
  DCT: array[0 .. cTileDCTSize - 1] of Single;
begin
  DS := TileDS[APaletteIndex];
  if DS^.KNNSize <= 0 then
    Exit;

  errCml := 0.0;

  for sy := 0 to Encoder.FTileMapHeight - 1 do
    for sx := 0 to Encoder.FTileMapWidth - 1 do
    begin
      TMI := @TileMap[sy, sx];

      if TMI^.PalIdx <> APaletteIndex then
        Continue;

      // prepare KNN query

      T := FrameTiles[sy * Encoder.FTileMapWidth + sx];

      if Encoder.FFrameTilingFromPalette then
        Encoder.DitherTile(T^, PaletteInfo[APaletteIndex].MixingPlan);

      Encoder.ComputeTilePsyVisFeatures(T^, Encoder.FFrameTilingFromPalette, False, cReconstructQWeighting, False, False, cColorCpns, AFTGamma, PaletteRGB[APaletteIndex], @DCT[0]);

      TMI^.HMirror := T^.HMirror_Initial;
      TMI^.VMirror := T^.VMirror_Initial;

      // query KNN

      dsIdx := ann_kdtree_single_search(DS^.ANN, @DCT[0], 0.0, @dsErr);

      // map keyframe tilemap items to reduced tiles and mirrors, parsing KNN query

      if InRange(dsIdx, 0, DS^.KNNSize - 1) then
      begin
        TMI^.TileIdx := dsIdx;
        TMI^.ResidualErr := dsErr;
        TMI^.Smoothed := False;

        system.InterLockedIncrement(Encoder.Tiles[dsIdx]^.UseCount);

        errCml += TMI^.ResidualErr;
      end;
    end;

  ReconstructErrCml += errCml;
end;

constructor TFrame.Create(AParent: TTilingEncoder; AIndex: Integer);
begin
  Encoder := AParent;
  Index := AIndex;
end;

procedure TFrame.PreparePalettes;
var
  palIdx: Integer;
begin
  try
    PKeyFrame.AcquireFrameTiles;
    try
      DoPalettization(IfThen(Encoder.FDitheringUseGamma, 0, -1));

      for palIdx := 0 to High(PaletteRGB) do
        DoQuantization(palIdx, Encoder.FQuantizerUseYakmo, Encoder.FQuantizerDennisLeeBitsPerComponent, IfThen(Encoder.FDitheringUseGamma, 0, -1));
    finally
      PKeyFrame.ReleaseFrameTiles;
    end;

    OptimizePalettes;
  finally
    InterLockedDecrement(Encoder.FFramesLeft);
  end;
end;

procedure TFrame.Reconstruct;
var
  palIdx: Integer;
begin
  PKeyFrame.AcquireFrameTiles;
  SetLength(TileDS, Length(PaletteRGB));
  try
    ReconstructErrCml := 0.0;

    for palIdx := 0 to High(PaletteRGB) do
    begin
      PrepareTiling(palIdx, IfThen(Encoder.FFrameTilingUseGamma, 0, -1));
      DoTiling(palIdx, IfThen(Encoder.FFrameTilingUseGamma, 0, -1));
      FinishTiling(palIdx);
    end;

    LogResidualErr;
  finally
    SetLength(TileDS, 0);
    PKeyFrame.ReleaseFrameTiles;
  end;
end;


{ TTilingEncoder }

procedure TTilingEncoder.InitLuts;
var
  g, i, v, u, y, x: Int64;
begin
  // gamma

  for g := -1 to High(FGamma) do
    for i := 0 to High(Byte) do
      if g >= 0 then
        FGammaCorLut[g, i] := power(i / 255.0, FGamma[g])
      else
        FGammaCorLut[g, i] := i / 255.0;

  // inverse

  for i := 0 to High(FVecInv) do
    FVecInv[i] := iDiv0(1 shl cVecInvWidth, i shr 2);

  // DCT

  i := 0;
  for v := 0 to cTileWidth * cDCTFeaturesMul - 1 do
    for u := 0 to cTileWidth * cDCTFeaturesMul - 1 do
      for y := 0 to cTileWidth - 1 do
        for x := 0 to cTileWidth - 1 do
        begin
          FDCTLutDouble[i] := cos((x + 0.5) * u * PI / (cTileWidth * cDCTFeaturesMul)) * cos((y + 0.5) * v * PI / (cTileWidth * cDCTFeaturesMul)) * cDCTUVRatio[Min(v, 7), Min(u, 7)];
          FDCTLut[i] := FDCTLutDouble[i];
          Inc(i);
        end;

  // inverse DCT

  i := 0;
  for v := 0 to cTileWidth - 1 do
    for u := 0 to cTileWidth - 1 do
      for y := 0 to cTileWidth * cDCTFeaturesMul - 1 do
        for x := 0 to cTileWidth * cDCTFeaturesMul - 1 do
        begin
          FInvDCTLutDouble[i] := cos((u + 0.5) * x * PI / (cTileWidth * cDCTFeaturesMul)) * cos((v + 0.5) * y * PI / (cTileWidth * cDCTFeaturesMul)) * cDCTUVRatio[Min(y, 7), Min(x, 7)] * 2 / (cTileWidth * cDCTFeaturesMul) * 2 / (cTileWidth * cDCTFeaturesMul);
          Inc(i);
        end;
end;

function TTilingEncoder.GammaCorrect(lut: Integer; x: Byte): TFloat;
begin
  Result := FGammaCorLut[lut, x];
end;

function TTilingEncoder.GammaUncorrect(lut: Integer; x: TFloat): Byte;
begin
  if lut >= 0 then
    x := power(Max(0, x), 1 / FGamma[lut]);
  Result := EnsureRange(Round(x * 255.0), 0, 255);
end;

procedure TTilingEncoder.Load;
var
  frmIdx, frmCnt, eqtc, startFrmIdx: Integer;
  fn: String;
  bmp: TPicture;
  wasAutoQ, manualKeyFrames: Boolean;
  qbTC: TFloat;
  FFMPEG: TFFMPEG;
begin
  eqtc := EqualQualityTileCount(FrameCount * FTileMapSize);
  wasAutoQ := (Length(FFrames) > 0) and (FGlobalTilingTileCount = round(FGlobalTilingQualityBasedTileCount * eqtc));

  ProgressRedraw(-1, '', esAll);

  FLoadedInputPath := '';
  ClearAll;

  ProgressRedraw(0, '', esLoad);

  // init Gamma LUTs

  InitLuts;

  // load video

  frmCnt := FFrameCountSetting;
  manualKeyFrames := False;
  FLoadedInputPath := FInputFileName;

  if FileExists(FLoadedInputPath) then
  begin
    FFMPEG := FFMPEG_Open(FLoadedInputPath, FScaling, False);
    try

      FFramesPerSecond := FFMPEG.FramesPerSecond;
      ReframeUI((FFMPEG.DstWidth - 1) div cTileWidth + 1, (FFMPEG.DstHeight - 1) div cTileWidth + 1);

      frmCnt := FFMPEG.FrameCount;
      if frmCnt > 0 then
        frmCnt -= FStartFrame;
      if FrameCountSetting > 0 then
        frmCnt := FrameCountSetting;

      WriteLn(frmCnt:8, ' frames, ', FFMPEG.DstWidth:4, ' x ', FFMPEG.DstHeight:4, ' @ ', FFramesPerSecond:6:3, ' fps');
    finally
      FFMPEG_Close(FFMPEG);
    end;
  end
  else
  begin
    FFramesPerSecond := 24.0;
    startFrmIdx := FStartFrame;
    manualKeyFrames := True;

    // automaticaly count frames if needed

    if frmCnt <= 0 then
    begin
      frmIdx := 0;
      repeat
        fn := Format(FLoadedInputPath, [frmIdx + startFrmIdx]);
        Inc(frmIdx);
      until not FileExists(fn);

      frmCnt := frmIdx - 1;
    end;

    // load frames bitmap data

    bmp := TPicture.Create;
    try
      bmp.Bitmap.PixelFormat:=pf32bit;
      bmp.LoadFromFile(Format(FLoadedInputPath, [startFrmIdx]));
      ReframeUI((bmp.Width - 1) div cTileWidth + 1, (bmp.Height - 1) div cTileWidth + 1);
    finally
      bmp.Free;
    end;
  end;

  InitFrames(frmCnt);

  ProgressRedraw(1, 'LoadFrames');

  FindKeyFrames(manualKeyFrames);

  ProgressRedraw(2, 'FindKeyFrames');

  if wasAutoQ or (FGlobalTilingTileCount <= 0) then
  begin
    qbTC := FGlobalTilingQualityBasedTileCount;
    SetGlobalTilingQualityBasedTileCount(0.0);
    SetGlobalTilingQualityBasedTileCount(qbTC);
  end;

  // print settings

  WriteLn(GetSettings);
end;

procedure TTilingEncoder.PreparePalettes;
var
  StepProgress: Integer;

  procedure DoRun(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    frame: TFrame;
  begin
    if not InRange(AIndex, 0, High(FFrames)) then
      Exit;

    frame := FFrames[AIndex];

    frame.PreparePalettes;

    Inc(StepProgress);
    ProgressRedraw(StepProgress, 'Frame: ' + IntToStr(frame.Index), esPreparePalettes, AItem.Thread);
  end;

begin
  if Length(FFrames) = 0 then
    Exit;

  StepProgress := 0;
  ProgressRedraw(0, '', esPreparePalettes);

  FFramesLeft := Length(FFrames);
  ProcThreadPool.DoParallelLocalProc(@DoRun, 0, High(FFrames));
end;

procedure TTilingEncoder.Cluster;
var
  frmIdx, palIdx, colIdx: Integer;
  BWPal, pal: TIntegerDynArray;
begin
  if FrameCount = 0 then
    Exit;

  ProgressRedraw(0, '', esCluster);

  OptimizeGlobalPalettes;

  ProgressRedraw(1, 'OptimizeGlobalPalettes');

  // build ditherers
  SetLength(BWPal, FPaletteSize);
  for frmIdx := 0 to High(FFrames) do
    for palIdx := 0 to High(FFrames[frmIdx].PaletteRGB) do
    begin
      pal := FFrames[frmIdx].PaletteRGB[palIdx];
      PreparePlan(FFrames[frmIdx].PaletteInfo[palIdx].MixingPlan, FDitheringYliluoma2MixedColors, pal);

      for colIdx := 0 to FPaletteSize - 1 do
        BWPal[colIdx] := ToBW(pal[colIdx]);
      PreparePlan(FFrames[frmIdx].PaletteInfo[palIdx].BWMixingPlan, FDitheringYliluoma2MixedColors, BWPal);
    end;

  ProgressRedraw(2, 'BuildDitherers');

  // cleanup any prior tile set
  TTile.Array1DDispose(FTiles);

  // run the clustering algorithm, which will group similar tiles until it reaches a fixed amount of groups
  DoGlobalKMeans(FGlobalTilingTileCount, IfThen(FGlobalTilingUseGamma, 0, -1), IfThen(FGlobalTilingLumaOnly, 1, 3), FGlobalTilingRatio, 1.0 - FGlobalTilingRatio);
end;

procedure TTilingEncoder.Reconstruct;
var
  StepProgress: Integer;

  procedure DoRun(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    frame: TFrame;
  begin
    if not InRange(AIndex, 0, High(FFrames)) then
      Exit;

    frame := FFrames[AIndex];

    frame.Reconstruct;

    Inc(StepProgress);
    ProgressRedraw(StepProgress, 'Frame: ' + IntToStr(frame.Index), esReconstruct, AItem.Thread);
  end;

var
  tIdx: Integer;
begin
  if Length(FFrames) = 0 then
    Exit;

  StepProgress := 0;
  ProgressRedraw(0, '', esReconstruct);

  for tIdx := 0 to High(Tiles) do
    Tiles[tIdx]^.UseCount := 0;
  FFramesLeft := Length(FFrames);

  ProcThreadPool.DoParallelLocalProc(@DoRun, 0, High(FFrames));
end;

procedure TTilingEncoder.Smooth;
var
  StepProgress: Integer;

  procedure DoRun(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    keyFrame: TKeyFrame;
  begin
    if not InRange(AIndex, 0, High(FKeyFrames)) then
      Exit;

    keyFrame := FKeyFrames[AIndex];

    keyFrame.Smooth;

    Inc(StepProgress, keyFrame.FrameCount);
    ProgressRedraw(StepProgress, 'KF: ' + IntToStr(keyFrame.StartFrame), esSmooth, AItem.Thread);
  end;

begin
  if Length(FFrames) = 0 then
    Exit;

  StepProgress := 0;
  ProgressRedraw(0, '', esSmooth);

  ProcThreadPool.DoParallelLocalProc(@DoRun, 0, High(FKeyFrames));
end;

procedure TTilingEncoder.Reindex;

  procedure HandleTileIndex(ATileIndex: Integer);
  begin
    if ATileIndex >= 0 then
    begin
      Inc(Tiles[ATileIndex]^.UseCount);
      Tiles[ATileIndex]^.Active := True;
    end;
  end;

var
  frmIdx, sx, sy: Integer;
  tidx: Int64;
  TMI: PTileMapItem;
begin
  if FrameCount = 0 then
    Exit;

  ProgressRedraw(0, '', esReindex);

  for tidx := 0 to High(Tiles) do
  begin
    Tiles[tidx]^.UseCount := 0;
    Tiles[tidx]^.Active := False;
  end;

  for frmIdx := 0 to High(FFrames) do
    for sy := 0 to FTileMapHeight - 1 do
      for sx := 0 to FTileMapWidth - 1 do
      begin
        TMI := @FFrames[frmIdx].TileMap[sy, sx];

        HandleTileIndex(TMI^.TileIdx);
      end;

  ProgressRedraw(1, 'UseCount');

  ReindexTiles(False);

  ProgressRedraw(2, 'Sort');
end;

procedure TTilingEncoder.Save;
var
  fs: TBufferedFileStream;
begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(0, '', esSave);

  fs := TBufferedFileStream.Create(FOutputFileName, fmCreate or fmShareDenyWrite);
  try
    SaveStream(fs);
  finally
    fs.Free;
  end;

  ProgressRedraw(1, '');
end;

procedure TTilingEncoder.ReloadGTM(AFileName: String);
var
  fs: TBufferedFileStream;
begin
  ProgressRedraw(0, '', esLoad);

  fs := TBufferedFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadStream(fs);
  finally
    fs.Free;
  end;

  ProgressRedraw(2, '');
end;

procedure TTilingEncoder.GeneratePNGs;
var
  palPict: TPortableNetworkGraphic;
  i, palIdx, colIdx, oldRenderFrameIndex : Integer;
  oldRenderPage: TRenderPage;
  palData: TStringList;
begin
  palPict := TFastPortableNetworkGraphic.Create;

  palPict.Width := FScreenWidth;
  palPict.Height := FScreenHeight;
  palPict.PixelFormat := pf24bit;

  palData := TStringList.Create;
  oldRenderFrameIndex := RenderFrameIndex;
  oldRenderPage := RenderPage;
  try
    RenderPage := rpOutput;

    for i := 0 to High(FFrames) do
    begin
      RenderFrameIndex := i;
      Render(True);

      palPict.Canvas.Draw(0, 0, FOutputBitmap);
      palPict.SaveToFile(Format('%s_%.4d.png', [ChangeFileExt(FOutputFileName, ''), i]));

      palData.Clear;
      for palIdx := 0 to High(FFrames[i].PaletteRGB) do
        for colIdx := 0 to FPaletteSize - 1 do
          palData.Add(IntToHex($ff000000 or FFrames[i].PaletteRGB[palIdx, colIdx], 8));
      palData.SaveToFile(Format('%s_%.4d.txt', [ChangeFileExt(FOutputFileName, ''), i]));
    end;
  finally
    palPict.Free;

    RenderFrameIndex := oldRenderFrameIndex;
    RenderPage := oldRenderPage;
    Render(False);

    palData.Free;
  end;
end;

procedure TTilingEncoder.GenerateY4M(AFileName: String);
var
  fx, fy, i, oldRenderFrameIndex : Integer;
  oldRenderPage: TRenderPage;
  fs: TBufferedFileStream;
  Header, FrameHeader: String;
  ptr: PByte;
  yf, uf, vf: TFloat;
  r, g, b: Byte;
  py, pu, pv: PByte;
  FrameData: TByteDynArray;
begin
  oldRenderFrameIndex := RenderFrameIndex;
  oldRenderPage := RenderPage;
  fs := TBufferedFileStream.Create(AFileName, fmCreate or fmShareDenyWrite);
  try
    Header := Format('YUV4MPEG2 W%d H%d F%d:1000000 C444'#10, [FTileMapWidth * cTileWidth, FTileMapHeight * cTileWidth, round(FFramesPerSecond * 1000000)]);
    fs.Write(Header[1], length(Header));

    SetLength(FrameData, FTileMapWidth * cTileWidth * FTileMapHeight * cTileWidth * cColorCpns);

    RenderPage := rpOutput;

    for i := 0 to High(FFrames) do
    begin
      FrameHeader := 'FRAME '#10;
      fs.Write(FrameHeader[1], Length(FrameHeader));

      RenderFrameIndex := i;
      Render(True);

      py := @FrameData[0 * Length(FrameData) div cColorCpns];
      pu := @FrameData[1 * Length(FrameData) div cColorCpns];
      pv := @FrameData[2 * Length(FrameData) div cColorCpns];

      FOutputBitmap.BeginUpdate;
      try
        for fy := 0 to FOutputBitmap.Height - 1 do
        begin
          ptr := PByte(FOutputBitmap.ScanLine[fy]);
          for fx := 0 to FOutputBitmap.Width - 1 do
          begin
            b := ptr^; Inc(ptr);
            g := ptr^; Inc(ptr);
            r := ptr^; Inc(ptr);
            Inc(ptr); // alpha

            RGBToYUV(r, g, b, -1, yf, uf, vf);

            py^ := Round(yf * High(Byte)); Inc(py);
            pu^ := Round((uf + 0.5) * High(Byte)); Inc(pu);
            pv^ := Round((vf + 0.5) * High(Byte)); Inc(pv);
          end;
        end;
      finally
        FOutputBitmap.EndUpdate;
      end;

      fs.Write(FrameData[0], Length(FrameData));
    end;
  finally
    RenderFrameIndex := oldRenderFrameIndex;
    RenderPage := oldRenderPage;
    Render(False);
    fs.Free;
  end;
end;

function TTilingEncoder.PearsonCorrelation(const x: TFloatDynArray; const y: TFloatDynArray): TFloat;
var
  mx, my, num, den, denx, deny: TFloat;
  i: Integer;
begin
  Assert(Length(x) = Length(y));

  mx := mean(x);
  my := mean(y);

  num := 0.0;
  denx := 0.0;
  deny := 0.0;
  for i := 0 to High(x) do
  begin
    num += (x[i] - mx) * (y[i] - my);
    denx += sqr(x[i] - mx);
    deny += sqr(y[i] - my);
  end;

  denx := sqrt(denx);
  deny := sqrt(deny);
  den := denx * deny;

  Result := 0.0;
  if den <> 0.0 then
    Result := num / den;
end;

function TTilingEncoder.GetKeyFrameCount: Integer;
begin
  Result := Length(FKeyFrames);
end;

function TTilingEncoder.GetMaxThreadCount: Integer;
begin
 Result := ProcThreadPool.MaxThreadCount;
end;

function TTilingEncoder.GetTiles: PTileDynArray;
begin
  Result := FTiles;
end;

function TTilingEncoder.GetFrameCount: Integer;
begin
  Result := Length(FFrames);
end;

function TTilingEncoder.PrepareInterFrameCorrelation(AFrame: TFrame): TFloatDynArray;
var
  sz, i, tx, ty, sx, sy: Integer;
  rr, gg, bb: Integer;
  par, pag, pab: PFloat;
  pat: PInteger;
  zeroPixels: TRGBPixels;
begin
  Result := nil;
  sz := FTileMapSize * Sqr(cTileWidth);
  SetLength(Result, sz * 3);
  FillChar(zeroPixels, SizeOf(zeroPixels), 0);

  if Assigned(AFrame) then AFrame.PKeyFrame.AcquireFrameTiles;
  try
    par := @Result[sz * 0]; pag := @Result[sz * 1]; pab := @Result[sz * 2];

    for sy := 0 to FTileMapHeight - 1 do
      for ty := 0 to cTileWidth - 1 do
        for sx := 0 to FTileMapWidth - 1 do
        begin
          i := sy * FTileMapWidth + sx;

          if Assigned(AFrame) then
            pat := PInteger(@AFrame.FrameTiles[i]^.GetRGBPixelsPtr^[ty, 0])
          else
            pat := PInteger(@zeroPixels[0, 0]);

          for tx := 0 to cTileWidth - 1 do
          begin
            FromRGB(pat^, rr, gg, bb);
            Inc(pat);
            RGBToLAB(rr, gg, bb, -1, par^, pag^, pab^);
            Inc(par); Inc(pag); Inc(pab);
          end;
        end;
  finally
    if Assigned(AFrame) then AFrame.PKeyFrame.ReleaseFrameTiles;
  end;
end;

function TTilingEncoder.GetSettings: String;
var
  tmpFN: String;
begin
  tmpFN := GetTempFileName;
  try
    SaveSettings(tmpFN);
    Result := ReadFileToString(tmpFN);
  finally
    DeleteFile(tmpFN);
  end;
end;

procedure TTilingEncoder.PreparePlan(var Plan: TMixingPlan; MixedColors: Integer; const pal: array of Integer);
var
  i, r, g, b: Integer;
begin
  FillChar(Plan, SizeOf(Plan), 0);

  Plan.Y2MixedColors := MixedColors;
  SetLength(Plan.LumaPal, Length(pal));
  SetLength(Plan.Y2Palette, Length(pal));

  for i := 0 to High(pal) do
  begin
    FromRGB(pal[i], r, g, b);

    Plan.LumaPal[i] := r*cRedMul + g*cGreenMul + b*cBlueMul;

    Plan.Y2Palette[i][0] := r;
    Plan.Y2Palette[i][1] := g;
    Plan.Y2Palette[i][2] := b;
    Plan.Y2Palette[i][3] := Plan.LumaPal[i] div cLumaDiv;
  end
end;

procedure TTilingEncoder.TerminatePlan(var Plan: TMixingPlan);
begin
  SetLength(Plan.LumaPal, 0);
  SetLength(Plan.Y2Palette, 0);
end;

function PlanCompareLuma(Item1,Item2,UserParameter:Pointer):Integer;
var
  pi1, pi2: PInteger;
begin
  pi1 := PInteger(UserParameter);
  pi2 := PInteger(UserParameter);

  Inc(pi1, PByte(Item1)^);
  Inc(pi2, PByte(Item2)^);

  Result := CompareValue(pi1^, pi2^);
end;

class function TTilingEncoder.ColorCompare(r1, g1, b1, r2, g2, b2: Int64): Int64;
var
  luma1, luma2, lumadiff, diffR, diffG, diffB: Int64;
begin
  luma1 := r1 * cRedMul + g1 * cGreenMul + b1 * cBlueMul;
  luma2 := r2 * cRedMul + g2 * cGreenMul + b2 * cBlueMul;
  lumadiff := (luma1 - luma2) div cLumaDiv;
  diffR := r1 - r2;
  diffG := g1 - g2;
  diffB := b1 - b2;
  Result := (diffR * diffR) * cRGBw;
  Result += (diffG * diffG) * cRGBw;
  Result += (diffB * diffB) * cRGBw;
  Result += (lumadiff * lumadiff) shl 5;
end;

function TTilingEncoder.DeviseBestMixingPlanYliluoma(var Plan: TMixingPlan; col: Integer; var List: array of Byte): Integer;
label
  pal_loop, inner_loop, worst;
var
  r, g, b: Integer;
  t, index, max_test_count, plan_count, y2pal_len: Integer;
  chosen_amount, chosen, least_penalty, penalty: Int64;
  so_far, sum, add: array[0..3] of Integer;
  VecInv: PCardinal;
  y2pal: PInteger;
  cachePos: Integer;
  pb: PByte;
begin
  FromRGB(col, r, g, b);

{$if defined(ASM_DBMP) and defined(CPUX86_64)}
  asm
    sub rsp, 16 * 6
    movdqu oword ptr [rsp + $00], xmm1
    movdqu oword ptr [rsp + $10], xmm2
    movdqu oword ptr [rsp + $20], xmm3
    movdqu oword ptr [rsp + $30], xmm4
    movdqu oword ptr [rsp + $40], xmm5
    movdqu oword ptr [rsp + $50], xmm6

    push rax
    push rbx
    push rcx
    push rdx

    mov eax, r
    mov ebx, g
    mov ecx, b

    pinsrd xmm4, eax, 0
    pinsrd xmm4, ebx, 1
    pinsrd xmm4, ecx, 2

    imul eax, cRedMul
    imul ebx, cGreenMul
    imul ecx, cBlueMul

    add eax, ebx
    add eax, ecx
    mov ecx, cLumaDiv
    xor edx, edx
    div ecx

    pinsrd xmm4, eax, 3

    mov rax, 1 or (1 shl 32)
    pinsrq xmm5, rax, 0
    pinsrq xmm5, rax, 1

    mov rax, cRGBw or (cRGBw shl 32)
    pinsrq xmm6, rax, 0
    mov rax, cRGBw or (32 shl 32)
    pinsrq xmm6, rax, 1

    pop rdx
    pop rcx
    pop rbx
    pop rax
  end;
{$endif}

  VecInv := @FVecInv[0];
  plan_count := 0;
  so_far[0] := 0; so_far[1] := 0; so_far[2] := 0; so_far[3] := 0;

  while plan_count < Plan.Y2MixedColors do
  begin
    max_test_count := IfThen(plan_count = 0, 1, plan_count);

{$if defined(ASM_DBMP) and defined(CPUX86_64)}
    y2pal_len := Length(Plan.Y2Palette);
    y2pal := @Plan.Y2Palette[0][0];

    asm
      push rax
      push rbx
      push rcx
      push rdx
      push rsi
      push rdi
      push r8
      push r9
      push r10

      xor r9, r9
      xor r10, r10
      inc r10

      mov rbx, (1 shl 63) - 1

      mov rdi, y2pal
      mov r8d, dword ptr [y2pal_len]
      shl r8d, 4
      add r8, rdi

      pal_loop:

        movdqu xmm1, oword ptr [so_far]
        movdqu xmm2, oword ptr [rdi]

        mov ecx, plan_count
        inc rcx
        mov edx, max_test_count
        shl rcx, 4
        shl rdx, 4
        add rcx, VecInv
        add rdx, rcx

        inner_loop:
          paddd xmm1, xmm2
          paddd xmm2, xmm5

          movdqu xmm3, oword ptr [rcx]

          pmulld xmm3, xmm1
          psrld xmm3, cVecInvWidth

          psubd xmm3, xmm4
          pmulld xmm3, xmm3
          pmulld xmm3, xmm6

          phaddd xmm3, xmm3
          phaddd xmm3, xmm3
          pextrd eax, xmm3, 0

          cmp rax, rbx
          jae worst

            mov rbx, rax
            mov r9, rdi
            mov r10, rcx

          worst:

        add rcx, 16
        cmp rcx, rdx
        jne inner_loop

      add rdi, 16
      cmp rdi, r8
      jne pal_loop

      sub r9, y2pal
      shr r9, 4
      mov chosen, r9

      sub r10, VecInv
      shr r10, 4
      sub r10d, plan_count
      mov chosen_amount, r10

      pop r10
      pop r9
      pop r8
      pop rdi
      pop rsi
      pop rdx
      pop rcx
      pop rbx
      pop rax
    end ['rax', 'rbx', 'rcx', 'rdx', 'rsi', 'rdi', 'r8', 'r9', 'r10'];
{$else}
    chosen_amount := 1;
    chosen := 0;

    least_penalty := High(Int64);

    for index := 0 to High(Plan.Y2Palette) do
    begin
      sum[0] := so_far[0]; sum[1] := so_far[1]; sum[2] := so_far[2]; sum[3] := so_far[3];
      add[0] := Plan.Y2Palette[index][0]; add[1] := Plan.Y2Palette[index][1]; add[2] := Plan.Y2Palette[index][2]; add[3] := Plan.Y2Palette[index][3];

      for t := plan_count + 1 to plan_count + max_test_count do
      begin
        sum[0] += add[0];
        sum[1] += add[1];
        sum[2] += add[2];

        Inc(add[0]);
        Inc(add[1]);
        Inc(add[2]);

        penalty := ColorCompare(r, g, b, sum[0] div t, sum[1] div t, sum[2] div t);

        if penalty < least_penalty then
        begin
          least_penalty := penalty;
          chosen := index;
          chosen_amount := t - plan_count;
        end;
      end;
    end;
{$endif}

    chosen_amount := Min(chosen_amount, Length(List) - plan_count);
    FillByte(List[plan_count], chosen_amount, chosen);
    Inc(plan_count, chosen_amount);

    so_far[0] += Plan.Y2Palette[chosen][0] * chosen_amount;
    so_far[1] += Plan.Y2Palette[chosen][1] * chosen_amount;
    so_far[2] += Plan.Y2Palette[chosen][2] * chosen_amount;
    so_far[3] += Plan.Y2Palette[chosen][3] * chosen_amount;
  end;

  QuickSort(List[0], 0, plan_count - 1, SizeOf(Byte), @PlanCompareLuma, @Plan.LumaPal[0]);

  Result := plan_count;

{$if defined(ASM_DBMP) and defined(CPUX86_64)}
  asm
    movdqu xmm1, oword ptr [rsp + $00]
    movdqu xmm2, oword ptr [rsp + $10]
    movdqu xmm3, oword ptr [rsp + $20]
    movdqu xmm4, oword ptr [rsp + $30]
    movdqu xmm5, oword ptr [rsp + $40]
    movdqu xmm6, oword ptr [rsp + $50]
    add rsp, 16 * 6
  end;
{$endif}
end;

procedure TTilingEncoder.DeviseBestMixingPlanThomasKnoll(var Plan: TMixingPlan; col: Integer; var List: array of Byte);
var
  index, chosen, c: Integer;
  src : array[0..2] of Byte;
  s, t, e: array[0..2] of Int64;
  least_penalty, penalty: Int64;
begin
  FromRGB(col, src[0], src[1], src[2]);

  s[0] := src[0];
  s[1] := src[1];
  s[2] := src[2];

  e[0] := 0;
  e[1] := 0;
  e[2] := 0;

  for c := 0 to cDitheringLen - 1 do
  begin
    t[0] := s[0] + (e[0] * 9) div 100;
    t[1] := s[1] + (e[1] * 9) div 100;
    t[2] := s[2] + (e[2] * 9) div 100;

    least_penalty := High(Int64);
    chosen := c mod length(Plan.Y2Palette);
    for index := 0 to length(Plan.Y2Palette) - 1 do
    begin
      penalty := ColorCompare(t[0], t[1], t[2], Plan.Y2Palette[index][0], Plan.Y2Palette[index][1], Plan.Y2Palette[index][2]);
      if penalty < least_penalty then
      begin
        least_penalty := penalty;
        chosen := index;
      end;
    end;

    List[c] := chosen;

    e[0] += s[0];
    e[1] += s[1];
    e[2] += s[2];

    e[0] -= Plan.Y2Palette[chosen][0];
    e[1] -= Plan.Y2Palette[chosen][1];
    e[2] -= Plan.Y2Palette[chosen][2];
  end;

  QuickSort(List[0], 0, cDitheringLen - 1, SizeOf(Byte), @PlanCompareLuma, @Plan.LumaPal[0]);
end;

function TTilingEncoder.GetTileCount(AActiveOnly: Boolean): Integer;
var
  tidx: Int64;
begin
  if AActiveOnly then
  begin
   Result := 0;
    for tidx := 0 to High(Tiles) do
      if Tiles[tidx]^.Active then
        Inc(Result);
  end
  else
  begin
    Result := Length(Tiles);
  end;
end;

procedure TTilingEncoder.ReframeUI(AWidth, AHeight: Integer);
begin
  FTileMapWidth := AWidth;
  FTileMapHeight := AHeight;

  FTileMapSize := FTileMapWidth * FTileMapHeight;
  FScreenWidth := FTileMapWidth * cTileWidth;
  FScreenHeight := FTileMapHeight * cTileWidth;

  FInputBitmap.Width:=FScreenWidth;
  FInputBitmap.Height:=FScreenHeight;
  FInputBitmap.PixelFormat:=pf32bit;

  FOutputBitmap.Width:=FScreenWidth;
  FOutputBitmap.Height:=FScreenHeight;
  FOutputBitmap.PixelFormat:=pf32bit;

  FTilesBitmap.Width:=FScreenWidth;
  FTilesBitmap.Height:=FScreenHeight;
  FTilesBitmap.PixelFormat:=pf32bit;

  FPaletteBitmap.Width := FPaletteSize;
  FPaletteBitmap.Height := FPaletteCount;
  FPaletteBitmap.PixelFormat:=pf32bit;
end;

procedure TTilingEncoder.InitFrames(AFrameCount: Integer);
var
  frmIdx, sx, sy: Integer;
  Frame: TFrame;
  TMI: PTileMapItem;
begin
  SetLength(FFrames, AFrameCount);

  for frmIdx := 0 to High(FFrames) do
  begin
    Frame := TFrame.Create(Self, frmIdx);

    SetLength(Frame.TileMap, FTileMapHeight, FTileMapWidth);

    for sy := 0 to FTileMapHeight - 1 do
      for sx := 0 to FTileMapWidth - 1 do
      begin
        TMI := @Frame.TileMap[sy, sx];

        TMI^.TileIdx := -1;
        TMI^.PalIdx := -1;
      end;

    FFrames[frmIdx] := Frame;
  end;
end;

procedure TTilingEncoder.DitherTile(var ATile: TTile; var Plan: TMixingPlan);
var
  x, y: Integer;
  count, map_value: Integer;
  TKList: array[0 .. cDitheringLen - 1] of Byte;
  YilList: array[0 .. cDitheringListLen - 1] of Byte;
begin
  // put tile back in its natural mirrors for ordered dithering to work properly
  if ATile.HMirror_Initial then HMirrorTile(ATile);
  if ATile.VMirror_Initial then VMirrorTile(ATile);
  try
    if FDitheringUseThomasKnoll then
    begin
      for y := 0 to (cTileWidth - 1) do
        for x := 0 to (cTileWidth - 1) do
        begin
          map_value := cDitheringMap[((y and 7) shl 3) or (x and 7)];
          DeviseBestMixingPlanThomasKnoll(Plan, ATile.RGBPixels[y, x], TKList);
          ATile.PalPixels[y, x] := TKList[map_value];
        end;
    end
    else
    begin
      for y := 0 to (cTileWidth - 1) do
        for x := 0 to (cTileWidth - 1) do
        begin
          map_value := cDitheringMap[((y and 7) shl 3) or (x and 7)];
          count := DeviseBestMixingPlanYliluoma(Plan, ATile.RGBPixels[y, x], YilList);
          map_value := (map_value * count) shr 6;
          ATile.PalPixels[y, x] := YilList[map_value];
        end;
    end;
  finally
    if ATile.HMirror_Initial then HMirrorTile(ATile);
    if ATile.VMirror_Initial then VMirrorTile(ATile);
  end;
end;

procedure TTilingEncoder.RGBToYUV(col: Integer; GammaCor: Integer; out y, u, v: TFloat); inline;
var
  yy, uu, vv: TFloat;
  r, g, b: Byte;
begin
  FromRGB(col, r, g, b);
  RGBToYUV(r, g, b, GammaCor, yy, uu, vv);
  y := yy; u := uu; v := vv; // for safe "out" param
end;

procedure TTilingEncoder.RGBToYUV(r, g, b: Byte; GammaCor: Integer; out y, u, v: TFloat);
var
  fr, fg, fb: TFloat;
  yy, uu, vv: TFloat;
begin
  fr := GammaCorrect(GammaCor, r);
  fg := GammaCorrect(GammaCor, g);
  fb := GammaCorrect(GammaCor, b);

  yy := fr * (cRedMul / cLumaDiv) + fg * (cGreenMul / cLumaDiv) + fb * (cBlueMul / cLumaDiv);
  uu := (fb - yy) * 0.492;
  vv := (fr - yy) * 0.877;
{$if cRedMul <> 299}
  {$error RGBToYUV should be changed!}
{$endif}

  y := yy; u := uu; v := vv; // for safe "out" param
end;

function TTilingEncoder.YUVToRGB(y, u, v: TFloat; GammaCor: Integer): Integer;
var
  r, g, b: TFloat;
begin
{$if cRedMul = 299}
  r := y + v * 1.13983;
  g := y - u * 0.39465 - v * 0.58060;
  b := y + u * 2.03211;
{$elseif cRedMul = 2126}
  r := y + v * 1.28033;
  g := y - u * 0.21482 - v * 0.38059;
  b := y + u * 2.12798;
{$else}
  {$error YUVToRGB not implemented!}
{$endif}

  Result := ToRGB(GammaUncorrect(GammaCor, r), GammaUncorrect(GammaCor, g), GammaUncorrect(GammaCor, b));
end;

procedure TTilingEncoder.RGBToHSV(col: Integer; out h, s, v: TFloat);
var
  bh, bs, bv: Byte;
begin
  bh := 0; bs := 0; bv := 0;
  RGBToHSV(col, bh, bs, bv);
  h := bh / 255.0;
  s := bs / 255.0;
  v := bv / 255.0;
end;

procedure TTilingEncoder.RGBToLAB(ir, ig, ib: Integer; GammaCor: Integer; out ol, oa, ob: TFloat); inline;
var
  r, g, b, x, y, z: TFloat;
begin
  r := GammaCorrect(GammaCor, ir);
  g := GammaCorrect(GammaCor, ig);
  b := GammaCorrect(GammaCor, ib);

  if r > 0.04045 then r := power((r + 0.055) / 1.055, 2.4) else r := r / 12.92;
  if g > 0.04045 then g := power((g + 0.055) / 1.055, 2.4) else g := g / 12.92;
  if b > 0.04045 then b := power((b + 0.055) / 1.055, 2.4) else b := b / 12.92;

  // CIE XYZ color space from the Wright–Guild data
  x := (r * 0.49000 + g * 0.31000 + b * 0.20000) / 0.17697;
  y := (r * 0.17697 + g * 0.81240 + b * 0.01063) / 0.17697;
  z := (r * 0.00000 + g * 0.01000 + b * 0.99000) / 0.17697;

{$if True}
  // Illuminant D50
  x *= 1 / (96.6797 / 100);
  y *= 1 / (100.000 / 100);
  z *= 1 / (82.5188 / 100);
{$else}
  // Illuminant D65
  x *= 1 / (95.0470 / 100);
  y *= 1 / (100.000 / 100);
  z *= 1 / (108.883 / 100);
{$endif}

  if x > 0.008856 then x := power(x, 1/3) else x := (7.787 * x) + 16/116;
  if y > 0.008856 then y := power(y, 1/3) else y := (7.787 * y) + 16/116;
  if z > 0.008856 then z := power(z, 1/3) else z := (7.787 * z) + 16/116;

  ol := (116 * y) - 16;
  oa := 500 * (x - y);
  ob := 200 * (y - z);
end;

procedure TTilingEncoder.RGBToLAB(r, g, b: TFloat; GammaCor: Integer; out ol, oa, ob: TFloat); inline;
var
  ll, aa, bb: TFloat;
begin
  RGBToLAB(Integer(round(r * 255.0)), round(g * 255.0), round(b * 255.0), GammaCor, ll, aa, bb);
  ol := ll;
  oa := aa;
  ob := bb;
end;

function TTilingEncoder.LABToRGB(ll, aa, bb: TFloat; GammaCor: Integer): Integer;
var
  x, y, z, r, g, b: TFloat;
begin
  y := (ll + 16) / 116;
  x := aa / 500 + y;
  z := y - bb / 200;

  if IntPower(y, 3) > 0.008856 then
    y := IntPower(y, 3)
  else
    y := (y - 16 / 116) / 7.787;
  if IntPower(x, 3) > 0.008856 then
    x := IntPower(x, 3)
  else
    x := (x - 16 / 116) / 7.787;
  if IntPower(z, 3) > 0.008856 then
    z := IntPower(z, 3)
  else
    z := (z - 16 / 116) / 7.787;

  // Illuminant D50
  x := 96.6797 / 100 * x;
  y := 100.000 / 100 * y;
  z := 82.5188 / 100 * z;

  r := x * 0.41847 + y * (-0.15866) + z * (-0.082835);
  g := x * (-0.091169) + y * 0.25243 + z * 0.015708;
  b := x * 0.00092090 + y * (-0.0025498) + z * 0.17860;

  if r > 0.0031308 then
    r := 1.055 * Power(r, 1 / 2.4) - 0.055
  else
    r := 12.92 * r;
  if g > 0.0031308 then
    g := 1.055 * Power(g, 1 / 2.4) - 0.055
  else
    g := 12.92 * g;
  if b > 0.0031308 then
    b := 1.055 * Power(b, 1 / 2.4) - 0.055
  else
    b := 12.92 * b;

  Result := ToRGB(GammaUncorrect(GammaCor, r), GammaUncorrect(GammaCor, g), GammaUncorrect(GammaCor, b));
end;

procedure TTilingEncoder.SetDitheringYliluoma2MixedColors(AValue: Integer);
begin
  if FDitheringYliluoma2MixedColors = AValue then Exit;
  FDitheringYliluoma2MixedColors := EnsureRange(AValue, 1, 16);
end;

procedure TTilingEncoder.SetEncoderGammaValue(AValue: Double);
begin
  if FEncoderGammaValue = AValue then Exit;
  FEncoderGammaValue := Max(0.0, AValue);

  FGamma[0] := FEncoderGammaValue;
  InitLuts;
end;

procedure TTilingEncoder.SetFrameCountSetting(AValue: Integer);
begin
  if FFrameCountSetting = AValue then Exit;
  FFrameCountSetting := Max(0, AValue);
end;

procedure TTilingEncoder.SetFramesPerSecond(AValue: Double);
begin
  if FFramesPerSecond = AValue then Exit;
  FFramesPerSecond := Max(0.0, AValue);
end;

procedure TTilingEncoder.SetGlobalTilingQualityBasedTileCount(AValue: Double);
var
  eqtc, RawTileCount: Int64;
begin
  if FGlobalTilingQualityBasedTileCount = AValue then Exit;
  FGlobalTilingQualityBasedTileCount := AValue;

  eqtc := EqualQualityTileCount(FrameCount * FTileMapSize);

  RawTileCount := Length(FFrames) * FTileMapSize;
  FGlobalTilingTileCount := min(round(AValue * eqtc), RawTileCount);
end;

procedure TTilingEncoder.SetGlobalTilingRatio(AValue: Double);
begin
 if FGlobalTilingRatio = AValue then Exit;
 FGlobalTilingRatio := EnsureRange(AValue, 0.0, 1.0);
end;

procedure TTilingEncoder.SetMaxThreadCount(AValue: Integer);
begin
 if ProcThreadPool.MaxThreadCount = AValue then Exit;
 ProcThreadPool.MaxThreadCount := EnsureRange(AValue, 1, NumberOfProcessors);
end;

procedure TTilingEncoder.SetPaletteCount(AValue: Integer);
begin
  if FPaletteCount = AValue then Exit;
  FPaletteCount := EnsureRange(AValue, 1, 256);
end;

procedure TTilingEncoder.SetPaletteSize(AValue: Integer);
begin
  if FPaletteSize = AValue then Exit;
  FPaletteSize := EnsureRange(AValue, 2, 64);
end;

procedure TTilingEncoder.SetGlobalTilingTileCount(AValue: Integer);
var
  RawTileCount: Integer;
begin
  if FGlobalTilingTileCount = AValue then Exit;
  FGlobalTilingTileCount := AValue;

  RawTileCount := Length(FFrames) * FTileMapSize;
  if RawTileCount <> 0 then
    FGlobalTilingTileCount := EnsureRange(FGlobalTilingTileCount, 0, RawTileCount)
  else
    FGlobalTilingTileCount := max(FGlobalTilingTileCount, 0);
end;

procedure TTilingEncoder.SetScaling(AValue: Double);
begin
  if FScaling = AValue then Exit;
  FScaling := Max(0.01, AValue);
end;

procedure TTilingEncoder.SetShotTransCorrelLoThres(AValue: Double);
begin
 if FShotTransCorrelLoThres = AValue then Exit;
 FShotTransCorrelLoThres := EnsureRange(AValue, -1.0, 1.0);
end;

procedure TTilingEncoder.SetShotTransMaxSecondsPerKF(AValue: Double);
begin
 if FShotTransMaxSecondsPerKF = AValue then Exit;
 FShotTransMaxSecondsPerKF := max(0.0, AValue);
end;

procedure TTilingEncoder.SetShotTransMinSecondsPerKF(AValue: Double);
begin
 if FShotTransMinSecondsPerKF = AValue then Exit;
 FShotTransMinSecondsPerKF := max(0.0, AValue);
end;

procedure TTilingEncoder.SetSmoothingFactor(AValue: Double);
begin
  if FSmoothingFactor = AValue then Exit;
  FSmoothingFactor := Max(0.0, AValue);
end;

procedure TTilingEncoder.SetStartFrame(AValue: Integer);
begin
  if FStartFrame = AValue then Exit;
  FStartFrame := Max(0, AValue);
end;

procedure TTilingEncoder.SetRenderFrameIndex(AValue: Integer);
begin
  if FRenderFrameIndex = AValue then Exit;
  FRenderFrameIndex := EnsureRange(AValue, 0, High(FFrames));
end;

procedure TTilingEncoder.SetRenderGammaValue(AValue: Double);
begin
  if FRenderGammaValue = AValue then Exit;
  FRenderGammaValue := Max(0.0, AValue);

  FGamma[1] := FRenderGammaValue;
  InitLuts;
end;

procedure TTilingEncoder.SetRenderPaletteIndex(AValue: Integer);
begin
  if FRenderPaletteIndex = AValue then Exit;
  FRenderPaletteIndex := EnsureRange(AValue, -1, FPaletteCount - 1);
end;

procedure TTilingEncoder.SetRenderTilePage(AValue: Integer);
begin
  if FRenderTilePage = AValue then Exit;
  FRenderTilePage := Max(0, AValue);
end;

procedure TTilingEncoder.SetQuantizerDennisLeeBitsPerComponent(AValue: Integer);
begin
  if FQuantizerDennisLeeBitsPerComponent = AValue then Exit;
  FQuantizerDennisLeeBitsPerComponent := EnsureRange(AValue, 2, 8);
end;

procedure TTilingEncoder.SetQuantizerPosterizeBitsPerComponent(AValue: Integer);
begin
 if FQuantizerPosterizeBitsPerComponent = AValue then Exit;
 FQuantizerPosterizeBitsPerComponent := EnsureRange(AValue, 1, 8);
end;

generic function DCTInner<T>(pCpn, pLut: T; count: Integer): Double;
var
  i: integer;
begin
  Result := 0;

  for i := 0 to count- 1 do
  begin
    // unroll y by cTileWidth

    // unroll x by cTileWidth
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);

    // unroll x by cTileWidth
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);

    // unroll x by cTileWidth
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);

    // unroll x by cTileWidth
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);

    // unroll x by cTileWidth
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);

    // unroll x by cTileWidth
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);

    // unroll x by cTileWidth
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);

    // unroll x by cTileWidth
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
    Result += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
  end;
end;

procedure TTilingEncoder.ComputeTilePsyVisFeatures(const ATile: TTile; FromPal, UseLAB, QWeighting, HMirror,
  VMirror: Boolean; ColorCpns, GammaCor: Integer; const pal: TIntegerDynArray; DCT: PFloat);
var
  u, v, x, y, xx, yy, cpn: Integer;
  z: Double;
  CpnPixels: TCpnPixels;
  pDCT, pLut: PFloat;

  procedure ToCpn(col, x, y: Integer); inline;
  var
    r, g, b: Byte;
    yy, uu, vv: TFloat;
  begin
    FromRGB(col, r, g, b);

    if UseLAB then
    begin
      RGBToLAB(r, g, b, GammaCor, yy, uu, vv)
    end
    else
    begin
      RGBToYUV(r, g, b, GammaCor, yy, uu, vv);
    end;

    CpnPixels[0, y, x] := yy;
    CpnPixels[1, y, x] := uu;
    CpnPixels[2, y, x] := vv;
  end;

begin
  if FromPal then
  begin
    for y := 0 to (cTileWidth - 1) do
      for x := 0 to (cTileWidth - 1) do
      begin
        xx := x;
        yy := y;
        if HMirror then xx := cTileWidth - 1 - x;
        if VMirror then yy := cTileWidth - 1 - y;

        ToCpn(pal[ATile.PalPixels[yy,xx]], x, y);
      end;
  end
  else
  begin
    for y := 0 to (cTileWidth - 1) do
      for x := 0 to (cTileWidth - 1) do
      begin
        xx := x;
        yy := y;
        if HMirror then xx := cTileWidth - 1 - x;
        if VMirror then yy := cTileWidth - 1 - y;

        ToCpn(ATile.RGBPixels[yy,xx], x, y);
      end;
  end;

  for cpn := 0 to ColorCpns - 1 do
  begin
    pDCT := @DCT[cpn];
    pLut := @FDCTLut[0];
    for v := 0 to cTileWidth * cDCTFeaturesMul - 1 do
      for u := 0 to cTileWidth * cDCTFeaturesMul - 1 do
      begin
  		  z := specialize DCTInner<PSingle>(@CpnPixels[cpn, 0, 0], pLut, 1);

        if QWeighting then
           z *= cDCTQuantization[cpn, v div cDCTFeaturesMul, u div cDCTFeaturesMul];

        pDCT^ := z;
        Inc(pDCT, ColorCpns);
        Inc(pLut, Sqr(cTileWidth));
      end;
  end;
end;

procedure TTilingEncoder.ComputeTilePsyVisFeatures(const ATile: TTile; FromPal, UseLAB, QWeighting, HMirror,
  VMirror: Boolean; ColorCpns, GammaCor: Integer; const pal: TIntegerDynArray; DCT: PDouble);
var
  u, v, x, y, xx, yy, cpn: Integer;
  z: Double;
  CpnPixels: TCpnPixelsDouble;
  pDCT, pLut: PDouble;

  procedure ToCpn(col, x, y: Integer); inline;
  var
    r, g, b: Byte;
    yy, uu, vv: TFloat;
  begin
    FromRGB(col, r, g, b);

    if UseLAB then
    begin
      RGBToLAB(r, g, b, GammaCor, yy, uu, vv)
    end
    else
    begin
      RGBToYUV(r, g, b, GammaCor, yy, uu, vv);
    end;

    CpnPixels[0, y, x] := yy;
    CpnPixels[1, y, x] := uu;
    CpnPixels[2, y, x] := vv;
  end;

begin
  if FromPal then
  begin
    for y := 0 to (cTileWidth - 1) do
      for x := 0 to (cTileWidth - 1) do
      begin
        xx := x;
        yy := y;
        if HMirror then xx := cTileWidth - 1 - x;
        if VMirror then yy := cTileWidth - 1 - y;

        ToCpn(pal[ATile.PalPixels[yy,xx]], x, y);
      end;
  end
  else
  begin
    for y := 0 to (cTileWidth - 1) do
      for x := 0 to (cTileWidth - 1) do
      begin
        xx := x;
        yy := y;
        if HMirror then xx := cTileWidth - 1 - x;
        if VMirror then yy := cTileWidth - 1 - y;

        ToCpn(ATile.RGBPixels[yy,xx], x, y);
      end;
  end;

  for cpn := 0 to ColorCpns - 1 do
  begin
    pDCT := @DCT[cpn];
    pLut := @FDCTLutDouble[0];
    for v := 0 to cTileWidth * cDCTFeaturesMul - 1 do
      for u := 0 to cTileWidth * cDCTFeaturesMul - 1 do
      begin
  		  z := specialize DCTInner<PDouble>(@CpnPixels[cpn, 0, 0], pLut, 1);

        if QWeighting then
           z *= cDCTQuantization[cpn, v div cDCTFeaturesMul, u div cDCTFeaturesMul];

        pDCT^ := z;
        Inc(pDCT, ColorCpns);
        Inc(pLut, Sqr(cTileWidth));
      end;
  end;
end;

procedure TTilingEncoder.ComputeInvTilePsyVisFeatures(DCT: PDouble; UseLAB, QWeighting: Boolean; ColorCpns, GammaCor: Integer;
 var ATile: TTile);
var
  u, v, x, y, cpn: Integer;
  CpnPixels: TCpnPixelsDouble;
  pCpn, pLut, pCur, pWDCT: PDouble;


  function FromCpn(x, y: Integer): Integer; inline;
  var
    yy, uu, vv: TFloat;
  begin
    yy := CpnPixels[0, y, x];
    uu := CpnPixels[1, y, x];
    vv := CpnPixels[2, y, x];

    if UseLAB then
      Result := LABToRGB(yy, uu, vv, GammaCor)
    else
      Result := YUVToRGB(yy, uu, vv, GammaCor);
  end;

begin
  pWDCT := GetMem(cTileDCTSize * SizeOf(Double));
  for cpn := 0 to ColorCpns - 1 do
  begin
    pCur := @DCT[cpn];
    for v := 0 to cTileWidth * cDCTFeaturesMul - 1 do
      for u := 0 to cTileWidth * cDCTFeaturesMul - 1 do
      begin
        if QWeighting then
          pWDCT^ := pCur^ / cDCTQuantization[cpn, v div cDCTFeaturesMul, u div cDCTFeaturesMul]
        else
          pWDCT^ := pCur^;
        Inc(pWDCT);
        Inc(pCur, ColorCpns);
      end;
  end;
  Dec(pWDCT, cTileDCTSize);

  for cpn := 0 to ColorCpns - 1 do
  begin
    pCpn := @CpnPixels[cpn, 0, 0];
    pLut := @FInvDCTLutDouble[0];

    for y := 0 to cTileWidth - 1 do
      for x := 0 to cTileWidth - 1 do
      begin
        pCpn^ := specialize DCTInner<PDouble>(@pWDCT[cpn * (cTileDCTSize div ColorCpns)], pLut, sqr(cDCTFeaturesMul));
        Inc(pCpn);
        Inc(pLut, Sqr(cTileWidth * cDCTFeaturesMul));
      end;
  end;

  Freemem(pWDCT);

  for y := 0 to (cTileWidth - 1) do
    for x := 0 to (cTileWidth - 1) do
      ATile.RGBPixels[y, x] := FromCpn(x, y);
end;

class procedure TTilingEncoder.VMirrorTile(var ATile: TTile; APalOnly: Boolean);
var
  j, i: Integer;
  v, sv: Integer;
begin
  // hardcode vertical mirror into the tile

  for j := 0 to cTileWidth div 2 - 1  do
    for i := 0 to cTileWidth - 1 do
    begin
      if ATile.HasPalPixels then
      begin
        v := ATile.PalPixels[j, i];
        sv := ATile.PalPixels[cTileWidth - 1 - j, i];
        ATile.PalPixels[j, i] := sv;
        ATile.PalPixels[cTileWidth - 1 - j, i] := v;
      end;

      if ATile.HasRGBPixels and not APalOnly then
      begin
        v := ATile.RGBPixels[j, i];
        sv := ATile.RGBPixels[cTileWidth - 1 - j, i];
        ATile.RGBPixels[j, i] := sv;
        ATile.RGBPixels[cTileWidth - 1 - j, i] := v;
      end;
    end;
end;

class procedure TTilingEncoder.HMirrorTile(var ATile: TTile; APalOnly: Boolean);
var
  i, j: Integer;
  v, sv: Integer;
begin
  // hardcode horizontal mirror into the tile

  for j := 0 to cTileWidth - 1 do
    for i := 0 to cTileWidth div 2 - 1  do
    begin
      if ATile.HasPalPixels then
      begin
        v := ATile.PalPixels[j, i];
        sv := ATile.PalPixels[j, cTileWidth - 1 - i];
        ATile.PalPixels[j, i] := sv;
        ATile.PalPixels[j, cTileWidth - 1 - i] := v;
      end;

      if ATile.HasRGBPixels and not APalOnly then
      begin
        v := ATile.RGBPixels[j, i];
        sv := ATile.RGBPixels[j, cTileWidth - 1 - i];
        ATile.RGBPixels[j, i] := sv;
        ATile.RGBPixels[j, cTileWidth - 1 - i] := v;
      end;
    end;
end;

procedure TTilingEncoder.LoadFrameTilesFromImage(var AFrame: TFrame; AImageWidth, AImageHeight: Integer; AImage: PInteger);
var
  i, j, col, ti, tx, ty: Integer;
  HMirror, VMirror: Boolean;
  pcol: PInteger;
  Tile: PTile;
begin
  // create frame tiles from image data

  AFrame.FrameTiles := TTile.Array1DNew(FTileMapSize, True, True);

  pcol := PInteger(AImage);
  for j := 0 to AImageHeight - 1 do
  begin
    for i := 0 to AImageWidth - 1 do
      begin
        col := pcol^;
        Inc(pcol);

        if (j < FScreenHeight) and (i < FScreenWidth) then
        begin
          ti := FTileMapWidth * (j shr cTileWidthBits) + (i shr cTileWidthBits);
          tx := i and (cTileWidth - 1);
          ty := j and (cTileWidth - 1);
          col := SwapRB(col);

          AFrame.FrameTiles[ti]^.RGBPixels[ty, tx] := col;
        end;
      end;
  end;

  for i := 0 to FTileMapSize - 1 do
  begin
    Tile := AFrame.FrameTiles[i];

    GetTileHVMirrorHeuristics(Tile^, HMirror, VMirror);

    Tile^.Active := True;
    Tile^.UseCount := 1;
    Tile^.TmpIndex := -1;
    Tile^.HMirror_Initial := HMirror;
    Tile^.VMirror_Initial := VMirror;

    if HMirror then
      HMirrorTile(Tile^);

    if VMirror then
      VMirrorTile(Tile^);
  end;
end;

procedure TTilingEncoder.FindKeyFrames(AManualMode: Boolean);
var
  Correlations: TFloatDynArray;
  doneFrameCount: Integer;
  dummyKeyFrames: array of TKeyFrame;
  dummyKFEndFrames: TFloatDynArray2;

  procedure DoCorrel(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    frmIdx: Integer;
    KeyFrame: TKeyFrame;
    FrameData: TFloatDynArray2;
    prevFrameData: TFloatDynArray;
  begin
    if not InRange(AIndex, 0, High(dummyKeyFrames)) then
      Exit;

    KeyFrame := dummyKeyFrames[AIndex];

    KeyFrame.AcquireFrameTiles;
    try
      SetLength(FrameData, KeyFrame.FrameCount);

      // reverse order to make end Frame ready for next frame asap
      for frmIdx := KeyFrame.EndFrame downto KeyFrame.StartFrame do
      begin
        FrameData[frmIdx - KeyFrame.StartFrame] := PrepareInterFrameCorrelation(FFrames[frmIdx]);
        if frmIdx = KeyFrame.EndFrame then
          dummyKFEndFrames[AIndex] := FrameData[frmIdx - KeyFrame.StartFrame];
      end;

      for frmIdx := Max(1, KeyFrame.StartFrame) to KeyFrame.EndFrame do
      begin
        if frmIdx > KeyFrame.StartFrame then
        begin
          prevFrameData := FrameData[frmIdx - 1 - KeyFrame.StartFrame];
        end
        else
        begin
          // wait for prev KeyFrame to compute end Frame
          while not Assigned(dummyKFEndFrames[AIndex - 1]) do
            Sleep(10);

          prevFrameData := dummyKFEndFrames[AIndex - 1];
          dummyKFEndFrames[AIndex - 1] := nil;
        end;

        Correlations[frmIdx] := PearsonCorrelation(prevFrameData, FrameData[frmIdx - KeyFrame.StartFrame]);
      end;
    finally
      KeyFrame.ReleaseFrameTiles;

      Write(InterLockedExchangeAdd(doneFrameCount, KeyFrame.FrameCount) + KeyFrame.FrameCount:8, ' / ', Length(FFrames):8, #13);
    end;
  end;

var
  frmIdx, kfIdx, lastKFIdx, dummyKFLen: Integer;
  correl: TFloat;
  kfReason: TKeyFrameReason;
  sfr, efr: Integer;
begin
  // compute interframe correlations

  doneFrameCount := 0;
  SetLength(Correlations, Length(FFrames));
  if not AManualMode then
  begin
    // needs dummy KeyFrames to be able to do AcquireFrameTiles / ReleaseFrameTiles

    dummyKFLen := Round(FFramesPerSecond * FShotTransMaxSecondsPerKF);

    SetLength(dummyKeyFrames, (Length(FFrames) - 1) div dummyKFLen + 1);
    SetLength(dummyKFEndFrames, Length(dummyKeyFrames));
    for kfIdx := 0 to High(dummyKeyFrames) do
    begin
      dummyKeyFrames[kfIdx] := TKeyFrame.Create(Self, kfIdx, kfIdx * dummyKFLen, min(High(FFrames), (kfIdx + 1) * dummyKFLen - 1));
      for frmIdx := dummyKeyFrames[kfIdx].StartFrame to dummyKeyFrames[kfIdx].EndFrame do
        FFrames[frmIdx].PKeyFrame := dummyKeyFrames[kfIdx];
    end;
    try
      ProcThreadPool.DoParallelLocalProc(@DoCorrel, 0, High(dummyKeyFrames));
    finally
      for frmIdx := 0 to High(FFrames) do
        FFrames[frmIdx].PKeyFrame := nil;

      for kfIdx := 0 to High(dummyKeyFrames) do
        FreeAndNil(dummyKeyFrames[kfIdx]);

      SetLength(dummyKeyFrames, 0);
    end;
  end;

  // find keyframes

  SetLength(FKeyFrames, Length(FFrames));
  kfIdx := 0;
  lastKFIdx := Low(Integer);
  for frmIdx := 0 to High(FFrames) do
  begin
    correl := Correlations[frmIdx];

    kfReason := kfrNone;
    if AManualMode then
    begin
      if FileExists(Format(ChangeFileExt(FLoadedInputPath, '.kf'), [frmIdx + StartFrame])) or (frmIdx = 0) then
        kfReason := kfrManual;
    end
    else
    begin
      if (kfReason = kfrNone) and (frmIdx = 0) then
        kfReason := kfrManual;

      if (kfReason = kfrNone) and (correl < FShotTransCorrelLoThres) then
        kfReason := kfrDecorrelation;

      if (kfReason = kfrNone) and ((frmIdx - lastKFIdx) >= (FShotTransMaxSecondsPerKF * FFramesPerSecond)) then
        kfReason := kfrLength;

      if (frmIdx - lastKFIdx) < (FShotTransMinSecondsPerKF * FFramesPerSecond) then
        kfReason := kfrNone;
    end;

    if kfReason <> kfrNone then
    begin
      FKeyFrames[kfIdx] := TKeyFrame.Create(Self, kfIdx, 0, 0);
      FKeyFrames[kfIdx].Reason := kfReason;

      Inc(kfIdx);

      lastKFIdx := frmIdx;
    end;

    FFrames[frmIdx].PKeyFrame := FKeyFrames[kfIdx - 1];
  end;

  SetLength(FKeyFrames, kfIdx);

  for kfIdx := 0 to High(FKeyFrames) do
  begin
    sfr := High(Integer);
    efr := Low(Integer);

    for frmIdx := 0 to High(FFrames) do
      if FFrames[frmIdx].PKeyFrame = FKeyFrames[kfIdx] then
      begin
        sfr := Min(sfr, frmIdx);
        efr := Max(efr, frmIdx);
      end;

    FKeyFrames[kfIdx].StartFrame := sfr;
    FKeyFrames[kfIdx].EndFrame := efr;
    FKeyFrames[kfIdx].FrameCount := efr - sfr + 1;

    WriteLn('KF: ', FKeyFrames[kfIdx].StartFrame:8, ' (', kfIdx:3, ') FCnt: ', FKeyFrames[kfIdx].FrameCount:3, ' Reason: ', Copy(GetEnumName(TypeInfo(TKeyFrameReason), Ord(FKeyFrames[kfIdx].Reason)), 4));
  end;
end;

procedure TTilingEncoder.ClearAll;
var
  i: Integer;
begin
  for i := 0 to High(FFrames) do
    if Assigned(FFrames[i]) then
      FreeAndNil(FFrames[i]);
  SetLength(FFrames, 0);

  for i := 0 to High(FKeyFrames) do
    if Assigned(FKeyFrames[i]) then
      FreeAndNil(FKeyFrames[i]);
  SetLength(FKeyFrames, 0);

  TTile.Array1DDispose(FTiles);

  FRenderPrevKeyFrame := nil;
end;

procedure TTilingEncoder.Render(AFast: Boolean);

  procedure DrawTile(bitmap: TBitmap; sx, sy: Integer; psyTile: PTile; tilePtr: PTile; pal: TIntegerDynArray; hmir, vmir: Boolean);
  var
    r, g, b, tx, ty, txm, tym, col: Integer;
    psl: PInteger;
  begin
    for ty := 0 to cTileWidth - 1 do
    begin
      psl := bitmap.ScanLine[ty + sy * cTileWidth];
      Inc(psl, sx * cTileWidth);

      tym := ty;
      if vmir then tym := cTileWidth - 1 - tym;

      for tx := 0 to cTileWidth - 1 do
      begin
        txm := tx;
        if hmir then txm := cTileWidth - 1 - txm;

        r := 255; g := 0; b := 255;
        if tilePtr^.Active then
        begin
          if Assigned(pal) then
          begin
            if tilePtr^.HasPalPixels then
              FromRGB(pal[tilePtr^.PalPixels[tym, txm]], r, g, b)
          end
          else
          begin
            if tilePtr^.HasRGBPixels then
              FromRGB(tilePtr^.RGBPixels[tym, txm], r, g, b);
          end;
        end;

        if FRenderUseGamma then
        begin
          r := round(GammaCorrect(1, r) * 255.0);
          g := round(GammaCorrect(1, g) * 255.0);
          b := round(GammaCorrect(1, b) * 255.0);
        end;

        col := ToRGB(r, g, b);
        psl^ := SwapRB(col);

        if Assigned(psyTile) then
          psyTile^.RGBPixels[ty, tx] := col;

        Inc(psl);
      end;
    end;
  end;

var
  i, j, sx, sy, frmIdx, globalTileCount: Integer;
  hmir, vmir: Boolean;
  tidx: Int64;
  p: PInteger;
  tilePtr: PTile;
  PsyTile: PTile;
  TMItem: TTileMapItem;
  Frame: TFrame;
  pal: TIntegerDynArray;
  chgDCT: TFloatDynArray3;
  DCT: array[0 .. cTileDCTSize - 1] of TFloat;
  q: Double;
begin
  if Length(FFrames) <= 0 then
    Exit;

  Frame := FFrames[FRenderFrameIndex];

  if not Assigned(Frame) or not Assigned(Frame.PKeyFrame) then
    Exit;

  PsyTile := TTile.New(True, False);
  try
    if not (FRenderPlaying or AFast) then
      SetLength(chgDCT, FTileMapHeight, FTileMapWidth, cTileDCTSize);

    // Global

    globalTileCount := GetTileCount(not (FRenderPlaying or AFast));

    FRenderTitleText := 'Global: ' + IntToStr(globalTileCount) + ' / Frame #' + IntToStr(FRenderFrameIndex) + IfThen(Frame.PKeyFrame.StartFrame = FRenderFrameIndex, ' [KF]', '     ') + ' : ' + IntToStr(GetFrameTileCount(Frame));

    // "Input" tab

    if FRenderPage = rpInput then
    begin
      FInputBitmap.Canvas.Brush.Color := clBlack;
      FInputBitmap.Canvas.Brush.Style := bsSolid;
      FInputBitmap.Canvas.FillRect(FInputBitmap.Canvas.ClipRect);
      FInputBitmap.Canvas.Brush.Color := $202020;
      FInputBitmap.Canvas.Brush.Style := bsDiagCross;
      FInputBitmap.Canvas.FillRect(FInputBitmap.Canvas.ClipRect);

      FInputBitmap.BeginUpdate;
      Frame.PKeyFrame.AcquireFrameTiles;
      try
        if Assigned(FRenderPrevKeyFrame) then
          FRenderPrevKeyFrame.ReleaseFrameTiles;

        for sy := 0 to FTileMapHeight - 1 do
          for sx := 0 to FTileMapWidth - 1 do
          begin
            tilePtr :=  Frame.FrameTiles[sy * FTileMapWidth + sx];
            if (FRenderPaletteIndex < 0) or (frame.TileMap[sy, sx].PalIdx = FRenderPaletteIndex) then
            begin
              hmir := tilePtr^.HMirror_Initial;
              vmir := tilePtr^.VMirror_Initial;

              if not FRenderMirrored then
              begin
                hmir := False;
                vmir := False;
              end;

              DrawTile(FInputBitmap, sx, sy, nil, tilePtr, nil, hmir, vmir);
            end;
          end;
      finally
        FInputBitmap.EndUpdate;
        FRenderPrevKeyFrame := Frame.PKeyFrame;
      end;
    end;

    // "Output" tab

    if (FRenderPage = rpOutput) or not (FRenderPlaying or AFast) then
    begin
      FOutputBitmap.Canvas.Brush.Color := clBlack;
      FOutputBitmap.Canvas.Brush.Style := bsSolid;
      FOutputBitmap.Canvas.FillRect(FOutputBitmap.Canvas.ClipRect);
      FOutputBitmap.Canvas.Brush.Color := $202020;
      FOutputBitmap.Canvas.Brush.Style := bsDiagCross;
      FOutputBitmap.Canvas.FillRect(FOutputBitmap.Canvas.ClipRect);

      FOutputBitmap.BeginUpdate;
      try
        for sy := 0 to FTileMapHeight - 1 do
          for sx := 0 to FTileMapWidth - 1 do
          begin
            frmIdx := Frame.Index;
            TMItem := FFrames[frmIdx].TileMap[sy, sx];
            if FRenderSmoothed then
            begin
              while TMItem.Smoothed do
              begin
                Dec(frmIdx);
                TMItem := FFrames[frmIdx].TileMap[sy, sx];
              end;
            end
            else
            begin
              if TMItem.Smoothed then
                Continue;
            end;

            if InRange(TMItem.TileIdx, 0, High(Tiles)) then
            begin
              pal := nil;
              if FRenderDithered then
                if FRenderPaletteIndex < 0 then
                begin
                  if not InRange(TMItem.PalIdx, 0, High(FFrames[frmIdx].PaletteRGB)) then
                    Continue;
                  pal := FFrames[frmIdx].PaletteRGB[TMItem.PalIdx]
                end
                else
                begin
                  if FRenderPaletteIndex <> TMItem.PalIdx then
                    Continue;
                  pal := FFrames[frmIdx].PaletteRGB[FRenderPaletteIndex];
                end;

              tilePtr := Tiles[TMItem.TileIdx];

              if not FRenderMirrored then
              begin
                TMItem.HMirror := False;
                TMItem.VMirror := False;
              end;

              DrawTile(FOutputBitmap, sx, sy, PsyTile, tilePtr, pal, TMItem.HMirror, TMItem.VMirror);

              if not (FRenderPlaying or AFast) then
                ComputeTilePsyVisFeatures(PsyTile^, False, False, False, False, False, cColorCpns, -1, nil, PFloat(@chgDCT[sy, sx, 0]));
            end;
          end;
      finally
        FOutputBitmap.EndUpdate;
      end;
    end;

    // "Palettes / Tiles" tab

    if FRenderPage = rpTilesPalette then
    begin
      FPaletteBitmap.BeginUpdate;
      try
        for j := 0 to FPaletteBitmap.Height - 1 do
        begin
          p := FPaletteBitmap.ScanLine[j];
          for i := 0 to FPaletteBitmap.Width - 1 do
          begin
            if Assigned(Frame.PaletteRGB) and Assigned(Frame.PaletteRGB[j]) then
              p^ := SwapRB(Frame.PaletteRGB[j, i])
            else
              p^ := clFuchsia;

            Inc(p);
          end;
        end;
      finally
        FPaletteBitmap.EndUpdate;
      end;

      FTilesBitmap.BeginUpdate;
      try
        FTilesBitmap.Canvas.Brush.Color := clAqua;
        FTilesBitmap.Canvas.Brush.Style := bsSolid;
        FTilesBitmap.Canvas.FillRect(FTilesBitmap.Canvas.ClipRect);

        for sy := 0 to FTileMapHeight - 1 do
          for sx := 0 to FTileMapWidth - 1 do
          begin
            tidx := FTileMapWidth * sy + sx + FTileMapSize * FRenderTilePage;

            if InRange(tidx, 0, High(Tiles)) then
            begin
              tilePtr := Tiles[tidx];
              pal := nil;
              if FRenderDithered then
                pal := Frame.PaletteRGB[Max(0, FRenderPaletteIndex)];

              hmir := tilePtr^.HMirror_Initial;
              vmir := tilePtr^.VMirror_Initial;

              if not FRenderMirrored then
              begin
                hmir := False;
                vmir := False;
              end;

              DrawTile(FTilesBitmap, sx, sy, nil, tilePtr, pal, hmir, vmir);
            end;
          end;
      finally
        FTilesBitmap.EndUpdate;
      end;
    end;

    if not (FRenderPlaying or AFast) then
    begin
      Frame.PKeyFrame.AcquireFrameTiles;
      try
        if Assigned(FRenderPrevKeyFrame) then
          FRenderPrevKeyFrame.ReleaseFrameTiles;

        q := 0.0;
        i := 0;
        for sy := 0 to FTileMapHeight - 1 do
          for sx := 0 to FTileMapWidth - 1 do
          begin
            tilePtr := Frame.FrameTiles[sy * FTileMapWidth + sx];
            if (FRenderPaletteIndex < 0) or (frame.TileMap[sy, sx].PalIdx = FRenderPaletteIndex) then
            begin
              hmir := tilePtr^.HMirror_Initial;
              vmir := tilePtr^.VMirror_Initial;

              if not FRenderMirrored then
              begin
                hmir := False;
                vmir := False;
              end;

              ComputeTilePsyVisFeatures(tilePtr^, False, False, False, hmir, vmir, cColorCpns, Ord(FRenderUseGamma) * 2 - 1, nil, PFloat(@DCT[0]));
              q += CompareEuclideanDCT(DCT, chgDCT[sy, sx]);
              Inc(i);
            end;
          end;
        if i <> 0 then
          q /= i;

        FRenderPsychoVisualQuality := Sqrt(q);
      finally
        FRenderPrevKeyFrame := Frame.PKeyFrame;
      end;
    end;
  finally
    TTile.Dispose(PsyTile);
  end;
end;

procedure TTilingEncoder.SaveSettings(ASettingsFileName: String);
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(ASettingsFileName, []);
  try

    ini.WriteString('Load', 'InputFileName', InputFileName);
    ini.WriteString('Load', 'OutputFileName', OutputFileName);
    ini.WriteInteger('Load', 'StartFrame', StartFrame);
    ini.WriteInteger('Load', 'FrameCount', FrameCountSetting);
    ini.WriteFloat('Load', 'Scaling', Scaling);

    ini.WriteInteger('Dither', 'PaletteSize', PaletteSize);
    ini.WriteInteger('Dither', 'PaletteCount', PaletteCount);
    ini.WriteBool('Dither', 'QuantizerUseYakmo', QuantizerUseYakmo);
    ini.WriteInteger('Dither', 'QuantizerDennisLeeBitsPerComponent', QuantizerDennisLeeBitsPerComponent);
    ini.WriteBool('Dither', 'QuantizerPosterize', QuantizerPosterize);
    ini.WriteInteger('Dither', 'QuantizerPosterizeBitsPerComponent', QuantizerPosterizeBitsPerComponent);
    ini.WriteBool('Dither', 'DitheringUseGamma', DitheringUseGamma);
    ini.WriteBool('Dither', 'DitheringUseThomasKnoll', DitheringUseThomasKnoll);
    ini.WriteInteger('Dither', 'DitheringYliluoma2MixedColors', DitheringYliluoma2MixedColors);

    ini.WriteBool('GlobalTiling', 'GlobalTilingUseGamma', GlobalTilingUseGamma);
    ini.WriteFloat('GlobalTiling', 'GlobalTilingQualityBasedTileCount', GlobalTilingQualityBasedTileCount);
    ini.WriteInteger('GlobalTiling', 'GlobalTilingTileCount', GlobalTilingTileCount);
    ini.WriteBool('GlobalTiling', 'GlobalTilingLumaOnly', GlobalTilingLumaOnly);
    ini.WriteFloat('GlobalTiling', 'GlobalTilingRatio', GlobalTilingRatio);

    ini.WriteBool('FrameTiling', 'FrameTilingFromPalette', FrameTilingFromPalette);
    ini.WriteBool('FrameTiling', 'FrameTilingUseGamma', FrameTilingUseGamma);

    ini.WriteFloat('Smoothing', 'SmoothingFactor', SmoothingFactor);

    ini.WriteFloat('Misc', 'EncoderGammaValue', EncoderGammaValue);
    ini.WriteInteger('Misc', 'MaxThreadCount', MaxThreadCount);

    ini.WriteFloat('Load', 'ShotTransMaxSecondsPerKF', ShotTransMaxSecondsPerKF);
    ini.WriteFloat('Load', 'ShotTransMinSecondsPerKF', ShotTransMinSecondsPerKF);
    ini.WriteFloat('Load', 'ShotTransCorrelLoThres', ShotTransCorrelLoThres);

  finally
    ini.Free;
  end;
end;

procedure TTilingEncoder.LoadSettings(ASettingsFileName: String);
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(ASettingsFileName, []);
  try
    LoadDefaultSettings;

    InputFileName := ini.ReadString('Load', 'InputFileName', InputFileName);
    OutputFileName := ini.ReadString('Load', 'OutputFileName', OutputFileName);
    StartFrame := ini.ReadInteger('Load', 'StartFrame', StartFrame);
    FrameCountSetting := ini.ReadInteger('Load', 'FrameCount', FrameCountSetting);
    Scaling := ini.ReadFloat('Load', 'Scaling', Scaling);

    PaletteSize := ini.ReadInteger('Dither', 'PaletteSize', PaletteSize);
    PaletteCount := ini.ReadInteger('Dither', 'PaletteCount', PaletteCount);
    QuantizerUseYakmo := ini.ReadBool('Dither', 'QuantizerUseYakmo', QuantizerUseYakmo);
    QuantizerDennisLeeBitsPerComponent := ini.ReadInteger('Dither', 'QuantizerDennisLeeBitsPerComponent', QuantizerDennisLeeBitsPerComponent);
    QuantizerPosterize := ini.ReadBool('Dither', 'QuantizerPosterize', QuantizerPosterize);
    QuantizerPosterizeBitsPerComponent := ini.ReadInteger('Dither', 'QuantizerPosterizeBitsPerComponent', QuantizerPosterizeBitsPerComponent);
    DitheringUseGamma := ini.ReadBool('Dither', 'DitheringUseGamma', DitheringUseGamma);
    DitheringUseThomasKnoll := ini.ReadBool('Dither', 'DitheringUseThomasKnoll', DitheringUseThomasKnoll);
    DitheringYliluoma2MixedColors := ini.ReadInteger('Dither', 'DitheringYliluoma2MixedColors', DitheringYliluoma2MixedColors);

    GlobalTilingUseGamma := ini.ReadBool('GlobalTiling', 'GlobalTilingUseGamma', GlobalTilingUseGamma);
    GlobalTilingQualityBasedTileCount := ini.ReadFloat('GlobalTiling', 'GlobalTilingQualityBasedTileCount', GlobalTilingQualityBasedTileCount);
    GlobalTilingTileCount := ini.ReadInteger('GlobalTiling', 'GlobalTilingTileCount', GlobalTilingTileCount); // after GlobalTilingQualityBasedTileCount because has priority
    GlobalTilingLumaOnly := ini.ReadBool('GlobalTiling', 'FGlobalTilingLumaOnly', FGlobalTilingLumaOnly);
    GlobalTilingRatio := ini.ReadFloat('GlobalTiling', 'GlobalTilingRatio', GlobalTilingRatio);

    FrameTilingFromPalette := ini.ReadBool('FrameTiling', 'FrameTilingFromPalette', FrameTilingFromPalette);
    FrameTilingUseGamma := ini.ReadBool('FrameTiling', 'FrameTilingUseGamma', FrameTilingUseGamma);

    SmoothingFactor := ini.ReadFloat('Smoothing', 'SmoothingFactor', SmoothingFactor);

    EncoderGammaValue := ini.ReadFloat('Misc', 'EncoderGammaValue', EncoderGammaValue);
    MaxThreadCount := ini.ReadInteger('Misc', 'MaxThreadCount', MaxThreadCount);

    ShotTransMaxSecondsPerKF := ini.ReadFloat('Load', 'ShotTransMaxSecondsPerKF', ShotTransMaxSecondsPerKF);
    ShotTransMinSecondsPerKF := ini.ReadFloat('Load', 'ShotTransMinSecondsPerKF', ShotTransMinSecondsPerKF);
    ShotTransCorrelLoThres := ini.ReadFloat('Load', 'ShotTransCorrelLoThres', ShotTransCorrelLoThres);

  finally
    ini.Free;
  end;
end;

procedure TTilingEncoder.LoadDefaultSettings;
begin
  InputFileName := '';
  OutputFileName := '';
  StartFrame := 0;
  FrameCountSetting := 0;
  Scaling := 1.0;

  PaletteSize := 32;
  PaletteCount := 64;
  QuantizerUseYakmo := True;
  QuantizerDennisLeeBitsPerComponent := 7;
  QuantizerPosterize := False;
  QuantizerPosterizeBitsPerComponent := 3;
  DitheringUseGamma := False;
  DitheringUseThomasKnoll := True;
  DitheringYliluoma2MixedColors := 4;

  GlobalTilingUseGamma := False;
  GlobalTilingQualityBasedTileCount := 7.0;
  GlobalTilingTileCount := 0; // after GlobalTilingQualityBasedTileCount because has priority
  GlobalTilingLumaOnly := False;
  GlobalTilingRatio := 0.9;

  FrameTilingFromPalette := False;
  FrameTilingUseGamma := False;

  SmoothingFactor := 0.2;

  EncoderGammaValue := 2.0;

  FShotTransMaxSecondsPerKF := 15.0;  // maximum seconds between keyframes
  FShotTransMinSecondsPerKF := 1.0;  // minimum seconds between keyframes
  FShotTransCorrelLoThres := 0.5;   // interframe pearson correlation low limit
end;

procedure TTilingEncoder.Test;
var
  i, j, rng: Integer;
  rr, gg, bb: Byte;
  l, a, b, y, u, v: TFloat;
  DCT: array [0..cTileDCTSize-1] of Double;
  T, T2: PTile;
begin
  InitLuts;

  for i := 0 to 10000 do
  begin
    rng := RandomRange(0, (1 shl 24) - 1);
    FromRGB(rng, rr, gg, bb);

    RGBToLAB(rr, gg, bb, -1, l, a, b);
    assert(rng = LABToRGB(l, a ,b, -1), 'RGBToLAB/LABToRGB mismatch');

    RGBToYUV(rr, gg, bb, -1, y, u, v);
    assert(rng = YUVToRGB(y, u, v, -1), 'RGBToYUV/YUVToRGB mismatch');
  end;

  T := TTile.New(True, False);
  T2 := TTile.New(True, False);

  for i := 0 to cTileWidth - 1 do
    for j := 0 to cTileWidth - 1 do
      T^.RGBPixels[i, j] := ToRGB(i*8, j * 32, i * j);

  ComputeTilePsyVisFeatures(T^, False, False, False, False, False, cColorCpns, -1, nil, @DCT[0]);
  ComputeInvTilePsyVisFeatures(@DCT[0], False, False, cColorCpns, -1, T2^);

  //for i := 0 to 7 do
  //  for j := 0 to 7 do
  //    write(IntToHex(T^.RGBPixels[i, j], 6), '  ');
  //WriteLn();
  //for i := 0 to 7 do
  //  for j := 0 to 7 do
  //    write(IntToHex(T2^.RGBPixels[i, j], 6), '  ');
  //WriteLn();

  Assert(CompareMem(T^.GetRGBPixelsPtr, T2^.GetRGBPixelsPtr, SizeOf(TRGBPixels)), 'DCT/InvDCT mismatch');

  ComputeTilePsyVisFeatures(T^, False, False, True, False, False, cColorCpns, -1, nil, @DCT[0]);
  ComputeInvTilePsyVisFeatures(@DCT[0], False, True, cColorCpns, -1, T2^);

  Assert(CompareMem(T^.GetRGBPixelsPtr, T2^.GetRGBPixelsPtr, SizeOf(TRGBPixels)), 'QWeighted DCT/InvDCT mismatch');

  TTile.Dispose(T);
  TTile.Dispose(T2);
end;

// from https://www.delphipraxis.net/157099-fast-integer-rgb-hsl.html
procedure TTilingEncoder.RGBToHSV(col: Integer; out h, s, v: Byte);
var
  rr, gg, bb: Integer;

  function RGBMaxValue: Integer;
  begin
    Result := rr;
    if (Result < gg) then Result := gg;
    if (Result < bb) then Result := bb;
  end;

  function RGBMinValue : Integer;
  begin
    Result := rr;
    if (Result > gg) then Result := gg;
    if (Result > bb) then Result := bb;
  end;

var
  Delta, mx, mn, hh, ss, ll: Integer;
begin
  FromRGB(col, rr, gg, bb);

  mx := RGBMaxValue;
  mn := RGBMinValue;

  hh := 0;
  ss := 0;
  ll := mx;
  if ll <> mn then
  begin
    Delta := ll - mn;
    ss := MulDiv(Delta, 255, ll);

    if (rr = ll) then
      hh := MulDiv(42, gg - bb, Delta)
    else if (gg = ll) then
      hh := MulDiv(42, bb - rr, Delta) + 84
    else if (bb = ll) then
      hh := MulDiv(42, rr - gg, Delta) + 168;

    hh := hh mod 252;
  end;

  h := hh and $ff;
  s := ss and $ff;
  v := ll and $ff;
end;

function TTilingEncoder.HSVToRGB(h, s, v: Byte): Integer;
const
  MaxHue: Integer = 252;
  MaxSat: Integer = 255;
  MaxLum: Integer = 255;
  Divisor: Integer = 42;
var
 f, LS, p, q, r: integer;
begin
 if (s = 0) then
   Result := ToRGB(v, v, v)
 else
  begin
   h := h mod MaxHue;
   s := EnsureRange(s, 0, MaxSat);
   v := EnsureRange(v, 0, MaxLum);

   f := h mod Divisor;
   h := h div Divisor;
   LS := v*s;
   p := v - LS div MaxLum;
   q := v - (LS*f) div (255 * Divisor);
   r := v - (LS*(Divisor - f)) div (255 * Divisor);
   case h of
    0: Result := ToRGB(v, r, p);
    1: Result := ToRGB(q, v, p);
    2: Result := ToRGB(p, v, r);
    3: Result := ToRGB(p, q, v);
    4: Result := ToRGB(r, p, v);
    5: Result := ToRGB(v, p, q);
   else
    Result := ToRGB(0, 0, 0);
   end;
  end;
end;

procedure TTilingEncoder.ProgressRedraw(ASubStepIdx: Integer; AReason: String; AProgressStep: TEncoderStep;
 AThread: TThread);

  function GetStepLen: Integer;
  begin
    Result := cEncoderStepLen[FProgressStep];

    if Result < 0 then
      Result *= -Length(FFrames);
  end;

const
  cProgressMul = 100;
var
  curTime: Int64;
  ProgressPosition, ProgressStepPosition, ProgressMax: Integer;
  ProgressHourGlass: Boolean;
begin
  if not Assigned(AThread) then
    scalable_allocation_command(TBBMALLOC_CLEAN_ALL_BUFFERS, nil); // force the mem allocator to release unused memory

  curTime := GetTickCount64;

  if (ASubStepIdx < 0) and (AProgressStep = esAll) then // reset
  begin
    FProgressStep := esAll;
    FProgressPrevTime := curTime;
    FProgressAllStartTime := curTime;
    FProgressProcessStartTime := curTime;
  end
  else if AProgressStep <> esAll then // new step?
  begin
    if ASubStepIdx = 0 then
    begin
      FProgressAllStartTime += curTime - FProgressPrevTime;
      FProgressProcessStartTime := curTime;
    end;
    FProgressStep := AProgressStep;
  end;

  ProgressMax := (Ord(High(TEncoderStep)) + 1) * cProgressMul;
  ProgressPosition := Ord(FProgressStep) * cProgressMul;

  ProgressStepPosition := 0;
  if ASubStepIdx >= 0 then
    ProgressStepPosition := iDiv0(ASubStepIdx * cProgressMul, GetStepLen);

  ProgressHourGlass := (AProgressStep <> esAll) and (ASubStepIdx < GetStepLen);

  if ASubStepIdx >= 0 then
  begin
    WriteLn('Step: ', Copy(GetEnumName(TypeInfo(TEncoderStep), Ord(FProgressStep)), 3), ' / ', ProgressStepPosition,
      #9'Time: ', FormatFloat('0.000', (curTime - FProgressProcessStartTime) / 1000),
      #9'All: ', FormatFloat('0.000', (curTime - FProgressAllStartTime) / 1000),
      IfThen(AReason <> '', ', Reason: '), AReason);
  end;
  FProgressPrevTime := curTime;

  // reset time for "named" substeps (ie. not by frame)
  if cEncoderStepLen[FProgressStep] >= 0 then
    FProgressProcessStartTime := curTime;

  EnterCriticalSection(FCS);
  try
    FProgressSyncPos := ProgressPosition + ProgressStepPosition;
    FProgressSyncMax := ProgressMax;
    FProgressSyncHG := ProgressHourGlass;

    if Assigned(AThread) then
      TThread.Queue(AThread, @SyncProgress)
    else
      SyncProgress;
  finally
    LeaveCriticalSection(FCS);
  end;
end;

procedure TTilingEncoder.SyncProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, FProgressSyncPos, FProgressSyncMax, FProgressSyncHG);
end;

function TTilingEncoder.GetFrameTileCount(AFrame: TFrame): Integer;
var
  Used: TByteDynArray;
  sx, sy: Integer;
  TMI: PTileMapItem;
begin
  Result := 0;

  if Length(Tiles) = 0 then
    Exit;

  SetLength(Used, Length(Tiles));
  FillByte(Used[0], Length(Tiles), 0);

  for sy := 0 to FTileMapHeight - 1 do
    for sx := 0 to FTileMapWidth - 1 do
    begin
      TMI := @AFrame.TileMap[sy, sx];
      if TMI^.TileIdx >= 0 then
        Used[TMI^.TileIdx] := 1;
    end;

  for sx := 0 to High(Used) do
    Inc(Result, Used[sx]);
end;

procedure TTilingEncoder.DoGlobalKMeans(AClusterCount, AGamma, AColorCpns: Integer; ABIRCHRatio, ABICORatio: Double);
var
  DSLen: Int64;
  featureCount, clusterCount, doneFrameCount: Integer;
  BIRCH: PBIRCH;
  BICO: PBICO;
  DCTs: TDoubleDynArray2;
  ANNClusters: TIntegerDynArray;
  ANNErrors: TDoubleDynArray;
  ANNPalIdxs: TSmallIntDynArray;
  ANN: PANNkdtree;
  ANNDataset: array of PDouble;
  TileLineIdxs: TInt64DynArray;

  procedure DoDither(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    frameOffset: Int64;
    frmIdx, sx, sy, si: Integer;
    KeyFrame: TKeyFrame;
    Frame: TFrame;
    Tile: PTile;
    TMI: PTileMapItem;
  begin
    if not InRange(AIndex, 0, High(FKeyFrames)) then
      Exit;

    KeyFrame := FKeyFrames[AIndex];

    KeyFrame.AcquireFrameTiles;
    try
      for frmIdx := KeyFrame.StartFrame to KeyFrame.EndFrame do
      begin
        Frame := FFrames[frmIdx];
        frameOffset := frmIdx * FTileMapSize;

        si := 0;
        for sy := 0 to FTileMapHeight - 1 do
          for sx := 0 to FTileMapWidth - 1 do
          begin
            Tile := Tiles[frmIdx * FTileMapSize + si];
            TMI := @Frame.TileMap[sy, sx];

            Tile^.CopyFrom(Frame.FrameTiles[si]^);

            DitherTile(Tile^, Frame.PaletteInfo[TMI^.PalIdx].MixingPlan);

            TMI^.HMirror := Tile^.HMirror_Initial;
            TMI^.VMirror := Tile^.VMirror_Initial;
            TMI^.TileIdx := frameOffset + si;

            Inc(si);
          end;

        Write(InterLockedIncrement(doneFrameCount):8, ' / ', Length(FFrames):8, #13);
      end;
    finally
      KeyFrame.ReleaseFrameTiles;
    end;
  end;

  procedure DoANN(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    frameOffset: Int64;
    sx, sy, si: Integer;
    KeyFrame: TKeyFrame;
    Frame: TFrame;
    Tile: PTile;
    TMI: PTileMapItem;
    DCT: array[0 .. cTileDCTSize - 1] of Double;
  begin
    if not InRange(AIndex, 0, High(FFrames)) then
      Exit;

    Frame := FFrames[AIndex];
    KeyFrame := Frame.PKeyFrame;

    KeyFrame.AcquireFrameTiles;
    try
      frameOffset := AIndex * FTileMapSize;

      si := 0;
      for sy := 0 to FTileMapHeight - 1 do
        for sx := 0 to FTileMapWidth - 1 do
        begin
          Tile := Frame.FrameTiles[si];

          ComputeTilePsyVisFeatures(Tile^, False, False, cClusterQWeighting, False, False, AColorCpns, AGamma, nil, @DCT[0]);

          ANNClusters[frameOffset + si] := ann_kdtree_search(ANN, @DCT[0], 0.0, @ANNErrors[frameOffset + si]);

          Tile := Frame.FrameTiles[si];
          TMI := @Frame.TileMap[sy, sx];

          TMI^.HMirror := Tile^.HMirror_Initial;
          TMI^.VMirror := Tile^.VMirror_Initial;
          TMI^.TileIdx := ANNClusters[frameOffset + si];

          ANNPalIdxs[frameOffset + si] := TMI^.PalIdx;

          Inc(si);
        end;

      Write(InterLockedIncrement(doneFrameCount):8, ' / ', Length(FFrames):8, #13);
    finally
      KeyFrame.ReleaseFrameTiles;
    end;
  end;

  procedure DoClusterDither(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    frmIdx, palIdx: Integer;
    Frame: TFrame;
    Tile: PTile;
    DCT: array[0 .. cTileDCTSize - 1] of Double;
  begin
    if not InRange(AIndex, 0, clusterCount - 1) then
      Exit;

    FillQWord(DCT[0], cTileDCTSize, 0);
    frmIdx :=  TileLineIdxs[AIndex] div FTileMapSize;

    Frame := FFrames[frmIdx];
    Tile := Tiles[AIndex];

    Move(ANNDataset[AIndex]^, DCT[0], featureCount * SizeOf(Double));
    ComputeInvTilePsyVisFeatures(@DCT[0], False, cClusterQWeighting, AColorCpns, AGamma, Tile^);

    palIdx := ANNPalIdxs[TileLineIdxs[AIndex]];
    if AColorCpns = 1 then
      DitherTile(Tile^, Frame.PaletteInfo[palIdx].BWMixingPlan)
    else
      DitherTile(Tile^, Frame.PaletteInfo[palIdx].MixingPlan);

    Tile^.Active := True;
  end;


  procedure DoDCTs(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    Frame: TFrame;
  begin
    if not InRange(AIndex, 0, FTileMapSize - 1) then
      Exit;

    Frame := TFrame(AData);
    ComputeTilePsyVisFeatures(Frame.FrameTiles[AIndex]^, False, False, cClusterQWeighting, False, False, AColorCpns, AGamma, nil, @DCTs[AIndex, 0]);
  end;

  procedure DoInsert(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    si: Integer;
  begin
    for si := 0 to FTileMapSize - 1 do
      case AIndex of
        0: bico_insert_line(BICO, @DCTs[si, 0], 1.0); // KLUDGE: must stay index 0 (main thread) to avoid OpenMP spawning too many threads
        1: birch_insert_line(BIRCH, @DCTs[si, 0]);
      end;
  end;

var
  i, kfIdx, frmIdx, clusterIdx: Integer;
  BIRCHClusterCount, BICOClusterCount: Integer;
  lineIdx: Int64;
  err: Single;
  KeyFrame: TKeyFrame;
  Frame: TFrame;
  BIRCHCentroids, BICOCentroids, BICOWeights: TDoubleDynArray;
  TileBestErr: TSingleDynArray;
begin
  featureCount := cTileDCTSize div cColorCpns * AColorCpns;

  DSLen := Length(FFrames) * FTileMapSize;
  if DSLen <= AClusterCount then
  begin
    // still dither tiles in case no need for clustering

    FTiles := TTile.Array1DNew(DSLen, True, True);

    doneFrameCount := 0;
    ProcThreadPool.DoParallelLocalProc(@DoDither, 0, High(FKeyFrames));

    ProgressRedraw(6, 'DitherTiles');
    Exit;
  end;

  // use BICO & BIRCH to prepare a noise-aware set of centroids

  SetLength(DCTs, FTileMapSize, cTileDCTSize);
  BIRCH := nil;
  BICO := nil;
  if ABIRCHRatio > 0.0 then
    BIRCH := birch_create(1.0, Round(AClusterCount * ABIRCHRatio), FTileMapSize);
  if ABICORatio > 0.0 then
    BICO := bico_create(featureCount, DSLen, Round(AClusterCount * ABICORatio), 32, Round(AClusterCount * ABICORatio), CRandomSeed);
  try
    if Assigned(BICO) then
    begin
      bico_set_num_threads(Max(1, MaxThreadCount - 2));
      bico_set_rebuild_properties(BICO, FTileMapSize, cPhi, cPhi);
    end;

    // insert frame tiles into BICO & BIRCH

    doneFrameCount := 0;
    for kfIdx := 0 to High(FKeyFrames) do
    begin
      KeyFrame := FKeyFrames[kfIdx];

      KeyFrame.AcquireFrameTiles;
      try
        for frmIdx := KeyFrame.StartFrame to KeyFrame.EndFrame do
        begin
          Frame := FFrames[frmIdx];

          //if (kfIdx = High(FKeyFrames)) and (frmIdx = KeyFrame.EndFrame) then
          //  bico_set_rebuild_properties(BICO, FTileMapSize, NaN, 1.05); // lower grow on last rebuild for better K enforcement

          // compute DCTs for Frame
          ProcThreadPool.DoParallelLocalProc(@DoDCTs, 0, FTileMapSize - 1, Frame);

          // insert line into BICO & BIRCH
          if Assigned(BIRCH) and not Assigned(BICO) then
            DoInsert(1, nil, nil)
          else if not Assigned(BIRCH) and Assigned(BICO) then
            DoInsert(0, nil, nil)
          else if Assigned(BIRCH) and Assigned(BICO) then
            ProcThreadPool.DoParallelLocalProc(@DoInsert, 0, 1);

          Write(InterLockedIncrement(doneFrameCount):8, ' / ', Length(FFrames):8, #13);
        end;

      finally
        KeyFrame.ReleaseFrameTiles;
      end;
    end;

    // get BIRCH results

    BIRCHClusterCount := 0;
    if Assigned(BIRCH) then
    begin
      BIRCHClusterCount := birch_compute(BIRCH, False, False);
      SetLength(BIRCHCentroids, BIRCHClusterCount * cTileDCTSize); // not featureCount because BIRCH is hardcoded to cTileDCTSize
      birch_get_centroids(BIRCH, @BIRCHCentroids[0]);
    end;

    // get BICO results

    BICOClusterCount := 0;
    if Assigned(BICO) then
    begin
      SetLength(BICOWeights, Round(AClusterCount * ABICORatio));
      SetLength(BICOCentroids, Length(BICOWeights) * featureCount);
      BICOClusterCount := Max(1, bico_get_results(BICO, @BICOCentroids[0], @BICOWeights[0]));
    end;

    // join them to create the dataset for ANN

    clusterCount := BICOClusterCount + BIRCHClusterCount;

    SetLength(ANNDataset, clusterCount);
    for i := 0 to BICOClusterCount - 1 do
      ANNDataset[i] := @BICOCentroids[i * featureCount];

    for i := 0 to BIRCHClusterCount - 1 do
      ANNDataset[BICOClusterCount + i] := @BIRCHCentroids[i * cTileDCTSize];

  finally
    if Assigned(BIRCH) then
      birch_destroy(BIRCH);
    if Assigned(BICO) then
      bico_destroy(BICO);
    SetLength(BICOWeights, 0);
    SetLength(DCTs, 0);
  end;

  WriteLn('KF: ', StartFrame:8, ' DatasetSize: ', DSLen:8, ' BICOClusterCount: ', BICOClusterCount:6, ' BIRCHClusterCount: ', BIRCHClusterCount:6);

  ProgressRedraw(3, 'Clustering');

  if clusterCount <= 0 then
    Exit;

  // use ANN to compute cluster indexes

  SetLength(ANNClusters, DSLen);
  SetLength(ANNErrors, DSLen);
  SetLength(ANNPalIdxs, DSLen);

  ANN := ann_kdtree_create(@ANNDataset[0], clusterCount, featureCount, 32, ANN_KD_STD);
  try
    doneFrameCount := 0;
    ProcThreadPool.DoParallelLocalProc(@DoANN, 0, High(FFrames));
  finally
    ann_kdtree_destroy(ANN);
  end;

  ProgressRedraw(4, 'ANNReconstruct');

  // allocate tile set

  FTiles := TTile.Array1DNew(clusterCount, True, True);
  SetLength(TileLineIdxs, clusterCount);
  SetLength(TileBestErr, clusterCount);

  // prepare final tiles infos

  for clusterIdx := 0 to clusterCount - 1 do
    TileBestErr[clusterIdx] := MaxSingle;

  for lineIdx := 0 to DSLen - 1 do
  begin
    clusterIdx := ANNClusters[lineIdx];

    err := ANNErrors[lineIdx];

    if err < TileBestErr[clusterIdx] then
    begin
      TileBestErr[clusterIdx] := err;
      TileLineIdxs[clusterIdx] := lineIdx;
    end;
  end;

  ProgressRedraw(5, 'PrepareTiles');

  // dither final tiles

  ProcThreadPool.DoParallelLocalProc(@DoClusterDither, 0, clusterCount - 1);

  ProgressRedraw(6, 'DitherTiles');
end;

procedure TTilingEncoder.OptimizeGlobalPalettes;
var
  i, j, frmIdx, palIdx, colIdx1, colIdx2, iteration, bestFrmIdx, bestColIdx1, bestColIdx2, uc, tmp: Integer;
  r, g, b: byte;
  prevBest, best, v: Double;
  InnerPerm: TByteDynArray;
  PalR, PalG, PalB, InnerPalR, InnerPalG, InnerPalB: TDoubleDynArray;
  GlobalPalette: TDoubleDynArray3;
  tmpArr: TDoubleDynArray;
begin
  SetLength(GlobalPalette, Length(FFrames), FPaletteSize, cColorCpns);
  SetLength(PalR, FPaletteSize);
  SetLength(PalG, FPaletteSize);
  SetLength(PalB, FPaletteSize);
  SetLength(InnerPalR, FPaletteSize);
  SetLength(InnerPalG, FPaletteSize);
  SetLength(InnerPalB, FPaletteSize);
  SetLength(InnerPerm, FPaletteSize);

  for frmIdx := 0 to High(FFrames) do
    for palIdx := 0 to High(FFrames[frmIdx].PaletteRGB) do
    begin
      uc := FFrames[frmIdx].PaletteInfo[palIdx].UseCount;
      for i := 0 to FPaletteSize - 1 do
      begin
        FromRGB(FFrames[frmIdx].PaletteRGB[palIdx, i], r, g, b);

        GlobalPalette[frmIdx, i, 0] += r * uc;
        GlobalPalette[frmIdx, i, 1] += g * uc;
        GlobalPalette[frmIdx, i, 2] += b * uc;
      end;
    end;

  // stepwise algorithm on palette colors permutations across keyframes

  best := 0;
  iteration := 0;
  repeat
    prevBest := best;
    best := 0;

    bestFrmIdx := -1;
    bestColIdx1 := -1;
    bestColIdx2 := -1;
    for frmIdx := 0 to High(FKeyFrames) do
    begin
      // accumulate the whole palette except for the keyframe that will be permutated

      FillQWord(PalR[0], Length(PalR), 0);
      FillQWord(PalG[0], Length(PalG), 0);
      FillQWord(PalB[0], Length(PalB), 0);
      for i := 0 to High(FKeyFrames) do
      begin
        uc := FKeyFrames[i].FrameCount;
        for j := 0 to FPaletteSize - 1 do
          if i <> frmIdx then
          begin
            PalR[j] += GlobalPalette[i, j, 0] * uc;
            PalG[j] += GlobalPalette[i, j, 1] * uc;
            PalB[j] += GlobalPalette[i, j, 2] * uc;
          end;
      end;

      // try all permutations in the current keyframe

      for colIdx1 := 0 to High(InnerPerm) do
        for colIdx2 := colIdx1 + 1 to High(InnerPerm) do
        begin
          for i := 0 to FPaletteSize - 1 do
            InnerPerm[i] := i;

          tmp := InnerPerm[colIdx1];
          InnerPerm[colIdx1] := InnerPerm[colIdx2];
          InnerPerm[colIdx2] := tmp;

          Move(PalR[0], InnerPalR[0], Length(PalR) * SizeOf(Double));
          Move(PalG[0], InnerPalG[0], Length(PalG) * SizeOf(Double));
          Move(PalB[0], InnerPalB[0], Length(PalB) * SizeOf(Double));

          uc := FKeyFrames[frmIdx].FrameCount;
          for i := 0 to FPaletteSize - 1 do
          begin
            tmpArr := GlobalPalette[frmIdx, InnerPerm[i]];

            InnerPalR[i] += tmpArr[0] * uc;
            InnerPalG[i] += tmpArr[1] * uc;
            InnerPalB[i] += tmpArr[2] * uc;
          end;

          // try to maximize accumulated palette standard deviation
          // rationale: the less samey it is, the better the palettes pair with each other across keyframes

          v := cRedMul * StdDev(InnerPalR) + cGreenMul * StdDev(InnerPalG) + cBlueMul * StdDev(InnerPalB);

          if v > best then
          begin
            best := v;
            bestFrmIdx := frmIdx;
            bestColIdx1 := colIdx1;
            bestColIdx2 := colIdx2;
          end;
        end;
    end;

    if (best > prevBest) and (bestFrmIdx >= 0) and (bestColIdx1 >= 0) and (bestColIdx2 >= 0) then
    begin
      tmpArr := GlobalPalette[bestFrmIdx, bestColIdx1];
      GlobalPalette[bestFrmIdx, bestColIdx1] := GlobalPalette[bestFrmIdx, bestColIdx2];
      GlobalPalette[bestFrmIdx, bestColIdx2] := tmpArr;

      for palIdx := 0 to High(FFrames[bestFrmIdx].PaletteRGB) do
      begin
        tmp := FFrames[bestFrmIdx].PaletteRGB[palIdx, bestColIdx1];
        FFrames[bestFrmIdx].PaletteRGB[palIdx, bestColIdx1] := FFrames[bestFrmIdx].PaletteRGB[palIdx, bestColIdx2];
        FFrames[bestFrmIdx].PaletteRGB[palIdx, bestColIdx2] := tmp;
      end;
    end;

    Inc(iteration);

    //WriteLn(iteration:3, bestFrmIdx:3, bestColIdx1:3, bestColIdx2:3, best:16:0);

  until best <= prevBest;

  WriteLn('OptimizeGlobalPalettes: ', iteration, ' iterations');
end;

procedure TTilingEncoder.ReindexTiles(KeepRGBPixels: Boolean);
var
  IdxMap: TInt64DynArray;
  Frame: TFrame;

  procedure Remap(var ATidx: Integer);
  var
    tidx: Integer;
  begin
    tidx := ATidx;
    if tidx >= 0 then
    begin
      tidx := IdxMap[tidx];
      Assert(tidx >= 0);
      ATidx := tidx;
    end;
  end;

var
  frmIdx, sx, sy: Integer;
  pos, cnt, tidx: Int64;
  LocTiles: PTileDynArray;
  TMI: PTileMapItem;
begin
  cnt := 0;
  for tidx := 0 to High(Tiles) do
  begin
    Tiles[tidx]^.TmpIndex := tidx;
    if (Tiles[tidx]^.Active) and (Tiles[tidx]^.UseCount > 0) then
      Inc(cnt);
  end;

  if cnt <= 0 then
    Exit;

  // pack the global Tiles, removing inactive ones

  LocTiles := TTile.Array1DNew(cnt, KeepRGBPixels, True);
  pos := 0;
  for tidx := 0 to High(Tiles) do
    if (Tiles[tidx]^.Active) and (Tiles[tidx]^.UseCount > 0) then
    begin
      LocTiles[pos]^.CopyFrom(Tiles[tidx]^);
      Inc(pos);
    end;

  SetLength(IdxMap, Length(Tiles));
  FillQWord(IdxMap[0], Length(Tiles), QWord(-1));

  TTile.Array1DDispose(FTiles);
  FTiles := LocTiles;
  LocTiles := nil;

  // sort tiles

  QuickSort(Tiles[0], 0, High(Tiles), SizeOf(PTile), @CompareTileUseCountRev);

  for tidx := 0 to High(Tiles) do
    IdxMap[Tiles[tidx]^.TmpIndex] := tidx;

  // point tilemap items on new tiles indexes

  for frmIdx := 0 to High(FFrames) do
  begin
    Frame := FFrames[frmIdx];
    for sy := 0 to FTileMapHeight - 1 do
      for sx := 0 to FTileMapWidth - 1 do
      begin
        TMI := @Frame.TileMap[sy, sx];

        Remap(TMI^.TileIdx);
      end;
  end;

  WriteLn('ReindexTiles: ', Length(Tiles), ' final tiles');
end;

class function TTilingEncoder.GetTileZoneSum(const ATile: TTile; x, y, w, h: Integer): Integer;
var
  i, j: Integer;
  r, g, b: Byte;
begin
  Result := 0;
  for j := y to y + h - 1 do
    for i := x to x + w - 1 do
    begin
      FromRGB(ATile.RGBPixels[j, i], r, g, b);
      Result += ToLuma(r, g, b);
    end;
end;

class procedure TTilingEncoder.GetTileHVMirrorHeuristics(const ATile: TTile; out AHMirror, AVMirror: Boolean);
var
  q00, q01, q10, q11: Integer;
begin
  // enforce an heuristical 'spin' on tiles mirrors (brighter top-left corner)

  q00 := GetTileZoneSum(ATile, 0, 0, cTileWidth div 2, cTileWidth div 2);
  q01 := GetTileZoneSum(ATile, cTileWidth div 2, 0, cTileWidth div 2, cTileWidth div 2);
  q10 := GetTileZoneSum(ATile, 0, cTileWidth div 2, cTileWidth div 2, cTileWidth div 2);
  q11 := GetTileZoneSum(ATile, cTileWidth div 2, cTileWidth div 2, cTileWidth div 2, cTileWidth div 2);

  AHMirror := q00 + q10 < q01 + q11;
  AVMirror := q00 + q01 < q10 + q11;
end;

procedure TTilingEncoder.LoadStream(AStream: TStream);
var
  KFStream: TMemoryStream;

  function ReadDWord: Cardinal;
  begin
    Result := KFStream.ReadDWord;
  end;

  function ReadWord: Word;
  begin
    Result := KFStream.ReadWord;
  end;

  function ReadByte: Byte;
  begin
    Result := KFStream.ReadByte;
  end;

  procedure ReadCmd(out Cmd: TGTMCommand; out Data: Word);
  var
    d: Word;
  begin
    d := ReadWord;
    Cmd := TGTMCommand(d and ((1 shl CGTMCommandCodeBits) - 1));
    Data := d shr CGTMCommandCodeBits;
  end;

  procedure ReadTiles(PaletteSize: Integer);
  var
    i, startIdx, endIdx: Integer;
  begin
    startIdx := ReadDWord; // start tile
    endIdx := ReadDWord; // end tile

    for i := startIdx to endIdx do
    begin
      KFStream.Read(FTiles[i]^.GetPalPixelsPtr^[0, 0], sqr(cTileWidth));
      FTiles[i]^.Active := True;
    end;

    FPaletteSize := PaletteSize;
  end;

  procedure ReadDimensions;
  var
    w, h, frmLen, tileCount: Integer;
  begin
    w := ReadWord; // frame tilemap width
    h := ReadWord; // frame tilemap height
    ReframeUI(w, h);

    frmLen := ReadDWord; // frame length in nanoseconds
    FFramesPerSecond := 1000*1000*1000 / frmLen;

    tileCount := ReadDWord; // tile count
    FTiles := TTile.Array1DNew(tileCount, False, True); // tile count
  end;

  procedure ReadPalette(Frame: TFrame);
  var
    i, palIdx: Integer;
  begin
    palIdx := ReadByte;
    ReadByte;

    if Length(Frame.PaletteRGB) <= palIdx then
    begin
      SetLength(Frame.PaletteRGB, palIdx + 1, FPaletteSize);
      SetLength(Frame.PaletteInfo, Length(Frame.PaletteRGB));
      FPaletteCount := Length(Frame.PaletteRGB);
    end;

    for i := 0 to FPaletteSize - 1 do
      Frame.PaletteRGB[palIdx, i] := ReadDWord and $ffffff;
  end;

  procedure SetTMI(tileIdx: Integer; attrs: Integer; var TMI: TTileMapItem);
  begin
    TMI.TileIdx := tileIdx;
    TMI.HMirror := attrs and 1 <> 0;
    TMI.VMirror := attrs and 2 <> 0;
    TMI.PalIdx := attrs shr 2;

    TMI.Smoothed := False;
  end;

  function AddFrame(KF: TKeyFrame): TFrame;
  begin
    SetLength(FFrames, Length(FFrames) + 1);
    Result := TFrame.Create(Self, High(FFrames));
    Result.PKeyFrame := kf;
    FFrames[High(FFrames)] := Result;

    SetLength(Result.TileMap, FTileMapHeight, FTileMapWidth);
  end;

  procedure SkipBlock(frm: TFrame; SkipCount: Integer; var tmPos: Integer);
  var
    i: Integer;
    sx, sy: Integer;
  begin
    Assert(frm.Index > 0);
    for i := tmPos to tmPos + SkipCount - 1 do
    begin
      DivMod(i, FTileMapWidth, sy, sx);
      frm.TileMap[sy, sx] := FFrames[frm.Index - 1].TileMap[sy, sx];
      frm.TileMap[sy, sx].Smoothed := True;
    end;
    tmPos += SkipCount;
  end;

var
  Header: TGTMHeader;
  Command: TGTMCommand;
  CommandData: Word;
  frmCount, tmPos: Integer;
  tileIdx: Cardinal;
  frm: TFrame;
  kf: TKeyFrame;
  compat: String;
begin
  FillChar(Header, SizeOf(Header), 0);

  AStream.ReadBuffer(Header, SizeOf(Header.FourCC));
  AStream.Seek(0, soBeginning);

  if Header.FourCC = 'GTMv' then
  begin
    AStream.ReadBuffer(Header, SizeOf(Header));
    AStream.Seek(Header.WholeHeaderSize, soBeginning);

    compat := '';
    if Header.FrameCount <> FrameCount then
      compat := compat + Format('GTM FrameCount = %d; FrameCount = %d' + sLineBreak, [Header.FrameCount, Length(FFrames)]);
    if Header.FramePixelWidth <> ScreenWidth then
      compat := compat + Format('GTM ScreenWidth = %d; ScreenWidth = %d' + sLineBreak, [Header.FramePixelWidth, FScreenWidth]);
    if Header.FramePixelHeight <> ScreenHeight then
      compat := compat + Format('GTM ScreenHeight = %d; ScreenHeight = %d' + sLineBreak, [Header.FramePixelHeight, FScreenHeight]);

    if compat <> '' then
      raise ETilingEncoderGTMReloadError.Create('Mismatch between GTM and loaded video!' + sLineBreak + compat);
  end;

  ClearAll;

  frm := nil;
  frmCount := 0;
  KFStream := TMemoryStream.Create;
  try
    repeat
      KFStream.Clear;
      LZDecompress(AStream, KFStream);
      KFStream.Seek(0,soBeginning);

      // add a keyframe
      SetLength(FKeyFrames, Length(FKeyFrames) + 1);
      kf := TKeyFrame.Create(Self, High(FKeyFrames), frmCount, -1);
      FKeyFrames[High(FKeyFrames)] := kf;

      tmPos := 0;
      repeat
        ReadCmd(Command, CommandData);

        case Command of
          gtExtendedCommand:
          begin
            KFStream.Seek(ReadDWord, soCurrent);
          end;
          gtSetDimensions:
          begin
            ReadDimensions;
          end;
          gtTileSet:
          begin
            ReadTiles(CommandData);
          end;
          gtLoadPalette:
          begin
            // create frame if needed
            if frm = nil then
              frm := AddFrame(kf);

            ReadPalette(frm);
          end;
          gtFrameEnd:
          begin
            Assert(tmPos = FTileMapSize, 'Incomplete tilemap');

            // will have to create a new frame
            frm := nil;
            tmPos := 0;
            Inc(frmCount);

            if (CommandData and 1) <> 0 then // keyframe end?
              Break;
          end;
          gtSkipBlock:
          begin
            // create frame if needed
            if frm = nil then
              frm := AddFrame(kf);

            SkipBlock(frm, CommandData + 1, tmPos);
          end;
          gtShortTileIdx, gtLongTileIdx,
          gtShortBlendTileIdx, gtLongBlendTileIdx,
          gtShortAdditionalTileIdx, gtLongAdditionalTileIdx,
          gtShortAddlBlendTileIdx, gtLongAddlBlendTileIdx:
          begin
            if Command in [gtShortTileIdx, gtShortBlendTileIdx, gtShortAdditionalTileIdx, gtShortAddlBlendTileIdx] then
              tileIdx := ReadWord
            else
              tileIdx := ReadDWord;

            // unused (not compatible anymore)
            if Command in [gtShortAdditionalTileIdx, gtShortAddlBlendTileIdx] then
              ReadWord
            else if Command in [gtLongAdditionalTileIdx, gtLongAddlBlendTileIdx] then
              ReadDWord;

            // unused (not compatible anymore)
            if Command in [gtShortBlendTileIdx, gtLongBlendTileIdx] then
              ReadWord
            else if Command in [gtShortAddlBlendTileIdx, gtLongAddlBlendTileIdx] then
              ReadWord;

            // create frame if needed
            if frm = nil then
              frm := AddFrame(kf);

            SetTMI(tileIdx, CommandData, frm.TileMap[tmPos div FTileMapWidth, tmPos mod FTileMapWidth]);
            Inc(tmPos);
          end;
          gtPrevFrameBlend:
          begin
            // unused (not compatible anymore)
            ReadWord;

            // create frame if needed
            if frm = nil then
              frm := AddFrame(kf);

            SkipBlock(frm, 1, tmPos);
          end

          else
           Assert(False, 'Unknown command: ' + IntToStr(Ord(Command)) + ', ' + IntToStr(CommandData));
        end;
      until False;

      kf.EndFrame := frmCount - 1;
      kf.FrameCount := kf.EndFrame - kf.StartFrame + 1;

    until AStream.Position >= AStream.Size;
  finally
    KFStream.Free;
  end;

  ReframeUI(FTileMapWidth, FTileMapHeight); // for FPaletteBitmap
end;

procedure TTilingEncoder.SaveStream(AStream: TStream);
const
  CMinBlkSkipCount = 1;
  CMaxBlkSkipCount = 1 shl CGTMCommandBits;

var
  ZStream: TMemoryStream;

  procedure DoDWord(v: Cardinal);
  begin
    ZStream.WriteDWord(v);
  end;

  procedure DoWord(v: Word);
  begin
    ZStream.WriteWord(v);
  end;

  procedure DoByte(v: Byte);
  begin
    ZStream.WriteByte(v);
  end;

  procedure DoCmd(Cmd: TGTMCommand; Data: Cardinal);
  begin
    assert(Data < (1 shl CGTMCommandBits));
    assert(Ord(Cmd) < CGTMCommandsCount);

    DoWord((Data shl CGTMCommandCodeBits) or Ord(Cmd));
  end;

  function ExtractTMIAttributes(const TMI: TTileMapItem; out attrs: Word): Integer;
  begin
    attrs := (TMI.PalIdx shl 2) or (Ord(TMI.VMirror) shl 1) or Ord(TMI.HMirror);
    Result := TMI.TileIdx;
  end;

  procedure DoTMI(const TMI: TTileMapItem);
  var
    tileIdx: Integer;
    attrs: Word;
  begin
    Assert((TMI.PalIdx >= 0) and (TMI.PalIdx < FPaletteCount));

    tileIdx := ExtractTMIAttributes(TMI, attrs);

    if tileIdx < (1 shl 16) then
    begin
      DoCmd(gtShortTileIdx, attrs);
      DoWord(tileIdx);
    end
    else
    begin
      DoCmd(gtLongTileIdx, attrs);
      DoDWord(tileIdx);
    end;
  end;

  procedure WriteFrameAttributes(Frame: TFrame);
  var
    i, j: Integer;
  begin
    for j := 0 to FPaletteCount - 1 do
    begin
      DoCmd(gtLoadPalette, 0);
      DoByte(j);
      DoByte(0);
      for i := 0 to FPaletteSize - 1 do
        DoDWord(Frame.PaletteRGB[j, i] or $ff000000);
    end;
  end;

  procedure WriteTiles;
  var
    i: Integer;
  begin
    DoCmd(gtTileSet, FPaletteSize);
    DoDWord(0); // start tile
    DoDWord(High(Tiles)); // end tile

    for i := 0 to High(Tiles) do
    begin
      Assert(Tiles[i]^.Active);
      ZStream.Write(Tiles[i]^.GetPalPixelsPtr^[0, 0], sqr(cTileWidth));
    end;
  end;

  procedure WriteDimensions;
  var
    maxTileCount: Integer;
  begin
    maxTileCount := Length(Tiles);

    DoCmd(gtSetDimensions, 0);
    DoWord(FTileMapWidth); // frame tilemap width
    DoWord(FTileMapHeight); // frame tilemap height
    DoDWord(round(1000*1000*1000 / FFramesPerSecond)); // frame length in nanoseconds
    DoDWord(maxTileCount); // tile count
  end;

  procedure WriteSettings;
  begin
    DoCmd(gtExtendedCommand, 0);
    ZStream.WriteAnsiString(GetSettings);
  end;

var
  StartPos, StreamSize, LastKF, KFCount, KFSize, BlkSkipCount: Integer;
  kfIdx, frmIdx, yx, yxs, cs, sx, sy: Integer;
  IsKF: Boolean;
  KeyFrame: TKeyFrame;
  Frame: TFrame;
  Header: TGTMHeader;
  KFInfo: array of TGTMKeyFrameInfo;
begin
  StartPos := AStream.Size;

  FillChar(Header, SizeOf(Header), 0);
  Header.FourCC := 'GTMv';
  Header.RIFFSize := SizeOf(Header) - SizeOf(Header.FourCC) - SizeOf(Header.RIFFSize);
  Header.EncoderVersion := 3; // 2 -> fixed blending extents; 3 -> *AddlBlendTileIdx
  Header.FramePixelWidth := FScreenWidth;
  Header.FramePixelHeight := FScreenHeight;
  Header.KFCount := Length(FKeyFrames);
  Header.FrameCount := Length(FFrames);
  Header.AverageBytesPerSec := 0;
  Header.KFMaxBytesPerSec := 0;
  AStream.WriteBuffer(Header, SizeOf(Header));

  SetLength(KFInfo, Length(FKeyFrames));
  for kfIdx := 0 to High(FKeyFrames) do
  begin
    FillChar(KFInfo[kfIdx], SizeOf(KFInfo[0]), 0);
    KFInfo[kfIdx].FourCC := 'GTMk';
    KFInfo[kfIdx].RIFFSize := SizeOf(KFInfo[0]) - SizeOf(KFInfo[0].FourCC) - SizeOf(KFInfo[0].RIFFSize);
    KFInfo[kfIdx].KFIndex := kfIdx;
    KFInfo[kfIdx].FrameIndex := FKeyFrames[kfIdx].StartFrame;
    KFInfo[kfIdx].TimeCodeMillisecond := Round(1000.0 * FKeyFrames[kfIdx].StartFrame / FFramesPerSecond);
    AStream.WriteBuffer(KFInfo[kfIdx], SizeOf(KFInfo[0]));
  end;

  Header.WholeHeaderSize := AStream.Size - StartPos;

  StartPos := AStream.Size;

  ZStream := TMemoryStream.Create;
  try
    WriteSettings;
    WriteDimensions;
    WriteTiles;

    LastKF := 0;
    for kfIdx := 0 to High(FKeyFrames) do
    begin
      KeyFrame := FKeyFrames[kfIdx];

      for frmIdx := KeyFrame.StartFrame to KeyFrame.EndFrame do
      begin
        Frame := FFrames[frmIdx];

        WriteFrameAttributes(Frame);

        cs := 0;
        BlkSkipCount := 0;
        for yx := 0 to FTileMapSize - 1 do
        begin
          if BlkSkipCount > 0 then
          begin
            // handle an ongoing block skip

            Dec(BlkSkipCount);
          end
          else
          begin
            // find a potential new skip

            BlkSkipCount := 0;
            for yxs := yx to FTileMapSize - 1 do
            begin
              DivMod(yxs, FTileMapWidth, sy, sx);
              if not Frame.TileMap[sy, sx].Smoothed then
                Break;
              Inc(BlkSkipCount);
            end;
            BlkSkipCount := min(CMaxBlkSkipCount, BlkSkipCount);

            // filter using heuristics to avoid unbeneficial skips

            if BlkSkipCount >= CMinBlkSkipCount then
            begin
              //writeln('blk ', BlkSkipCount);

              DoCmd(gtSkipBlock, BlkSkipCount - 1);
              Inc(cs, BlkSkipCount);
              Dec(BlkSkipCount);
            end
            else
            begin
              // standard case: emit tilemap item

              BlkSkipCount := 0;

              DivMod(yx, FTileMapWidth, sy, sx);
              DoTMI(Frame.TileMap[sy, sx]);
              Inc(cs);
            end;
          end;
        end;
        Assert(cs = FTileMapSize, 'incomplete TM');
        Assert(BlkSkipCount = 0, 'pending skips');

        IsKF := (frmIdx = KeyFrame.EndFrame);

        DoCmd(gtFrameEnd, Ord(IsKF));

        if IsKF then
        begin
          KFCount := KeyFrame.EndFrame - LastKF + 1;
          LastKF := KeyFrame.EndFrame + 1;

          AStream.Position := AStream.Size;
          KFSize := AStream.Position;
          LZCompress(ZStream, AStream);

          KFSize := AStream.Size - KFSize;

          KFInfo[kfIdx].RawSize := ZStream.Size;
          KFInfo[kfIdx].CompressedSize := KFSize;
          if (kfIdx > 0) or (Length(FKeyFrames) = 1) then
            Header.KFMaxBytesPerSec := max(Header.KFMaxBytesPerSec, round(KFSize * FFramesPerSecond / KFCount));
          Header.AverageBytesPerSec += KFSize;

          WriteLn('KF: ', KeyFrame.StartFrame:8, ' FCnt: ', KFCount:4, ' Raw: ', KFInfo[kfIdx].RawSize:8, ' Written: ', KFSize:8, ' Bitrate: ', (KFSize / 1024.0 * 8.0 / KFCount):8:2, ' kbpf   (', (KFSize / 1024.0 * 8.0 / KFCount * FFramesPerSecond):8:2, ' kbps)');

          ZStream.Clear;
        end;
      end;
    end;
  finally
    ZStream.Free;
  end;

  Header.AverageBytesPerSec := round(Header.AverageBytesPerSec * FFramesPerSecond / Length(FFrames));
  AStream.Position := 0;
  AStream.WriteBuffer(Header, SizeOf(Header));
  for kfIdx := 0 to High(FKeyFrames) do
    AStream.WriteBuffer(KFInfo[kfIdx], SizeOf(KFInfo[0]));
  AStream.Position := AStream.Size;

  StreamSize := AStream.Size - StartPos;

  WriteLn('Written: ', StreamSize:12, ' Bitrate: ', (StreamSize / 1024.0 * 8.0 / Length(FFrames)):8:2, ' kbpf  (', (StreamSize / 1024.0 * 8.0 / Length(FFrames) * FFramesPerSecond):8:2, ' kbps)');
end;

constructor TTilingEncoder.Create;
begin
  FormatSettings.DecimalSeparator := '.';
  InitializeCriticalSection(FCS);

{$ifdef DEBUG}
  ProcThreadPool.MaxThreadCount := 1;
{$else}
  SetPriorityClass(GetCurrentProcess(), IDLE_PRIORITY_CLASS);
{$endif}

  FGamma[0] := 2.0;
  FGamma[1] := 0.6;

  FInputBitmap := TBitmap.Create;
  FOutputBitmap := TBitmap.Create;
  FTilesBitmap := TBitmap.Create;
  FPaletteBitmap := TBitmap.Create;

  FRenderPage := rpOutput;
  ReframeUI(80, 45);
  FFramesPerSecond := 24.0;

  LoadDefaultSettings;
end;

destructor TTilingEncoder.Destroy;
begin
  ClearAll;

  DeleteCriticalSection(FCS);

  FInputBitmap.Free;
  FOutputBitmap.Free;
  FTilesBitmap.Free;
  FPaletteBitmap.Free;
end;

procedure TTilingEncoder.Run(AStep: TEncoderStep);
var
  step: TEncoderStep;
begin
  case AStep of
    esAll:
      for step := Succ(esAll) to High(step) do
        Run(step);
    esLoad:
      Load;
    esPreparePalettes:
      PreparePalettes;
    esCluster:
      Cluster;
    esReconstruct:
      Reconstruct;
    esSmooth:
      Smooth;
    esReindex:
      Reindex;
    esSave:
      Save;
  end;
end;

end.

