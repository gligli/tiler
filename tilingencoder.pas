unit tilingencoder;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}
{$TYPEDADDRESS ON}

{$define ASM_DBMP}


interface

uses
  windows, Classes, SysUtils, strutils, types, Math, FileUtil, typinfo, zstream, Process, LazLogger, IniFiles,
  Graphics, IntfGraphics, FPimage, FPCanvas, FPWritePNG, GraphType, fgl, MTProcs, extern, tbbmalloc, kmodes, sle;

type
  TEncoderStep = (esNone = -1, esLoad = 0, esMakeUnique, esDither, esGlobalTiling, esFrameTiling, esReindex, esSmooth, esSave);
  TRenderPage = (rpNone, rpInput, rpOutput, rpTilesPalette);

const
  // tweakable constants

  cBitsPerComp = 8;
  cFTQWeighting = False;
  cFTBinSize = 16;
  cYakmoMaxIterations = 300;
  cPerFrameTileCountMultiplier = 7;

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
  cYUVLumaFactor = 1.5;
  cYUVChromaFactor = 0.75;

  // don't change these

  cMaxFTBlend = 16;
  cLumaDiv = cRedMul + cGreenMul + cBlueMul;
  cSmoothingPrevFrame = 1;
  cVecInvWidth = 16;
  cTileWidthBits = 3;
  cTileWidth = 1 shl cTileWidthBits;
  cColorCpns = 3;
  cTileDCTSize = cColorCpns * sqr(cTileWidth) + 1;
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

  cEncoderStepLen: array[TEncoderStep] of Integer = (0, 4, 2, 1, 3, 1, 2, 2, 1);

  cQ = sqrt(16);
  cDCTQuantization: array[0..cColorCpns-1{YUV}, 0..7, 0..7] of TFloat = (
    (
      // Luma
      (cQ / sqrt(16), cQ / sqrt( 11), cQ / sqrt( 10), cQ / sqrt( 16), cQ / sqrt( 24), cQ / sqrt( 40), cQ / sqrt( 51), cQ / sqrt( 61)),
      (cQ / sqrt(12), cQ / sqrt( 12), cQ / sqrt( 14), cQ / sqrt( 19), cQ / sqrt( 26), cQ / sqrt( 58), cQ / sqrt( 60), cQ / sqrt( 55)),
      (cQ / sqrt(14), cQ / sqrt( 13), cQ / sqrt( 16), cQ / sqrt( 24), cQ / sqrt( 40), cQ / sqrt( 57), cQ / sqrt( 69), cQ / sqrt( 56)),
      (cQ / sqrt(14), cQ / sqrt( 17), cQ / sqrt( 22), cQ / sqrt( 29), cQ / sqrt( 51), cQ / sqrt( 87), cQ / sqrt( 80), cQ / sqrt( 62)),
      (cQ / sqrt(18), cQ / sqrt( 22), cQ / sqrt( 37), cQ / sqrt( 56), cQ / sqrt( 68), cQ / sqrt(109), cQ / sqrt(103), cQ / sqrt( 77)),
      (cQ / sqrt(24), cQ / sqrt( 35), cQ / sqrt( 55), cQ / sqrt( 64), cQ / sqrt( 81), cQ / sqrt(104), cQ / sqrt(113), cQ / sqrt( 92)),
      (cQ / sqrt(49), cQ / sqrt( 64), cQ / sqrt( 78), cQ / sqrt( 87), cQ / sqrt(103), cQ / sqrt(121), cQ / sqrt(120), cQ / sqrt(101)),
      (cQ / sqrt(72), cQ / sqrt( 92), cQ / sqrt( 95), cQ / sqrt( 98), cQ / sqrt(112), cQ / sqrt(100), cQ / sqrt(103), cQ / sqrt( 99))
    ),
    (
      // U, weighted by luma importance
      (cQ / sqrt(17), cQ / sqrt( 18), cQ / sqrt( 24), cQ / sqrt( 47), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt( 99)),
      (cQ / sqrt(18), cQ / sqrt( 21), cQ / sqrt( 26), cQ / sqrt( 66), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt(112)),
      (cQ / sqrt(24), cQ / sqrt( 26), cQ / sqrt( 56), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt(112), cQ / sqrt(128)),
      (cQ / sqrt(47), cQ / sqrt( 66), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt(112), cQ / sqrt(128), cQ / sqrt(144)),
      (cQ / sqrt(99), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt(112), cQ / sqrt(128), cQ / sqrt(144), cQ / sqrt(160)),
      (cQ / sqrt(99), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt(112), cQ / sqrt(128), cQ / sqrt(144), cQ / sqrt(160), cQ / sqrt(176)),
      (cQ / sqrt(99), cQ / sqrt( 99), cQ / sqrt(112), cQ / sqrt(128), cQ / sqrt(144), cQ / sqrt(160), cQ / sqrt(176), cQ / sqrt(192)),
      (cQ / sqrt(99), cQ / sqrt(112), cQ / sqrt(128), cQ / sqrt(144), cQ / sqrt(160), cQ / sqrt(176), cQ / sqrt(192), cQ / sqrt(208))
    ),
    (
      // V, weighted by luma importance
      (cQ / sqrt(17), cQ / sqrt( 18), cQ / sqrt( 24), cQ / sqrt( 47), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt( 99)),
      (cQ / sqrt(18), cQ / sqrt( 21), cQ / sqrt( 26), cQ / sqrt( 66), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt(112)),
      (cQ / sqrt(24), cQ / sqrt( 26), cQ / sqrt( 56), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt(112), cQ / sqrt(128)),
      (cQ / sqrt(47), cQ / sqrt( 66), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt(112), cQ / sqrt(128), cQ / sqrt(144)),
      (cQ / sqrt(99), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt(112), cQ / sqrt(128), cQ / sqrt(144), cQ / sqrt(160)),
      (cQ / sqrt(99), cQ / sqrt( 99), cQ / sqrt( 99), cQ / sqrt(112), cQ / sqrt(128), cQ / sqrt(144), cQ / sqrt(160), cQ / sqrt(176)),
      (cQ / sqrt(99), cQ / sqrt( 99), cQ / sqrt(112), cQ / sqrt(128), cQ / sqrt(144), cQ / sqrt(160), cQ / sqrt(176), cQ / sqrt(192)),
      (cQ / sqrt(99), cQ / sqrt(112), cQ / sqrt(128), cQ / sqrt(144), cQ / sqrt(160), cQ / sqrt(176), cQ / sqrt(192), cQ / sqrt(208))
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
  // SkipBlock:         data -> none; commandBits -> skip count - 1 (10 bits)
  // ShortTileIdx:      data -> tile index (16 bits); commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
  // LongTileIdx:       data -> tile index (32 bits); commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
  // LoadPalette:       data -> palette index (8 bits); palette format (8 bits) (0: RGBA32); RGBA bytes (32bits); commandBits -> none
  // ShortBlendTileIdx: data -> tile index (16 bits); BlendY offset(4 bits); BlendX offset(4 bits); Previous frame factor (4 bits); Current frame factor (4 bits);
  //                      commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
  // LongLendTileIdx:   data -> tile index (32 bits); BlendY offset(4 bits); BlendX offset(4 bits); Previous frame factor (4 bits); Current frame factor (4 bits);
  //                      commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
  //
  // (insert new commands here...)
  //
  // FrameEnd:          data -> none; commandBits bit 0 -> keyframe end
  // TileSet:           data -> start tile (32 bits); end tile (32 bits); { indexes per tile (64 bytes) } * count; commandBits -> indexes count per palette
  // SetDimensions:     data -> height in tiles (16 bits); width in tiles (16 bits); frame length in nanoseconds (32 bits) (2^32-1: still frame); tile count (32 bits); commandBits -> none
  // ExtendedCommand:   data -> custom commands, proprietary extensions, ...; commandBits -> extended command index (10 bits)
  //
  // ReservedArea:      reserving the MSB for future use (do not use for new commands)

  TGTMCommand = (
    gtSkipBlock = 0,
    gtShortTileIdx = 1,
    gtLongTileIdx = 2,
    gtLoadPalette = 3,
    gtShortBlendTileIdx = 4,
    gtLongBlendTileIdx = 5,

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

  { TTile }

  TTile = packed record // /!\ update TTileHelper.CopyFrom each time this structure is changed /!\
    UseCount: Cardinal;
    Weight: TFloat;
    TmpIndexUpper, MergeIndexUpper: Integer;
    KFSoleIndex: SmallInt;
    TmpIndexLower, MergeIndexLower: Byte;
    Flags: set of (tfActive, tfIntraKF, tfAdditional, tfHasRGBPixels, tfHasPalPixels);
  end;

  PTileArray = array of PTile;

  { TTileHelper }

  TTileHelper = record helper for TTile
  private
    function GetActive: Boolean;
    function GetAdditional: Boolean;
    function GetHasPalPixels: Boolean;
    function GetHasRGBPixels: Boolean;
    function GetIntraKF: Boolean;
    function GetMergeIndex: Int64;
    function GetTmpIndex: Int64;
    procedure SetActive(AValue: Boolean);
    procedure SetAdditional(AValue: Boolean);
    procedure SetHasPalPixels(AValue: Boolean);
    procedure SetHasRGBPixels(AValue: Boolean);
    procedure SetIntraKF(AValue: Boolean);
    procedure SetMergeIndex(AValue: Int64);
    procedure SetTmpIndex(AValue: Int64);
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
    property IntraKF: Boolean read GetIntraKF write SetIntraKF;
    property Additional: Boolean read GetAdditional write SetAdditional;
    property HasRGBPixels: Boolean read GetHasRGBPixels write SetHasRGBPixels;
    property HasPalPixels: Boolean read GetHasPalPixels write SetHasPalPixels;
    property TmpIndex: Int64 read GetTmpIndex write SetTmpIndex;
    property MergeIndex: Int64 read GetMergeIndex write SetMergeIndex;
  end;

  { TTileMapItem }

  TTileMapItem = packed record
    TileIdxUpper, SmoothedTileIdxUpper, FTTileDSIdx: Integer;
    PalIdx, SmoothedPalIdx: SmallInt;
    TileIdxLower, SmoothedTileIdxLower: Byte;
    BlendCur, BlendPrev: Byte;
    BlendX, BlendY: ShortInt;
    Flags: set of (tmfSmoothed, tmfHMirror, tmfVMirror, tmfSmoothedHMirror, tmfSmoothedVMirror);
  end;
  PTileMapItem = ^TTileMapItem;

  TTileMapItems = array of TTileMapItem;

  { TTileMapItemHelper }

  TTileMapItemHelper = record helper for TTileMapItem
  private
    function GetHMirror: Boolean;
    function GetSmoothed: Boolean;
    function GetSmoothedHMirror: Boolean;
    function GetSmoothedTileIdx: Int64;
    function GetSmoothedVMirror: Boolean;
    function GetTileIdx: Int64;
    function GetVMirror: Boolean;
    procedure SetHMirror(AValue: Boolean);
    procedure SetSmoothed(AValue: Boolean);
    procedure SetSmoothedHMirror(AValue: Boolean);
    procedure SetSmoothedTileIdx(AValue: Int64);
    procedure SetSmoothedVMirror(AValue: Boolean);
    procedure SetTileIdx(AValue: Int64);
    procedure SetVMirror(AValue: Boolean);
  public
    property Smoothed: Boolean read GetSmoothed write SetSmoothed;
    property HMirror: Boolean read GetHMirror write SetHMirror;
    property VMirror: Boolean read GetVMirror write SetVMirror;
    property SmoothedHMirror: Boolean read GetSmoothedHMirror write SetSmoothedHMirror;
    property SmoothedVMirror: Boolean read GetSmoothedVMirror write SetSmoothedVMirror;
    property TileIdx: Int64 read GetTileIdx write SetTileIdx;
    property SmoothedTileIdx: Int64 read GetSmoothedTileIdx write SetSmoothedTileIdx;
  end;


  { TTilingDataset }

  TTilingDataset = record
    Dataset: TFloatDynArray;
    TDToTileIdx: TInt64DynArray;
    TDToAttrs: TByteDynArray;
    FLANN: flann_index_t;
    FLANNParams: PFLANNParameters;
  end;

  PTilingDataset = ^TTilingDataset;

  { TCountIndex }

  TCountIndex = record
    Index, Count: Integer;
    R, G, B, Luma: Byte;
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
    bestErr: TFloat;
    bestIdx: Integer;
    bestBlendCur: Integer;
    bestBlendPrev: Integer;
    bestX: Integer;
    bestY: Integer;
  end;

  TKeyFrame = class;

  { TFrame }

  TFrame = class
    PKeyFrame: TKeyFrame;
    Index: Integer;

    TileMap: array of array of TTileMapItem;

    FrameTiles: array of PTile;

    FrameTilesRefCount: Integer;
    FrameTilesEvent: THandle;
    CompressedFrameTiles: TMemoryStream;

    procedure CompressFrameTiles;
    procedure AcquireFrameTiles;
    procedure ReleaseFrameTiles;

    constructor Create;
    destructor Destroy; override;
  end;

  TFrameArray =  array of TFrame;

  { TKeyFrame }

  TKeyFrame = class
    Index, StartFrame, EndFrame, FrameCount: Integer;
    FramesLeft: Integer;

    PaletteRGB: TIntegerDynArray2;
    PaletteCentroids: TDoubleDynArray2;
    CS: TRTLCriticalSection;
    MixingPlans: array of TMixingPlan;
    TileDS: array of PTilingDataset;

    PaletteUseCount: array of record
      UseCount: Integer;
      Palette: TIntegerDynArray;
      PalIdx: Integer;
    end;

    FTPaletteDoneEvent: array of THandle;
    PalettesLeft: Integer;
    FTErrCml: Double;

    constructor Create(AIndex, APaletteCount, AStartFrame, AEndFrame: Integer);
    destructor Destroy; override;
  end;

  TKeyFrameArray =  array of TKeyFrame;

  TTilingEncoder = class;

  TTilingEncoderProgressEvent = procedure(ASender: TTilingEncoder; APosition, AMax: Integer; AHourGlass: Boolean) of object;

  { TTilingEncoder }

  TTilingEncoder = class
  private
    // encoder state variables

    FCS: TRTLCriticalSection;
    FLock: TSpinlock;

    FGamma: array[0..1] of TFloat;
    FGammaCorLut: array[-1..1, 0..High(Byte)] of TFloat;
    FVecInv: array[0..256 * 4 - 1] of Cardinal;
    FDCTLut:array[0..sqr(sqr(cTileWidth)) - 1] of TFloat;
    FDCTLutDouble:array[0..sqr(sqr(cTileWidth)) - 1] of Double;
    FInvDCTLut:array[0..sqr(sqr(cTileWidth)) - 1] of TFloat;
    FInvDCTLutDouble:array[0..sqr(sqr(cTileWidth)) - 1] of Double;

    FKeyFrames: TKeyFrameArray;
    FFrames: TFrameArray;
    FTiles: PTileArray;
    FAdditionalTiles: TThreadList;

    // video properties

    FInputPath: String;
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
    FScaling: TFloat;
    FEncoderGammaValue: TFloat;
    FPaletteSize: Integer;
    FPaletteCount: Integer;
    FQuantizerUseYakmo: Boolean;
    FQuantizerDennisLeeBitsPerComponent: Integer;
    FDitheringUseGamma: Boolean;
    FDitheringUseThomasKnoll: Boolean;
    FDitheringYliluoma2MixedColors: Integer;
    FGlobalTilingTileCount: Integer;
    FGlobalTilingQualityBasedTileCount: TFloat;
    FGlobalTilingSoftClusteringThreshold: TFloat;
    FReloadTileset: Boolean;
    FReloadTilesetFileName: String;
    FFrameTilingUseGamma: Boolean;
    FFrameTilingBlendingSize: Integer;
    FFrameTilingBlendingThreshold: TFloat;
    FSmoothingFactor: TFloat;
    FSmoothingAdditionalTilesThreshold: TFloat;

    // GUI state variables

    FRenderBlended: Boolean;
    FRenderFrameIndex: Integer;
    FRenderGammaValue: TFloat;
    FRenderPage: TRenderPage;
    FRenderPsychoVisualQuality: TFloat;
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
    FProgressStartTime, FProgressPrevTime: Int64;

    function GetFrameCount: Integer;
    function GetKeyFrameCount: Integer;
    procedure SetDitheringYliluoma2MixedColors(AValue: Integer);
    procedure SetEncoderGammaValue(AValue: TFloat);
    procedure SetFrameCountSetting(AValue: Integer);
    procedure SetFramesPerSecond(AValue: Double);
    procedure SetFrameTilingBlendingSize(AValue: Integer);
    procedure SetFrameTilingBlendingThreshold(AValue: TFloat);
    procedure SetGlobalTilingQualityBasedTileCount(AValue: TFloat);
    procedure SetPaletteCount(AValue: Integer);
    procedure SetPaletteSize(AValue: Integer);
    procedure SetQuantizerDennisLeeBitsPerComponent(AValue: Integer);
    procedure SetRenderFrameIndex(AValue: Integer);
    procedure SetRenderGammaValue(AValue: TFloat);
    procedure SetRenderPaletteIndex(AValue: Integer);
    procedure SetRenderTilePage(AValue: Integer);
    procedure SetGlobalTilingTileCount(AValue: Integer);
    procedure SetScaling(AValue: TFloat);
    procedure SetSmoothingAdditionalTilesThreshold(AValue: TFloat);
    procedure SetSmoothingFactor(AValue: TFloat);
    procedure SetStartFrame(AValue: Integer);

    function PearsonCorrelation(const x: TFloatDynArray; const y: TFloatDynArray): TFloat;
    function ComputeCorrelationBGR(const a: TIntegerDynArray; const b: TIntegerDynArray): TFloat;
    function ComputeDistanceRGB(const a: TIntegerDynArray; const b: TIntegerDynArray): TFloat;
    function ComputeInterFrameCorrelation(a, b: TFrame; out EuclideanDist: TFloat): TFloat;

    procedure ClearAll;
    procedure ProgressRedraw(ASubStepIdx: Integer = -1; AProgressStep: TEncoderStep = esNone);
    procedure InitLuts(ATilePaletteSize, APaletteCount: Integer);
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

    procedure WaveletGS(Data: PFloat; Output: PFloat; dx, dy, depth: cardinal);
    procedure DeWaveletGS(wl: PFloat; pic: PFloat; dx, dy, depth: longint);
    procedure ComputeTilePsyVisFeatures(const ATile: TTile; FromPal, UseLAB, QWeighting, HMirror,
      VMirror: Boolean; GammaCor: Integer; const pal: TIntegerDynArray; DCT: PFloat); inline; overload;
    procedure ComputeTilePsyVisFeatures(const ATile: TTile; FromPal, UseLAB, QWeighting, HMirror, VMirror: Boolean;
      GammaCor: Integer; const pal: TIntegerDynArray; DCT: PDouble); inline; overload;
    procedure ComputeInvTilePsyVisFeatures(DCT: PDouble; UseLAB: Boolean; GammaCor: Integer; var ATile: TTile);
    procedure ComputeInvTilePsyVisFeatures(DCT: PFloat; UseLAB: Boolean; GammaCor: Integer; var ATile: TTile);

    // Dithering algorithms ported from http://bisqwit.iki.fi/story/howto/dither/jy/

    class function ColorCompare(r1, g1, b1, r2, g2, b2: Int64): Int64;
    procedure PreparePlan(var Plan: TMixingPlan; MixedColors: Integer; const pal: array of Integer);
    procedure TerminatePlan(var Plan: TMixingPlan);
    function DeviseBestMixingPlanYliluoma(var Plan: TMixingPlan; col: Integer; var List: array of Byte): Integer;
    procedure DeviseBestMixingPlanThomasKnoll(var Plan: TMixingPlan; col: Integer; var List: array of Byte);
    procedure DitherTileFloydSteinberg(ATile: TTile; out RGBPixels: TRGBPixels);

    procedure LoadFrame(var AFrame: TFrame; AFrameIndex: Integer; const ABitmap: TRawImage);
    procedure FindKeyFrames;
    procedure LoadTiles;
    function GetGlobalTileCount: Integer;
    function GetFrameTileCount(AFrame: TFrame): Integer;
    function GetTileIndexTMItem(const ATile: TTile; out AFrame: TFrame): PTileMapItem;

    procedure GetKeyFrameTileList(AKeyFrame: TKeyFrame; var ATileList: TInt64DynArray; var ATilesUseCount: TIntegerDynArray; APaletteIndex: Integer = -1);
    procedure PreparePalettes(AKeyFrame: TKeyFrame; ADitheringGamma: Integer);
    procedure QuantizePalette(AKeyFrame: TKeyFrame; APalIdx: Integer; UseYakmo: Boolean; DLv3BPC: Integer; ADitheringGamma: Integer);
    procedure FinishQuantizePalettes(AKeyFrame: TKeyFrame);
    procedure DitherTile(var ATile: TTile; var Plan: TMixingPlan);
    procedure DitherTiles(AKeyFrame: TKeyFrame);

    function GetTileZoneSum(const ATile: TTile; x, y, w, h: Integer): Integer;
    function GetTilePalZoneThres(const ATile: TTile; ZoneCount: Integer; Zones: PByte): Integer;
    procedure MakeTilesUnique(FirstTileIndex, TileCount: Int64);
    procedure PackTiles;
    procedure MergeTiles(const TileIndexes: array of Int64; TileCount: Integer; BestIdx: Int64; NewTile: PPalPixels; NewTileRGB: PRGBPixels);
    procedure InitMergeTiles;
    procedure FinishMergeTiles;
    procedure DoGlobalKMeans(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
    procedure DoGlobalTiling(DesiredNbTiles: Integer);

    procedure ReloadPreviousTiling(AFN: String);

    procedure HMirrorTile(var ATile: TTile);
    procedure VMirrorTile(var ATile: TTile);
    procedure PrepareKFTiling(AKF: TKeyFrame; APaletteIndex, AFTGamma: Integer);
    procedure FinishKFTiling(AKF: TKeyFrame;  APaletteIndex: Integer);
    procedure DoTileBlending(AFrame: TFrame; APaletteIndex, APrevKFPaletteIndex, AFTGamma, AFTBlend, x, y: Integer;
     APlainDCT: PFloat; ACurIdxs: PInteger; ACurErrs: PFloat; var AKFTilingBest: TKFTilingBest);
    procedure DoKFTiling(AKF: TKeyFrame; APaletteIndex: Integer; AFTGamma: Integer; AFTBlend: Integer; AFTBlendThres: TFloat);

    function GetTileUseCount(ATileIndex: Int64): Integer;
    procedure ReindexTiles(KeepRGBPixels: Boolean);
    procedure DoTemporalSmoothing(AFrame, APrevFrame: TFrame; Y: Integer; Strength: TFloat; AddlTilesThres: TFloat;
      NonAddlCount: Int64);

    procedure SaveStream(AStream: TStream; ASpecificKF: Integer = -1);
    procedure SaveRawTiles(OutFN: String);

    procedure ReframeUI(AWidth, AHeight: Integer);
    function DoExternalFFMpeg(AFN: String; var AVidPath: String; AStartFrame, AFrameCount: Integer; AScale: Double; out
      AFPS: Double): String;
  public
    // constructor / destructor

    constructor Create;
    destructor Destroy; override;

    // processes / functions

    procedure Load;
    procedure MakeUnique;
    procedure Dither;
    procedure GlobalTiling;
    procedure FrameTiling;
    procedure ReColor;
    procedure Reindex;
    procedure Smooth;
    procedure Save;
    procedure BitrateLoop(ABitrate: TFloat);

    procedure Render(AFast: Boolean);
    procedure GeneratePNGs;
    procedure SaveSettings(ASettingsFileName: String);
    procedure LoadSettings(ASettingsFileName: String);

    procedure Test;

    // encoder state variables

    property KeyFrames: TKeyFrameArray read FKeyFrames;
    property Frames: TFrameArray read FFrames;
    property Tiles: PTileArray read FTiles;

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
    property Scaling: TFloat read FScaling write SetScaling;
    property EncoderGammaValue: TFloat read FEncoderGammaValue write SetEncoderGammaValue;
    property PaletteSize: Integer read FPaletteSize write SetPaletteSize;
    property PaletteCount: Integer read FPaletteCount write SetPaletteCount;
    property QuantizerUseYakmo: Boolean read FQuantizerUseYakmo write FQuantizerUseYakmo;
    property QuantizerDennisLeeBitsPerComponent: Integer read FQuantizerDennisLeeBitsPerComponent write SetQuantizerDennisLeeBitsPerComponent;
    property DitheringUseGamma: Boolean read FDitheringUseGamma write FDitheringUseGamma;
    property DitheringUseThomasKnoll: Boolean read FDitheringUseThomasKnoll write FDitheringUseThomasKnoll;
    property DitheringYliluoma2MixedColors: Integer read FDitheringYliluoma2MixedColors write SetDitheringYliluoma2MixedColors;
    property GlobalTilingTileCount: Integer read FGlobalTilingTileCount write SetGlobalTilingTileCount;
    property GlobalTilingQualityBasedTileCount: TFloat read FGlobalTilingQualityBasedTileCount write SetGlobalTilingQualityBasedTileCount;
    property ReloadTileset: Boolean read FReloadTileset write FReloadTileset;
    property ReloadTilesetFileName: String read FReloadTilesetFileName write FReloadTilesetFileName;
    property FrameTilingUseGamma: Boolean read FFrameTilingUseGamma write FFrameTilingUseGamma;
    property FrameTilingBlendingSize: Integer read FFrameTilingBlendingSize write SetFrameTilingBlendingSize;
    property FrameTilingBlendingThreshold: TFloat read FFrameTilingBlendingThreshold write SetFrameTilingBlendingThreshold;
    property GlobalTilingSoftClusteringThreshold: TFloat read FGlobalTilingSoftClusteringThreshold write FGlobalTilingSoftClusteringThreshold;
    property SmoothingFactor: TFloat read FSmoothingFactor write SetSmoothingFactor;
    property SmoothingAdditionalTilesThreshold: TFloat read FSmoothingAdditionalTilesThreshold write SetSmoothingAdditionalTilesThreshold;

    // GUI state variables

    property RenderPlaying: Boolean read FRenderPlaying write FRenderPlaying;
    property RenderFrameIndex: Integer read FRenderFrameIndex write SetRenderFrameIndex;
    property RenderBlended: Boolean read FRenderBlended write FRenderBlended;
    property RenderMirrored: Boolean read FRenderMirrored write FRenderMirrored;
    property RenderSmoothed: Boolean read FRenderSmoothed write FRenderSmoothed;
    property RenderDithered: Boolean read FRenderDithered write FRenderDithered;
    property RenderUseGamma: Boolean read FRenderUseGamma write FRenderUseGamma;
    property RenderPaletteIndex: Integer read FRenderPaletteIndex write SetRenderPaletteIndex;
    property RenderTilePage: Integer read FRenderTilePage write SetRenderTilePage;
    property RenderGammaValue: TFloat read FRenderGammaValue write SetRenderGammaValue;
    property RenderPage: TRenderPage read FRenderPage write FRenderPage;
    property RenderTitleText: String read FRenderTitleText;
    property RenderPsychoVisualQuality: TFloat read FRenderPsychoVisualQuality;
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

function ToLuma(r, g, b: Byte): Byte; inline;
begin
  Result := (r * cRedMul + g * cGreenMul + b * cBlueMul) div cLumaDiv;
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
  Result := sqrt(Result);
end;

function CompareEuclidean(a, b: PDouble; size: Integer): Double; inline;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to size - 1 do
    Result += sqr(a[i] - b[i]);
  Result := sqrt(Result);
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

function ComputeBlendingError(PPlain, PCur, PPrev: PFloat; bc, bp: TFloat): TFloat; inline;
var
  i: Integer;
begin
  Result := 0.0;
  for i := 0 to cTileDCTSize div 8 - 1 do // unroll by 8
  begin
    Result += Sqr(PPlain^ - (PCur^ * bc + Pprev^ * bp)); Inc(PPlain); Inc(PCur); Inc(PPrev);
    Result += Sqr(PPlain^ - (PCur^ * bc + Pprev^ * bp)); Inc(PPlain); Inc(PCur); Inc(PPrev);
    Result += Sqr(PPlain^ - (PCur^ * bc + Pprev^ * bp)); Inc(PPlain); Inc(PCur); Inc(PPrev);
    Result += Sqr(PPlain^ - (PCur^ * bc + Pprev^ * bp)); Inc(PPlain); Inc(PCur); Inc(PPrev);
    Result += Sqr(PPlain^ - (PCur^ * bc + Pprev^ * bp)); Inc(PPlain); Inc(PCur); Inc(PPrev);
    Result += Sqr(PPlain^ - (PCur^ * bc + Pprev^ * bp)); Inc(PPlain); Inc(PCur); Inc(PPrev);
    Result += Sqr(PPlain^ - (PCur^ * bc + Pprev^ * bp)); Inc(PPlain); Inc(PCur); Inc(PPrev);
    Result += Sqr(PPlain^ - (PCur^ * bc + Pprev^ * bp)); Inc(PPlain); Inc(PCur); Inc(PPrev);
  end;
end;

const
  CvtPre =  (1 shl cBitsPerComp) - 1;
  CvtPost = 256 div CvtPre;

function Posterize(v: Integer): Byte; inline;
begin
  Result := min(255, (((v * CvtPre) div 255) * CvtPost));
end;

function Decimate(col: Integer): Integer; inline;
var
  r, g, b: Byte;
begin
  FromRGB(col, r, g, b);
  r := r shr (8 - cBitsPerComp);
  g := g shr (8 - cBitsPerComp);
  b := b shr (8 - cBitsPerComp);
  Result := r or (g shl cBitsPerComp) or (b shl (cBitsPerComp * 2));
end;

function EqualQualityTileCount(tileCount: TFloat): Integer;
begin
  Result := round(sqrt(tileCount) * log2(1 + tileCount));
end;

function GoldenRatioSearch(Func: TFloatFloatFunction; Mini, Maxi: TFloat; ObjectiveY: TFloat = 0.0; Epsilon: TFloat = 1e-12; Data: Pointer = nil): TFloat;
var
  x, y: TFloat;
begin
  if SameValue(Mini, Maxi) then
  begin
    WriteLn('GoldenRatioSearch failed!');
    Result := NaN;
    Exit;
  end;

  if Mini < Maxi then
    x := lerp(Mini, Maxi, 1.0 - cInvPhi)
  else
    x := lerp(Mini, Maxi, cInvPhi);

  y := Func(x, Data);

  WriteLn('X: ', FormatFloat('0.000', x), #9'Y: ', FormatFloat('0.000', y), #9'Mini: ', FormatFloat('0.000', Mini), #9'Maxi: ', FormatFloat('0.000', Maxi));

  case CompareValue(y, ObjectiveY, Epsilon) of
    LessThanValue:
      Result := GoldenRatioSearch(Func, x, Maxi, ObjectiveY, Epsilon, Data);
    GreaterThanValue:
      Result := GoldenRatioSearch(Func, Mini, x, ObjectiveY, Epsilon, Data);
  else
      Result := x;
  end;
end;

{ TTileMapItemHelper }

function TTileMapItemHelper.GetHMirror: Boolean;
begin
  Result := tmfHMirror in Flags;
end;

function TTileMapItemHelper.GetSmoothed: Boolean;
begin
  Result := tmfSmoothed in Flags;
end;

function TTileMapItemHelper.GetSmoothedHMirror: Boolean;
begin
  Result := tmfSmoothedHMirror in Flags;
end;

function TTileMapItemHelper.GetSmoothedTileIdx: Int64;
begin
  Result := Int64(SmoothedTileIdxLower) or (Int64(SmoothedTileIdxUpper) shl 8);
end;

function TTileMapItemHelper.GetSmoothedVMirror: Boolean;
begin
  Result := tmfSmoothedVMirror in Flags;
end;

function TTileMapItemHelper.GetTileIdx: Int64;
begin
  Result := Int64(TileIdxLower) or (Int64(TileIdxUpper) shl 8);
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

procedure TTileMapItemHelper.SetSmoothed(AValue: Boolean);
begin
  if AValue then
    Flags += [tmfSmoothed]
  else
    Flags -= [tmfSmoothed];
end;

procedure TTileMapItemHelper.SetSmoothedHMirror(AValue: Boolean);
begin
  if AValue then
    Flags += [tmfSmoothedHMirror]
  else
    Flags -= [tmfSmoothedHMirror];
end;

procedure TTileMapItemHelper.SetSmoothedTileIdx(AValue: Int64);
begin
  SmoothedTileIdxLower := AValue and $ff;
  SmoothedTileIdxUpper := Integer(AValue shr 8);
end;

procedure TTileMapItemHelper.SetSmoothedVMirror(AValue: Boolean);
begin
  if AValue then
    Flags += [tmfSmoothedVMirror]
  else
    Flags -= [tmfSmoothedVMirror];
end;

procedure TTileMapItemHelper.SetTileIdx(AValue: Int64);
begin
  TileIdxLower := AValue and $ff;
  TileIdxUpper := Integer(AValue shr 8);
end;

procedure TTileMapItemHelper.SetVMirror(AValue: Boolean);
begin
  if AValue then
    Flags += [tmfVMirror]
  else
    Flags -= [tmfVMirror];
end;

{ TFrame }

procedure TFrame.CompressFrameTiles;
var
  CompStream: Tcompressionstream;
begin
  CompStream := Tcompressionstream.create(Tcompressionlevel.clfastest, CompressedFrameTiles, True);
  try
    CompStream.WriteBuffer(FrameTiles[0]^, Length(TileMap) * Length(TileMap[0]) * (SizeOf(TTile) + SizeOf(TRGBPixels)));
    CompStream.flush;
  finally
    CompStream.Free;
  end;

  // now that FrameTiles are compressed, dispose them

  TTile.Array1DDispose(FrameTiles);
end;

procedure TFrame.AcquireFrameTiles;
var
  CompStream: Tdecompressionstream;
begin
  if InterLockedIncrement(FrameTilesRefCount) = 1 then
  begin
    Assert(CompressedFrameTiles.Size > 0);

    CompressedFrameTiles.Position := 0;
    FrameTiles := TTile.Array1DNew(Length(TileMap) * Length(TileMap[0]), True, False);

    CompStream := Tdecompressionstream.create(CompressedFrameTiles, True);
    try
      CompStream.ReadBuffer(FrameTiles[0]^, Length(TileMap) * Length(TileMap[0]) * (SizeOf(TTile) + SizeOf(TRGBPixels)));
    finally
      CompStream.Free;
    end;

    // signal other threads decompression is done
    SetEvent(FrameTilesEvent);
  end
  else
  begin
    WaitForSingleObject(FrameTilesEvent, INFINITE);
  end;
end;

procedure TFrame.ReleaseFrameTiles;
var
  ftrc: Integer;
begin
  ftrc := InterLockedDecrement(FrameTilesRefCount);
  if ftrc <= 0 then
  begin
    ResetEvent(FrameTilesEvent);
    TTile.Array1DDispose(FrameTiles);
    Assert(ftrc = 0);
  end;
end;

constructor TFrame.Create;
begin
  CompressedFrameTiles := TMemoryStream.Create;
  FrameTilesEvent := CreateEvent(nil, True, False, nil);
end;

destructor TFrame.Destroy;
begin
  CompressedFrameTiles.Free;
  CloseHandle(FrameTilesEvent);

  inherited Destroy;
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

function TTileHelper.GetAdditional: Boolean;
begin
  Result := tfAdditional in Flags;
end;

function TTileHelper.GetHasPalPixels: Boolean;
begin
  Result := tfHasPalPixels in Flags;
end;

function TTileHelper.GetHasRGBPixels: Boolean;
begin
  Result := tfHasRGBPixels in Flags;
end;

function TTileHelper.GetIntraKF: Boolean;
begin
  Result := tfIntraKF in Flags;
end;

function TTileHelper.GetMergeIndex: Int64;
begin
  Result := Int64(MergeIndexLower) or (Int64(MergeIndexUpper) shl 8);
end;

function TTileHelper.GetTmpIndex: Int64;
begin
  Result := Int64(TmpIndexLower) or (Int64(TmpIndexUpper) shl 8);
end;

procedure TTileHelper.SetActive(AValue: Boolean);
begin
  if AValue then
    Flags += [tfActive]
  else
    Flags -= [tfActive];
end;

procedure TTileHelper.SetAdditional(AValue: Boolean);
begin
  if AValue then
    Flags += [tfAdditional]
  else
    Flags -= [tfAdditional];
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

procedure TTileHelper.SetIntraKF(AValue: Boolean);
begin
  if AValue then
    Flags += [tfIntraKF]
  else
    Flags -= [tfIntraKF];
end;

procedure TTileHelper.SetMergeIndex(AValue: Int64);
begin
  MergeIndexLower := AValue and $ff;
  MergeIndexUpper := Integer(AValue shr 8);
end;

procedure TTileHelper.SetTmpIndex(AValue: Int64);
begin
  TmpIndexLower := AValue and $ff;
  TmpIndexUpper := Integer(AValue shr 8);
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
  Result := CompareDWord(GetPalPixelsPtr^[0, 0], ATile.GetPalPixelsPtr^[0, 0], sqr(cTileWidth) div SizeOf(DWORD));
end;

function TTileHelper.CompareRGBPixelsTo(const ATile: TTile): Integer;
begin
  Result := CompareDWord(GetRGBPixelsPtr^[0, 0], ATile.GetRGBPixelsPtr^[0, 0], sqr(cTileWidth));
end;

procedure TTileHelper.CopyFrom(const ATile: TTile);
begin
  Active := ATile.Active;
  IntraKF := ATile.IntraKF;
  TmpIndex := ATile.TmpIndex;
  MergeIndex := ATile.MergeIndex;
  UseCount := ATile.UseCount;
  KFSoleIndex := ATile.KFSoleIndex;
  Additional := ATile.Additional;
  Weight := ATile.Weight;

  if HasPalPixels and ATile.HasPalPixels then
    CopyPalPixels(ATile.GetPalPixelsPtr^);
  if HasRGBPixels and ATile.HasRGBPixels then
    CopyRGBPixels(ATile.GetRGBPixelsPtr^);
end;

{ TKeyFrame }

constructor TKeyFrame.Create(AIndex, APaletteCount, AStartFrame, AEndFrame: Integer);
begin
  Assert(AIndex <= High(SmallInt), 'More than 32767 KeyFrames!');
  Index := AIndex;
  StartFrame := AStartFrame;
  EndFrame := AEndFrame;
  FrameCount := AEndFrame - AStartFrame + 1;
  FramesLeft := -1;
  InitializeCriticalSection(CS);
  SetLength(MixingPlans, APaletteCount);
  SetLength(PaletteRGB, APaletteCount);
end;

destructor TKeyFrame.Destroy;
begin
  DeleteCriticalSection(CS);
  inherited Destroy;
end;

{ TTilingEncoder }

procedure TTilingEncoder.InitLuts(ATilePaletteSize, APaletteCount: Integer);
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
  for v := 0 to cTileWidth - 1 do
    for u := 0 to cTileWidth - 1 do
      for y := 0 to cTileWidth - 1 do
        for x := 0 to cTileWidth - 1 do
        begin
          FDCTLutDouble[i] := cos((x + 0.5) * u * PI / cTileWidth) * cos((y + 0.5) * v * PI / cTileWidth) * cDCTUVRatio[Min(v, 7), Min(u, 7)];
          FDCTLut[i] := FDCTLutDouble[i];
          FInvDCTLutDouble[i] := cos((u + 0.5) * x * PI / cTileWidth) * cos((v + 0.5) * y * PI / cTileWidth) * cDCTUVRatio[Min(y, 7), Min(x, 7)] * 2 / cTileWidth * 2 / cTileWidth;
          FInvDCTLut[i] := FInvDCTLutDouble[i];
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
    x := power(x, 1 / FGamma[lut]);
  Result := EnsureRange(Round(x * 255.0), 0, 255);
end;

procedure TTilingEncoder.GlobalTiling;
begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(0, esGlobalTiling);

  if FReloadTileset then
  begin
    if not FileExists(FReloadTilesetFileName) then
      raise EFileNotFoundException.Create('File not found: ' + FReloadTilesetFileName);
    ReloadPreviousTiling(FReloadTilesetFileName);
  end
  else
  begin
    DoGlobalTiling(FGlobalTilingTileCount);

    SaveRawTiles(FReloadTilesetFileName);
  end;

  // ensure inter block tile unicity

  InitMergeTiles;
  MakeTilesUnique(0, Length(FTiles));
  FinishMergeTiles;

  PackTiles;

  ProgressRedraw(3);
end;

procedure TTilingEncoder.Dither;

  procedure DoDither(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    palIdx: Integer;
  begin
    if not InRange(AIndex, 0, High(FKeyFrames)) then
      Exit;

    PreparePalettes(FKeyFrames[AIndex], IfThen(FDitheringUseGamma, 0, -1));

    SetLength(FKeyFrames[AIndex].PaletteUseCount, FPaletteCount);
    FKeyFrames[AIndex].PalettesLeft := FPaletteCount;

    for palIdx := 0 to FPaletteCount - 1 do
      QuantizePalette(FKeyFrames[AIndex], palIdx, FQuantizerUseYakmo, FQuantizerDennisLeeBitsPerComponent, IfThen(FDitheringUseGamma, 0, -1));

    FinishQuantizePalettes(FKeyFrames[AIndex]);
    FKeyFrames[AIndex].PalettesLeft := -1;

    DitherTiles(FKeyFrames[AIndex]);

    SetLength(FKeyFrames[AIndex].PaletteUseCount, 0);
  end;

var
  tidx: Int64;
begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(0, esDither);

  for tidx := 0 to High(FTiles) do
    FTiles[tidx]^.TmpIndex := -1;

  ProcThreadPool.DoParallelLocalProc(@DoDither, 0, High(FKeyFrames));

  ProgressRedraw(1);
end;

procedure TTilingEncoder.MakeUnique;
var
  TilesAtATime: Int64;

  procedure DoMakeUnique(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    if not InRange(AIndex, 0, NumberOfProcessors - 1) then
      Exit;

    MakeTilesUnique(AIndex * TilesAtATime, Min(Length(FTiles) - AIndex * TilesAtATime, TilesAtATime));
  end;

begin
  TilesAtATime := ceil(Length(FTiles) / NumberOfProcessors);

  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(0, esMakeUnique);

  InitMergeTiles;
  ProcThreadPool.DoParallelLocalProc(@DoMakeUnique, 0, NumberOfProcessors - 1);
  FinishMergeTiles;

  ProgressRedraw(1);

  PackTiles;

  ProgressRedraw(2);
end;

function CompareFrames(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PInteger(Item2)^, PInteger(Item1)^);
end;

procedure TTilingEncoder.FrameTiling;

  procedure DoKFPal(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    kf, pal: Integer;
  begin
    DivMod(AIndex, FPaletteCount, kf, pal);

    PrepareKFTiling(FKeyFrames[kf], pal, IfThen(FFrameTilingUseGamma, 0, -1));

    DoKFTiling(FKeyFrames[kf], pal, IfThen(FFrameTilingUseGamma, 0, -1), FFrameTilingBlendingSize, FFrameTilingBlendingThreshold);

    FinishKFTiling(FKeyFrames[kf], pal);
  end;
var
  kf, i: Integer;
  tidx: Int64;
begin
  if Length(FKeyFrames) = 0 then
    Exit;

  ProgressRedraw(0, esFrameTiling);

  for kf := 0 to high(FKeyFrames) do
  begin
    FKeyFrames[kf].FTErrCml := 0.0;
    FKeyFrames[kf].PalettesLeft := FPaletteCount;
    SetLength(FKeyFrames[kf].TileDS, FPaletteCount);
    SetLength(FKeyFrames[kf].FTPaletteDoneEvent, FPaletteCount);
    for i := 0 to FPaletteCount - 1 do
      FKeyFrames[kf].FTPaletteDoneEvent[i] := CreateEvent(nil, True, False, nil);
  end;
  for tidx := 0 to High(FTiles) do
    FTiles[tidx]^.Active := False;

  try
    ProcThreadPool.DoParallelLocalProc(@DoKFPal, 0, FPaletteCount * Length(FKeyFrames) - 1, Pointer(0));
  finally
    for kf := 0 to high(FKeyFrames) do
    begin
      for i := 0 to FPaletteCount - 1 do
        CloseHandle(FKeyFrames[kf].FTPaletteDoneEvent[i]);
      SetLength(FKeyFrames[kf].FTPaletteDoneEvent, 0);
      SetLength(FKeyFrames[kf].TileDS, 0);
    end;
  end;

  ProgressRedraw(1);
end;

procedure TTilingEncoder.ReColor;

  procedure DoReColor(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    sx, sy, tx, ty, mtx, mty, frmIdx, palIdx, colIdx, cnt: Integer;
    r, g, b, pr, pg, pb: Byte;
    Frame, PrevFrame: TFrame;
    TMI, PrevTMI: PTileMapItem;
    FTTile, TMTile: PTile;
    Accumulators: array of array of array[0 .. cColorCpns - 1 + 1] of Int64;
  begin
    if not InRange(AIndex, 0, High(FKeyFrames)) then
      Exit;

    SetLength(Accumulators, FPaletteCount, FPaletteSize);

    for frmIdx := FKeyFrames[AIndex].StartFrame to FKeyFrames[AIndex].EndFrame do
    begin
      Frame := FFrames[frmIdx];
      PrevFrame := nil;
      if frmIdx > 0 then
        PrevFrame := FFrames[frmIdx - 1];

      Frame.AcquireFrameTiles;
      try
        for sy := 0 to FTileMapHeight - 1 do
          for sx := 0 to FTileMapWidth - 1 do
          begin
            TMI := @Frame.TileMap[sy, sx];
            FTTile := Frame.FrameTiles[sy * FTileMapWidth + sx];
            TMTile := FTiles[TMI^.TileIdx];

            for ty := 0 to cTileWidth - 1 do
              for tx := 0 to cTileWidth - 1 do
              begin
                // tilemap tile is potentially mirrored
                mtx := tx;
                mty := ty;
                if TMI^.HMirror then mtx := cTileWidth - 1 - mtx;
                if TMI^.VMirror then mty := cTileWidth - 1 - mty;

                colIdx := TMTile^.PalPixels[mty, mtx];

                FromRGB(FTTile^.RGBPixels[ty, tx], r, g, b);

                pr := 0; pg := 0; pb := 0;
                if Assigned(PrevFrame) then
                begin
                  PrevTMI := @PrevFrame.TileMap[sy + TMI^.BlendY, sx + TMI^.BlendX];

                  mtx := tx;
                  mty := ty;
                  if PrevTMI^.HMirror then mtx := cTileWidth - 1 - mtx;
                  if PrevTMI^.VMirror then mty := cTileWidth - 1 - mty;

                  FromRGB(PrevFrame.PKeyFrame.PaletteRGB[PrevTMI^.PalIdx, FTiles[PrevTMI^.TileIdx]^.PalPixels[mty, mtx]], pr, pg, pb);
                end;

                Accumulators[TMI^.PalIdx, colIdx, 0] += EnsureRange((r * cMaxFTBlend - pr * TMI^.BlendPrev) div TMI^.BlendCur, 0, 255);
                Accumulators[TMI^.PalIdx, colIdx, 1] += EnsureRange((g * cMaxFTBlend - pg * TMI^.BlendPrev) div TMI^.BlendCur, 0, 255);
                Accumulators[TMI^.PalIdx, colIdx, 2] += EnsureRange((b * cMaxFTBlend - pb * TMI^.BlendPrev) div TMI^.BlendCur, 0, 255);
                Accumulators[TMI^.PalIdx, colIdx, 3] += 1;
              end;
          end;
      finally
        Frame.ReleaseFrameTiles;
      end;
    end;

    for palIdx := 0 to FPaletteCount - 1 do
      for colIdx := 0 to FPaletteSize - 1 do
      begin
        cnt := Accumulators[palIdx, colIdx, 3];
        r := iDiv0(Accumulators[palIdx, colIdx, 0] + cnt div 2, cnt);
        g := iDiv0(Accumulators[palIdx, colIdx, 1] + cnt div 2, cnt);
        b := iDiv0(Accumulators[palIdx, colIdx, 2] + cnt div 2, cnt);

        FKeyFrames[AIndex].PaletteRGB[palIdx, colIdx] := ToRGB(r, g, b);
      end;
  end;

begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(0, esDither);

  ProcThreadPool.DoParallelLocalProc(@DoReColor, 0, High(FKeyFrames));

  ProgressRedraw(1);
end;

procedure TTilingEncoder.Load;

  procedure DoLoadFrame(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    bmp: TPortableNetworkGraphic;
  begin
    if not InRange(AIndex, 0, High(FFrames)) then
      Exit;

    bmp := TPortableNetworkGraphic.Create;
    try
      bmp.PixelFormat:=pf32bit;
      bmp.LoadFromFile(Format(FInputPath, [AIndex + PtrUInt(AData)]));

      LoadFrame(FFrames[AIndex], AIndex, bmp.RawImage);

      FFrames[AIndex].Index := AIndex;

      Write('.');
    finally
      bmp.Free;
    end;
  end;

var
  i, frc, startFrmIdx: Integer;
  fn: String;
  bmp: TPicture;
  wasAutoQ: Boolean;
  qbTC: TFloat;
begin
  wasAutoQ := (Length(FFrames) > 0) and (FGlobalTilingTileCount = round(FGlobalTilingQualityBasedTileCount * EqualQualityTileCount(Length(FFrames) * FTileMapSize)));

  ProgressRedraw(-1, esNone);

  ClearAll;

  ProgressRedraw(0, esLoad);

  // init Gamma LUTs

  InitLuts(FPaletteSize, FPaletteCount);

  // load video

  frc := FFrameCountSetting;

  if FileExists(FInputFileName) then
  begin
    DoExternalFFMpeg(FInputFileName, FInputPath, FStartFrame, frc, FScaling, FFramesPerSecond);
    startFrmIdx := 1;
  end
  else
  begin
    FInputPath := FInputFileName;
    FFramesPerSecond := 24.0;
    startFrmIdx := FStartFrame;
  end;

  // automaticaly count frames if needed

  if frc <= 0 then
  begin
    i := 0;
    repeat
      fn := Format(FInputPath, [i + startFrmIdx]);
      Inc(i);
    until not FileExists(fn);

    frc := i - 1;
  end;

  // load frames bitmap data

  bmp := TPicture.Create;
  try
    bmp.Bitmap.PixelFormat:=pf32bit;
    bmp.LoadFromFile(Format(FInputPath, [startFrmIdx]));
    ReframeUI(bmp.Width div cTileWidth, bmp.Height div cTileWidth);
  finally
    bmp.Free;
  end;

  ProgressRedraw(1);

  SetLength(FFrames, frc);
  ProcThreadPool.DoParallelLocalProc(@DoLoadFrame, 0, High(FFrames), Pointer(startFrmIdx));
  WriteLn;

  ProgressRedraw(2);

  FindKeyFrames;

  ProgressRedraw(3);

  if wasAutoQ or (FGlobalTilingTileCount <= 0) then
  begin
    qbTC := FGlobalTilingQualityBasedTileCount;
    SetGlobalTilingQualityBasedTileCount(0.0);
    SetGlobalTilingQualityBasedTileCount(qbTC);
  end;

  LoadTiles;

  ProgressRedraw(4);
end;

procedure TTilingEncoder.Reindex;
var
  i, sx, sy: Integer;
  tidx: Int64;
begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(0, esReindex);

  for tidx := 0 to High(FTiles) do
  begin
    FTiles[tidx]^.UseCount := 0;
    FTiles[tidx]^.Active := False;
  end;

  for i := 0 to High(FFrames) do
    for sy := 0 to FTileMapHeight - 1 do
      for sx := 0 to FTileMapWidth - 1 do
      begin
        tidx := FFrames[i].TileMap[sy, sx].TileIdx;
        Inc(FTiles[tidx]^.UseCount);
        FTiles[tidx]^.Active := True;
      end;

  ProgressRedraw(1);

  ReindexTiles(False);

  ProgressRedraw(2);
end;

procedure TTilingEncoder.Save;
var
  fs: TFileStream;
begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(0, esSave);

  fs := TFileStream.Create(FOutputFileName, fmCreate or fmShareDenyWrite);
  try
    SaveStream(fs, FFrameTilingBlendingSize);
  finally
    fs.Free;
  end;

  ProgressRedraw(1);
end;

function GROne(x: TFloat; Data: Pointer): TFloat;
var
  Self: TTilingEncoder;
  ms: TMemoryStream;
begin
  Self := TTilingEncoder(Data);

  with Self do
  begin

    FGlobalTilingSoftClusteringThreshold := x;

    FrameTiling;
    ReColor;
    Smooth;

    ms := TMemoryStream.Create;
    try
      SaveStream(ms);

      Result := ms.Size * (8.0 / 1024.0) * FFramesPerSecond / Length(FFrames);
    finally
      ms.Free;
    end;
  end;
end;

procedure TTilingEncoder.BitrateLoop(ABitrate: TFloat);
begin
  GoldenRatioSearch(@GROne, 0.0, 2.0, ABitrate, ABitrate * 0.01, Self);
end;

procedure TTilingEncoder.Smooth;
var
  NonAddlCount: Int64;

  procedure DoSmoothing(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    i: Integer;
  begin
    if not InRange(AIndex, 0, FTileMapHeight - 1) then
      Exit;

    for i := cSmoothingPrevFrame to High(FFrames) do
      DoTemporalSmoothing(FFrames[i], FFrames[i - cSmoothingPrevFrame], AIndex, FSmoothingFactor, FSmoothingAdditionalTilesThreshold, NonAddlCount);
  end;

var
  frm, j, i: Integer;
  tidx: Int64;
  TMI: PTileMapItem;
  ATList: TList;
  T: PTile;
begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(0, esSmooth);

  for frm := 0 to high(FFrames) do
    for j := 0 to (FTileMapHeight - 1) do
      for i := 0 to (FTileMapWidth - 1) do
      begin
        TMI := @FFrames[frm].TileMap[j, i];

        TMI^.Smoothed := False;
        TMI^.SmoothedTileIdx := TMI^.TileIdx;
        TMI^.SmoothedPalIdx := TMI^.PalIdx;
        TMI^.SmoothedHMirror := TMI^.HMirror;
        TMI^.SmoothedVMirror := TMI^.VMirror;
      end;

  FAdditionalTiles := TThreadList.Create;
  try
    NonAddlCount := Length(FTiles);
    for tidx := 0 to High(FTiles) do
      if FTiles[tidx]^.Additional then
      begin
        NonAddlCount := tidx;
        Break;
      end;

    ProcThreadPool.DoParallelLocalProc(@DoSmoothing, 0, FTileMapHeight - 1);

    ProgressRedraw(1);

    // reintegrate AdditionalTiles into global tiles

    ATList := FAdditionalTiles.LockList;
    try
      TTile.Array1DRealloc(FTiles, NonAddlCount + ATList.Count);
      for i := 0 to ATList.Count - 1 do
      begin
        T := PTile(ATList[i]);
        FTiles[i + Length(FTiles) - ATList.Count]^.CopyFrom(T^);
        TTile.Dispose(T);
      end;

      WriteLn('AdditionalTiles: ', ATList.Count);

      ATList.Clear;
    finally
      FAdditionalTiles.UnlockList;
    end;

    ProgressRedraw(2);
  finally
    FreeAndNil(FAdditionalTiles);
  end;
end;

procedure TTilingEncoder.GeneratePNGs;
var
  palPict: TPortableNetworkGraphic;
  i, oldRenderFrameIndex : Integer;
begin
  palPict := TFastPortableNetworkGraphic.Create;

  palPict.Width := FScreenWidth;
  palPict.Height := FScreenHeight;
  palPict.PixelFormat := pf24bit;


  oldRenderFrameIndex := RenderFrameIndex;
  try
    for i := 0 to High(FFrames) do
    begin
      RenderFrameIndex := i;
      Render(True);

      palPict.Canvas.Draw(0, 0, FOutputBitmap);
      palPict.SaveToFile(Format('%s_%.4d.png', [ChangeFileExt(FOutputFileName, ''), i]));
    end;
  finally
    palPict.Free;

    RenderFrameIndex := oldRenderFrameIndex;
    Render(False);
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

function TTilingEncoder.GetFrameCount: Integer;
begin
  Result := Length(FFrames);
end;

function TTilingEncoder.ComputeCorrelationBGR(const a: TIntegerDynArray; const b: TIntegerDynArray): TFloat;
var
  i: Integer;
  ya, yb: TFloatDynArray;
  fr, fg, fb: TFloat;
begin
  SetLength(ya, Length(a) * 3);
  SetLength(yb, Length(a) * 3);

  for i := 0 to High(a) do
  begin
    fr := (a[i] shr 16) and $ff; fg := (a[i] shr 8) and $ff; fb := a[i] and $ff;
    ya[i] := fr * cRedMul; ya[i + Length(a)] := fg * cGreenMul; ya[i + Length(a) * 2] := fb * cBlueMul;

    fr := (b[i] shr 16) and $ff; fg := (b[i] shr 8) and $ff; fb := b[i] and $ff;
    yb[i] := fr * cRedMul; yb[i + Length(a)] := fg * cGreenMul; yb[i + Length(a) * 2] := fb * cBlueMul;
  end;

  Result := PearsonCorrelation(ya, yb);
end;

function TTilingEncoder.ComputeDistanceRGB(const a: TIntegerDynArray; const b: TIntegerDynArray): TFloat;
var
  i: Integer;
  ya, yb: TFloatDynArray;
  fr, fg, fb: TFloat;
begin
  SetLength(ya, Length(a) * 3);
  SetLength(yb, Length(a) * 3);

  for i := 0 to High(a) do
  begin
    fr := a[i] and $ff; fg := (a[i] shr 8) and $ff; fb := (a[i] shr 16) and $ff;
    ya[i] := fr * cRedMul; ya[i + Length(a)] := fg * cGreenMul; ya[i + Length(a) * 2] := fb * cBlueMul;

    fr := b[i] and $ff; fg := (b[i] shr 8) and $ff; fb := (b[i] shr 16) and $ff;
    yb[i] := fr * cRedMul; yb[i + Length(a)] := fg * cGreenMul; yb[i + Length(a) * 2] := fb * cBlueMul;
  end;

  Result := CompareEuclidean(ya, yb) / (Length(a) * cLumaDiv * 256.0);
end;

function TTilingEncoder.ComputeInterFrameCorrelation(a, b: TFrame; out EuclideanDist: TFloat): TFloat;
var
  sz, i, tx, ty, sx, sy: Integer;
  rr, gg, bb: Integer;
  ya, yb: TFloatDynArray;
  par, pag, pab, pbr, pbg, pbb: PFloat;
  pat, pbt: PInteger;
begin
  sz := FTileMapSize * Sqr(cTileWidth);
  SetLength(ya, sz * 3);
  SetLength(yb, sz * 3);

  a.AcquireFrameTiles;
  b.AcquireFrameTiles;
  try
    par := @ya[sz * 0]; pag := @ya[sz * 1]; pab := @ya[sz * 2];
    pbr := @yb[sz * 0]; pbg := @yb[sz * 1]; pbb := @yb[sz * 2];

    for sy := 0 to FTileMapHeight - 1 do
      for ty := 0 to cTileWidth - 1 do
        for sx := 0 to FTileMapWidth - 1 do
        begin
          i := sy * FTileMapWidth + sx;
          pat := PInteger(@a.FrameTiles[i]^.GetRGBPixelsPtr^[ty, 0]);
          pbt := PInteger(@b.FrameTiles[i]^.GetRGBPixelsPtr^[ty, 0]);
          for tx := 0 to cTileWidth - 1 do
          begin
            FromRGB(pat^, rr, gg, bb);
            Inc(pat);
            RGBToLAB(rr, gg, bb, -1, par^, pag^, pab^);
            Inc(par); Inc(pag); Inc(pab);

            FromRGB(pbt^, rr, gg, bb);
            Inc(pbt);
            RGBToLAB(rr, gg, bb, -1, pbr^, pbg^, pbb^);
            Inc(pbr); Inc(pbg); Inc(pbb);
          end;
        end;

    Result := PearsonCorrelation(ya, yb);

    par := @ya[0];
    pbr := @yb[0];
    EuclideanDist := 0;
    for i := 0 to Length(ya) div cTileDCTSize - 1 do
    begin
      EuclideanDist += CompareEuclideanDCTPtr_asm(par, pbr);
      Inc(par, cTileDCTSize);
      Inc(pbr, cTileDCTSize);
    end;

    EuclideanDist := Sqrt(EuclideanDist) / sz;
  finally
    a.ReleaseFrameTiles;
    b.ReleaseFrameTiles;
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

procedure TTilingEncoder.DitherTileFloydSteinberg(ATile: TTile; out RGBPixels: TRGBPixels);
var
  x, y, c, yp, xm, xp: Integer;
  OldPixel, NewPixel, QuantError: Integer;
  Pixels: array[-1..cTileWidth, -1..cTileWidth, 0..2{RGB}] of Integer;
begin
  for y := 0 to (cTileWidth - 1) do
  begin
    for x := 0 to (cTileWidth - 1) do
      FromRGB(ATile.RGBPixels[y, x], Pixels[y, x, 0], Pixels[y, x, 1], Pixels[y, x, 2]);

    Pixels[y, -1, 0] := Pixels[y, 0, 0];
    Pixels[y, -1, 1] := Pixels[y, 0, 1];
    Pixels[y, -1, 2] := Pixels[y, 0, 2];
    Pixels[y, 8, 0] := Pixels[y, 7, 0];
    Pixels[y, 8, 1] := Pixels[y, 7, 1];
    Pixels[y, 8, 2] := Pixels[y, 7, 2];
  end;

  for x := -1 to cTileWidth do
  begin
    Pixels[-1, x, 0] := Pixels[0, x, 0];
    Pixels[-1, x, 1] := Pixels[0, x, 1];
    Pixels[-1, x, 2] := Pixels[0, x, 2];
    Pixels[8, x, 0] := Pixels[7, x, 0];
    Pixels[8, x, 1] := Pixels[7, x, 1];
    Pixels[8, x, 2] := Pixels[7, x, 2];
  end;

  for y := 0 to (cTileWidth - 1) do
    for x := 0 to (cTileWidth - 1) do
      for c := 0 to 2 do
      begin
        OldPixel := Pixels[y, x, c];
        NewPixel := Posterize(OldPixel);
        QuantError := OldPixel - NewPixel;

        yp := y + 1;
        xp := x + 1;
        xm := x - 1;

        Pixels[y,  x,  c] := NewPixel;

        Pixels[y,  xp, c] += (QuantError * 7) shr 4;
        Pixels[yp, xm, c] += (QuantError * 3) shr 4;
        Pixels[yp, x,  c] += (QuantError * 5) shr 4;
        Pixels[yp, xp, c] += (QuantError * 1) shr 4;
      end;

  for y := 0 to (cTileWidth - 1) do
    for x := 0 to (cTileWidth - 1) do
      RGBPixels[y, x] := ToRGB(min(255, Pixels[y, x, 0]), min(255, Pixels[y, x, 1]), min(255, Pixels[y, x, 2]));
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

procedure TTilingEncoder.DitherTile(var ATile: TTile; var Plan: TMixingPlan);
var
  x, y: Integer;
  count, map_value: Integer;
  TKList: array[0 .. cDitheringLen - 1] of Byte;
  YilList: array[0 .. cDitheringListLen - 1] of Byte;
begin
  if FDitheringUseThomasKnoll then
  begin
    for y := 0 to (cTileWidth - 1) do
      for x := 0 to (cTileWidth - 1) do
      begin
        map_value := cDitheringMap[(y * cTileWidth) + x];
        DeviseBestMixingPlanThomasKnoll(Plan, ATile.RGBPixels[y, x], TKList);
        ATile.PalPixels[y, x] := TKList[map_value];
      end;
  end
  else
  begin
    for y := 0 to (cTileWidth - 1) do
      for x := 0 to (cTileWidth - 1) do
      begin
        map_value := cDitheringMap[(y * cTileWidth) + x];
        count := DeviseBestMixingPlanYliluoma(Plan, ATile.RGBPixels[y, x], YilList);
        map_value := (map_value * count) shr 6;
        ATile.PalPixels[y, x] := YilList[map_value];
      end;
  end;
end;

function CompareCMULHS(const Item1,Item2:PCountIndex):Integer;
begin
  Result := CompareValue(Item1^.Luma, Item2^.Luma);
  if Result = 0 then
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

procedure TTilingEncoder.PreparePalettes(AKeyFrame: TKeyFrame; ADitheringGamma: Integer);
var
  sx, sy, frmIdx, i: Integer;
  GTile: PTile;

  YakmoDataset: TDoubleDynArray2;
  YakmoClusters: TIntegerDynArray;

  Yakmo: PYakmo;
  TileList: TInt64DynArray;
  TileUseCount: TIntegerDynArray;
begin
  GetKeyFrameTileList(AKeyFrame, TileList, TileUseCount);

  SetLength(YakmoDataset, Length(TileList), cTileDCTSize);
  SetLength(YakmoClusters, Length(YakmoDataset));
  SetLength(AKeyFrame.PaletteCentroids, FPaletteCount, cTileDCTSize);

  for i := 0 to High(TileList) do
  begin
    GTile := FTiles[TileList[i]];
    ComputeTilePsyVisFeatures(GTile^, False, True, True, False, False, ADitheringGamma, nil, @YakmoDataset[i, 0]);
  end;

  WriteLn('KF: ', AKeyFrame.StartFrame:8, ' Palettization start');

  if (Length(YakmoDataset) >= FPaletteCount) and (FPaletteCount > 1) then
  begin
    Yakmo := yakmo_create(FPaletteCount, 1, cYakmoMaxIterations, 1, 0, 0, 0);
    yakmo_load_train_data(Yakmo, Length(YakmoDataset), cTileDCTSize, PPDouble(@YakmoDataset[0]));
    SetLength(YakmoDataset, 0); // free up some memmory
    yakmo_train_on_data(Yakmo, @YakmoClusters[0]);
    yakmo_get_centroids(Yakmo, PPDouble(@AKeyFrame.PaletteCentroids[0]));
    yakmo_destroy(Yakmo);
  end
  else
  begin
    SetLength(YakmoClusters, Length(TileList));
  end;

  for i := 0 to High(TileList) do
    for frmIdx := AKeyFrame.StartFrame to AKeyFrame.EndFrame do
      for sy := 0 to FTileMapHeight - 1 do
        for sx := 0 to FTileMapWidth - 1 do
          if FFrames[frmIdx].TileMap[sy, sx].TileIdx = TileList[i] then
            FFrames[frmIdx].TileMap[sy, sx].PalIdx := YakmoClusters[i];

  WriteLn('KF: ', AKeyFrame.StartFrame:8, ' Palettization end');
end;

procedure TTilingEncoder.QuantizePalette(AKeyFrame: TKeyFrame; APalIdx: Integer; UseYakmo: Boolean; DLv3BPC: Integer;
  ADitheringGamma: Integer);
var
  CMPal: TCountIndexList;

  procedure DoDennisLeeV3;
  var
    dlCnt: Integer;
    dlInput: PByte;
    i, j, sy, sx, dx, dy, ty, tx, k, tileCnt, tileFx, tileFy, best: Integer;
    dlPal: TDLUserPal;
    GTile: PTile;
    CMItem: PCountIndex;
  begin
    dlCnt := AKeyFrame.FrameCount * FScreenWidth * FScreenHeight;
    dlInput := GetMem(dlCnt * 3);
    try
      FillChar(dlInput^, dlCnt * 3, 0);
      FillChar(dlPal[0, 0], SizeOf(dlPal), $ff);

      // find width and height of a rectangular area to arrange tiles

      tileCnt := 0;
      for i := AKeyFrame.StartFrame to AKeyFrame.EndFrame do
        for sy := 0 to FTileMapHeight - 1 do
          for sx := 0 to FTileMapWidth - 1 do
          begin
            GTile := FTiles[FFrames[i].TileMap[sy, sx].TileIdx];
            if GTile^.Active and (FFrames[i].TileMap[sy, sx].PalIdx = APalIdx) then
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
      for i := AKeyFrame.StartFrame to AKeyFrame.EndFrame do
      begin
        for sy := 0 to FTileMapHeight - 1 do
          for sx := 0 to FTileMapWidth - 1 do
          begin
            GTile := FTiles[FFrames[i].TileMap[sy, sx].TileIdx];

            if GTile^.Active and (FFrames[i].TileMap[sy, sx].PalIdx = APalIdx) then
            begin
              j := ((dy * cTileWidth) * tileFx * cTileWidth + (dx * cTileWidth)) * 3;
              k := sy * FTileMapWidth + sx;
              for ty := 0 to cTileWidth - 1 do
              begin
                for tx := 0 to cTileWidth - 1 do
                begin
                  FromRGB(FFrames[i].FrameTiles[k]^.RGBPixels[ty, tx], dlInput[j + 0], dlInput[j + 1], dlInput[j + 2]);
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

              Inc(AKeyFrame.PaletteUseCount[APalIdx].UseCount);
            end;
          end;
      end;

      // call Dennis Lee v3 method

      dl3quant(dlInput, tileFx * cTileWidth, tileFy * cTileWidth, FPaletteSize, DLv3BPC, @dlPal);
    finally
      Freemem(dlInput);
    end;

    // retrieve palette data

    CMPal.Count := FPaletteSize;
    for i := 0 to FPaletteSize - 1 do
    begin
      New(CMItem);
      CMItem^.Count := 0;
      CMItem^.R := dlPal[0][i]; CMItem^.G := dlPal[1][i]; CMItem^.B := dlPal[2][i];
      CMItem^.Luma := ToLuma(CMItem^.R, CMItem^.G, CMItem^.B);
      RGBToHSV(ToRGB(CMItem^.R, CMItem^.G, CMItem^.B), CMItem^.Hue, CMItem^.Sat, CMItem^.Val);
      CMPal[i] := CMItem;
    end;
  end;

  procedure DoYakmo;
  const
    cFeatureCount = 3;
  var
    i, di, ty, tx: Integer;
    rr, gg, bb: Byte;
    GTile: PTile;
    Dataset, Centroids: TDoubleDynArray2;
    Clusters: TIntegerDynArray;
    Yakmo: PYakmo;
    CMItem: PCountIndex;

    TileList: TInt64DynArray;
    TileUseCount: TIntegerDynArray;
  begin
    GetKeyFrameTileList(AKeyFrame, TileList, TileUseCount, APalIdx);

    SetLength(Dataset, Length(TileList) * Sqr(cTileWidth), cFeatureCount);
    SetLength(Clusters, Length(Dataset));
    SetLength(Centroids, FPaletteSize, cFeatureCount);

    // build a dataset of RGB pixels

    di := 0;
    for i := 0 to High(TileList) do
    begin
      GTile := FTiles[TileList[i]];
      for ty := 0 to cTileWidth - 1 do
        for tx := 0 to cTileWidth - 1 do
        begin
          FromRGB(GTile^.RGBPixels[ty, tx], rr, gg, bb);
          Dataset[di, 0] := rr;
          Dataset[di, 1] := gg;
          Dataset[di, 2] := bb;
          Inc(di);
        end;
      Inc(AKeyFrame.PaletteUseCount[APalIdx].UseCount, TileUseCount[i]);
    end;
    Assert(di = Length(Dataset));

    // use KMeans to quantize to FPaletteSize elements

    if Length(Dataset) >= FPaletteSize then
    begin
      Yakmo := yakmo_create(FPaletteSize, 1, cYakmoMaxIterations, 1, 0, 0, 0);
      yakmo_load_train_data(Yakmo, Length(Dataset), cFeatureCount, PPDouble(@Dataset[0]));

      SetLength(Dataset, 0); // free up some memmory

      yakmo_train_on_data(Yakmo, @Clusters[0]);
      yakmo_get_centroids(Yakmo, PPDouble(@Centroids[0]));
      yakmo_destroy(Yakmo);
    end;

    // retrieve palette data

    CMPal.Count := FPaletteSize;
    for i := 0 to FPaletteSize - 1 do
    begin
      New(CMItem);

      if Length(Clusters) >= FPaletteSize then
      begin
        CMItem^.R := 255;
        CMItem^.G := 255;
        CMItem^.B := 255;
        if not IsNan(Centroids[i, 0]) and not IsNan(Centroids[i, 1]) and  not IsNan(Centroids[i, 2]) then
        begin
          CMItem^.R := EnsureRange(Round(Centroids[i, 0]), 0, 255);
          CMItem^.G := EnsureRange(Round(Centroids[i, 1]), 0, 255);
          CMItem^.B := EnsureRange(Round(Centroids[i, 2]), 0, 255);
        end;
      end;

      CMItem^.Count := 0;
      CMItem^.Luma := ToLuma(CMItem^.R, CMItem^.G, CMItem^.B);
      RGBToHSV(ToRGB(CMItem^.R, CMItem^.G, CMItem^.B), CMItem^.Hue, CMItem^.Sat, CMItem^.Val);
      CMPal[i] := CMItem;
    end;
  end;

var
  i: Integer;
begin
  AKeyFrame.PaletteUseCount[APalIdx].UseCount := 0;

  CMPal := TCountIndexList.Create;
  try
    // do quantize

    if UseYakmo then
      DoYakmo
    else
      DoDennisLeeV3;

    // split most used colors into tile palettes

    CMPal.Sort(@CompareCMULHS);

    SetLength(AKeyFrame.PaletteRGB[APalIdx], FPaletteSize);
    for i := 0 to FPaletteSize - 1 do
      AKeyFrame.PaletteRGB[APalIdx, i] := ToRGB(CMPal[i]^.R, CMPal[i]^.G, CMPal[i]^.B);

    for i := 0 to CMPal.Count - 1 do
      Dispose(CMPal[i]);
  finally
    CMPal.Free;
  end;

  if InterLockedDecrement(AKeyFrame.PalettesLeft) <= 0 then
    WriteLn('KF: ', AKeyFrame.StartFrame:8, ' Quantization end');
end;

procedure TTilingEncoder.FinishQuantizePalettes(AKeyFrame: TKeyFrame);
var
  i, sy, sx, PalIdx: Integer;
  PalIdxLUT: TIntegerDynArray;
  TmpCentroids: TDoubleDynArray2;
begin
  SetLength(PalIdxLUT, FPaletteCount);

  // sort entire palettes by use count
  for PalIdx := 0 to FPaletteCount - 1 do
  begin
    AKeyFrame.PaletteUseCount[PalIdx].Palette := AKeyFrame.PaletteRGB[PalIdx];
    AKeyFrame.PaletteUseCount[PalIdx].PalIdx := PalIdx;
  end;
  QuickSort(AKeyFrame.PaletteUseCount[0], 0, FPaletteCount - 1, SizeOf(AKeyFrame.PaletteUseCount[0]), @ComparePaletteUseCount, AKeyFrame);
  for PalIdx := 0 to FPaletteCount - 1 do
  begin
    AKeyFrame.PaletteRGB[PalIdx] := AKeyFrame.PaletteUseCount[PalIdx].Palette;
    PalIdxLUT[AKeyFrame.PaletteUseCount[PalIdx].PalIdx] := PalIdx;
  end;

  for i := AKeyFrame.StartFrame to AKeyFrame.EndFrame do
    for sy := 0 to FTileMapHeight - 1 do
      for sx := 0 to FTileMapWidth - 1 do
        FFrames[i].TileMap[sy, sx].PalIdx := PalIdxLUT[FFrames[i].TileMap[sy, sx].PalIdx];
  TmpCentroids := Copy(AKeyFrame.PaletteCentroids);
  for PalIdx := 0 to FPaletteCount - 1 do
    AKeyFrame.PaletteCentroids[PalIdxLUT[PalIdx]] := TmpCentroids[PalIdx];
end;

procedure TTilingEncoder.DitherTiles(AKeyFrame: TKeyFrame);
var
  i, PalIdx: Integer;
  sx, sy: Integer;
  T: PTile;
begin
  // build ditherer
  for i := 0 to FPaletteCount - 1 do
    PreparePlan(AKeyFrame.MixingPlans[i], FDitheringYliluoma2MixedColors, AKeyFrame.PaletteRGB[i]);

  // dither tile in chosen palette
  for i := AKeyFrame.StartFrame to AKeyFrame.EndFrame do
    for sy := 0 to FTileMapHeight - 1 do
      for sx := 0 to FTileMapWidth - 1 do
      begin
        T := FTiles[FFrames[i].TileMap[sy, sx].TileIdx];

        if T^.Active and (T^.TmpIndex = -1) then
        begin
          PalIdx := FFrames[i].TileMap[sy, sx].PalIdx;
          Assert(InRange(PalIdx, 0, FPaletteCount - 1));
          DitherTile(T^, AKeyFrame.MixingPlans[PalIdx]);

          T^.TmpIndex := Int64(i) * FTileMapSize + sy * FTileMapWidth + sx;
        end;
      end;

  // cleanup ditherer
  for i := 0 to FPaletteCount - 1 do
    TerminatePlan(AKeyFrame.MixingPlans[i]);

  WriteLn('KF: ', AKeyFrame.StartFrame:8, ' Dithering end');
end;

function CompareTilePixels(Item1, Item2:Pointer):Integer;
var
  t1, t2: PTile;
begin
  t1 := PTile(Item1);
  t2 := PTile(Item2);
  Result := t1^.CompareRGBPixelsTo(t2^);
end;

procedure TTilingEncoder.MakeTilesUnique(FirstTileIndex, TileCount: Int64);
var
  i, pos, firstSameIdx: Int64;
  sortList: TFPList;
  sameIdx: array of Int64;

  procedure DoOneMerge;
  var
    j: Int64;
  begin
    if i - firstSameIdx >= 2 then
    begin
      for j := firstSameIdx to i - 1 do
        sameIdx[j - firstSameIdx] := PTile(sortList[j])^.TmpIndex;
      MergeTiles(sameIdx, i - firstSameIdx, sameIdx[0], nil, nil);
    end;
    firstSameIdx := i;
  end;

begin
  sortList := TFPList.Create;
  try

    // sort global tiles by palette indexes (L to R, T to B)

    SetLength(sameIdx, TileCount);

    sortList.Count := TileCount;
    pos := 0;
    for i := 0 to TileCount - 1 do
      if FTiles[i + FirstTileIndex]^.Active then
      begin
        sortList[pos] := FTiles[i + FirstTileIndex];
        PTile(sortList[pos])^.TmpIndex := i + FirstTileIndex;
        Inc(pos);
      end;
    sortList.Count := pos;

    sortList.Sort(@CompareTilePixels);

    // merge exactly similar tiles (so, consecutive after prev code)

    firstSameIdx := 0;
    for i := 1 to sortList.Count - 1 do
      if PTile(sortList[i - 1])^.CompareRGBPixelsTo(PTile(sortList[i])^) <> 0 then
        DoOneMerge;

    i := sortList.Count;
    DoOneMerge;

  finally
    sortList.Free;
  end;
end;

procedure TTilingEncoder.PackTiles;
var
  tidx, copyPos: Int64;
begin
  InitMergeTiles;

  // pack tiles

  copyPos := 0;
  for tidx := High(FTiles) downto 0 do
  begin
    if FTiles[tidx]^.Active then
    begin
      while (copyPos < Length(FTiles)) and FTiles[copyPos]^.Active do
        Inc(copyPos);

      if copyPos < tidx then
      begin
        FTiles[copyPos]^.CopyFrom(FTiles[tidx]^);
        FTiles[tidx]^.MergeIndex := copyPos;
        FTiles[tidx]^.Active := False;
      end
      else
      begin
        Break;
      end;
    end;
  end;

  FinishMergeTiles;

  // redim tile list

  TTile.Array1DRealloc(FTiles, copyPos);
end;

procedure TTilingEncoder.LoadTiles;
var
  ClustersPerFrame: Integer;

  procedure DoFrame(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    i, sx, sy, BICOClusterCount, clusterIdx, clusterLineCount, lineIdx: Integer;
    q00, q01, q10, q11: Integer;
    tilePos: Int64;
    err: Double;
    Frame: TFrame;
    BICO: PBICO;
    ANN: PANNkdtree;
    Dataset: TDoubleDynArray2;
    BICOCentroids, BICOWeights: TDoubleDynArray;
    ANNClusters: TIntegerDynArray;
    ANNDataset: array of PANNFloat;
    ToMergeIdxs: TInt64DynArray;
    HMirrors, VMirrors: TBooleanDynArray;
  begin
    if not InRange(AIndex, 0, High(FFrames)) then
      Exit;

    Frame := FFrames[AIndex];

    Frame.AcquireFrameTiles;
    try
      BICO := bico_create(cTileDCTSize, FTileMapSize, ClustersPerFrame, 1, ClustersPerFrame, $42381337);
      try
        SetLength(Dataset, FTileMapSize, cTileDCTSize);
        SetLength(HMirrors, FTileMapSize);
        SetLength(VMirrors, FTileMapSize);

        for i := 0 to FTileMapSize - 1 do
        begin
          // enforce a 'spin' on tiles mirrors (brighter top-left corner)

          q00 := GetTileZoneSum(Frame.FrameTiles[i]^, 0, 0, cTileWidth div 2, cTileWidth div 2);
          q01 := GetTileZoneSum(Frame.FrameTiles[i]^, cTileWidth div 2, 0, cTileWidth div 2, cTileWidth div 2);
          q10 := GetTileZoneSum(Frame.FrameTiles[i]^, 0, cTileWidth div 2, cTileWidth div 2, cTileWidth div 2);
          q11 := GetTileZoneSum(Frame.FrameTiles[i]^, cTileWidth div 2, cTileWidth div 2, cTileWidth div 2, cTileWidth div 2);

          HMirrors[i] := q00 + q10 < q01 + q11;
          VMirrors[i] := q00 + q01 < q10 + q11;

          ComputeTilePsyVisFeatures(Frame.FrameTiles[i]^, False, False, False, HMirrors[i], VMirrors[i], -1, nil, @Dataset[i, 0]);

          // insert line into BICO
          bico_insert_line(BICO, @Dataset[i, 0], 1);
        end;

        // get BICO results

        SetLength(BICOCentroids, ClustersPerFrame * cTileDCTSize);
        SetLength(BICOWeights, ClustersPerFrame);

        BICOClusterCount := bico_get_results(BICO, @BICOCentroids[0], @BICOWeights[0]);
      finally
        bico_destroy(BICO);
      end;

      BICOClusterCount := Max(1, BICOClusterCount); // can't have 0 clusters

      // use ANN to compute cluster indexes

      SetLength(ANNDataset, BICOClusterCount);
      SetLength(ANNClusters, FTileMapSize);

      for i := 0 to BICOClusterCount - 1 do
        ANNDataset[i] := @BICOCentroids[i * cTileDCTSize];

      ANN := ann_kdtree_create(@ANNDataset[0], BICOClusterCount, cTileDCTSize, 8, ANN_KD_STD);
      try
        for i := 0 to FTileMapSize - 1 do
          ANNClusters[i] := ann_kdtree_search(ANN, @Dataset[i, 0], 0.0, @err);
      finally
        ann_kdtree_destroy(ANN);
      end;

      // build a list of this centroid tiles

      SetLength(ToMergeIdxs, FTileMapSize);

      tilePos := AIndex * ClustersPerFrame;
      for clusterIdx := 0 to BICOClusterCount - 1 do
      begin
        clusterLineCount := 0;
        for lineIdx := 0 to FTileMapSize - 1 do
          if ANNClusters[lineIdx] = clusterIdx then
          begin
            ToMergeIdxs[clusterLineCount] := lineIdx;
            Inc(clusterLineCount);
          end;

        // choose a tile for the cluster

        if clusterLineCount >= 1 then
        begin
          // reverse the PsyV model to get the final tile

          FTiles[tilePos + clusterIdx]^.CopyFrom(Frame.FrameTiles[ToMergeIdxs[0]]^);
          FTiles[tilePos + clusterIdx]^.UseCount := Round(BICOWeights[clusterIdx]);

          ComputeInvTilePsyVisFeatures(ANNDataset[clusterIdx], False, -1, FTiles[tilePos + clusterIdx]^);

          // update tilemap

          for i := 0 to clusterLineCount - 1 do
          begin
            DivMod(ToMergeIdxs[i], FTileMapWidth, sy, sx);
            Frame.TileMap[sy, sx].TileIdx :=  tilePos + clusterIdx;
            Frame.TileMap[sy, sx].HMirror :=  HMirrors[ToMergeIdxs[i]];
            Frame.TileMap[sy, sx].VMirror :=  VMirrors[ToMergeIdxs[i]];
          end;
        end;
      end;

      // unused tiles
      for i := BICOClusterCount to ClustersPerFrame - 1 do
        FTiles[tilePos + i]^.Active := False;

    finally
      Frame.ReleaseFrameTiles;
    end;

    Write('.');
  end;

var
  tileCnt: Integer;
begin
  // free memory from a prev run
  TTile.Array1DDispose(FTiles);

  ClustersPerFrame := Max(1 , trunc(FGlobalTilingTileCount / Length(FFrames) * cPerFrameTileCountMultiplier));

  tileCnt := Length(FFrames) * ClustersPerFrame;

  // allocate tiles
  FTiles := TTile.Array1DNew(tileCnt, True, True);

  ProcThreadPool.DoParallelLocalProc(@DoFrame, 0, High(FFrames));

  WriteLn;
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

procedure TTilingEncoder.SetEncoderGammaValue(AValue: TFloat);
begin
  if FEncoderGammaValue = AValue then Exit;
  FEncoderGammaValue := Max(0.0, AValue);

  FGamma[0] := FEncoderGammaValue;
  InitLuts(FPaletteSize, FPaletteCount);
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

procedure TTilingEncoder.SetFrameTilingBlendingSize(AValue: Integer);
begin
  if FFrameTilingBlendingSize = AValue then Exit;
  FFrameTilingBlendingSize := EnsureRange(AValue, -1, 7);
end;

procedure TTilingEncoder.SetFrameTilingBlendingThreshold(AValue: TFloat);
begin
  if FFrameTilingBlendingThreshold = AValue then Exit;
  FFrameTilingBlendingThreshold := Max(0.0, AValue);
end;

procedure TTilingEncoder.SetGlobalTilingQualityBasedTileCount(AValue: TFloat);
var
  RawTileCount: Integer;
begin
  if FGlobalTilingQualityBasedTileCount = AValue then Exit;
  FGlobalTilingQualityBasedTileCount := AValue;

  RawTileCount := Length(FFrames) * FTileMapSize;
  FGlobalTilingTileCount := min(round(AValue * EqualQualityTileCount(RawTileCount)), RawTileCount);
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

procedure TTilingEncoder.SetScaling(AValue: TFloat);
begin
  if FScaling = AValue then Exit;
  FScaling := Max(0.01, AValue);
end;

procedure TTilingEncoder.SetSmoothingAdditionalTilesThreshold(AValue: TFloat);
begin
  if FSmoothingAdditionalTilesThreshold = AValue then Exit;
  FSmoothingAdditionalTilesThreshold := Max(0.0, AValue);
end;

procedure TTilingEncoder.SetSmoothingFactor(AValue: TFloat);
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

procedure TTilingEncoder.SetRenderGammaValue(AValue: TFloat);
begin
  if FRenderGammaValue = AValue then Exit;
  FRenderGammaValue := Max(0.0, AValue);

  FGamma[1] := FRenderGammaValue;
  InitLuts(FPaletteSize, FPaletteCount);
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

// from https://lists.freepascal.org/pipermail/fpc-announce/2006-September/000508.html
procedure TTilingEncoder.WaveletGS(Data : PFloat; Output : PFloat; dx, dy, depth : cardinal);
var
  x, y: longint;
  offset: cardinal;
  factor: TFloat;
  tempX: array[0 .. sqr(cTileWidth) - 1] of TFloat;
  tempY: array[0 .. sqr(cTileWidth) - 1] of TFloat;
begin
  FillChar(tempX[0], SizeOf(tempX), 0);
  FillChar(tempY[0], SizeOf(tempY), 0);

  factor:=(1.0 / sqrt(2.0)); //Normalized Haar

  for y:=0 to dy - 1 do //Transform Rows
  begin
    offset := y * cTileWidth;
    for x := 0 to (dx div 2) - 1 do
    begin
      tempX[x + offset]             := (Data[x * 2 + offset] + Data[(x * 2 + 1) + offset]) * factor; //LOW-PASS
      tempX[(x + dx div 2) +offset] := (Data[x * 2 + offset] - Data[(x * 2 + 1) + offset]) * factor; //HIGH-PASS
    end;
  end;

  for x := 0 to dx - 1 do //Transform Columns
    for y := 0 to (dy div 2) - 1 do
    begin
      tempY[x +y * cTileWidth]              := (tempX[x +y * 2 * cTileWidth] + tempX[x +(y * 2 + 1) * cTileWidth]) * factor; //LOW-PASS
      tempY[x +(y + dy div 2) * cTileWidth] := (tempX[x +y * 2 * cTileWidth] - tempX[x +(y * 2 + 1) * cTileWidth]) * factor; //HIGH-PASS
    end;

  for y := 0 to dy - 1 do
    Move(tempY[y * cTileWidth], Output[y * cTileWidth], dx * sizeof(TFloat)); //Copy to Wavelet

  if depth>0 then
    waveletgs(Output, Output, dx div 2, dy div 2, depth - 1); //Repeat for SubDivisionDepth
end;

procedure TTilingEncoder.DeWaveletGS(wl: PFloat; pic: PFloat; dx, dy, depth: longint);
 Var x,y : longint;
     tempX: array[0 .. sqr(cTileWidth) - 1] of TFloat;
     tempY: array[0 .. sqr(cTileWidth) - 1] of TFloat;
     offset,offsetm1,offsetp1 : longint;
     factor : TFloat;
     dyoff,yhalf,yhalfoff,yhalfoff2,yhalfoff3 : longint;
BEGIN
  FillChar(tempX[0], SizeOf(tempX), 0);
  FillChar(tempY[0], SizeOf(tempY), 0);

  if depth>0 then dewaveletgs(wl,wl,dx div 2,dy div 2,depth-1); //Repeat for SubDivisionDepth

  factor:=(1.0/sqrt(2.0)); //Normalized Haar

  ////

  yhalf:=(dy div 2)-1;
  dyoff:=(dy div 2)*cTileWidth;
  yhalfoff:=yhalf*cTileWidth;
  yhalfoff2:=(yhalf+(dy div 2))*cTileWidth;
  yhalfoff3:=yhalfoff*2 +cTileWidth;

  if (yhalf>0) then begin //The first and last pixel has to be done "normal"
   for x:=0 to dx-1 do begin
    tempy[x]     := (wl[x] + wl[x+dyoff])*factor; //LOW-PASS
    tempy[x+cTileWidth]:= (wl[x] - wl[x+dyoff])*factor; //HIGH-PASS

    tempy[x +yhalfoff*2]:= (wl[x +yhalfoff] + wl[x +yhalfoff2])*factor; //LOW-PASS
    tempy[x +yhalfoff3] := (wl[x +yhalfoff] - wl[x +yhalfoff2])*factor; //HIGH-PASS
   end;
  end else begin
   for x:=0 to dx-1 do begin
    tempy[x]     := (wl[x] + wl[x+dyoff])*factor; //LOW-PASS
    tempy[x+cTileWidth]:= (wl[x] - wl[x+dyoff])*factor; //HIGH-PASS
   end;
  end;

  //

  dyoff:=(dy div 2)*cTileWidth;
  yhalf:=(dy div 2)-2;

  if (yhalf>=1) then begin                  //More then 2 pixels in the row?
   //
   if (dy>=4) then begin                    //DY must be greater then 4 to make the faked algo look good.. else it must be done "normal"
   //
    for x:=0 to dx-1 do begin               //Inverse Transform Colums (fake: if (high-pass coefficient=0.0) and (surrounding high-pass coefficients=0.0) then interpolate between surrounding low-pass coefficients)
     offsetm1:=0;
     offset:=cTileWidth;
     offsetp1:=cTileWidth*2;

     for y:=1 to yhalf do begin
      if (wl[x +offset+dyoff]<>0.0) then begin //!UPDATED
       tempy[x +offset*2]       := (wl[x +offset] + wl[x +offset+dyoff])*factor; //LOW-PASS
       tempy[x +offset*2 +cTileWidth] := (wl[x +offset] - wl[x +offset+dyoff])*factor; //HIGH-PASS
      end else begin //!UPDATED
       if (wl[x +offsetm1 +dyoff]=0.0) and (wl[x +offsetp1]<>wl[x +offset]) and ((y=yhalf) or (wl[x +offsetp1]<>wl[x +offsetp1 +cTileWidth])) then tempy[x +offset*2]:=(wl[x +offset]*0.8 + wl[x +offsetm1]*0.2)*factor //LOW-PASS
        else tempy[x +offset*2]:=wl[x +offset]*factor;
       if (wl[x +offsetp1 +dyoff]=0.0) and (wl[x +offsetm1]<>wl[x +offset]) and ((y=1) or (wl[x +offsetm1]<>wl[x +offsetm1 -cTileWidth])) then tempy[x +offset*2 +cTileWidth]:=(wl[x +offset]*0.8 + wl[x +offsetp1]*0.2)*factor //HIGH-PASS
        else tempy[x +offset*2 +cTileWidth]:=wl[x +offset]*factor;
      end;

      inc(offsetm1,cTileWidth);
      inc(offset,cTileWidth);
      inc(offsetp1,cTileWidth);
     end;

    end;
   //
   end else //DY<4
   //
    for x:=0 to dx-1 do begin
     offset:=cTileWidth;
     for y:=1 to yhalf do begin
      tempy[x +offset*2]      := (wl[x +offset] + wl[x +offset +dyoff])*factor; //LOW-PASS
      tempy[x +offset*2+cTileWidth] := (wl[x +offset] - wl[x +offset +dyoff])*factor; //HIGH-PASS

      inc(offset,cTileWidth);
     end;
    end;
   //
  end;

  ////

  offset:=0;
  yhalf:=(dx div 2)-1;
  yhalfoff:=(yhalf+dx div 2);
  yhalfoff2:=yhalf*2+1;

  if (yhalf>0) then begin
   for y:=0 to dy-1 do begin //The first and last pixel has to be done "normal"
    tempx[offset]   :=(tempy[offset] + tempy[yhalf+1 +offset])*factor; //LOW-PASS
    tempx[offset+1] :=(tempy[offset] - tempy[yhalf+1 +offset])*factor; //HIGH-PASS

    tempx[yhalf*2 +offset]   :=(tempy[yhalf +offset] + tempy[yhalfoff +offset])*factor; //LOW-PASS
    tempx[yhalfoff2 +offset] :=(tempy[yhalf +offset] - tempy[yhalfoff +offset])*factor; //HIGH-PASS

    inc(offset,cTileWidth);
   end;
  end else begin
   for y:=0 to dy-1 do begin //The first and last pixel has to be done "normal"
    tempx[offset]   :=(tempy[offset] + tempy[yhalf+1 +offset])*factor; //LOW-PASS
    tempx[offset+1] :=(tempy[offset] - tempy[yhalf+1 +offset])*factor; //HIGH-PASS

    inc(offset,cTileWidth);
   end;
  end;

  //

  dyoff:=(dx div 2);
  yhalf:=(dx div 2)-2;

  if (yhalf>=1) then begin

   if (dx>=4) then begin

    offset:=0;
    for y:=0 to dy-1 do begin               //Inverse Transform Rows (fake: if (high-pass coefficient=0.0) and (surrounding high-pass coefficients=0.0) then interpolate between surrounding low-pass coefficients)
     for x:=1 to yhalf do
      if (tempy[x +dyoff +offset]<>0.0) then begin //!UPDATED
       tempx[x*2 +offset]   :=(tempy[x +offset] + tempy[x +dyoff +offset])*factor; //LOW-PASS
       tempx[x*2+1 +offset] :=(tempy[x +offset] - tempy[x +dyoff +offset])*factor; //HIGH-PASS
      end else begin //!UPDATED
       if (tempy[x-1+dyoff +offset]=0.0) and (tempy[x+1 +offset]<>tempy[x +offset]) and ((x=yhalf) or (tempy[x+1 +offset]<>tempy[x+2 +offset])) then tempx[x*2 +offset]:=(tempy[x +offset]*0.8 + tempy[x-1 +offset]*0.2)*factor //LOW-PASS
        else tempx[x*2 +offset]:=tempy[x +offset]*factor;
       if (tempy[x+1+dyoff +offset]=0.0) and (tempy[x-1 +offset]<>tempy[x +offset]) and ((x=1) or (tempy[x-1 +offset]<>tempy[x-2 +offset])) then tempx[x*2+1 +offset]:=(tempy[x +offset]*0.8 + tempy[x+1 +offset]*0.2)*factor //HIGH-PASS
        else tempx[x*2+1 +offset]:=tempy[x +offset]*factor;
      end;
     inc(offset,cTileWidth);
    end;

   end else begin //DX<4

    offset:=0;
    for y:=0 to dy-1 do begin               //Inverse Transform Rows (fake: if (high-pass coefficient=0.0) and (surrounding high-pass coefficients=0.0) then interpolate between surrounding low-pass coefficients)
     for x:=1 to yhalf do begin
      tempx[x*2 +offset]   := (tempy[x +offset] + tempy[x +dyoff +offset])*factor; //LOW-PASS
      tempx[x*2+1 +offset] := (tempy[x +offset] - tempy[x +dyoff +offset])*factor; //HIGH-PASS
     end;
     inc(offset,cTileWidth);
    end;

   end;

  end;

  ////

  for y:=0 to dy-1 do
   move(tempx[y*cTileWidth],pic[y*cTileWidth],dx*sizeof(TFloat)); //Copy to Pic
END;

generic function DCTInner<T>(pCpn, pLut: T): Double;
begin
  Result := 0;

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

procedure TTilingEncoder.ComputeTilePsyVisFeatures(const ATile: TTile; FromPal, UseLAB, QWeighting, HMirror,
  VMirror: Boolean; GammaCor: Integer; const pal: TIntegerDynArray; DCT: PFloat);
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
      yy *= cYUVLumaFactor; uu *= cYUVChromaFactor; vv *= cYUVChromaFactor;
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

  pDCT := @DCT[0];
  for cpn := 0 to cColorCpns - 1 do
  begin
    pLut := @FDCTLut[0];
    for v := 0 to cTileWidth - 1 do
      for u := 0 to cTileWidth - 1 do
      begin
  		  z := specialize DCTInner<PSingle>(@CpnPixels[cpn, 0, 0], pLut);

        if QWeighting then
           z *= cDCTQuantization[cpn, v, u];

        pDCT^ := z;
        Inc(pDCT);
        Inc(pLut, Sqr(cTileWidth));
      end;
  end;
end;

procedure TTilingEncoder.ComputeTilePsyVisFeatures(const ATile: TTile; FromPal, UseLAB, QWeighting, HMirror, VMirror: Boolean;
  GammaCor: Integer; const pal: TIntegerDynArray; DCT: PDouble);
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
      yy *= cYUVLumaFactor; uu *= cYUVChromaFactor; vv *= cYUVChromaFactor;
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

  pDCT := @DCT[0];
  for cpn := 0 to cColorCpns - 1 do
  begin
    pLut := @FDCTLutDouble[0];
    for v := 0 to cTileWidth - 1 do
      for u := 0 to cTileWidth - 1 do
      begin
  		  z := specialize DCTInner<PDouble>(@CpnPixels[cpn, 0, 0], pLut);

        if QWeighting then
           z *= cDCTQuantization[cpn, v, u];

        pDCT^ := z;
        Inc(pDCT);
        Inc(pLut, Sqr(cTileWidth));
      end;
  end;
end;

procedure TTilingEncoder.ComputeInvTilePsyVisFeatures(DCT: PFloat; UseLAB: Boolean; GammaCor: Integer; var ATile: TTile);
var
  x, y, cpn: Integer;
  CpnPixels: TCpnPixels;
  pCpn, pLut: PFloat;

  function FromCpn(x, y: Integer): Integer; inline;
  var
    yy, uu, vv: TFloat;
  begin
    yy := CpnPixels[0, y, x];
    uu := CpnPixels[1, y, x];
    vv := CpnPixels[2, y, x];

    yy *= 1.0 / cYUVLumaFactor; uu *= 1.0 / cYUVChromaFactor; vv *= 1.0 / cYUVChromaFactor;

    if UseLAB then
      Result := LABToRGB(yy, uu, vv , GammaCor)
    else
      Result := YUVToRGB(yy, uu, vv, GammaCor);
  end;

begin
  pCpn := @CpnPixels[0, 0, 0];
  for cpn := 0 to cColorCpns - 1 do
  begin
    pLut := @FInvDCTLut[0];

    for y := 0 to (cTileWidth - 1) do
      for x := 0 to (cTileWidth - 1) do
      begin
        pCpn^ := specialize DCTInner<PSingle>(@DCT[cpn * (cTileDCTSize div cColorCpns)], pLut);
        Inc(pCpn);
        Inc(pLut, Sqr(cTileWidth));
      end;
  end;

  for y := 0 to (cTileWidth - 1) do
    for x := 0 to (cTileWidth - 1) do
      ATile.RGBPixels[y, x] := FromCpn(x, y);
end;


procedure TTilingEncoder.ComputeInvTilePsyVisFeatures(DCT: PDouble; UseLAB: Boolean; GammaCor: Integer; var ATile: TTile);
var
  x, y, cpn: Integer;
  CpnPixels: TCpnPixelsDouble;
  pCpn, pLut: PDouble;

  function FromCpn(x, y: Integer): Integer; inline;
  var
    yy, uu, vv: TFloat;
  begin
    yy := CpnPixels[0, y, x];
    uu := CpnPixels[1, y, x];
    vv := CpnPixels[2, y, x];

    yy *= 1.0 / cYUVLumaFactor; uu *= 1.0 / cYUVChromaFactor; vv *= 1.0 / cYUVChromaFactor;

    if UseLAB then
      Result := LABToRGB(yy, uu, vv , GammaCor)
    else
      Result := YUVToRGB(yy, uu, vv, GammaCor);
  end;

begin
  pCpn := @CpnPixels[0, 0, 0];
  for cpn := 0 to cColorCpns - 1 do
  begin
    pLut := @FInvDCTLutDouble[0];

    for y := 0 to (cTileWidth - 1) do
      for x := 0 to (cTileWidth - 1) do
      begin
        pCpn^ := specialize DCTInner<PDouble>(@DCT[cpn * (cTileDCTSize div cColorCpns)], pLut);
        Inc(pCpn);
        Inc(pLut, Sqr(cTileWidth));
      end;
  end;

  for y := 0 to (cTileWidth - 1) do
    for x := 0 to (cTileWidth - 1) do
      ATile.RGBPixels[y, x] := FromCpn(x, y);
end;

procedure TTilingEncoder.VMirrorTile(var ATile: TTile);
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

      if ATile.HasRGBPixels then
      begin
        v := ATile.RGBPixels[j, i];
        sv := ATile.RGBPixels[cTileWidth - 1 - j, i];
        ATile.RGBPixels[j, i] := sv;
        ATile.RGBPixels[cTileWidth - 1 - j, i] := v;
      end;
    end;
end;

procedure TTilingEncoder.HMirrorTile(var ATile: TTile);
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

      if ATile.HasRGBPixels then
      begin
        v := ATile.RGBPixels[j, i];
        sv := ATile.RGBPixels[j, cTileWidth - 1 - i];
        ATile.RGBPixels[j, i] := sv;
        ATile.RGBPixels[j, cTileWidth - 1 - i] := v;
      end;
    end;
end;

procedure TTilingEncoder.LoadFrame(var AFrame: TFrame; AFrameIndex: Integer; const ABitmap: TRawImage);
var
  i, j, col, ti, tx, ty: Integer;
  pcol: PInteger;
  TMI: PTileMapItem;
  T: PTile;
begin
  AFrame := TFrame.Create;

  SetLength(AFrame.TileMap, FTileMapHeight, FTileMapWidth);

  for j := 0 to FTileMapHeight - 1 do
    for i := 0 to FTileMapWidth - 1 do
    begin
      TMI := @AFrame.TileMap[j, i];

      TMI^.TileIdx := Int64(AFrameIndex) * FTileMapSize + j * FTileMapWidth + i;
      TMI^.PalIdx := -1;
      TMI^.HMirror := False;
      TMI^.VMirror := False;

      TMI^.Smoothed := False;
      TMI^.SmoothedTileIdx := -1;
      TMI^.SmoothedPalIdx := -1;
      TMI^.SmoothedHMirror := False;
      TMI^.SmoothedVMirror := False;

      TMI^.BlendCur := cMaxFTBlend;
      TMI^.BlendPrev := 0;
      TMI^.BlendX := 0;
      TMI^.BlendY := 0;

      TMI^.FTTileDSIdx := -1;
    end;

  Assert(ABitmap.Description.Width >= FScreenWidth, 'Wrong video width!');
  Assert(ABitmap.Description.Height >= FScreenHeight, 'Wrong video height!');

  // create frame tiles from image data

  AFrame.FrameTiles := TTile.Array1DNew(FTileMapSize, True, False);

  for i := 0 to FTileMapSize - 1 do
  begin
    T := AFrame.FrameTiles[i];

    T^.Active := True;
    T^.UseCount := 1;
    T^.TmpIndex := -1;
    T^.MergeIndex := -1;
    T^.KFSoleIndex := -1;
  end;

  pcol := PInteger(ABitmap.Data);
  for j := 0 to FScreenHeight - 1 do
  begin
    for i := 0 to FScreenWidth - 1 do
      begin
        col := pcol^;
        Inc(pcol);

        ti := FTileMapWidth * (j shr cTileWidthBits) + (i shr cTileWidthBits);
        tx := i and (cTileWidth - 1);
        ty := j and (cTileWidth - 1);
        col := SwapRB(col);

        AFrame.FrameTiles[ti]^.RGBPixels[ty, tx] := col;
      end;
  end;

  // also compress frame tiles to save memory

  AFrame.CompressFrameTiles;
end;

procedure TTilingEncoder.FindKeyFrames;
var
  Correlations: TFloatDynArray;
  EuclideanDist: TFloatDynArray;

  procedure DoCorrel(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    Correlations[AIndex] := ComputeInterFrameCorrelation(FFrames[AIndex - 1], FFrames[AIndex], EuclideanDist[AIndex]);
  end;

const
  CShotTransMaxSecondsPerKF = 2.0;  // maximum seconds between keyframes
  CShotTransMinSecondsPerKF = 0.25; // minimum seconds between keyframes
  CShotTransEuclideanHiThres = 1.0; // frame equivalent accumulated distance
  CShotTransCorrelLoThres = 0.75;   // interframe pearson correlation low limit
var
  i, j, LastKFIdx: Integer;
  correl, euclidean: TFloat;
  kfIdx: Integer;
  isKf: Boolean;
  sfr, efr: Integer;
begin
  // compute interframe correlations

  SetLength(Correlations, Length(FFrames));
  SetLength(EuclideanDist, Length(FFrames));
  Correlations[0] := 0.0;
  EuclideanDist[0] := Infinity;
  ProcThreadPool.DoParallelLocalProc(@DoCorrel, 1, High(FFrames));

  // find keyframes

  SetLength(FKeyFrames, Length(FFrames));
  kfIdx := 0;
  euclidean := 0.0;
  LastKFIdx := Low(Integer);
  for i := 0 to High(FFrames) do
  begin
    correl := Correlations[i];
    euclidean += EuclideanDist[i];

    isKf := (correl < CShotTransCorrelLoThres) or (euclidean > CShotTransEuclideanHiThres) or
      ((i - LastKFIdx) > (CShotTransMaxSecondsPerKF * FFramesPerSecond));

    isKf := isKf and ((i - LastKFIdx) > (CShotTransMinSecondsPerKF * FFramesPerSecond));

    if isKf then
    begin
      FKeyFrames[kfIdx] := TKeyFrame.Create(kfIdx, FPaletteCount, 0, 0);

      WriteLn('KF: ', i:8, ' (', kfIdx:3, ') Correlation: ', correl:12:9, ' Euclidean: ', euclidean:12:9);

      Inc(kfIdx);

      euclidean := 0.0;
      LastKFIdx := i;
    end;

    FFrames[i].PKeyFrame := FKeyFrames[kfIdx - 1];
  end;

  SetLength(FKeyFrames, kfIdx);

  for j := 0 to High(FKeyFrames) do
  begin
    sfr := High(Integer);
    efr := Low(Integer);

    for i := 0 to High(FFrames) do
      if FFrames[i].PKeyFrame = FKeyFrames[j] then
      begin
        sfr := Min(sfr, i);
        efr := Max(efr, i);
      end;

    FKeyFrames[j].StartFrame := sfr;
    FKeyFrames[j].EndFrame := efr;
    FKeyFrames[j].FrameCount := efr - sfr + 1;
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
end;

procedure TTilingEncoder.Render(AFast: Boolean);

  procedure DrawTile(bitmap: TBitmap; sx, sy: Integer; psyTile: PTile; tilePtr: PTile; pal: TIntegerDynArray; hmir, vmir: Boolean; prevtilePtr: PTile; prevPal: TIntegerDynArray; prevHmir, prevVmir: Boolean; blendCur, blendPrev: Integer);
  var
    r, g, b, pr, pg, pb, tx, ty, txm, tym, ptxm, ptym, col: Integer;
    psl: PInteger;
  begin
    for ty := 0 to cTileWidth - 1 do
    begin
      psl := bitmap.ScanLine[ty + sy * cTileWidth];
      Inc(psl, sx * cTileWidth);

      tym := ty;
      if vmir and FRenderMirrored then tym := cTileWidth - 1 - tym;
      ptym := ty;
      if prevVmir and FRenderMirrored then ptym := cTileWidth - 1 - ptym;

      for tx := 0 to cTileWidth - 1 do
      begin
        txm := tx;
        if hmir and FRenderMirrored then txm := cTileWidth - 1 - txm;
        ptxm := tx;
        if prevHmir and FRenderMirrored then ptxm := cTileWidth - 1 - ptxm;

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

        pr := 0; pg := 255; pb := 255;
        if Assigned(prevtilePtr) and prevtilePtr^.Active then
        begin
          if Assigned(prevPal) then
          begin
            if prevtilePtr^.HasPalPixels then
              FromRGB(prevPal[prevtilePtr^.PalPixels[ptym, ptxm]], pr, pg, pb)
          end
          else
          begin
            if prevtilePtr^.HasRGBPixels then
              FromRGB(prevtilePtr^.RGBPixels[ptym, ptxm], pr, pg, pb);
          end;
        end;

        r := EnsureRange((r * blendCur + pr * blendPrev) div cMaxFTBlend, 0, 255);
        g := EnsureRange((g * blendCur + pg * blendPrev) div cMaxFTBlend, 0, 255);
        b := EnsureRange((b * blendCur + pb * blendPrev) div cMaxFTBlend, 0, 255);

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
  i, j, sx, sy, frmIdx, prevFrmIdx, globalTileCount: Integer;
  tidx: Int64;
  p: PInteger;
  prevTilePtr, tilePtr: PTile;
  PsyTile: PTile;
  prevTMItem, TMItem: TTileMapItem;
  Frame: TFrame;
  prevPal, pal: TIntegerDynArray;
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

    if not (FRenderPlaying or AFast) then
      globalTileCount := GetGlobalTileCount
    else
      globalTileCount := Length(FTiles);

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
      Frame.AcquireFrameTiles;
      try
        for sy := 0 to FTileMapHeight - 1 do
          for sx := 0 to FTileMapWidth - 1 do
          begin
            tilePtr :=  Frame.FrameTiles[sy * FTileMapWidth + sx];
            if (FRenderPaletteIndex < 0) or (frame.TileMap[sy, sx].PalIdx = FRenderPaletteIndex) then
              DrawTile(FInputBitmap, sx, sy, nil, tilePtr, nil, False, False, nil, nil, False, False, cMaxFTBlend, 0);
          end;
      finally
        Frame.ReleaseFrameTiles;
        FInputBitmap.EndUpdate;
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

            prevTMItem.TileIdx := -1;
            if (frmIdx > 0) and (TMItem.BlendCur < cMaxFTBlend) and (TMItem.BlendPrev > 0) then
            begin
              prevFrmIdx := frmIdx - 1;
              prevTMItem := FFrames[prevFrmIdx].TileMap[sy + TMItem.BlendY, sx + TMItem.BlendX];
              while TMItem.Smoothed do
              begin
                Dec(prevFrmIdx);
                TMItem := FFrames[prevFrmIdx].TileMap[sy + TMItem.BlendY, sx + TMItem.BlendX];
              end;
            end;

            if InRange(TMItem.TileIdx, 0, High(FTiles)) then
            begin
              pal := nil;
              if FRenderDithered then
                if FRenderPaletteIndex < 0 then
                begin
                  if not InRange(TMItem.PalIdx, 0, High(Frame.PKeyFrame.PaletteRGB)) then
                    Continue;
                  pal := Frame.PKeyFrame.PaletteRGB[TMItem.PalIdx]
                end
                else
                begin
                  if FRenderPaletteIndex <> TMItem.PalIdx then
                    Continue;
                  pal := Frame.PKeyFrame.PaletteRGB[FRenderPaletteIndex];
                end;

              prevTilePtr := nil;
              tilePtr := FTiles[TMItem.TileIdx];
              if prevTMItem.TileIdx >= 0 then
                prevTilePtr := FTiles[prevTMItem.TileIdx];

              if FRenderBlended then
              begin
                if prevTMItem.TileIdx >= 0 then
                begin
                  prevPal := nil;
                  if FRenderDithered then
                    prevPal := FFrames[prevFrmIdx].PKeyFrame.PaletteRGB[prevTMItem.PalIdx];

                  DrawTile(FOutputBitmap, sx, sy, PsyTile, tilePtr, pal, TMItem.HMirror, TMItem.VMirror, prevTilePtr, prevPal, prevTMItem.HMirror, prevTMItem.VMirror, TMItem.BlendCur, TMItem.BlendPrev);
                end
                else
                begin
                  DrawTile(FOutputBitmap, sx, sy, PsyTile, tilePtr, pal, TMItem.HMirror, TMItem.VMirror, nil, nil, False, False, TMItem.BlendCur, 0);
                end;
              end
              else
              begin
                DrawTile(FOutputBitmap, sx, sy, PsyTile, tilePtr, pal, TMItem.HMirror, TMItem.VMirror, nil, nil, False, False, cMaxFTBlend, 0);
              end;

              if not (FRenderPlaying or AFast) then
                ComputeTilePsyVisFeatures(PsyTile^, False, False, False, False, False, -1, nil, PFloat(@chgDCT[sy, sx, 0]));
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
            if Assigned(Frame.PKeyFrame.PaletteRGB[j]) then
              p^ := SwapRB(Frame.PKeyFrame.PaletteRGB[j, i])
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

            if InRange(tidx, 0, High(FTiles)) then
            begin
              tilePtr := FTiles[tidx];
              pal := nil;
              if FRenderDithered then
                pal := Frame.PKeyFrame.PaletteRGB[Max(0, FRenderPaletteIndex)];

              DrawTile(FTilesBitmap, sx, sy, nil, tilePtr, pal, False, False, nil, nil, False, False, cMaxFTBlend, 0);
            end;
          end;
      finally
        FTilesBitmap.EndUpdate;
      end;
    end;

    if not (FRenderPlaying or AFast) then
    begin
      Frame.AcquireFrameTiles;
      try
        q := 0.0;
        i := 0;
        for sy := 0 to FTileMapHeight - 1 do
          for sx := 0 to FTileMapWidth - 1 do
          begin
            tilePtr :=  Frame.FrameTiles[sy * FTileMapWidth + sx];
            if (FRenderPaletteIndex < 0) or (frame.TileMap[sy, sx].PalIdx = FRenderPaletteIndex) then
            begin
              ComputeTilePsyVisFeatures(tilePtr^, False, False, False, False, False, Ord(FRenderUseGamma) * 2 - 1, nil, PFloat(@DCT[0]));
              q += CompareEuclideanDCT(DCT, chgDCT[sy, sx]);
              Inc(i);
            end;
          end;
        if i <> 0 then
          q /= i;

        FRenderPsychoVisualQuality := Sqrt(q);
      finally
        Frame.ReleaseFrameTiles;
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
    ini.WriteFloat('Load', 'Scaling', RoundTo(Double(Scaling), -7));

    ini.WriteInteger('Dither', 'PaletteSize', PaletteSize);
    ini.WriteInteger('Dither', 'PaletteCount', PaletteCount);
    ini.WriteBool('Dither', 'QuantizerUseYakmo', QuantizerUseYakmo);
    ini.WriteInteger('Dither', 'QuantizerDennisLeeBitsPerComponent', QuantizerDennisLeeBitsPerComponent);
    ini.WriteBool('Dither', 'DitheringUseGamma', DitheringUseGamma);
    ini.WriteBool('Dither', 'DitheringUseThomasKnoll', DitheringUseThomasKnoll);
    ini.WriteInteger('Dither', 'DitheringYliluoma2MixedColors', DitheringYliluoma2MixedColors);

    ini.WriteFloat('GlobalTiling', 'GlobalTilingQualityBasedTileCount', RoundTo(Double(GlobalTilingQualityBasedTileCount), -7));
    ini.WriteInteger('GlobalTiling', 'GlobalTilingTileCount', GlobalTilingTileCount);
    ini.WriteFloat('GlobalTiling', 'GlobalTilingSoftClusteringThreshold', GlobalTilingSoftClusteringThreshold);
    ini.WriteBool('GlobalTiling', 'ReloadTileset', ReloadTileset);
    ini.WriteString('GlobalTiling', 'ReloadTilesetFileName', ReloadTilesetFileName);

    ini.WriteBool('FrameTiling', 'FrameTilingUseGamma', FrameTilingUseGamma);
    ini.WriteInteger('FrameTiling', 'FrameTilingBlendingSize', FrameTilingBlendingSize);
    ini.WriteFloat('FrameTiling', 'FrameTilingBlendingThreshold', RoundTo(Double(FrameTilingBlendingThreshold), -7));

    ini.WriteFloat('Smoothing', 'SmoothingFactor', RoundTo(Double(SmoothingFactor), -7));
    ini.WriteFloat('Smoothing', 'SmoothingAdditionalTilesThreshold', RoundTo(Double(SmoothingAdditionalTilesThreshold), -7));

    ini.WriteFloat('Misc', 'EncoderGammaValue', RoundTo(Double(EncoderGammaValue), -7));

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

    InputFileName := ini.ReadString('Load', 'InputFileName', '');
    OutputFileName := ini.ReadString('Load', 'OutputFileName', '');
    StartFrame := ini.ReadInteger('Load', 'StartFrame', 0);
    FrameCountSetting := ini.ReadInteger('Load', 'FrameCount', 0);
    Scaling := ini.ReadFloat('Load', 'Scaling', 1.0);

    PaletteSize := ini.ReadInteger('Dither', 'PaletteSize', 128);
    PaletteCount := ini.ReadInteger('Dither', 'PaletteCount', 16);
    QuantizerUseYakmo := ini.ReadBool('Dither', 'QuantizerUseYakmo', True);
    QuantizerDennisLeeBitsPerComponent := ini.ReadInteger('Dither', 'QuantizerDennisLeeBitsPerComponent', 7);
    DitheringUseGamma := ini.ReadBool('Dither', 'DitheringUseGamma', False);
    DitheringUseThomasKnoll := ini.ReadBool('Dither', 'DitheringUseThomasKnoll', True);
    DitheringYliluoma2MixedColors := ini.ReadInteger('Dither', 'DitheringYliluoma2MixedColors', 4);

    GlobalTilingQualityBasedTileCount := ini.ReadFloat('GlobalTiling', 'GlobalTilingQualityBasedTileCount', 2.0);
    GlobalTilingTileCount := ini.ReadInteger('GlobalTiling', 'GlobalTilingTileCount', 0); // after GlobalTilingQualityBasedTileCount because has priority
    GlobalTilingSoftClusteringThreshold := ini.ReadFloat('GlobalTiling', 'GlobalTilingSoftClusteringThreshold', 0.0);
    ReloadTileset := ini.ReadBool('GlobalTiling', 'ReloadTileset', False);
    ReloadTilesetFileName := ini.ReadString('GlobalTiling', 'ReloadTilesetFileName', '');

    FrameTilingUseGamma := ini.ReadBool('FrameTiling', 'FrameTilingUseGamma', False);
    FrameTilingBlendingSize := ini.ReadInteger('FrameTiling', 'FrameTilingBlendingSize', 3);
    FrameTilingBlendingThreshold := ini.ReadFloat('FrameTiling', 'FrameTilingBlendingThreshold', 1.0);

    SmoothingFactor := ini.ReadFloat('Smoothing', 'SmoothingFactor', 0.02);
    SmoothingAdditionalTilesThreshold := ini.ReadFloat('Smoothing', 'SmoothingAdditionalTilesThreshold', 0.0);

    EncoderGammaValue := ini.ReadFloat('Misc', 'EncoderGammaValue', 2.0);

  finally
    ini.Free;
  end;
end;

procedure TTilingEncoder.Test;
var
  i, j, rng: Integer;
  rr, gg, bb: Byte;
  l, a, b, y, u, v: TFloat;
  DCT: array [0..cTileDCTSize-1] of Double;
  T, T2: PTile;
begin
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

  for i := 0 to 7 do
    for j := 0 to 7 do
      T^.RGBPixels[i, j] := ToRGB(i*8, j * 32, i * j);

  ComputeTilePsyVisFeatures(T^, False, False, False, False, False, -1, nil, @DCT[0]);
  ComputeInvTilePsyVisFeatures(@DCT[0], False, -1, T2^);

  //for i := 0 to 7 do
  //  for j := 0 to 7 do
  //    write(IntToHex(T^.RGBPixels[i, j], 6), '  ');
  //WriteLn();
  //for i := 0 to 7 do
  //  for j := 0 to 7 do
  //    write(IntToHex(T2^.RGBPixels[i, j], 6), '  ');
  //WriteLn();


  Assert(CompareMem(T^.GetRGBPixelsPtr, T2^.GetRGBPixelsPtr, SizeOf(TRGBPixels)), 'DCT/InvDCT mismatch');

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

procedure TTilingEncoder.ProgressRedraw(ASubStepIdx: Integer; AProgressStep: TEncoderStep);
const
  cProgressMul = 100;
var
  curTime: Int64;
  ProgressPosition, ProgressStepPosition, ProgressMax: Integer;
  ProgressHourGlass: Boolean;
begin
  scalable_allocation_command(TBBMALLOC_CLEAN_ALL_BUFFERS, nil); // force the mem allocator to release unused memory

  curTime := GetTickCount64;

  if (ASubStepIdx < 0) and (AProgressStep = esNone) then // reset
  begin
    FProgressStep := esNone;
    FProgressPrevTime := GetTickCount64;
    FProgressStartTime := FProgressPrevTime;
  end
  else if AProgressStep <> esNone then // new step?
  begin
    if ASubStepIdx = 0 then
      FProgressStartTime += curTime - FProgressPrevTime;

    FProgressStep := AProgressStep;
    FProgressPrevTime := GetTickCount64;
  end;

  ProgressMax := (Ord(High(TEncoderStep)) + 1) * cProgressMul;
  ProgressPosition := Ord(FProgressStep) * cProgressMul;

  ProgressStepPosition := 0;
  if ASubStepIdx >= 0 then
    ProgressStepPosition := iDiv0(ASubStepIdx * cProgressMul, cEncoderStepLen[FProgressStep]);

  ProgressHourGlass := (AProgressStep <> esNone) and (ASubStepIdx < cEncoderStepLen[FProgressStep]);

  if ASubStepIdx >= 0 then
  begin
    WriteLn('Step: ', Copy(GetEnumName(TypeInfo(TEncoderStep), Ord(FProgressStep)), 3), ' / ', ProgressStepPosition,
      #9'Time: ', FormatFloat('0.000', (curTime - FProgressPrevTime) / 1000), #9'All: ', FormatFloat('0.000', (curTime - FProgressStartTime) / 1000));
  end;
  FProgressPrevTime := curTime;

  if Assigned(FOnProgress) then
    FOnProgress(Self, ProgressPosition + ProgressStepPosition, ProgressMax, ProgressHourGlass);
end;

function TTilingEncoder.GetGlobalTileCount: Integer;
var tidx: Int64;
begin
  Result := 0;
  for tidx := 0 to High(FTiles) do
    if FTiles[tidx]^.Active then
      Inc(Result);
end;

function TTilingEncoder.GetFrameTileCount(AFrame: TFrame): Integer;
var
  Used: TByteDynArray;
  sx, sy: Integer;
begin
  Result := 0;

  if Length(FTiles) = 0 then
    Exit;

  SetLength(Used, Length(FTiles));
  FillByte(Used[0], Length(FTiles), 0);

  for sy := 0 to FTileMapHeight - 1 do
    for sx := 0 to FTileMapWidth - 1 do
      Used[AFrame.TileMap[sy, sx].TileIdx] := 1;

  for sx := 0 to High(Used) do
    Inc(Result, Used[sx]);
end;

function TTilingEncoder.GetTileIndexTMItem(const ATile: TTile; out AFrame: TFrame): PTileMapItem;
var
  frmIdx, int, sy, sx: Integer;
begin
  Assert(ATile.TmpIndex >= 0);

  DivMod(ATile.TmpIndex , FTileMapSize, frmIdx, int);
  DivMod(int, FTileMapWidth, sy, sx);

  Result := @FFrames[frmIdx].TileMap[sy, sx];
  AFrame := FFrames[frmIdx];
end;

procedure TTilingEncoder.GetKeyFrameTileList(AKeyFrame: TKeyFrame; var ATileList: TInt64DynArray;
 var ATilesUseCount: TIntegerDynArray; APaletteIndex: Integer);
var
  i, frmIdx, sy, sx, pos, TileCount: Integer;
  Ind: TIntegerDynArray;
begin
  SetLength(Ind, Length(FTiles));

  TileCount := 0;
  for frmIdx := AKeyFrame.StartFrame to AKeyFrame.EndFrame do
    for sy := 0 to FTileMapHeight - 1 do
      for sx := 0 to FTileMapWidth - 1 do
        if (APaletteIndex < 0) or (FFrames[frmIdx].TileMap[sy, sx].PalIdx = APaletteIndex) then
        begin
          if Ind[FFrames[frmIdx].TileMap[sy, sx].TileIdx] <= 0 then
            Inc(TileCount);
          Inc(Ind[FFrames[frmIdx].TileMap[sy, sx].TileIdx]);
        end;

  SetLength(ATileList, TileCount);
  SetLength(ATilesUseCount, TileCount);

  pos := 0;
  for i := 0 to High(Ind) do
  begin
    if Ind[i] > 0 then
    begin
      ATileList[pos] := i;
      ATilesUseCount[pos] := Ind[i];
      Inc(pos);
    end;
  end;
end;

procedure TTilingEncoder.MergeTiles(const TileIndexes: array of Int64; TileCount: Integer; BestIdx: Int64;
  NewTile: PPalPixels; NewTileRGB: PRGBPixels);
var
  i: Integer;
  tidx: Int64;
begin
  if TileCount <= 0 then
    Exit;

  if Assigned(NewTile) then
    FTiles[BestIdx]^.CopyPalPixels(NewTile^);

  if Assigned(NewTileRGB) then
    FTiles[BestIdx]^.CopyRGBPixels(NewTileRGB^);

  for i := 0 to TileCount - 1 do
  begin
    tidx := TileIndexes[i];

    if tidx = BestIdx then
      Continue;

    Inc(FTiles[BestIdx]^.UseCount, FTiles[tidx]^.UseCount);

    FTiles[tidx]^.Active := False;
    FTiles[tidx]^.UseCount := 0;
    FTiles[tidx]^.MergeIndex := BestIdx;

    FTiles[tidx]^.ClearPixels;
  end;
end;

procedure TTilingEncoder.InitMergeTiles;
var
  tidx: Int64;
begin
  for tidx := 0 to High(FTiles) do
    FTiles[tidx]^.MergeIndex := -1;
end;

procedure TTilingEncoder.FinishMergeTiles;
var
  sx, sy, frmIdx: Integer;
  tidx: Int64;
begin
  for frmIdx := 0 to High(FFrames) do
    for sy := 0 to (FTileMapHeight - 1) do
      for sx := 0 to (FTileMapWidth - 1) do
      begin
        tidx := FTiles[FFrames[frmIdx].TileMap[sy, sx].TileIdx]^.MergeIndex;
        if tidx >= 0 then
          FFrames[frmIdx].TileMap[sy, sx].TileIdx := tidx;
      end;
end;

procedure TTilingEncoder.PrepareKFTiling(AKF: TKeyFrame; APaletteIndex, AFTGamma: Integer);
var
  DS: PTilingDataset;

  function DoPsyV: Integer;
  var
    di, hvmir: Integer;
    T: PTile;
    tidx: Int64;
  begin
    di := 0;
    for tidx := 0 to High(FTiles) do
    begin
      T := FTiles[tidx];

      for hvmir := 0 to 3 do
      begin
        DS^.TDToTileIdx[di] := tidx;
        DS^.TDToAttrs[di] := hvmir;

        ComputeTilePsyVisFeatures(T^, True, False, cFTQWeighting, hvmir and 1 <> 0, hvmir and 2 <> 0, AFTGamma, AKF.PaletteRGB[APaletteIndex], PFloat(@DS^.Dataset[di * cTileDCTSize]));
        DS^.Dataset[di * cTileDCTSize + cTileDCTSize - 1] := T^.Weight;

        // prune redundancies
        if (di <= 0) or (CompareDWord(DS^.Dataset[(di - 1) * cTileDCTSize], DS^.Dataset[di * cTileDCTSize], cTileDCTSize) <> 0) then
          Inc(di);
      end;
    end;

    Result := di;
  end;

var
  KNNSize: Integer;
  speedup: Single;
begin
  // Compute psycho visual model for all tiles (in all palettes / mirrors)

  DS := New(PTilingDataset);
  FillChar(DS^, SizeOf(TTilingDataset), 0);

  KNNSize := Length(FTiles) * 4;
  SetLength(DS^.TDToTileIdx, KNNSize);
  SetLength(DS^.TDToAttrs, KNNSize);
  SetLength(DS^.Dataset, KNNSize * cTileDCTSize);

  KNNSize := DoPsyV;
  SetLength(DS^.TDToTileIdx, KNNSize);
  SetLength(DS^.TDToAttrs, KNNSize);
  SetLength(DS^.Dataset, KNNSize * cTileDCTSize);

  // Build KNN

  New(DS^.FLANNParams);
  DS^.FLANNParams^ := CDefaultFLANNParameters;
  DS^.FLANNParams^.checks := cTileDCTSize;

  DS^.FLANN := flann_build_index(@DS^.Dataset[0], KNNSize, cTileDCTSize, @speedup, DS^.FLANNParams);

  // Dataset is ready

  AKF.TileDS[APaletteIndex] := DS;
end;

procedure TTilingEncoder.FinishKFTiling(AKF: TKeyFrame; APaletteIndex: Integer);
var
  tileResd: TFloat;
begin
  if Length(AKF.TileDS[APaletteIndex]^.Dataset) > 0 then
    flann_free_index(AKF.TileDS[APaletteIndex]^.FLANN, AKF.TileDS[APaletteIndex]^.FLANNParams);
  AKF.TileDS[APaletteIndex]^.FLANN := nil;
  SetLength(AKF.TileDS[APaletteIndex]^.Dataset, 0);
  SetLength(AKF.TileDS[APaletteIndex]^.TDToTileIdx, 0);
  SetLength(AKF.TileDS[APaletteIndex]^.TDToAttrs, 0);
  Dispose(AKF.TileDS[APaletteIndex]);

  AKF.TileDS[APaletteIndex] := nil;
  SetEvent(AKF.FTPaletteDoneEvent[APaletteIndex]);
  InterLockedDecrement(AKF.PalettesLeft);

  Write('.');

  if AKF.PalettesLeft <= 0 then
  begin
    tileResd := Sqrt(AKF.FTErrCml / (FTileMapSize * AKF.FrameCount));
    WriteLn;
    WriteLn('KF: ', AKF.StartFrame:8, #9'ResidualErr: ', (tileResd * FTileMapSize * AKF.FrameCount):12:3, ' (by frame) ', tileResd:12:6, ' (by tile)');
  end;
end;

procedure TTilingEncoder.DoTileBlending(AFrame: TFrame; APaletteIndex, APrevKFPaletteIndex, AFTGamma, AFTBlend, x, y: Integer; APlainDCT: PFloat; ACurIdxs: PInteger; ACurErrs: PFloat; var AKFTilingBest: TKFTilingBest);
var
  i, ox, oy, bucketIdx: Integer;
  prevTMI: PTileMapItem;
  DS: PTilingDataset;
  PrevDCT: array[0 .. cTileDCTSize - 1] of TFloat;
  CurPrevDCT: array[0 .. cTileDCTSize * 2 - 1] of TFloat;
  LocalPrevPaletteDone: TBooleanDynArray;

  procedure TestBestErr(err: TANNFloat; bc, bp: Integer);
  begin
    if CompareValue(err, AKFTilingBest.bestErr, FFrameTilingBlendingThreshold * 0.01) = LessThanValue then
    begin
      AKFTilingBest.bestErr := err;
      AKFTilingBest.bestIdx := ACurIdxs[bucketIdx];
      AKFTilingBest.bestBlendCur := bc;
      AKFTilingBest.bestBlendPrev := bp;
      AKFTilingBest.bestX := ox;
      AKFTilingBest.bestY := oy;
    end;
  end;

  procedure SearchBlending2P(Plain: PFloat);
  var
    term, bc, bp: Integer;
    fcp: array[0 .. 1] of ArbFloat;
    fc, fp, err: TFloat;
  begin
    slegls(CurPrevDCT[0], cTileDCTSize, 2, 2, Plain[0], fcp[0], term);
    if term = 1 then
    begin
      bc := EnsureRange(round(fcp[0] * cMaxFTBlend), 1, cMaxFTBlend - 1);
      fc := bc * (1.0 / cMaxFTBlend);

      // try to compensate for rounding to 16 levels by sending rounding error to other parameter

      fp := fcp[1] + fcp[0] - fc;
      bp := EnsureRange(round(fp * cMaxFTBlend), 1, cMaxFTBlend - 1);
      fp := bp * (1.0 / cMaxFTBlend);

      err := ComputeBlendingError(@Plain[0], @DS^.Dataset[ACurIdxs[bucketIdx] * cTileDCTSize], @PrevDCT[0], fc, fp);
      TestBestErr(err, bc, bp);
    end;
  end;

  procedure SearchBlending1P(Plain, Cur: PFloat);
  var
    term, bc: Integer;
    fc: ArbFloat;
    err: TFloat;
  begin
    slegls(Cur[0], cTileDCTSize, 1, 1, Plain[0], fc, term);
    if term = 1 then
    begin
      bc := EnsureRange(round(fc * cMaxFTBlend), 1, cMaxFTBlend - 1);
      fc := bc * (1.0 / cMaxFTBlend);
      err := ComputeBlendingError(@Plain[0], @Cur[0], @Cur[0], fc, 0.0);
      TestBestErr(err, bc, 0);
    end;
  end;

begin
  DS := AFrame.PKeyFrame.TileDS[APaletteIndex];

  SetLength(LocalPrevPaletteDone, FPaletteCount);

  // try to blend a local tile of the previous frame to improve likeliness

  for bucketIdx := 0 to cFTBinSize - 1 do
  begin
    for i := 0 to cTileDCTSize - 1 do
      CurPrevDCT[i * 2] := DS^.Dataset[ACurIdxs[bucketIdx] * cTileDCTSize + i];

    for oy := y - AFTBlend to y + AFTBlend do
    begin
      if not InRange(oy, 0, FTileMapHeight - 1) then
        Continue;

      for ox := x - AFTBlend to x + AFTBlend do
      begin
        if not InRange(ox, 0, FTileMapWidth - 1) then
          Continue;

        if AFrame.Index > 0 then
        begin
          prevTMI := @FFrames[AFrame.Index - 1].TileMap[oy, ox];

          if FFrames[AFrame.Index - 1].PKeyFrame <> AFrame.PKeyFrame then
          begin
            if prevTMI^.PalIdx <> APrevKFPaletteIndex then
              Continue;

            if not LocalPrevPaletteDone[prevTMI^.PalIdx] then
            begin
              WaitForSingleObject(FFrames[AFrame.Index - 1].PKeyFrame.FTPaletteDoneEvent[prevTMI^.PalIdx], INFINITE); // wait prev keyframe for palette done
              LocalPrevPaletteDone[prevTMI^.PalIdx] := True;
            end;

            ComputeTilePsyVisFeatures(FTiles[prevTMI^.TileIdx]^, True, False, cFTQWeighting, prevTMI^.HMirror, prevTMI^.VMirror, AFTGamma, FFrames[AFrame.Index - 1].PKeyFrame.PaletteRGB[prevTMI^.PalIdx], PFloat(@PrevDCT[0]));
            PrevDCT[cTileDCTSize - 1] := FTiles[prevTMI^.TileIdx]^.Weight;
          end
          else
          begin
            if prevTMI^.PalIdx <> APaletteIndex then
              Continue;

            for i := 0 to cTileDCTSize - 1 do
              PrevDCT[i] := DS^.Dataset[prevTMI^.FTTileDSIdx * cTileDCTSize + i];
          end;

           for i := 0 to cTileDCTSize - 1 do
            CurPrevDCT[i * 2 + 1] := PrevDCT[i];

          SearchBlending2P(APlainDCT);
        end
        else
        begin
          if (ox <> x) or (oy <> y) then
            Continue;

          FillDWord(PrevDCT[0], cTileDCTSize, 0);

          SearchBlending1P(APlainDCT, @DS^.Dataset[ACurIdxs[bucketIdx] * cTileDCTSize]);
        end;
      end;
    end;
  end;
end;

procedure TTilingEncoder.DoKFTiling(AKF: TKeyFrame; APaletteIndex: Integer; AFTGamma: Integer; AFTBlend: Integer;
  AFTBlendThres: TFloat);
var
  x, y, frmIdx, annQueryCount, annQueryPos, palIdx, prevKFPalIdx: Integer;
  attrs: Byte;
  errCml, bestPal, v: Double;

  Frame: TFrame;
  tmiO: PTileMapItem;

  DS: PTilingDataset;
  idxs: TIntegerDynArray;
  errs: TFloatDynArray;
  DCTs: TFloatDynArray;

  Best: TKFTilingBest;
begin
  DS := AKF.TileDS[APaletteIndex];
  if Length(DS^.Dataset) < cTileDCTSize then
    Exit;

  errCml := 0.0;

  // match current KF palette to the closest prev KF palette

  prevKFPalIdx := -1;
  if AKF.StartFrame > 0 then
  begin
    bestPal := MaxSingle;
    for palIdx := 0 to FPaletteCount - 1 do
    begin
      v := CompareEuclidean(@AKF.PaletteCentroids[APaletteIndex, 0], @FFrames[AKF.StartFrame - 1].PKeyFrame.PaletteCentroids[palIdx, 0], cTileDCTSize);
      if not IsNan(v) and (v < bestPal) then
      begin
        bestPal := v;
        prevKFPalIdx := palIdx;
      end;
    end;
  end;

  // compute KNN query count

  annQueryCount := 0;
  for frmIdx := AKF.StartFrame to AKF.EndFrame do
  begin
    Frame := FFrames[frmIdx];

    for y := 0 to FTileMapHeight - 1 do
      for x := 0 to FTileMapWidth - 1 do
      begin
        if Frame.TileMap[y, x].PalIdx <> APaletteIndex then
          Continue;

        Inc(annQueryCount);
      end;
  end;

  if annQueryCount <= 0 then
    Exit;

  SetLength(DCTs, annQueryCount * cTileDCTSize);
  SetLength(idxs, annQueryCount * cFTBinSize);
  SetLength(errs, annQueryCount * cFTBinSize);

  // prepare KNN queries

  annQueryCount := 0;
  for frmIdx := AKF.StartFrame to AKF.EndFrame do
  begin
    Frame := FFrames[frmIdx];

    Frame.AcquireFrameTiles;
    try
      for y := 0 to FTileMapHeight - 1 do
        for x := 0 to FTileMapWidth - 1 do
        begin
          if Frame.TileMap[y, x].PalIdx <> APaletteIndex then
            Continue;

          ComputeTilePsyVisFeatures(Frame.FrameTiles[y * FTileMapWidth + x]^, False, False, cFTQWeighting, False, False, AFTGamma, nil, PFloat(@DCTs[annQueryCount * cTileDCTSize]));

          Inc(annQueryCount);
        end;
    finally
      Frame.ReleaseFrameTiles;
    end;
  end;

  // query KNN

  flann_find_nearest_neighbors_index(DS^.FLANN, @DCTs[0], annQueryCount, @idxs[0], @errs[0], cFTBinSize, DS^.FLANNParams);

  // map keyframe tilemap items to reduced tiles and mirrors, parsing KNN query

  annQueryPos := 0;
  for frmIdx := AKF.StartFrame to AKF.EndFrame do
  begin
    Frame := FFrames[frmIdx];

    for y := 0 to FTileMapHeight - 1 do
      for x := 0 to FTileMapWidth - 1 do
      begin
        if Frame.TileMap[y, x].PalIdx <> APaletteIndex then
          Continue;

        Best.bestIdx := idxs[annQueryPos * cFTBinSize];
        Best.bestErr := errs[annQueryPos * cFTBinSize];
        Best.bestBlendCur := cMaxFTBlend;
        Best.bestBlendPrev := 0;
        Best.bestX := x;
        Best.bestY := y;

        // try to blend a local tile of the previous frame to improve likeliness

        if (AFTBlend >= 0) and not IsZero(sqrt(Best.bestErr), AFTBlendThres) then
          DoTileBlending(Frame, APaletteIndex, prevKFPalIdx, AFTGamma, AFTBlend, x, y, @DCTs[annQueryPos * cTileDCTSize], @idxs[annQueryPos * cFTBinSize], @errs[annQueryPos * cFTBinSize], Best);

        tmiO := @FFrames[Frame.Index].TileMap[y, x];

        attrs := DS^.TDToAttrs[Best.bestIdx];
        tmiO^.FTTileDSIdx := Best.bestIdx;
        tmiO^.TileIdx := DS^.TDToTileIdx[Best.bestIdx];
        tmiO^.HMirror := (attrs and 1) <> 0;
        tmiO^.VMirror := (attrs and 2) <> 0;
        tmiO^.BlendCur := Best.bestBlendCur;
        tmiO^.BlendPrev := Best.bestBlendPrev;
        tmiO^.BlendX := Best.bestX - x;
        tmiO^.BlendY := Best.bestY - y;

        tmiO^.SmoothedTileIdx := tmiO^.TileIdx;
        tmiO^.SmoothedPalIdx := tmiO^.PalIdx;
        tmiO^.SmoothedHMirror := tmiO^.HMirror;
        tmiO^.SmoothedVMirror := tmiO^.VMirror;

        errCml += Best.bestErr;

        FTiles[tmiO^.TileIdx]^.Active := True;

        Inc(annQueryPos);
      end;
  end;
  Assert(annQueryPos = annQueryCount);

  SpinEnter(@FLock);
  AKF.FTErrCml += errCml;
  SpinLeave(@FLock);
end;

procedure TTilingEncoder.DoTemporalSmoothing(AFrame, APrevFrame: TFrame; Y: Integer; Strength: TFloat;
  AddlTilesThres: TFloat; NonAddlCount: Int64);
const
  cSqrtFactor = 1 / cTileDCTSize;
var
  sx: Integer;
  cmp: TFloat;
  TMI, PrevTMI: PTileMapItem;
  DCT, PrevDCT, PrevPlainDCT: array[0 .. cTileDCTSize - 1] of TFloat;
  ATList: TList;
  addlTile: PTile;
  plan: TMixingPlan;
begin
  APrevFrame.AcquireFrameTiles;
  try
    for sx := 0 to FTileMapWidth - 1 do
    begin
      PrevTMI := @APrevFrame.TileMap[Y, sx];
      ComputeTilePsyVisFeatures(FTiles[PrevTMI^.SmoothedTileIdx]^, True, False, True, PrevTMI^.SmoothedHMirror, PrevTMI^.SmoothedVMirror, -1, APrevFrame.PKeyFrame.PaletteRGB[PrevTMI^.SmoothedPalIdx], PFloat(@PrevDCT[0]));

      // prevent smoothing from crossing keyframes (to ensure proper seek)

      if AFrame.PKeyFrame = APrevFrame.PKeyFrame then
      begin
        // compare DCT of current tile with tile from prev frame tilemap

        TMI := @AFrame.TileMap[Y, sx];
        ComputeTilePsyVisFeatures(FTiles[TMI^.SmoothedTileIdx]^, True, False, True, TMI^.SmoothedHMirror, TMI^.SmoothedVMirror, -1, AFrame.PKeyFrame.PaletteRGB[TMI^.SmoothedPalIdx], PFloat(@DCT[0]));

        cmp := CompareEuclideanDCT(DCT, PrevDCT);
        cmp := sqrt(cmp * cSqrtFactor);

        // if difference is low enough, mark the tile as smoothed for tilemap compression use

        if (cmp <= Strength) then
        begin
          if TMI^.SmoothedTileIdx >= PrevTMI^.SmoothedTileIdx then // lower tile index means the tile is used more often
          begin
            TMI^.SmoothedTileIdx := PrevTMI^.SmoothedTileIdx;
            TMI^.SmoothedPalIdx := PrevTMI^.SmoothedPalIdx;
            TMI^.SmoothedHMirror := PrevTMI^.SmoothedHMirror;
            TMI^.SmoothedVMirror := PrevTMI^.SmoothedVMirror;
          end
          else
          begin
            PrevTMI^.SmoothedTileIdx := TMI^.SmoothedTileIdx;
            PrevTMI^.SmoothedPalIdx := TMI^.SmoothedPalIdx;
            PrevTMI^.SmoothedHMirror := TMI^.SmoothedHMirror;
            PrevTMI^.SmoothedVMirror := TMI^.SmoothedVMirror;
          end;

          TMI^.Smoothed := True;
        end
        else
        begin
          TMI^.Smoothed := False;
        end;
      end;

      // compare the DCT of previous tile with the plaintext tile, if the difference is too high, add it to "additional tiles"

      ComputeTilePsyVisFeatures(APrevFrame.FrameTiles[Y * FTileMapWidth + sx]^, False, False, True, False, False, -1, nil, PFloat(@PrevPlainDCT[0]));

      cmp := CompareEuclideanDCT(PrevDCT, PrevPlainDCT);
      cmp := sqrt(cmp * cSqrtFactor);

      if (AddlTilesThres <> 0.0) and (cmp > AddlTilesThres) then
      begin
        ATList := FAdditionalTiles.LockList;
        try
          addlTile := TTile.New(True, True);
          addlTile^.CopyFrom(APrevFrame.FrameTiles[Y * FTileMapWidth + sx]^);
          addlTile^.UseCount := 1;
          addlTile^.Active := True;
          addlTile^.Additional := True;
          ATList.Add(addlTile);

          // redither tile (frame tiles don't have the paletted version)
          PreparePlan(plan, FDitheringYliluoma2MixedColors, APrevFrame.PKeyFrame.PaletteRGB[PrevTMI^.SmoothedPalIdx]);
          DitherTile(addlTile^, plan);
          TerminatePlan(plan);

          PrevTMI^.SmoothedTileIdx := NonAddlCount + ATList.Count - 1;
          PrevTMI^.SmoothedHMirror := False;
          PrevTMI^.SmoothedVMirror := False;
        finally
          FAdditionalTiles.UnlockList;
        end;
      end;
    end;
  finally
    APrevFrame.ReleaseFrameTiles;
  end;
end;

function TTilingEncoder.GetTileUseCount(ATileIndex: Int64): Integer;
var
  i, sx, sy: Integer;
begin
  Result := 0;
  for i := 0 to High(FFrames) do
    for sy := 0 to FTileMapHeight - 1 do
      for sx := 0 to FTileMapWidth - 1 do
        Inc(Result, Ord(FFrames[i].TileMap[sy, sx].TileIdx = ATileIndex));
end;

function TTilingEncoder.GetTileZoneSum(const ATile: TTile; x, y, w, h: Integer): Integer;
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

function TTilingEncoder.GetTilePalZoneThres(const ATile: TTile; ZoneCount: Integer; Zones: PByte): Integer;
var
  i, x, y: Integer;
  b: Byte;
  signi: Boolean;
  acc: array[0..sqr(cTileWidth)-1] of Byte;
begin
  FillByte(acc[0], Length(acc), 0);
  for y := 0 to cTileWidth - 1 do
    for x := 0 to cTileWidth - 1 do
    begin
      b := ATile.PalPixels[y, x];
      Inc(acc[b * ZoneCount div FPaletteSize]);
    end;

  Result := sqr(cTileWidth);
  for i := 0 to ZoneCount - 1 do
  begin
    Result := Min(Result, sqr(cTileWidth) - acc[i]);
    if Assigned(Zones) then
    begin
      signi := acc[i] > (FPaletteSize div ZoneCount);
      Zones^ := Ord(signi);
      Inc(Zones);
    end;
  end;
end;

type
  TKModesBin = record
    TileBin: TShortIntDynArray;
    ClusterCount: Integer;
    DatasetLength: Int64;
  end;

  PKModesBin = ^TKModesBin;
  TKModesBinArray = array of TKModesBin;
  PKModesBinArray = ^TKModesBinArray;

procedure TTilingEncoder.DoGlobalKMeans(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
var
  i, bin, lineIdx, clusterIdx, clusterLineCount, DSLen, BICOClusterCount, bestIdx: Integer;
  cnt, tidx: Int64;
  best, err: Double;

  T: PTile;
  KMBin: PKModesBin;
  BICO: PBICO;
  ANN: PANNkdtree;

  TileIndices: TInt64DynArray;
  Dataset: TDoubleDynArray2;
  ANNClusters: TIntegerDynArray;
  BICOCentroids, BICOWeights: TDoubleDynArray;
  ANNDataset: array of PANNFloat;
  ToMergeIdxs: TInt64DynArray;
begin
  KMBin := @PKModesBinArray(AData)^[AIndex];
  DSLen := KMBin^.DatasetLength;
  if (DSLen <= KMBin^.ClusterCount) or (KMBin^.ClusterCount <= 1) then
    Exit;

  // use BICO to compute a noise-aware set of centroids

  BICO := bico_create(cTileDCTSize, DSLen, KMBin^.ClusterCount, 1, KMBin^.ClusterCount, $42381337);
  try
    // parse bin dataset

    SetLength(TileIndices, DSLen);
    SetLength(Dataset, DSLen, cTileDCTSize);
    cnt := 0;
    for tidx := 0 to High(FTiles) do
    begin
      bin := KMBin^.TileBin[tidx];

      if bin <> AIndex then
        Continue;

      ComputeTilePsyVisFeatures(FTiles[tidx]^, False, False, False, False, False, -1, nil, @Dataset[cnt, 0]);

      // insert line into BICO
      bico_insert_line(BICO, @Dataset[cnt, 0], 1.0);

      TileIndices[cnt] := tidx;
      Inc(cnt);
    end;
    Assert(cnt = DSLen);

    // get BICO results

    SetLength(BICOCentroids, KMBin^.ClusterCount * cTileDCTSize);
    SetLength(BICOWeights, KMBin^.ClusterCount);

    BICOClusterCount := bico_get_results(BICO, @BICOCentroids[0], @BICOWeights[0]);

    WriteLn('Bin: ', AIndex:2, ' DatasetSize: ', cnt:8, ' BICOClusterCount: ', BICOClusterCount:6, ' ClusterCount: ', KMBin^.ClusterCount:6);

  finally
    bico_destroy(BICO);
  end;

  if BICOClusterCount <= 0 then
    Exit;

  // use ANN to compute cluster indexes

  SetLength(ANNDataset, BICOClusterCount);
  SetLength(ANNClusters, DSLen);

  for clusterIdx := 0 to BICOClusterCount - 1 do
    ANNDataset[clusterIdx] := @BICOCentroids[clusterIdx * cTileDCTSize];

  ANN := ann_kdtree_create(@ANNDataset[0], BICOClusterCount, cTileDCTSize, 8, ANN_KD_SUGGEST);
  try
    for i := 0 to DSLen - 1 do
      ANNClusters[i] := ann_kdtree_search(ANN, @Dataset[i, 0], 0.0, @err);
  finally
    ann_kdtree_destroy(ANN);
  end;

  // build a list of this centroid tiles

  SetLength(ToMergeIdxs, DSLen);

  for clusterIdx := 0 to BICOClusterCount - 1 do
  begin
    bestIdx := -1;
    best := MaxSingle;
    clusterLineCount := 0;
    for lineIdx := 0 to DSLen - 1 do
      if ANNClusters[lineIdx] = clusterIdx then
      begin
        T := FTiles[TileIndices[lineIdx]];

        // compute a tile weight of centroid closeness
        T^.Weight := CompareEuclidean(@Dataset[lineIdx, 0], @BICOCentroids[clusterIdx * cTileDCTSize], cTileDCTSize);

        if T^.Weight < best then
        begin
          best := T^.Weight;
          bestIdx := TileIndices[lineIdx];
        end;

        if T^.Weight > FGlobalTilingSoftClusteringThreshold then
        begin
          ToMergeIdxs[clusterLineCount] := TileIndices[lineIdx];
          Inc(clusterLineCount);
        end;
      end;

    if clusterLineCount >= 2 then
    begin
      SpinEnter(@FLock);
      MergeTiles(ToMergeIdxs, clusterLineCount, IfThen(bestIdx >= 0, bestIdx, ToMergeIdxs[0]), nil, nil);
      SpinLeave(@FLock);
    end;
  end;

  WriteLn('Bin: ', AIndex:2, ' Done!');
end;


procedure TTilingEncoder.DoGlobalTiling(DesiredNbTiles: Integer);
const
  cBinCountShift = 0;
var
  KMBins: TKModesBinArray;
  bin: Integer;
  i, disCnt, tidx: Int64;
  cnt: TInt64DynArray;
  share: TFloat;
  TileBin: TShortIntDynArray;
begin

  SetLength(KMBins, Sqr(cTileWidth) shr cBinCountShift);
  SetLength(cnt, Length(KMBins));

  // precompute dataset size

  SetLength(TileBin, Length(FTiles));
  FillByte(TileBin[0], Length(TileBin), Byte(-1));
  FillQWord(cnt[0], Length(cnt), 0);
  for tidx := 0 to High(FTiles) do
  begin
    if not FTiles[tidx]^.Active then
      Continue;

    bin := GetTilePalZoneThres(FTiles[tidx]^, sqr(cTileWidth), nil);
    bin := bin shr cBinCountShift;

    TileBin[tidx] := bin;

    Inc(cnt[bin]);
  end;

  for i := 0 to High(KMBins) do
  begin
    KMBins[i].TileBin := TileBin;
    KMBins[i].DatasetLength := cnt[i];
  end;

  // share DesiredNbTiles among bins, proportional to amount of tiles

  disCnt := 0;
  for i := 0 to High(cnt) do
    disCnt += cnt[i];
  share := DesiredNbTiles / disCnt;

  for i := 0 to High(cnt) do
    KMBins[i].ClusterCount := Max(1, Trunc(cnt[i] * share));

  ProgressRedraw(1);

  // run the clustering algorithm, which will group similar tiles until it reaches a fixed amount of groups

  InitMergeTiles;
  ProcThreadPool.DoParallel(@DoGlobalKMeans, 0, High(KMBins), @KMBins);
  FinishMergeTiles;

  ProgressRedraw(2);
end;

procedure TTilingEncoder.ReloadPreviousTiling(AFN: String);
var
  ParallelCount: PtrUInt;
  KNNDataset: TFloatDynArray;
  FLANN: flann_index_t;
  FLANNParameters: TFLANNParameters;

  procedure DoFindBest(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    bin, knnTidx: Integer;
    last, tidx: Int64;
    DataLine: array[0 .. sqr(cTileWidth) - 1] of TFloat;
    err: TFloat;
  begin
    if not InRange(AIndex, 0, ParallelCount - 1) then
      Exit;

    bin := Length(FTiles) div ParallelCount;
    last := (AIndex + 1) * bin - 1;
    if AIndex >= ParallelCount - 1 then
      last := High(FTiles);

    for tidx := bin * AIndex to last do
    begin
      if FTiles[tidx]^.Active then
      begin
        FTiles[tidx]^.ExtractPalPixels(DataLine);

        flann_find_nearest_neighbors_index(FLANN, @DataLine[0], 1, @knnTidx, @err, 1, @FLANNParameters);

        FTiles[tidx]^.LoadPalPixels(@KNNDataset[knnTidx * Sqr(cTileWidth)]);
      end;

      if tidx mod 10000 = 0 then
        WriteLn('Thread: ', GetCurrentThreadId:6, ' TileIdx: ', tidx:12);
    end;
  end;

var
  i, y, x, Version, NewTileCount: Integer;
  fs: TFileStream;
  T: PTile;
  TilingPaletteSize: Integer;
  PalPixels: TPalPixels;
  speedup: Single;
begin
  fs := TFileStream.Create(AFN, fmOpenRead or fmShareDenyNone);
  T := TTile.New(False, True);
  FLANN := nil;
  try
    T^.Active := True;

    Version := -1;
    if fs.Size mod (sqr(cTileWidth) + SizeOf(Integer)) = 2 then
      Version := fs.ReadByte; // Version

    TilingPaletteSize := sqr(cTileWidth);
    if (Version >= 0) or (fs.Size mod sqr(cTileWidth) <> 0) then
      TilingPaletteSize := fs.ReadByte;

    NewTileCount := fs.Size div sqr(cTileWidth);
    if Version >=0 then
      NewTileCount := fs.Size div (sqr(cTileWidth) + SizeOf(Integer));

    SetLength(KNNDataset, NewTileCount * Sqr(cTileWidth));

    for i := 0 to NewTileCount - 1 do
    begin
      if Version >= 0 then
        T^.UseCount := fs.ReadDWord;
      fs.ReadBuffer(PalPixels[0, 0], SizeOf(TPalPixels));
      for y := 0 to cTileWidth - 1 do
        for x := 0 to cTileWidth - 1 do
          PalPixels[y, x] := (PalPixels[y, x] * FPaletteSize) div TilingPaletteSize;
      T^.CopyPalPixels(PalPixels);
      T^.ExtractPalPixels(@KNNDataset[i * Sqr(cTileWidth)]);
    end;

    FLANNParameters := CDefaultFLANNParameters;
    FLANNParameters.checks := Sqr(cTileWidth);
    FLANN := flann_build_index(@KNNDataset[0], NewTileCount, Sqr(cTileWidth), @speedup, @FLANNParameters);

    ProgressRedraw(1);

    ParallelCount := ProcThreadPool.MaxThreadCount * 10;
    ProcThreadPool.DoParallelLocalProc(@DoFindBest, 0, ParallelCount - 1);

    ProgressRedraw(2);

  finally
    if Assigned(FLANN) then
      flann_free_index(FLANN, @FLANNParameters);
    TTile.Dispose(T);
    fs.Free;
  end;
end;

function CompareTileUseCountRev(Item1, Item2, UserParameter:Pointer):Integer;
var
  t1, t2: PTile;
begin
  t1 := PPTile(Item1)^;
  t2 := PPTile(Item2)^;
  Result := CompareValue(Ord(t1^.Additional), Ord(t2^.Additional));
  if Result = 0 then
    Result := CompareValue(t2^.UseCount, t1^.UseCount);
  if Result = 0 then
    Result := CompareValue(t1^.TmpIndex, t2^.TmpIndex);
end;

procedure TTilingEncoder.ReindexTiles(KeepRGBPixels: Boolean);
var
  i, x, y: Integer;
  pos, cnt, tidx: Int64;
  IdxMap: TInt64DynArray;
  Frame: ^TFrame;
  LocTiles: PTileDynArray;
begin
  cnt := 0;
  for tidx := 0 to High(FTiles) do
  begin
    FTiles[tidx]^.KFSoleIndex := -1;
    FTiles[tidx]^.TmpIndex := tidx;
    if (FTiles[tidx]^.Active) and (FTiles[tidx]^.UseCount > 0) then
      Inc(cnt);
  end;

  if cnt <= 0 then
    Exit;

  // pack the global Tiles, removing inactive ones

  LocTiles := TTile.Array1DNew(cnt, KeepRGBPixels, True);
  pos := 0;
  for tidx := 0 to High(FTiles) do
    if (FTiles[tidx]^.Active) and (FTiles[tidx]^.UseCount > 0) then
    begin
      LocTiles[pos]^.CopyFrom(FTiles[tidx]^);
      Inc(pos);
    end;

  SetLength(IdxMap, Length(FTiles));
  FillQWord(IdxMap[0], Length(FTiles), QWord(-1));

  TTile.Array1DDispose(FTiles);
  FTiles := LocTiles;
  LocTiles := nil;

  // sort global Tiles by use count descending (to make smoothing work better) then by tile index (to make tile indexes compression work better)

  QuickSort(FTiles[0], 0, High(FTiles), SizeOf(PTile), @CompareTileUseCountRev);

  // point tilemap items on new Tiles indexes

  for tidx := 0 to High(FTiles) do
    IdxMap[FTiles[tidx]^.TmpIndex] := tidx;

  for i := 0 to High(FFrames) do
  begin
    Frame := @FFrames[i];
    for y := 0 to (FTileMapHeight - 1) do
      for x := 0 to (FTileMapWidth - 1) do
      begin
        tidx := Frame^.TileMap[y,x].SmoothedTileIdx;
        if tidx >= 0 then
          Frame^.TileMap[y,x].SmoothedTileIdx := IdxMap[tidx];

        tidx := Frame^.TileMap[y,x].TileIdx;
        if tidx >= 0 then
        begin
          tidx := IdxMap[tidx];

          Frame^.TileMap[y,x].TileIdx := tidx;

          if tidx >= 0 then
          begin
            if FTiles[tidx]^.KFSoleIndex < 0 then
              FTiles[tidx]^.KFSoleIndex := Frame^.PKeyFrame.Index
            else if FTiles[tidx]^.KFSoleIndex <> Frame^.PKeyFrame.Index then
              FTiles[tidx]^.KFSoleIndex := MaxSmallint;
          end;
        end;
      end;
  end;

  // in case a tile is used by only one keyframe, make it intra keyframe (ie. located in the corresponging keyframe)

  cnt := 0;
  for tidx := 0 to High(FTiles) do
  begin
    FTiles[tidx]^.IntraKF := FTiles[tidx]^.KFSoleIndex <> MaxSmallint;
    if FTiles[tidx]^.IntraKF then
      Inc(cnt);
  end;
  WriteLn('ReindexTiles: ', cnt, ' / ', Length(FTiles), ' made intra');
end;

procedure TTilingEncoder.SaveStream(AStream: TStream; ASpecificKF: Integer);
const
  CGTMCommandsCount = Ord(High(TGTMCommand)) + 1;
  CGTMCommandCodeBits = round(ln(CGTMCommandsCount) / ln(2));
  CGTMCommandBits = 16 - CGTMCommandCodeBits;
  CMinBlkSkipCount = 1;
  CMaxBlkSkipCount = 1 shl CGTMCommandBits;

var
  ZStream: TMemoryStream;

  procedure Do3Bytes(v: Cardinal);
  begin
    Assert(v < 1 shl 24);
    ZStream.WriteByte(v and $ff);
    v := v shr 8;
    ZStream.WriteByte(v and $ff);
    v := v shr 8;
    ZStream.WriteByte(v and $ff);
  end;

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

  function ExtractTMIAttributes(const TMI: TTileMapItem; out attrs, blend: Word; out isBlend: Boolean): Integer;
  begin
    attrs := (TMI.SmoothedPalIdx shl 2) or (Ord(TMI.SmoothedVMirror) shl 1) or Ord(TMI.SmoothedHMirror);
    blend := (Word(TMI.BlendY and $f) shl 12) or (Word(TMI.BlendX and $f) shl 8) or (Word(TMI.BlendPrev and $f) shl 4) or Word(TMI.BlendCur and $f);
    isBlend := (FFrameTilingBlendingSize < 0) or ((TMI.BlendPrev = 0) and (TMI.BlendCur >= cMaxFTBlend)) and not FTiles[TMI.TileIdx]^.Additional;
    Result := TMI.SmoothedTileIdx;
  end;

  procedure DoTMI(const TMI: TTileMapItem);
  var
    tileIdx: Integer;
    attrs, blend: Word;
    isBlend: Boolean;
  begin
    Assert((TMI.SmoothedPalIdx >= 0) and (TMI.SmoothedPalIdx < FPaletteCount));

    tileIdx := ExtractTMIAttributes(TMI, attrs, blend, isBlend);

    if tileIdx < (1 shl 16) then
    begin
      if isBlend then
      begin
        DoCmd(gtShortTileIdx, attrs);
        DoWord(tileIdx);
      end
      else
      begin
        DoCmd(gtShortBlendTileIdx, attrs);
        DoWord(tileIdx);
        DoWord(blend);
      end;
    end
    else
    begin
      if isBlend then
      begin
        DoCmd(gtLongTileIdx, attrs);
        DoDWord(tileIdx);
      end
      else
      begin
        DoCmd(gtLongBlendTileIdx, attrs);
        DoDWord(tileIdx);
        DoWord(blend);
      end;
    end;
  end;

  procedure WriteKFAttributes(KF: TKeyFrame);
  var
    i, j: Integer;
  begin
    for j := 0 to FPaletteCount - 1 do
    begin
      DoCmd(gtLoadPalette, 0);
      DoByte(j);
      DoByte(0);
      for i := 0 to FPaletteSize - 1 do
        DoDWord(KF.PaletteRGB[j, i] or $ff000000);
    end;
  end;

  procedure WriteListTiles(List: TIntegerList);
  var
    PrevIdx, StartIdx, idx, i, j: Integer;
  begin
    List.Sort(@CompareIntegers);

    if List.Count > 0 then
    begin
      StartIdx := List[0];
      for i := 1 to List.Count - 1 + 1 do
      begin
        idx := MaxInt;
        if i < List.Count then
          idx := List[i];
        PrevIdx := List[i - 1];

        if idx - PrevIdx > 1 then
        begin
          DoCmd(gtTileSet, FPaletteSize);
          DoDWord(StartIdx); // start tile
          DoDWord(PrevIdx); // end tile

          for j := StartIdx to PrevIdx do
          begin
            Assert(FTiles[j]^.Active);
            ZStream.Write(FTiles[j]^.GetPalPixelsPtr^[0, 0], sqr(cTileWidth));
          end;

          StartIdx := idx;
        end;
      end;
    end;
  end;

  procedure WriteKFIntraTiles(KF: Integer);
  var
    fri, tx, ty: Integer;
    frm: TFrame;
    tmi: PTileMapItem;
    IntraList: TIntegerList;
  begin
    IntraList := TIntegerList.Create;
    try
      for fri := FKeyFrames[KF].StartFrame to FKeyFrames[KF].EndFrame do
      begin
        frm := FFrames[fri];
        for ty := 0 to FTileMapHeight - 1 do
          for tx := 0 to FTileMapWidth - 1 do
          begin
            tmi := @frm.TileMap[ty, tx];
            if FTiles[tmi^.SmoothedTileIdx]^.IntraKF then
              IntraList.Add(tmi^.SmoothedTileIdx);
          end;
      end;

      WriteListTiles(IntraList);
    finally
      IntraList.Free;
    end;
  end;

  procedure WriteGlobalTiles;
  var
    i: Int64;
    GlobalList: TIntegerList;
  begin
    DoCmd(gtSetDimensions, 0);
    DoWord(FTileMapWidth); // frame tilemap width
    DoWord(FTileMapHeight); // frame tilemap height
    DoDWord(round(1000*1000*1000 / FFramesPerSecond)); // frame length in nanoseconds
    DoDWord(GetGlobalTileCount); // tile count

    GlobalList := TIntegerList.Create;
    try
      for i := 0 to High(FTiles) do
        if FTiles[i]^.Active and not FTiles[i]^.IntraKF then
          GlobalList.Add(i);

      WriteListTiles(GlobalList);
    finally
      GlobalList.Free;
    end;
  end;

var
  StartPos, StreamSize, LastKF, KFCount, KFSize, BlkSkipCount: Integer;
  kf, fri, yx, yxs, cs, sx, sy: Integer;
  IsKF: Boolean;
  frm: TFrame;
  Header: TGTMHeader;
  KFInfo: array of TGTMKeyFrameInfo;
begin
  StartPos := AStream.Size;

  FillChar(Header, SizeOf(Header), 0);
  Header.FourCC := 'GTMv';
  Header.RIFFSize := SizeOf(Header) - SizeOf(Header.FourCC) - SizeOf(Header.RIFFSize);
  Header.EncoderVersion := 2; // 2 -> fixed blending extents
  Header.FramePixelWidth := FScreenWidth;
  Header.FramePixelHeight := FScreenHeight;
  Header.KFCount := Length(FKeyFrames);
  Header.FrameCount := Length(FFrames);
  Header.AverageBytesPerSec := 0;
  Header.KFMaxBytesPerSec := 0;
  AStream.WriteBuffer(Header, SizeOf(Header));

  SetLength(KFInfo, Length(FKeyFrames));
  for kf := 0 to High(FKeyFrames) do
  begin
    FillChar(KFInfo[kf], SizeOf(KFInfo[0]), 0);
    KFInfo[kf].FourCC := 'GTMk';
    KFInfo[kf].RIFFSize := SizeOf(KFInfo[0]) - SizeOf(KFInfo[0].FourCC) - SizeOf(KFInfo[0].RIFFSize);
    KFInfo[kf].KFIndex := kf;
    KFInfo[kf].FrameIndex := FKeyFrames[kf].StartFrame;
    KFInfo[kf].TimeCodeMillisecond := Round(1000.0 * FKeyFrames[kf].StartFrame / FFramesPerSecond);
    AStream.WriteBuffer(KFInfo[kf], SizeOf(KFInfo[0]));
  end;

  Header.WholeHeaderSize := AStream.Size - StartPos;

  StartPos := AStream.Size;

  ZStream := TMemoryStream.Create;
  try
    WriteGlobalTiles;

    LastKF := 0;
    for kf := 0 to High(FKeyFrames) do
    begin
      WriteKFIntraTiles(kf); // has to be called before writing palettes
      WriteKFAttributes(FKeyFrames[kf]);

      for fri := FKeyFrames[kf].StartFrame to FKeyFrames[kf].EndFrame do
      begin
        frm := FFrames[fri];

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
              if not frm.TileMap[sy, sx].Smoothed then
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
              DoTMI(frm.TileMap[sy, sx]);
              Inc(cs);
            end;
          end;
        end;
        Assert(cs = FTileMapSize, 'incomplete TM');
        Assert(BlkSkipCount = 0, 'pending skips');

        IsKF := (fri = FKeyFrames[kf].EndFrame);

        DoCmd(gtFrameEnd, Ord(IsKF));

        if IsKF then
        begin
          KFCount := FKeyFrames[kf].EndFrame - LastKF + 1;
          LastKF := FKeyFrames[kf].EndFrame + 1;

          AStream.Position := AStream.Size;
          KFSize := AStream.Position;
          LZCompress(ZStream, False, AStream);
          ZStream.Clear;

          KFSize := AStream.Size - KFSize;

          KFInfo[kf].RawSize := ZStream.Size;
          KFInfo[kf].CompressedSize := KFSize;
          if (kf > 0) or (Length(FKeyFrames) = 1) then
            Header.KFMaxBytesPerSec := max(Header.KFMaxBytesPerSec, round(KFSize * FFramesPerSecond / KFCount));
          Header.AverageBytesPerSec += KFSize;

          WriteLn('KF: ', FKeyFrames[kf].StartFrame:8, ' FCnt: ', KFCount:4, ' Written: ', KFSize:8, ' Bitrate: ', (KFSize / 1024.0 * 8.0 / KFCount):8:2, ' kbpf   (', (KFSize / 1024.0 * 8.0 / KFCount * FFramesPerSecond):8:2, ' kbps)');
        end;
      end;
    end;
  finally
    ZStream.Free;
  end;

  Header.AverageBytesPerSec := round(Header.AverageBytesPerSec * FFramesPerSecond / Length(FFrames));
  AStream.Position := 0;
  AStream.WriteBuffer(Header, SizeOf(Header));
  for kf := 0 to High(FKeyFrames) do
    AStream.WriteBuffer(KFInfo[kf], SizeOf(KFInfo[0]));
  AStream.Position := AStream.Size;

  StreamSize := AStream.Size - StartPos;

  WriteLn('Written: ', StreamSize:12, ' Bitrate: ', (StreamSize / 1024.0 * 8.0 / Length(FFrames)):8:2, ' kbpf  (', (StreamSize / 1024.0 * 8.0 / Length(FFrames) * FFramesPerSecond):8:2, ' kbps)');
end;

procedure TTilingEncoder.SaveRawTiles(OutFN: String);
var
  fs: TFileStream;
  tidx: Int64;
begin
  // save raw tiles

  if DirectoryExists(ExtractFilePath(OutFN)) then
  begin
    fs := TFileStream.Create(OutFN, fmCreate or fmShareDenyWrite);
    try
      fs.WriteByte(0); // version
      fs.WriteByte(FPaletteSize);
      for tidx := 0 to High(FTiles) do
        if FTiles[tidx]^.Active then
        begin
          fs.WriteDWord(FTiles[tidx]^.UseCount);
          fs.Write(FTiles[tidx]^.GetPalPixelsPtr^[0, 0], sqr(cTileWidth));
        end;
    finally
      fs.Free;
    end;
  end;
end;

function TTilingEncoder.DoExternalFFMpeg(AFN: String; var AVidPath: String; AStartFrame, AFrameCount: Integer; AScale: Double; out AFPS: Double): String;
var
  i: Integer;
  Output, ErrOut, vfl, s: String;
  Process: TProcess;
begin
  Process := TProcess.Create(nil);

  Result := IncludeTrailingPathDelimiter(sysutils.GetTempDir) + 'tiler_png\';
  ForceDirectories(Result);

  DeleteDirectory(Result, True);

  AVidPath := Result + '%.4d.png';

  vfl := ' -vf select="between(n\,' + IntToStr(AStartFrame) + '\,' +
    IntToStr(IfThen(AFrameCount > 0, AStartFrame + AFrameCount - 1, MaxInt)) +
    '),setpts=PTS-STARTPTS,scale=in_range=auto:out_range=full",scale=iw*' + FloatToStr(AScale, InvariantFormatSettings) + ':ih*' + FloatToStr(AScale, InvariantFormatSettings) + ':flags=lanczos ';

  Process.CurrentDirectory := ExtractFilePath(ParamStr(0));
  Process.Executable := 'ffmpeg.exe';
  Process.Parameters.Add('-y -i "' + AFN + '" ' + vfl + ' -compression_level 5 -pix_fmt rgb24 "' + Result + '%04d.png' + '"');
  Process.ShowWindow := swoHIDE;
  Process.Priority := ppIdle;

  i := 0;
  Output := '';
  ErrOut := '';
  internalRuncommand(Process, Output, ErrOut, i, True); // destroys Process
  WriteLn;

  s := ErrOut;
  s := Copy(s, 1, Pos(' fps', s) - 1);
  s := ReverseString(s);
  s := Copy(s, 1, Pos(' ', s) - 1);
  s := ReverseString(s);
  AFPS := StrToFloatDef(s, 24.0, InvariantFormatSettings);
end;

constructor TTilingEncoder.Create;
begin
  FormatSettings.DecimalSeparator := '.';
  InitializeCriticalSection(FCS);
  SpinLeave(@FLock);

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

  FGlobalTilingSoftClusteringThreshold := 0.3;
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

end.

