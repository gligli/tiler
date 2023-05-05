unit tilingencoder;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}
{$TYPEDADDRESS ON}

{$define ASM_DBMP}


interface

uses
  windows, Classes, SysUtils, strutils, types, Math, FileUtil, typinfo, zstream, Process, LazLogger, IniFiles,
  Graphics, IntfGraphics, FPimage, FPCanvas, FPWritePNG, GraphType, fgl, MTProcs, extern, tbbmalloc, kmodes;

type
  TEncoderStep = (esNone = -1, esLoad = 0, esDither, esTile, esSave);
  TKeyFrameReason = (kfrNone, kfrManual, kfrLength, kfrDecorrelation);
  TKeyFrameProcess = (kfpAll = -1, kfpLoadTiles = 0, kfpDither, kfpCluster, kfpReconstruct, kfpReColor, kfpReindex, kfpSmooth);
  TRenderPage = (rpNone, rpInput, rpOutput, rpTilesPalette);

const
  // tweakable constants

  cFTQWeighting = False;
  cYakmoMaxIterations = 300;
  cGlobalTilingCoresetMultiplier = 1.0;

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
  cMaxFTBlend = 16;
  cLumaDiv = cRedMul + cGreenMul + cBlueMul;
  cVecInvWidth = 16;
  cTileWidthBits = 3;
  cTileWidth = 1 shl cTileWidthBits;
  cColorCpns = 3;
  cTileDCTSize = cColorCpns * sqr(cTileWidth);
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

  cEncoderStepLen: array[TEncoderStep] of Integer = (0, 3, -1, -1, 1);

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
  // SkipBlock:               data -> none; commandBits -> skip count - 1 (10 bits)
  // ShortTileIdx:            data -> tile index (16 bits); commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
  // LongTileIdx:             data -> tile index (32 bits); commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
  // LoadPalette:             data -> palette index (8 bits); palette format (8 bits) (0: RGBA32); RGBA bytes (32bits); commandBits -> none
  // ShortBlendTileIdx:       data -> tile index (16 bits); BlendY offset(4 bits); BlendX offset(4 bits); Previous frame factor (4 bits); Current frame factor (4 bits);
  //                            commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
  // LongLendTileIdx:         data -> tile index (32 bits); BlendY offset(4 bits); BlendX offset(4 bits); Previous frame factor (4 bits); Current frame factor (4 bits);
  //                            commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
  // ShortAdditionalTileIdx:  data -> tile index (16 bits) * 2; commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
  // LongAdditionalTileIdx:   data -> tile index (32 bits) * 2; commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
  //
  // (insert new commands here...)
  //
  // FrameEnd:                data -> none; commandBits bit 0 -> keyframe end
  // TileSet:                 data -> start tile (32 bits); end tile (32 bits); { indexes per tile (64 bytes) } * count; commandBits -> indexes count per palette
  // SetDimensions:           data -> height in tiles (16 bits); width in tiles (16 bits); frame length in nanoseconds (32 bits) (2^32-1: still frame); tile count (32 bits); commandBits -> none
  // ExtendedCommand:         data -> custom commands, proprietary extensions, ...; commandBits -> extended command index (10 bits)
  //
  // ReservedArea:            reserving the MSB for future use (do not use for new commands)

  TGTMCommand = (
    gtSkipBlock = 0,
    gtShortTileIdx = 1,
    gtLongTileIdx = 2,
    gtLoadPalette = 3,
    gtShortBlendTileIdx = 4,
    gtLongBlendTileIdx = 5,
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

  { TTile }

  TTile = packed record // /!\ update TTileHelper.CopyFrom each time this structure is changed /!\
    UseCount: Cardinal;
    TmpIndex, MergeIndex: Integer;
    FrameSoleIndex: SmallInt;
    Flags: set of (tfActive, tfIntraFrame, tfAdditional, tfHasRGBPixels, tfHasPalPixels);
  end;

  PTileArray = array of PTile;

  { TTileHelper }

  TTileHelper = record helper for TTile
  private
    function GetActive: Boolean;
    function GetAdditional: Boolean;
    function GetHasPalPixels: Boolean;
    function GetHasRGBPixels: Boolean;
    function GetIntraFrame: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetAdditional(AValue: Boolean);
    procedure SetHasPalPixels(AValue: Boolean);
    procedure SetHasRGBPixels(AValue: Boolean);
    procedure SetIntraFrame(AValue: Boolean);
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
    property IntraFrame: Boolean read GetIntraFrame write SetIntraFrame;
    property Additional: Boolean read GetAdditional write SetAdditional;
    property HasRGBPixels: Boolean read GetHasRGBPixels write SetHasRGBPixels;
    property HasPalPixels: Boolean read GetHasPalPixels write SetHasPalPixels;
  end;

  { TTileMapItem }

  TTileMapItem = packed record
    TileIdx, BlendedTileIdx, TileIdx_Smoothed, BlendedTileIdx_Smoothed: Integer;
    DitherPsyV: TFloat;
    PalIdx, PalIdx_Smoothed: SmallInt;
    Flags: set of (tmfSmoothed, tmfHMirror, tmfVMirror, tmfHMirror_Smoothed, tmfVMirror_Smoothed, tmfAdditional);
  end;
  PTileMapItem = ^TTileMapItem;

  TTileMapItems = array of TTileMapItem;

  { TTileMapItemHelper }

  TTileMapItemHelper = record helper for TTileMapItem
  private
    function GetHMirror: Boolean;
    function GetSmoothed: Boolean;
    function GetAdditional: Boolean;
    function GetHMirror_Smoothed: Boolean;
    function GetVMirror_Smoothed: Boolean;
    function GetVMirror: Boolean;
    procedure SetHMirror(AValue: Boolean);
    procedure SetSmoothed(AValue: Boolean);
    procedure SetAdditional(AValue: Boolean);
    procedure SetHMirror_Smoothed(AValue: Boolean);
    procedure SetVMirror_Smoothed(AValue: Boolean);
    procedure SetVMirror(AValue: Boolean);
  public
    property Smoothed: Boolean read GetSmoothed write SetSmoothed;
    property HMirror: Boolean read GetHMirror write SetHMirror;
    property VMirror: Boolean read GetVMirror write SetVMirror;
    property HMirror_Smoothed: Boolean read GetHMirror_Smoothed write SetHMirror_Smoothed;
    property VMirror_Smoothed: Boolean read GetVMirror_Smoothed write SetVMirror_Smoothed;
    property Additional: Boolean read GetAdditional write SetAdditional;

    procedure CopyToSmoothed;
  end;


  { TTilingDataset }

  TTilingDataset = record
    Dataset: TFloatDynArray;
    DSToTileIdx: TIntegerDynArray;
    FLANN: flann_index_t;
    FLANNParams: PFLANNParameters;
  end;

  PTilingDataset = ^TTilingDataset;

  { TCountIndex }

  TCountIndex = record
    Index, Count, Luma: Integer;
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
    BlendedTileIdx: Integer;
  end;

  TTilingEncoder = class;
  TKeyFrame = class;

  { TFrame }

  TFrame = class
    PKeyFrame: TKeyFrame;
    Index: Integer;
    DitherPsyV: TFloat;

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
    Encoder: TTilingEncoder;
    Index, StartFrame, EndFrame, FrameCount: Integer;
    FramesLeft: Integer;
    Reason: TKeyFrameReason;

    FLock: TSpinlock;

    Tiles: PTileArray;

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

    PalettesLeft: Integer;
    ReconstructErrCml: Double;

    constructor Create(AParent: TTilingEncoder; AIndex, APaletteCount, AStartFrame, AEndFrame: Integer);
    destructor Destroy; override;

    // algorithms

    function GetFreeThreadCount: Integer;
    function GetTileCount(AActiveOnly: Boolean): Integer;

    procedure MergeTiles(const TileIndexes: array of Int64; TileCount: Integer; BestIdx: Int64; NewTile: PPalPixels; NewTileRGB: PRGBPixels);
    procedure InitMergeTiles;
    procedure FinishMergeTiles;

    procedure MakeTilesUnique;

    procedure PreparePalettes(ADitheringGamma: Integer);
    procedure QuantizePalette(APalIdx: Integer; UseYakmo: Boolean; DLv3BPC: Integer; ADitheringGamma: Integer);
    procedure FinishQuantizePalettes;
    procedure DitherTiles;

    procedure DoKeyFrameKMeans(AClusterCount: Integer);

    procedure PrepareKFTiling(APaletteIndex, AFTGamma: Integer);
    procedure FinishKFTiling(APaletteIndex: Integer);
    procedure DoKFTiling(APaletteIndex: Integer; AFTGamma: Integer; AFTBlendThres: TFloat; AFromPal: Boolean);

    procedure ReindexTiles(KeepRGBPixels: Boolean);

    procedure DoTemporalSmoothing(AFrame, APrevFrame: TFrame; Strength: TFloat; AddlTilesThres: TFloat; NonAddlCount: Int64; ATList: TList);

    // processes

    procedure LoadTiles;
    procedure Dither;
    procedure Cluster;
    procedure Reconstruct;
    procedure ReColor;
    procedure Smooth;
    procedure Reindex;

    procedure RunAll_Dither;
    procedure RunAll_Tile;
  end;

  TKeyFrameArray =  array of TKeyFrame;

  TTilingEncoderProgressEvent = procedure(ASender: TTilingEncoder; APosition, AMax: Integer; AHourGlass: Boolean) of object;

  { TTilingEncoder }

  TTilingEncoder = class
  private
    // encoder state variables

    FCS: TRTLCriticalSection;
    FKeyFramesLeft: Integer;
    FKeyFramesThreadCount: Integer;

    FGamma: array[0..1] of TFloat;
    FGammaCorLut: array[-1..1, 0..High(Byte)] of TFloat;
    FVecInv: array[0..256 * 4 - 1] of Cardinal;
    FDCTLut:array[Boolean {inversible?}, 0..sqr(sqr(cTileWidth)) - 1] of TFloat;
    FDCTLutDouble:array[Boolean {inversible?}, 0..sqr(sqr(cTileWidth)) - 1] of Double;
    FInvDCTLutDouble:array[0..sqr(sqr(cTileWidth)) - 1] of Double;

    FKeyFrames: TKeyFrameArray;
    FFrames: TFrameArray;

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
    FLoadPerFrameTileCountMultiplier: TFloat;
    FPaletteSize: Integer;
    FPaletteCount: Integer;
    FQuantizerUseYakmo: Boolean;
    FQuantizerDennisLeeBitsPerComponent: Integer;
    FQuantizerPosterize: Boolean;
    FQuantizerPosterizeBitsPerComponent: Integer;
    FDitheringUseGamma: Boolean;
    FDitheringUseThomasKnoll: Boolean;
    FDitheringYliluoma2MixedColors: Integer;
    FGlobalTilingTileCount: Integer;
    FGlobalTilingQualityBasedTileCount: TFloat;
    FFrameTilingFromPalette: Boolean;
    FFrameTilingUseGamma: Boolean;
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

    FProgressSyncPos, FProgressSyncMax: Integer;
    FProgressSyncHG: Boolean;

    function GetFrameCount: Integer;
    function GetKeyFrameCount: Integer;
    function GetMaxThreadCount: Integer;
    procedure SetDitheringYliluoma2MixedColors(AValue: Integer);
    procedure SetEncoderGammaValue(AValue: TFloat);
    procedure SetFrameCountSetting(AValue: Integer);
    procedure SetFramesPerSecond(AValue: Double);
    procedure SetFrameTilingBlendingThreshold(AValue: TFloat);
    procedure SetGlobalTilingQualityBasedTileCount(AValue: TFloat);
    procedure SetLoadPerFrameTileCountMultiplier(AValue: TFloat);
    procedure SetMaxThreadCount(AValue: Integer);
    procedure SetPaletteCount(AValue: Integer);
    procedure SetPaletteSize(AValue: Integer);
    procedure SetQuantizerDennisLeeBitsPerComponent(AValue: Integer);
    procedure SetQuantizerPosterizeBitsPerComponent(AValue: Integer);
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
    function ComputeInterFrameCorrelation(a, b: TFrame): TFloat;

    function GetSettings: String;
    procedure ClearAll;
    procedure ProgressRedraw(ASubStepIdx: Integer = -1; AProgressStep: TEncoderStep = esNone; AThread: TThread = nil);
    procedure SyncProgress;
    procedure InitLuts(ATilePaletteSize, APaletteCount: Integer);
    function GammaCorrect(lut: Integer; x: Byte): TFloat; inline;
    function GammaUncorrect(lut: Integer; x: TFloat): Byte; inline;

    function DoExternalFFMpeg(AFN: String; var AVidPath: String; AStartFrame, AFrameCount: Integer; AScale: Double; out
      AFPS: Double): String;

    function HSVToRGB(h, s, v: Byte): Integer;
    procedure RGBToHSV(col: Integer; out h, s, v: Byte); overload;
    procedure RGBToHSV(col: Integer; out h, s, v: TFloat); overload;
    procedure RGBToYUV(col: Integer; GammaCor: Integer; out y, u, v: TFloat);
    procedure RGBToYUV(r, g, b: Byte; GammaCor: Integer; out y, u, v: TFloat);
    procedure RGBToLAB(r, g, b: TFloat; GammaCor: Integer; out ol, oa, ob: TFloat);
    procedure RGBToLAB(ir, ig, ib: Integer; GammaCor: Integer; out ol, oa, ob: TFloat);
    function LABToRGB(ll, aa, bb: TFloat; GammaCor: Integer): Integer;
    function YUVToRGB(y, u, v: TFloat; GammaCor: Integer): Integer;

    procedure ComputeTilePsyVisFeatures(const ATile: TTile; FromPal, UseLAB, QWeighting, HMirror, VMirror,
     Inversible: Boolean; GammaCor: Integer; const pal: TIntegerDynArray; DCT: PFloat); inline; overload;
    procedure ComputeTilePsyVisFeatures(const ATile: TTile; FromPal, UseLAB, QWeighting, HMirror, VMirror,
     Inversible: Boolean; GammaCor: Integer; const pal: TIntegerDynArray; DCT: PDouble); inline; overload;
    procedure ComputeInvTilePsyVisFeatures(DCT: PDouble; UseLAB: Boolean; GammaCor: Integer; var ATile: TTile);

    // Dithering algorithms ported from http://bisqwit.iki.fi/story/howto/dither/jy/

    class function ColorCompare(r1, g1, b1, r2, g2, b2: Int64): Int64;
    procedure PreparePlan(var Plan: TMixingPlan; MixedColors: Integer; const pal: array of Integer);
    procedure TerminatePlan(var Plan: TMixingPlan);
    function DeviseBestMixingPlanYliluoma(var Plan: TMixingPlan; col: Integer; var List: array of Byte): Integer;
    procedure DeviseBestMixingPlanThomasKnoll(var Plan: TMixingPlan; col: Integer; var List: array of Byte);

    procedure LoadFrame(var AFrame: TFrame; AFrameIndex: Integer; const ABitmap: TRawImage);
    procedure FindKeyFrames(AManualMode: Boolean);
    function GetGlobalTileCount(AActiveOnly: Boolean): Integer;
    function GetFrameTileCount(AFrame: TFrame): Integer;

    function GetTileIndexTMItem(const ATile: TTile; out AFrame: TFrame): PTileMapItem;
    procedure DitherTile(var ATile: TTile; var Plan: TMixingPlan);
    class function GetTileZoneSum(const ATile: TTile; x, y, w, h: Integer): Integer;
    class function GetTilePalPixelsSum(const ATile: TTile): Integer;
    function GetTilePalZoneThres(const ATile: TTile; ZoneCount: Integer; Zones: PByte): Integer;
    class procedure GetTileHVMirrorHeuristics(const ATile: TTile; out AHMirror, AVMirror: Boolean);
    class procedure HMirrorTile(var ATile: TTile; APalOnly: Boolean = False);
    class procedure VMirrorTile(var ATile: TTile; APalOnly: Boolean = False);
    function GetTileUseCount(ATileIndex: Int64): Integer;

    procedure SaveStream(AStream: TStream; ASpecificKF: Integer = -1);

    procedure ReframeUI(AWidth, AHeight: Integer);
  public
    // constructor / destructor

    constructor Create;
    destructor Destroy; override;

    // processes / functions

    procedure Load;
    procedure Dither(AKeyFrameProcess: TKeyFrameProcess = kfpAll);
    procedure Tile(AKeyFrameProcess: TKeyFrameProcess = kfpAll);
    procedure Save;

    procedure Render(AFast: Boolean);
    procedure GeneratePNGs;
    procedure SaveSettings(ASettingsFileName: String);
    procedure LoadSettings(ASettingsFileName: String);

    procedure Test;

    // encoder state variables

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
    property Scaling: TFloat read FScaling write SetScaling;
    property EncoderGammaValue: TFloat read FEncoderGammaValue write SetEncoderGammaValue;
    property PaletteSize: Integer read FPaletteSize write SetPaletteSize;
    property PaletteCount: Integer read FPaletteCount write SetPaletteCount;
    property QuantizerUseYakmo: Boolean read FQuantizerUseYakmo write FQuantizerUseYakmo;
    property QuantizerDennisLeeBitsPerComponent: Integer read FQuantizerDennisLeeBitsPerComponent write SetQuantizerDennisLeeBitsPerComponent;
    property QuantizerPosterizeBitsPerComponent: Integer read FQuantizerPosterizeBitsPerComponent write SetQuantizerPosterizeBitsPerComponent;
    property QuantizerPosterize: Boolean read FQuantizerPosterize write FQuantizerPosterize;
    property DitheringUseGamma: Boolean read FDitheringUseGamma write FDitheringUseGamma;
    property DitheringUseThomasKnoll: Boolean read FDitheringUseThomasKnoll write FDitheringUseThomasKnoll;
    property DitheringYliluoma2MixedColors: Integer read FDitheringYliluoma2MixedColors write SetDitheringYliluoma2MixedColors;
    property GlobalTilingTileCount: Integer read FGlobalTilingTileCount write SetGlobalTilingTileCount;
    property GlobalTilingQualityBasedTileCount: TFloat read FGlobalTilingQualityBasedTileCount write SetGlobalTilingQualityBasedTileCount;
    property FrameTilingFromPalette: Boolean read FFrameTilingFromPalette write FFrameTilingFromPalette;
    property FrameTilingUseGamma: Boolean read FFrameTilingUseGamma write FFrameTilingUseGamma;
    property FrameTilingBlendingThreshold: TFloat read FFrameTilingBlendingThreshold write SetFrameTilingBlendingThreshold;
    property SmoothingFactor: TFloat read FSmoothingFactor write SetSmoothingFactor;
    property SmoothingAdditionalTilesThreshold: TFloat read FSmoothingAdditionalTilesThreshold write SetSmoothingAdditionalTilesThreshold;
    property MaxThreadCount: Integer read GetMaxThreadCount write SetMaxThreadCount;

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

function ToLuma(r, g, b: Byte): Integer; inline;
begin
  Result := r * cRedMul + g * cGreenMul + b * cBlueMul;
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

function CompareTileUseCountRev(Item1, Item2, UserParameter:Pointer):Integer;
var
  t1, t2: PTile;
begin
  t1 := PPTile(Item1)^;
  t2 := PPTile(Item2)^;

  Result := CompareValue(t1^.FrameSoleIndex, t2^.FrameSoleIndex);
  if Result = 0 then
    Result := CompareValue(Ord(t1^.Additional), Ord(t2^.Additional));
  if Result = 0 then
    Result := CompareValue(t2^.UseCount, t1^.UseCount);
  if Result = 0 then
    Result := CompareValue(TTilingEncoder.GetTilePalPixelsSum(t1^), TTilingEncoder.GetTilePalPixelsSum(t2^));
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


function ComputeBlendingError_Asm(PPlain_rcx, PCur_rdx, PPrev_r8: PFloat; bc_xmm3, bp_stack: TFloat): TFloat; register; assembler;
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
  pshufd xmm2, dword ptr [bp_stack], 0
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

function TTileMapItemHelper.GetSmoothed: Boolean;
begin
  Result := tmfSmoothed in Flags;
end;

function TTileMapItemHelper.GetAdditional: Boolean;
begin
  Result := tmfAdditional in Flags;
end;

function TTileMapItemHelper.GetHMirror_Smoothed: Boolean;
begin
  Result := tmfHMirror_Smoothed in Flags;
end;

function TTileMapItemHelper.GetVMirror_Smoothed: Boolean;
begin
  Result := tmfVMirror_Smoothed in Flags;
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

procedure TTileMapItemHelper.SetAdditional(AValue: Boolean);
begin
  if AValue then
    Flags += [tmfAdditional]
  else
    Flags -= [tmfAdditional];
end;

procedure TTileMapItemHelper.SetHMirror_Smoothed(AValue: Boolean);
begin
  if AValue then
    Flags += [tmfHMirror_Smoothed]
  else
    Flags -= [tmfHMirror_Smoothed];
end;

procedure TTileMapItemHelper.SetVMirror_Smoothed(AValue: Boolean);
begin
  if AValue then
    Flags += [tmfVMirror_Smoothed]
  else
    Flags -= [tmfVMirror_Smoothed];
end;

procedure TTileMapItemHelper.SetVMirror(AValue: Boolean);
begin
  if AValue then
    Flags += [tmfVMirror]
  else
    Flags -= [tmfVMirror];
end;

procedure TTileMapItemHelper.CopyToSmoothed;
begin
  TileIdx_Smoothed := TileIdx;
  BlendedTileIdx_Smoothed := BlendedTileIdx;
  PalIdx_Smoothed := PalIdx;
  HMirror_Smoothed := HMirror;
  VMirror_Smoothed := VMirror;
end;

{ TFrame }

procedure TFrame.CompressFrameTiles;
var
  CompStream: Tcompressionstream;
begin
  CompressedFrameTiles.Clear;
  CompStream := Tcompressionstream.create(Tcompressionlevel.clfastest, CompressedFrameTiles, True);
  try
    CompStream.WriteBuffer(FrameTiles[0]^, Length(TileMap) * Length(TileMap[0]) * (SizeOf(TTile) + SizeOf(TRGBPixels) + SizeOf(TPalPixels)));
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
    FrameTiles := TTile.Array1DNew(Length(TileMap) * Length(TileMap[0]), True, True);

    CompStream := Tdecompressionstream.create(CompressedFrameTiles, True);
    try
      CompStream.ReadBuffer(FrameTiles[0]^, Length(TileMap) * Length(TileMap[0]) * (SizeOf(TTile) + SizeOf(TRGBPixels) + SizeOf(TPalPixels)));
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
  DitherPsyV := NaN;
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

function TTileHelper.GetIntraFrame: Boolean;
begin
  Result := tfIntraFrame in Flags;
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

procedure TTileHelper.SetIntraFrame(AValue: Boolean);
begin
  if AValue then
    Flags += [tfIntraFrame]
  else
    Flags -= [tfIntraFrame];
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
  Result := CompareDWord(GetPalPixelsPtr^[0, 0], ATile.GetPalPixelsPtr^[0, 0], sqr(cTileWidth) div SizeOf(DWORD));
end;

function TTileHelper.CompareRGBPixelsTo(const ATile: TTile): Integer;
begin
  Result := CompareDWord(GetRGBPixelsPtr^[0, 0], ATile.GetRGBPixelsPtr^[0, 0], sqr(cTileWidth));
end;

procedure TTileHelper.CopyFrom(const ATile: TTile);
begin
  UseCount := ATile.UseCount;
  TmpIndex := ATile.TmpIndex;
  MergeIndex := ATile.MergeIndex;
  FrameSoleIndex := ATile.FrameSoleIndex;
  Active := ATile.Active;
  IntraFrame := ATile.IntraFrame;
  Additional := ATile.Additional;

  if HasPalPixels and ATile.HasPalPixels then
    CopyPalPixels(ATile.GetPalPixelsPtr^);
  if HasRGBPixels and ATile.HasRGBPixels then
    CopyRGBPixels(ATile.GetRGBPixelsPtr^);
end;

{ TKeyFrame }

constructor TKeyFrame.Create(AParent: TTilingEncoder; AIndex, APaletteCount, AStartFrame, AEndFrame: Integer);
begin
  Assert(AIndex <= High(SmallInt), 'More than 32767 KeyFrames!');
  Encoder := AParent;
  Index := AIndex;
  StartFrame := AStartFrame;
  EndFrame := AEndFrame;
  FrameCount := AEndFrame - AStartFrame + 1;
  FramesLeft := -1;

  InitializeCriticalSection(CS);
  SpinLeave(@FLock);

  SetLength(MixingPlans, APaletteCount);
  SetLength(PaletteRGB, APaletteCount);
end;

destructor TKeyFrame.Destroy;
begin
  TTile.Array1DDispose(Tiles);
  DeleteCriticalSection(CS);
  inherited Destroy;
end;

function TKeyFrame.GetFreeThreadCount: Integer;
begin
  Result := Max(1, ProcThreadPool.MaxThreadCount - Encoder.FKeyFramesThreadCount);
  //WriteLn('FreeThreadCount ', Result:2);
end;

function TKeyFrame.GetTileCount(AActiveOnly: Boolean): Integer;
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

procedure TKeyFrame.MakeTilesUnique;
var
  tidx: Int64;
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

    SetLength(sameIdx, Length(Tiles));

    sortList.Count := Length(Tiles);
    pos := 0;
    for tidx := 0 to High(Tiles) do
      if Tiles[tidx]^.Active then
      begin
        sortList[pos] := Tiles[tidx];
        PTile(sortList[pos])^.TmpIndex := tidx;
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

procedure TKeyFrame.PreparePalettes(ADitheringGamma: Integer);
const
  cFeatureCount = cTileDCTSize + cColorCpns;
var
  tidx: Int64;
  sx, sy, ty, tx, frmIdx: Integer;
  rr, gg, bb: Byte;
  l, a, b: TFloat;
  Tile: PTile;

  YakmoDataset: TDoubleDynArray2;
  YakmoClusters: TIntegerDynArray;

  Yakmo: PYakmo;
begin
  SetLength(YakmoDataset, Length(Tiles), cFeatureCount);
  SetLength(YakmoClusters, Length(YakmoDataset));
  SetLength(PaletteCentroids, Encoder.FPaletteCount, cFeatureCount);

  for tidx := 0 to High(Tiles) do
  begin
    Tile := Tiles[tidx];
    Encoder.ComputeTilePsyVisFeatures(Tile^, False, True, True, False, False, True, ADitheringGamma, nil, @YakmoDataset[tidx, cColorCpns]);

    for ty := 0 to cTileWidth - 1 do
      for tx := 0 to cTileWidth - 1 do
      begin
        FromRGB(Tile^.RGBPixels[ty, tx], rr, gg, bb);

        Encoder.RGBToLAB(rr, gg, bb, ADitheringGamma, l, a, b);

        YakmoDataset[tidx, 0] += l;
        YakmoDataset[tidx, 1] += a;
        YakmoDataset[tidx, 2] += b;
      end;
  end;

  WriteLn('KF: ', StartFrame:8, ' Palettization start');

  if (Length(YakmoDataset) >= Encoder.FPaletteCount) and (Encoder.FPaletteCount > 1) then
  begin
    Yakmo := yakmo_create(Encoder.FPaletteCount, 1, cYakmoMaxIterations, 1, 0, 0, 0);
    yakmo_load_train_data(Yakmo, Length(YakmoDataset), cFeatureCount, PPDouble(@YakmoDataset[0]));
    SetLength(YakmoDataset, 0); // free up some memmory
    yakmo_train_on_data(Yakmo, @YakmoClusters[0]);
    yakmo_get_centroids(Yakmo, PPDouble(@PaletteCentroids[0]));
    yakmo_destroy(Yakmo);
  end
  else
  begin
    SetLength(YakmoClusters, Length(Tiles));
  end;

  for tidx := 0 to High(Tiles) do
    for frmIdx := StartFrame to EndFrame do
      for sy := 0 to Encoder.FTileMapHeight - 1 do
        for sx := 0 to Encoder.FTileMapWidth - 1 do
          if Encoder.FFrames[frmIdx].TileMap[sy, sx].TileIdx = tidx then
            Encoder.FFrames[frmIdx].TileMap[sy, sx].PalIdx := YakmoClusters[tidx];

  WriteLn('KF: ', StartFrame:8, ' Palettization end');
end;

procedure TKeyFrame.QuantizePalette(APalIdx: Integer; UseYakmo: Boolean; DLv3BPC: Integer; ADitheringGamma: Integer);
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
    dlCnt := FrameCount * Encoder.FScreenWidth * Encoder.FScreenHeight;
    dlInput := GetMem(dlCnt * 3);
    try
      FillChar(dlInput^, dlCnt * 3, 0);
      FillChar(dlPal[0, 0], SizeOf(dlPal), $ff);

      // find width and height of a rectangular area to arrange tiles

      tileCnt := 0;
      for i := StartFrame to EndFrame do
        for sy := 0 to Encoder.FTileMapHeight - 1 do
          for sx := 0 to Encoder.FTileMapWidth - 1 do
          begin
            Tile := Tiles[Encoder.FFrames[i].TileMap[sy, sx].TileIdx];
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
      for i := StartFrame to EndFrame do
      begin
        for sy := 0 to Encoder.FTileMapHeight - 1 do
          for sx := 0 to Encoder.FTileMapWidth - 1 do
          begin
            Tile := Tiles[Encoder.FFrames[i].TileMap[sy, sx].TileIdx];

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

              Inc(PaletteUseCount[APalIdx].UseCount);
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
      CMItem^.Luma := ToLuma(CMItem^.R, CMItem^.G, CMItem^.B);
      Encoder.RGBToHSV(ToRGB(CMItem^.R, CMItem^.G, CMItem^.B), CMItem^.Hue, CMItem^.Sat, CMItem^.Val);
      CMPal.Add(CMItem);
    end;
  end;


  procedure DoYakmo(AColorCount, APosterizeBpc: Integer);
  const
    cFeatureCount = 3;
  var
    tidx: Int64;
    i, di, ty, tx, sy, sx, frmIdx, cnt: Integer;
    rr, gg, bb: Byte;
    Tile: PTile;
    Dataset, Centroids: TDoubleDynArray2;
    Clusters: TIntegerDynArray;
    Yakmo: PYakmo;
    CMItem: PCountIndex;
    TilesInd: TBooleanDynArray;
  begin
    // build an indicator of this palette tiles

    SetLength(TilesInd, Length(Tiles));

    for frmIdx := StartFrame to EndFrame do
      for sy := 0 to Encoder.FTileMapHeight - 1 do
        for sx := 0 to Encoder.FTileMapWidth - 1 do
          if Encoder.FFrames[frmIdx].TileMap[sy, sx].PalIdx = APalIdx then
            TilesInd[Encoder.FFrames[frmIdx].TileMap[sy, sx].TileIdx] := True;

    cnt := 0;
    for i := 0 to High(TilesInd) do
      if TilesInd[i] then
        Inc(cnt);

    SetLength(Dataset, cnt * Sqr(cTileWidth), cFeatureCount);
    SetLength(Clusters, Length(Dataset));
    SetLength(Centroids, AColorCount, cFeatureCount);

    // build a dataset of RGB pixels

    di := 0;
    for tidx := 0 to High(Tiles) do
      if TilesInd[tidx] then
      begin
        Tile := Tiles[tidx];
        for ty := 0 to cTileWidth - 1 do
          for tx := 0 to cTileWidth - 1 do
          begin
            FromRGB(Tile^.RGBPixels[ty, tx], rr, gg, bb);
            Dataset[di, 0] := rr;
            Dataset[di, 1] := gg;
            Dataset[di, 2] := bb;
            Inc(di);
          end;
        Inc(PaletteUseCount[APalIdx].UseCount, Tile^.UseCount);
      end;
    Assert(di = Length(Dataset));

    SetLength(TilesInd, 0);

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
      CMItem^.Luma := ToLuma(CMItem^.R, CMItem^.G, CMItem^.B);
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
      CMItem^.Luma := ToLuma(CMItem^.R, CMItem^.G, CMItem^.B);
      col := ToRGB(CMItem^.R, CMItem^.G, CMItem^.B);
      Encoder.RGBToHSV(col, CMItem^.Hue, CMItem^.Sat, CMItem^.Val);

      CMPal.Add(CMItem);
    end;
  end;

var
  i: Integer;
begin
  PaletteUseCount[APalIdx].UseCount := 0;

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

  if InterLockedDecrement(PalettesLeft) <= 0 then
    WriteLn('KF: ', StartFrame:8, ' Quantization end');
end;

procedure TKeyFrame.FinishQuantizePalettes;
var
  i, sy, sx, PalIdx: Integer;
  PalIdxLUT: TIntegerDynArray;
  TmpCentroids: TDoubleDynArray2;
begin
  SetLength(PalIdxLUT, Encoder.FPaletteCount);

  // sort entire palettes by use count
  for PalIdx := 0 to Encoder.FPaletteCount - 1 do
  begin
    PaletteUseCount[PalIdx].Palette := PaletteRGB[PalIdx];
    PaletteUseCount[PalIdx].PalIdx := PalIdx;
  end;
  QuickSort(PaletteUseCount[0], 0, Encoder.FPaletteCount - 1, SizeOf(PaletteUseCount[0]), @ComparePaletteUseCount, Self);
  for PalIdx := 0 to Encoder.FPaletteCount - 1 do
  begin
    PaletteRGB[PalIdx] := PaletteUseCount[PalIdx].Palette;
    PalIdxLUT[PaletteUseCount[PalIdx].PalIdx] := PalIdx;
  end;

  for i := StartFrame to EndFrame do
    for sy := 0 to Encoder.FTileMapHeight - 1 do
      for sx := 0 to Encoder.FTileMapWidth - 1 do
        Encoder.FFrames[i].TileMap[sy, sx].PalIdx := PalIdxLUT[Encoder.FFrames[i].TileMap[sy, sx].PalIdx];
  TmpCentroids := Copy(PaletteCentroids);
  for PalIdx := 0 to Encoder.FPaletteCount - 1 do
    PaletteCentroids[PalIdxLUT[PalIdx]] := TmpCentroids[PalIdx];
end;

procedure TKeyFrame.DitherTiles;

  procedure DoFrame(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    PalIdx, TMPos: Integer;
    sx, sy: Integer;
    T: PTile;
    TMI: PTileMapItem;
  begin
    if not InRange(AIndex, StartFrame, EndFrame) then
      Exit;

    InterLockedIncrement(Encoder.FKeyFramesThreadCount);
    try
      Encoder.FFrames[AIndex].AcquireFrameTiles;

      for sy := 0 to Encoder.FTileMapHeight - 1 do
        for sx := 0 to Encoder.FTileMapWidth - 1 do
        begin
          TMPos := sy * Encoder.FTileMapWidth + sx;
          TMI := @Encoder.FFrames[AIndex].TileMap[sy, sx];
          T := Tiles[TMI^.TileIdx];
          PalIdx := TMI^.PalIdx;

          if T^.Active and (InterlockedCompareExchange(T^.TmpIndex, Int64(AIndex) * Encoder.FTileMapSize + TMPos, -1) = -1) then
          begin
            Assert(InRange(PalIdx, 0, Encoder.FPaletteCount - 1));

            // put tile back in its natural mirrors for ordered dithering to work properly
            if TMI^.HMirror then Encoder.HMirrorTile(T^);
            if TMI^.VMirror then Encoder.VMirrorTile(T^);
            try
              // dither tile
              Encoder.DitherTile(T^, MixingPlans[PalIdx]);
            finally
              if TMI^.HMirror then Encoder.HMirrorTile(T^);
              if TMI^.VMirror then Encoder.VMirrorTile(T^);
            end;
          end;

          if Encoder.FrameTilingFromPalette then
          begin
            // also dither FrameTiles
            Encoder.DitherTile(Encoder.FFrames[AIndex].FrameTiles[TMPos]^, MixingPlans[PalIdx]);
          end;
        end;

    finally
      Encoder.FFrames[AIndex].CompressFrameTiles;
      Encoder.FFrames[AIndex].ReleaseFrameTiles;
      InterLockedDecrement(Encoder.FKeyFramesThreadCount);
    end;
  end;

var
  i: Integer;
begin
  // build ditherer
  for i := 0 to Encoder.FPaletteCount - 1 do
    Encoder.PreparePlan(MixingPlans[i], Encoder.FDitheringYliluoma2MixedColors, PaletteRGB[i]);

  // dither tile in chosen palette

  InterLockedDecrement(Encoder.FKeyFramesThreadCount);
  try
    ProcThreadPool.DoParallelLocalProc(@DoFrame, StartFrame, EndFrame, nil, GetFreeThreadCount);
  finally
    InterLockedIncrement(Encoder.FKeyFramesThreadCount);
  end;

  // cleanup ditherer
  for i := 0 to Encoder.FPaletteCount - 1 do
    Encoder.TerminatePlan(MixingPlans[i]);

  WriteLn('KF: ', StartFrame:8, ' Dithering end');
end;

procedure TKeyFrame.DoKeyFrameKMeans(AClusterCount: Integer);
var
  i, lineIdx, clusterIdx, clusterLineCount, DSLen, BICOClusterCount, BICOCoresetSize, clusterLineIdx: Integer;
  cnt, tidx: Int64;
  speedup: Double;
  v, best: TFloat;

  BICO: PBICO;
  FLANN: flann_index_t;
  FLANNParams: TFLANNParameters;
  Tile: PTile;

  TileIndices: TInt64DynArray;
  Dataset: TDoubleDynArray;
  BICOCentroids, BICOWeights: TDoubleDynArray;
  FLANNClusters: TIntegerDynArray;
  FLANNErrors: TDoubleDynArray;
  ToMerge: array of PDouble;
  ToMergeIdxs: TInt64DynArray;
begin
  DSLen := GetTileCount(True);
  if (DSLen <= AClusterCount) or (AClusterCount <= 1) then
    Exit;

  // use BICO to prepare a noise-aware set of centroids

  BICOCoresetSize := Ceil(AClusterCount * cGlobalTilingCoresetMultiplier);
  BICO := bico_create(cTileDCTSize, DSLen, AClusterCount, 7, BICOCoresetSize, $42381337);
  try
    // parse bin dataset

    SetLength(TileIndices, DSLen);
    SetLength(Dataset, DSLen * cTileDCTSize);
    cnt := 0;
    for tidx := 0 to High(Tiles) do
    begin
      Tile := Tiles[tidx];

      if Tile^.Active then
      begin
        Encoder.ComputeTilePsyVisFeatures(Tile^, False, False, False, False, False, False, -1, nil, @Dataset[cnt * cTileDCTSize]);

        // insert line into BICO
        bico_insert_line(BICO, @Dataset[cnt * cTileDCTSize], Tile^.UseCount);

        TileIndices[cnt] := tidx;
        Inc(cnt);
      end;
    end;
    Assert(cnt = DSLen);

    // get BICO results

    SetLength(BICOCentroids, BICOCoresetSize * cTileDCTSize);
    SetLength(BICOWeights, BICOCoresetSize);

    BICOClusterCount := bico_get_results(BICO, @BICOCentroids[0], @BICOWeights[0]);

  finally
    bico_destroy(BICO);
  end;

  WriteLn('KF: ', StartFrame:8, ' DatasetSize: ', DSLen:8, ' BICOClusterCount: ', BICOClusterCount:6, ' ClusterCount: ', AClusterCount:6);

  if BICOClusterCount <= 0 then
    Exit;

  // use FLANN to compute cluster indexes

  SetLength(FLANNClusters, DSLen);
  SetLength(FLANNErrors, DSLen);

  FLANNParams := CDefaultFLANNParameters;
  FLANNParams.checks := cTileDCTSize;
  FLANNParams.algorithm := FLANN_INDEX_KDTREE_SINGLE;
  FLANNParams.sorted := 0;
  FLANNParams.max_neighbors := 1;

  FLANN := flann_build_index_double(@BICOCentroids[0], BICOClusterCount, cTileDCTSize, @speedup, @FLANNParams);
  try
    flann_find_nearest_neighbors_index_double(FLANN, @Dataset[0], DSLen, @FLANNClusters[0], @FLANNErrors[0], 1, @FLANNParams);
  finally
    flann_free_index_double(FLANN, @FLANNParams);
  end;

  // build a list of this centroid tiles

  SetLength(ToMerge, DSLen);
  SetLength(ToMergeIdxs, DSLen);

  for clusterIdx := 0 to BICOClusterCount - 1 do
  begin
    clusterLineCount := 0;
    for lineIdx := 0 to DSLen - 1 do
      if FLANNClusters[lineIdx] = clusterIdx then
      begin
        ToMerge[clusterLineCount] := @Dataset[lineIdx * cTileDCTSize];
        ToMergeIdxs[clusterLineCount] := TileIndices[lineIdx];
        Inc(clusterLineCount);
      end;

    // choose a tile from the cluster

    if clusterLineCount >= 2 then
    begin
      // get the closest tile to the centroid

      best := MaxSingle;
      clusterLineIdx := -1;
      for i := 0 to clusterLineCount - 1 do
      begin
        v := CompareEuclidean(ToMerge[i], @BICOCentroids[clusterIdx * cTileDCTSize], cTileDCTSize);
        if v < best then
        begin
          best := v;
          clusterLineIdx := i;
        end;
      end;

      // merge centroid

      SpinEnter(@FLock);
      MergeTiles(ToMergeIdxs, clusterLineCount, ToMergeIdxs[clusterLineIdx], nil, nil);
      SpinLeave(@FLock);
    end;
  end;

  WriteLn('KF: ', StartFrame:8, ' Clustering end');
end;

procedure TKeyFrame.PrepareKFTiling(APaletteIndex, AFTGamma: Integer);
var
  DS: PTilingDataset;

  procedure DoPsyV;
  var
    T: PTile;
    tidx: Int64;
    pDCT: PFloat;
    pDS: PInteger;
  begin
    pDCT := PFloat(@DS^.Dataset[0]);
    pDS := PInteger(@DS^.DSToTileIdx[0]);
    for tidx := 0 to High(Tiles) do
    begin
      T := Tiles[tidx];

      if T^.Active then
      begin
        Encoder.ComputeTilePsyVisFeatures(T^, True, False, cFTQWeighting, False, False, False, AFTGamma, PaletteRGB[APaletteIndex], pDCT);
        Inc(pDCT, cTileDCTSize);
        pDS^ := tidx;
        Inc(pDS);
      end;
    end;
  end;

var
  KNNSize: Integer;
  speedup: Single;
begin
  // Compute psycho visual model for all tiles (in curent palette)

  DS := New(PTilingDataset);
  FillChar(DS^, SizeOf(TTilingDataset), 0);

  KNNSize := GetTileCount(True);
  SetLength(DS^.Dataset, KNNSize * cTileDCTSize);
  SetLength(DS^.DSToTileIdx, KNNSize);

  DoPsyV;

  // Build KNN

  New(DS^.FLANNParams);
  DS^.FLANNParams^ := CDefaultFLANNParameters;
  DS^.FLANNParams^.checks := cTileDCTSize;

  DS^.FLANN := flann_build_index(@DS^.Dataset[0], KNNSize, cTileDCTSize, @speedup, DS^.FLANNParams);

  // Dataset is ready

  TileDS[APaletteIndex] := DS;
end;

procedure TKeyFrame.FinishKFTiling(APaletteIndex: Integer);
var
  tileResd: TFloat;
begin
  if Length(TileDS[APaletteIndex]^.Dataset) > 0 then
    flann_free_index(TileDS[APaletteIndex]^.FLANN, TileDS[APaletteIndex]^.FLANNParams);
  TileDS[APaletteIndex]^.FLANN := nil;
  SetLength(TileDS[APaletteIndex]^.Dataset, 0);
  SetLength(TileDS[APaletteIndex]^.DSToTileIdx, 0);
  Dispose(TileDS[APaletteIndex]);

  TileDS[APaletteIndex] := nil;
  InterLockedDecrement(PalettesLeft);

  if PalettesLeft <= 0 then
  begin
    tileResd := Sqrt(ReconstructErrCml / (Encoder.FTileMapSize * FrameCount));
    WriteLn('KF: ', StartFrame:8, ' ResidualErr: ', (tileResd * Encoder.FTileMapSize):12:3, ' (by frame) ', tileResd:12:6, ' (by tile)');
  end;
end;

procedure TKeyFrame.DoKFTiling(APaletteIndex: Integer; AFTGamma: Integer; AFTBlendThres: TFloat; AFromPal: Boolean);
const
  CBinSize = 64;
var
  sx, sy, frmIdx, annQueryCount, annQueryPos, dsIdx, blendDsIdx, diffIdx, binIdx: Integer;
  errCml: Double;
  blendThres, diffErr, blendErr: TFloat;

  T: PTile;
  Frame: TFrame;
  TMI: PTileMapItem;

  DS: PTilingDataset;
  idxs: TIntegerDynArray;
  errs: TFloatDynArray;
  DCTs: TFloatDynArray;
  DiffDCT: array[0 .. cTileDCTSize - 1] of TFloat;
  blendIdxs: array[0 .. CBinSize - 1] of Integer;
  blendErrs: array[0 .. CBinSize - 1] of TFloat;

  Best: TKFTilingBest;
begin
  DS := TileDS[APaletteIndex];
  if Length(DS^.DSToTileIdx) < 1 then
    Exit;

  errCml := 0.0;

  // compute KNN query count

  annQueryCount := 0;
  for frmIdx := StartFrame to EndFrame do
  begin
    Frame := Encoder.FFrames[frmIdx];

    for sy := 0 to Encoder.FTileMapHeight - 1 do
      for sx := 0 to Encoder.FTileMapWidth - 1 do
      begin
        if Frame.TileMap[sy, sx].PalIdx <> APaletteIndex then
          Continue;

        Inc(annQueryCount);
      end;
  end;

  if annQueryCount <= 0 then
    Exit;

  SetLength(DCTs, annQueryCount * cTileDCTSize);
  SetLength(idxs, annQueryCount);
  SetLength(errs, annQueryCount);

  // prepare KNN queries

  annQueryPos := 0;
  for frmIdx := StartFrame to EndFrame do
  begin
    Frame := Encoder.FFrames[frmIdx];

    Frame.AcquireFrameTiles;
    try
      for sy := 0 to Encoder.FTileMapHeight - 1 do
        for sx := 0 to Encoder.FTileMapWidth - 1 do
        begin
          TMI := @Frame.TileMap[sy, sx];

          if TMI^.PalIdx <> APaletteIndex then
            Continue;

          T := Frame.FrameTiles[sy * Encoder.FTileMapWidth + sx];

          Encoder.ComputeTilePsyVisFeatures(T^, AFromPal, False, cFTQWeighting, TMI^.HMirror, TMI^.VMirror, False, AFTGamma, Frame.PKeyFrame.PaletteRGB[APaletteIndex], @DCTs[annQueryPos * cTileDCTSize]);

          Inc(annQueryPos);
        end;
    finally
      Frame.ReleaseFrameTiles;
    end;
  end;

  // query KNN

  flann_find_nearest_neighbors_index(DS^.FLANN, @DCTs[0], annQueryCount, @idxs[0], @errs[0], 1, DS^.FLANNParams);

  // map keyframe tilemap items to reduced tiles and mirrors, parsing KNN query

  annQueryPos := 0;
  for frmIdx := StartFrame to EndFrame do
  begin
    Frame := Encoder.FFrames[frmIdx];

    for sy := 0 to Encoder.FTileMapHeight - 1 do
      for sx := 0 to Encoder.FTileMapWidth - 1 do
      begin
        if Frame.TileMap[sy, sx].PalIdx <> APaletteIndex then
          Continue;

        dsIdx := idxs[annQueryPos];

        if InRange(dsIdx, 0, High(DS^.DSToTileIdx)) then
        begin
          Best.TileIdx := DS^.DSToTileIdx[dsIdx];
          Best.ResidualErr := errs[annQueryPos];
          Best.BlendedTileIdx := -1;

          // try to blend another tile to improve likeliness

          blendThres := AFTBlendThres;
          if IsZero(blendThres) then
            blendThres := Frame.TileMap[sy, sx].DitherPsyV;

          if not IsZero(sqrt(Best.ResidualErr), blendThres) then
          begin
            // search CBinSize likely tiles in the KNN
            flann_find_nearest_neighbors_index(DS^.FLANN, @DCTs[annQueryPos * cTileDCTSize], 1, @blendIdxs[0], @blendErrs[0], CBinSize, DS^.FLANNParams);

            for binIdx := 0 to CBinSize - 1 do
            begin
              blendDsIdx := blendIdxs[binIdx];

              if InRange(blendDsIdx, 0, High(DS^.DSToTileIdx)) then
              begin
                // compute an additional tile that would be blended with the current tile (lerp 0.5)
                ComputeBlending_Asm(@DiffDCT[0], @DCTs[annQueryPos * cTileDCTSize], @DS^.Dataset[blendDsIdx * cTileDCTSize], 2.0, -1.0);

                // search for it in the KNN
                flann_find_nearest_neighbors_index(DS^.FLANN, @DiffDCT[0], 1, @diffIdx, @diffErr, 1, DS^.FLANNParams);

                if InRange(diffIdx, 0, High(DS^.DSToTileIdx)) then
                begin
                  // compute error from the final interpolation (lerp 0.5)
                  blendErr := ComputeBlendingError_Asm(@DCTs[annQueryPos * cTileDCTSize], @DS^.Dataset[diffIdx * cTileDCTSize], @DS^.Dataset[blendDsIdx * cTileDCTSize], 0.5, 0.5);

                  if blendErr < Best.ResidualErr then
                  begin
                    Best.TileIdx := DS^.DSToTileIdx[blendDsIdx];
                    Best.ResidualErr := blendErr;
                    Best.BlendedTileIdx := DS^.DSToTileIdx[diffIdx];
                  end;
                end;
              end;
            end;
          end;

          TMI := @Frame.TileMap[sy, sx];

          TMI^.TileIdx := Best.TileIdx;
          TMI^.BlendedTileIdx := Best.BlendedTileIdx;

          TMI^.CopyToSmoothed;

          errCml += Best.ResidualErr;
        end;

        Inc(annQueryPos);
      end;
  end;
  Assert(annQueryPos = annQueryCount);

  SpinEnter(@FLock);
  ReconstructErrCml += errCml;
  SpinLeave(@FLock);
end;

procedure TKeyFrame.ReindexTiles(KeepRGBPixels: Boolean);
var
  IdxMap: TInt64DynArray;
  Frame: TFrame;

  procedure HandleIntraFrame(ATidx: Int64);
  begin
    if ATidx >= 0 then
    begin
      if Tiles[ATidx]^.FrameSoleIndex = MaxSmallint then
        Tiles[ATidx]^.FrameSoleIndex := Frame.Index
      else if Tiles[ATidx]^.FrameSoleIndex <> Frame.Index then
        Tiles[ATidx]^.FrameSoleIndex := -1;
    end;
  end;

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
    Tiles[tidx]^.FrameSoleIndex := MaxSmallint;
    Tiles[tidx]^.TmpIndex := tidx;
    if (Tiles[tidx]^.Active) and (Tiles[tidx]^.UseCount > 0) then
      Inc(cnt);
  end;

  if cnt <= 0 then
    Exit;

  // prepare tiles "intra frame" flag

  for frmIdx := StartFrame to EndFrame do
  begin
    Frame := Encoder.FFrames[frmIdx];
    for sy := 0 to (Encoder.FTileMapHeight - 1) do
      for sx := 0 to (Encoder.FTileMapWidth - 1) do
      begin
        TMI := @Frame.TileMap[sy, sx];

        HandleIntraFrame(TMI^.TileIdx_Smoothed);
        HandleIntraFrame(TMI^.BlendedTileIdx_Smoothed);
      end;
  end;

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

  TTile.Array1DDispose(Tiles);
  Tiles := LocTiles;
  LocTiles := nil;

  // sort tiles

  QuickSort(Tiles[0], 0, High(Tiles), SizeOf(PTile), @CompareTileUseCountRev);

  for tidx := 0 to High(Tiles) do
    IdxMap[Tiles[tidx]^.TmpIndex] := tidx;

  // point tilemap items on new tiles indexes

  for frmIdx := StartFrame to EndFrame do
  begin
    Frame := Encoder.FFrames[frmIdx];
    for sy := 0 to (Encoder.FTileMapHeight - 1) do
      for sx := 0 to (Encoder.FTileMapWidth - 1) do
      begin
        TMI := @Frame.TileMap[sy, sx];

        Remap(TMI^.TileIdx);
        Remap(TMI^.BlendedTileIdx);
        Remap(TMI^.TileIdx_Smoothed);
        Remap(TMI^.BlendedTileIdx_Smoothed);
      end;
  end;

  // mark tiles "intra frame" when they are used by only one frame

  cnt := 0;
  for tidx := 0 to High(Tiles) do
  begin
    Tiles[tidx]^.IntraFrame := Tiles[tidx]^.FrameSoleIndex >= 0;
    if Tiles[tidx]^.IntraFrame then
      Inc(cnt);
  end;

  WriteLn('KF: ', StartFrame:8, ' ReindexTiles: ', cnt, ' / ', Length(Tiles), ' made intra');
end;

procedure TKeyFrame.DoTemporalSmoothing(AFrame, APrevFrame: TFrame; Strength: TFloat; AddlTilesThres: TFloat;
 NonAddlCount: Int64; ATList: TList);
var
  sx, sy: Integer;
  cmp, cmlUseCount, prevCmlUseCount: TFloat;
  TMI, PrevTMI: PTileMapItem;
  DCT, CurDCT, PrevDCT, BlendDCT, PrevPlainDCT: array[0 .. cTileDCTSize - 1] of TFloat;
  addlTile: PTile;
  plan: TMixingPlan;
  AdditionalTiles: TList;
begin
  Assert(not Assigned(AFrame) or (AFrame.PKeyFrame = Self));
  Assert(Assigned(APrevFrame));
  Assert(APrevFrame.PKeyFrame = Self);

  APrevFrame.AcquireFrameTiles;
  try
    for sy := 0 to Encoder.FTileMapHeight - 1 do
      for sx := 0 to Encoder.FTileMapWidth - 1 do
      begin
        PrevTMI := @APrevFrame.TileMap[sy, sx];
        Encoder.ComputeTilePsyVisFeatures(APrevFrame.PKeyFrame.Tiles[PrevTMI^.TileIdx_Smoothed]^, True, False, True, PrevTMI^.HMirror_Smoothed, PrevTMI^.VMirror_Smoothed, False, -1, APrevFrame.PKeyFrame.PaletteRGB[PrevTMI^.PalIdx_Smoothed], PFloat(@PrevDCT[0]));
        if PrevTMI^.BlendedTileIdx_Smoothed >= 0 then
        begin
          Move(PrevDCT[0], DCT[0], SizeOf(DCT));
          Encoder.ComputeTilePsyVisFeatures(APrevFrame.PKeyFrame.Tiles[PrevTMI^.BlendedTileIdx_Smoothed]^, True, False, True, PrevTMI^.HMirror_Smoothed, PrevTMI^.VMirror_Smoothed, False, -1, APrevFrame.PKeyFrame.PaletteRGB[PrevTMI^.PalIdx_Smoothed], PFloat(@BlendDCT[0]));
          ComputeBlending(PFloat(@PrevDCT[0]), PFloat(@DCT[0]), PFloat(@BlendDCT[0]), 0.5, 0.5);
        end;

        // prevent smoothing from crossing keyframes (to ensure proper seek)

        if Assigned(AFrame) and (AFrame.PKeyFrame = APrevFrame.PKeyFrame) then
        begin
          // compare DCT of current tile with tile from prev frame tilemap

          TMI := @AFrame.TileMap[sy, sx];
          Encoder.ComputeTilePsyVisFeatures(AFrame.PKeyFrame.Tiles[TMI^.TileIdx_Smoothed]^, True, False, True, TMI^.HMirror_Smoothed, TMI^.VMirror_Smoothed, False, -1, AFrame.PKeyFrame.PaletteRGB[TMI^.PalIdx_Smoothed], PFloat(@CurDCT[0]));
          if TMI^.BlendedTileIdx_Smoothed >= 0 then
          begin
            Move(CurDCT[0], DCT[0], SizeOf(DCT));
            Encoder.ComputeTilePsyVisFeatures(AFrame.PKeyFrame.Tiles[TMI^.BlendedTileIdx_Smoothed]^, True, False, True, TMI^.HMirror_Smoothed, TMI^.VMirror_Smoothed, False, -1, AFrame.PKeyFrame.PaletteRGB[TMI^.PalIdx_Smoothed], PFloat(@BlendDCT[0]));
            ComputeBlending(PFloat(@CurDCT[0]), PFloat(@DCT[0]), PFloat(@BlendDCT[0]), 0.5, 0.5);
          end;

          cmp := CompareEuclideanDCT(CurDCT, PrevDCT);
          cmp := sqrt(cmp);

          // if difference is low enough, mark the tile as smoothed for tilemap compression use

          if (cmp <= Strength) then
          begin
            Assert(InRange(TMI^.TileIdx_Smoothed, 0, High(Tiles)));
            cmlUseCount := Tiles[TMI^.TileIdx_Smoothed]^.UseCount;
            if TMI^.BlendedTileIdx_Smoothed >= 0 then
              cmlUseCount := 0.5 * (cmlUseCount + Tiles[TMI^.BlendedTileIdx_Smoothed]^.UseCount);

            Assert(InRange(PrevTMI^.TileIdx_Smoothed, 0, High(Tiles)));
            prevCmlUseCount := Tiles[PrevTMI^.TileIdx_Smoothed]^.UseCount;
            if PrevTMI^.BlendedTileIdx_Smoothed >= 0 then
              prevCmlUseCount := 0.5 * (prevCmlUseCount + Tiles[PrevTMI^.BlendedTileIdx_Smoothed]^.UseCount);

            if cmlUseCount >= prevCmlUseCount then // a higher usecount means more compressibility
            begin
              TMI^.TileIdx_Smoothed := PrevTMI^.TileIdx_Smoothed;
              TMI^.BlendedTileIdx_Smoothed := PrevTMI^.BlendedTileIdx_Smoothed;
              TMI^.PalIdx_Smoothed := PrevTMI^.PalIdx_Smoothed;
              TMI^.HMirror_Smoothed := PrevTMI^.HMirror_Smoothed;
              TMI^.VMirror_Smoothed := PrevTMI^.VMirror_Smoothed;
            end
            else
            begin
              PrevTMI^.TileIdx_Smoothed := TMI^.TileIdx_Smoothed;
              PrevTMI^.BlendedTileIdx_Smoothed := TMI^.BlendedTileIdx_Smoothed;
              PrevTMI^.PalIdx_Smoothed := TMI^.PalIdx_Smoothed;
              PrevTMI^.HMirror_Smoothed := TMI^.HMirror_Smoothed;
              PrevTMI^.VMirror_Smoothed := TMI^.VMirror_Smoothed;
            end;

            TMI^.Smoothed := True;
          end
          else
          begin
            TMI^.Smoothed := False;
          end;
        end;

        // compare the DCT of previous tile with the plaintext tile, if the difference is too high, add it to "additional tiles"

        Encoder.ComputeTilePsyVisFeatures(APrevFrame.FrameTiles[sy * Encoder.FTileMapWidth + sx]^, False, False, True, False, False, False, -1, nil, PFloat(@PrevPlainDCT[0]));

        cmp := CompareEuclideanDCT(PrevDCT, PrevPlainDCT);
        cmp := sqrt(cmp);

        if (AddlTilesThres <> 0.0) and (cmp > AddlTilesThres) then
        begin
          addlTile := TTile.New(True, True);
          addlTile^.CopyFrom(APrevFrame.FrameTiles[sy * Encoder.FTileMapWidth + sx]^);
          addlTile^.UseCount := 1;
          addlTile^.Active := True;
          addlTile^.Additional := True;
          addlTile^.IntraFrame := True;
          addlTile^.FrameSoleIndex := APrevFrame.Index;
          AdditionalTiles.Add(addlTile);

          // redither tile (frame tiles don't have the paletted version)
          Encoder.PreparePlan(plan, Encoder.FDitheringYliluoma2MixedColors, APrevFrame.PKeyFrame.PaletteRGB[PrevTMI^.PalIdx_Smoothed]);
          Encoder.DitherTile(addlTile^, plan);
          Encoder.TerminatePlan(plan);

          PrevTMI^.Smoothed := True;
          PrevTMI^.Additional := True;
          PrevTMI^.TileIdx_Smoothed := NonAddlCount + AdditionalTiles.Count - 1;
          PrevTMI^.BlendedTileIdx_Smoothed := -1;
          PrevTMI^.HMirror_Smoothed := False;
          PrevTMI^.VMirror_Smoothed := False;
        end;
      end;
  finally
    APrevFrame.ReleaseFrameTiles;
  end;
end;

procedure TKeyFrame.MergeTiles(const TileIndexes: array of Int64; TileCount: Integer; BestIdx: Int64;
 NewTile: PPalPixels; NewTileRGB: PRGBPixels);
var
  i: Integer;
  tidx: Int64;
begin
  if TileCount <= 0 then
    Exit;

  if Assigned(NewTile) then
    Tiles[BestIdx]^.CopyPalPixels(NewTile^);

  if Assigned(NewTileRGB) then
    Tiles[BestIdx]^.CopyRGBPixels(NewTileRGB^);

  for i := 0 to TileCount - 1 do
  begin
    tidx := TileIndexes[i];

    if tidx = BestIdx then
      Continue;

    Inc(Tiles[BestIdx]^.UseCount, Tiles[tidx]^.UseCount);

    Tiles[tidx]^.Active := False;
    Tiles[tidx]^.UseCount := 0;
    Tiles[tidx]^.MergeIndex := BestIdx;

    Tiles[tidx]^.ClearPixels;
  end;
end;

procedure TKeyFrame.InitMergeTiles;
var
  tidx: Int64;
begin
  for tidx := 0 to High(Tiles) do
    Tiles[tidx]^.MergeIndex := -1;
end;

procedure TKeyFrame.FinishMergeTiles;
var
  sx, sy, frmIdx: Integer;
  tidx: Int64;
  TMI: PTileMapItem;
begin
  for frmIdx := StartFrame to EndFrame do
    for sy := 0 to (Encoder.FTileMapHeight - 1) do
      for sx := 0 to (Encoder.FTileMapWidth - 1) do
      begin
        TMI := @Encoder.FFrames[frmIdx].TileMap[sy, sx];

        tidx := Tiles[TMI^.TileIdx]^.MergeIndex;
        if tidx >= 0 then
        begin
          TMI^.TileIdx := tidx;
          TMI^.TileIdx_Smoothed := tidx;
        end;
      end;
end;

procedure TKeyFrame.LoadTiles;

  procedure DoFrame(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    i, sx, sy: Integer;
    HMirror, VMirror: Boolean;
    tilePos: Int64;
    Frame: TFrame;
    TMI: PTileMapItem;
  begin
    if not InRange(AIndex, StartFrame, EndFrame) then
      Exit;

    InterLockedIncrement(Encoder.FKeyFramesThreadCount);
    try
      Frame := Encoder.FFrames[AIndex];

      tilePos := (Frame.Index - StartFrame) * Encoder.FTileMapSize;

      Frame.AcquireFrameTiles;
      try
        for i := 0 to Encoder.FTileMapSize - 1 do
        begin
          Encoder.GetTileHVMirrorHeuristics(Frame.FrameTiles[i]^, HMirror, VMirror);

          Tiles[tilePos + i]^.CopyFrom(Frame.FrameTiles[i]^);
          Tiles[tilePos + i]^.UseCount := 1;

          if HMirror then
            Encoder.HMirrorTile(Tiles[tilePos + i]^);

          if VMirror then
            Encoder.VMirrorTile(Tiles[tilePos + i]^);

          // update tilemap

          DivMod(i, Encoder.FTileMapWidth, sy, sx);

          TMI := @Frame.TileMap[sy, sx];

          TMI^.TileIdx := tilePos + i;
          TMI^.HMirror := HMirror;
          TMI^.VMirror := VMirror;
          TMI^.HMirror_Smoothed := HMirror;
          TMI^.VMirror_Smoothed := VMirror;
        end;

      finally
        Frame.ReleaseFrameTiles;
      end;
    finally
      InterLockedDecrement(Encoder.FKeyFramesThreadCount);
    end;
  end;

begin
  if FrameCount = 0 then
    Exit;

  // free memory from a prev run
  TTile.Array1DDispose(Tiles);

  // allocate tiles
  Tiles := TTile.Array1DNew(FrameCount * Encoder.FTileMapSize, True, True);

  InterLockedDecrement(Encoder.FKeyFramesThreadCount);
  try
    ProcThreadPool.DoParallelLocalProc(@DoFrame, StartFrame, EndFrame, nil, GetFreeThreadCount);
  finally
    InterLockedIncrement(Encoder.FKeyFramesThreadCount);
  end;

  InitMergeTiles;
  MakeTilesUnique;
  FinishMergeTiles;
end;

procedure TKeyFrame.Dither;

  procedure DoQuantize(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    if not InRange(AIndex, 0, Encoder.FPaletteCount - 1) then
      Exit;

    InterLockedIncrement(Encoder.FKeyFramesThreadCount);
    try
      QuantizePalette(AIndex, Encoder.FQuantizerUseYakmo, Encoder.FQuantizerDennisLeeBitsPerComponent, IfThen(Encoder.FDitheringUseGamma, 0, -1));
    finally
      InterLockedDecrement(Encoder.FKeyFramesThreadCount);
    end;
  end;

var
  tidx: Int64;
begin
  if FrameCount = 0 then
    Exit;

  for tidx := 0 to High(Tiles) do
    Tiles[tidx]^.TmpIndex := -1;

  PreparePalettes(IfThen(Encoder.FDitheringUseGamma, 0, -1));

  SetLength(PaletteUseCount, Encoder.FPaletteCount);
  PalettesLeft := Encoder.FPaletteCount;

  InterLockedDecrement(Encoder.FKeyFramesThreadCount);
  try
    ProcThreadPool.DoParallelLocalProc(@DoQuantize, 0, Encoder.FPaletteCount - 1, nil, GetFreeThreadCount);
  finally
    InterLockedIncrement(Encoder.FKeyFramesThreadCount);
  end;

  FinishQuantizePalettes;
  PalettesLeft := -1;

  DitherTiles;
end;

procedure TKeyFrame.Cluster;
const
  CAllocEpsilon = 0.0001;
var
  frmIdx, ClusterCount: Integer;
  AllPsyV, KFPsyV: Double;
begin
  if FrameCount = 0 then
    Exit;

  // share GlobalTilingTileCount among keyframes, proportional to the amount of dithering psychovisual error
  // the rationale being: high dithering error means complex frame

  AllPsyV := 0.0;
  for frmIdx := 0 to High(Encoder.FFrames) do
    AllPsyV += CAllocEpsilon + Encoder.FFrames[frmIdx].DitherPsyV;

  KFPsyV := 0.0;
  for frmIdx := StartFrame to EndFrame do
    KFPsyV += CAllocEpsilon + Encoder.FFrames[frmIdx].DitherPsyV;

  ClusterCount := max(1, trunc(Div0(Encoder.FGlobalTilingTileCount * KFPsyV, AllPsyV)));

  // run the clustering algorithm, which will group similar tiles until it reaches a fixed amount of groups

  InitMergeTiles;
  DoKeyFrameKMeans(ClusterCount);
  FinishMergeTiles;
end;

procedure TKeyFrame.Reconstruct;

  procedure DoTiling(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    if not InRange(AIndex, 0, Encoder.FPaletteCount - 1) then
      Exit;

    InterLockedIncrement(Encoder.FKeyFramesThreadCount);
    try
      PrepareKFTiling(AIndex, IfThen(Encoder.FFrameTilingUseGamma, 0, -1));
      DoKFTiling(AIndex, IfThen(Encoder.FFrameTilingUseGamma, 0, -1), Encoder.FFrameTilingBlendingThreshold, Encoder.FFrameTilingFromPalette);
      FinishKFTiling(AIndex);
    finally
      InterLockedDecrement(Encoder.FKeyFramesThreadCount);
    end;
  end;

var
  kfIdx: Integer;
  tileResd, errCml: Double;
begin
  if FrameCount = 0 then
    Exit;

  ReconstructErrCml := 0.0;
  PalettesLeft := Encoder.FPaletteCount;
  SetLength(TileDS, Encoder.FPaletteCount);
  InterLockedDecrement(Encoder.FKeyFramesThreadCount);
  try
    ProcThreadPool.DoParallelLocalProc(@DoTiling, 0, Encoder.FPaletteCount - 1, nil, GetFreeThreadCount);
  finally
    InterLockedIncrement(Encoder.FKeyFramesThreadCount);
    SetLength(TileDS, 0);
  end;

  InterLockedDecrement(Encoder.FKeyFramesLeft);
  if Encoder.FKeyFramesLeft <= 0 then
  begin
    errCml := 0.0;
    for kfIdx := 0 to High(Encoder.FKeyFrames) do
      errCml += Encoder.FKeyFrames[kfIdx].ReconstructErrCml;

    tileResd := Sqrt(errCml / (Encoder.FTileMapSize * Length(Encoder.FFrames)));
    WriteLn('All:', Length(Encoder.FFrames):8, ' ResidualErr: ', (tileResd * Encoder.FTileMapSize):12:3, ' (by frame) ', tileResd:12:6, ' (by tile)');
  end;
end;

procedure TKeyFrame.ReColor;
var
  sx, sy, tx, ty, mtx, mty, frmIdx, palIdx, colIdx, cnt: Integer;
  r, g, b: Byte;
  Frame: TFrame;
  TMI: PTileMapItem;
  FTTile, TMTile: PTile;
  Accumulators: array of array of array[0 .. cColorCpns - 1 + 1] of Int64;
begin
  if FrameCount = 0 then
    Exit;

  SetLength(Accumulators, Encoder.FPaletteCount, Encoder.FPaletteSize);

  for frmIdx := StartFrame to EndFrame do
  begin
    Frame := Encoder.FFrames[frmIdx];

    Frame.AcquireFrameTiles;
    try
      for sy := 0 to Encoder.FTileMapHeight - 1 do
        for sx := 0 to Encoder.FTileMapWidth - 1 do
        begin
          TMI := @Frame.TileMap[sy, sx];
          FTTile := Frame.FrameTiles[sy * Encoder.FTileMapWidth + sx];
          TMTile := Tiles[TMI^.TileIdx];

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

              Accumulators[TMI^.PalIdx, colIdx, 0] += r;
              Accumulators[TMI^.PalIdx, colIdx, 1] += g;
              Accumulators[TMI^.PalIdx, colIdx, 2] += b;
              Accumulators[TMI^.PalIdx, colIdx, 3] += 1;
            end;
        end;
    finally
      Frame.ReleaseFrameTiles;
    end;
  end;

  for palIdx := 0 to Encoder.FPaletteCount - 1 do
    for colIdx := 0 to Encoder.FPaletteSize - 1 do
    begin
      cnt := Accumulators[palIdx, colIdx, 3];
      r := iDiv0(Accumulators[palIdx, colIdx, 0] + cnt div 2, cnt);
      g := iDiv0(Accumulators[palIdx, colIdx, 1] + cnt div 2, cnt);
      b := iDiv0(Accumulators[palIdx, colIdx, 2] + cnt div 2, cnt);

      PaletteRGB[palIdx, colIdx] := ToRGB(r, g, b);
    end;
end;

procedure TKeyFrame.Reindex;

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

  for tidx := 0 to High(Tiles) do
  begin
    Tiles[tidx]^.UseCount := 0;
    Tiles[tidx]^.Active := False;
  end;

  for frmIdx := StartFrame to EndFrame do
    for sy := 0 to Encoder.FTileMapHeight - 1 do
      for sx := 0 to Encoder.FTileMapWidth - 1 do
      begin
        TMI := @Encoder.FFrames[frmIdx].TileMap[sy, sx];

        HandleTileIndex(TMI^.TileIdx);
        HandleTileIndex(TMI^.TileIdx_Smoothed);
        HandleTileIndex(TMI^.BlendedTileIdx);
        HandleTileIndex(TMI^.BlendedTileIdx_Smoothed);
      end;

  ReindexTiles(False);
end;

procedure TKeyFrame.Smooth;
var
  NonAddlCount: Int64;
  frmIdx, i, sx, sy: Integer;
  TMI: PTileMapItem;
  ATList: TList;
  T: PTile;
begin
  if FrameCount = 0 then
    Exit;

  for frmIdx := StartFrame to EndFrame do
    for sy := 0 to (Encoder.FTileMapHeight - 1) do
      for sx := 0 to (Encoder.FTileMapWidth - 1) do
      begin
        TMI := @Encoder.FFrames[frmIdx].TileMap[sy, sx];

        TMI^.Smoothed := False;
        TMI^.Additional  := False;

        TMI^.CopyToSmoothed;
      end;

  ATList := TList.Create;
  try
    NonAddlCount := Length(Tiles);

    // iteratively smooth frames

    for frmIdx := StartFrame + 1 to EndFrame do
      DoTemporalSmoothing(Encoder.FFrames[frmIdx], Encoder.FFrames[frmIdx - 1], Encoder.FSmoothingFactor, Encoder.FSmoothingAdditionalTilesThreshold, NonAddlCount, ATList);
    DoTemporalSmoothing(nil, Encoder.FFrames[EndFrame], Encoder.FSmoothingFactor, Encoder.FSmoothingAdditionalTilesThreshold, NonAddlCount, ATList);

    // reintegrate AdditionalTiles into global tiles

    TTile.Array1DRealloc(Tiles, NonAddlCount + ATList.Count);
    for i := 0 to ATList.Count - 1 do
    begin
      T := PTile(ATList[i]);
      Tiles[i + Length(Tiles) - ATList.Count]^.CopyFrom(T^);
      TTile.Dispose(T);
    end;

    WriteLn('KF: ', StartFrame:8, ' AdditionalTiles: ', ATList.Count);
  finally
    ATList.Free;
  end;
end;

procedure TKeyFrame.RunAll_Dither;
begin
  LoadTiles;
  Dither;
end;

procedure TKeyFrame.RunAll_Tile;
begin
  Cluster;
  Reconstruct;
  Smooth;
  Reindex;
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
          FDCTLutDouble[False, i] := cos((x + 0.5) * u * PI / (cTileWidth * 2.0)) * cos((y + 0.5) * v * PI / (cTileWidth * 2.0)) * cDCTUVRatio[Min(v, 7), Min(u, 7)];
          FDCTLut[False, i] := FDCTLutDouble[False, i];

          FDCTLutDouble[True, i] := cos((x + 0.5) * u * PI / cTileWidth) * cos((y + 0.5) * v * PI / cTileWidth) * cDCTUVRatio[Min(v, 7), Min(u, 7)];
          FDCTLut[True, i] := FDCTLutDouble[True, i];

          FInvDCTLutDouble[i] := cos((u + 0.5) * x * PI / cTileWidth) * cos((v + 0.5) * y * PI / cTileWidth) * cDCTUVRatio[Min(y, 7), Min(x, 7)] * 2 / cTileWidth * 2 / cTileWidth;
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

procedure TTilingEncoder.Load;
var
  loadedFrameCount: Integer;

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

      InterLockedIncrement(loadedFrameCount);
      Write(loadedFrameCount:8, ' / ', Length(FFrames):8, #13);
    finally
      bmp.Free;
    end;
  end;

var
  frmIdx, frmCnt, eqtc, kfIdx, startFrmIdx: Integer;
  fn: String;
  bmp: TPicture;
  wasAutoQ, manualKeyFrames: Boolean;
  qbTC: TFloat;
begin
  eqtc := 0;
  for kfIdx := 0 to High(FKeyFrames) do
    eqtc += EqualQualityTileCount(FKeyFrames[kfIdx].FrameCount * FTileMapSize);
  wasAutoQ := (Length(FFrames) > 0) and (FGlobalTilingTileCount = round(FGlobalTilingQualityBasedTileCount * eqtc));

  ProgressRedraw(-1, esNone);

  ClearAll;

  // print settings

  WriteLn(GetSettings);

  ProgressRedraw(0, esLoad);

  // init Gamma LUTs

  InitLuts(FPaletteSize, FPaletteCount);

  // load video

  frmCnt := FFrameCountSetting;
  manualKeyFrames := False;

  if FileExists(FInputFileName) then
  begin
    DoExternalFFMpeg(FInputFileName, FInputPath, FStartFrame, frmCnt, FScaling, FFramesPerSecond);
    startFrmIdx := 1;
  end
  else
  begin
    FInputPath := FInputFileName;
    FFramesPerSecond := 24.0;
    startFrmIdx := FStartFrame;
    manualKeyFrames := True;
  end;

  // automaticaly count frames if needed

  if frmCnt <= 0 then
  begin
    frmIdx := 0;
    repeat
      fn := Format(FInputPath, [frmIdx + startFrmIdx]);
      Inc(frmIdx);
    until not FileExists(fn);

    frmCnt := frmIdx - 1;
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

  loadedFrameCount := 0;
  SetLength(FFrames, frmCnt);
  ProcThreadPool.DoParallelLocalProc(@DoLoadFrame, 0, High(FFrames), Pointer(PtrInt(startFrmIdx)));
  WriteLn;

  ProgressRedraw(2);

  FindKeyFrames(manualKeyFrames);
  FKeyFramesLeft := Length(FKeyFrames);

  ProgressRedraw(3);

  if wasAutoQ or (FGlobalTilingTileCount <= 0) then
  begin
    qbTC := FGlobalTilingQualityBasedTileCount;
    SetGlobalTilingQualityBasedTileCount(0.0);
    SetGlobalTilingQualityBasedTileCount(qbTC);
  end;
end;

procedure TTilingEncoder.Dither(AKeyFrameProcess: TKeyFrameProcess);
var
  StepProgress: Integer;

  procedure DoRun(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    keyFrame: TKeyFrame;
  begin
    if not InRange(AIndex, 0, High(FKeyFrames)) then
      Exit;

    InterLockedIncrement(FKeyFramesThreadCount);
    try
      keyFrame := FKeyFrames[AIndex];

      case AKeyFrameProcess of
        kfpAll: keyFrame.RunAll_Dither;
        kfpLoadTiles: keyFrame.LoadTiles;
        kfpDither: keyFrame.Dither;
      else
        Assert(False);
      end;
    finally
      InterLockedDecrement(FKeyFramesThreadCount);
    end;

    Inc(StepProgress, keyFrame.FrameCount);
    ProgressRedraw(StepProgress, esNone, AItem.Thread);
  end;

begin
  if Length(FFrames) = 0 then
    Exit;

  StepProgress := 0;
  ProgressRedraw(0, esDither);

  ProcThreadPool.DoParallelLocalProc(@DoRun, 0, High(FKeyFrames));
end;

procedure TTilingEncoder.Tile(AKeyFrameProcess: TKeyFrameProcess);
var
  StepProgress: Integer;

  procedure DoPsyV(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    sx, sy: Integer;
    psyV: TFloat;
    acc: Double;
    Frame: TFrame;
    PlainDCT, DitherDCT: array[0 .. cTileDCTSize - 1] of TFloat;
    TMI: PTileMapItem;
  begin
    if not InRange(AIndex, 0, High(FFrames)) then
      Exit;

    Frame := FFrames[AIndex];

    // already run?
    if not IsNan(Frame.DitherPsyV) then
      Exit;

    Frame.AcquireFrameTiles;
    try
      acc := 0.0;
      for sy := 0 to FTileMapHeight - 1 do
        for sx := 0 to FTileMapWidth - 1 do
        begin
          TMI := @Frame.TileMap[sy, sx];

          ComputeTilePsyVisFeatures(Frame.FrameTiles[sy * FTileMapWidth + sx]^, False, False, False, False, False, False, -1, nil, PFloat(@PlainDCT[0]));
          ComputeTilePsyVisFeatures(Frame.PKeyFrame.Tiles[TMI^.TileIdx]^, True, False, False, TMI^.HMirror, TMI^.VMirror, False, -1, Frame.PKeyFrame.PaletteRGB[TMI^.PalIdx], PFloat(@DitherDCT[0]));
          psyV := CompareEuclideanDCTPtr_asm(PFloat(@PlainDCT[0]), PFloat(@DitherDCT[0]));

          TMI^.DitherPsyV := Sqrt(psyV);

          acc += psyV;
        end;
      acc /= FTileMapSize;

      Frame.DitherPsyV := Sqrt(acc);
    finally
      Frame.ReleaseFrameTiles;
    end;
  end;

  procedure DoRun(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    keyFrame: TKeyFrame;
  begin
    if not InRange(AIndex, 0, High(FKeyFrames)) then
      Exit;

    InterLockedIncrement(FKeyFramesThreadCount);
    try
      keyFrame := FKeyFrames[AIndex];

      case AKeyFrameProcess of
        kfpAll: keyFrame.RunAll_Tile;
        kfpCluster: keyFrame.Cluster;
        kfpReconstruct: keyFrame.Reconstruct;
        kfpReColor: keyFrame.ReColor;
        kfpReindex: keyFrame.Reindex;
        kfpSmooth: keyFrame.Smooth;
      else
        Assert(False);
      end;

      Inc(StepProgress, keyFrame.FrameCount);
      ProgressRedraw(StepProgress, esNone, AItem.Thread);
    finally
      InterLockedDecrement(FKeyFramesThreadCount);
    end;
  end;

begin
  if Length(FFrames) = 0 then
    Exit;

  StepProgress := 0;
  ProgressRedraw(0, esTile);

  ProcThreadPool.DoParallelLocalProc(@DoPsyV, 0, High(FFrames));

  ProcThreadPool.DoParallelLocalProc(@DoRun, 0, High(FKeyFrames));
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
    SaveStream(fs);
  finally
    fs.Free;
  end;

  ProgressRedraw(1);
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
      for palIdx := 0 to FPaletteCount - 1 do
        for colIdx := 0 to FPaletteSize - 1 do
          palData.Add(IntToHex($ff000000 or FFrames[i].PKeyFrame.PaletteRGB[palIdx, colIdx], 8));
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

function TTilingEncoder.ComputeInterFrameCorrelation(a, b: TFrame): TFloat;
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
  finally
    a.ReleaseFrameTiles;
    b.ReleaseFrameTiles;
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

  // CIE XYZ color space from the Wright�Guild data
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

procedure TTilingEncoder.SetFrameTilingBlendingThreshold(AValue: TFloat);
begin
  if FFrameTilingBlendingThreshold = AValue then Exit;
  FFrameTilingBlendingThreshold := Max(0.0, AValue);
end;

procedure TTilingEncoder.SetGlobalTilingQualityBasedTileCount(AValue: TFloat);
var
  eqtc, kfIdx, RawTileCount: Integer;
begin
  if FGlobalTilingQualityBasedTileCount = AValue then Exit;
  FGlobalTilingQualityBasedTileCount := AValue;

  eqtc := 0;
  for kfIdx := 0 to High(FKeyFrames) do
    eqtc += EqualQualityTileCount(FKeyFrames[kfIdx].FrameCount * FTileMapSize);

  RawTileCount := Length(FFrames) * FTileMapSize;
  FGlobalTilingTileCount := min(round(AValue * eqtc), RawTileCount);
end;

procedure TTilingEncoder.SetLoadPerFrameTileCountMultiplier(AValue: TFloat);
begin
 if FLoadPerFrameTileCountMultiplier = AValue then Exit;
 FLoadPerFrameTileCountMultiplier := Max(1.0, AValue);
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

procedure TTilingEncoder.SetQuantizerPosterizeBitsPerComponent(AValue: Integer);
begin
 if FQuantizerPosterizeBitsPerComponent = AValue then Exit;
 FQuantizerPosterizeBitsPerComponent := EnsureRange(AValue, 1, 8);
end;

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
  VMirror, Inversible: Boolean; GammaCor: Integer; const pal: TIntegerDynArray; DCT: PFloat);
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

  pDCT := @DCT[0];
  for cpn := 0 to cColorCpns - 1 do
  begin
    pLut := @FDCTLut[Inversible, 0];
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

procedure TTilingEncoder.ComputeTilePsyVisFeatures(const ATile: TTile; FromPal, UseLAB, QWeighting, HMirror,
  VMirror, Inversible: Boolean; GammaCor: Integer; const pal: TIntegerDynArray; DCT: PDouble);
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

  pDCT := @DCT[0];
  for cpn := 0 to cColorCpns - 1 do
  begin
    pLut := @FDCTLutDouble[Inversible, 0];
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
      TMI^.BlendedTileIdx := -1;
      TMI^.PalIdx := -1;
      TMI^.HMirror := False;
      TMI^.VMirror := False;

      TMI^.Smoothed := False;
      TMI^.Additional := False;

      TMI^.CopyToSmoothed;
    end;

  Assert(ABitmap.Description.Width >= FScreenWidth, 'Wrong video width!');
  Assert(ABitmap.Description.Height >= FScreenHeight, 'Wrong video height!');

  // create frame tiles from image data

  AFrame.FrameTiles := TTile.Array1DNew(FTileMapSize, True, True);

  for i := 0 to FTileMapSize - 1 do
  begin
    T := AFrame.FrameTiles[i];

    T^.Active := True;
    T^.UseCount := 1;
    T^.TmpIndex := -1;
    T^.MergeIndex := -1;
    T^.FrameSoleIndex := -1;
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

procedure TTilingEncoder.FindKeyFrames(AManualMode: Boolean);
var
  Correlations: TFloatDynArray;

  procedure DoCorrel(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    if not InRange(AIndex, 1, High(FFrames)) then
      Exit;

    Correlations[AIndex] := ComputeInterFrameCorrelation(FFrames[AIndex - 1], FFrames[AIndex]);
  end;

const
  CShotTransMaxSecondsPerKF = 2.0;  // maximum seconds between keyframes
  CShotTransMinSecondsPerKF = 0.25; // minimum seconds between keyframes
  CShotTransCorrelLoThres = 0.75;   // interframe pearson correlation low limit
var
  frmIdx, kfIdx, lastKFIdx: Integer;
  correl: TFloat;
  kfReason: TKeyFrameReason;
  sfr, efr: Integer;
begin
  // compute interframe correlations

  SetLength(Correlations, Length(FFrames));
  Correlations[0] := 0.0;

  if not AManualMode then
    ProcThreadPool.DoParallelLocalProc(@DoCorrel, 1, High(FFrames));

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
      if FileExists(Format(ChangeFileExt(FInputPath, '.kf'), [frmIdx + StartFrame])) or (frmIdx = 0) then
        kfReason := kfrManual;
    end
    else
    begin
      if (kfReason = kfrNone) and (frmIdx = 0) then
        kfReason := kfrManual;

      if (kfReason = kfrNone) and (correl < CShotTransCorrelLoThres) then
        kfReason := kfrDecorrelation;

      if (kfReason = kfrNone) and ((frmIdx - lastKFIdx) >= (CShotTransMaxSecondsPerKF * FFramesPerSecond)) then
        kfReason := kfrLength;

      if (frmIdx - lastKFIdx) < (CShotTransMinSecondsPerKF * FFramesPerSecond) then
        kfReason := kfrNone;
    end;

    if kfReason <> kfrNone then
    begin
      FKeyFrames[kfIdx] := TKeyFrame.Create(Self, kfIdx, FPaletteCount, 0, 0);
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
      if vmir then tym := cTileWidth - 1 - tym;
      ptym := ty;
      if prevVmir then ptym := cTileWidth - 1 - ptym;

      for tx := 0 to cTileWidth - 1 do
      begin
        txm := tx;
        if hmir then txm := cTileWidth - 1 - txm;
        ptxm := tx;
        if prevHmir then ptxm := cTileWidth - 1 - ptxm;

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
  i, j, sx, sy, frmIdx, globalTileCount, kfTileCount: Integer;
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

    globalTileCount := GetGlobalTileCount(not (FRenderPlaying or AFast));
    kfTileCount := Frame.PKeyFrame.GetTileCount(not (FRenderPlaying or AFast));

    FRenderTitleText := 'Global: ' + IntToStr(globalTileCount) + ' / KeyFrame: ' + IntToStr(kfTileCount) + ' / Frame #' + IntToStr(FRenderFrameIndex) + IfThen(Frame.PKeyFrame.StartFrame = FRenderFrameIndex, ' [KF]', '     ') + ' : ' + IntToStr(GetFrameTileCount(Frame));

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
              if TMItem.Additional then
              begin
                TMItem.TileIdx := TMItem.TileIdx_Smoothed;
                TMItem.PalIdx := TMItem.PalIdx_Smoothed;
                TMItem.HMirror := TMItem.HMirror_Smoothed;
                TMItem.VMirror := TMItem.VMirror_Smoothed;
                TMItem.BlendedTileIdx := -1;
              end
              else
              begin
                while TMItem.Smoothed and not TMItem.Additional do
                begin
                  Dec(frmIdx);
                  TMItem := FFrames[frmIdx].TileMap[sy, sx];
                end;
              end;
            end
            else
            begin
              if TMItem.Smoothed and not TMItem.Additional then
                Continue;
            end;

            if InRange(TMItem.TileIdx, 0, High(Frame.PKeyFrame.Tiles)) then
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

              tilePtr := Frame.PKeyFrame.Tiles[TMItem.TileIdx];

              if not FRenderMirrored then
              begin
                TMItem.HMirror := False;
                TMItem.VMirror := False;
              end;

              if FRenderBlended and InRange(TMItem.BlendedTileIdx, 0, High(Frame.PKeyFrame.Tiles)) then
              begin
                DrawTile(FOutputBitmap, sx, sy, PsyTile, tilePtr, pal, TMItem.HMirror, TMItem.VMirror, Frame.PKeyFrame.Tiles[TMItem.BlendedTileIdx], pal, TMItem.HMirror, TMItem.VMirror, cMaxFTBlend div 2, cMaxFTBlend div 2);
              end
              else
              begin
                DrawTile(FOutputBitmap, sx, sy, PsyTile, tilePtr, pal, TMItem.HMirror, TMItem.VMirror, nil, nil, False, False, cMaxFTBlend, 0);
              end;

              if not (FRenderPlaying or AFast) then
                ComputeTilePsyVisFeatures(PsyTile^, False, False, False, False, False, False, -1, nil, PFloat(@chgDCT[sy, sx, 0]));
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

            if InRange(tidx, 0, High(Frame.PKeyFrame.Tiles)) then
            begin
              tilePtr := Frame.PKeyFrame.Tiles[tidx];
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
              ComputeTilePsyVisFeatures(tilePtr^, False, False, False, False, False, False, Ord(FRenderUseGamma) * 2 - 1, nil, PFloat(@DCT[0]));
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
    ini.WriteBool('Dither', 'QuantizerPosterize', QuantizerPosterize);
    ini.WriteInteger('Dither', 'QuantizerPosterizeBitsPerComponent', QuantizerPosterizeBitsPerComponent);
    ini.WriteBool('Dither', 'DitheringUseGamma', DitheringUseGamma);
    ini.WriteBool('Dither', 'DitheringUseThomasKnoll', DitheringUseThomasKnoll);
    ini.WriteInteger('Dither', 'DitheringYliluoma2MixedColors', DitheringYliluoma2MixedColors);

    ini.WriteFloat('GlobalTiling', 'GlobalTilingQualityBasedTileCount', RoundTo(Double(GlobalTilingQualityBasedTileCount), -7));
    ini.WriteInteger('GlobalTiling', 'GlobalTilingTileCount', GlobalTilingTileCount);

    ini.WriteBool('FrameTiling', 'FrameTilingFromPalette', FrameTilingFromPalette);
    ini.WriteBool('FrameTiling', 'FrameTilingUseGamma', FrameTilingUseGamma);
    ini.WriteFloat('FrameTiling', 'FrameTilingBlendingThreshold', RoundTo(Double(FrameTilingBlendingThreshold), -7));

    ini.WriteFloat('Smoothing', 'SmoothingFactor', RoundTo(Double(SmoothingFactor), -7));
    ini.WriteFloat('Smoothing', 'SmoothingAdditionalTilesThreshold', RoundTo(Double(SmoothingAdditionalTilesThreshold), -7));

    ini.WriteFloat('Misc', 'EncoderGammaValue', RoundTo(Double(EncoderGammaValue), -7));
    ini.WriteFloat('Misc', 'MaxThreadCount', MaxThreadCount);

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
    QuantizerPosterize := ini.ReadBool('Dither', 'QuantizerPosterize', False);
    QuantizerPosterizeBitsPerComponent := ini.ReadInteger('Dither', 'QuantizerPosterizeBitsPerComponent', 3);
    DitheringUseGamma := ini.ReadBool('Dither', 'DitheringUseGamma', False);
    DitheringUseThomasKnoll := ini.ReadBool('Dither', 'DitheringUseThomasKnoll', True);
    DitheringYliluoma2MixedColors := ini.ReadInteger('Dither', 'DitheringYliluoma2MixedColors', 4);

    GlobalTilingQualityBasedTileCount := ini.ReadFloat('GlobalTiling', 'GlobalTilingQualityBasedTileCount', 2.0);
    GlobalTilingTileCount := ini.ReadInteger('GlobalTiling', 'GlobalTilingTileCount', 0); // after GlobalTilingQualityBasedTileCount because has priority

    FrameTilingFromPalette := ini.ReadBool('FrameTiling', 'FrameTilingFromPalette', False);
    FrameTilingUseGamma := ini.ReadBool('FrameTiling', 'FrameTilingUseGamma', False);
    FrameTilingBlendingThreshold := ini.ReadFloat('FrameTiling', 'FrameTilingBlendingThreshold', 1.0);

    SmoothingFactor := ini.ReadFloat('Smoothing', 'SmoothingFactor', 0.02);
    SmoothingAdditionalTilesThreshold := ini.ReadFloat('Smoothing', 'SmoothingAdditionalTilesThreshold', 0.0);

    EncoderGammaValue := ini.ReadFloat('Misc', 'EncoderGammaValue', 2.0);
    MaxThreadCount := ini.ReadInteger('Misc', 'MaxThreadCount', MaxThreadCount);

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

  ComputeTilePsyVisFeatures(T^, False, False, False, False, False, True, -1, nil, @DCT[0]);
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

procedure TTilingEncoder.ProgressRedraw(ASubStepIdx: Integer; AProgressStep: TEncoderStep; AThread: TThread);

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
    ProgressStepPosition := iDiv0(ASubStepIdx * cProgressMul, GetStepLen);

  ProgressHourGlass := (AProgressStep <> esNone) and (ASubStepIdx < GetStepLen);

  if ASubStepIdx >= 0 then
  begin
    WriteLn('Step: ', Copy(GetEnumName(TypeInfo(TEncoderStep), Ord(FProgressStep)), 3), ' / ', ProgressStepPosition,
      #9'Time: ', FormatFloat('0.000', (curTime - FProgressPrevTime) / 1000), #9'All: ', FormatFloat('0.000', (curTime - FProgressStartTime) / 1000));
  end;
  FProgressPrevTime := curTime;

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

function TTilingEncoder.GetGlobalTileCount(AActiveOnly: Boolean): Integer;
var
  kfIdx: Integer;
  tidx: Int64;
begin
  Result := 0;
  for kfIdx := 0 to High(FKeyFrames) do
    if AActiveOnly then
    begin
      for tidx := 0 to High(FKeyFrames[kfIdx].Tiles) do
        if FKeyFrames[kfIdx].Tiles[tidx]^.Active then
          Inc(Result);
    end
    else
    begin
      Inc(Result, Length(FKeyFrames[kfIdx].Tiles));
    end;
end;

function TTilingEncoder.GetFrameTileCount(AFrame: TFrame): Integer;
var
  Used: TByteDynArray;
  sx, sy: Integer;
begin
  Result := 0;

  if Length(AFrame.PKeyFrame.Tiles) = 0 then
    Exit;

  SetLength(Used, Length(AFrame.PKeyFrame.Tiles));
  FillByte(Used[0], Length(AFrame.PKeyFrame.Tiles), 0);

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

class function TTilingEncoder.GetTilePalPixelsSum(const ATile: TTile): Integer;
var
  i, j: Integer;
begin
  Result := 0;
  for j := 0 to cTileWidth - 1 do
    for i := 0 to cTileWidth - 1 do
      Result += ATile.PalPixels[j, i];
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

  function ExtractTMIAttributes(KeyFrame: TKeyFrame; const TMI: TTileMapItem; IndexOffset: Integer; out attrs: Word; out isBlended: Boolean; out blendedIdx: Integer): Integer;
  begin
    attrs := (TMI.PalIdx_Smoothed shl 2) or (Ord(TMI.VMirror_Smoothed) shl 1) or Ord(TMI.HMirror_Smoothed);
    isBlended := (TMI.BlendedTileIdx_Smoothed >= 0) and not TMI.Additional;
    blendedIdx := TMI.BlendedTileIdx_Smoothed;
    Result := TMI.TileIdx_Smoothed;

    Assert(InRange(Result, 0, High(KeyFrame.Tiles)));
    if KeyFrame.Tiles[Result]^.IntraFrame then
      Result -= IndexOffset;

    if isBlended then
    begin
      Assert(InRange(blendedIdx, 0, High(KeyFrame.Tiles)));
      if KeyFrame.Tiles[blendedIdx]^.IntraFrame then
        blendedIdx -= IndexOffset;
    end;
  end;

  procedure DoTMI(KeyFrame: TKeyFrame; const TMI: TTileMapItem; IndexOffset: Integer);
  var
    tileIdx, blendedIdx: Integer;
    attrs: Word;
    isBlended: Boolean;
  begin
    Assert((TMI.PalIdx_Smoothed >= 0) and (TMI.PalIdx_Smoothed < FPaletteCount));

    tileIdx := ExtractTMIAttributes(KeyFrame, TMI, IndexOffset, attrs, isBlended, blendedIdx);

    if (tileIdx < (1 shl 16)) and (blendedIdx < (1 shl 16)) then
    begin
      if isBlended then
      begin
        DoCmd(gtShortAdditionalTileIdx, attrs);
        DoWord(tileIdx);
        DoWord(blendedIdx);
      end
      else
      begin
        DoCmd(gtShortTileIdx, attrs);
        DoWord(tileIdx);
      end;
    end
    else
    begin
      if isBlended then
      begin
        DoCmd(gtLongAdditionalTileIdx, attrs);
        DoDWord(tileIdx);
        DoDWord(blendedIdx);
      end
      else
      begin
        DoCmd(gtLongTileIdx, attrs);
        DoDWord(tileIdx);
      end;
    end;
  end;

  procedure WriteKFAttributes(KeyFrame: TKeyFrame);
  var
    i, j: Integer;
  begin
    for j := 0 to FPaletteCount - 1 do
    begin
      DoCmd(gtLoadPalette, 0);
      DoByte(j);
      DoByte(0);
      for i := 0 to FPaletteSize - 1 do
        DoDWord(KeyFrame.PaletteRGB[j, i] or $ff000000);
    end;
  end;

  procedure WriteListTiles(KeyFrame: TKeyFrame; List: TIntegerList; IndexOffset: Integer = 0);
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
          DoDWord(StartIdx - IndexOffset); // start tile
          DoDWord(PrevIdx - IndexOffset); // end tile
          //writeln(StartIdx - IndexOffset:8, PrevIdx - IndexOffset:8, IndexOffset:8);

          for j := StartIdx to PrevIdx do
          begin
            Assert(KeyFrame.Tiles[j]^.Active);
            ZStream.Write(KeyFrame.Tiles[j]^.GetPalPixelsPtr^[0, 0], sqr(cTileWidth));
          end;

          StartIdx := idx;
        end;
      end;
    end;
  end;

  procedure WriteKFTiles(KeyFrame: TKeyFrame);
  var
    fri, tx, ty: Integer;
    frm: TFrame;
    tmi: PTileMapItem;
    KFList: TIntegerList;
  begin
    KFList := TIntegerList.Create;
    try
      for fri := KeyFrame.StartFrame to KeyFrame.EndFrame do
      begin
        frm := FFrames[fri];
        for ty := 0 to FTileMapHeight - 1 do
          for tx := 0 to FTileMapWidth - 1 do
          begin
            tmi := @frm.TileMap[ty, tx];
            if not KeyFrame.Tiles[tmi^.TileIdx_Smoothed]^.IntraFrame then
              KFList.Add(tmi^.TileIdx_Smoothed);
            if (tmi^.BlendedTileIdx_Smoothed >= 0) and not KeyFrame.Tiles[tmi^.BlendedTileIdx_Smoothed]^.IntraFrame then
              KFList.Add(tmi^.BlendedTileIdx_Smoothed);
          end;
      end;

      WriteListTiles(KeyFrame, KFList);
    finally
      KFList.Free;
    end;
  end;

  procedure WriteFrameIntraTiles(Frame: TFrame; IndexOffset: Integer);
  var
    tx, ty: Integer;
    tmi: PTileMapItem;
    IntraList: TIntegerList;
  begin
    IntraList := TIntegerList.Create;
    try
      for ty := 0 to FTileMapHeight - 1 do
        for tx := 0 to FTileMapWidth - 1 do
        begin
          tmi := @Frame.TileMap[ty, tx];
          if Frame.PKeyFrame.Tiles[tmi^.TileIdx_Smoothed]^.IntraFrame then
            IntraList.Add(tmi^.TileIdx_Smoothed);
          if (tmi^.BlendedTileIdx_Smoothed >= 0) and Frame.PKeyFrame.Tiles[tmi^.BlendedTileIdx_Smoothed]^.IntraFrame then
            IntraList.Add(tmi^.BlendedTileIdx_Smoothed);
        end;

      WriteListTiles(Frame.PKeyFrame, IntraList, IndexOffset);
    finally
      IntraList.Free;
    end;
  end;

  procedure WriteDimensions;
  var
    maxTileCount, kfIdx: Integer;
  begin
    maxTileCount := 0;
    for kfIdx := 0 to High(FKeyFrames) do
      maxTileCount := Max(maxTileCount, Length(FKeyFrames[kfIdx].Tiles));

    DoCmd(gtSetDimensions, 0);
    DoWord(FTileMapWidth); // frame tilemap width
    DoWord(FTileMapHeight); // frame tilemap height
    DoDWord(round(1000*1000*1000 / FFramesPerSecond)); // frame length in nanoseconds
    DoDWord(maxTileCount); // tile count
  end;

  function GetIntraFrameEnd(KeyFrame: TKeyFrame; AFrameIdx: Integer): Integer;
  var
    tidx: Integer;
  begin
    Result := 0;
    for tidx := 0 to High(KeyFrame.Tiles) do
      if KeyFrame.Tiles[tidx]^.FrameSoleIndex = AFrameIdx then
        Result := tidx;
    Inc(Result);
  end;

var
  StartPos, StreamSize, LastKF, KFCount, KFSize, BlkSkipCount, FirstIntraIndex, FrameIntraIndex, IntraOffset: Integer;
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
  Header.EncoderVersion := 2; // 2 -> fixed blending extents
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
    WriteDimensions;

    LastKF := 0;
    for kfIdx := 0 to High(FKeyFrames) do
    begin
      KeyFrame := FKeyFrames[kfIdx];

      FirstIntraIndex := GetIntraFrameEnd(KeyFrame, -1);
      FrameIntraIndex := FirstIntraIndex;
      WriteKFTiles(KeyFrame); // has to be called before writing palettes

      WriteKFAttributes(KeyFrame);

      for frmIdx := KeyFrame.StartFrame to KeyFrame.EndFrame do
      begin
        Frame := FFrames[frmIdx];

        IntraOffset := FrameIntraIndex - FirstIntraIndex;
        FrameIntraIndex := GetIntraFrameEnd(KeyFrame, frmIdx);
        WriteFrameIntraTiles(Frame, IntraOffset);

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
              if not Frame.TileMap[sy, sx].Smoothed or Frame.TileMap[sy, sx].Additional then
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
              DoTMI(KeyFrame, Frame.TileMap[sy, sx], IntraOffset);
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
          LZCompress(ZStream, False, AStream);

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
  FLoadPerFrameTileCountMultiplier := 10.0;
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

