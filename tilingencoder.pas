unit tilingencoder;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}
{$TYPEDADDRESS ON}

{$define ASM_DBMP}


interface

uses
  windows, Classes, SysUtils, strutils, types, Math, FileUtil, typinfo, zstream, IniFiles, Graphics,
  IntfGraphics, FPimage, FPCanvas, FPWritePNG, GraphType, fgl, MTProcs, extern, tbbmalloc, bufstream, utils, kmodes;

type
  TEncoderStep = (esAll = -1, esLoad = 0, esPredictMotion, esReduce, esPreparePalettes, esDither, esReconstruct, esReindex, esSave);
  TKeyFrameReason = (kfrNone, kfrManual, kfrLength, kfrDecorrelation, kfrEuclidean);
  TRenderPage = (rpNone, rpInput, rpOutput, rpTilesPalette);
  TPsyVisMode = (pvsDCT, pvsWeightedDCT, pvsWavelets, pvsSpeDCT, pvsWeightedSpeDCT);
  TClusteringMethod = (cmBIRCH, cmBICO, cmTransferTiles);

const
  cEncoderStepLen: array[TEncoderStep] of Integer = (0, 3, 1, 2, 3, 2, 2, 3, 1);

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
  // PredictedTileShortOffsets:        data -> none; commandBits -> y offset (5 bits); x offset (5 bits)
  // PredictedTileLongOffsets:         data -> x offset (8 bits); y offset (8 bits); commandBits -> none
  // ShortTileIdxShortPalIdx:          data -> tile index (16 bits); commandBits -> palette index (10 bits); V mirror (1 bit); H mirror (1 bit)
  // LongTileIdxShortPalIdx:           data -> tile index (32 bits); commandBits -> palette index (10 bits); V mirror (1 bit); H mirror (1 bit)
  // LongTileIdxLongPalIdx:            data -> palette index (16 bits); tile index (32 bits); commandBits -> none (10 bits); V mirror (1 bit); H mirror (1 bit)
  // IntraTile:                        data -> palette index (16 bits); indexes per pixel (64 bytes); commandBits -> none (10 bits); V mirror (1 bit); H mirror (1 bit)
  // SkipBlock:                        data -> none; commandBits -> skip count - 1 (12 bits)
  //
  // (insert new commands here...)
  //
  // FrameEnd:                         data -> none; commandBits -> none (11 bits); keyframe end (1 bit)
  // LoadPalette:                      data -> palette index (16 bits); { RGBA bytes (32bits) } * indexes count; commandBits -> palette format (0: RGBA32) (12 bits)
  // TileSet:                          data -> start tile (32 bits); end tile (32 bits); { indexes per pixel (64 bytes) } * count; commandBits -> indexes count per palette
  // SetDimensions:                    data -> width in tiles (16 bits); height in tiles (16 bits); frame length in nanoseconds (32 bits) (2^32-1: still frame); tile count (32 bits); commandBits -> none
  // ExtendedCommand:                  data -> following bytes count (32 bits); custom commands, proprietary extensions, ...; commandBits -> extended command index (12 bits)

  TGTMCommand = (
    gtPredictedTileShortOffsets = 0,
    gtPredictedTileLongOffsets = 1,
    gtShortTileIdxShortPalIdx = 2,
    gtLongTileIdxShortPalIdx = 3,
    gtLongTileIdxLongPalIdx = 4,
    gtIntraTile = 5,
    gtSkipBlock = 6,

    gtFrameEnd = 11,
    gtLoadPalette = 12,
    gtTileSet = 13,
    gtSetDimensions = 14,
    gtExtendedCommand = 15
  );

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
    TmpIndex, MergeIndex: Integer;
    PalIdx_Initial: Integer;
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
    procedure CopyRGBPixels(const ARGBPixels: TRGBPixels); overload;
    procedure CopyRGBPixels(var AFrameBuffer: TIntegerDynArray2; AX, AY: Integer); overload;
    procedure ClearPalPixels;
    procedure ClearRGBPixels;
    procedure ClearPixels;
    procedure ExtractPalPixels(AArray: PFloat);
    procedure LoadPalPixels(AArray: PFloat);
    function ComparePalPixelsTo(const ATile: TTile): Integer;
    function CompareRGBPixelsTo(const ATile: TTile): Integer;
    function CompareRGBColorsTo(const ATile: TTile): Double;

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
    PalIdx: Integer;
    PredictedX, PredictedY: ShortInt;
    ResidualErr: TFloat;
    Flags: set of (tmfHMirror, tmfVMirror, tmfPredicted);
  end;

  PTileMapItem = ^TTileMapItem;

  TTileMapItems = array of TTileMapItem;

  { TTileMapItemHelper }

  TTileMapItemHelper = record helper for TTileMapItem
  private
    function GetHMirror: Boolean;
    function GetIsSmoothed: Boolean;
    function GetVMirror: Boolean;
    procedure SetHMirror(AValue: Boolean);
    procedure SetVMirror(AValue: Boolean);
    function GetIsPredicted: Boolean;
    procedure SetIsPredicted(AValue: Boolean);
  public
    property IsPredicted: Boolean read GetIsPredicted write SetIsPredicted;
    property IsSmoothed: Boolean read GetIsSmoothed;
    property HMirror: Boolean read GetHMirror write SetHMirror;
    property VMirror: Boolean read GetVMirror write SetVMirror;
  end;

  { TTilingDataset }

  TTilingDataset = record
    KNNSize: Integer;
    Dataset: TSingleDynArray2;
    ANN: PANNkdtree;

    PalDataset: TSingleDynArray2;
    PalANN: PANNkdtree;
  end;

  PTilingDataset = ^TTilingDataset;

  { TMixingPlan }

  TMixingPlan = record
    // static
    LumaPal: array of Integer;
    Remap: array of Byte;
    Y2Palette: array of array[0..3] of Integer;
    Y2MixedColors: Integer;
  end;

  TTilingEncoder = class;
  TKeyFrame = class;

  { TPalette }

  TPalette = record
    UseCount: Integer;
    PalIdx_Initial: Integer;
    PaletteRGB: TIntegerDynArray;
    MixingPlan: TMixingPlan;
    TileCount, TileOffset: Integer;
    CMPal: TCountIndexList;
  end;

  TPaletteArray = array of TPalette;

  { TFrame }

  TFrame = class
    Encoder: TTilingEncoder;
    PKeyFrame: TKeyFrame;
    Index: Integer;

    TileMap: array of array of TTileMapItem;

    InterframeCorrelationData: TFloatDynArray;
    InterframeCorrelation: TFloat; // with previous frame
    InterframeDistance: TFloat; // with previous frame
    InterframeCorrelationEvent: THandle;
    LoadFromImageFinishedEvent: THandle;

    FrameTiles: array of PTile;
    FrameTilesRefCount: Integer;
    FrameTilesEvent: THandle;
    FrameTilesLock: TSpinlock;
    CompressedFrameTiles: TMemoryStream;


    constructor Create(AParent: TTilingEncoder; AIndex: Integer);
    destructor Destroy; override;

    function PrepareInterFrameData: TFloatDynArray;
    procedure ComputeInterFrameAndCompress;

    procedure CompressFrameTiles;
    procedure AcquireFrameTiles;
    procedure ReleaseFrameTiles;

    // processes

    procedure LoadFromImage(AImageWidth, AImageHeight: Integer; AImage: PInteger);
    procedure PredictMotion(ARadius: Integer; var AFrontBuffer, ABackBuffer: TIntegerDynArray2);
    procedure Reconstruct(ARadius, AFTGamma: Integer; var AFrontBuffer, ABackBuffer: TIntegerDynArray2);
  end;

  TFrameArray =  array of TFrame;

  { TKeyFrame }

  TKeyFrame = class
    Encoder: TTilingEncoder;
    Index, StartFrame, EndFrame, FrameCount: Integer;
    Reason: TKeyFrameReason;

    ReconstructFramesLeft: Integer;
    ReconstructErrCml: Double;
    ReconstructLock: TSpinlock;

    procedure LogResidualErr;

    constructor Create(AParent: TTilingEncoder; AIndex, AStartFrame, AEndFrame: Integer);
    destructor Destroy; override;
  end;

  TKeyFrameArray =  array of TKeyFrame;

  TTilingEncoderProgressEvent = procedure(ASender: TTilingEncoder; APosition, AMax: Integer; AHourGlass: Boolean) of object;

  { TTilingEncoder }

  TTilingEncoder = class
  private
    // encoder state variables

    FCS: TRTLCriticalSection;
    FKeyFramesLeft: Integer;
    FTileDS: PTilingDataset;

    FGamma: array[0..1] of TFloat;
    FGammaCorLut: array[-1..1, 0..High(Byte)] of TFloat;
    FVecInv: array[0..256 * 4 - 1] of Cardinal;
    FDCTLut:array[Boolean {Special?}, 0..cUnrolledDCTSize - 1] of TFloat;
    FDCTLutDouble:array[Boolean {Special?}, 0..cUnrolledDCTSize - 1] of Double;
    FInvDCTLutDouble:array[0..cUnrolledDCTSize - 1] of Double;

    FTiles: PTileDynArray;
    FKeyFrames: TKeyFrameArray;
    FFrames: TFrameArray;
    FPalettes: TPaletteArray;

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
    FPaletteSize: Integer;
    FPaletteCount: Integer;
    FMotionPredictRadius: Integer;
    FDitheringUseGamma: Boolean;
    FDitheringMode: TPsyVisMode;
    FDitheringUseThomasKnoll: Boolean;
    FDitheringYliluoma2MixedColors: Integer;
    FGlobalTilingTileCount: Integer;
    FGlobalTilingQualityBasedTileCount: Double;
    FFrameTilingUseGamma: Boolean;
    FFrameTilingExtendedPaletteUsage: Boolean;
    FFrameTilingMode: TPsyVisMode;
    FShotTransMaxSecondsPerKF: Double;
    FShotTransMinSecondsPerKF: Double;
    FShotTransCorrelLoThres: Double;
    FShotTransDistHiThres: Double;

    // GUI state variables

    FRenderPredicted: Boolean;
    FRenderFrameIndex: Integer;
    FRenderPrevFrameIndex: Integer;
    FRenderPage: TRenderPage;
    FRenderPsychoVisualQuality: Double;
    FRenderTitleText: String;
    FRenderUseGamma: Boolean;
    FRenderMirrored: Boolean;
    FRenderPaletteIndex: Integer;
    FRenderPlaying: Boolean;
    FRenderOutputDithered: Boolean;
    FRenderTilePage: Integer;
    FRenderBackBuffer: TBitmap;
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
    function GetEncoderGammaValue: Double;
    function GetRenderGammaValue: Double;
    procedure SetDitheringYliluoma2MixedColors(AValue: Integer);
    procedure SetEncoderGammaValue(AValue: Double);
    procedure SetFrameCountSetting(AValue: Integer);
    procedure SetFramesPerSecond(AValue: Double);
    procedure SetGlobalTilingQualityBasedTileCount(AValue: Double);
    procedure SetMaxThreadCount(AValue: Integer);
    procedure SetPaletteCount(AValue: Integer);
    procedure SetPaletteSize(AValue: Integer);
    procedure SetMotionPredictRadius(AValue: Integer);
    procedure SetRenderFrameIndex(AValue: Integer);
    procedure SetRenderGammaValue(AValue: Double);
    procedure SetRenderPaletteIndex(AValue: Integer);
    procedure SetRenderTilePage(AValue: Integer);
    procedure SetGlobalTilingTileCount(AValue: Integer);
    procedure SetScaling(AValue: Double);
    procedure SetShotTransCorrelLoThres(AValue: Double);
    procedure SetShotTransDistHiThres(AValue: Double);
    procedure SetShotTransMaxSecondsPerKF(AValue: Double);
    procedure SetShotTransMinSecondsPerKF(AValue: Double);
    procedure SetStartFrame(AValue: Integer);

    function PearsonCorrelation(const x: TFloatDynArray; const y: TFloatDynArray): TFloat;

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

    generic procedure WaveletGS<T, PT>(Data: PT; Output: PT; dx, dy, depth: cardinal);
    generic procedure DeWaveletGS<T, PT>(wl: PT; pic: PT; dx, dy, depth: longint);
    procedure ComputeTilePsyVisFeatures(const ATile: TTile; Mode: TPsyVisMode; FromPal, UseLAB, HMirror, VMirror: Boolean;
     ColorCpns, GammaCor: Integer; const pal: TIntegerDynArray; DCT: PFloat); inline; overload;
    procedure ComputeTilePsyVisFeatures(const ATile: TTile; Mode: TPsyVisMode; FromPal, UseLAB, HMirror, VMirror: Boolean;
     ColorCpns, GammaCor: Integer; const pal: TIntegerDynArray; DCT: PDouble); inline; overload;
    procedure ComputeInvTilePsyVisFeatures(DCT: PDouble; Mode: TPsyVisMode; UseLAB: Boolean; ColorCpns, GammaCor: Integer;
      var ATile: TTile);

    // Dithering algorithms ported from http://bisqwit.iki.fi/story/howto/dither/jy/

    class function ColorCompare(r1, g1, b1, r2, g2, b2: Int64): Int64;
    procedure PreparePlan(var Plan: TMixingPlan; const pal: array of Integer);
    procedure TerminatePlan(var Plan: TMixingPlan);
    function DeviseBestMixingPlanYliluoma(var Plan: TMixingPlan; col: Integer; var List: array of Byte): Integer;
    procedure DeviseBestMixingPlanThomasKnoll(var Plan: TMixingPlan; col: Integer; var List: array of Byte);

    function GetTileCount(AActiveOnly: Boolean): Integer;
    function GetFrameTileCount(AFrame: TFrame): Integer;
    procedure DitherTile(var ATile: TTile; var Plan: TMixingPlan);
    class function GetTileZoneSum(const ATile: TTile; AOnPal: Boolean; x, y, w, h: Integer): Integer;
    class procedure GetTileHVMirrorHeuristics(const ATile: TTile; AOnPal: Boolean; out AHMirror, AVMirror: Boolean);
    class procedure HMirrorTile(var ATile: TTile; APalOnly: Boolean = False);
    class procedure VMirrorTile(var ATile: TTile; APalOnly: Boolean = False);

    procedure InitLuts;
    procedure ClearAll(AKeepFrames: Boolean);
    procedure ReframeUI(AWidth, AHeight: Integer);
    procedure InitFrames(AFrameCount: Integer);
    procedure LoadInputVideo;
    procedure FindKeyFrames(AManualMode: Boolean);

    function STCGREval(x: Double; Data: Pointer): Double;
    function SolveTileCount(ATileCount: Integer): Double;
    procedure TransferTiles(ATileCount: Integer);

    procedure DoPalettization(ADitheringGamma: Integer);
    procedure QuantizeUsingYakmo(APalIdx, AColorCount, APosterize: Integer);
    procedure DoQuantization(APalIdx: Integer; ADitheringGamma: Integer);
    procedure OptimizePalettes;

    procedure PrepareReconstruct(AFTGamma: Integer);
    procedure FinishReconstruct;

    procedure ReindexTiles(OnRGBPixels: Boolean);
    procedure MakeTilesUnique(OnRGBPixels: Boolean);
    procedure InitMergeTiles;
    procedure FinishMergeTiles;
    procedure MergeTiles(const TileIndexes: array of Int64; TileCount: Integer; BestIdx: Int64; NewTile: PPalPixels;
      NewTileRGB: PRGBPixels);

    procedure LoadStream(AStream: TStream);
    procedure SaveStream(AStream: TStream);

    // processes

    procedure Load;
    procedure PredictMotion;
    procedure Reduce;
    procedure PreparePalettes;
    procedure Dither;
    procedure Reconstruct;
    procedure Reindex;
    procedure Save;
  public
    // constructor / destructor

    constructor Create;
    destructor Destroy; override;

    // functions

    procedure Run(AStep: TEncoderStep = esAll);

    procedure Render;
    procedure GeneratePNGs(AInput: Boolean);
    procedure GenerateY4M(AFileName: String; AInput: Boolean);
    procedure SaveSettings(ASettingsFileName: String);
    procedure LoadSettings(ASettingsFileName: String);
    procedure LoadDefaultSettings;

    procedure ReloadGTM(AFileName: String);

    procedure Test;

    // encoder state variables

    property Tiles: PTileDynArray read GetTiles;
    property KeyFrames: TKeyFrameArray read FKeyFrames;
    property Frames: TFrameArray read FFrames;
    property Palettes: TPaletteArray read FPalettes;

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
    property EncoderGammaValue: Double read GetEncoderGammaValue write SetEncoderGammaValue;
    property PaletteSize: Integer read FPaletteSize write SetPaletteSize;
    property PaletteCount: Integer read FPaletteCount write SetPaletteCount;
    property MotionPredictRadius: Integer read FMotionPredictRadius write SetMotionPredictRadius;
    property DitheringUseGamma: Boolean read FDitheringUseGamma write FDitheringUseGamma;
    property DitheringMode: TPsyVisMode read FDitheringMode write FDitheringMode;
    property DitheringUseThomasKnoll: Boolean read FDitheringUseThomasKnoll write FDitheringUseThomasKnoll;
    property DitheringYliluoma2MixedColors: Integer read FDitheringYliluoma2MixedColors write SetDitheringYliluoma2MixedColors;
    property GlobalTilingTileCount: Integer read FGlobalTilingTileCount write SetGlobalTilingTileCount;
    property GlobalTilingQualityBasedTileCount: Double read FGlobalTilingQualityBasedTileCount write SetGlobalTilingQualityBasedTileCount;
    property FrameTilingUseGamma: Boolean read FFrameTilingUseGamma write FFrameTilingUseGamma;
    property FrameTilingExtendedPaletteUsage: Boolean read FFrameTilingExtendedPaletteUsage write FFrameTilingExtendedPaletteUsage;
    property FrameTilingMode: TPsyVisMode read FFrameTilingMode write FFrameTilingMode;
    property MaxThreadCount: Integer read GetMaxThreadCount write SetMaxThreadCount;
    property ShotTransMaxSecondsPerKF: Double read FShotTransMaxSecondsPerKF write SetShotTransMaxSecondsPerKF;
    property ShotTransMinSecondsPerKF: Double read FShotTransMinSecondsPerKF write SetShotTransMinSecondsPerKF;
    property ShotTransCorrelLoThres: Double read FShotTransCorrelLoThres write SetShotTransCorrelLoThres;
    property ShotTransDistHiThres: Double read FShotTransDistHiThres write SetShotTransDistHiThres;

    // GUI state variables

    property RenderPlaying: Boolean read FRenderPlaying write FRenderPlaying;
    property RenderFrameIndex: Integer read FRenderFrameIndex write SetRenderFrameIndex;
    property RenderPredicted: Boolean read FRenderPredicted write FRenderPredicted;
    property RenderMirrored: Boolean read FRenderMirrored write FRenderMirrored;
    property RenderOutputDithered: Boolean read FRenderOutputDithered write FRenderOutputDithered;
    property RenderUseGamma: Boolean read FRenderUseGamma write FRenderUseGamma;
    property RenderPaletteIndex: Integer read FRenderPaletteIndex write SetRenderPaletteIndex;
    property RenderTilePage: Integer read FRenderTilePage write SetRenderTilePage;
    property RenderGammaValue: Double read GetRenderGammaValue write SetRenderGammaValue;
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


implementation

const
  CGTMCommandsCount = Ord(High(TGTMCommand)) + 1;
  CGTMCommandCodeBits = round(ln(CGTMCommandsCount) / ln(2));
  CGTMCommandBits = 16 - CGTMCommandCodeBits;

  function CompareTileUseCountRev(Item1, Item2, UserParameter:Pointer):Integer;
  var
    t1, t2: PTile;
  begin
    t1 := PPTile(Item1)^;
    t2 := PPTile(Item2)^;

    Result := CompareValue(t2^.UseCount, t1^.UseCount);
    if Result = 0 then
    begin
      if Assigned(UserParameter) then
        Result := t1^.CompareRGBPixelsTo(t2^)
      else
        Result := t1^.ComparePalPixelsTo(t2^);
    end;
  end;

{ TTileMapItemHelper }

function TTileMapItemHelper.GetIsPredicted: Boolean;
begin
  Result := tmfPredicted in Flags;
end;

procedure TTileMapItemHelper.SetIsPredicted(AValue: Boolean);
begin
  if AValue then
    Flags += [tmfPredicted]
  else
    Flags -= [tmfPredicted];
end;

function TTileMapItemHelper.GetHMirror: Boolean;
begin
  Result := tmfHMirror in Flags;
end;

function TTileMapItemHelper.GetIsSmoothed: Boolean;
begin
  Result := IsPredicted and (PredictedX = 0) and (PredictedY = 0);
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
    PTile(data)^.PalIdx_Initial := -1;
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
    AArray[i]^.PalIdx_Initial := -1;
    Inc(data, size);
  end;
end;

class function TTileHelper.New(ARGBPixels, APalPixels: Boolean): PTile;
begin
  Result := AllocMem(SizeOf(TTile) + IfThen(APalPixels, SizeOf(TPalPixels)) + IfThen(ARGBPixels, SizeOf(TRGBPixels)));
  FillByte(Result^, SizeOf(TTile), 0);

  Result^.HasPalPixels := APalPixels;
  Result^.HasRGBPixels := ARGBPixels;
  Result^.PalIdx_Initial := -1;

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

procedure TTileHelper.CopyRGBPixels(var AFrameBuffer: TIntegerDynArray2; AX, AY: Integer);
var
  ty: Integer;
begin
  for ty := 0 to cTileWidth - 1 do
  begin
    Move(AFrameBuffer[AY, AX], GetRGBPixelsPtr^[ty, 0], cTileWidth * SizeOf(Integer));
    Inc(AY);
  end;
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
  if HasPalPixels then ClearPalPixels;
  if HasRGBPixels then ClearRGBPixels;
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

function TTileHelper.CompareRGBColorsTo(const ATile: TTile): Double;

  function DoOneComponent(APSelf, APOther: PByte): Integer;
  var
    ty: Integer;
  begin
    Result := 0;
    for ty := 0 to cTileWidth - 1 do
    begin
      // unroll by cTileWidth

      Result += Abs(APSelf[ 0] - APOther[ 0]);
      Result += Abs(APSelf[ 4] - APOther[ 4]);
      Result += Abs(APSelf[ 8] - APOther[ 8]);
      Result += Abs(APSelf[12] - APOther[12]);
      Result += Abs(APSelf[16] - APOther[16]);
      Result += Abs(APSelf[20] - APOther[20]);
      Result += Abs(APSelf[24] - APOther[24]);
      Result += Abs(APSelf[28] - APOther[28]);

      inc(APSelf, sizeof(Integer) * cTileWidth);
      inc(APOther, sizeof(Integer) * cTileWidth);
    end;
  end;

var
  PSelf, POther: PByte;
begin
  PSelf := PByte(GetRGBPixelsPtr);
  POther := PByte(ATile.GetRGBPixelsPtr);

  Result := DoOneComponent(@PSelf[0], @POther[0]);
  Result += DoOneComponent(@PSelf[1], @POther[1]);
  Result += DoOneComponent(@PSelf[2], @POther[2]);

  Result /= Sqr(cTileWidth) * cColorCpns;
end;

procedure TTileHelper.CopyFrom(const ATile: TTile);
begin
  UseCount := ATile.UseCount;
  TmpIndex := ATile.TmpIndex;
  PalIdx_Initial := ATile.PalIdx_Initial;
  MergeIndex := ATile.MergeIndex;
  Active := ATile.Active;
  HMirror_Initial := ATile.HMirror_Initial;
  VMirror_Initial := ATile.VMirror_Initial;

  if HasPalPixels and ATile.HasPalPixels then
    CopyPalPixels(ATile.GetPalPixelsPtr^);
  if HasRGBPixels and ATile.HasRGBPixels then
    CopyRGBPixels(ATile.GetRGBPixelsPtr^);
end;

{ TKeyFrame }

procedure TKeyFrame.LogResidualErr;
var
  kfIdx: Integer;
  tileResd, errCml: Double;
begin
  InterLockedDecrement(ReconstructFramesLeft);
  if ReconstructFramesLeft <= 0 then
  begin
    tileResd := Sqrt(ReconstructErrCml / (Encoder.FTileMapSize * FrameCount));
    WriteLn('KF: ', StartFrame:8, ' ResidualErr: ', tileResd:12:6, ' (by tile)');

    InterLockedDecrement(Encoder.FKeyFramesLeft);
    if Encoder.FKeyFramesLeft <= 0 then
    begin
      errCml := 0.0;
      for kfIdx := 0 to High(Encoder.FKeyFrames) do
        errCml += Encoder.FKeyFrames[kfIdx].ReconstructErrCml;

      tileResd := Sqrt(errCml / (Encoder.FTileMapSize * Length(Encoder.FFrames)));
      WriteLn('All:', Length(Encoder.FFrames):8, ' ResidualErr: ', tileResd:12:6, ' (by tile)');
    end;
  end;
end;

constructor TKeyFrame.Create(AParent: TTilingEncoder; AIndex, AStartFrame, AEndFrame: Integer);
begin
  Encoder := AParent;
  Index := AIndex;
  StartFrame := AStartFrame;
  EndFrame := AEndFrame;
  FrameCount := AEndFrame - AStartFrame + 1;

  SpinLeave(@ReconstructLock);
end;

destructor TKeyFrame.Destroy;
begin
  inherited Destroy;
end;

function CompareDSPixel(Item1,Item2,UserParameter:Pointer):Integer;
var
  a1: ^TDoubleDynArray absolute Item1;
  a2: ^TDoubleDynArray absolute Item2;
begin
  Result := CompareValue(a1^[1], a2^[1]); // G
  if Result = 0 then
    Result := CompareValue(a1^[0], a2^[0]); // R
  if Result = 0 then
    Result := CompareValue(a1^[2], a2^[2]); // B
end;

{ TFrame }

constructor TFrame.Create(AParent: TTilingEncoder; AIndex: Integer);
begin
  Encoder := AParent;
  Index := AIndex;

  FrameTilesEvent := CreateEvent(nil, True, False, nil);
  CompressedFrameTiles := TMemoryStream.Create;
  InterframeCorrelationEvent := CreateEvent(nil, True, False, nil);
  LoadFromImageFinishedEvent := CreateEvent(nil, True, False, nil);
  SpinLeave(@FrameTilesLock);
end;

destructor TFrame.Destroy;
begin
  CloseHandle(LoadFromImageFinishedEvent);
  CloseHandle(InterframeCorrelationEvent);
  CompressedFrameTiles.Free;
  CloseHandle(FrameTilesEvent);
  inherited Destroy;
end;

procedure TFrame.CompressFrameTiles;
var
  CompStream: Tcompressionstream;
begin
  CompressedFrameTiles.Clear;
  CompStream := Tcompressionstream.create(Tcompressionlevel.cldefault, CompressedFrameTiles, True);
  try
    CompStream.WriteBuffer(FrameTiles[0]^, Length(TileMap) * Length(TileMap[0]) * (SizeOf(TTile) + SizeOf(TRGBPixels)));
    CompStream.flush;
  finally
    CompStream.Free;
  end;

  Assert(CompressedFrameTiles.Size > 0);

  // now that FrameTiles are compressed, dispose them

  TTile.Array1DDispose(FrameTiles);
end;

procedure TFrame.AcquireFrameTiles;
var
  CompStream: Tdecompressionstream;
  ftrc: Integer;
begin
  SpinEnter(@FrameTilesLock);
  try
    Inc(FrameTilesRefCount);
    ftrc := FrameTilesRefCount;
  finally
    SpinLeave(@FrameTilesLock);
  end;

  if ftrc = 1 then
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
begin
  SpinEnter(@FrameTilesLock);
  try
    Dec(FrameTilesRefCount);
    if FrameTilesRefCount <= 0 then
    begin
      Assert(FrameTilesRefCount = 0);
      TTile.Array1DDispose(FrameTiles);

      ResetEvent(FrameTilesEvent);
    end;
  finally
    SpinLeave(@FrameTilesLock);
  end;
end;

procedure TFrame.PredictMotion(ARadius: Integer; var AFrontBuffer, ABackBuffer: TIntegerDynArray2);
var
  DCTs: array of array of array[0 .. cTileDCTSize - 1] of TFloat;

  procedure DoDCTs(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    x: Integer;
    DCTTile: PTile;
  begin
    if not InRange(AIndex, 0, Encoder.FScreenHeight - cTileWidth) then
      Exit;

    DCTTile := TTile.New(True, False);
    try
      for x := 0 to Encoder.FScreenWidth - cTileWidth do
      begin
        DCTTile^.CopyRGBPixels(ABackBuffer, x, AIndex);
        Encoder.ComputeTilePsyVisFeatures(DCTTile^, Encoder.FrameTilingMode, False, False, False, False, cColorCpns, -1, nil, DCTs[AIndex, x]);
      end;
    finally
      TTile.Dispose(DCTTile);
    end;
  end;

  procedure DoXY(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    dx, dy, sy, sx, oy, ox, oymn, oymx, oxmn, oxmx, ty, bestX, bestY: Integer;
    TMI: PTileMapItem;
    FrameTile: PTile;
    err, bestErr: TFloat;
    CurDCT: array[0 .. cTileDCTSize - 1] of TFloat;
  begin
    if not InRange(AIndex, 0, Encoder.FTileMapSize - 1) then
      Exit;

    DivMod(AIndex, Encoder.FTileMapWidth, sy, sx);

    FrameTile := TTile.New(True, False);
    try
      TMI := @TileMap[sy, sx];
      FrameTile^.CopyFrom(FrameTiles[AIndex]^);
      if FrameTile^.HMirror_Initial then Encoder.HMirrorTile(FrameTile^);
      if FrameTile^.VMirror_Initial then Encoder.VMirrorTile(FrameTile^);

      Encoder.ComputeTilePsyVisFeatures(FrameTile^, Encoder.FrameTilingMode, False, False, False, False, cColorCpns, -1, nil, CurDCT);

      bestX := MaxInt;
      bestY := MaxInt;
      bestErr := Infinity;

      dx := sx shl cTileWidthBits;
      dy := sy shl cTileWidthBits;

      oymn := Max(0, dy - ARadius - 1);
      oymx := Min(Encoder.FScreenHeight - cTileWidth, dy + ARadius);
      oxmn := Max(0, dx - ARadius - 1);
      oxmx := Min(Encoder.FScreenWidth - cTileWidth, dx + ARadius);

      for oy := oymn to oymx do
        for ox := oxmn to oxmx do
        begin
          err := CompareEuclideanDCTPtr_asm(CurDCT, DCTs[oy, ox]);

          // apply a penalty of float mantissa's unit times the manhattan distance to the center
          // rationale: slightly favoring the center in case of ties improves compressibility
          Inc(PInteger(@err)^, Abs(ox - dx) + Abs(oy - dy));

          if err < bestErr then
          begin
            bestErr := err;
            bestX := ox;
            bestY := oy;
          end;
        end;

      TMI^.ResidualErr := bestErr;
      TMI^.PredictedX := bestX - dx;
      TMI^.PredictedY := bestY - dy;

      // draw fb
      for ty := 0 to cTileWidth - 1 do
      begin
        Move(FrameTile^.GetRGBPixelsPtr^[ty, 0], AFrontBuffer[dy, dx], cTileWidth * SizeOf(Integer));
        Inc(dy);
      end;
    finally
      TTile.Dispose(FrameTile);
    end;
  end;


begin
  if ARadius <= 0 then
    Exit;

  Dec(ARadius);

  SetLength(DCTs, Encoder.FScreenHeight, Encoder.FScreenWidth);

  AcquireFrameTiles;
  try
    ProcThreadPool.DoParallelLocalProc(@DoDCTs, 0, Encoder.FScreenHeight - cTileWidth);

    ProcThreadPool.DoParallelLocalProc(@DoXY, 0, Encoder.FTileMapSize);
  finally
    ReleaseFrameTiles;
  end;
end;

procedure DoAsyncComputeInterFrameCorrelationAndCompress(AData : Pointer);
var
  Frame: TFrame;
begin
  Frame := TFrame(AData);

  Frame.ComputeInterFrameAndCompress;
end;

procedure TFrame.LoadFromImage(AImageWidth, AImageHeight: Integer; AImage: PInteger);
var
  i, j, col, ti, tx, ty: Integer;
  HMirror, VMirror: Boolean;
  pcol: PInteger;
  Tile: PTile;
  TMI: PTileMapItem;
begin
  // create frame tiles from image data

  FrameTiles := TTile.Array1DNew(Encoder.FTileMapSize, True, False);

  pcol := PInteger(AImage);
  for j := 0 to AImageHeight - 1 do
  begin
    for i := 0 to AImageWidth - 1 do
      begin
        col := pcol^;
        Inc(pcol);

        if (j < Encoder.FScreenHeight) and (i < Encoder.FScreenWidth) then
        begin
          ti := Encoder.FTileMapWidth * (j shr cTileWidthBits) + (i shr cTileWidthBits);
          tx := i and (cTileWidth - 1);
          ty := j and (cTileWidth - 1);
          col := SwapRB(col);

          FrameTiles[ti]^.RGBPixels[ty, tx] := col;
        end;
      end;
  end;

  for i := 0 to Encoder.FTileMapSize - 1 do
  begin
    Tile := FrameTiles[i];
    TMI := @TileMap[i div Encoder.FTileMapWidth, i mod Encoder.FTileMapWidth];

    Encoder.GetTileHVMirrorHeuristics(Tile^, False, HMirror, VMirror);

    Tile^.Active := True;
    Tile^.UseCount := 1;
    Tile^.TmpIndex := -1;
    Tile^.HMirror_Initial := HMirror;
    Tile^.VMirror_Initial := VMirror;

    TMI^.HMirror := HMirror;
    TMI^.VMirror := VMirror;

    if HMirror then Encoder.HMirrorTile(Tile^);
    if VMirror then Encoder.VMirrorTile(Tile^);
  end;

  // moderate the number of threads
  if Index >= Encoder.MaxThreadCount then
    WaitForSingleObject(Encoder.FFrames[Index - Encoder.MaxThreadCount].InterframeCorrelationEvent, INFINITE);

  TThread.ExecuteInThread(@DoAsyncComputeInterFrameCorrelationAndCompress, Self);
end;

function TFrame.PrepareInterFrameData: TFloatDynArray;
var
  i, sy, sx, ty, tx, sz, di: Integer;
  rr, gg, bb: Integer;
  lll, aaa, bbb, invSize: TFloat;
  pat: PInteger;
begin
  Result := nil;
  sz := Encoder.FTileMapSize;

  SetLength(Result, sz * cColorCpns);

  invSize := 1 / Sqr(cTileWidth);
  di := 0;
  for sy := 0 to Encoder.FTileMapHeight - 1 do
    for sx := 0 to Encoder.FTileMapWidth - 1 do
    begin
      i := sy * Encoder.FTileMapWidth + sx;
      pat := PInteger(@FrameTiles[i]^.GetRGBPixelsPtr^[0, 0]);

      for ty := 0 to cTileWidth - 1 do
        for tx := 0 to cTileWidth - 1 do
        begin
          FromRGB(pat^, rr, gg, bb);
          Inc(pat);
          Encoder.RGBToLAB(rr, gg, bb, -1, lll, aaa, bbb);
          Result[di + 0] += lll;
          Result[di + 1] += aaa;
          Result[di + 2] += bbb;
        end;

      Result[di + 0] *= invSize;
      Result[di + 1] *= invSize;
      Result[di + 2] *= invSize;

      Inc(di, 3);
    end;
  Assert(di = sz * cColorCpns);
end;

procedure TFrame.ComputeInterFrameAndCompress;
var
  i, j: Integer;
  prevLSum, curLSum: TFloat;
  prevFrameICD: TFloatDynArray;
begin
  // compute inter-frame correlations

  InterframeCorrelationData := PrepareInterFrameData;
  SetEvent(InterframeCorrelationEvent);

  if Index > 0 then
  begin
    // wait prev frame InterframeCorrelationData
    WaitForSingleObject(Encoder.FFrames[Index - 1].InterframeCorrelationEvent, INFINITE);

    prevFrameICD := Encoder.FFrames[Index - 1].InterframeCorrelationData;
    InterframeCorrelation := Encoder.PearsonCorrelation(prevFrameICD, InterframeCorrelationData);

    prevLSum := 0;
    curLSum := 0;
    j := 0;
    for i := 0 to Encoder.FTileMapSize - 1 do
    begin
      prevLSum += prevFrameICD[j];
      curLSum += InterframeCorrelationData[j];
      Inc(j, cColorCpns);
    end;
    InterframeDistance := Abs(prevLSum - curLSum) / Encoder.FTileMapSize;
  end;

  // compress frame tiles to save memory

  CompressFrameTiles;

  Write(Index + 1:8, ' / ', Length(Encoder.FFrames):8, #13);

  // wait until the next frame has finished
  if Index < High(Encoder.FFrames) then
    WaitForSingleObject(Encoder.FFrames[Index + 1].LoadFromImageFinishedEvent, INFINITE);
  SetLength(InterframeCorrelationData, 0);

  SetEvent(LoadFromImageFinishedEvent);
end;

procedure TFrame.Reconstruct(ARadius, AFTGamma: Integer; var AFrontBuffer, ABackBuffer: TIntegerDynArray2);
var
  DS: PTilingDataset;

  DCTs: array of array of array[0 .. cTileDCTSize - 1] of TFloat;

  procedure DoDCTs(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    x: Integer;
    DCTTile: PTile;
  begin
    if not InRange(AIndex, 0, Encoder.FScreenHeight - cTileWidth) then
      Exit;

    DCTTile := TTile.New(True, False);
    try
      for x := 0 to Encoder.FScreenWidth - cTileWidth do
      begin
        DCTTile^.CopyRGBPixels(ABackBuffer, x, AIndex);
        Encoder.ComputeTilePsyVisFeatures(DCTTile^, Encoder.FrameTilingMode, False, False, False, False, cColorCpns, -1, nil, DCTs[AIndex, x]);
      end;
    finally
      TTile.Dispose(DCTTile);
    end;
  end;

  procedure DoXY(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    sx, sy, dx, dy, ty, tx, tym, txm, ox, oy, oxmn, oxmx, oymn, oymx, palDsIdx, dsIdx, palIdx: Integer;
    dsErr, fbErr, palDsErr, err: Single;

    FrameTile, Tile: PTile;
    TMI: PTileMapItem;

    FTDCT, DCT: array[0 .. cTileDCTSize - 1] of Single;
  begin
    if not InRange(AIndex, 0, Encoder.FTileMapSize - 1) then
      Exit;

    FrameTile := TTile.New(True, True);
    try
      DivMod(AIndex, Encoder.FTileMapWidth, sy, sx);

      dx := sx shl cTileWidthBits;
      dy := sy shl cTileWidthBits;

      //

      TMI := @TileMap[sy, sx];

      FrameTile^.CopyFrom(FrameTiles[AIndex]^);

      Encoder.ComputeTilePsyVisFeatures(FrameTile^, Encoder.FrameTilingMode, False, False, False, False, cColorCpns, AFTGamma, nil, @FTDCT[0]);

      palDsErr := Infinity;
      palDsIdx := ann_kdtree_single_search(DS^.PalANN, @FTDCT[0], 0.0, @palDsErr);
      if not InRange(palDsIdx, 0, DS^.KNNSize - 1) then
      begin
        palDsIdx := -1;
        palDsErr := Infinity;
      end;

      palIdx := Encoder.FTiles[palDsIdx]^.PalIdx_Initial;

      dsIdx := -1;
      dsErr := Infinity;
      if Encoder.FrameTilingExtendedPaletteUsage and not IsZero(palDsErr) then
      begin
        Encoder.DitherTile(FrameTile^, Encoder.FPalettes[palIdx].MixingPlan);
        FrameTile^.ExtractPalPixels(@DCT[0]);

        dsIdx := ann_kdtree_single_search(DS^.ANN, @DCT[0], 0.0, @dsErr);
        if not InRange(dsIdx, 0, DS^.KNNSize - 1) then
        begin
          dsIdx := -1;
          dsErr := Infinity;
        end;

        Encoder.ComputeTilePsyVisFeatures(Encoder.FTiles[dsIdx]^, Encoder.FrameTilingMode, True, False, False, False, cColorCpns, AFTGamma, Encoder.FPalettes[palIdx].PaletteRGB, @DCT[0]);
        dsErr := CompareEuclideanDCTPtr_asm(FTDCT, DCT);
      end;

      if dsErr >= palDsErr then
      begin
        dsErr := palDsErr;
        dsIdx := palDsIdx;
      end;

      // repredict (account for palette)

      fbErr := Infinity;
      if (Index <> PKeyFrame.StartFrame) and not IsZero(dsErr) and (ARadius >= 0) then
      begin
        Encoder.ComputeTilePsyVisFeatures(FrameTile^, Encoder.FrameTilingMode, False, False, FrameTile^.HMirror_Initial, FrameTile^.VMirror_Initial, cColorCpns, AFTGamma, nil, @DCT[0]);

        oymn := Max(0, dy - ARadius - 1);
        oymx := Min(Encoder.FScreenHeight - cTileWidth, dy + ARadius);
        oxmn := Max(0, dx - ARadius - 1);
        oxmx := Min(Encoder.FScreenWidth - cTileWidth, dx + ARadius);

        for oy := oymn to oymx do
          for ox := oxmn to oxmx do
          begin
            err := CompareEuclideanDCTPtr_asm(DCT, DCTs[oy, ox]);

            // apply a penalty of float mantissa's unit times the manhattan distance to the center
            // rationale: slightly favoring the center in case of ties improves compressibility
            Inc(PInteger(@err)^, Abs(ox - dx) + Abs(oy - dy));

            if err < fbErr then
            begin
              fbErr := err;
              TMI^.PredictedX := ox - dx;
              TMI^.PredictedY := oy - dy;
            end;
          end;
      end;

      // map tilemap items to reduced tiles, parsing KNN query

      if InRange(dsIdx, 0, DS^.KNNSize - 1) and (dsErr < fbErr) then
      begin
        Tile := Encoder.FTiles[dsIdx];

        TMI^.TileIdx := dsIdx;
        TMI^.PalIdx := palIdx;
        TMI^.ResidualErr := dsErr;
        TMI^.IsPredicted := False;

        // draw fb (pal tile)
        for ty := 0 to cTileWidth - 1 do
        begin
          tym := ty;
          if TMI^.VMirror then tym := cTileWidth - 1 - tym;

          for tx := 0 to cTileWidth - 1 do
          begin
            txm := tx;
            if TMI^.HMirror then txm := cTileWidth - 1 - txm;

            AFrontBuffer[dy + ty, dx + tx] := Encoder.FPalettes[TMI^.PalIdx].PaletteRGB[Tile^.PalPixels[tym, txm]];
          end;
        end;
      end
      else
      begin
        TMI^.ResidualErr := fbErr;
        TMI^.IsPredicted := True;

        // draw fb (predicted tile)
        for ty := 0 to cTileWidth - 1 do
        begin
          Move(ABackBuffer[dy + TMI^.PredictedY, dx + TMI^.PredictedX], AFrontBuffer[dy, dx], cTileWidth * SizeOf(Integer));
          Inc(dy);
        end;
      end;
    finally
      TTile.Dispose(FrameTile);
    end;

    SpinEnter(@PKeyFrame.ReconstructLock);
    PKeyFrame.ReconstructErrCml += TMI^.ResidualErr;
    SpinLeave(@PKeyFrame.ReconstructLock);
  end;

begin
  DS := Encoder.FTileDS;
  if DS^.KNNSize <= 0 then
    Exit;

  Dec(ARadius);

  if ARadius >= 0 then
  begin
    SetLength(DCTs, Encoder.FScreenHeight, Encoder.FScreenWidth);
    ProcThreadPool.DoParallelLocalProc(@DoDCTs, 0, Encoder.FScreenHeight - cTileWidth);
  end;

  AcquireFrameTiles;
  try
    ProcThreadPool.DoParallelLocalProc(@DoXY, 0, Encoder.FTileMapSize - 1);
  finally
    ReleaseFrameTiles;
  end;

  PKeyFrame.LogResidualErr;
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
  for v := 0 to cTileWidth - 1 do
    for u := 0 to cTileWidth - 1 do
      for y := 0 to cTileWidth - 1 do
        for x := 0 to cTileWidth - 1 do
        begin
          FDCTLutDouble[False, i] := cos((x + 0.5) * u * PI / (cTileWidth)) * cos((y + 0.5) * v * PI / (cTileWidth)) * cDCTUVRatio[Min(v, 7), Min(u, 7)];
          FDCTLutDouble[True, i] := cos((x + 0.5) * u * PI / (cTileWidth * 2)) * cos((y + 0.5) * v * PI / (cTileWidth * 2)) * cDCTUVRatio[Min(v, 7), Min(u, 7)];
          FDCTLut[False, i] := FDCTLutDouble[False, i];
          FDCTLut[True, i] := FDCTLutDouble[True, i];
          Inc(i);
        end;

  // inverse DCT

  i := 0;
  for v := 0 to cTileWidth - 1 do
    for u := 0 to cTileWidth - 1 do
      for y := 0 to cTileWidth - 1 do
        for x := 0 to cTileWidth - 1 do
        begin
          FInvDCTLutDouble[i] := cos((u + 0.5) * x * PI / (cTileWidth)) * cos((v + 0.5) * y * PI / (cTileWidth)) * cDCTUVRatio[Min(y, 7), Min(x, 7)] * 2 / (cTileWidth) * 2 / (cTileWidth);
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
  ClearAll(False);

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

  ProgressRedraw(1, 'ProbeInputVideo');

  InitFrames(frmCnt);
  LoadInputVideo;

  ProgressRedraw(2, 'LoadInputVideo');

  FindKeyFrames(manualKeyFrames);

  ProgressRedraw(3, 'FindKeyFrames');

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

  procedure DoQuant(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    if not InRange(AIndex, 0, High(FPalettes)) then
      Exit;

    DoQuantization(AIndex, IfThen(FDitheringUseGamma, 0, -1));
  end;

begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(0, '', esPreparePalettes);

  DoPalettization(IfThen(FDitheringUseGamma, 0, -1));

  ProgressRedraw(1, 'Palettization');

  ProcThreadPool.DoParallelLocalProc(@DoQuant, 0, High(FPalettes));

  ProgressRedraw(2, 'Quantization');

  OptimizePalettes;

  ProgressRedraw(3, 'OptimizePalettes');
end;

procedure TTilingEncoder.Dither;

  procedure DoDither(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    Tile: PTile;
  begin
    if not InRange(AIndex, 0, High(FTiles)) then
      Exit;

    Tile := FTiles[AIndex];

    if not Tile^.Active then
      Exit;

    DitherTile(Tile^, FPalettes[Tile^.PalIdx_Initial].MixingPlan);
  end;

var
  palIdx: Integer;
begin
  if FrameCount = 0 then
    Exit;

  ProgressRedraw(0, '', esDither);

  // build ditherers
  for palIdx := 0 to High(FPalettes) do
    PreparePlan(FPalettes[palIdx].MixingPlan, FPalettes[palIdx].PaletteRGB);

  ProgressRedraw(1, 'BuildDitherers');

  ProcThreadPool.DoParallelLocalProc(@DoDither, 0, High(FTiles));

  ProgressRedraw(2, 'Dither');
end;

procedure TTilingEncoder.Reduce;
begin
  if FrameCount = 0 then
    Exit;

  ProgressRedraw(0, '', esReduce);

  SolveTileCount(FGlobalTilingTileCount);

  ProgressRedraw(1, 'SolveTileCount');

  ReindexTiles(True);

  ProgressRedraw(2, 'ReindexTiles');
end;

procedure TTilingEncoder.Reconstruct;
var
  frmIdx: Integer;
  gammaCor: Integer;
  curBuffer: Boolean;

  FrameBuffer: array[Boolean] of TIntegerDynArray2;
begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(0, '', esReconstruct);

  FKeyFramesLeft := Length(FKeyFrames);
  gammaCor := IfThen(FFrameTilingUseGamma, 0, -1);
  curBuffer := False;
  SetLength(FrameBuffer[False], FScreenHeight, FScreenWidth);
  SetLength(FrameBuffer[True], FScreenHeight, FScreenWidth);


  PrepareReconstruct(gammaCor);
  ProgressRedraw(1, 'PrepareReconstruct', esReconstruct);
  try
    for frmIdx := 0 to High(FFrames) do
    begin
      FFrames[frmIdx].Reconstruct(FMotionPredictRadius, gammaCor, FrameBuffer[not curBuffer], FrameBuffer[curBuffer]);
      curBuffer := not curBuffer;

      Write(frmIdx + 1:8, ' / ', Length(FFrames):8, #13);
    end;
  finally
    FinishReconstruct;
  end;

  ProgressRedraw(2, 'Reconstruct', esReconstruct);
end;

procedure TTilingEncoder.PredictMotion;
var
  frmIdx: Integer;
  curBuffer: Boolean;

  FrameBuffer: array[Boolean] of TIntegerDynArray2;
begin
  if (Length(FFrames) = 0) or (FMotionPredictRadius <= 0) then
    Exit;

  ProgressRedraw(0, '', esPredictMotion);

  curBuffer := False;
  SetLength(FrameBuffer[False], FScreenHeight, FScreenWidth);
  SetLength(FrameBuffer[True], FScreenHeight, FScreenWidth);

  for frmIdx := -Min(1, High(FFrames)) to High(FFrames) do // first frame is predicted from next frame if it exists
  begin
    FFrames[Abs(frmIdx)].PredictMotion(FMotionPredictRadius, FrameBuffer[not curBuffer], FrameBuffer[curBuffer]);
    curBuffer := not curBuffer;

    Write(frmIdx + 1:8, ' / ', Length(FFrames):8, #13);
  end;

  ProgressRedraw(1, '', esPredictMotion);
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

  MakeTilesUnique(False);

  ProgressRedraw(1, 'MakeTilesUnique');

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

  ProgressRedraw(2, 'UseCount');

  ReindexTiles(False);

  ProgressRedraw(3, 'Sort');
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

  ProgressRedraw(3, 'ReloadGTM');
end;

procedure TTilingEncoder.GeneratePNGs(AInput: Boolean);
var
  palPict: TPortableNetworkGraphic;
  i, palIdx, colIdx, oldRenderFrameIndex : Integer;
  oldRenderPage: TRenderPage;
  palData: TStringList;
  BMP: TBitmap;
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
    BMP := FOutputBitmap;
    if AInput then
    begin
      RenderPage := rpInput;
      BMP := FInputBitmap;
    end;

    palData.Clear;
    for palIdx := 0 to High(FPalettes) do
      for colIdx := 0 to FPaletteSize - 1 do
        palData.Add(IntToHex($ff000000 or FPalettes[palIdx].PaletteRGB[colIdx], 8));
    palData.SaveToFile(ChangeFileExt(FOutputFileName, '.txt'));

    for i := 0 to High(FFrames) do
    begin
      RenderFrameIndex := i;
      Render;

      palPict.Canvas.Draw(0, 0, BMP);
      palPict.SaveToFile(Format('%s_%.4d.png', [ChangeFileExt(FOutputFileName, ''), i]));
    end;
  finally
    palPict.Free;

    RenderFrameIndex := oldRenderFrameIndex;
    RenderPage := oldRenderPage;
    Render;

    palData.Free;
  end;
end;

procedure TTilingEncoder.GenerateY4M(AFileName: String; AInput: Boolean);
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
  BMP: TBitmap;
begin
  oldRenderFrameIndex := RenderFrameIndex;
  oldRenderPage := RenderPage;
  fs := TBufferedFileStream.Create(AFileName, fmCreate or fmShareDenyWrite);
  try
    Header := Format('YUV4MPEG2 W%d H%d F%d:1000000 C444'#10, [FTileMapWidth * cTileWidth, FTileMapHeight * cTileWidth, round(FFramesPerSecond * 1000000)]);
    fs.Write(Header[1], length(Header));

    SetLength(FrameData, FTileMapWidth * cTileWidth * FTileMapHeight * cTileWidth * cColorCpns);

    RenderPage := rpOutput;
    BMP := FOutputBitmap;
    if AInput then
    begin
      RenderPage := rpInput;
      BMP := FInputBitmap;
    end;

    for i := 0 to High(FFrames) do
    begin
      FrameHeader := 'FRAME '#10;
      fs.Write(FrameHeader[1], Length(FrameHeader));

      RenderFrameIndex := i;
      Render;

      py := @FrameData[0 * Length(FrameData) div cColorCpns];
      pu := @FrameData[1 * Length(FrameData) div cColorCpns];
      pv := @FrameData[2 * Length(FrameData) div cColorCpns];

      BMP.BeginUpdate;
      try
        for fy := 0 to BMP.Height - 1 do
        begin
          ptr := PByte(BMP.ScanLine[fy]);
          for fx := 0 to BMP.Width - 1 do
          begin
            b := ptr^; Inc(ptr);
            g := ptr^; Inc(ptr);
            r := ptr^; Inc(ptr);
            Inc(ptr); // alpha

            RGBToYUV(r, g, b, -1, yf, uf, vf);

            py^ := EnsureRange(Round(yf * High(Byte)), 0, High(Byte)); Inc(py);
            pu^ := EnsureRange(Round((uf + 0.5) * High(Byte)), 0, High(Byte)); Inc(pu);
            pv^ := EnsureRange(Round((vf + 0.5) * High(Byte)), 0, High(Byte)); Inc(pv);
          end;
        end;
      finally
        BMP.EndUpdate;
      end;

      fs.Write(FrameData[0], Length(FrameData));
    end;
  finally
    RenderFrameIndex := oldRenderFrameIndex;
    RenderPage := oldRenderPage;
    Render;
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

  Result := 1.0;
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

function TTilingEncoder.GetEncoderGammaValue: Double;
begin
  Result := FGamma[0];
end;

function TTilingEncoder.GetRenderGammaValue: Double;
begin
  Result := FGamma[1];
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

procedure TTilingEncoder.PreparePlan(var Plan: TMixingPlan; const pal: array of Integer);
var
  i, cnt, r, g, b: Integer;
begin
  FillChar(Plan, SizeOf(Plan), 0);

  Plan.Y2MixedColors := FDitheringYliluoma2MixedColors;
  SetLength(Plan.LumaPal, length(pal));
  SetLength(Plan.Y2Palette, length(pal));
  SetLength(Plan.Remap, length(pal));

  cnt := 0;
  for i := 0 to High(pal) do
  begin
    if pal[i] = cDitheringNullColor then
      Continue;

    FromRGB(pal[i], r, g, b);

    Plan.LumaPal[cnt] := r*cRedMul + g*cGreenMul + b*cBlueMul;

    Plan.Y2Palette[cnt][0] := r;
    Plan.Y2Palette[cnt][1] := g;
    Plan.Y2Palette[cnt][2] := b;
    Plan.Y2Palette[cnt][3] := Plan.LumaPal[cnt] div cLumaDiv;

    Plan.Remap[cnt] := i;
    Inc(cnt);
  end;

  SetLength(Plan.LumaPal, cnt);
  SetLength(Plan.Y2Palette, cnt);
  SetLength(Plan.Remap, cnt);
end;

procedure TTilingEncoder.TerminatePlan(var Plan: TMixingPlan);
begin
  SetLength(Plan.LumaPal, 0);
  SetLength(Plan.Y2Palette, 0);
  SetLength(Plan.Remap, 0);
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

  FRenderBackBuffer.Width := FScreenWidth;
  FRenderBackBuffer.Height := FScreenHeight;
  FRenderBackBuffer.PixelFormat := pf32bit;

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
          ATile.PalPixels[y, x] := Plan.Remap[TKList[map_value]];
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
          ATile.PalPixels[y, x] := Plan.Remap[YilList[map_value]];
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

// from https://lists.freepascal.org/pipermail/fpc-announce/2006-September/000508.html
generic procedure TTilingEncoder.WaveletGS<T, PT>(Data: PT; Output: PT; dx, dy, depth: cardinal);
var
  x, y: longint;
  offset: cardinal;
  factor: T;
  tempX: array[0 .. sqr(cTileWidth) - 1] of T;
  tempY: array[0 .. sqr(cTileWidth) - 1] of T;
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
    Move(tempY[y * cTileWidth], Output[y * cTileWidth], dx * sizeof(T)); //Copy to Wavelet

  if depth>0 then
    specialize waveletgs<T, PT>(Output, Output, dx div 2, dy div 2, depth - 1); //Repeat for SubDivisionDepth
end;

generic procedure TTilingEncoder.DeWaveletGS<T, PT>(wl: PT; pic: PT; dx, dy, depth: longint);
Var x,y : longint;
    tempX: array[0 .. sqr(cTileWidth) - 1] of T;
    tempY: array[0 .. sqr(cTileWidth) - 1] of T;
    offset,offsetm1,offsetp1 : longint;
    factor : T;
    dyoff,yhalf,yhalfoff,yhalfoff2,yhalfoff3 : longint;
BEGIN
 FillChar(tempX[0], SizeOf(tempX), 0);
 FillChar(tempY[0], SizeOf(tempY), 0);

 if depth>0 then specialize dewaveletgs<T, PT>(wl,wl,dx div 2,dy div 2,depth-1); //Repeat for SubDivisionDepth

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
  move(tempx[y*cTileWidth],pic[y*cTileWidth],dx*sizeof(T)); //Copy to Pic
END;

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

procedure TTilingEncoder.SetEncoderGammaValue(AValue: Double);
begin
  if FGamma[0] = AValue then Exit;
  FGamma[0] := Max(0.0, AValue);
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

procedure TTilingEncoder.SetMaxThreadCount(AValue: Integer);
begin
 if ProcThreadPool.MaxThreadCount = AValue then Exit;
 ProcThreadPool.MaxThreadCount := max(1, AValue);
end;

procedure TTilingEncoder.SetPaletteCount(AValue: Integer);
begin
  if FPaletteCount = AValue then Exit;
  FPaletteCount := EnsureRange(AValue, 1, 65536);
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

procedure TTilingEncoder.SetShotTransDistHiThres(AValue: Double);
begin
 if FShotTransDistHiThres = AValue then Exit;
 FShotTransDistHiThres := max(0.0, AValue);
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
  if FGamma[1] = AValue then Exit;
  FGamma[1] := Max(0.0, AValue);
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

procedure TTilingEncoder.SetMotionPredictRadius(AValue: Integer);
begin
  if FMotionPredictRadius = AValue then Exit;
  FMotionPredictRadius := EnsureRange(AValue, 1, -Low(ShortInt));
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

procedure TTilingEncoder.ComputeTilePsyVisFeatures(const ATile: TTile; Mode: TPsyVisMode; FromPal, UseLAB, HMirror,
  VMirror: Boolean; ColorCpns, GammaCor: Integer; const pal: TIntegerDynArray; DCT: PFloat);
var
  i, u, v, x, y, xx, yy, cpn: Integer;
  z: Double;
  CpnPixels: TCpnPixels;
  pDCT, pLut: PFloat;
  LocalDCT: array[0..cTileDCTSize - 1] of TFloat;

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

  if Mode = pvsWavelets then
  begin
   for cpn := 0 to ColorCpns - 1 do
   begin
     pDCT := @LocalDCT[cpn * sqr(cTileWidth)];
     specialize WaveletGS<Single, PSingle>(@CpnPixels[cpn, 0, 0], pDCT, cTileWidth, cTileWidth, 2);
   end;
  end
  else
  begin
    for cpn := 0 to ColorCpns - 1 do
    begin
      pDCT := @LocalDCT[cpn * sqr(cTileWidth)];
      pLut := @FDCTLut[Mode in [pvsSpeDCT, pvsWeightedSpeDCT], 0];
      for v := 0 to cTileWidth - 1 do
        for u := 0 to cTileWidth - 1 do
        begin
  		    z := specialize DCTInner<PSingle>(@CpnPixels[cpn, 0, 0], pLut, 1);

          if Mode in [pvsWeightedDCT, pvsWeightedSpeDCT] then
             z *= cDCTQuantization[cpn, v, u];

          pDCT^ := z;
          Inc(pDCT);
          Inc(pLut, Sqr(cTileWidth));
        end;
    end;
  end;

  for cpn := 0 to ColorCpns - 1 do
    for i := 0 to sqr(cTileWidth) - 1 do
      DCT[cDCTSnake[i] + cpn * sqr(cTileWidth)] := LocalDCT[i + cpn * sqr(cTileWidth)];
end;

procedure TTilingEncoder.ComputeTilePsyVisFeatures(const ATile: TTile; Mode: TPsyVisMode; FromPal, UseLAB, HMirror,
  VMirror: Boolean; ColorCpns, GammaCor: Integer; const pal: TIntegerDynArray; DCT: PDouble);
var
  i, u, v, x, y, xx, yy, cpn: Integer;
  z: Double;
  CpnPixels: TCpnPixelsDouble;
  pDCT, pLut: PDouble;
  LocalDCT: array[0..cTileDCTSize - 1] of Double;

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

  if Mode = pvsWavelets then
  begin
   for cpn := 0 to ColorCpns - 1 do
   begin
     pDCT := @LocalDCT[cpn * sqr(cTileWidth)];
     specialize WaveletGS<Double, PDouble>(@CpnPixels[cpn, 0, 0], pDCT, cTileWidth, cTileWidth, 2);
   end;
  end
  else
  begin
    for cpn := 0 to ColorCpns - 1 do
    begin
      pDCT := @LocalDCT[cpn * sqr(cTileWidth)];
      pLut := @FDCTLutDouble[Mode in [pvsSpeDCT, pvsWeightedSpeDCT], 0];
      for v := 0 to cTileWidth - 1 do
        for u := 0 to cTileWidth - 1 do
        begin
  		    z := specialize DCTInner<PDouble>(@CpnPixels[cpn, 0, 0], pLut, 1);

          if Mode in [pvsWeightedDCT, pvsWeightedSpeDCT] then
             z *= cDCTQuantization[cpn, v, u];

          pDCT^ := z;
          Inc(pDCT);
          Inc(pLut, Sqr(cTileWidth));
        end;
    end;
  end;

  for cpn := 0 to ColorCpns - 1 do
    for i := 0 to sqr(cTileWidth) - 1 do
      DCT[cDCTSnake[i] + cpn * sqr(cTileWidth)] := LocalDCT[i + cpn * sqr(cTileWidth)];
end;

procedure TTilingEncoder.ComputeInvTilePsyVisFeatures(DCT: PDouble; Mode: TPsyVisMode; UseLAB: Boolean; ColorCpns, GammaCor: Integer;
 var ATile: TTile);
var
  i, u, v, x, y, cpn: Integer;
  CpnPixels: TCpnPixelsDouble;
  pCpn, pLut, pDCT: PDouble;
  LocalDCT: array[0..cTileDCTSize - 1] of Double;
  d: Double;


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
  Assert(not (Mode in [pvsSpeDCT, pvsWeightedSpeDCT]), 'Special DCT is non-inversible');

  pDCT := @LocalDCT[0];
  for cpn := 0 to ColorCpns - 1 do
  begin
    i := 0;
    for v := 0 to cTileWidth - 1 do
      for u := 0 to cTileWidth - 1 do
      begin
        d := DCT[cDCTSnake[i] + cpn * sqr(cTileWidth)];
        if Mode in [pvsWeightedDCT, pvsWeightedSpeDCT] then
          pDCT^ := d / cDCTQuantization[cpn, v, u]
        else
          pDCT^ := d;
        Inc(pDCT);
        Inc(i);
      end;
  end;

  if Mode = pvsWavelets then
  begin
    for cpn := 0 to ColorCpns - 1 do
    begin
      pCpn := @CpnPixels[cpn, 0, 0];
      specialize DeWaveletGS<Double, PDouble>(@LocalDCT[cpn * sqr(cTileWidth)], pCpn, cTileWidth, cTileWidth, 2);
    end;
  end
  else
  begin
    for cpn := 0 to ColorCpns - 1 do
    begin
      pCpn := @CpnPixels[cpn, 0, 0];
      pLut := @FInvDCTLutDouble[0];

      for y := 0 to cTileWidth - 1 do
        for x := 0 to cTileWidth - 1 do
        begin
          pCpn^ := specialize DCTInner<PDouble>(@LocalDCT[cpn * sqr(cTileWidth)], pLut, 1);
          Inc(pCpn);
          Inc(pLut, Sqr(cTileWidth));
        end;
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

procedure DoLoadFFMPEGFrame(AIndex, AWidth, AHeight:Integer; AFrameData: PInteger; AUserParameter: Pointer);
var
  Encoder: TTilingEncoder;
  frmIdx: Integer;
begin
  Encoder := TTilingEncoder(AUserParameter);
  frmIdx := AIndex - Encoder.FStartFrame;

  Encoder.FFrames[frmIdx].LoadFromImage(AWidth, AHeight, AFrameData);
end;

procedure TTilingEncoder.LoadInputVideo;
var
  FFMPEG: TFFMPEG;
  PNG: TPortableNetworkGraphic;
  frmIdx: Integer;
begin
  if FileExists(FLoadedInputPath) then
  begin
    FFMPEG := FFMPEG_Open(FLoadedInputPath, FScaling, True);
    try
      FFMPEG_LoadFrames(FFMPEG, FStartFrame, Length(FFrames), @DoLoadFFMPEGFrame, Self);
    finally
      FFMPEG_Close(FFMPEG);
    end;
  end
  else
  begin
    PNG := TPortableNetworkGraphic.Create;
    try
      PNG.PixelFormat:=pf32bit;
      for frmIdx := 0 to High(FFrames) do
      begin
        PNG.LoadFromFile(Format(FLoadedInputPath, [frmIdx + FStartFrame]));
        FFrames[frmIdx].LoadFromImage(PNG.RawImage.Description.Width, PNG.RawImage.Description.Height, PInteger(PNG.RawImage.Data));
      end;
    finally
      PNG.Free;
    end;
  end;

  // wait last frame LoadFromImageFinishedEvent (ensures all frames processed)

  WaitForSingleObject(FFrames[High(FFrames)].LoadFromImageFinishedEvent, INFINITE);
end;

procedure TTilingEncoder.FindKeyFrames(AManualMode: Boolean);
var
  frmIdx, kfIdx, lastKFIdx: Integer;
  correl, dist: TFloat;
  kfReason: TKeyFrameReason;
  sfr, efr: Integer;
begin
  // find keyframes

  SetLength(FKeyFrames, Length(FFrames));
  kfIdx := 0;
  lastKFIdx := Low(Integer);
  for frmIdx := 0 to High(FFrames) do
  begin
    correl := FFrames[frmIdx].InterframeCorrelation;
    dist := FFrames[frmIdx].InterframeDistance;

    //writeln(frmIdx:8,correl:8:3,dist:12:3);

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

      if (kfReason = kfrNone) and (dist >= FShotTransDistHiThres) then
        kfReason := kfrEuclidean;

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

procedure TTilingEncoder.ClearAll(AKeepFrames: Boolean);
var
  i: Integer;
begin
  if not AKeepFrames then
  begin
    for i := 0 to High(FFrames) do
      if Assigned(FFrames[i]) then
        FreeAndNil(FFrames[i]);
    SetLength(FFrames, 0);
  end;

  for i := 0 to High(FKeyFrames) do
    if Assigned(FKeyFrames[i]) then
      FreeAndNil(FKeyFrames[i]);
  SetLength(FKeyFrames, 0);

  TTile.Array1DDispose(FTiles);
end;

procedure TTilingEncoder.Render;

  procedure DrawTile(bitmap: TBitmap; sx, sy: Integer; psyTile: PTile; tilePtr: PTile; pal: TIntegerDynArray; hmir, vmir, forceActive: Boolean); inline;
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
        if tilePtr^.Active or forceActive then
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
  i, j, sx, sy, ty, globalTileCount: Integer;
  posl, pbsl: PInteger;
  hmir, vmir: Boolean;
  tidx: Int64;
  p: PInteger;
  tilePtr: PTile;
  TMItem: TTileMapItem;
  Frame: TFrame;
  pal: TIntegerDynArray;
  q: Double;
begin
  if Length(FFrames) <= 0 then
    Exit;

  Frame := FFrames[FRenderFrameIndex];

  if not Assigned(Frame) or not Assigned(Frame.PKeyFrame) then
    Exit;

  try

    // Global

    globalTileCount := GetTileCount(False);

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

            hmir := tilePtr^.HMirror_Initial;
            vmir := tilePtr^.VMirror_Initial;

            if not FRenderMirrored then
            begin
              hmir := False;
              vmir := False;
            end;

            DrawTile(FInputBitmap, sx, sy, nil, tilePtr, nil, hmir, vmir, True);
          end;
      finally
        FInputBitmap.EndUpdate;
        Frame.ReleaseFrameTiles;
      end;
    end;

    // "Output" tab

    if FRenderPage = rpOutput then
    begin
      if Frame.Index <> FRenderPrevFrameIndex then
        FRenderBackBuffer.Canvas.CopyRect(FRenderBackBuffer.Canvas.ClipRect, FOutputBitmap.Canvas, FOutputBitmap.Canvas.ClipRect);

      FOutputBitmap.Canvas.Pen.Color := clWhite;
      FOutputBitmap.Canvas.Pen.Style := psSolid;
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
            TMItem := Frame.TileMap[sy, sx];

            if TMItem.IsPredicted and FRenderPredicted then
            begin
              for ty := 0 to cTileWidth - 1 do
              begin
                posl := FOutputBitmap.ScanLine[(sy shl cTileWidthBits) + ty];
                Inc(posl, sx shl cTileWidthBits);
                pbsl := FRenderBackBuffer.ScanLine[(sy shl cTileWidthBits) + TMItem.PredictedY + ty];
                Inc(pbsl, (sx shl cTileWidthBits) + TMItem.PredictedX);

                Move(pbsl^, posl^, cTileWidth * SizeOf(Integer));
              end;
            end
            else if InRange(TMItem.TileIdx, 0, High(Tiles)) then
            begin
              tilePtr := Tiles[TMItem.TileIdx];

              pal := nil;
              if FRenderOutputDithered then
                if FRenderPaletteIndex < 0 then
                begin
                  if not InRange(TMItem.PalIdx, 0, High(FPalettes)) then
                    Continue;
                  pal := FPalettes[TMItem.PalIdx].PaletteRGB;
                end
                else
                begin
                  if FRenderPaletteIndex <> TMItem.PalIdx then
                    Continue;
                  pal := FPalettes[FRenderPaletteIndex].PaletteRGB;
                end;

              if not FRenderMirrored then
              begin
                TMItem.HMirror := False;
                TMItem.VMirror := False;
              end;

              DrawTile(FOutputBitmap, sx, sy, nil, tilePtr, pal, TMItem.HMirror, TMItem.VMirror, False);
            end;
          end;
      finally
        FOutputBitmap.EndUpdate;
      end;

      if not FRenderPredicted then
      begin
       for sy := 0 to FTileMapHeight - 1 do
         for sx := 0 to FTileMapWidth - 1 do
         begin
           TMItem := Frame.TileMap[sy, sx];

           if TMItem.IsPredicted then
             FOutputBitmap.Canvas.Line(
                 (sx shl cTileWidthBits) + cTileWidth div 2, (sy shl cTileWidthBits) + cTileWidth div 2,
                 (sx shl cTileWidthBits) + TMItem.PredictedX + cTileWidth div 2, (sy shl cTileWidthBits) + TMItem.PredictedY + cTileWidth div 2
             );
         end;
      end;
    end;

    // "FPalettes / Tiles" tab

    if FRenderPage = rpTilesPalette then
    begin
      FPaletteBitmap.BeginUpdate;
      try
        for j := 0 to FPaletteBitmap.Height - 1 do
        begin
          p := FPaletteBitmap.ScanLine[j];
          for i := 0 to FPaletteBitmap.Width - 1 do
          begin
            if Assigned(FPalettes) and Assigned(FPalettes[j].PaletteRGB) then
              p^ := SwapRB(FPalettes[j].PaletteRGB[i])
            else
              p^ := clFuchsia;

            Inc(p);
          end;
        end;
      finally
        FPaletteBitmap.EndUpdate;
      end;

      FTilesBitmap.Canvas.Brush.Color := clAqua;
      FTilesBitmap.Canvas.Brush.Style := bsSolid;
      FTilesBitmap.Canvas.FillRect(FTilesBitmap.Canvas.ClipRect);

      FTilesBitmap.BeginUpdate;
      try
        for sy := 0 to FTileMapHeight - 1 do
          for sx := 0 to FTileMapWidth - 1 do
          begin
            tidx := FTileMapWidth * sy + sx + FTileMapSize * FRenderTilePage;

            if InRange(tidx, 0, High(Tiles)) then
            begin
              tilePtr := Tiles[tidx];
              pal := nil;
              if FRenderOutputDithered and (Length(FPalettes) > 0) then
                pal := FPalettes[IfThen(FRenderPaletteIndex < 0, Max(0, tilePtr^.PalIdx_Initial), FRenderPaletteIndex)].PaletteRGB;

              hmir := tilePtr^.HMirror_Initial;
              vmir := tilePtr^.VMirror_Initial;

              if not FRenderMirrored then
              begin
                hmir := False;
                vmir := False;
              end;

              DrawTile(FTilesBitmap, sx, sy, nil, tilePtr, pal, hmir, vmir, False);
            end;
          end;
      finally
        FTilesBitmap.EndUpdate;
      end;
    end;

    // ResidualErr indicator

    q := 0.0;
    i := 0;
    for sy := 0 to FTileMapHeight - 1 do
      for sx := 0 to FTileMapWidth - 1 do
      begin
        TMItem := Frame.TileMap[sy, sx];

        if not IsInfinite(TMItem.ResidualErr) then
        begin
          q += TMItem.ResidualErr;
          Inc(i);
        end;
      end;
    if i <> 0 then
      q /= i;

    FRenderPsychoVisualQuality := Sqrt(q);

  finally
    FRenderPrevFrameIndex := FRenderFrameIndex;
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

    ini.WriteInteger('MotionPredict', 'MotionPredictRadius', MotionPredictRadius);

    ini.WriteFloat('GlobalTiling', 'GlobalTilingQualityBasedTileCount', GlobalTilingQualityBasedTileCount);
    ini.WriteInteger('GlobalTiling', 'GlobalTilingTileCount', GlobalTilingTileCount);

    ini.WriteInteger('Dither', 'PaletteSize', PaletteSize);
    ini.WriteInteger('Dither', 'PaletteCount', PaletteCount);
    ini.WriteBool('Dither', 'DitheringUseGamma', DitheringUseGamma);
    ini.WriteInteger('Dither', 'DitheringMode', Ord(DitheringMode));
    ini.WriteBool('Dither', 'DitheringUseThomasKnoll', DitheringUseThomasKnoll);
    ini.WriteInteger('Dither', 'DitheringYliluoma2MixedColors', DitheringYliluoma2MixedColors);

    ini.WriteBool('FrameTiling', 'FrameTilingUseGamma', FrameTilingUseGamma);
    ini.WriteInteger('FrameTiling', 'FrameTilingMode', Ord(FrameTilingMode));
    ini.WriteBool('FrameTiling', 'FrameTilingExtendedPaletteUsage', FrameTilingExtendedPaletteUsage);

    ini.WriteFloat('Misc', 'EncoderGammaValue', EncoderGammaValue);
    ini.WriteInteger('Misc', 'MaxThreadCount', MaxThreadCount);

    ini.WriteFloat('Load', 'ShotTransMaxSecondsPerKF', ShotTransMaxSecondsPerKF);
    ini.WriteFloat('Load', 'ShotTransMinSecondsPerKF', ShotTransMinSecondsPerKF);
    ini.WriteFloat('Load', 'ShotTransCorrelLoThres', ShotTransCorrelLoThres);
    ini.WriteFloat('Load', 'ShotTransDistHiThres', ShotTransDistHiThres);

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

    MotionPredictRadius := ini.ReadInteger('MotionPredict', 'MotionPredictRadius', MotionPredictRadius);

    GlobalTilingQualityBasedTileCount := ini.ReadFloat('GlobalTiling', 'GlobalTilingQualityBasedTileCount', GlobalTilingQualityBasedTileCount);
    GlobalTilingTileCount := ini.ReadInteger('GlobalTiling', 'GlobalTilingTileCount', GlobalTilingTileCount); // after GlobalTilingQualityBasedTileCount because has priority

    PaletteSize := ini.ReadInteger('Dither', 'PaletteSize', PaletteSize);
    PaletteCount := ini.ReadInteger('Dither', 'PaletteCount', PaletteCount);
    DitheringUseGamma := ini.ReadBool('Dither', 'DitheringUseGamma', DitheringUseGamma);
    DitheringMode := TPsyVisMode(EnsureRange(ini.ReadInteger('Dither', 'DitheringMode', Ord(DitheringMode)), Ord(Low(TPsyVisMode)), Ord(High(TPsyVisMode))));
    DitheringUseThomasKnoll := ini.ReadBool('Dither', 'DitheringUseThomasKnoll', DitheringUseThomasKnoll);
    DitheringYliluoma2MixedColors := ini.ReadInteger('Dither', 'DitheringYliluoma2MixedColors', DitheringYliluoma2MixedColors);

    FrameTilingUseGamma := ini.ReadBool('FrameTiling', 'FrameTilingUseGamma', FrameTilingUseGamma);
    FrameTilingMode := TPsyVisMode(EnsureRange(ini.ReadInteger('FrameTiling', 'FrameTilingMode', Ord(FrameTilingMode)), Ord(Low(TPsyVisMode)), Ord(High(TPsyVisMode))));
    FrameTilingExtendedPaletteUsage := ini.ReadBool('FrameTiling', 'FrameTilingExtendedPaletteUsage', FrameTilingExtendedPaletteUsage);

    EncoderGammaValue := ini.ReadFloat('Misc', 'EncoderGammaValue', EncoderGammaValue);
    MaxThreadCount := ini.ReadInteger('Misc', 'MaxThreadCount', MaxThreadCount);

    ShotTransMaxSecondsPerKF := ini.ReadFloat('Load', 'ShotTransMaxSecondsPerKF', ShotTransMaxSecondsPerKF);
    ShotTransMinSecondsPerKF := ini.ReadFloat('Load', 'ShotTransMinSecondsPerKF', ShotTransMinSecondsPerKF);
    ShotTransCorrelLoThres := ini.ReadFloat('Load', 'ShotTransCorrelLoThres', ShotTransCorrelLoThres);
    ShotTransDistHiThres := ini.ReadFloat('Load', 'ShotTransDistHiThres', ShotTransDistHiThres);

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
  MaxThreadCount := NumberOfProcessors;

  PaletteSize := 16;
  PaletteCount := 1024;

  MotionPredictRadius := 32;

  GlobalTilingQualityBasedTileCount := 7.0;
  GlobalTilingTileCount := 0; // after GlobalTilingQualityBasedTileCount because has priority

  DitheringUseGamma := False;
  DitheringMode := pvsWeightedSpeDCT;
  DitheringUseThomasKnoll := True;
  DitheringYliluoma2MixedColors := 4;

  FrameTilingUseGamma := False;
  FrameTilingMode := pvsSpeDCT;
  FrameTilingExtendedPaletteUsage := False;

  EncoderGammaValue := 2.0;

  ShotTransMaxSecondsPerKF := 15.0;  // maximum seconds between keyframes
  ShotTransMinSecondsPerKF := 1.0;  // minimum seconds between keyframes
  ShotTransCorrelLoThres := 0.8;   // interframe pearson correlation low limit
  ShotTransDistHiThres := 3.0;   // interframe distance high limit
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

  ComputeTilePsyVisFeatures(T^, pvsDCT, False, False, False, False, cColorCpns, -1, nil, @DCT[0]);
  ComputeInvTilePsyVisFeatures(@DCT[0], pvsDCT, False, cColorCpns, -1, T2^);

  //for i := 0 to 7 do
  //  for j := 0 to 7 do
  //    write(IntToHex(T^.RGBPixels[i, j], 6), '  ');
  //WriteLn();
  //for i := 0 to 7 do
  //  for j := 0 to 7 do
  //    write(IntToHex(T2^.RGBPixels[i, j], 6), '  ');
  //WriteLn();

  Assert(CompareMem(T^.GetRGBPixelsPtr, T2^.GetRGBPixelsPtr, SizeOf(TRGBPixels)), 'DCT/InvDCT mismatch');

  ComputeTilePsyVisFeatures(T^, pvsWeightedDCT, False, False, False, False, cColorCpns, -1, nil, @DCT[0]);
  ComputeInvTilePsyVisFeatures(@DCT[0], pvsWeightedDCT, False, cColorCpns, -1, T2^);

  Assert(CompareMem(T^.GetRGBPixelsPtr, T2^.GetRGBPixelsPtr, SizeOf(TRGBPixels)), 'QWeighted DCT/InvDCT mismatch');

  ComputeTilePsyVisFeatures(T^, pvsWavelets, False, False, False, False, cColorCpns, -1, nil, @DCT[0]);
  ComputeInvTilePsyVisFeatures(@DCT[0], pvsWavelets, False, cColorCpns, -1, T2^);

  Assert(CompareMem(T^.GetRGBPixelsPtr, T2^.GetRGBPixelsPtr, SizeOf(TRGBPixels)), 'WL/InvWL mismatch');

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


function TTilingEncoder.STCGREval(x: Double; Data: Pointer): Double;
var
  frmIdx, sy, sx, unpredictedTileCount: Integer;
  TMI: PTileMapItem;
begin
  x *= x;
  unpredictedTileCount := 0;
  for frmIdx := 0 to High(FFrames) do
    for sy := 0 to FTileMapHeight - 1 do
      for sx := 0 to FTileMapWidth - 1 do
      begin
        TMI := @FFrames[frmIdx].TileMap[sy, sx];

        TMI^.IsPredicted := TMI^.ResidualErr < x;

        inc(unpredictedTileCount, Ord(not TMI^.IsPredicted));
      end;

  TransferTiles(unpredictedTileCount);

  MakeTilesUnique(True);

  Result := GetTileCount(True);
end;

function TTilingEncoder.SolveTileCount(ATileCount: Integer): Double;
begin
  Result := GoldenRatioSearch(@STCGREval, cTileDCTSize, 0.0, ATileCount, 1e-6, 0.5, nil);
end;

procedure TTilingEncoder.TransferTiles(ATileCount: Integer);
var
  doneFrameCount: Integer;
  newTIdx: Integer;

  procedure DoTransfer(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    tIdx, sx, sy: Integer;
    Frame: TFrame;
    Tile: PTile;
    TMI: PTileMapItem;
  begin
    if not InRange(AIndex, 0, High(FFrames)) then
      Exit;

    Frame := FFrames[AIndex];

    Frame.AcquireFrameTiles;
    try
      for sy := 0 to FTileMapHeight - 1 do
        for sx := 0 to FTileMapWidth - 1 do
        begin
          TMI := @Frame.TileMap[sy, sx];

          if not TMI^.IsPredicted then
          begin
            tIdx := InterLockedIncrement(newTIdx);

            Tile := Tiles[tIdx];
            Tile^.CopyFrom(Frame.FrameTiles[sy * FTileMapWidth + sx]^);

            TMI^.TileIdx := tIdx;
          end
          else
          begin
            TMI^.TileIdx := -1;
          end;
        end;

      Write(InterLockedIncrement(doneFrameCount):8, ' / ', Length(FFrames):8, #13);

    finally
      Frame.ReleaseFrameTiles;
    end;
  end;

begin
  TTile.Array1DDispose(FTiles);
  FTiles := TTile.Array1DNew(ATileCount, True, True);

  doneFrameCount := 0;
  newTIdx := -1;
  ProcThreadPool.DoParallelLocalProc(@DoTransfer, 0, High(FFrames));

  Assert(newTIdx = ATileCount - 1);
end;

procedure TTilingEncoder.DoPalettization(ADitheringGamma: Integer);
const
  cFeatureCount = cTileDCTSize;
var
  BICO: PBICO;
  ANN: PANNkdtree;
  ANNClusters: TIntegerDynArray;

  procedure DoDataset(ACluster: Boolean);
  var
    tIdx: Integer;
    Tile: PTile;
    DCT: array[0 .. cTileDCTSize - 1] of Double;
    ANNError: Double;
  begin
    for tIdx := 0 to High(FTiles) do
    begin
      Tile := FTiles[tIdx];
      Assert(Tile^.Active);

      ComputeTilePsyVisFeatures(Tile^, DitheringMode, False, True, False, False, cColorCpns, ADitheringGamma, nil, DCT);

      if ACluster then
        ANNClusters[tIdx] := ann_kdtree_search(ANN, DCT, 0.0, @ANNError)
      else
        bico_insert_line(BICO, DCT, Tile^.UseCount);
    end;
  end;

var
  DSLen, tIdx, di, palIdx, BICOClusterCount: Integer;

  Tile: PTile;

  Yakmo: PYakmo;

  BICOCentroids, BICOWeights: TDoubleDynArray;

  ANNDataset: array of PDouble;
  YakmoClusters: TIntegerDynArray;
  PalIdxLUT: TIntegerDynArray;

begin
  // build dataset

  DSLen := Length(FTiles);
  BICOClusterCount := FPaletteCount shl 3;

  BICO := bico_create(cFeatureCount, DSLen, BICOClusterCount, 32, BICOClusterCount, CRandomSeed);
  try
    DoDataset(False);

    SetLength(BICOCentroids, BICOClusterCount * cFeatureCount);
    SetLength(BICOWeights, BICOClusterCount);

    BICOClusterCount := bico_get_results(BICO, @BICOCentroids[0], @BICOWeights[0]);

    WriteLn('BICOClusterCount: ', BICOClusterCount:6);
  finally
    bico_destroy(BICO);
  end;

  // use ANN to compute cluster indexes

  SetLength(ANNDataset, BICOClusterCount);
  for di := 0 to High(ANNDataset) do
    ANNDataset[di] := @BICOCentroids[di * cFeatureCount];

  SetLength(ANNClusters, DSLen);

  ANN := ann_kdtree_create(@ANNDataset[0], BICOClusterCount, cFeatureCount, 32, ANN_KD_STD);
  try
    DoDataset(True);
  finally
    ann_kdtree_destroy(ANN);
  end;

  // cluster by palette index

  if BICOClusterCount > FPaletteCount then
  begin
    if FPaletteCount > 1 then
    begin
      SetLength(YakmoClusters, BICOClusterCount);

      Yakmo := yakmo_create(FPaletteCount, 1, cYakmoMaxIterations, 1, 0, 0, 0);
      yakmo_load_train_data(Yakmo, Length(ANNDataset), cFeatureCount, PPDouble(@ANNDataset[0]));
      SetLength(ANNDataset, 0); // free up some memmory
      yakmo_train_on_data(Yakmo, @YakmoClusters[0]);
      yakmo_destroy(Yakmo);
    end
    else
    begin
      SetLength(YakmoClusters, BICOClusterCount);
    end;
  end
  else
  begin
    SetLength(YakmoClusters, BICOClusterCount);
    for di := 0 to High(YakmoClusters) do
      YakmoClusters[di] := di;
  end;

  // sort entire FPalettes by use count

  SetLength(FPalettes, FPaletteCount);
  SetLength(PalIdxLUT, FPaletteCount);

  for palIdx := 0 to FPaletteCount - 1 do
    FPalettes[palIdx].PalIdx_Initial := palIdx;

  for di := 0 to High(ANNClusters) do
    Inc(FPalettes[YakmoClusters[ANNClusters[di]]].UseCount);

  QuickSort(FPalettes[0], 0, FPaletteCount - 1, SizeOf(FPalettes[0]), @ComparePaletteUseCount, Self);
  for palIdx := 0 to FPaletteCount - 1 do
    PalIdxLUT[FPalettes[palIdx].PalIdx_Initial] := palIdx;

  // assign final palette indexes

  for tIdx := 0 to High(FTiles) do
  begin
    Tile := FTiles[tIdx];
    Assert(Tile^.Active);

    Tile^.PalIdx_Initial := PalIdxLUT[YakmoClusters[ANNClusters[tIdx]]];;
  end;
end;

procedure TTilingEncoder.OptimizePalettes;
var
  Bests: TDoubleDynArray;
  BestsPalIdx, BestsColIdx1, BestsColIdx2: TIntegerDynArray;

  procedure DoPal(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    i, j, colIdx1, colIdx2, uc: Integer;
    r, g, b: byte;
    best, v: Double;
    bestPalIdx, bestColIdx1, bestColIdx2: Integer;

    InnerPerm: array[0 .. Sqr(cTileWidth) - 1] of Integer;
    PalR, PalG, PalB, InnerPalR, InnerPalG, InnerPalB: array[0 .. Sqr(cTileWidth) - 1] of Double;
  begin
    if not InRange(AIndex, 0, FPaletteCount - 1) then
      Exit;

    // accumulate the whole palette except the one that will be permutated

    FillQWord(PalR[0], FPaletteSize, 0);
    FillQWord(PalG[0], FPaletteSize, 0);
    FillQWord(PalB[0], FPaletteSize, 0);
    for i := 0 to FPaletteCount - 1 do
    begin
      uc := FPalettes[i].UseCount;
      for j := 0 to FPaletteSize - 1 do
        if i <> AIndex then
        begin
          FromRGB(FPalettes[i].PaletteRGB[j], r, g, b);
          PalR[j] += r * uc;
          PalG[j] += g * uc;
          PalB[j] += b * uc;
        end;
    end;

    // try all permutations in the current palette

    best := 0;
    bestPalIdx := -1;
    bestColIdx1 := -1;
    bestColIdx2 := -1;

    for colIdx1 := 0 to FPaletteSize - 1 do
      for colIdx2 := colIdx1 + 1 to FPaletteSize - 1 do
      begin
        for i := 0 to FPaletteSize - 1 do
          InnerPerm[i] := i;

        Exchange(InnerPerm[colIdx1], InnerPerm[colIdx2]);

        Move(PalR[0], InnerPalR[0], FPaletteSize * SizeOf(Double));
        Move(PalG[0], InnerPalG[0], FPaletteSize * SizeOf(Double));
        Move(PalB[0], InnerPalB[0], FPaletteSize * SizeOf(Double));

        uc := FPalettes[AIndex].UseCount;
        for i := 0 to FPaletteSize - 1 do
        begin
          FromRGB(FPalettes[AIndex].PaletteRGB[InnerPerm[i]], r, g, b);
          InnerPalR[i] += r * uc;
          InnerPalG[i] += g * uc;
          InnerPalB[i] += b * uc;
        end;

        // try to maximize accumulated palette standard deviation
        // rationale: the less samey it is, the better the colors pair with each other across FPalettes

        v := cRedMul * StdDev(InnerPalR, FPaletteSize) + cGreenMul * StdDev(InnerPalG, FPaletteSize) + cBlueMul * StdDev(InnerPalB, FPaletteSize);

        if v > best then
        begin
          best := v;
          bestPalIdx := AIndex;
          bestColIdx1 := colIdx1;
          bestColIdx2 := colIdx2;
        end;
      end;

    Bests[AIndex] := best;
    BestsPalIdx[AIndex] := bestPalIdx;
    BestsColIdx1[AIndex] := bestColIdx1;
    BestsColIdx2[AIndex] := bestColIdx2;
  end;

var
  iteration, palIdx: Integer;
  best, prevBest: Double;
  bestPalIdx, bestColIdx1, bestColIdx2: Integer;
begin
  SetLength(Bests, FPaletteCount);
  SetLength(BestsPalIdx, FPaletteCount);
  SetLength(BestsColIdx1, FPaletteCount);
  SetLength(BestsColIdx2, FPaletteCount);

  // stepwise algorithm on palette colors permutations

  best := 0;
  iteration := 0;
  repeat
    prevBest := best;

    ProcThreadPool.DoParallelLocalProc(@DoPal, 0, FPaletteCount - 1, nil);

    best := 0;
    bestPalIdx := -1;
    bestColIdx1 := -1;
    bestColIdx2 := -1;
    for palIdx := 0 to FPaletteCount - 1 do
      if Bests[palIdx] > best then
      begin
        best := Bests[palIdx];
        bestPalIdx := BestsPalIdx[palIdx];
        bestColIdx1 := BestsColIdx1[palIdx];
        bestColIdx2 := BestsColIdx2[palIdx];
      end;

    if (best > prevBest) and (bestPalIdx >= 0) and (bestColIdx1 >= 0) and (bestColIdx2 >= 0) then
      Exchange(FPalettes[bestPalIdx].PaletteRGB[bestColIdx1], FPalettes[bestPalIdx].PaletteRGB[bestColIdx2]);

    Inc(iteration);

    //WriteLn(iteration:3, bestPalIdx:3, bestColIdx1:3, bestColIdx2:3, best:12:0);

  until best <= prevBest;

  WriteLn('OptimizePalettes: ', iteration, ' iterations');
end;

procedure TTilingEncoder.QuantizeUsingYakmo(APalIdx, AColorCount, APosterize: Integer);
const
  cFeatureCount = 3;
var
  i, j, di, ty, tx, tIdx, DSLen: Integer;
  rr, gg, bb: Byte;
  Tile: PTile;
  Dataset, Centroids: TDoubleDynArray2;
  Clusters: TIntegerDynArray;
  Yakmo: PYakmo;
  CMPal: TCountIndexList;
  CMItem: PCountIndex;
begin
  CMPal := FPalettes[APalIdx].CMPal;

  for i := 0 to CMPal.Count - 1 do
    Dispose(CMPal[i]);
  CMPal.Clear;

  DSLen := 0;
  for tIdx := 0 to High(FTiles) do
    Inc(DSLen, sqr(cTileWidth) * Ord(FTiles[tIdx]^.PalIdx_Initial = APalIdx));

  if DSLen <= 0 then
    Exit;

  SetLength(Dataset, DSLen, cFeatureCount);
  SetLength(Clusters, DSLen);
  SetLength(Centroids, AColorCount, cFeatureCount);

  AColorCount := Min(AColorCount, DSLen);

  // build a dataset of RGB pixels

  di := 0;
  for tIdx := 0 to High(FTiles) do
  begin
    Tile := FTiles[tIdx];

    if Tile^.Active and (Tile^.PalIdx_Initial = APalIdx) then
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

  QuickSort(Dataset[0], 0, di - 1, SizeOf(Pointer), @CompareDSPixel);

  // use KMeans to quantize to AColorCount elements

  if AColorCount > 1 then
  begin
    Yakmo := yakmo_create(AColorCount, 1, cYakmoMaxIterations, 1, 0, 0, 0);
    yakmo_load_train_data(Yakmo, DSLen, cFeatureCount, PPDouble(@Dataset[0]));

    //SetLength(Dataset, 0); // free up some memmory

    yakmo_train_on_data(Yakmo, @Clusters[0]);
    yakmo_get_centroids(Yakmo, PPDouble(@Centroids[0]));
    yakmo_destroy(Yakmo);
  end
  else
  begin
    for j := 0 to DSLen - 1 do
      for i := 0 to cFeatureCount - 1 do
        Centroids[0, i] += Dataset[j, i];
    for i := 0 to cFeatureCount - 1 do
      Centroids[0, i] /= di;
  end;

  // retrieve palette data

  for i := 0 to AColorCount - 1 do
  begin
    New(CMItem);

    CMItem^.R := 0;
    CMItem^.G := 0;
    CMItem^.B := 0;

    if not IsNan(Centroids[i, 0]) and not IsNan(Centroids[i, 1]) and not IsNan(Centroids[i, 2]) then
    begin
      CMItem^.R := Posterize(EnsureRange(Round(Centroids[i, 0]), 0, 255), APosterize);
      CMItem^.G := Posterize(EnsureRange(Round(Centroids[i, 1]), 0, 255), APosterize);
      CMItem^.B := Posterize(EnsureRange(Round(Centroids[i, 2]), 0, 255), APosterize);
    end;

    CMItem^.Count := 0;
    RGBToHSV(ToRGB(CMItem^.R, CMItem^.G, CMItem^.B), CMItem^.Hue, CMItem^.Sat, CMItem^.Val);
    CMPal.Add(CMItem);
  end;
end;

procedure TTilingEncoder.DoQuantization(APalIdx: Integer; ADitheringGamma: Integer);
var
  CMPal: TCountIndexList;
  i: Integer;
begin
  CMPal := TCountIndexList.Create;
  FPalettes[APalIdx].CMPal := CMPal;
  try
    // do quantize

    QuantizeUsingYakmo(APalIdx, FPaletteSize, 1 shl cBitsPerComp);

    // split most used colors into tile palettes

    CMPal.Sort(@CompareCountIndexVSH);

    SetLength(FPalettes[APalIdx].PaletteRGB, FPaletteSize);
    for i := 0 to CMPal.Count - 1 do
    begin
      FPalettes[APalIdx].PaletteRGB[i] := ToRGB(CMPal[i]^.R, CMPal[i]^.G, CMPal[i]^.B);
      Dispose(CMPal[i]);
    end;

    for i := CMPal.Count to FPaletteSize - 1 do
      FPalettes[APalIdx].PaletteRGB[i] := cDitheringNullColor;

  finally
    CMPal.Free;
    FPalettes[APalIdx].CMPal := nil;
  end;
end;

procedure TTilingEncoder.PrepareReconstruct(AFTGamma: Integer);
var
  DS: PTilingDataset;

  procedure DoPsyV(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    T: PTile;
  begin
    if not InRange(AIndex, 0, High(FTiles)) then
      Exit;

    T := Tiles[AIndex];
    Assert(T^.Active);

    ComputeTilePsyVisFeatures(T^, FrameTilingMode, True, False, False, False, cColorCpns, AFTGamma, FPalettes[T^.PalIdx_Initial].PaletteRGB, PSingle(@DS^.PalDataset[AIndex, 0]));

    if FFrameTilingExtendedPaletteUsage then
      T^.ExtractPalPixels(PSingle(@DS^.Dataset[AIndex, 0]));
  end;

var
  kfIdx: Integer;
begin
  // Compute psycho visual model for all tiles in all palettes

  DS := New(PTilingDataset);
  FillChar(DS^, SizeOf(TTilingDataset), 0);

  DS^.KNNSize := Length(FTiles);
  SetLength(DS^.PalDataset, DS^.KNNSize, cTileDCTSize);
  if FFrameTilingExtendedPaletteUsage then
    SetLength(DS^.Dataset, DS^.KNNSize, sqr(cTileWidth));

  ProcThreadPool.DoParallelLocalProc(@DoPsyV, 0, High(FTiles));

  // Build KNN

  DS^.PalANN := ann_kdtree_single_create(PPSingle(@DS^.PalDataset[0]), DS^.KNNSize, cTileDCTSize, 32, ANN_KD_STD);
  if FFrameTilingExtendedPaletteUsage then
    DS^.ANN := ann_kdtree_single_create(PPSingle(@DS^.Dataset[0]), DS^.KNNSize, sqr(cTileWidth), 32, ANN_KD_STD);

  // Dataset is ready

  FTileDS := DS;

  // init for LogResidualErr

  for kfIdx := 0 to High(FKeyFrames) do
  begin
    FKeyFrames[kfIdx].ReconstructErrCml := 0;
    FKeyFrames[kfIdx].ReconstructFramesLeft := FKeyFrames[kfIdx].FrameCount;
  end;
end;

procedure TTilingEncoder.FinishReconstruct;
begin
  if Length(FTileDS^.Dataset) > 0 then
  begin
    ann_kdtree_single_destroy(FTileDS^.PalANN);
    if Assigned(FTileDS^.PalANN) then
      ann_kdtree_single_destroy(FTileDS^.ANN);
  end;
  FTileDS^.ANN := nil;
  FTileDS^.PalANN := nil;
  SetLength(FTileDS^.Dataset, 0);
  SetLength(FTileDS^.PalDataset, 0);
  Dispose(FTileDS);

  FTileDS := nil;
end;

procedure TTilingEncoder.ReindexTiles(OnRGBPixels: Boolean);
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

  LocTiles := TTile.Array1DNew(cnt, True, True);
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

  QuickSort(Tiles[0], 0, High(Tiles), SizeOf(PTile), @CompareTileUseCountRev, Pointer(Ord(OnRGBPixels)));

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

  WriteLn('ReindexTiles: ', Length(Tiles):12, ' / ', Length(FFrames) * FTileMapSize:12,  ' final tiles, (', Length(Tiles) * 100.0 / (Length(FFrames) * FTileMapSize):4:3, '%)');
end;

function CompareTilePalPixels(Item1, Item2:Pointer):Integer;
var
  t1, t2: PTile;
begin
  t1 := PTile(Item1);
  t2 := PTile(Item2);
  Result := t1^.ComparePalPixelsTo(t2^);
end;

function CompareTileRGBPixels(Item1, Item2:Pointer):Integer;
var
  t1, t2: PTile;
begin
  t1 := PTile(Item1);
  t2 := PTile(Item2);
  Result := t1^.CompareRGBPixelsTo(t2^);
end;

procedure TTilingEncoder.MakeTilesUnique(OnRGBPixels: Boolean);
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

var
  PixelLSC: TListSortCompare;
begin
  PixelLSC := @CompareTilePalPixels;
  if OnRGBPixels then
    PixelLSC := @CompareTileRGBPixels;

  InitMergeTiles;
  sortList := TFPList.Create;
  try

    // sort global tiles by palette indexes (L to R, T to B)

    SetLength(sameIdx, Length(FTiles));

    sortList.Count := Length(FTiles);
    pos := 0;
    for i := 0 to High(FTiles) do
      if FTiles[i]^.Active then
      begin
        sortList[pos] := FTiles[i];
        PTile(sortList[pos])^.TmpIndex := i;
        Inc(pos);
      end;
    sortList.Count := pos;

    sortList.Sort(PixelLSC);

    // merge exactly similar tiles (so, consecutive after prev code)

    firstSameIdx := 0;
    for i := 1 to sortList.Count - 1 do
      if PixelLSC(sortList[i - 1], sortList[i]) <> 0 then
        DoOneMerge;

    i := sortList.Count;
    DoOneMerge;

  finally
    sortList.Free;
    FinishMergeTiles;
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
        tidx := FFrames[frmIdx].TileMap[sy, sx].TileIdx;
        if tidx >= 0 then
        begin
          tidx := FTiles[tidx]^.MergeIndex;
          if tidx >= 0 then
            FFrames[frmIdx].TileMap[sy, sx].TileIdx := tidx;
        end;
      end;
end;

class function TTilingEncoder.GetTileZoneSum(const ATile: TTile; AOnPal: Boolean; x, y, w, h: Integer): Integer;
var
  i, j: Integer;
  r, g, b: Byte;
begin
  Result := 0;
  if AOnPal then
  begin
   for j := y to y + h - 1 do
     for i := x to x + w - 1 do
       Result += ATile.PalPixels[j, i];
  end
  else
  begin
    for j := y to y + h - 1 do
      for i := x to x + w - 1 do
      begin
        FromRGB(ATile.RGBPixels[j, i], r, g, b);
        Result += ToLuma(r, g, b);
      end;
  end;
end;

class procedure TTilingEncoder.GetTileHVMirrorHeuristics(const ATile: TTile; AOnPal: Boolean; out AHMirror, AVMirror: Boolean);
var
  q00, q01, q10, q11: Integer;
begin
  // enforce an heuristical 'spin' on tiles mirrors (brighter top-left corner)

  q00 := GetTileZoneSum(ATile, AOnPal, 0, 0, cTileWidth div 2, cTileWidth div 2);
  q01 := GetTileZoneSum(ATile, AOnPal, cTileWidth div 2, 0, cTileWidth div 2, cTileWidth div 2);
  q10 := GetTileZoneSum(ATile, AOnPal, 0, cTileWidth div 2, cTileWidth div 2, cTileWidth div 2);
  q11 := GetTileZoneSum(ATile, AOnPal, cTileWidth div 2, cTileWidth div 2, cTileWidth div 2, cTileWidth div 2);

  AHMirror := q00 + q10 < q01 + q11;
  AVMirror := q00 + q01 < q10 + q11;
end;

procedure TTilingEncoder.LoadStream(AStream: TStream);
var
  KFStream: TMemoryStream;
  frmIdx, lastTileIdx: Integer;

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

    lastTileIdx := max(lastTileIdx, endIdx);
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

  procedure ReadPalette;
  var
    i, palIdx: Integer;
  begin
    palIdx := ReadWord;

    if Length(FPalettes) <= palIdx then
    begin
      SetLength(FPalettes, palIdx + 1);
      for i := 0 to palIdx do
        SetLength(FPalettes[i].PaletteRGB, FPaletteSize);

      FPaletteCount := Length(FPalettes);
    end;

    for i := 0 to FPaletteSize - 1 do
      FPalettes[palIdx].PaletteRGB[i] := ReadDWord and $ffffff;
  end;

  procedure SetTMI(tileIdx, palIdx: Integer; attrs: Integer; var TMI: TTileMapItem);
  begin
    TMI.TileIdx := tileIdx;
    TMI.PalIdx := palIdx;
    TMI.HMirror := attrs and 1 <> 0;
    TMI.VMirror := attrs and 2 <> 0;

    TMI.IsPredicted := False;

    Inc(FTiles[tileIdx]^.UseCount);
  end;

  function NextFrame(KF: TKeyFrame): TFrame;
  begin
    Inc(frmIdx);
    Result := FFrames[frmIdx];
    Result.PKeyFrame := kf;
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
      frm.TileMap[sy, sx].IsPredicted := True;
      frm.TileMap[sy, sx].PredictedX := 0;
      frm.TileMap[sy, sx].PredictedY := 0;
    end;
    tmPos += SkipCount;
  end;

var
  Header: TGTMHeader;
  Command, prevCommand: TGTMCommand;
  CommandData: Word;
  loadedFrmCount, tmPos: Integer;
  tileIdx: Cardinal;
  palIdx: Word;
  frm: TFrame;
  kf: TKeyFrame;
  compat: String;
  TMI: PTileMapItem;
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

  ClearAll(True);

  frm := nil;
  frmIdx := -1;
  lastTileIdx := -1;
  loadedFrmCount := 0;
  KFStream := TMemoryStream.Create;
  try
    repeat
      KFStream.Clear;
      LZDecompress(AStream, KFStream);
      KFStream.Seek(0,soBeginning);

      // add a keyframe
      SetLength(FKeyFrames, Length(FKeyFrames) + 1);
      kf := TKeyFrame.Create(Self, High(FKeyFrames), loadedFrmCount, -1);
      FKeyFrames[High(FKeyFrames)] := kf;

      prevCommand := gtExtendedCommand;
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
            ReadPalette;
          end;
          gtFrameEnd:
          begin
            Assert(tmPos = FTileMapSize, 'Incomplete tilemap');

            // will load to create a new frame
            frm := nil;
            tmPos := 0;
            Inc(loadedFrmCount);

            if (CommandData and 1) <> 0 then // keyframe end?
              Break;
          end;
          gtSkipBlock:
          begin
            // next frame if needed
            if frm = nil then
              frm := NextFrame(kf);

            SkipBlock(frm, CommandData + 1, tmPos);
          end;
          gtShortTileIdxShortPalIdx, gtLongTileIdxShortPalIdx, gtLongTileIdxLongPalIdx:
          begin
            if Command in [gtLongTileIdxLongPalIdx] then
              palIdx := ReadWord
            else
              palIdx := (CommandData shr 2) and ((1 shl (CGTMCommandBits - 2)) - 1);

            if Command in [gtShortTileIdxShortPalIdx] then
              tileIdx := ReadWord
            else
              tileIdx := ReadDWord;

            // next frame if needed
            if frm = nil then
              frm := NextFrame(kf);

            SetTMI(tileIdx, palIdx, CommandData, frm.TileMap[tmPos div FTileMapWidth, tmPos mod FTileMapWidth]);
            Inc(tmPos);
          end;
          gtPredictedTileShortOffsets:
          begin
            // next frame if needed
            if frm = nil then
              frm := NextFrame(kf);

            TMI := @frm.TileMap[tmPos div FTileMapWidth, tmPos mod FTileMapWidth];

            TMI^.PredictedX := (CommandData and 31) - (CommandData and 32);
            TMI^.PredictedY := ((CommandData shr 6) and 31) - ((CommandData shr 6) and 32);
            TMI^.IsPredicted := True;

            Inc(tmPos);
          end;
          gtPredictedTileLongOffsets:
          begin
            // next frame if needed
            if frm = nil then
              frm := NextFrame(kf);

            TMI := @frm.TileMap[tmPos div FTileMapWidth, tmPos mod FTileMapWidth];

            TMI^.PredictedX := ShortInt(ReadByte);
            TMI^.PredictedY := ShortInt(ReadByte);

            TMI^.IsPredicted := True;

            Inc(tmPos);
          end;
          gtIntraTile:
          begin
            palIdx := ReadWord;

            Inc(lastTileIdx);
            Assert(lastTileIdx < Length(FTiles));
            KFStream.Read(FTiles[lastTileIdx]^.GetPalPixelsPtr^[0, 0], sqr(cTileWidth));
            FTiles[lastTileIdx]^.Active := True;

            // next frame if needed
            if frm = nil then
              frm := NextFrame(kf);

            SetTMI(lastTileIdx, palIdx, CommandData, frm.TileMap[tmPos div FTileMapWidth, tmPos mod FTileMapWidth]);
            Inc(tmPos);
          end

          else
            Assert(False, 'Unknown command: ' + IntToStr(Ord(Command)) + ', commandData: ' + IntToStr(CommandData) + ', prevCommand: '+ IntToStr(Ord(prevCommand)));
        end;

        prevCommand := Command;

      until False;

      kf.EndFrame := loadedFrmCount - 1;
      kf.FrameCount := kf.EndFrame - kf.StartFrame + 1;

    until AStream.Position >= AStream.Size;
  finally
    KFStream.Free;
  end;

  ReframeUI(FTileMapWidth, FTileMapHeight); // for FPaletteBitmap
end;

procedure TTilingEncoder.SaveStream(AStream: TStream);
const
  CMinBlkSkipCount = 4;
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

  procedure DoTMI(const TMI: TTileMapItem);
  var
    tileIdx: Cardinal;
    palIdx, attrs: Word;
    isIntra, isLongTile, isLongPal, isLongOffsets: Boolean;
  begin
    if TMI.IsPredicted then
    begin
      isLongOffsets :=  not InRange(TMI.PredictedX, -32, 31) or not InRange(TMI.PredictedY, -32, 31);

      if isLongOffsets then
      begin
        DoCmd(gtPredictedTileLongOffsets, 0);
        DoByte(PByte(@TMI.PredictedX)^);
        DoByte(PByte(@TMI.PredictedY)^);
      end
      else
      begin
        attrs := (PByte(@TMI.PredictedX)^ and 63) or ((PByte(@TMI.PredictedY)^ and 63) shl 6);

        DoCmd(gtPredictedTileShortOffsets, attrs);
      end;
    end
    else
    begin
      tileIdx := Max(0, TMI.TileIdx);
      palIdx := Max(0, TMI.PalIdx);

      isIntra := FTiles[tileIdx]^.UseCount <= 1;
      isLongTile := tileIdx > High(Word);
      isLongPal := palIdx >= (1 shl (CGTMCommandBits - 2));

      attrs := (Ord(TMI.VMirror) shl 1) or Ord(TMI.HMirror);

      if isIntra then
      begin
        DoCmd(gtIntraTile, attrs);
        DoWord(palIdx);
        ZStream.Write(Tiles[tileIdx]^.GetPalPixelsPtr^[0, 0], sqr(cTileWidth));
      end
      else
      begin
        if not isLongTile and not isLongPal then
        begin
          DoCmd(gtShortTileIdxShortPalIdx, attrs or (palIdx shl 2));
          DoWord(tileIdx);
        end
        else if not isLongPal then
        begin
          DoCmd(gtLongTileIdxShortPalIdx, attrs or (palIdx shl 2));
          DoDWord(tileIdx);
        end
        else
        begin
          DoCmd(gtLongTileIdxLongPalIdx, attrs);
          DoWord(palIdx);
          DoDWord(tileIdx);
        end;
      end;
    end;
  end;

  procedure WritePalettes;
  var
    colIdx, palIdx, col: Integer;
  begin
    for palIdx := 0 to FPaletteCount - 1 do
    begin
      DoCmd(gtLoadPalette, 0);
      DoWord(palIdx);
      for colIdx := 0 to FPaletteSize - 1 do
      begin
        col := FPalettes[palIdx].PaletteRGB[colIdx];

        if col = cDitheringNullColor then
          col := $ffffff;

        DoDWord(col or $ff000000);
      end;
    end;
  end;

  procedure WriteTiles;
  var
    tIdx, reusedTileCount, tx, ty: Integer;
  begin
    reusedTileCount := 0;
    for tIdx := 0 to High(Tiles) do
    begin
      Assert(Tiles[tIdx]^.Active);
      if Tiles[tIdx]^.UseCount = 1 then
      begin
        reusedTileCount := tIdx;
        Break;
      end;
    end;

    if reusedTileCount > 0 then
    begin
      DoCmd(gtTileSet, FPaletteSize);
      DoDWord(0); // start tile
      DoDWord(reusedTileCount - 1); // end tile

      for tIdx := 0 to reusedTileCount - 1 do
        ZStream.Write(Tiles[tIdx]^.GetPalPixelsPtr^[0, 0], sqr(cTileWidth));
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
  Header.EncoderVersion := 4; // 2 -> fixed blending extents; 3 -> *AddlBlendTileIdx; 4 -> PredictMotion;
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
    WritePalettes;

    LastKF := 0;
    for kfIdx := 0 to High(FKeyFrames) do
    begin
      KeyFrame := FKeyFrames[kfIdx];

      for frmIdx := KeyFrame.StartFrame to KeyFrame.EndFrame do
      begin
        Frame := FFrames[frmIdx];

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
              if not Frame.TileMap[sy, sx].IsSmoothed then
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

  FRenderBackBuffer := TBitmap.Create;
  FInputBitmap := TBitmap.Create;
  FOutputBitmap := TBitmap.Create;
  FTilesBitmap := TBitmap.Create;
  FPaletteBitmap := TBitmap.Create;

  FRenderPrevFrameIndex := -1;
  FRenderPredicted := True;
  FRenderMirrored := True;
  FRenderOutputDithered := True;

  FRenderPage := rpOutput;
  ReframeUI(80, 45);
  FFramesPerSecond := 24.0;

  LoadDefaultSettings;
end;

destructor TTilingEncoder.Destroy;
begin
  ClearAll(False);

  DeleteCriticalSection(FCS);

  FRenderBackBuffer.Free;
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
    esReduce:
      Reduce;
    esPreparePalettes:
      PreparePalettes;
    esDither:
      Dither;
    esReconstruct:
      Reconstruct;
    esPredictMotion:
      PredictMotion;
    esReindex:
      Reindex;
    esSave:
      Save;
  end;
end;

end.

