unit tilingencoder;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}

{$define ASM_DBMP}


interface

uses
  windows, Classes, SysUtils, strutils, types, Math, FileUtil, typinfo, zstream, Process, LazLogger,
  Graphics, IntfGraphics, FPimage, FPCanvas, FPWritePNG, GraphType, fgl, MTProcs, extern, tbbmalloc;

type
  TEncoderStep = (esNone = -1, esLoad = 0, esDither, esMakeUnique, esGlobalTiling, esFrameTiling, esReindex, esSmooth, esSave);
  TRenderPage = (rpNone, rpInput, rpOutput, rpTilesPalette);

const
  // tweakable constants

  cBitsPerComp = 8;
  cMaxFTBlend = 16;
  cFTQWeighting = False;

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

  cLumaDiv = cRedMul + cGreenMul + cBlueMul;
  cSmoothingPrevFrame = 1;
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

  cEncoderStepLen: array[TEncoderStep] of Integer = (0, 3, 3, 1, 5, 1, 2, 2, 1);

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


  cDCTUVRatio: array[0..cTileWidth-1,0..cTileWidth-1] of TFloat = (
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
  // ShortBlendTileIdx: data -> tile index (16 bits); BlendY offset(4 bits); BlendX offset(4 bits); Previous frame factor (4 bits); Current frame factor (4 bits); commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
  // LongLendTileIdx:   data -> tile index (32 bits); BlendY offset(4 bits); BlendX offset(4 bits); Previous frame factor (4 bits); Current frame factor (4 bits); commandBits -> palette index (8 bits); V mirror (1 bit); H mirror (1 bit)
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

  PTile = ^TTile;
  PPTile = ^PTile;

  PTileDynArray = array of PTile;
  PTileDynArray2 = array of PTileDynArray;

  TRGBPixels = array[0..(cTileWidth - 1),0..(cTileWidth - 1)] of Integer;
  TPalPixels = array[0..(cTileWidth - 1),0..(cTileWidth - 1)] of Byte;
  TCpnPixels = array[0..cColorCpns-1, 0..cTileWidth-1,0..cTileWidth-1] of TFloat;
  PRGBPixels = ^TRGBPixels;
  PPalPixels = ^TPalPixels;
  PCpnPixels = ^TCpnPixels;

  TTile = packed record // /!\ update TTileHelper.CopyFrom each time this structure is changed /!\
    UseCount, TmpIndex, MergeIndex, KFSoleIndex: Integer;
    Active, IntraKF, Additional, HasRGBPixels, HasPalPixels: Boolean;
  end;

  PTileArray = array of PTile;

  { TTileHelper }

  TTileHelper = record helper for TTile
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
    procedure ExtractPalPixels(AArray: PANNFloat);
    procedure LoadPalPixels(AArray: PANNFloat);
    function ComparePalPixelsTo(const ATile: TTile): Integer;

    property RGBPixels[y, x: Integer]: Integer read GetRGBPixels write SetRGBPixels;
    property PalPixels[y, x: Integer]: Byte read GetPalPixels write SetPalPixels;
  end;

  PTileMapItem = ^TTileMapItem;

  TTileMapItem = packed record
    TileIdx, SmoothedTileIdx, FTTileDSIdx: Integer;
    PalIdx, SmoothedPalIdx: SmallInt;
    Smoothed, HMirror, VMirror, SmoothedHMirror, SmoothedVMirror: Boolean;
    BlendCur, BlendPrev: Byte;
    BlendX, BlendY: ShortInt;
  end;

  TTileMapItems = array of TTileMapItem;

  TTilingDataset = record
    Dataset: TANNFloatDynArray;
    TDToTileIdx: TIntegerDynArray;
    TDToAttrs: TByteDynArray;
    FLANN: flann_index_t;
    FLANNParams: PFLANNParameters;
  end;

  PTilingDataset = ^TTilingDataset;

  TCountIndex = record
    Index, Count: Integer;
    R, G, B, Luma: Byte;
    Hue, Sat, Val: Byte;
  end;

  PCountIndex = ^TCountIndex;
  TCountIndexList = specialize TFPGList<PCountIndex>;

  TMixingPlan = record
    // static
    LumaPal: array of Integer;
    Y2Palette: array of array[0..3] of Integer;
    Y2MixedColors: Integer;
  end;

  TKeyFrame = class;

  { TFrame }

  TFrame = class
    PKeyFrame: TKeyFrame;
    Index: Integer;

    TileMap: array of array of TTileMapItem;

    Tiles: array of PTile;
    FSPixels: TByteDynArray;
  end;

  TFrameArray =  array of TFrame;

  { TKeyFrame }

  TKeyFrame = class
    StartFrame, EndFrame, FrameCount: Integer;
    FramesLeft: Integer;

    PaletteRGB: TIntegerDynArray2;
    CS: TRTLCriticalSection;
    MixingPlans: array of TMixingPlan;
    TileDS: array of PTilingDataset;

    PaletteUseCount: array of record
      UseCount: Integer;
      Palette: TIntegerDynArray;
      PalIdx: Integer;
    end;

    FTPaletteDoneEvent: array of THandle;
    FTPalettesLeft: Integer;
    FTErrCml: TFloat;

    constructor Create(APaletteCount, AStartFrame, AEndFrame: Integer);
    destructor Destroy; override;
  end;

  TKeyFrameArray =  array of TKeyFrame;

  TTilingEncoder = class;

  TTilingEncoderProgressEvent = procedure(ASender: TTilingEncoder; APosition, AMax: Integer; AHourGlass: Boolean) of object;

  { TTilingEncoder }

  TTilingEncoder = class
  private
    FGamma: array[0..1] of TFloat;
    FGammaCorLut: array[-1..1, 0..High(Byte)] of TFloat;
    FVecInv: array[0..256 * 4 - 1] of Cardinal;
    FDCTLut:array[0..sqr(sqr(cTileWidth)) - 1] of TFloat;
    FPalettePattern : TFloatDynArray2;

    FDitheringUseGamma: Boolean;
    FEncoderGammaValue: TFloat;
    FFrameTilingBlendingSize: Integer;
    FFrameTilingBlendingThreshold: TFloat;
    FFrameTilingUseGamma: Boolean;
    FOnProgress: TTilingEncoderProgressEvent;
    FGlobalTilingQualityBasedTileCount: TFloat;
    FQuantizerDennisLeeBitsPerComponent: Integer;
    FQuantizerUseYakmo: Boolean;
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
    FRenderTilePage: Integer;
    FGlobalTilingTileCount: Integer;
    FAdditionalTiles: TThreadList;
    FDitheringUseThomasKnoll: Boolean;
    FDitheringYliluoma2MixedColors: Integer;
    FInputPath: String;
    FFramesPerSecond: Double;

    FProgressStep: TEncoderStep;
    FProgressStartTime, FProgressPrevTime: Int64;

    FTileMapWidth: Integer;
    FTileMapHeight: Integer;
    FTileMapSize: Integer;
    FScreenWidth: Integer;
    FScreenHeight: Integer;

    FPaletteCount: Integer;
    FPaletteSize: Integer;

    FCS: TRTLCriticalSection;
    FLock: TSpinlock;
    FConcurrentKModesBins: Integer;

    FOutputBitmap: TBitmap;
    FInputBitmap: TBitmap;
    FPaletteBitmap: TBitmap;
    FTilesBitmap: TBitmap;

    FKeyFrames: TKeyFrameArray;
    FFrames: TFrameArray;
    FTiles: PTileArray;

    function GetFrameCount: Integer;
    function GetKeyFrameCount: Integer;
    function PearsonCorrelation(const x: TFloatDynArray; const y: TFloatDynArray): TFloat;
    function ComputeCorrelationBGR(const a: TIntegerDynArray; const b: TIntegerDynArray): TFloat;
    function ComputeDistanceRGB(const a: TIntegerDynArray; const b: TIntegerDynArray): TFloat;
    function ComputeInterFrameCorrelation(a, b: TFrame; out EuclideanDist: TFloat): TFloat;

    procedure ClearAll;
    procedure ProgressRedraw(ASubStepIdx: Integer = -1; AProgressStep: TEncoderStep = esNone);
    procedure InitLuts(ATilePaletteSize, APaletteCount: Integer);
    function GammaCorrect(lut: Integer; x: Byte): TFloat; inline;
    function GammaUncorrect(lut: Integer; x: TFloat): Byte; inline;

    procedure DitherFloydSteinberg(const AScreen: TByteDynArray);

    function HSVToRGB(h, s, v: Byte): Integer;
    procedure RGBToHSV(col: Integer; out h, s, v: Byte); overload;
    procedure RGBToHSV(col: Integer; out h, s, v: TFloat); overload;
    procedure RGBToYUV(col: Integer; GammaCor: Integer; out y, u, v: TFloat);
    procedure RGBToYUV(r, g, b: Byte; GammaCor: Integer; out y, u, v: TFloat);
    procedure RGBToLAB(r, g, b: TFloat; GammaCor: Integer; out ol, oa, ob: TFloat);
    procedure RGBToLAB(ir, ig, ib: Integer; GammaCor: Integer; out ol, oa, ob: TFloat);
    function LABToRGB(ll, aa, bb: TFloat): Integer;
    procedure SetDitheringYliluoma2MixedColors(AValue: Integer);
    procedure SetEncoderGammaValue(AValue: TFloat);
    procedure SetFramesPerSecond(AValue: Double);
    procedure SetFrameTilingBlendingSize(AValue: Integer);
    procedure SetFrameTilingBlendingThreshold(AValue: TFloat);
    procedure SetGlobalTilingQualityBasedTileCount(AValue: TFloat);
    procedure SetQuantizerDennisLeeBitsPerComponent(AValue: Integer);
    procedure SetRenderFrameIndex(AValue: Integer);
    procedure SetRenderGammaValue(AValue: TFloat);
    procedure SetRenderPaletteIndex(AValue: Integer);
    procedure SetRenderTilePage(AValue: Integer);
    procedure SetGlobalTilingTileCount(AValue: Integer);
    function YUVToRGB(y, u, v: TFloat): Integer;

    procedure WaveletGS(Data: PFloat; Output: PFloat; dx, dy, depth: cardinal);
    procedure DeWaveletGS(wl: PFloat; pic: PFloat; dx, dy, depth: longint);
    procedure ComputeTilePsyVisFeatures(const ATile: TTile; FromPal, UseLAB, QWeighting, HMirror,
      VMirror: Boolean; GammaCor: Integer; const pal: TIntegerDynArray; DCT: PFloat); inline;

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
    function GetTileIndexTMItem(ATileIndex: Integer; out AFrame: TFrame): PTileMapItem;

    procedure PreparePalettes(AKeyFrame: TKeyFrame; ADitheringGamma: Integer);
    procedure QuantizePalette(AKeyFrame: TKeyFrame; APalIdx: Integer; UseYakmo: Boolean; DLv3BPC: Integer; ADitheringGamma: Integer);
    procedure FinishQuantizePalettes(AKeyFrame: TKeyFrame);
    procedure DitherTile(var ATile: TTile; var Plan: TMixingPlan);
    procedure DitherTiles(AFrame: TFrame);

    function GetTileZoneSum(const ATile: TTile; x, y, w, h: Integer): Integer;
    function GetTilePalZoneThres(const ATile: TTile; ZoneCount: Integer; Zones: PByte): Integer;
    procedure MakeTilesUnique(FirstTileIndex, TileCount: Integer);
    procedure MergeTiles(const TileIndexes: array of Integer; TileCount: Integer; BestIdx: Integer; NewTile: PPalPixels; NewTileRGB: PRGBPixels);
    procedure InitMergeTiles;
    procedure FinishMergeTiles;
    procedure DoGlobalKMeans(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
    procedure DoGlobalTiling(DesiredNbTiles: Integer);

    procedure ReloadPreviousTiling(AFN: String);

    function HMirrorPalTile(var ATile: TTile): Boolean;
    function VMirrorPalTile(var ATile: TTile): Boolean;
    procedure PrepareKFTiling(AKF: TKeyFrame; APaletteIndex, AFTGamma: Integer);
    procedure FinishKFTiling(AKF: TKeyFrame;  APaletteIndex: Integer);
    procedure DoKFTiling(AKF: TKeyFrame; APaletteIndex: Integer; AFTGamma: Integer; AFTBlend: Integer; AFTBlendThres: TFloat
      );

    function GetTileUseCount(ATileIndex: Integer): Integer;
    procedure ReindexTiles;
    procedure DoTemporalSmoothing(AFrame, APrevFrame: TFrame; Y: Integer; Strength: TFloat; AddlTilesThres: TFloat; NonAddlCount: Integer);

    procedure SaveStream(AStream: TStream; AFTBlend: Integer);
    procedure SaveRawTiles(OutFN: String);

    function DoExternalFFMpeg(AFN: String; var AVidPath: String; AStartFrame, AFrameCount: Integer; AScale: Double; out
      AFPS: Double): String;
  public

    constructor Create;
    destructor Destroy; override;

    procedure Load(AFileName: String; AStartFrame, AFrameCount, APaletteCount, ATilePaletteSize: Integer; AScaling: TFloat = 1.0);
    procedure Dither;
    procedure MakeUnique;
    procedure GlobalTiling(AReloadTiles: Boolean = False; AReloadFN: String = '');
    procedure FrameTiling;
    procedure Reindex;
    procedure Smooth(ASmoothingFactor: TFloat; AAddlTilesThres: TFloat);
    procedure Save(AOutputFN: String);
    procedure GeneratePNGs(AOutputFN: String);

    procedure Render;
    procedure ReframeUI(AWidth, AHeight: Integer);

    property KeyFrames: TKeyFrameArray read FKeyFrames;
    property Frames: TFrameArray read FFrames;
    property Tiles: PTileArray read FTiles;

    property ScreenWidth: Integer read FScreenWidth;
    property ScreenHeight: Integer read FScreenHeight;
    property FramesPerSecond: Double read FFramesPerSecond write SetFramesPerSecond;
    property TileMapWidth: Integer read FTileMapWidth;
    property TileMapHeight: Integer read FTileMapHeight;
    property TileMapSize: Integer read FTileMapSize;
    property TilePaletteSize: Integer read FPaletteSize;
    property PaletteCount: Integer read FPaletteCount;
    property FrameCount: Integer read GetFrameCount;
    property KeyFrameCount: Integer read GetKeyFrameCount;

    property InputPath: String read FInputPath;
    property EncoderGammaValue: TFloat read FEncoderGammaValue write SetEncoderGammaValue;
    property ProgressStep: TEncoderStep read FProgressStep;

    property RenderPlaying: Boolean read FRenderPlaying write FRenderPlaying;
    property RenderFrameIndex: Integer read FRenderFrameIndex write SetRenderFrameIndex;
    property RenderBlended: Boolean read FRenderBlended write FRenderBlended;
    property RenderMirrored: Boolean read FRenderMirrored write FRenderMirrored;
    property RenderSmoothed: Boolean read FRenderSmoothed write FRenderSmoothed;
    property RenderUseGamma: Boolean read FRenderUseGamma write FRenderUseGamma;
    property RenderPaletteIndex: Integer read FRenderPaletteIndex write SetRenderPaletteIndex;
    property RenderTilePage: Integer read FRenderTilePage write SetRenderTilePage;
    property RenderGammaValue: TFloat read FRenderGammaValue write SetRenderGammaValue;

    property RenderPage: TRenderPage read FRenderPage write FRenderPage;
    property RenderTitleText: String read FRenderTitleText;
    property RenderPsychoVisualQuality: TFloat read FRenderPsychoVisualQuality;

    property QuantizerUseYakmo: Boolean read FQuantizerUseYakmo write FQuantizerUseYakmo;
    property QuantizerDennisLeeBitsPerComponent: Integer read FQuantizerDennisLeeBitsPerComponent write SetQuantizerDennisLeeBitsPerComponent;
    property DitheringUseGamma: Boolean read FDitheringUseGamma write FDitheringUseGamma;
    property DitheringYliluoma2MixedColors: Integer read FDitheringYliluoma2MixedColors write SetDitheringYliluoma2MixedColors;
    property DitheringUseThomasKnoll: Boolean read FDitheringUseThomasKnoll write FDitheringUseThomasKnoll;

    property GlobalTilingTileCount: Integer read FGlobalTilingTileCount write SetGlobalTilingTileCount;
    property GlobalTilingQualityBasedTileCount: TFloat read FGlobalTilingQualityBasedTileCount write SetGlobalTilingQualityBasedTileCount;

    property FrameTilingUseGamma: Boolean read FFrameTilingUseGamma write FFrameTilingUseGamma;
    property FrameTilingBlendingSize: Integer read FFrameTilingBlendingSize write SetFrameTilingBlendingSize;
    property FrameTilingBlendingThreshold: TFloat read FFrameTilingBlendingThreshold write SetFrameTilingBlendingThreshold;

    property OutputBitmap: TBitmap read FOutputBitmap;
    property InputBitmap: TBitmap read FInputBitmap;
    property PaletteBitmap: TBitmap read FPaletteBitmap;
    property TilesBitmap: TBitmap read FTilesBitmap;

    property OnProgress: TTilingEncoderProgressEvent read FOnProgress write FOnProgress;
  end;

  { TFastPortableNetworkGraphic }

  TFastPortableNetworkGraphic = class(TPortableNetworkGraphic)
    procedure InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter); override;
  end;


  function iDiv0(x,y:Integer):Integer;inline;

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

function iDiv0(x, y: Integer): Integer;inline;
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

function CompareEuclideanDCT(const a, b: TFloatDynArray): TFloat; inline; overload;
begin
  Result := CompareEuclideanDCTPtr(@a[0], @b[0]);
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

{ TFastPortableNetworkGraphic }

procedure TFastPortableNetworkGraphic.InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter);
var
  W: TFPWriterPNG absolute AWriter;
begin
  inherited InitializeWriter(AImage, AWriter);
  W.CompressionLevel := clfastest;
end;

{ TTileHelper }

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
  HasPalPixels, HasRGBPixels: Boolean;
begin
  Assert(Length(AArray) > 0);

  // account for the array having been sorted
  smallest := AArray[0];
  for i := 1 to High(AArray) do
    if AArray[i] < smallest then
      smallest := AArray[i];

  prevLen := Length(AArray);
  HasPalPixels := smallest^.HasPalPixels;
  HasRGBPixels := smallest^.HasRGBPixels;

  size := SizeOf(TTile) + IfThen(HasPalPixels, SizeOf(TPalPixels)) + IfThen(HasRGBPixels, SizeOf(TRGBPixels));
  data := PByte(smallest);

  data := ReAllocMem(data, size * ANewX);
  SetLength(AArray, ANewX);

  // recompute existing array pointers
  for i := 0 to prevLen - 1 do
    AArray[i] := PTile(PByte(AArray[i]) - PByte(smallest) + data);

  // init new pointers
  Inc(data, size * prevLen);
  for i := prevLen to ANewX - 1 do
  begin
    AArray[i] := PTile(data);
    AArray[i]^.HasPalPixels := HasPalPixels;
    AArray[i]^.HasRGBPixels := HasRGBPixels;
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

procedure TTileHelper.ExtractPalPixels(AArray: PANNFloat);
var
  i: Integer;
  PB: PByte;
  PF: PANNFloat;
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

procedure TTileHelper.LoadPalPixels(AArray: PANNFloat);
var
  i: Integer;
  PB: PByte;
  PF: PANNFloat;
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

procedure TTileHelper.CopyFrom(const ATile: TTile);
begin
  Active := ATile.Active;
  IntraKF := ATile.IntraKF;
  TmpIndex := ATile.TmpIndex;
  MergeIndex := ATile.MergeIndex;
  UseCount := ATile.UseCount;
  KFSoleIndex := ATile.KFSoleIndex;
  Additional := ATile.Additional;

  if HasPalPixels and ATile.HasPalPixels then
    CopyPalPixels(ATile.GetPalPixelsPtr^);
  if HasRGBPixels and ATile.HasRGBPixels then
    CopyRGBPixels(ATile.GetRGBPixelsPtr^);
end;

{ TKeyFrame }

constructor TKeyFrame.Create(APaletteCount, AStartFrame, AEndFrame: Integer);
begin
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
  inherited Destroy;
  DeleteCriticalSection(CS);
end;

{ TTilingEncoder }

procedure TTilingEncoder.InitLuts(ATilePaletteSize, APaletteCount: Integer);
const
  cCurvature = 2.0;
var
  g, i, j, v, u, y, x: Int64;
  f, fp: TFloat;
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
  for v := 0 to (cTileWidth - 1) do
    for u := 0 to (cTileWidth - 1) do
      for y := 0 to (cTileWidth - 1) do
        for x := 0 to (cTileWidth - 1) do
        begin
          FDCTLut[i] := cos((x + 0.5) * u * PI / (cTileWidth * 2)) * cos((y + 0.5) * v * PI / (cTileWidth * 2)) * cDCTUVRatio[v, u];
          Inc(i);
        end;

  // palette pattern

  SetLength(FPalettePattern, APaletteCount, ATilePaletteSize);

  f := 0;
  for i := 0 to ATilePaletteSize - 1 do
  begin
    fp := f;
    f := power(i + 2, cCurvature);

    for j := 0 to APaletteCount - 1 do
      FPalettePattern[j, i] := ((j + 1) / APaletteCount) * max(APaletteCount, f - fp) + fp;
  end;

  for j := 0 to APaletteCount - 1 do
    for i := 0 to ATilePaletteSize - 1 do
      FPalettePattern[j, i] /= FPalettePattern[APaletteCount - 1, ATilePaletteSize - 1];
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

procedure TTilingEncoder.GlobalTiling(AReloadTiles: Boolean; AReloadFN: String);
begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(0, esGlobalTiling);

  if AReloadTiles then
  begin
    if not FileExists(AReloadFN) then
      raise EFileNotFoundException.Create('File not found: ' + AReloadFN);
    ReloadPreviousTiling(AReloadFN);
  end
  else
  begin
    DoGlobalTiling(FGlobalTilingTileCount);

    SaveRawTiles(AReloadFN);
  end;

  ProgressRedraw(5);
end;

procedure TTilingEncoder.Dither;

  procedure DoPrepare(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    if not InRange(AIndex, 0, High(FKeyFrames)) then
      Exit;

    PreparePalettes(FKeyFrames[AIndex], IfThen(FDitheringUseGamma, 0, -1));
  end;

  procedure DoQuantize(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    if not InRange(AIndex, 0, Length(FKeyFrames) * FPaletteCount - 1) then
      Exit;

    QuantizePalette(FKeyFrames[AIndex div FPaletteCount], AIndex mod FPaletteCount, FQuantizerUseYakmo, FQuantizerDennisLeeBitsPerComponent, IfThen(FDitheringUseGamma, 0, -1));
  end;

  procedure DoFinish(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    if not InRange(AIndex, 0, High(FFrames)) then
      Exit;

    DitherTiles(FFrames[AIndex]);
  end;

var
  i: Integer;
begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(0, esDither);
  ProcThreadPool.DoParallelLocalProc(@DoPrepare, 0, High(FKeyFrames));
  ProgressRedraw(1);

  for i := 0 to High(FKeyFrames) do
    SetLength(FKeyFrames[i].PaletteUseCount, FPaletteCount);
  ProcThreadPool.DoParallelLocalProc(@DoQuantize, 0, Length(FKeyFrames) * FPaletteCount - 1);
  for i := 0 to High(FKeyFrames) do
    FinishQuantizePalettes(FKeyFrames[i]);
  ProgressRedraw(2);

  ProcThreadPool.DoParallelLocalProc(@DoFinish, 0, High(FFrames));
  ProgressRedraw(3);
  for i := 0 to High(FKeyFrames) do
    SetLength(FKeyFrames[i].PaletteUseCount, 0);
end;

procedure TTilingEncoder.MakeUnique;
var
  TilesAtATime: Integer;

  procedure DoMakeUnique(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    if not InRange(AIndex, 0, High(FTiles) div TilesAtATime) then
      Exit;

    MakeTilesUnique(AIndex * TilesAtATime, Min(Length(FTiles) - AIndex * TilesAtATime, TilesAtATime));
  end;

begin
  TilesAtATime := FTileMapSize * 25;

  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(0, esMakeUnique);

  InitMergeTiles;

  ProcThreadPool.DoParallelLocalProc(@DoMakeUnique, 0, High(FTiles) div TilesAtATime);

  FinishMergeTiles;

  ProgressRedraw(1);
end;

function CompareFrames(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PInteger(Item2)^, PInteger(Item1)^);
end;

procedure TTilingEncoder.FrameTiling;

  procedure DoKFPal(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    kf, pal, frm: Integer;
  begin
    DivMod(AIndex, Length(FKeyFrames), pal, kf);

    PrepareKFTiling(FKeyFrames[kf], pal, IfThen(FFrameTilingUseGamma, 0, -1));

    DoKFTiling(FKeyFrames[kf], pal, IfThen(FFrameTilingUseGamma, 0, -1), FFrameTilingBlendingSize, FFrameTilingBlendingThreshold);

    FinishKFTiling(FKeyFrames[kf], pal);
  end;

var
  kf, i: Integer;
begin
  if Length(FKeyFrames) = 0 then
    Exit;

  ProgressRedraw(0, esFrameTiling);

  for kf := 0 to high(FKeyFrames) do
  begin
    FKeyFrames[kf].FTPalettesLeft := FPaletteCount;
    SetLength(FKeyFrames[kf].TileDS, FPaletteCount);
    SetLength(FKeyFrames[kf].FTPaletteDoneEvent, FPaletteCount);
    for i := 0 to FPaletteCount - 1 do
      FKeyFrames[kf].FTPaletteDoneEvent[i] := CreateEvent(nil, True, False, nil);
  end;
  try
    ProcThreadPool.DoParallelLocalProc(@DoKFPal, 0, FPaletteCount * Length(FKeyFrames) - 1, nil, NumberOfProcessors);
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

procedure TTilingEncoder.Load(AFileName: String; AStartFrame, AFrameCount, APaletteCount, ATilePaletteSize: Integer;
  AScaling: TFloat);

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
  i, StartFrame, frc: Integer;
  fn: String;
  bmp: TPicture;
  wasAutoQ: Boolean;
begin
  FPaletteSize := EnsureRange(ATilePaletteSize, 2, 64);
  FPaletteCount := EnsureRange(APaletteCount, 1, 256);
  wasAutoQ := FGlobalTilingTileCount = round(FGlobalTilingQualityBasedTileCount * EqualQualityTileCount(Length(FFrames) * FTileMapSize));

  ProgressRedraw(-1, esNone);

  ClearAll;

  ProgressRedraw(0, esLoad);

  // init Gamma LUTs

  InitLuts(FPaletteSize, FPaletteCount);

  // load video

  StartFrame := AStartFrame;
  frc := AFrameCount;

  if FileExists(AFileName) then
  begin
    DoExternalFFMpeg(AFileName, FInputPath, StartFrame, frc, AScaling, FFramesPerSecond);
    StartFrame := 1;
  end
  else
  begin
    FInputPath := AFileName;
    FFramesPerSecond := 24.0;
  end;

  // automaticaly count frames if needed

  if frc <= 0 then
  begin
    i := 0;
    repeat
      fn := Format(FInputPath, [i + StartFrame]);
      Inc(i);
    until not FileExists(fn);

    frc := i - 1;
  end;

  // load frames bitmap data

  SetLength(FFrames, frc);

  bmp := TPicture.Create;
  try
    bmp.Bitmap.PixelFormat:=pf32bit;
    bmp.LoadFromFile(Format(FInputPath, [StartFrame]));
    ReframeUI(bmp.Width div cTileWidth, bmp.Height div cTileWidth);
  finally
    bmp.Free;
  end;

  ProcThreadPool.DoParallelLocalProc(@DoLoadFrame, 0, High(FFrames), Pointer(StartFrame));
  WriteLn;

  ProgressRedraw(1);

  FindKeyFrames;

  ProgressRedraw(2);

  LoadTiles;

  // free up frame memory
  for i := 0 to High(FFrames) do
    SetLength(FFrames[i].FSPixels, 0);

  if wasAutoQ or (FGlobalTilingTileCount <= 0) then
  begin
    SetGlobalTilingQualityBasedTileCount(0.0);
    SetGlobalTilingQualityBasedTileCount(FGlobalTilingQualityBasedTileCount);
  end;

  ProgressRedraw(3);
end;

procedure TTilingEncoder.Reindex;
var
  i, sx, sy, tidx: Integer;
begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(0, esReindex);

  for i := 0 to High(FTiles) do
  begin
    FTiles[i]^.UseCount := 0;
    FTiles[i]^.Active := False;
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

  ReindexTiles;

  ProgressRedraw(2);
end;

procedure TTilingEncoder.Save(AOutputFN: String);
var
  fs: TFileStream;
begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(0, esSave);

  fs := TFileStream.Create(AOutputFN, fmCreate or fmShareDenyWrite);
  try
    SaveStream(fs, FFrameTilingBlendingSize);
  finally
    fs.Free;
  end;

  ProgressRedraw(1);
end;

procedure TTilingEncoder.Smooth(ASmoothingFactor: TFloat; AAddlTilesThres: TFloat);
var
  NonAddlCount: Integer;

  procedure DoSmoothing(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    i: Integer;
  begin
    if not InRange(AIndex, 0, FTileMapHeight - 1) then
      Exit;

    for i := cSmoothingPrevFrame to High(FFrames) do
      DoTemporalSmoothing(FFrames[i], FFrames[i - cSmoothingPrevFrame], AIndex, ASmoothingFactor, AAddlTilesThres, NonAddlCount);
  end;

var
  frm, j, i: Integer;
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
    for i := 0 to High(FTiles) do
      if FTiles[i]^.Additional then
      begin
        NonAddlCount := i;
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

procedure TTilingEncoder.GeneratePNGs(AOutputFN: String);
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
      Render;

      palPict.Canvas.Draw(0, 0, FOutputBitmap);
      palPict.SaveToFile(Format('%s_%.4d.png', [ChangeFileExt(AOutputFN, ''), i]));
    end;
  finally
    palPict.Free;

    RenderFrameIndex := oldRenderFrameIndex;
    Render;
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

function TTilingEncoder.GetFrameCount: Integer;
begin
  Result := Length(FFrames);
end;

function TTilingEncoder.GetKeyFrameCount: Integer;
begin
  Result := Length(FKeyFrames);
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
  sz, i: Integer;
  ya, yb: TFloatDynArray;
begin
  Assert(Length(a.FSPixels) = Length(b.FSPixels));
  sz := Length(a.FSPixels) div 3;
  SetLength(ya, sz * 3);
  SetLength(yb, sz * 3);

  for i := 0 to sz - 1 do
  begin
    RGBToLAB(a.FSPixels[i * 3 + 0], a.FSPixels[i * 3 + 1], a.FSPixels[i * 3 + 2], -1, ya[i + sz * 0], ya[i + sz * 1], ya[i + sz * 2]);
    RGBToLAB(b.FSPixels[i * 3 + 0], b.FSPixels[i * 3 + 1], b.FSPixels[i * 3 + 2], -1, yb[i + sz * 0], yb[i + sz * 1], yb[i + sz * 2]);
  end;

  Result := PearsonCorrelation(ya, yb);
  EuclideanDist := CompareEuclidean(ya, yb) / Length(a.FSPixels);
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
  FTileMapWidth := min(AWidth, 1920 div cTileWidth);
  FTileMapHeight := min(AHeight, 1080 div cTileWidth);

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

procedure TTilingEncoder.DitherFloydSteinberg(const AScreen: TByteDynArray);
var
  x, y, c, yp, xm, xp: Integer;
  OldPixel, NewPixel, QuantError: Integer;
  ppx: PByte;
begin
  ppx := @AScreen[0];
  for y := 0 to FScreenHeight - 1 do
    for x := 0 to FScreenWidth - 1 do
    begin
      yp := IfThen(y < FScreenHeight - 1, FScreenWidth * 3, 0);
      xp := IfThen(x < FScreenWidth - 1, 3, 0);
      xm := IfThen(x > 0, -3, 0);

      for c := 0 to 2 do
      begin
        OldPixel := ppx^;
        NewPixel := Posterize(OldPixel);
        QuantError := OldPixel - NewPixel;

        ppx^ := NewPixel;

        ppx[xp] := EnsureRange(ppx[xp] + (QuantError * 7) shr 4, 0, 255);
        ppx[yp + xm] := EnsureRange(ppx[yp + xm] + (QuantError * 3) shr 4, 0, 255);
        ppx[yp] := EnsureRange(ppx[yp] + (QuantError * 5) shr 4, 0, 255);
        ppx[yp + xp] := EnsureRange(ppx[yp + xp] + (QuantError * 1) shr 4, 0, 255);

        Inc(ppx);
      end;
    end;
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
  sx, sy, i, j, k, cnt, BIRCHClusterCount: Integer;
  GTile: PTile;

  Dataset, YakmoDataset: TFloatDynArray2;
  Clusters, YakmoClusters: TIntegerDynArray;
  di: Integer;

  BIRCH: PBIRCH;
  Yakmo: PYakmoSingle;
begin
  Assert(FPaletteCount <= Length(FPalettePattern));

  SetLength(Dataset, AKeyFrame.FrameCount * FTileMapSize, cTileDCTSize);
  SetLength(Clusters, Length(Dataset));

  di := 0;
  for i := AKeyFrame.StartFrame to AKeyFrame.EndFrame do
    for sy := 0 to FTileMapHeight - 1 do
      for sx := 0 to FTileMapWidth - 1 do
      begin
        GTile := FTiles[FFrames[i].TileMap[sy, sx].TileIdx];
        ComputeTilePsyVisFeatures(GTile^, False, True, True, False, False, ADitheringGamma, nil, @Dataset[di, 0]);
        Inc(di);
      end;
  assert(di = Length(Dataset));

  WriteLn('KF: ', AKeyFrame.StartFrame:8, ' Palettization start');

  if (di > 1) and (FPaletteCount > 1) then
  begin
    BIRCH := birch_create(100000, 64 * 1024 * 1024);
    for i := 0 to di - 1 do
      birch_insert_line(BIRCH, @Dataset[i, 0]);
    birch_get_results(BIRCH, @Clusters[0]);
    birch_destroy(BIRCH);
    BIRCHClusterCount := MaxIntValue(Clusters) + 1;

    WriteLn('KF: ', AKeyFrame.StartFrame:8, ' DatasetSize: ', di:8, ' BIRCHClusterCount: ', BIRCHClusterCount:6);

    SetLength(YakmoClusters, BIRCHClusterCount);
    SetLength(YakmoDataset, BIRCHClusterCount, cTileDCTSize);

    for i := 0 to BIRCHClusterCount - 1 do
    begin
      cnt := 0;
      for j := 0 to di - 1 do
        if Clusters[j] = i then
        begin
          for k := 0 to cTileDCTSize - 1 do
            YakmoDataset[i, k] += Dataset[j, k];
          Inc(cnt)
        end;

      if cnt > 0 then
        for k := 0 to cTileDCTSize - 1 do
          YakmoDataset[i, k] /= cnt;
    end;

    SetLength(Dataset, 0); // free up some memmory

    if BIRCHClusterCount > FPaletteCount then
    begin
      Yakmo := yakmo_single_create(FPaletteCount, 1, MaxInt, 1, 0, 0, 0);
      yakmo_single_load_train_data(Yakmo, BIRCHClusterCount, cTileDCTSize, @YakmoDataset[0]);
      SetLength(YakmoDataset, 0); // free up some memmory
      yakmo_single_train_on_data(Yakmo, @YakmoClusters[0]);
      yakmo_single_destroy(Yakmo);
    end
    else
    begin
      for i := 0 to BIRCHClusterCount - 1 do
        YakmoClusters[i] := i;
    end;
  end
  else
  begin
    SetLength(YakmoClusters, 1);
  end;

  di := 0;
  for i := AKeyFrame.StartFrame to AKeyFrame.EndFrame do
    for sy := 0 to FTileMapHeight - 1 do
      for sx := 0 to FTileMapWidth - 1 do
      begin
        FFrames[i].TileMap[sy, sx].PalIdx := YakmoClusters[Clusters[di]];
        Inc(di);
      end;
  assert(di = Length(Clusters));

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
                  FromRGB(FFrames[i].Tiles[k]^.RGBPixels[ty, tx], dlInput[j + 0], dlInput[j + 1], dlInput[j + 2]);
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
    i, di, sy, sx, ty, tx: Integer;
    rr, gg, bb: Byte;
    GTile: PTile;
    Dataset, Centroids: TFloatDynArray2;
    Clusters: TIntegerDynArray;
    Yakmo: PYakmoSingle;
    CMItem: PCountIndex;
  begin
    di := 0;
    for i := AKeyFrame.StartFrame to AKeyFrame.EndFrame do
      for sy := 0 to FTileMapHeight - 1 do
        for sx := 0 to FTileMapWidth - 1 do
        begin
          GTile := FTiles[FFrames[i].TileMap[sy, sx].TileIdx];
          if GTile^.Active and (FFrames[i].TileMap[sy, sx].PalIdx = APalIdx) then
            Inc(di, Sqr(cTileWidth));
        end;

    SetLength(Centroids, FPaletteSize, cFeatureCount);
    for i := 0 to FPaletteSize - 1 do
    begin
      Centroids[i, 0] := NaN;
      Centroids[i, 1] := NaN;
      Centroids[i, 2] := NaN;
    end;

    if di >= FPaletteSize then
    begin
      SetLength(Dataset, di, cFeatureCount);
      SetLength(Clusters, di);

      // build a dataset of RGB pixels

      di := 0;
      for i := AKeyFrame.StartFrame to AKeyFrame.EndFrame do
        for sy := 0 to FTileMapHeight - 1 do
          for sx := 0 to FTileMapWidth - 1 do
          begin
            GTile := FTiles[FFrames[i].TileMap[sy, sx].TileIdx];

            if GTile^.Active and (FFrames[i].TileMap[sy, sx].PalIdx = APalIdx) then
            begin
              for ty := 0 to cTileWidth - 1 do
                for tx := 0 to cTileWidth - 1 do
                begin
                  FromRGB(GTile^.RGBPixels[ty, tx], rr, gg, bb);
                  Dataset[di, 0] := GammaCorrect(ADitheringGamma, rr);
                  Dataset[di, 1] := GammaCorrect(ADitheringGamma, gg);
                  Dataset[di, 2] := GammaCorrect(ADitheringGamma, bb);
                  Inc(di);
                end;

              Inc(AKeyFrame.PaletteUseCount[APalIdx].UseCount);
            end;
          end;

      // use KMeans to quantize to FPaletteSize elements

      Yakmo := yakmo_single_create(FPaletteSize, 1, 200, 1, 0, 0, 0);
      yakmo_single_load_train_data(Yakmo, di, cFeatureCount, @Dataset[0]);

      SetLength(Dataset, 0); // free up some memmory

      yakmo_single_train_on_data(Yakmo, @Clusters[0]);
      yakmo_single_get_centroids(Yakmo, @Centroids[0]);
      yakmo_single_destroy(Yakmo);
    end;

    // retrieve palette data

    CMPal.Count := FPaletteSize;
    for i := 0 to FPaletteSize - 1 do
    begin
      New(CMItem);

      if di >= FPaletteSize then
      begin
        CMItem^.R := GammaUncorrect(ADitheringGamma, NanDef(Centroids[i, 0], 1.0));
        CMItem^.G := GammaUncorrect(ADitheringGamma, NanDef(Centroids[i, 1], 1.0));
        CMItem^.B := GammaUncorrect(ADitheringGamma, NanDef(Centroids[i, 2], 1.0));
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
  Assert(FPaletteCount <= Length(FPalettePattern));

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

  if APalIdx = FPaletteCount - 1 then
    WriteLn('KF: ', AKeyFrame.StartFrame:8, ' Quantization end');
end;

procedure TTilingEncoder.FinishQuantizePalettes(AKeyFrame: TKeyFrame);
var
  i, sy, sx, PalIdx: Integer;
  PalIdxLUT: TIntegerDynArray;
  GTile: PTile;
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
end;

procedure TTilingEncoder.DitherTiles(AFrame: TFrame);
var
  i, PalIdx: Integer;
  sx, sy: Integer;
  T: PTile;
begin
  EnterCriticalSection(AFrame.PKeyFrame.CS);
  if AFrame.PKeyFrame.FramesLeft < 0 then
  begin
    // build ditherer
    for i := 0 to FPaletteCount - 1 do
      PreparePlan(AFrame.PKeyFrame.MixingPlans[i], FDitheringYliluoma2MixedColors, AFrame.PKeyFrame.PaletteRGB[i]);
    AFrame.PKeyFrame.FramesLeft := AFrame.PKeyFrame.FrameCount;
  end;
  LeaveCriticalSection(AFrame.PKeyFrame.CS);

  for sy := 0 to FTileMapHeight - 1 do
    for sx := 0 to FTileMapWidth - 1 do
    begin
      T := FTiles[AFrame.TileMap[sy, sx].TileIdx];

      if T^.Active then
      begin
        // dither tile in chosen palette

        PalIdx := AFrame.TileMap[sy, sx].PalIdx;
        Assert(InRange(PalIdx, 0, FPaletteCount - 1));
        DitherTile(T^, AFrame.PKeyFrame.MixingPlans[PalIdx]);
      end;
    end;

  EnterCriticalSection(AFrame.PKeyFrame.CS);
  Dec(AFrame.PKeyFrame.FramesLeft);
  if AFrame.PKeyFrame.FramesLeft <= 0 then
  begin
    // cleanup ditherer
    for i := 0 to FPaletteCount - 1 do
      TerminatePlan(AFrame.PKeyFrame.MixingPlans[i]);
    WriteLn('KF: ', AFrame.PKeyFrame.StartFrame:8, ' Dithering end');
  end;
  LeaveCriticalSection(AFrame.PKeyFrame.CS);
end;

function CompareTilePalPixels(Item1, Item2:Pointer):Integer;
var
  t1, t2: PTile;
begin
  t1 := PTile(Item1);
  t2 := PTile(Item2);
  Result := t1^.ComparePalPixelsTo(t2^);
end;

procedure TTilingEncoder.MakeTilesUnique(FirstTileIndex, TileCount: Integer);
var
  i, pos, firstSameIdx: Integer;
  sortList: TFPList;
  sameIdx: array of Integer;

  procedure DoOneMerge;
  var
    j: Integer;
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

    sortList.Sort(@CompareTilePalPixels);

    // merge exactly similar tiles (so, consecutive after prev code)

    firstSameIdx := 0;
    for i := 1 to sortList.Count - 1 do
      if PTile(sortList[i - 1])^.ComparePalPixelsTo(PTile(sortList[i])^) <> 0 then
        DoOneMerge;

    i := sortList.Count;
    DoOneMerge;

  finally
    sortList.Free;
  end;
end;

procedure TTilingEncoder.LoadTiles;
var
  i, j: Integer;
  tileCnt: Integer;
begin
  // free memory from a prev run
  TTile.Array1DDispose(FTiles);

  tileCnt := Length(FFrames) * FTileMapSize;

  // allocate tiles
  FTiles := TTile.Array1DNew(tileCnt, True, True);

  // copy frame tiles to global tiles
  for i := 0 to High(FFrames) do
  begin
    tileCnt := i * FTileMapSize;
    for j := 0 to FTileMapSize - 1 do
      FTiles[tileCnt + j]^.CopyFrom(FFrames[i].Tiles[j]^);
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
  uu := (fb - yy) * (0.5 / (1.0 - cBlueMul / cLumaDiv));
  vv := (fr - yy) * (0.5 / (1.0 - cRedMul / cLumaDiv));

  y := yy; u := uu; v := vv; // for safe "out" param
end;

function TTilingEncoder.YUVToRGB(y, u, v: TFloat): Integer;
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

  Result := ToRGB(EnsureRange(Round(r * 255), 0, 255), EnsureRange(Round(g * 255), 0, 255), EnsureRange(Round(b * 255), 0, 255));
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
  x /= 96.6797 / 100;
  y /= 100.000 / 100;
  z /= 82.5188 / 100;
{$else}
  // Illuminant D65
  x /= 95.0470 / 100;
  y /= 100.000 / 100;
  z /= 108.883 / 100;
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

function TTilingEncoder.LABToRGB(ll, aa, bb: TFloat): Integer;
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

  x := 96.6797 / 100 * x;
  y := 100.000 / 100 * y;
  z := 182.5188 / 100 * z;

  r := x * 0.41847 + y * (-0.15866) + z * (-0.082835);
  g := x * (-0.091169) + y * 0.25243 + z * 0.015708;
  b := x * 0.00092090 + y * (-0.0025498) + z * 0.17860;

  if r > 0.04045 then
    r := 1.055 * Power(r, 1 / 2.4) - 0.055
  else
    r := 12.92 * r;
  if g > 0.04045 then
    g := 1.055 * Power(g, 1 / 2.4) - 0.055
  else
    g := 12.92 * g;
  if b > 0.04045 then
    b := 1.055 * Power(b, 1 / 2.4) - 0.055
  else
    b := 12.92 * b;

  Result := ToRGB(EnsureRange(Round(r * 255), 0, 255), EnsureRange(Round(g * 255), 0, 255), EnsureRange(Round(b * 255), 0, 255));
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

procedure TTilingEncoder.SetGlobalTilingTileCount(AValue: Integer);
var
  RawTileCount: Integer;
begin
  if FGlobalTilingTileCount = AValue then Exit;
  FGlobalTilingTileCount := AValue;

  RawTileCount := Length(FFrames) * FTileMapSize;
  FGlobalTilingTileCount := EnsureRange(FGlobalTilingTileCount, 1, RawTileCount);
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

procedure TTilingEncoder.ComputeTilePsyVisFeatures(const ATile: TTile; FromPal, UseLAB, QWeighting, HMirror,
  VMirror: Boolean; GammaCor: Integer; const pal: TIntegerDynArray; DCT: PFloat);
var
  u, v, x, y, xx, yy, cpn: Integer;
  z: Double;
  CpnPixels: TCpnPixels;
  pDCT, pCpn, pLut: PFloat;

  procedure ToCpn(col, x, y: Integer); inline;
  var
    r, g, b: Byte;
    yy, uu, vv: TFloat;
  begin
    FromRGB(col, r, g, b);

    if UseLAB then
      RGBToLAB(r, g, b, GammaCor, yy, uu, vv)
    else
      RGBToYUV(r, g, b, GammaCor, yy, uu, vv);

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

    for v := 0 to (cTileWidth - 1) do
      for u := 0 to (cTileWidth - 1) do
      begin
  		  z := 0.0;
        pCpn := @CpnPixels[cpn, 0, 0];

        // unroll y by cTileWidth

        // unroll x by cTileWidth
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);

        // unroll x by cTileWidth
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);

        // unroll x by cTileWidth
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);

        // unroll x by cTileWidth
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);

        // unroll x by cTileWidth
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);

        // unroll x by cTileWidth
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);

        // unroll x by cTileWidth
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);

        // unroll x by cTileWidth
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);
        z += pCpn^ * pLut^; Inc(pCpn); Inc(pLut);

        if QWeighting then
           z *= cDCTQuantization[cpn, v, u];

        pDCT^ := z;
        Inc(pDCT);
      end;
  end;
end;

function TTilingEncoder.VMirrorPalTile(var ATile: TTile): Boolean; // returns True if tile actually changed
var
  j, i: Integer;
  v, sv: Integer;
begin
  // hardcode vertical mirror into the tile

  Result := False;
  for j := 0 to cTileWidth div 2 - 1  do
    for i := 0 to cTileWidth - 1 do
    begin
      v := ATile.PalPixels[j, i];
      sv := ATile.PalPixels[cTileWidth - 1 - j, i];
      ATile.PalPixels[j, i] := sv;
      if v <> sv then
      begin
        Result := True;
        ATile.PalPixels[cTileWidth - 1 - j, i] := v;
      end;
    end;
end;

function TTilingEncoder.HMirrorPalTile(var ATile: TTile): Boolean; // returns True if tile actually changed
var
  i, j: Integer;
  v, sv: Integer;
begin
  // hardcode horizontal mirror into the tile

  Result := False;
  for j := 0 to cTileWidth - 1 do
    for i := 0 to cTileWidth div 2 - 1  do
    begin
      v := ATile.PalPixels[j, i];
      sv := ATile.PalPixels[j, cTileWidth - 1 - i];
      ATile.PalPixels[j, i] := sv;
      if v <> sv then
      begin
        Result := True;
        ATile.PalPixels[j, cTileWidth - 1 - i] := v;
      end;
    end;
end;

procedure TTilingEncoder.LoadFrame(var AFrame: TFrame; AFrameIndex: Integer; const ABitmap: TRawImage);
var
  i, j, col, ti, tx, ty: Integer;
  pcol: PInteger;
  pfs: PByte;
  TMI: PTileMapItem;
  T: PTile;
begin
  AFrame := TFrame.Create;

  SetLength(AFrame.TileMap, FTileMapHeight, FTileMapWidth);
  SetLength(AFrame.FSPixels, FScreenHeight * FScreenWidth * 3);

  AFrame.Tiles := TTile.Array1DNew(FTileMapSize, True, False);
  for i := 0 to (FTileMapSize - 1) do
  begin
    T := AFrame.Tiles[i];

    T^.Active := True;
    T^.UseCount := 1;
    T^.TmpIndex := -1;
    T^.MergeIndex := -1;
    T^.KFSoleIndex := -1;
  end;

  for j := 0 to (FTileMapHeight - 1) do
    for i := 0 to (FTileMapWidth - 1) do
    begin
      TMI := @AFrame.TileMap[j, i];

      TMI^.TileIdx := AFrameIndex * FTileMapSize + j * FTileMapWidth + i;
      TMI^.PalIdx := -1;
      TMI^.HMirror := False;
      TMI^.VMirror := False;

      TMI^.Smoothed := False;
      TMI^.SmoothedTileIdx := -1;
      TMI^.SmoothedPalIdx := -1;
      TMI^.SmoothedHMirror := False;
      TMI^.SmoothedVMirror := False;

      TMI^.BlendCur := cMaxFTBlend - 1;
      TMI^.BlendPrev := 0;
      TMI^.BlendX := 0;
      TMI^.BlendY := 0;

      TMI^.FTTileDSIdx := -1;
    end;

  Assert(ABitmap.Description.Width >= FScreenWidth, 'Wrong video width!');
  Assert(ABitmap.Description.Height >= FScreenHeight, 'Wrong video height!');

  pfs := @AFrame.FSPixels[0];
  pcol := PInteger(ABitmap.Data);
  for j := 0 to (FScreenHeight - 1) do
  begin
    for i := 0 to (FScreenWidth - 1) do
      begin
        col := pcol^;
        Inc(pcol);

        ti := FTileMapWidth * (j shr cTileWidthBits) + (i shr cTileWidthBits);
        tx := i and (cTileWidth - 1);
        ty := j and (cTileWidth - 1);
        col := SwapRB(col);

        AFrame.Tiles[ti]^.RGBPixels[ty, tx] := col;

        FromRGB(col, pfs[0], pfs[1], pfs[2]);
        Inc(pfs, 3);
      end;
  end;
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
  CShotTransMaxTilesPerKF = 96 * 1920 * 1080 div sqr(cTileWidth); // limiter for the amount of data in a keyframe
  CShotTransEuclideanHiThres = 1.0; // frame equivalent accumulated distance
  CShotTransCorrelLoThres = 0.75; // interframe pearson correlation low limit
  CShotTransGracePeriod = 12; // minimum frames between keyframes
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
      ((i - LastKFIdx) * FTileMapSize > CShotTransMaxTilesPerKF);

    isKf := isKf and ((i - LastKFIdx) > CShotTransGracePeriod);

    if isKf then
    begin
      FKeyFrames[kfIdx] := TKeyFrame.Create(FPaletteCount, 0, 0);
      Inc(kfIdx);

      WriteLn('KF: ', i:8, ' (', kfIdx:3, ') Correlation: ', correl:12:9, ' Euclidean: ', euclidean:12:9);

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
    begin
      TTile.Array1DDispose(FFrames[i].Tiles);
      FreeAndNil(FFrames[i]);
    end;
  SetLength(FFrames, 0);

  for i := 0 to High(FKeyFrames) do
    if Assigned(FKeyFrames[i]) then
      FreeAndNil(FKeyFrames[i]);
  SetLength(FKeyFrames, 0);

  TTile.Array1DDispose(FTiles);
end;

procedure TTilingEncoder.Render;

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
            FromRGB(pal[tilePtr^.PalPixels[tym, txm]], r, g, b)
          else
            FromRGB(tilePtr^.RGBPixels[tym, txm], r, g, b);
        end;

        pr := 0; pg := 255; pb := 255;
        if Assigned(prevtilePtr) and prevtilePtr^.Active then
        begin
          if Assigned(prevPal) then
            FromRGB(prevPal[prevtilePtr^.PalPixels[ptym, ptxm]], pr, pg, pb)
          else
            FromRGB(prevtilePtr^.RGBPixels[ptym, ptxm], pr, pg, pb);
        end;

        r := min((r * blendCur + pr * blendPrev) div (cMaxFTBlend - 1), 255);
        g := min((g * blendCur + pg * blendPrev) div (cMaxFTBlend - 1), 255);
        b := min((b * blendCur + pb * blendPrev) div (cMaxFTBlend - 1), 255);

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
  i, j, sx, sy, ti, frmIdx, prevFrmIdx: Integer;
  p: PInteger;
  prevTilePtr, tilePtr: PTile;
  PsyTile: PTile;
  prevTMItem, TMItem: TTileMapItem;
  Frame: TFrame;
  prevPal, pal: TIntegerDynArray;
  chgDCT: TFloatDynArray3;
  DCT: array[0 .. cTileDCTSize - 1] of TFloat;
  q: TFloat;
begin
  if Length(FFrames) <= 0 then
    Exit;

  Frame := FFrames[FRenderFrameIndex];

  if not Assigned(Frame) or not Assigned(Frame.PKeyFrame) then
    Exit;

  PsyTile := TTile.New(True, False);
  try
    if not FRenderPlaying then
      SetLength(chgDCT, FTileMapHeight, FTileMapWidth, cTileDCTSize);

    // Global

    FRenderTitleText := 'Global: ' + IntToStr(GetGlobalTileCount) + ' / Frame #' + IntToStr(FRenderFrameIndex) + IfThen(Frame.PKeyFrame.StartFrame = FRenderFrameIndex, ' [KF]', '     ') + ' : ' + IntToStr(GetFrameTileCount(Frame));

    // "Input" tab

    if FRenderPage = rpInput then
    begin
      FInputBitmap.BeginUpdate;
      try
        FInputBitmap.Canvas.Brush.Color := clBlack;
        FInputBitmap.Canvas.Brush.Style := bsSolid;
        FInputBitmap.Canvas.FillRect(FInputBitmap.Canvas.ClipRect);
        FInputBitmap.Canvas.Brush.Color := $202020;
        FInputBitmap.Canvas.Brush.Style := bsDiagCross;
        FInputBitmap.Canvas.FillRect(FInputBitmap.Canvas.ClipRect);

        for sy := 0 to FTileMapHeight - 1 do
          for sx := 0 to FTileMapWidth - 1 do
          begin
            tilePtr :=  Frame.Tiles[sy * FTileMapWidth + sx];
            if (FRenderPaletteIndex < 0) or (frame.TileMap[sy, sx].PalIdx = FRenderPaletteIndex) then
              DrawTile(FInputBitmap, sx, sy, nil, tilePtr, nil, False, False, nil, nil, False, False, cMaxFTBlend - 1, 0);
          end;
      finally
        FInputBitmap.EndUpdate;
      end;
    end;

    // "Output" tab

    if (FRenderPage = rpOutput) or not FRenderPlaying then
    begin
      FOutputBitmap.BeginUpdate;
      try
        FOutputBitmap.Canvas.Brush.Color := clBlack;
        FOutputBitmap.Canvas.Brush.Style := bsSolid;
        FOutputBitmap.Canvas.FillRect(FOutputBitmap.Canvas.ClipRect);
        FOutputBitmap.Canvas.Brush.Color := $202020;
        FOutputBitmap.Canvas.Brush.Style := bsDiagCross;
        FOutputBitmap.Canvas.FillRect(FOutputBitmap.Canvas.ClipRect);

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
            if (frmIdx > 0) and (TMItem.BlendCur < cMaxFTBlend - 1) and (TMItem.BlendPrev > 0) then
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
                DrawTile(FOutputBitmap, sx, sy, PsyTile, tilePtr, pal, TMItem.HMirror, TMItem.VMirror, nil, nil, False, False, cMaxFTBlend - 1, 0);
              end;

              if not FRenderPlaying then
                ComputeTilePsyVisFeatures(PsyTile^, False, False, False, False, False, Ord(FRenderUseGamma) * 2 - 1, nil, @chgDCT[sy, sx, 0]);
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
        FTilesBitmap.Canvas.Clear;

        for sy := 0 to FTileMapHeight - 1 do
          for sx := 0 to FTileMapWidth - 1 do
          begin
            ti := FTileMapWidth * sy + sx + FTileMapSize * FRenderTilePage;

            if InRange(ti, 0, High(FTiles)) then
            begin
              tilePtr := FTiles[ti];
              pal := Frame.PKeyFrame.PaletteRGB[Max(0, FRenderPaletteIndex)];

              DrawTile(FTilesBitmap, sx, sy, nil, tilePtr, pal, False, False, nil, nil, False, False, cMaxFTBlend - 1, 0);
            end;
          end;
      finally
        FTilesBitmap.EndUpdate;
      end;
    end;

    if not FRenderPlaying then
    begin
      q := 0.0;
      i := 0;
      for sy := 0 to FTileMapHeight - 1 do
        for sx := 0 to FTileMapWidth - 1 do
        begin
          tilePtr :=  Frame.Tiles[sy * FTileMapWidth + sx];
          if (FRenderPaletteIndex < 0) or (frame.TileMap[sy, sx].PalIdx = FRenderPaletteIndex) then
          begin
            ComputeTilePsyVisFeatures(tilePtr^, False, False, False, False, False, Ord(FRenderUseGamma) * 2 - 1, nil, @DCT[0]);
            q += CompareEuclideanDCT(DCT, chgDCT[sy, sx]);
            Inc(i);
          end;
        end;
      if i <> 0 then
        q /= i;

      FRenderPsychoVisualQuality := q;
    end;
  finally
    TTile.Dispose(PsyTile);
  end;
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
  t: Int64;
  ProgressPosition, ProgressStepPosition, ProgressMax: Integer;
  ProgressHourGlass: Boolean;
begin
  scalable_allocation_command(TBBMALLOC_CLEAN_ALL_BUFFERS, nil); // force the mem allocator to release unused memory

  if AProgressStep <> esNone then // new step?
  begin
    FProgressStep := AProgressStep;
    FProgressPrevTime := GetTickCount64;
  end;

  if (ASubStepIdx < 0) and (AProgressStep = esNone) then // reset
  begin
    FProgressStep := esNone;
    FProgressPrevTime := GetTickCount64;
    FProgressStartTime := FProgressPrevTime;
  end;

  ProgressMax := (Ord(High(TEncoderStep)) + 1) * cProgressMul;
  ProgressPosition := Ord(FProgressStep) * cProgressMul;

  ProgressStepPosition := 0;
  if ASubStepIdx >= 0 then
    ProgressStepPosition := iDiv0(ASubStepIdx * cProgressMul, cEncoderStepLen[FProgressStep]);

  ProgressHourGlass := (AProgressStep <> esNone) and (ASubStepIdx < cEncoderStepLen[FProgressStep]);

  t := GetTickCount64;
  if ASubStepIdx >= 0 then
  begin
    WriteLn('Step: ', Copy(GetEnumName(TypeInfo(TEncoderStep), Ord(FProgressStep)), 3), ' / ', ProgressStepPosition,
      #9'Time: ', FormatFloat('0.000', (t - FProgressPrevTime) / 1000), #9'All: ', FormatFloat('0.000', (t - FProgressStartTime) / 1000));
  end;
  FProgressPrevTime := t;

  if Assigned(FOnProgress) then
    FOnProgress(Self, ProgressPosition + ProgressStepPosition, ProgressMax, ProgressHourGlass);
end;

function TTilingEncoder.GetGlobalTileCount: Integer;
var i: Integer;
begin
  Result := 0;
  for i := 0 to High(FTiles) do
    if FTiles[i]^.Active then
      Inc(Result);
end;

function TTilingEncoder.GetFrameTileCount(AFrame: TFrame): Integer;
var
  Used: TIntegerDynArray;
  i, j: Integer;
begin
  Result := 0;

  if Length(FTiles) = 0 then
    Exit;

  SetLength(Used, Length(FTiles));
  FillDWord(Used[0], Length(FTiles), 0);

  for j := 0 to FTileMapHeight - 1 do
    for i := 0 to FTileMapWidth - 1 do
      Used[AFrame.TileMap[j, i].TileIdx] := 1;

  for i := 0 to High(Used) do
    Inc(Result, Used[i]);
end;

function TTilingEncoder.GetTileIndexTMItem(ATileIndex: Integer; out AFrame: TFrame): PTileMapItem;
var
  frmIdx, int, sy, sx: Integer;
begin
  DivMod(ATileIndex, FTileMapSize, frmIdx, int);
  DivMod(int, FTileMapWidth, sy, sx);

  Result := @FFrames[frmIdx].TileMap[sy, sx];
  AFrame := FFrames[frmIdx];

  Assert(Result^.TileIdx = ATileIndex);
end;

procedure TTilingEncoder.MergeTiles(const TileIndexes: array of Integer; TileCount: Integer; BestIdx: Integer;
  NewTile: PPalPixels; NewTileRGB: PRGBPixels);
var
  j, k: Integer;
begin
  if TileCount <= 0 then
    Exit;

  if Assigned(NewTile) then
    FTiles[BestIdx]^.CopyPalPixels(NewTile^);

  if Assigned(NewTileRGB) then
    FTiles[BestIdx]^.CopyRGBPixels(NewTileRGB^);

  for k := 0 to TileCount - 1 do
  begin
    j := TileIndexes[k];

    if j = BestIdx then
      Continue;

    Inc(FTiles[BestIdx]^.UseCount, FTiles[j]^.UseCount);

    FTiles[j]^.Active := False;
    FTiles[j]^.MergeIndex := BestIdx;

    FTiles[j]^.ClearPixels;
  end;
end;

procedure TTilingEncoder.InitMergeTiles;
var
  i: Integer;
begin
  for i := 0 to High(FTiles) do
    FTiles[i]^.MergeIndex := -1;
end;

procedure TTilingEncoder.FinishMergeTiles;
var
  i, j, k, idx: Integer;
begin
  for k := 0 to High(FFrames) do
    for j := 0 to (FTileMapHeight - 1) do
      for i := 0 to (FTileMapWidth - 1) do
      begin
        idx := FTiles[FFrames[k].TileMap[j, i].TileIdx]^.MergeIndex;
        if idx >= 0 then
          FFrames[k].TileMap[j, i].TileIdx := idx;
      end;
end;

procedure TTilingEncoder.PrepareKFTiling(AKF: TKeyFrame; APaletteIndex, AFTGamma: Integer);
var
  DS: PTilingDataset;

  function DoPsyV: Integer;
  var
    di, ti, hvmir: Integer;
    T: PTile;
  begin
    di := 0;
    for ti := 0 to High(FTiles) do
    begin
      T := FTiles[ti];

      if T^.Active then
      begin
        for hvmir := 0 to 3 do
        begin
          DS^.TDToTileIdx[di] := ti;
          DS^.TDToAttrs[di] := hvmir;

          ComputeTilePsyVisFeatures(T^, True, False, cFTQWeighting, hvmir and 1 <> 0, hvmir and 2 <> 0, AFTGamma, AKF.PaletteRGB[APaletteIndex], @DS^.Dataset[di * cTileDCTSize]);

          // prune redundancies
          if (di <= 0) or (CompareDWord(DS^.Dataset[(di - 1) * cTileDCTSize], DS^.Dataset[di * cTileDCTSize], cTileDCTSize) <> 0) then
            Inc(di);
        end;
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
  InterLockedDecrement(AKF.FTPalettesLeft);

  Write('.');

  if AKF.FTPalettesLeft <= 0 then
  begin
    WriteLn;
    WriteLn('KF: ', AKF.StartFrame:8, #9'ResidualErr: ', (AKF.FTErrCml / AKF.FrameCount):12:3, ' (by frame) ', (AKF.FTErrCml / (FTileMapSize * AKF.FrameCount)):12:6, ' (by tile)');
  end;
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

procedure TTilingEncoder.DoKFTiling(AKF: TKeyFrame; APaletteIndex: Integer; AFTGamma: Integer; AFTBlend: Integer;
  AFTBlendThres: TFloat);
var
  i, frmIdx, bestIdx, idx, annQueryCount, annQueryPos, blend: Integer;
  attrs, bestBlendCur, bestBlendPrev: Byte;
  x, y, ox, oy, bestX, bestY: Integer;
  errCml, bestErr, fc, rfc, err: TFloat;

  Frame: TFrame;
  tmiO, prevTMI: PTileMapItem;

  DS: PTilingDataset;
  idxs: TIntegerDynArray;
  errs: TANNFloatDynArray;
  DCTs: TFloatDynArray;
  SearchDCT: array[0 .. cTileDCTSize - 1] of TFloat;
  PrevDCT: array[0 .. cTileDCTSize - 1] of TFloat;

  PrevKFPaletteDone: TBooleanDynArray;
begin
  DS := AKF.TileDS[APaletteIndex];
  if Length(DS^.Dataset) < cTileDCTSize then
    Exit;

  errCml := 0.0;
  SetLength(PrevKFPaletteDone, FPaletteCount);

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

  SetLength(DCTs, annQueryCount * cTileDCTSize);
  SetLength(idxs, annQueryCount);
  SetLength(errs, annQueryCount);

  // prepare KNN queries

  annQueryCount := 0;
  for frmIdx := AKF.StartFrame to AKF.EndFrame do
  begin
    Frame := FFrames[frmIdx];

    for y := 0 to FTileMapHeight - 1 do
      for x := 0 to FTileMapWidth - 1 do
      begin
        if Frame.TileMap[y, x].PalIdx <> APaletteIndex then
          Continue;

        ComputeTilePsyVisFeatures(Frame.Tiles[y * FTileMapWidth + x]^, False, False, cFTQWeighting, False, False, AFTGamma, nil, @DCTs[annQueryCount * cTileDCTSize]);

        Inc(annQueryCount);
      end;
  end;

  // query KNN

  flann_find_nearest_neighbors_index(DS^.FLANN, @DCTs[0], annQueryCount, @idxs[0], @errs[0], 1, DS^.FLANNParams);

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

        bestIdx := idxs[annQueryPos];
        bestErr := errs[annQueryPos];
        bestBlendCur := cMaxFTBlend - 1;
        bestBlendPrev := 0;
        bestX := x;
        bestY := y;

        if (AFTBlend >= 0) and not IsZero(errs[annQueryPos], AFTBlendThres) then
        begin

          // try to blend a local tile of the previous frame to improve likeliness

          for oy := y - AFTBlend to y + AFTBlend do
          begin
            if not InRange(oy, 0, FTileMapHeight - 1) then
              Continue;

            for ox := x - AFTBlend to x + AFTBlend do
            begin
              if not InRange(ox, 0, FTileMapWidth - 1) then
                Continue;

              if Frame.Index > 0 then
              begin
                prevTMI := @FFrames[Frame.Index - 1].TileMap[oy, ox];

                if prevTMI^.PalIdx <> APaletteIndex then
                  Continue;

                if FFrames[Frame.Index - 1].PKeyFrame <> Frame.PKeyFrame then
                begin
                  if not PrevKFPaletteDone[prevTMI^.PalIdx] then
                  begin
                    WaitForSingleObject(FFrames[Frame.Index - 1].PKeyFrame.FTPaletteDoneEvent[prevTMI^.PalIdx], INFINITE); // wait prev keyframe for palette done
                    PrevKFPaletteDone[prevTMI^.PalIdx] := True;
                  end;

                  ComputeTilePsyVisFeatures(FTiles[prevTMI^.TileIdx]^, True, False, cFTQWeighting, prevTMI^.HMirror, prevTMI^.VMirror, AFTGamma, FFrames[Frame.Index - 1].PKeyFrame.PaletteRGB[prevTMI^.PalIdx], @PrevDCT[0]);
                end
                else
                begin
                  for i := 0 to cTileDCTSize - 1 do
                    PrevDCT[i] := DS^.Dataset[prevTMI^.FTTileDSIdx * cTileDCTSize + i];
                end;
              end
              else
              begin
                if (ox <> x) or (oy <> y) then
                  Continue;

                FillDWord(PrevDCT[0], cTileDCTSize, 0);
              end;

              for blend := 1 to cMaxFTBlend - 2 do
              begin
                fc := blend * (1.0 / (cMaxFTBlend - 1));
                rfc := 1.0 / fc;

                ComputeBlending_Asm(@SearchDCT[0], @PrevDCT[0], @DCTs[annQueryPos * cTileDCTSize], 1.0 - rfc, rfc);

                flann_find_nearest_neighbors_index(DS^.FLANN, @SearchDCT[0], 1, @idx, @err, 1, DS^.FLANNParams);

                err := sqr(sqrt(err)*fc);

                if err < bestErr then
                begin
                  bestErr := err;
                  bestIdx := idx;
                  bestBlendCur := blend;
                  bestBlendPrev := cMaxFTBlend - 1 - blend;
                  bestX := ox;
                  bestY := oy;
                end;
              end;
            end;
          end;
        end;

        tmiO := @FFrames[Frame.Index].TileMap[y, x];

        attrs := DS^.TDToAttrs[bestIdx];
        tmiO^.FTTileDSIdx := bestIdx;
        tmiO^.TileIdx := DS^.TDToTileIdx[bestIdx];
        tmiO^.HMirror := (attrs and 1) <> 0;
        tmiO^.VMirror := (attrs and 2) <> 0;
        tmiO^.BlendCur := bestBlendCur;
        tmiO^.BlendPrev := bestBlendPrev;
        tmiO^.BlendX := bestX - x;
        tmiO^.BlendY := bestY - y;

        errCml += bestErr;

        InterLockedIncrement(FTiles[tmiO^.TileIdx]^.UseCount);

        Inc(annQueryPos);
      end;
  end;

  SpinEnter(@FLock);
  Frame.PKeyFrame.FTErrCml += errCml;
  SpinLeave(@FLock);
end;

procedure TTilingEncoder.DoTemporalSmoothing(AFrame, APrevFrame: TFrame; Y: Integer; Strength: TFloat;
  AddlTilesThres: TFloat; NonAddlCount: Integer);
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
  for sx := 0 to FTileMapWidth - 1 do
  begin
    PrevTMI := @APrevFrame.TileMap[Y, sx];
    ComputeTilePsyVisFeatures(FTiles[PrevTMI^.SmoothedTileIdx]^, True, False, True, PrevTMI^.SmoothedHMirror, PrevTMI^.SmoothedVMirror, -1, APrevFrame.PKeyFrame.PaletteRGB[PrevTMI^.SmoothedPalIdx], @PrevDCT[0]);

    // prevent smoothing from crossing keyframes (to ensure proper seek)

    if AFrame.PKeyFrame = APrevFrame.PKeyFrame then
    begin
      // compare DCT of current tile with tile from prev frame tilemap

      TMI := @AFrame.TileMap[Y, sx];
      ComputeTilePsyVisFeatures(FTiles[TMI^.SmoothedTileIdx]^, True, False, True, TMI^.SmoothedHMirror, TMI^.SmoothedVMirror, -1, AFrame.PKeyFrame.PaletteRGB[TMI^.SmoothedPalIdx], @DCT[0]);

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

    ComputeTilePsyVisFeatures(APrevFrame.Tiles[Y * FTileMapWidth + sx]^, False, False, True, False, False, -1, nil, @PrevPlainDCT[0]);

    cmp := CompareEuclideanDCT(PrevDCT, PrevPlainDCT);
    cmp := sqrt(cmp * cSqrtFactor);

    if (AddlTilesThres <> 0.0) and (cmp > AddlTilesThres) then
    begin
      ATList := FAdditionalTiles.LockList;
      try
        addlTile := TTile.New(True, True);
        addlTile^.CopyFrom(APrevFrame.Tiles[Y * FTileMapWidth + sx]^);
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
end;

function TTilingEncoder.GetTileUseCount(ATileIndex: Integer): Integer;
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
begin
  Result := 0;
  for j := y to y + h - 1 do
    for i := x to x + w - 1 do
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

type
  TKModesBin = record
    TileBin: TIntegerDynArray;
    ClusterCount: Integer;
    DatasetLength: Integer;
  end;

  PKModesBin = ^TKModesBin;
  TKModesBinArray = array of TKModesBin;
  PKModesBinArray = ^TKModesBinArray;

procedure TTilingEncoder.DoGlobalKMeans(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
var
  YakmoDataset, YakmoCentroids: TFloatDynArray2;
  Clusters, YakmoClusters: TIntegerDynArray;
  i, bin, cnt, lineIdx, clusterIdx, clusterLineCount, DSLen, clusterLineIdx, BIRCHClusterCount: Integer;
  v, best: TFloat;
  ToMerge: TFloatDynArray2;
  ToMergeIdxs: TIntegerDynArray;
  KMBin: PKModesBin;
  Yakmo: PYakmoSingle;
  BIRCH: PBIRCH;
  Dataset: TFloatDynArray2;
  TileIndices: TIntegerDynArray;
begin
  KMBin := @PKModesBinArray(AData)^[AIndex];
  DSLen := KMBin^.DatasetLength;
  if (DSLen <= KMBin^.ClusterCount) or (KMBin^.ClusterCount <= 1) then
    Exit;

  // build bin dataset

  SetLength(TileIndices, DSLen);
  SetLength(Dataset, DSLen, cTileDCTSize);
  cnt := 0;
  for i := 0 to High(FTiles) do
  begin
    bin := KMBin^.TileBin[i];

    if bin <> AIndex then
      Continue;

    ComputeTilePsyVisFeatures(FTiles[i]^, False, False, False, False, False, -1, nil, @Dataset[cnt, 0]);

    TileIndices[cnt] := i;
    Inc(cnt);
  end;
  Assert(cnt = DSLen);

  // cluster dataset using BIRCH + Yakmo

  SetLength(Clusters, DSLen);

  BIRCH := birch_create(0.001, 64 * 1024 * 1024);
  for i := 0 to DSLen - 1 do
    birch_insert_line(BIRCH, @Dataset[i, 0]);
  birch_get_results(BIRCH, @Clusters[0]);
  birch_destroy(BIRCH);
  BIRCHClusterCount := MaxIntValue(Clusters) + 1;

  WriteLn('Bin: ', AIndex:2, ' DatasetSize: ', DSLen:8, ' BIRCHClusterCount: ', BIRCHClusterCount:6);

  SetLength(YakmoClusters, BIRCHClusterCount);
  SetLength(YakmoDataset, BIRCHClusterCount, cTileDCTSize);
  SetLength(YakmoCentroids, KMBin^.ClusterCount, cTileDCTSize);

  for clusterIdx := 0 to BIRCHClusterCount - 1 do
  begin
    clusterLineCount := 0;
    for lineIdx := 0 to DSLen - 1 do
      if Clusters[lineIdx] = clusterIdx then
      begin
        for i := 0 to cTileDCTSize - 1 do
          YakmoDataset[clusterIdx, i] += Dataset[lineIdx, i];
        Inc(clusterLineCount)
      end;

    if clusterLineCount > 0 then
      for i := 0 to cTileDCTSize - 1 do
        YakmoDataset[clusterIdx, i] /= clusterLineCount;
  end;

  if BIRCHClusterCount > KMBin^.ClusterCount then
  begin
    Yakmo := yakmo_single_create(KMBin^.ClusterCount, 1, MaxInt, 1, 0, 0, 0);
    yakmo_single_load_train_data(Yakmo, BIRCHClusterCount, cTileDCTSize, @YakmoDataset[0]);
    yakmo_single_train_on_data(Yakmo, @YakmoClusters[0]);
    yakmo_single_get_centroids(Yakmo, @YakmoCentroids[0]);
    yakmo_single_destroy(Yakmo);

    Assert(MaxIntValue(YakmoClusters) = KMBin^.ClusterCount - 1);
  end
  else
  begin
    for i := 0 to BIRCHClusterCount - 1 do
      YakmoClusters[i] := i;
  end;

  SetLength(ToMerge, DSLen);
  SetLength(ToMergeIdxs, DSLen);

  // build a list of this centroid tiles

  for clusterIdx := 0 to KMBin^.ClusterCount - 1 do
  begin
    clusterLineCount := 0;
    for lineIdx := 0 to High(TileIndices) do
    begin
      if YakmoClusters[Clusters[lineIdx]] = clusterIdx then
      begin
        ToMerge[clusterLineCount] := Dataset[lineIdx];
        ToMergeIdxs[clusterLineCount] := TileIndices[lineIdx];
        Inc(clusterLineCount);
      end;
    end;

    // choose a tile from the cluster

    if clusterLineCount >= 2 then
    begin
      best := MaxSingle;
      clusterLineIdx := -1;
      for i := 0 to clusterLineCount - 1 do
      begin
        v := CompareEuclideanDCT(ToMerge[i], YakmoCentroids[clusterIdx]);
        if v < best then
        begin
          best := v;
          clusterLineIdx := i;
        end;
      end;

      SpinEnter(@FLock);
      MergeTiles(ToMergeIdxs, clusterLineCount, ToMergeIdxs[clusterLineIdx], nil, nil);
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
  i, disCnt, bin: Integer;
  cnt: TIntegerDynArray;
  best: TIntegerDynArray;
  share: TFloat;
  TileBin: TIntegerDynArray;
begin

  FConcurrentKModesBins := Sqr(cTileWidth) shr cBinCountShift;
  SetLength(KMBins, FConcurrentKModesBins);
  SetLength(cnt, Length(KMBins));
  SetLength(best, Length(KMBins));

  // precompute dataset size

  SetLength(TileBin, Length(FTiles));
  FillDWord(TileBin[0], Length(TileBin), DWORD(-1));
  FillDWord(cnt[0], Length(cnt), 0);
  for i := 0 to High(FTiles) do
  begin
    if not FTiles[i]^.Active then
      Continue;

    bin := GetTilePalZoneThres(FTiles[i]^, sqr(cTileWidth), nil);
    bin := bin shr cBinCountShift;

    TileBin[i] := bin;

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
    KMBins[i].ClusterCount := ceil(cnt[i] * share);

  InitMergeTiles;

  ProgressRedraw(1);

  // run the clustering algorithm, which will group similar tiles until it reaches a fixed amount of groups

  ProcThreadPool.DoParallel(@DoGlobalKMeans, 0, High(KMBins), @KMBins);

  ProgressRedraw(2);

  FinishMergeTiles;

  // ensure inter block tile unicity

  InitMergeTiles;
  MakeTilesUnique(0, Length(FTiles));
  FinishMergeTiles;

  ProgressRedraw(3);

  // put most probable tiles first and remove unused tiles

  ReindexTiles;

  ProgressRedraw(4);
end;

procedure TTilingEncoder.ReloadPreviousTiling(AFN: String);
var
  ParallelCount: PtrUInt;
  KNNDataset: TANNFloatDynArray;
  FLANN: flann_index_t;
  FLANNParameters: TFLANNParameters;

  procedure DoFindBest(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    last, bin, i, tidx: Integer;
    DataLine: array[0 .. sqr(cTileWidth) - 1] of TANNFloat;
    err: TANNFloat;
  begin
    if not InRange(AIndex, 0, ParallelCount - 1) then
      Exit;

    bin := Length(FTiles) div ParallelCount;
    last := (AIndex + 1) * bin - 1;
    if AIndex >= ParallelCount - 1 then
      last := High(FTiles);

    for i := bin * AIndex to last do
    begin
      if FTiles[i]^.Active then
      begin
        FTiles[i]^.ExtractPalPixels(DataLine);

        flann_find_nearest_neighbors_index(FLANN, @DataLine[0], 1, @tidx, @err, 1, @FLANNParameters);

        FTiles[i]^.LoadPalPixels(@KNNDataset[tidx * Sqr(cTileWidth)]);
      end;

      if i mod 10000 = 0 then
        WriteLn('Thread: ', GetCurrentThreadId, #9'TileIdx: ', i);
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

    ProgressRedraw(3);

    InitMergeTiles;
    MakeTilesUnique(0, Length(FTiles));
    FinishMergeTiles;

    ProgressRedraw(4);
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

procedure TTilingEncoder.ReindexTiles;
var
  i, j, x, y, cnt, idx: Integer;
  IdxMap: TIntegerDynArray;
  Frame: ^TFrame;
  LocTiles: PTileDynArray;
begin
  cnt := 0;
  for i := 0 to High(FTiles) do
  begin
    FTiles[i]^.KFSoleIndex := -1;
    FTiles[i]^.TmpIndex := i;
    if (FTiles[i]^.Active) and (FTiles[i]^.UseCount > 0) then
      Inc(cnt);
  end;

  // pack the global Tiles, removing inactive ones

  LocTiles := TTile.Array1DNew(cnt, False, True);
  j := 0;
  for i := 0 to High(FTiles) do
    if (FTiles[i]^.Active) and (FTiles[i]^.UseCount > 0) then
    begin
      LocTiles[j]^.CopyFrom(FTiles[i]^);
      Inc(j);
    end;

  SetLength(IdxMap, Length(FTiles));
  FillDWord(IdxMap[0], Length(FTiles), $ffffffff);

  TTile.Array1DDispose(FTiles);
  FTiles := LocTiles;
  LocTiles := nil;

  // sort global Tiles by use count descending (to make smoothing work better) then by tile index (to make tile indexes compression work better)

  QuickSort(FTiles[0], 0, High(FTiles), SizeOf(PTile), @CompareTileUseCountRev);

  // point tilemap items on new Tiles indexes

  for i := 0 to High(FTiles) do
    IdxMap[FTiles[i]^.TmpIndex] := i;

  for i := 0 to High(FFrames) do
  begin
    Frame := @FFrames[i];
    for y := 0 to (FTileMapHeight - 1) do
      for x := 0 to (FTileMapWidth - 1) do
      begin
        idx := Frame^.TileMap[y,x].SmoothedTileIdx;
        if idx >= 0 then
          Frame^.TileMap[y,x].SmoothedTileIdx := IdxMap[idx];

        idx := Frame^.TileMap[y,x].TileIdx;
        if idx >= 0 then
        begin
          idx := IdxMap[idx];

          Frame^.TileMap[y,x].TileIdx := idx;

          if idx >= 0 then
          begin
            if FTiles[idx]^.KFSoleIndex < 0 then
              FTiles[idx]^.KFSoleIndex := Frame^.PKeyFrame.StartFrame
            else if FTiles[idx]^.KFSoleIndex <> Frame^.PKeyFrame.StartFrame then
              FTiles[idx]^.KFSoleIndex := MaxInt;
          end;
        end;
      end;
  end;

  // in case a tile is used by only one keyframe, make it intra keyframe (ie. located in the corresponging keyframe)

  cnt := 0;
  for i := 0 to High(FTiles) do
  begin
    FTiles[i]^.IntraKF := FTiles[i]^.KFSoleIndex <> MaxInt;
    if FTiles[i]^.IntraKF then
      Inc(cnt);
  end;
  WriteLn('ReindexTiles: ', cnt, ' / ', Length(FTiles), ' made intra');
end;

procedure TTilingEncoder.SaveStream(AStream: TStream; AFTBlend: Integer);
const
  CGTMCommandsCount = Ord(High(TGTMCommand)) + 1;
  CGTMCommandsBits = round(ln(CGTMCommandsCount) / ln(2));
  CGTMAttributeBits = 16 - CGTMCommandsBits;
  CMinBlkSkipCount = 1;
  CMaxBlkSkipCount = 1 shl CGTMAttributeBits;

var
  ZStream: TMemoryStream;

  procedure DoDWord(v: Cardinal);
  begin
    ZStream.WriteDWord(v);
  end;

  procedure Do3Bytes(v: Cardinal);
  begin
    Assert(v < 1 shl 24);
    ZStream.WriteByte(v and $ff);
    v := v shr 8;
    ZStream.WriteByte(v and $ff);
    v := v shr 8;
    ZStream.WriteByte(v and $ff);
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
    assert(Data < (1 shl CGTMAttributeBits));
    assert(Ord(Cmd) < CGTMCommandsCount);

    DoWord((Data shl CGTMCommandsBits) or Ord(Cmd));
  end;

  procedure DoTMI(TMI: TTileMapItem);
  var
    attrs, blend: Word;
    isBlend: Boolean;
  begin
    Assert((TMI.SmoothedPalIdx >= 0) and (TMI.SmoothedPalIdx < FPaletteCount));

    attrs := (TMI.SmoothedPalIdx shl 2) or (Ord(TMI.SmoothedVMirror) shl 1) or Ord(TMI.SmoothedHMirror);
    blend := (Word(TMI.BlendY and $f) shl 12) or (Word(TMI.BlendX and $f) shl 8) or (Word(TMI.BlendPrev and $f) shl 4) or Word(TMI.BlendCur and $f);
    isBlend := (AFTBlend < 0) or ((TMI.BlendPrev = 0) and (TMI.BlendCur = cMaxFTBlend - 1)) and not FTiles[TMI.TileIdx]^.Additional;

    if TMI.TileIdx < (1 shl 16) then
    begin
      if isBlend then
      begin
        DoCmd(gtShortTileIdx, attrs);
        DoWord(TMI.SmoothedTileIdx);
      end
      else
      begin
        DoCmd(gtShortBlendTileIdx, attrs);
        DoWord(TMI.SmoothedTileIdx);
        DoWord(blend);
      end;
    end
    else
    begin
      if isBlend then
      begin
        DoCmd(gtLongTileIdx, attrs);
        DoDWord(TMI.SmoothedTileIdx);
      end
      else
      begin
        DoCmd(gtLongBlendTileIdx, attrs);
        DoDWord(TMI.SmoothedTileIdx);
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
    i: Integer;
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
        if not FTiles[i]^.IntraKF then
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
  Header.EncoderVersion := 1;
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

          WriteLn('KF: ', FKeyFrames[kf].StartFrame:8, #9'FCnt: ', KFCount, #9'Written: ', KFSize, #9'Bitrate: ', FormatFloat('0.00', KFSize / 1024.0 * 8.0 / KFCount) + ' kbpf  '#9'(' + FormatFloat('0.00', KFSize / 1024.0 * 8.0 / KFCount * FFramesPerSecond)+' kbps)');
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

  WriteLn('Written: ', StreamSize, #9'Bitrate: ', FormatFloat('0.00', StreamSize / 1024.0 * 8.0 / Length(FFrames)) + ' kbpf  '#9'(' + FormatFloat('0.00', StreamSize / 1024.0 * 8.0 / Length(FFrames) * FFramesPerSecond)+' kbps)');
end;

procedure TTilingEncoder.SaveRawTiles(OutFN: String);
var
  fs: TFileStream;
  i: Integer;
begin
  // save raw tiles

  if DirectoryExists(ExtractFilePath(OutFN)) then
  begin
    fs := TFileStream.Create(OutFN, fmCreate or fmShareDenyWrite);
    try
      fs.WriteByte(0); // version
      fs.WriteByte(FPaletteSize);
      for i := 0 to High(FTiles) do
        if FTiles[i]^.Active then
        begin
          fs.WriteDWord(FTiles[i]^.UseCount);
          fs.Write(FTiles[i]^.GetPalPixelsPtr^[0, 0], sqr(cTileWidth));
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
  //ProcThreadPool.MaxThreadCount := 1;
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

