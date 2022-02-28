unit main;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

{$define ASM_DBMP}

interface

uses
  windows, Classes, SysUtils, strutils, types, Math, FileUtil, typinfo, zstream, Process, LazLogger,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Spin, Menus, IntfGraphics, FPimage, FPCanvas, FPWritePNG, fgl,
  MTProcs, kmodes, extern, typ, sle;

type
  TEncoderStep = (esNone = -1, esLoad = 0, esDither, esMakeUnique, esGlobalTiling, esFrameTiling, esReindex, esSmooth, esSave);
  TFTQuality = (ftFast, ftMedium, ftSlow);

const
  // tweakable constants

  cBitsPerComp = 8;
  cRandomKModesCount = 7;
  cFTIntraPaletteTol: array[TFTQuality] of TFloat = (0.01, 0.05, 0.2);
  cFTInterPaletteTol: array[TFTQuality] of TFloat = (0.001, 0.005, 0.02);
  cMaxFTBlend = 16;
  cGlobalFTBucketSize = 8;
  cFrameFTBucketSize = 8;

{$if true}
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
  cRGBBitsPerComp = 8;
  cRGBColors = 1 shl (cRGBBitsPerComp * 3);
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

  cEncoderStepLen: array[TEncoderStep] of Integer = (0, 3, 3, 1, 4, 3, 2, 2, 2);

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
  PRGBPixels = ^TRGBPixels;
  PPalPixels = ^TPalPixels;

  TTile = packed record // /!\ update TTileHelper.CopyFrom each time this structure is changed /!\
    UseCount, TmpIndex, MergeIndex, OriginalReloadedIndex, DitheringPalIndex, KFSoleIndex: Integer;
    Active, IntraKF, HasRGBPixels, HasPalPixels: Boolean;
  end;

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
    procedure ExtractPalPixels(var AArray: array of TANNFloat);
    function ComparePalPixelsTo(const ATile: TTile): Integer;

    property RGBPixels[y, x: Integer]: Integer read GetRGBPixels write SetRGBPixels;
    property PalPixels[y, x: Integer]: Byte read GetPalPixels write SetPalPixels;
  end;

  PTileMapItem = ^TTileMapItem;

  TTileMapItem = packed record
    TileIdx, SmoothedTileIdx: Integer;
    PalIdx, SmoothedPalIdx: Integer;
    Smoothed, HMirror, VMirror, SmoothedHMirror, SmoothedVMirror: Boolean;
    BlendCur, BlendPrev: Byte;
    BlendX, BlendY: ShortInt;
  end;

  TTileMapItems = array of TTileMapItem;

  TTilingDataset = record
    Dataset: TANNFloatDynArray2;
    TRToTileIdx: TIntegerDynArray;
    TRToPalIdx: TByteDynArray;
    TRToAttrs: TByteDynArray;
    KDT: PANNkdtree;
    DistErrCml: TFloat;
    DistErrCnt: Integer;
  end;

  PTilingDataset = ^TTilingDataset;

  TCountIndexArray = packed record
    Count, Index: Integer;
    Hue, Sat, Val, Luma: Byte;
  end;

  PCountIndexArray = ^TCountIndexArray;

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

    FTYDone: TBooleanDynArray;
  end;

  { TKeyFrame }

  TKeyFrame = class
    StartFrame, EndFrame, FrameCount: Integer;
    FramesLeft: Integer;
    TileDS: array of PTilingDataset;
    CS: TRTLCriticalSection;
    FTDoPrepareEvent, FTDoFinishEvent, FTPreparedEvent: THandle;
    MixingPlans: array of TMixingPlan;
    PaletteRGB: TIntegerDynArray2;
    PaletteCentroids: TFloatDynArray2;

    PaletteUseCount: array of record
      UseCount: Integer;
      Palette: TIntegerDynArray;
      PalIdx: Integer;
    end;

    constructor Create(APaletteCount, AStartFrame, AEndFrame: Integer);
    destructor Destroy; override;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    btnGTS: TButton;
    btnInput: TButton;
    btnGTM: TButton;
    btnRunAll: TButton;
    cbxFTQ: TComboBox;
    cbxScaling: TComboBox;
    cbxEndStep: TComboBox;
    cbxPalCount: TComboBox;
    cbxStartStep: TComboBox;
    cbxYilMix: TComboBox;
    cbxPalSize: TComboBox;
    cbxDLBPC: TComboBox;
    chkFTGamma: TCheckBox;
    chkUseKMQuant: TCheckBox;
    chkUseWL: TCheckBox;
    chkGamma: TCheckBox;
    chkDitheringGamma: TCheckBox;
    chkReduced: TCheckBox;
    chkMirrored: TCheckBox;
    chkBlended: TCheckBox;
    chkPlay: TCheckBox;
    chkReload: TCheckBox;
    chkUseTK: TCheckBox;
    edInput: TEdit;
    edOutput: TEdit;
    edReload: TEdit;
    From: TLabel;
    imgDest: TImage;
    imgPalette: TImage;
    imgSource: TImage;
    imgTiles: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    lblPct: TLabel;
    MenuItem8: TMenuItem;
    odFFInput: TOpenDialog;
    pbFrameO: TPaintBox;
    pbFrameI: TPaintBox;
    pbProgress: TProgressBar;
    pcPages: TPageControl;
    pnLbl: TPanel;
    sbTiles: TScrollBox;
    sdGTM: TSaveDialog;
    sdGTS: TSaveDialog;
    seQbTiles: TFloatSpinEdit;
    seFTBlend: TSpinEdit;
    seVisGamma: TFloatSpinEdit;
    seFrameCount: TSpinEdit;
    seMaxTiles: TSpinEdit;
    sePage: TSpinEdit;
    seAddlTiles: TFloatSpinEdit;
    seStartFrame: TSpinEdit;
    seTempoSmoo: TFloatSpinEdit;
    seEncGamma: TFloatSpinEdit;
    tsTilesPal: TTabSheet;
    To1: TLabel;
    tsSettings: TTabSheet;
    tsInput: TTabSheet;
    tsOutput: TTabSheet;
    Label5: TLabel;
    Label8: TLabel;
    lblCorrel: TLabel;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    miLoad: TMenuItem;
    MenuItem1: TMenuItem;
    pmProcesses: TPopupMenu;
    PopupMenu1: TPopupMenu;
    sedPalIdx: TSpinEdit;
    IdleTimer: TIdleTimer;
    tbFrame: TTrackBar;

    // processes
    procedure btnLoadClick(Sender: TObject);
    procedure btnDitherClick(Sender: TObject);
    procedure btnDoMakeUniqueClick(Sender: TObject);
    procedure btnDoGlobalTilingClick(Sender: TObject);
    procedure btnDoFrameTilingClick(Sender: TObject);
    procedure btnReindexClick(Sender: TObject);
    procedure btnSmoothClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);

    procedure btnGTMClick(Sender: TObject);
    procedure btnGTSClick(Sender: TObject);
    procedure btnInputClick(Sender: TObject);
    procedure btnRunAllClick(Sender: TObject);
    procedure btnDebugClick(Sender: TObject);
    procedure cbxYilMixChange(Sender: TObject);
    procedure chkUseTKChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure IdleTimerTimer(Sender: TObject);
    procedure imgPaletteClick(Sender: TObject);
    procedure imgPaletteDblClick(Sender: TObject);
    procedure btnGeneratePNGsClick(Sender: TObject);
    procedure pbFramePaint(Sender: TObject);
    procedure seEncGammaChange(Sender: TObject);
    procedure seMaxTilesEditingDone(Sender: TObject);
    procedure seQbTilesEditingDone(Sender: TObject);
    procedure tbFrameChange(Sender: TObject);
  private
    FKeyFrames: array of TKeyFrame;
    FFrames: array of TFrame;
    FColorMap: array[0..cRGBColors - 1, 0..6] of Byte;
    FTiles: array of PTile;
    FAdditionalTiles: TThreadList;
    FUseThomasKnoll: Boolean;
    FY2MixedColors: Integer;
    FInputPath: String;
    FFramesPerSecond: Double;

    FProgressStep: TEncoderStep;
    FProgressPosition, FOldProgressPosition, FProgressStartTime, FProgressPrevTime: Int64;

    FTileMapWidth: Integer;
    FTileMapHeight: Integer;
    FTileMapSize: Integer;
    FScreenWidth: Integer;
    FScreenHeight: Integer;

    FPaletteCount: Integer;
    FTilePaletteSize: Integer;

    FCS: TRTLCriticalSection;
    FLock: TSpinlock;
    FConcurrentKModesBins: Integer;

    FGlobalDS: TTilingDataset;

    function PearsonCorrelation(const x: TFloatDynArray; const y: TFloatDynArray): TFloat;
    function ComputeCorrelationBGR(const a: TIntegerDynArray; const b: TIntegerDynArray): TFloat;
    function ComputeDistanceRGB(const a: TIntegerDynArray; const b: TIntegerDynArray): TFloat;
    function ComputeInterFrameCorrelation(a, b: TFrame): TFloat;

    procedure ClearAll;
    procedure ProgressRedraw(CurFrameIdx: Integer = -1; ProgressStep: TEncoderStep = esNone);
    procedure Render(AFrameIndex: Integer; playing, blended, mirrored, reduced, gamma: Boolean; palIdx: Integer;
      ATilePage: Integer);
    procedure ReframeUI(AWidth, AHeight: Integer);

    procedure DitherFloydSteinberg(const AScreen: TByteDynArray);

    function HSVToRGB(h, s, v: Byte): Integer;
    procedure RGBToHSV(col: Integer; out h, s, v: Byte); overload;
    procedure RGBToHSV(col: Integer; out h, s, v: TFloat); overload;
    procedure RGBToYUV(col: Integer; GammaCor: Integer; out y, u, v: TFloat);
    procedure RGBToYUV(r, g, b: Byte; GammaCor: Integer; out y, u, v: TFloat);
    procedure RGBToLAB(r, g, b: TFloat; GammaCor: Integer; out ol, oa, ob: TFloat);
    procedure RGBToLAB(ir, ig, ib: Integer; GammaCor: Integer; out ol, oa, ob: TFloat);
    function LABToRGB(ll, aa, bb: TFloat): Integer;
    function YUVToRGB(y, u, v: TFloat): Integer;

    procedure WaveletGS(Data: PFloat; Output: PFloat; dx, dy, depth: cardinal);
    procedure DeWaveletGS(wl: PFloat; pic: PFloat; dx, dy, depth: longint);
    procedure ComputeTilePsyVisFeatures(const ATile: TTile; FromPal, UseWavelets, UseLAB, QWeighting, HMirror,
      VMirror: Boolean; GammaCor: Integer; const pal: TIntegerDynArray; var DCT: array of TFloat); inline;

    // Dithering algorithms ported from http://bisqwit.iki.fi/story/howto/dither/jy/

    function ColorCompare(r1, g1, b1, r2, g2, b2: Int64): Int64;
    procedure PreparePlan(var Plan: TMixingPlan; MixedColors: Integer; const pal: array of Integer);
    procedure TerminatePlan(var Plan: TMixingPlan);
    function DeviseBestMixingPlanYliluoma(var Plan: TMixingPlan; col: Integer; var List: array of Byte): Integer;
    procedure DeviseBestMixingPlanThomasKnoll(var Plan: TMixingPlan; col: Integer; var List: array of Byte);
    procedure DitherTileFloydSteinberg(ATile: TTile; out RGBPixels: TRGBPixels);

    procedure LoadFrame(var AFrame: TFrame; AFrameIndex: Integer; ABitmap: TBitmap);
    procedure FindKeyFrames;
    procedure LoadTiles;
    function GetGlobalTileCount: Integer;
    function GetFrameTileCount(AFrame: TFrame): Integer;

    procedure PrepareDitherTiles(AKeyFrame: TKeyFrame; ADitheringGamma: Integer; AUseWavelets: Boolean);
    procedure QuantizePalette(AKeyFrame: TKeyFrame; APalIdx: Integer; UseYakmo: Boolean; DLv3BPC: Integer);
    procedure FinishQuantizePalette(AKeyFrame: TKeyFrame);
    procedure DitherTile(var ATile: TTile; var Plan: TMixingPlan);
    procedure FinishDitherTiles(AFrame: TFrame; ADitheringGamma: Integer; AUseWavelets: Boolean);

    function GetTileZoneSum(const ATile: TTile; x, y, w, h: Integer): Integer;
    function GetTilePalZoneThres(const ATile: TTile; ZoneCount: Integer; Zones: PByte): Integer;
    procedure MakeTilesUnique(FirstTileIndex, TileCount: Integer);
    procedure MergeTiles(const TileIndexes: array of Integer; TileCount: Integer; BestIdx: Integer; NewTile: PPalPixels; NewTileRGB: PRGBPixels);
    procedure InitMergeTiles;
    procedure FinishMergeTiles;
    function WriteTileDatasetLine(const ATile: TTile; DataLine: PByte; out PalSigni: Integer): Integer;
    procedure DoGlobalKModes(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
    procedure DoGlobalTiling(DesiredNbTiles, RestartCount: Integer);

    procedure ReloadPreviousTiling(AFN: String);

    function HMirrorPalTile(var ATile: TTile): Boolean;
    function VMirrorPalTile(var ATile: TTile): Boolean;
    procedure PrepareGlobalFT;
    procedure FinishGlobalFT;
    procedure PrepareFrameTiling(AKF: TKeyFrame; AFTGamma: Integer; AUseWavelets: Boolean; AFTQuality: TFTQuality);
    procedure FinishFrameTiling(AKF: TKeyFrame);
    procedure DoFrameTiling(AFrame: TFrame; Y: Integer; AFTGamma: Integer; AUseWavelets: Boolean; AAddlTilesThres: TFloat;
      AFTBlend: Integer);

    function GetTileUseCount(ATileIndex: Integer): Integer;
    procedure ReindexTiles;
    procedure DoTemporalSmoothing(AFrame, APrevFrame: TFrame; Y: Integer; Strength: TFloat);

    procedure SaveStream(AStream: TStream; AFTBlend: Integer);
    procedure SaveRawTiles(OutFN: String);

    function DoExternalFFMpeg(AFN: String; var AVidPath: String; AStartFrame, AFrameCount: Integer; AScale: Double; out
      AFPS: Double): String;
  public
    { public declarations }

    FLastIOTabSheet: TTabSheet;
  end;

  { T8BitPortableNetworkGraphic }

  T8BitPortableNetworkGraphic = class(TPortableNetworkGraphic)
    procedure InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter); override;
  end;

  { TFastPortableNetworkGraphic }

  TFastPortableNetworkGraphic = class(TPortableNetworkGraphic)
    procedure InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter); override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

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

function HasParam(p: String): Boolean;
var i: Integer;
begin
  Result := False;
  for i := 1 to ParamCount do
    if SameText(p, ParamStr(i)) then
      Exit(True);
end;

function ParamStart(p: String): Integer;
var i: Integer;
begin
  Result := -1;
  for i := 1 to ParamCount do
    if AnsiStartsStr(p, ParamStr(i)) then
      Exit(i);
end;

function ParamValue(p: String; def: Double): Double;
var
  idx: Integer;
begin
  idx := ParamStart(p);
  if idx < 0 then
    Exit(def);
  Result := StrToFloatDef(system.copy(ParamStr(idx), Length(p) + 1), def);
end;

procedure Exchange(var a, b: Integer);
var
  tmp: Integer;
begin
  tmp := b;
  b := a;
  a := tmp;
end;

function iDiv0(x,y:Integer):Integer;inline;
begin
  Result:=0;
  if y <> 0 then
    Result:=x div y;
end;

function Nan0(x: TFloat): TFloat; inline;
begin
  Result := x;
  if IsNan(Result) then
    Result := 0.0;
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

function CompareIntegers(const Item1, Item2: Integer): Integer;
begin
  Result := CompareValue(Item1, Item2);
end;

var
  gGamma: array[0..1] of TFloat = (2.0, 0.6);
  gGammaCorLut: array[-1..High(gGamma), 0..High(Byte)] of TFloat;
  gVecInv: array[0..256 * 4 - 1] of Cardinal;
  gDCTLut:array[0..sqr(sqr(cTileWidth)) - 1] of TFloat;
  gPalettePattern : TFloatDynArray2;

procedure InitLuts(ATilePaletteSize, APaletteCount: Integer);
const
  cCurvature = 2.0;
var
  g, i, j, v, u, y, x: Int64;
  f, fp: TFloat;
begin
  // gamma

  for g := -1 to High(gGamma) do
    for i := 0 to High(Byte) do
      if g >= 0 then
        gGammaCorLut[g, i] := power(i / 255.0, gGamma[g])
      else
        gGammaCorLut[g, i] := i / 255.0;

  // inverse

  for i := 0 to High(gVecInv) do
    gVecInv[i] := iDiv0(1 shl cVecInvWidth, i shr 2);

  // DCT

  i := 0;
  for v := 0 to (cTileWidth - 1) do
    for u := 0 to (cTileWidth - 1) do
      for y := 0 to (cTileWidth - 1) do
        for x := 0 to (cTileWidth - 1) do
        begin
          gDCTLut[i] := cos((x + 0.5) * u * PI / 16.0) * cos((y + 0.5) * v * PI / 16.0);
          Inc(i);
        end;

  // palette pattern

  SetLength(gPalettePattern, APaletteCount, ATilePaletteSize);

  f := 0;
  for i := 0 to ATilePaletteSize - 1 do
  begin
    fp := f;
    f := power(i + 2, cCurvature);

    for j := 0 to APaletteCount - 1 do
      gPalettePattern[j, i] := ((j + 1) / APaletteCount) * max(APaletteCount, f - fp) + fp;
  end;

  for j := 0 to APaletteCount - 1 do
    for i := 0 to ATilePaletteSize - 1 do
      gPalettePattern[j, i] /= gPalettePattern[APaletteCount - 1, ATilePaletteSize - 1];
end;

function GammaCorrect(lut: Integer; x: Byte): TFloat; inline;
begin
  Result := gGammaCorLut[lut, x];
end;

function lerp(x, y, alpha: TFloat): TFloat; inline;
begin
  Result := x + (y - x) * alpha;
end;

function ilerp(x, y, alpha, maxAlpha: Integer): Integer; inline;
begin
  Result := x + ((y - x) * alpha) div maxAlpha;
end;

function revlerp(x, y, alpha: TFloat): TFloat; inline;
begin
  Result := (alpha - x) / (y - x);
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

procedure TTileHelper.ExtractPalPixels(var AArray: array of TANNFloat);
var
  i: Integer;
  PB: PByte;
  PF: ^TANNFloat;
begin
  Assert(HasPalPixels);
  PB := @GetPalPixelsPtr^[0, 0];
  PF := @AArray[0];
  for i := 0 to Sqr(cTileWidth) - 1 do
  begin
    PF^ := PB^;
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
  OriginalReloadedIndex := ATile.OriginalReloadedIndex;
  DitheringPalIndex := ATile.DitheringPalIndex;
  KFSoleIndex := ATile.KFSoleIndex;

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
  FTDoPrepareEvent := CreateEvent(nil, True, False, nil);
  FTDoFinishEvent := CreateEvent(nil, True, False, nil);
  FTPreparedEvent := CreateEvent(nil, True, False, nil);
  SetLength(MixingPlans, APaletteCount);
  SetLength(PaletteRGB, APaletteCount);
end;

destructor TKeyFrame.Destroy;
begin
  inherited Destroy;
  DeleteCriticalSection(CS);
  CloseHandle(FTDoPrepareEvent);
  CloseHandle(FTDoFinishEvent);
  CloseHandle(FTPreparedEvent);
end;

{ T8BitPortableNetworkGraphic }

procedure T8BitPortableNetworkGraphic.InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter);
var
  W: TFPWriterPNG absolute AWriter;
begin
  inherited InitializeWriter(AImage, AWriter);
  W.Indexed := True;
  W.UseAlpha := False;
  W.CompressionLevel := clfastest;
end;

{ TFastPortableNetworkGraphic }

procedure TFastPortableNetworkGraphic.InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter);
var
  W: TFPWriterPNG absolute AWriter;
begin
  inherited InitializeWriter(AImage, AWriter);
  W.CompressionLevel := clfastest;
end;

function TMainForm.ComputeCorrelationBGR(const a: TIntegerDynArray; const b: TIntegerDynArray): TFloat;
var
  i: Integer;
  ya, yb: TDoubleDynArray;
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

function TMainForm.ComputeDistanceRGB(const a: TIntegerDynArray; const b: TIntegerDynArray): TFloat;
var
  i: Integer;
  ya, yb: TDoubleDynArray;
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

function TMainForm.ComputeInterFrameCorrelation(a, b: TFrame): TFloat;
var
  sz, i: Integer;
  ya, yb: TDoubleDynArray;
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
end;

{ TMainForm }

procedure TMainForm.btnDoGlobalTilingClick(Sender: TObject);
begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(-1, esGlobalTiling);

  if chkReload.Checked then
  begin
    if not FileExists(edReload.Text) then
      raise EFileNotFoundException.Create('File not found: ' + edReload.Text);
    ReloadPreviousTiling(edReload.Text);
  end
  else
  begin
    DoGlobalTiling(seMaxTiles.Value, cRandomKModesCount);
  end;

  tbFrameChange(nil);
end;

procedure TMainForm.btnDitherClick(Sender: TObject);
var
  Gamma: Integer;
  UseWavelets: Boolean;
  UseYakmo: Boolean;
  DLBPC: Integer;
  i: Integer;

  procedure DoPrepare(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    if not InRange(AIndex, 0, High(FKeyFrames)) then
      Exit;

    PrepareDitherTiles(FKeyFrames[AIndex], Gamma, UseWavelets);
  end;

  procedure DoQuantize(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    if not InRange(AIndex, 0, Length(FKeyFrames) * FPaletteCount - 1) then
      Exit;

    QuantizePalette(FKeyFrames[AIndex div FPaletteCount], AIndex mod FPaletteCount, UseYakmo, DLBPC);
  end;

  procedure DoFinish(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    if not InRange(AIndex, 0, High(FFrames)) then
      Exit;

    FinishDitherTiles(FFrames[AIndex], Gamma, UseWavelets);
  end;

begin
  if Length(FFrames) = 0 then
    Exit;

  Gamma := IfThen(chkDitheringGamma.Checked, 0, -1);
  UseWavelets := chkUseWL.Checked;
  UseYakmo := chkUseKMQuant.Checked;
  DLBPC := StrToInt(cbxDLBPC.Text);

  ProgressRedraw(-1, esDither);
  ProcThreadPool.DoParallelLocalProc(@DoPrepare, 0, High(FKeyFrames));
  WriteLn;
  ProgressRedraw(1);

  for i := 0 to High(FKeyFrames) do
    SetLength(FKeyFrames[i].PaletteUseCount, FPaletteCount);

  ProcThreadPool.DoParallelLocalProc(@DoQuantize, 0, Length(FKeyFrames) * FPaletteCount - 1);

  for i := 0 to High(FKeyFrames) do
  begin
    FinishQuantizePalette(FKeyFrames[i]);
    SetLength(FKeyFrames[i].PaletteUseCount, 0);
  end;
  ProgressRedraw(2);

  ProcThreadPool.DoParallelLocalProc(@DoFinish, 0, High(FFrames));
  ProgressRedraw(3);

  tbFrameChange(nil);
end;

procedure TMainForm.btnDoMakeUniqueClick(Sender: TObject);
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

  ProgressRedraw(-1, esMakeUnique);

  ProcThreadPool.DoParallelLocalProc(@DoMakeUnique, 0, High(FTiles) div TilesAtATime);

  ProgressRedraw(1);

  tbFrameChange(nil);
end;

function CompareFrames(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PInteger(Item2)^, PInteger(Item1)^);
end;

procedure TMainForm.btnDoFrameTilingClick(Sender: TObject);
var
  Gamma: Integer;
  UseWavelets: Boolean;
  FTQuality: TFTQuality;
  FTBlend: Integer;
  AddlTilesThres: TFloat;

  procedure DoBoth(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);

    procedure DoPrepare(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
    var
      KF: TKeyFrame;
    begin
      if not InRange(AIndex, 0, High(FKeyFrames)) then
        Exit;

      KF := FKeyFrames[AIndex];

      WaitForSingleObject(KF.FTDoPrepareEvent, INFINITE);

      PrepareFrameTiling(KF, Gamma, UseWavelets, FTQuality);

      SetEvent(KF.FTPreparedEvent);
      WaitForSingleObject(KF.FTDoFinishEvent, INFINITE);

      FinishFrameTiling(KF);
    end;

    procedure DoFrm(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
    var
      Frame: TFrame;
      idx, y, i: Integer;
      ok: Boolean;
    begin
      if not InRange(AIndex, 0, Length(FFrames) * FTileMapHeight - 1) then
        Exit;

      DivMod(AIndex, FTileMapHeight, idx, y);
      Frame := FFrames[idx];

      SpinEnter(@FLock);
      if Frame.PKeyFrame.FramesLeft < 0 then
      begin
        Frame.PKeyFrame.FramesLeft := Frame.PKeyFrame.FrameCount;
        SetEvent(Frame.PKeyFrame.FTDoPrepareEvent); // signal DoPrepare thread to start preparing KF
      end;
      SpinLeave(@FLock);

      WaitForSingleObject(Frame.PKeyFrame.FTPreparedEvent, INFINITE); // wait for KF prepared

      DoFrameTiling(Frame, y, Gamma, UseWavelets, AddlTilesThres, FTBlend);

      SpinEnter(@FLock);
      Frame.FTYDone[y] := True;
      ok := True;
      for i := 0 to FTileMapHeight - 1 do
        ok := ok and Frame.FTYDone[i];
      if ok then
      begin
        Dec(Frame.PKeyFrame.FramesLeft);
        if Frame.PKeyFrame.FramesLeft <= 0 then
          SetEvent(Frame.PKeyFrame.FTDoFinishEvent); // signal DoPrepare thread to start finishing KF
      end;
      SpinLeave(@FLock);
    end;

  begin
    case AIndex of
      0: ProcThreadPool.DoParallelLocalProc(@DoPrepare, 0, High(FKeyFrames), nil, NumberOfProcessors);
      1: ProcThreadPool.DoParallelLocalProc(@DoFrm, 0, Length(FFrames) * FTileMapHeight - 1, nil, NumberOfProcessors);
    end;
  end;

var
  i, mtcSave: Integer;
  ATList: TList;
  T: PTile;
begin
  if Length(FKeyFrames) = 0 then
    Exit;

  Gamma := IfThen(chkFTGamma.Checked, 0, -1);
  UseWavelets := chkUseWL.Checked;
  FTQuality := TFTQuality(cbxFTQ.ItemIndex);
  FTBlend := seFTBlend.Value;
  AddlTilesThres := seAddlTiles.Value;

  for i := 0 to High(FKeyFrames) do
    FKeyFrames[i].FramesLeft := -1;
  for i := 0 to High(FFrames) do
    FillByte(FFrames[i].FTYDone[0], FTileMapHeight, 0);

  FAdditionalTiles := TThreadList.Create;
  try
    ProgressRedraw(-1, esFrameTiling);
    PrepareGlobalFT;
    ProgressRedraw(1);

    mtcSave := ProcThreadPool.MaxThreadCount;
    try
      ProcThreadPool.MaxThreadCount := MaxInt;
      ProcThreadPool.DoParallelLocalProc(@DoBoth, 0, 1);
    finally
      ProcThreadPool.MaxThreadCount := mtcSave;
    end;

    ProgressRedraw(2);

    FinishGlobalFT;

    ATList := FAdditionalTiles.LockList;
    try
      TTile.Array1DRealloc(FTiles, Length(FTiles) + ATList.Count);
      for i := 0 to ATList.Count - 1 do
      begin
        T := PTile(ATList[i]);
        FTiles[i + Length(FTiles) - ATList.Count]^.CopyFrom(T^);
        TTile.Dispose(T);
      end;

      WriteLn('AdditionalTiles: ', ATList.Count);
    finally
      FAdditionalTiles.UnlockList;
    end;
  finally
    FreeAndNil(FAdditionalTiles);
  end;

  ProgressRedraw(3);

  tbFrameChange(nil);
end;

procedure TMainForm.chkUseTKChange(Sender: TObject);
begin
  FUseThomasKnoll := chkUseTK.Checked;
end;

procedure TMainForm.btnLoadClick(Sender: TObject);

  procedure DoLoadFrame(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    bmp: TPicture;
  begin
    if not InRange(AIndex, 0, High(FFrames)) then
      Exit;

    bmp := TPicture.Create;
    try
      EnterCriticalSection(FCS);
      bmp.Bitmap.PixelFormat:=pf32bit;
      bmp.LoadFromFile(Format(FInputPath, [AIndex + PtrUInt(AData)]));
      LeaveCriticalSection(FCS);

      LoadFrame(FFrames[AIndex], AIndex, bmp.Bitmap);

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
  FTilePaletteSize := StrToInt(cbxPalSize.Text);
  FPaletteCount := StrToInt(cbxPalCount.Text);
  wasAutoQ := seMaxTiles.Value = round(seQbTiles.Value * EqualQualityTileCount(Length(FFrames) * FTileMapSize));

  ProgressRedraw;

  ClearAll;

  ProgressRedraw(-1, esLoad);

  // init Gamma LUTs

  InitLuts(FTilePaletteSize, FPaletteCount);

  // load video

  StartFrame := seStartFrame.Value;
  frc := seFrameCount.Value;

  if FileExists(edInput.Text) then
  begin
    DoExternalFFMpeg(edInput.Text, FInputPath, StartFrame, frc, StrToFloat(cbxScaling.Text), FFramesPerSecond);
    StartFrame := 1;
  end
  else
  begin
    FInputPath := edInput.Text;
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

    seFrameCount.Value := frc;
    seFrameCount.Repaint;
  end;

  // load frames bitmap data

  SetLength(FFrames, frc);
  tbFrame.Max := High(FFrames);

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

  ProgressRedraw(3);

  // free up frame memory
  for i := 0 to High(FFrames) do
    SetLength(FFrames[i].FSPixels, 0);

  if wasAutoQ or (seMaxTiles.Value <= 0) then
    seQbTilesEditingDone(nil);
  tbFrameChange(nil);
end;

procedure TMainForm.btnInputClick(Sender: TObject);
begin
  odFFInput.InitialDir := ExtractFileDir(edInput.Text);
  if odFFInput.Execute then
  begin
    if (edOutput.Text = '') or (edOutput.Text = ChangeFileExt(edInput.Text, '.gtm')) then
    begin
      edOutput.Text := ChangeFileExt(odFFInput.FileName, '.gtm');
      sdGTM.FileName := edOutput.Text;
    end;
    if (edReload.Text = '') or (edReload.Text = ChangeFileExt(edInput.Text, '.gts')) then
    begin
      edReload.Text := ChangeFileExt(odFFInput.FileName, '.gts');
      sdGTS.FileName := edReload.Text;
    end;
    edInput.Text := odFFInput.FileName;
  end;
end;

procedure TMainForm.btnGTMClick(Sender: TObject);
begin
  if sdGTM.Execute then
    edOutput.Text := sdGTM.FileName;
end;

procedure TMainForm.btnGTSClick(Sender: TObject);
begin
  if sdGTS.Execute then
    edReload.Text := sdGTS.FileName;
end;

procedure TMainForm.btnReindexClick(Sender: TObject);
var
  i, sx, sy, tidx: Integer;
begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(-1, esReindex);

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

  tbFrameChange(nil);
end;

procedure TMainForm.btnRunAllClick(Sender: TObject);
var
  firstStep: TEncoderStep;
  lastStep: TEncoderStep;

  function OkStep(Step: TEncoderStep): Boolean;
  begin
    Result := (Step >= firstStep) and (Step <= lastStep);
  end;

begin
  firstStep := TEncoderStep(cbxStartStep.ItemIndex);
  lastStep := TEncoderStep(cbxEndStep.ItemIndex);

  if OkStep(esLoad) then
    btnLoadClick(nil);

  if OkStep(esDither) then
    btnDitherClick(nil);

  if OkStep(esMakeUnique) then
    btnDoMakeUniqueClick(nil);

  if OkStep(esGlobalTiling) then
    btnDoGlobalTilingClick(nil);

  if OkStep(esFrameTiling) then
    btnDoFrameTilingClick(nil);

  if OkStep(esReindex) then
    btnReindexClick(nil);

  if OkStep(esSmooth) then
    btnSmoothClick(nil);

  if OkStep(esSave) then
    btnSaveClick(nil);

  ProgressRedraw;
  tbFrameChange(nil);
end;

procedure TMainForm.btnDebugClick(Sender: TObject);
begin
  edInput.Text := 'C:\tiler_misc\factory_1080p30.y4m';
  edOutput.Text := 'C:\tiler\debug.gtm';
  edReload.Text := '';
  seFrameCount.Value := IfThen(seFrameCount.Value = 12, 48, 12);
  cbxScaling.ItemIndex := 2;
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
var
  fs: TFileStream;
begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(-1, esSave);

  fs := TFileStream.Create(edOutput.Text, fmCreate or fmShareDenyWrite);
  try
    SaveStream(fs, seFTBlend.Value);
  finally
    fs.Free;
  end;

  ProgressRedraw(1);

  SaveRawTiles(edReload.Text);

  ProgressRedraw(2);

  tbFrameChange(nil);
end;

procedure TMainForm.btnSmoothClick(Sender: TObject);
var
  smoo: TFloat;

  procedure DoSmoothing(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    i: Integer;
  begin
    if not InRange(AIndex, 0, FTileMapHeight - 1) then
      Exit;

    for i := cSmoothingPrevFrame to High(FFrames) do
      DoTemporalSmoothing(FFrames[i], FFrames[i - cSmoothingPrevFrame], AIndex, smoo);
  end;

var
  frm, j, i: Integer;
  TMI: PTileMapItem;
begin
  if Length(FFrames) = 0 then
    Exit;

  smoo := seTempoSmoo.Value / 1000.0;

  ProgressRedraw(-1, esSmooth);

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
  ProgressRedraw(1);

  ProcThreadPool.DoParallelLocalProc(@DoSmoothing, 0, FTileMapHeight - 1);
  ProgressRedraw(2);

  tbFrameChange(nil);
end;

procedure TMainForm.cbxYilMixChange(Sender: TObject);
begin
  FY2MixedColors := StrToIntDef(cbxYilMix.Text, 16);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  k: Word;
begin
  k := Key;
  if k in [VK_F2, VK_F3, VK_F4, VK_F5, VK_F6, VK_F7, VK_F8, VK_F9, VK_F10, VK_F11, VK_F12, VK_ESCAPE] then
    Key := 0; // KLUDGE: workaround event called twice
  case k of
    VK_F2: btnLoadClick(nil);
    VK_F3: btnDitherClick(nil);
    VK_F4: btnDoMakeUniqueClick(nil);
    VK_F5: btnDoGlobalTilingClick(nil);
    VK_F6: btnDoFrameTilingClick(nil);
    VK_F7: btnReindexClick(nil);
    VK_F8: btnSmoothClick(nil);
    VK_F9: btnSaveClick(nil);
    VK_F10: btnRunAllClick(nil);
    VK_F11: chkPlay.Checked := not chkPlay.Checked;
    VK_F12:
      if ssCtrl in Shift then
      begin
        btnDebugClick(nil);
      end
      else
      begin
        if pcPages.ActivePage <> FLastIOTabSheet then
        begin
          if Assigned(FLastIOTabSheet) then
           pcPages.ActivePage := FLastIOTabSheet
          else
           pcPages.ActivePage := tsOutput;
        end
        else
        begin
          if pcPages.ActivePage = tsOutput then
           pcPages.ActivePage := tsInput
          else
           pcPages.ActivePage := tsOutput;
        end;
        FLastIOTabSheet := pcPages.ActivePage;
      end;
    VK_ESCAPE: TerminateProcess(GetCurrentProcess, 1);
  end;
end;

procedure TMainForm.IdleTimerTimer(Sender: TObject);
begin
  if chkPlay.Checked then
  begin
    if tbFrame.Position >= tbFrame.Max then
    begin
      tbFrame.Position := 0;
      Exit;
    end;

    tbFrame.Position := tbFrame.Position + 1;
  end;
end;

procedure TMainForm.imgPaletteClick(Sender: TObject);
var
  P: TPoint;
begin
  P := imgPalette.ScreenToClient(Mouse.CursorPos);
  sedPalIdx.Value := iDiv0(P.y * FPaletteCount, imgPalette.Height);
end;

procedure TMainForm.imgPaletteDblClick(Sender: TObject);
begin
  sedPalIdx.Value := -1;
end;

procedure TMainForm.btnGeneratePNGsClick(Sender: TObject);
var
  palPict: TPortableNetworkGraphic;
  i: Integer;
begin
  palPict := TFastPortableNetworkGraphic.Create;

  palPict.Width := FScreenWidth;
  palPict.Height := FScreenHeight;
  palPict.PixelFormat := pf24bit;

  try
    for i := 0 to High(FFrames) do
    begin
      Render(i, True, chkBlended.Checked, chkMirrored.Checked, chkReduced.Checked, chkGamma.Checked, sedPalIdx.Value, sePage.Value);
      imgDest.Repaint;

      palPict.Canvas.Draw(0, 0, imgDest.Picture.Bitmap);
      palPict.SaveToFile(Format('%s_output_%.4d.png', [ChangeFileExt(edOutput.Text, ''), i]));
    end;
  finally
    palPict.Free;
  end;

  tbFrameChange(nil);
end;

procedure TMainForm.pbFramePaint(Sender: TObject);
begin
  TPaintBox(Sender).Canvas.Brush.Color := clBlack;
  TPaintBox(Sender).Canvas.Brush.Style := TFPBrushStyle.bsSolid;
  TPaintBox(Sender).Canvas.FillRect(TPaintBox(Sender).Canvas.ClipRect);
  TPaintBox(Sender).Canvas.Brush.Color := IfThen(Sender = pbFrameI, $600070, $605000);
  TPaintBox(Sender).Canvas.Brush.Style := TFPBrushStyle.bsDiagCross;
  TPaintBox(Sender).Canvas.FillRect(TPaintBox(Sender).Canvas.ClipRect);
end;

procedure TMainForm.seQbTilesEditingDone(Sender: TObject);
var
  RawTileCount: Integer;
begin
  if Length(FFrames) * FTileMapSize = 0 then Exit;
  RawTileCount := Length(FFrames) * FTileMapSize;
  seMaxTiles.Value := min(round(seQbTiles.Value * EqualQualityTileCount(RawTileCount)), RawTileCount);
end;

procedure TMainForm.seEncGammaChange(Sender: TObject);
begin
  gGamma[0] := seEncGamma.Value;
  gGamma[1] := seVisGamma.Value;
  InitLuts(FTilePaletteSize, FPaletteCount);
  tbFrameChange(nil);
end;

procedure TMainForm.seMaxTilesEditingDone(Sender: TObject);
var
  RawTileCount: Integer;
begin
  if Length(FFrames) * FTileMapSize = 0 then Exit;
  RawTileCount := Length(FFrames) * FTileMapSize;
  seMaxTiles.Value := EnsureRange(seMaxTiles.Value, 1, RawTileCount);
end;

procedure TMainForm.tbFrameChange(Sender: TObject);
begin
  IdleTimer.Interval := round(1000 / FFramesPerSecond);
  Screen.Cursor := crDefault;
  Render(tbFrame.Position, chkPlay.Checked, chkBlended.Checked, chkMirrored.Checked, chkReduced.Checked,  chkGamma.Checked, sedPalIdx.Value, sePage.Value);
end;

function TMainForm.PearsonCorrelation(const x: TFloatDynArray; const y: TFloatDynArray): TFloat;
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

procedure TMainForm.PreparePlan(var Plan: TMixingPlan; MixedColors: Integer; const pal: array of Integer);
var
  i, col, r, g, b: Integer;
begin
  FillChar(Plan, SizeOf(Plan), 0);

  Plan.Y2MixedColors := MixedColors;
  SetLength(Plan.LumaPal, Length(pal));
  SetLength(Plan.Y2Palette, Length(pal));

  for i := 0 to High(pal) do
  begin
    col := pal[i];
    r := col and $ff;
    g := (col shr 8) and $ff;
    b := (col shr 16) and $ff;

    Plan.LumaPal[i] := r*cRedMul + g*cGreenMul + b*cBlueMul;

    Plan.Y2Palette[i][0] := r;
    Plan.Y2Palette[i][1] := g;
    Plan.Y2Palette[i][2] := b;
    Plan.Y2Palette[i][3] := Plan.LumaPal[i] div cLumaDiv;
  end
end;

procedure TMainForm.TerminatePlan(var Plan: TMixingPlan);
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

function TMainForm.ColorCompare(r1, g1, b1, r2, g2, b2: Int64): Int64;
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

function TMainForm.DeviseBestMixingPlanYliluoma(var Plan: TMixingPlan; col: Integer; var List: array of Byte): Integer;
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

  VecInv := @gVecInv[0];
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

procedure TMainForm.DeviseBestMixingPlanThomasKnoll(var Plan: TMixingPlan; col: Integer; var List: array of Byte);
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
    chosen := c and (length(Plan.Y2Palette) - 1);
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

procedure TMainForm.DitherTileFloydSteinberg(ATile: TTile; out RGBPixels: TRGBPixels);
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

procedure TMainForm.ReframeUI(AWidth, AHeight: Integer);
begin
  FTileMapWidth := min(AWidth, 1920 div cTileWidth);
  FTileMapHeight := min(AHeight, 1080 div cTileWidth);

  FTileMapSize := FTileMapWidth * FTileMapHeight;
  FScreenWidth := FTileMapWidth * cTileWidth;
  FScreenHeight := FTileMapHeight * cTileWidth;

  imgSource.Picture.Bitmap.Width:=FScreenWidth;
  imgSource.Picture.Bitmap.Height:=FScreenHeight;
  imgSource.Picture.Bitmap.PixelFormat:=pf32bit;

  imgDest.Picture.Bitmap.Width:=FScreenWidth;
  imgDest.Picture.Bitmap.Height:=FScreenHeight;
  imgDest.Picture.Bitmap.PixelFormat:=pf32bit;

  imgTiles.Picture.Bitmap.Width:=FScreenWidth;
  imgTiles.Picture.Bitmap.Height:=FScreenHeight;
  imgTiles.Picture.Bitmap.PixelFormat:=pf32bit;

  imgPalette.Picture.Bitmap.Width := FTilePaletteSize;
  imgPalette.Picture.Bitmap.Height := FPaletteCount;
  imgPalette.Picture.Bitmap.PixelFormat:=pf32bit;

  imgTiles.Width := FScreenWidth shl IfThen(FScreenHeight <= 256, 2, 1);
  imgTiles.Height := FScreenHeight shl IfThen(FScreenHeight <= 256, 2, 1);
  imgSource.Width := FScreenWidth shl IfThen(FScreenHeight <= 256, 1, 0);
  imgSource.Height := FScreenHeight shl IfThen(FScreenHeight <= 256, 1, 0);
  imgDest.Width := FScreenWidth shl IfThen(FScreenHeight <= 256, 1, 0);
  imgDest.Height := FScreenHeight shl IfThen(FScreenHeight <= 256, 1, 0);

  sedPalIdx.MaxValue := FPaletteCount - 1;
end;

procedure TMainForm.DitherFloydSteinberg(const AScreen: TByteDynArray);
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

procedure TMainForm.DitherTile(var ATile: TTile; var Plan: TMixingPlan);
var
  x, y: Integer;
  count, map_value: Integer;
  TKList: array[0 .. cDitheringLen - 1] of Byte;
  YilList: array[0 .. cDitheringListLen - 1] of Byte;
begin
  if FUseThomasKnoll then
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
        map_value := cDitheringMap[(y shl 3) + x];
        count := DeviseBestMixingPlanYliluoma(Plan, ATile.RGBPixels[y,x], YilList);
        map_value := (map_value * count) shr 6;
        ATile.PalPixels[y, x] := YilList[map_value];
      end;
  end;
end;

function CompareCMUCntHLS(Item1,Item2:Pointer):Integer;
begin
  Result := PCountIndexArray(Item2)^.Count - PCountIndexArray(Item1)^.Count;
  if Result = 0 then
    Result := CompareValue(PCountIndexArray(Item1)^.Hue, PCountIndexArray(Item2)^.Hue);
  if Result = 0 then
    Result := CompareValue(PCountIndexArray(Item1)^.Val, PCountIndexArray(Item2)^.Val);
  if Result = 0 then
    Result := CompareValue(PCountIndexArray(Item1)^.Sat, PCountIndexArray(Item2)^.Sat);
end;

function CompareCMIntraPalette(Item1,Item2:Pointer):Integer;
begin
  Result := Integer(CompareValue(PCountIndexArray(Item1)^.Luma, PCountIndexArray(Item2)^.Luma)) shl 8;
  Result += CompareValue(PCountIndexArray(Item1)^.Hue, PCountIndexArray(Item2)^.Hue);
end;

function ComparePaletteUseCount(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PInteger(Item2)^, PInteger(Item1)^);
end;

procedure TMainForm.PrepareDitherTiles(AKeyFrame: TKeyFrame; ADitheringGamma: Integer; AUseWavelets: Boolean);
var
  sx, sy, i: Integer;
  GTile: PTile;

  Dataset: TFloatDynArray2;
  Clusters: TIntegerDynArray;
  di: Integer;

  Yakmo: PYakmo;
begin
  Assert(FPaletteCount <= Length(gPalettePattern));

  SetLength(Dataset, AKeyFrame.FrameCount * FTileMapSize, cTileDCTSize);
  SetLength(Clusters, Length(Dataset));
  SetLength(AKeyFrame.PaletteCentroids, FPaletteCount, cTileDCTSize);

  di := 0;
  for i := AKeyFrame.StartFrame to AKeyFrame.EndFrame do
    for sy := 0 to FTileMapHeight - 1 do
      for sx := 0 to FTileMapWidth - 1 do
      begin
        GTile := FTiles[FFrames[i].TileMap[sy, sx].TileIdx];
        ComputeTilePsyVisFeatures(GTile^, False, AUseWavelets, True, False, False, False, ADitheringGamma, nil, Dataset[di]);
        Inc(di);
      end;
  assert(di = Length(Dataset));

  if (di > 1) and (FPaletteCount > 1) then
  begin
   Yakmo := yakmo_create(FPaletteCount, 1, MaxInt, 1, 0, 0, 1);
   yakmo_load_train_data(Yakmo, di, cTileDCTSize, @Dataset[0]);
   SetLength(Dataset, 0); // free up some memmory
   WriteLn('KF: ', AKeyFrame.StartFrame, ' Yakmo start');
   yakmo_train_on_data(Yakmo, @Clusters[0]);
   yakmo_get_centroids(Yakmo, @AKeyFrame.PaletteCentroids[0]);
   yakmo_destroy(Yakmo);
  end
  else
  begin
    FillDWord(Clusters[0], Length(Clusters), 0);
  end;

  di := 0;
  for i := AKeyFrame.StartFrame to AKeyFrame.EndFrame do
    for sy := 0 to FTileMapHeight - 1 do
      for sx := 0 to FTileMapWidth - 1 do
      begin
        GTile := FTiles[FFrames[i].TileMap[sy, sx].TileIdx];
        GTile^.DitheringPalIndex := Clusters[di];
        Inc(di);
      end;
  assert(di = Length(Clusters));

  WriteLn('KF: ', AKeyFrame.StartFrame, ' Yakmo end');
end;

procedure TMainForm.QuantizePalette(AKeyFrame: TKeyFrame; APalIdx: Integer; UseYakmo: Boolean; DLv3BPC: Integer);
var
  col, i: Integer;
  CMPal: TFPList;
  CMItem: PCountIndexArray;

  procedure DoDennisLeeV3;
  var
    dlCnt: Integer;
    dlInput: PByte;
    i, j, sy, sx, dx, dy, ty, tx, k, tileCnt, tileFx, tileFy, best: Integer;
    dlPal: TDLUserPal;
    GTile: PTile;
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
            if GTile^.Active and (GTile^.DitheringPalIndex = APalIdx) then
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

            if GTile^.Active and (GTile^.DitheringPalIndex = APalIdx) then
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

      dl3quant(dlInput, tileFx * cTileWidth, tileFy * cTileWidth, FTilePaletteSize, DLv3BPC, @dlPal);
    finally
      Freemem(dlInput);
    end;

    // retrieve palette data

    CMPal.Count := FTilePaletteSize;
    for i := 0 to FTilePaletteSize - 1 do
    begin
      col := ToRGB(dlPal[0][i], dlPal[1][i], dlPal[2][i]);

      New(CMItem);
      CMItem^.Index := col;
      CMItem^.Count := 1;
      CMItem^.Hue := FColorMap[col, 3]; CMItem^.Sat := FColorMap[col, 4]; CMItem^.Val := FColorMap[col, 5];
      CMItem^.Luma := FColorMap[col, 6];
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
    Yakmo: PYakmo;
  begin
    di := 0;
    for i := AKeyFrame.StartFrame to AKeyFrame.EndFrame do
      for sy := 0 to FTileMapHeight - 1 do
        for sx := 0 to FTileMapWidth - 1 do
          if FTiles[FFrames[i].TileMap[sy, sx].TileIdx]^.DitheringPalIndex = APalIdx then
            Inc(di, Sqr(cTileWidth));

    SetLength(Centroids, FTilePaletteSize, cFeatureCount);

    if di >= FTilePaletteSize then
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
            if GTile^.DitheringPalIndex = APalIdx then
            begin
              for ty := 0 to cTileWidth - 1 do
                for tx := 0 to cTileWidth - 1 do
                begin
                  FromRGB(GTile^.RGBPixels[ty, tx], rr, gg, bb);
                  Dataset[di, 0] := rr; Dataset[di, 1] := gg; Dataset[di, 2] := bb;
                  Inc(di);
                end;
            end;
          end;

      // use KMeans to quantize to FTilePaletteSize elements

      Yakmo := yakmo_create(FTilePaletteSize, 1, MaxInt, 1, 0, 0, 0);
      yakmo_load_train_data(Yakmo, di, cFeatureCount, @Dataset[0]);

      SetLength(Dataset, 0); // free up some memmory

      yakmo_train_on_data(Yakmo, @Clusters[0]);
      yakmo_get_centroids(Yakmo, @Centroids[0]);
      yakmo_destroy(Yakmo);
    end;

    // retrieve palette data

    CMPal.Count := FTilePaletteSize;
    for i := 0 to FTilePaletteSize - 1 do
    begin
      col := 0;
      if di >= FTilePaletteSize then
        col := ToRGB(Round(Nan0(Centroids[i, 0])), Round(Nan0(Centroids[i, 1])), Round(Nan0(Centroids[i, 2])));

      New(CMItem);
      CMItem^.Index := col;
      CMItem^.Count := 1;
      CMItem^.Hue := FColorMap[col, 3]; CMItem^.Sat := FColorMap[col, 4]; CMItem^.Val := FColorMap[col, 5];
      CMItem^.Luma := FColorMap[col, 6];
      CMPal[i] := CMItem;
    end;
  end;

begin
  Assert(FPaletteCount <= Length(gPalettePattern));

  AKeyFrame.PaletteUseCount[APalIdx].UseCount := 0;

  CMPal := TFPList.Create;
  try
    if UseYakmo then
      DoYakmo
    else
      DoDennisLeeV3;

    // split most used colors into tile palettes

    CMPal.Sort(@CompareCMIntraPalette);

    SetLength(AKeyFrame.PaletteRGB[APalIdx], FTilePaletteSize);
    for i := 0 to FTilePaletteSize - 1 do
      AKeyFrame.PaletteRGB[APalIdx, i] := PCountIndexArray(CMPal[i])^.Index;

    for i := 0 to CMPal.Count - 1 do
      Dispose(PCountIndexArray(CMPal[i]));
  finally
    CMPal.Free;
  end;

  if APalIdx = FPaletteCount - 1 then
    WriteLn('KF: ', AKeyFrame.StartFrame);
end;

procedure TMainForm.FinishQuantizePalette(AKeyFrame: TKeyFrame);
var
  i, di, sy, sx, PalIdx: Integer;
  PalIdxLUT: TIntegerDynArray;
  TmpCentroids: TFloatDynArray2;
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

  di := 0;
  for i := AKeyFrame.StartFrame to AKeyFrame.EndFrame do
    for sy := 0 to FTileMapHeight - 1 do
      for sx := 0 to FTileMapWidth - 1 do
      begin
        GTile := FTiles[FFrames[i].TileMap[sy, sx].TileIdx];
        GTile^.DitheringPalIndex := PalIdxLUT[GTile^.DitheringPalIndex];
        Inc(di);
      end;

  TmpCentroids := Copy(AKeyFrame.PaletteCentroids);
  for PalIdx := 0 to FPaletteCount - 1 do
    AKeyFrame.PaletteCentroids[PalIdxLUT[PalIdx]] := TmpCentroids[PalIdx];
end;

procedure TMainForm.FinishDitherTiles(AFrame: TFrame; ADitheringGamma: Integer; AUseWavelets: Boolean);
var
  i, PalIdx: Integer;
  sx, sy: Integer;
  OrigTile: PTile;
begin
  EnterCriticalSection(AFrame.PKeyFrame.CS);
  if AFrame.PKeyFrame.FramesLeft < 0 then
  begin
    for i := 0 to FPaletteCount - 1 do
      PreparePlan(AFrame.PKeyFrame.MixingPlans[i], FY2MixedColors, AFrame.PKeyFrame.PaletteRGB[i]);
    AFrame.PKeyFrame.FramesLeft := AFrame.PKeyFrame.FrameCount;
  end;
  LeaveCriticalSection(AFrame.PKeyFrame.CS);

  for sy := 0 to FTileMapHeight - 1 do
    for sx := 0 to FTileMapWidth - 1 do
    begin
      OrigTile := FTiles[AFrame.TileMap[sy, sx].TileIdx];

      if OrigTile^.Active then
      begin
        // choose best palette from the keyframe by comparing DCT of the tile colored with either palette

        PalIdx := OrigTile^.DitheringPalIndex;
        Assert(InRange(PalIdx, 0, FPaletteCount - 1));
        DitherTile(OrigTile^, AFrame.PKeyFrame.MixingPlans[PalIdx]);

        // now that the palette is chosen, update tilemap

        AFrame.TileMap[sy, sx].PalIdx := PalIdx;
      end;
    end;

  EnterCriticalSection(AFrame.PKeyFrame.CS);
  Dec(AFrame.PKeyFrame.FramesLeft);
  if AFrame.PKeyFrame.FramesLeft <= 0 then
  begin
    for i := 0 to FPaletteCount - 1 do
      TerminatePlan(AFrame.PKeyFrame.MixingPlans[i]);

    WriteLn('KF: ', AFrame.PKeyFrame.StartFrame);
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

procedure TMainForm.MakeTilesUnique(FirstTileIndex, TileCount: Integer);
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
  InitMergeTiles;

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

    i := sortList.Count - 1;
    DoOneMerge;

  finally
    sortList.Free;
  end;

  FinishMergeTiles;
end;

procedure TMainForm.LoadTiles;
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

procedure TMainForm.RGBToYUV(col: Integer; GammaCor: Integer; out y, u, v: TFloat); inline;
var
  yy, uu, vv: TFloat;
  r, g, b: Byte;
begin
  FromRGB(col, r, g, b);
  RGBToYUV(r, g, b, GammaCor, yy, uu, vv);
  y := yy; u := uu; v := vv; // for safe "out" param
end;

procedure TMainForm.RGBToYUV(r, g, b: Byte; GammaCor: Integer; out y, u, v: TFloat);
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

function TMainForm.YUVToRGB(y, u, v: TFloat): Integer;
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

procedure TMainForm.RGBToHSV(col: Integer; out h, s, v: TFloat);
var
  bh, bs, bv: Byte;
begin
  bh := 0; bs := 0; bv := 0;
  RGBToHSV(col, bh, bs, bv);
  h := bh / 255.0;
  s := bs / 255.0;
  v := bv / 255.0;
end;

procedure TMainForm.RGBToLAB(ir, ig, ib: Integer; GammaCor: Integer; out ol, oa, ob: TFloat); inline;
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

procedure TMainForm.RGBToLAB(r, g, b: TFloat; GammaCor: Integer; out ol, oa, ob: TFloat); inline;
var
  ll, aa, bb: TFloat;
begin
  RGBToLAB(Integer(round(r * 255.0)), round(g * 255.0), round(b * 255.0), GammaCor, ll, aa, bb);
  ol := ll;
  oa := aa;
  ob := bb;
end;

function TMainForm.LABToRGB(ll, aa, bb: TFloat): Integer;
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

// from https://lists.freepascal.org/pipermail/fpc-announce/2006-September/000508.html
procedure TMainForm.WaveletGS(Data : PFloat; Output : PFloat; dx, dy, depth : cardinal);
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

procedure TMainForm.DeWaveletGS(wl: PFloat; pic: PFloat; dx, dy, depth: longint);
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

procedure TMainForm.ComputeTilePsyVisFeatures(const ATile: TTile; FromPal, UseWavelets, UseLAB, QWeighting, HMirror,
  VMirror: Boolean; GammaCor: Integer; const pal: TIntegerDynArray; var DCT: array of TFloat);
const
  cUVRatio: array[0..cTileWidth-1,0..cTileWidth-1] of TFloat = (
    (0.5, sqrt(0.5), sqrt(0.5), sqrt(0.5), sqrt(0.5), sqrt(0.5), sqrt(0.5), sqrt(0.5)),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1),
    (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1)
  );
var
  u, v, x, y, xx, yy, cpn: Integer;
  z: TFloat;
  CpnPixels: array[0..cColorCpns-1, 0..cTileWidth-1,0..cTileWidth-1] of TFloat;
  pRatio, pDCT, pCpn, pLut: PFloat;

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
  Assert(Length(DCT) >= cTileDCTSize, 'DCT too small!');

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
  if UseWavelets then
  begin
    for cpn := 0 to cColorCpns - 1 do
    begin
      pCpn := @CpnPixels[cpn, 0, 0];
      WaveletGS(pCpn, pDCT, cTileWidth, cTileWidth, 2);
      Inc(pDCT, sqr(cTileWidth));
    end;
  end
  else
  begin
    for cpn := 0 to cColorCpns - 1 do
    begin
      pRatio := @cUVRatio[0, 0];
      pLut := @gDCTLut[0];

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

          pDCT^ := z * pRatio^;
          Inc(pDCT);
          Inc(pRatio);
        end;
    end;
  end;
end;

function TMainForm.VMirrorPalTile(var ATile: TTile): Boolean; // returns True if tile actually changed
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

function TMainForm.HMirrorPalTile(var ATile: TTile): Boolean; // returns True if tile actually changed
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

procedure TMainForm.LoadFrame(var AFrame: TFrame; AFrameIndex: Integer; ABitmap: TBitmap);
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
  SetLength(AFrame.FTYDone, FScreenHeight);

  AFrame.Tiles := TTile.Array1DNew(FTileMapSize, True, False);
  for i := 0 to (FTileMapSize - 1) do
  begin
    T := AFrame.Tiles[i];

    T^.Active := True;
    T^.UseCount := 1;
    T^.TmpIndex := -1;
    T^.MergeIndex := -1;
    T^.OriginalReloadedIndex := -1;
    T^.DitheringPalIndex := -1;
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
      TMI^.SmoothedTileIdx := TMI^.TileIdx;
      TMI^.SmoothedPalIdx := -1;
      TMI^.SmoothedHMirror := False;
      TMI^.SmoothedVMirror := False;

      TMI^.BlendCur := 0;
      TMI^.BlendPrev := 0;
      TMI^.BlendX := 0;
      TMI^.BlendY := 0;
    end;

  Assert(ABitmap.Width >= FScreenWidth, 'Wrong video width!');
  Assert(ABitmap.Height >= FScreenHeight, 'Wrong video height!');

  ABitmap.BeginUpdate;
  try
    pfs := @AFrame.FSPixels[0];
    for j := 0 to (FScreenHeight - 1) do
    begin
      pcol := ABitmap.ScanLine[j];
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

  finally
    ABitmap.EndUpdate;
  end;
end;

procedure TMainForm.FindKeyFrames;
var
  Correlations: TFloatDynArray;

  procedure DoCorrel(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    Correlations[AIndex] := ComputeInterFrameCorrelation(FFrames[AIndex - 1], FFrames[AIndex]);
  end;

const
  CShotTransMaxTilesPerKF = 24 * 1920 * 1080 div sqr(cTileWidth);
  CShotTransGracePeriod = 24;
  CShotTransSAvgFrames = 6;
  CShotTransSoftThres = 0.96;
  CShotTransHardThres = 0.5;
var
  i, j, LastKFIdx: Integer;
  r, ratio, prevRatio, softRatio: TFloat;
  kfIdx: Integer;
  isKf: Boolean;
  sfr, efr: Integer;
begin
  // compute interframe correlations

  SetLength(Correlations, Length(FFrames));
  ProcThreadPool.DoParallelLocalProc(@DoCorrel, 1, High(FFrames));

  // find keyframes

  kfIdx := 0;
  SetLength(FKeyFrames, Length(FFrames));
  FKeyFrames[0] := TKeyFrame.Create(FPaletteCount, 0, 0);
  FFrames[0].PKeyFrame := FKeyFrames[0];

  prevRatio := 1.0;
  softRatio := 1.0;
  LastKFIdx := 0;
  for i := 1 to High(FFrames) do
  begin
    r := Correlations[i];

    ratio := prevRatio * (1.0 - 1.0 / CShotTransSAvgFrames) + r * (1.0 / CShotTransSAvgFrames);
    prevRatio := ratio;

    softRatio -= 1.0 - ratio;

    isKf := (ratio < CShotTransHardThres) or
      (softRatio < CShotTransSoftThres) and ((i - LastKFIdx + 1) > CShotTransGracePeriod) or
      ((i - LastKFIdx + 1) * FTileMapSize > CShotTransMaxTilesPerKF);
    if isKf then
    begin
      Inc(kfIdx);
      FKeyFrames[kfIdx] := TKeyFrame.Create(FPaletteCount, 0, 0);

      WriteLn('KF: ', kfIdx, #9'Frame: -> ', i, #9'HardRatio: ', FloatToStr(ratio), #9'SoftRatio: ', FloatToStr(softRatio));

      prevRatio := 1.0;
      softRatio := 1.0;
      LastKFIdx := i;
    end;

    FFrames[i].PKeyFrame := FKeyFrames[kfIdx];
  end;

  SetLength(FKeyFrames, kfIdx + 1);

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

procedure TMainForm.ClearAll;
var
  i: Integer;
begin
  for i := 0 to High(FFrames) do
  begin
    TTile.Array1DDispose(FFrames[i].Tiles);
    FFrames[i].Free;
  end;
  SetLength(FFrames, 0);

  for i := 0 to High(FKeyFrames) do
    FKeyFrames[i].Free;
  SetLength(FKeyFrames, 0);

  TTile.Array1DDispose(FTiles);
end;

procedure TMainForm.Render(AFrameIndex: Integer; playing, blended, mirrored, reduced, gamma: Boolean; palIdx: Integer;
  ATilePage: Integer);

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
      if vmir and mirrored then tym := cTileWidth - 1 - tym;
      ptym := ty;
      if prevVmir and mirrored then ptym := cTileWidth - 1 - ptym;

      for tx := 0 to cTileWidth - 1 do
      begin
        txm := tx;
        if hmir and mirrored then txm := cTileWidth - 1 - txm;
        ptxm := tx;
        if prevHmir and mirrored then ptxm := cTileWidth - 1 - ptxm;

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

        if gamma then
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
  i, j, sx, sy, ti: Integer;
  p: PInteger;
  prevTilePtr, tilePtr: PTile;
  PsyTile: PTile;
  prevTMItem, TMItem: TTileMapItem;
  Frame: TFrame;
  prevPal, pal: TIntegerDynArray;
  oriCorr, chgCorr: TFloatDynArray3;
  q: TFloat;
begin
  if Length(FFrames) <= 0 then
    Exit;

  AFrameIndex := EnsureRange(AFrameIndex, 0, high(FFrames));

  Frame := FFrames[AFrameIndex];

  if not Assigned(Frame) or not Assigned(Frame.PKeyFrame) then
    Exit;

  PsyTile := TTile.New(True, False);
  try
    if not playing then
    begin
      SetLength(oriCorr, FTileMapHeight, FTileMapWidth, cTileDCTSize);
      SetLength(chgCorr, FTileMapHeight, FTileMapWidth, cTileDCTSize);
    end;

    if not playing then
    begin
      pnLbl.Caption := 'Global: ' + IntToStr(GetGlobalTileCount) + ' / Frame #' + IntToStr(AFrameIndex) + IfThen(Frame.PKeyFrame.StartFrame = AFrameIndex, ' [KF]', '     ') + ' : ' + IntToStr(GetFrameTileCount(Frame));

      imgTiles.Picture.Bitmap.BeginUpdate;
      try
        imgTiles.Picture.Bitmap.Canvas.Brush.Color := clAqua;
        imgTiles.Picture.Bitmap.Canvas.Brush.Style := bsSolid;
        imgTiles.Picture.Bitmap.Canvas.Clear;

        for sy := 0 to FTileMapHeight - 1 do
          for sx := 0 to FTileMapWidth - 1 do
          begin
            ti := FTileMapWidth * sy + sx + FTileMapSize * ATilePage;

            if InRange(ti, 0, High(FTiles)) then
            begin
              tilePtr := FTiles[ti];
              pal := Frame.PKeyFrame.PaletteRGB[Max(0, palIdx)];

              DrawTile(imgTiles.Picture.Bitmap, sx, sy, nil, tilePtr, pal, False, False, nil, nil, False, False, cMaxFTBlend - 1, 0);
            end;
          end;
      finally
        imgTiles.Picture.Bitmap.EndUpdate;
      end;
    end;

    imgSource.Picture.Bitmap.BeginUpdate;
    try
      for sy := 0 to FTileMapHeight - 1 do
        for sx := 0 to FTileMapWidth - 1 do
        begin
          tilePtr :=  Frame.Tiles[sy * FTileMapWidth + sx];
          DrawTile(imgSource.Picture.Bitmap, sx, sy, nil, tilePtr, nil, False, False, nil, nil, False, False, cMaxFTBlend - 1, 0);
          if not playing then
            ComputeTilePsyVisFeatures(tilePtr^, False, True, False, False, False, False, Ord(gamma) * 2 - 1, nil, oriCorr[sy, sx]);
        end;
    finally
      imgSource.Picture.Bitmap.EndUpdate;
    end;

    imgDest.Picture.Bitmap.BeginUpdate;
    try
      imgDest.Picture.Bitmap.Canvas.Brush.Color := clFuchsia;
      imgDest.Picture.Bitmap.Canvas.Brush.Style := bsDiagCross;
      imgDest.Picture.Bitmap.Canvas.Clear;

      for sy := 0 to FTileMapHeight - 1 do
        for sx := 0 to FTileMapWidth - 1 do
        begin
          TMItem := Frame.TileMap[sy, sx];
          if TMItem.Smoothed then
          begin
            TMItem.TileIdx := TMItem.SmoothedTileIdx;
            TMItem.PalIdx := TMItem.PalIdx;
            TMItem.HMirror := TMItem.HMirror;
            TMItem.VMirror := TMItem.VMirror;
          end;

          prevTMItem.TileIdx := -1;
          if (Frame.Index > 0) and (TMItem.BlendCur < cMaxFTBlend - 1) and (TMItem.BlendPrev > 0) then
          begin
            prevTMItem := FFrames[Frame.Index - 1].TileMap[sy + TMItem.BlendY, sx + TMItem.BlendX];
            if prevTMItem.Smoothed then
            begin
              prevTMItem.TileIdx := prevTMItem.SmoothedTileIdx;
              prevTMItem.PalIdx := prevTMItem.PalIdx;
              prevTMItem.HMirror := prevTMItem.HMirror;
              prevTMItem.VMirror := prevTMItem.VMirror;
            end;
          end;

          if InRange(TMItem.TileIdx, 0, High(FTiles)) then
          begin
            pal := nil;
            if palIdx < 0 then
            begin
              if not InRange(TMItem.PalIdx, 0, High(Frame.PKeyFrame.PaletteRGB)) then
                Continue;
              pal := Frame.PKeyFrame.PaletteRGB[TMItem.PalIdx]
            end
            else
            begin
              if palIdx <> TMItem.PalIdx then
                Continue;
              pal := Frame.PKeyFrame.PaletteRGB[palIdx];
            end;

            prevTilePtr := nil;
            if reduced then
            begin
              tilePtr := FTiles[TMItem.TileIdx];
              if prevTMItem.TileIdx >= 0 then
                prevTilePtr := FTiles[prevTMItem.TileIdx]
            end
            else
            begin
              tilePtr := Frame.Tiles[sy * FTileMapWidth + sx];
              pal := nil;
            end;

            if blended then
            begin
              if prevTMItem.TileIdx >= 0 then
              begin
                prevPal := FFrames[Frame.Index - 1].PKeyFrame.PaletteRGB[prevTMItem.PalIdx];
                DrawTile(imgDest.Picture.Bitmap, sx, sy, PsyTile, tilePtr, pal, TMItem.HMirror, TMItem.VMirror, prevTilePtr, prevPal, prevTMItem.HMirror, prevTMItem.VMirror, TMItem.BlendCur, TMItem.BlendPrev);
              end
              else
              begin
                DrawTile(imgDest.Picture.Bitmap, sx, sy, PsyTile, tilePtr, pal, TMItem.HMirror, TMItem.VMirror, nil, nil, False, False, TMItem.BlendCur, 0);
              end;
            end
            else
            begin
              DrawTile(imgDest.Picture.Bitmap, sx, sy, PsyTile, tilePtr, pal, TMItem.HMirror, TMItem.VMirror, nil, nil, False, False, cMaxFTBlend - 1, 0);
            end;

            if not playing then
              ComputeTilePsyVisFeatures(PsyTile^, False, True, False, False, False, False, Ord(gamma) * 2 - 1, nil, chgCorr[sy, sx]);
          end;
        end;
    finally
      imgDest.Picture.Bitmap.EndUpdate;
    end;

    imgPalette.Picture.Bitmap.BeginUpdate;
    try
      for j := 0 to imgPalette.Picture.Bitmap.Height - 1 do
      begin
        p := imgPalette.Picture.Bitmap.ScanLine[j];
        for i := 0 to imgPalette.Picture.Bitmap.Width - 1 do
        begin
          if Assigned(Frame.PKeyFrame.PaletteRGB[j]) then
            p^ := SwapRB(Frame.PKeyFrame.PaletteRGB[j, i])
          else
            p^ := clFuchsia;

          Inc(p);
        end;
      end;
    finally
      imgPalette.Picture.Bitmap.EndUpdate;
    end;

    if not playing then
    begin
      q := 0.0;
      for sy := 0 to FTileMapHeight - 1 do
        for sx := 0 to FTileMapWidth - 1 do
          q += Sqrt(CompareEuclideanDCT(oriCorr[sy, sx], chgCorr[sy, sx]));
      q /= FTileMapSize;

      lblCorrel.Caption := FormatFloat('0.0000000', q);
    end;
  finally
    Repaint;
    TTile.Dispose(PsyTile);
  end;
end;

// from https://www.delphipraxis.net/157099-fast-integer-rgb-hsl.html
procedure TMainForm.RGBToHSV(col: Integer; out h, s, v: Byte);
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

function TMainForm.HSVToRGB(h, s, v: Byte): Integer;
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

procedure TMainForm.ProgressRedraw(CurFrameIdx: Integer; ProgressStep: TEncoderStep);
const
  cProgressMul = 100;
var
  esLen: Integer;
  t: Int64;
begin
  pbProgress.Max := (Ord(High(TEncoderStep)) + 1) * cProgressMul;

  if CurFrameIdx >= 0 then
  begin
    esLen := Max(0, cEncoderStepLen[FProgressStep]) + Max(0, -cEncoderStepLen[FProgressStep]) * Length(FKeyFrames);
    FProgressPosition := iDiv0(CurFrameIdx * cProgressMul, esLen);
  end;

  if ProgressStep <> esNone then
  begin
    FProgressPosition := 0;
    FOldProgressPosition := 0;
    FProgressStep := ProgressStep;
    pbProgress.Position := Ord(FProgressStep) * cProgressMul;
    Screen.Cursor := crHourGlass;
    FProgressPrevTime := GetTickCount64;
  end;

  if (CurFrameIdx < 0) and (ProgressStep = esNone) then
  begin
    FProgressPosition := 0;
    FOldProgressPosition := 0;
    FProgressStep := esNone;
    FProgressPosition := 0;
    FProgressPrevTime := GetTickCount64;
    FProgressStartTime := FProgressPrevTime;
  end;

  pbProgress.Position := pbProgress.Position + (FProgressPosition - FOldProgressPosition);
  pbProgress.Invalidate;
  lblPct.Caption := IntToStr(pbProgress.Position * 100 div pbProgress.Max) + '%';
  lblPct.Invalidate;
  Repaint;

  t := GetTickCount64;
  if CurFrameIdx >= 0 then
  begin
    WriteLn('Step: ', Copy(GetEnumName(TypeInfo(TEncoderStep), Ord(FProgressStep)), 3), ' / ', FProgressPosition,
      #9'Time: ', FormatFloat('0.000', (t - FProgressPrevTime) / 1000), #9'All: ', FormatFloat('0.000', (t - FProgressStartTime) / 1000));
  end;
  FProgressPrevTime := t;

  FOldProgressPosition := FProgressPosition;
end;

function TMainForm.GetGlobalTileCount: Integer;
var i: Integer;
begin
  Result := 0;
  for i := 0 to High(FTiles) do
    if FTiles[i]^.Active then
      Inc(Result);
end;

function TMainForm.GetFrameTileCount(AFrame: TFrame): Integer;
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

procedure TMainForm.MergeTiles(const TileIndexes: array of Integer; TileCount: Integer; BestIdx: Integer;
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

procedure TMainForm.InitMergeTiles;
var
  i: Integer;
begin
  for i := 0 to High(FTiles) do
    FTiles[i]^.MergeIndex := -1;
end;

procedure TMainForm.FinishMergeTiles;
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

procedure TMainForm.PrepareGlobalFT;
var
  di, i: Integer;
  T: PTile;

  procedure DoOne(TileIdx: Integer; HMirror, VMirror: Boolean);
  begin
    T^.ExtractPalPixels(FGlobalDS.Dataset[di]);

    FGlobalDS.TRToTileIdx[di] := TileIdx;
    FGlobalDS.TRToAttrs[di] := Ord(HMirror) or (Ord(VMirror) shl 1);
    Inc(di);
  end;

begin
  SetLength(FGlobalDS.Dataset, GetGlobalTileCount * 4, Sqr(cTileWidth));
  SetLength(FGlobalDS.TRToTileIdx, Length(FGlobalDS.Dataset));
  SetLength(FGlobalDS.TRToAttrs, Length(FGlobalDS.Dataset));

  di := 0;
  for i := 0 to High(FTiles) do
  begin
    T := FTiles[i];
    if T^.Active then
    begin
      DoOne(i, False, False);
      if HMirrorPalTile(T^) then
        DoOne(i, True, False);
      if VMirrorPalTile(T^) then
        DoOne(i, True, True);
      if HMirrorPalTile(T^) then
        DoOne(i, False, True);
      VMirrorPalTile(T^);
    end;
  end;

  SetLength(FGlobalDS.Dataset, di, Sqr(cTileWidth));
  SetLength(FGlobalDS.TRToTileIdx, Length(FGlobalDS.Dataset));
  SetLength(FGlobalDS.TRToAttrs, Length(FGlobalDS.Dataset));

  FGlobalDS.KDT := ann_kdtree_create(@FGlobalDS.Dataset[0], Length(FGlobalDS.Dataset), Sqr(cTileWidth), 32, ANN_KD_STD);
end;

procedure TMainForm.FinishGlobalFT;
begin
  ann_kdtree_destroy(FGlobalDS.KDT);
  FGlobalDS.KDT := nil;
  SetLength(FGlobalDS.Dataset, 0);
  SetLength(FGlobalDS.TRToTileIdx, 0);
  SetLength(FGlobalDS.TRToAttrs, 0);
end;

procedure TMainForm.PrepareFrameTiling(AKF: TKeyFrame; AFTGamma: Integer; AUseWavelets: Boolean; AFTQuality: TFTQuality
  );
var
  DS: array of PTilingDataset;
  used: array of array of array[-1..3] of Boolean;
  usedCount: TIntegerDynArray;
  PaletteDists: TFloatDynArray2;
  HighestDist: TFloat;

  procedure UseOne(Item: PTileMapItem);
  var
    i, idx: Integer;
    palIdx: Integer;
    idxs: array[0 .. cGlobalFTBucketSize - 1] of Integer;
    errs: array[0 .. cGlobalFTBucketSize - 1] of TANNFloat;
    Line: array[0 .. Sqr(cTileWidth) - 1] of TANNFloat;
    wasUsed: Boolean;
  begin
    SpinEnter(@FLock);
    wasUsed := used[Item^.PalIdx, Item^.TileIdx, -1];
    used[Item^.PalIdx, Item^.TileIdx, -1] := True;
    SpinLeave(@FLock);

    if not wasUsed then
    begin
      FTiles[Item^.TileIdx]^.ExtractPalPixels(Line);

      ann_kdtree_pri_search_multi(FGlobalDS.KDT, @idxs[0], @errs[0], cGlobalFTBucketSize, @Line[0], 0.0);

      for i := 0 to cGlobalFTBucketSize - 1 do
      begin
        idx := idxs[i];
        if idx < 0 then
          Continue;

         for palIdx := 0 to FPaletteCount - 1 do
          if PaletteDists[palIdx, Item^.PalIdx] <= cFTIntraPaletteTol[AFTQuality] * HighestDist then
            used[palIdx, FGlobalDS.TRToTileIdx[idx], FGlobalDS.TRToAttrs[idx]] := True;
      end;
    end;
  end;

  function BuildPaletteDistsTriangle: TFloatDynArray2;
  var
    i, j : Integer;
  begin
    SetLength(Result, FPaletteCount, FPaletteCount);
    for j := 0 to FPaletteCount - 1 do
      for i := 0 to FPaletteCount - 1 do
      begin
        Result[j, i] := CompareEuclideanDCT(AKF.PaletteCentroids[j], AKF.PaletteCentroids[i]);
        if not IsNan(Result[j, i]) then
          HighestDist := Max(HighestDist, Result[j, i]);
      end;
  end;

  procedure DoBuild(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    frm: TFrame;
    idx, sy, sx: Integer;
  begin
    if not InRange(AIndex, 0, AKF.FrameCount * FTileMapHeight - 1) then
      Exit;

    DivMod(AIndex, FTileMapHeight, idx, sy);

    frm := FFrames[AKF.StartFrame + idx];
    for sx := 0 to FTileMapWidth - 1 do
      UseOne(@frm.TileMap[sy, sx]);
  end;

  procedure DoPsyV(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    di, ti, j, hvmir, palIdx2: Integer;
    T: PTile;
    DCT: array[0 .. cTileDCTSize - 1] of TFloat;
  begin
    if not InRange(AIndex, 0, FPaletteCount - 1) then
      Exit;

    di := 0;
    for ti := 0 to High(FTiles) do
      for hvmir := 0 to 3 do
        if used[AIndex, ti, hvmir] then
          for palIdx2 := 0 to FPaletteCount - 1 do
            if (PaletteDists[AIndex, palIdx2] <= cFTInterPaletteTol[AFTQuality] * HighestDist) and used[palIdx2, ti, hvmir] then
            begin
              T := FTiles[ti];
              DS[AIndex]^.TRToTileIdx[di] := ti;
              DS[AIndex]^.TRToPalIdx[di] := palIdx2;
              DS[AIndex]^.TRToAttrs[di] := hvmir;

              ComputeTilePsyVisFeatures(T^, True, AUseWavelets, False, False, hvmir and 1 <> 0, hvmir and 2 <> 0, AFTGamma, AKF.PaletteRGB[palIdx2], DCT);
              for j := 0 to cTileDCTSize - 1 do
                DS[AIndex]^.Dataset[di, j] := DCT[j];
              Inc(di);
            end;

    Assert(di = usedCount[AIndex]);

    if Length(DS[AIndex]^.Dataset) > 0  then
      DS[AIndex]^.KDT := ann_kdtree_create(@DS[AIndex]^.Dataset[0], Length(DS[AIndex]^.Dataset), cTileDCTSize, 32, ANN_KD_STD);
  end;

var
  KNNSize, ti, hvmir: Integer;
  palIdx, palIdx2: Integer;
  thres: TFloat;
begin
  HighestDist := 0.0;
  PaletteDists := BuildPaletteDistsTriangle;

  SetLength(usedCount, FPaletteCount);
  SetLength(used, FPaletteCount, Length(FTiles));
  for palIdx := 0 to FPaletteCount - 1 do
    FillByte(used[palIdx, 0], Length(FTiles) * SizeOf(used[0, 0]), 0);

  // Build an indicator table of used tiles

  ProcThreadPool.DoParallelLocalProc(@DoBuild, 0, AKF.FrameCount * FTileMapHeight - 1, nil, NumberOfProcessors);

  // Compute psycho visual model for all used tiles (in all palettes / mirrors)

  FillDWord(usedCount[0], FPaletteCount, 0);
  thres := cFTInterPaletteTol[AFTQuality] * HighestDist;
  for palIdx := 0 to FPaletteCount - 1 do
    for palIdx2 := 0 to FPaletteCount - 1 do
      for ti := 0 to High(FTiles) do
        for hvmir := 0 to 3 do
          Inc(usedCount[palIdx], Ord(used[palIdx, ti, hvmir] and used[palIdx2, ti, hvmir] and (PaletteDists[palIdx, palIdx2] <= thres)));

  SetLength(DS, FPaletteCount);
  AKF.TileDS := DS;
  for palIdx := 0 to FPaletteCount - 1 do
  begin
    KNNSize := usedCount[palIdx];

    DS[palIdx] := New(PTilingDataset);
    FillChar(DS[palIdx]^, SizeOf(TTilingDataset), 0);

    SetLength(DS[palIdx]^.TRToTileIdx, KNNSize);
    SetLength(DS[palIdx]^.TRToPalIdx, KNNSize);
    SetLength(DS[palIdx]^.TRToAttrs, KNNSize);
    SetLength(DS[palIdx]^.Dataset, KNNSize, cTileDCTSize);

    DS[palIdx]^.DistErrCml := 0.0;
    DS[palIdx]^.DistErrCnt := 0;
  end;

  ProcThreadPool.DoParallelLocalProc(@DoPsyV, 0, FPaletteCount - 1, nil, NumberOfProcessors);

  // Build KNN

  KNNSize := 0;
  for palIdx := 0 to FPaletteCount - 1 do
    KNNSize += usedCount[palIdx];

  WriteLn('KF: ', AKF.StartFrame, #9'KNNSize: ', KNNSize);
end;

procedure TMainForm.FinishFrameTiling(AKF: TKeyFrame);
var
  i: Integer;
  resDist: TFloat;
begin
  resDist := 0.0;
  for i := 0 to FPaletteCount - 1 do
    if AKF.TileDS[i]^.DistErrCnt <> 0 then
    begin
      //WriteLn(AKF.StartFrame, #9, i, #9, FloatToStr(AKF.TileDS^.DistErrCml[i] / AKF.TileDS^.DistErrCnt[i]));
      resDist += AKF.TileDS[i]^.DistErrCml;
    end;
  WriteLn('KF: ', AKF.StartFrame, #9'ResidualErr: ', FloatToStr(resDist));

  for i := 0 to FPaletteCount - 1 do
  begin
    if Length(AKF.TileDS[i]^.Dataset) > 0 then
      ann_kdtree_destroy(AKF.TileDS[i]^.KDT);
    AKF.TileDS[i]^.KDT := nil;
    SetLength(AKF.TileDS[i]^.Dataset, 0);
    SetLength(AKF.TileDS[i]^.TRToTileIdx, 0);
    SetLength(AKF.TileDS[i]^.TRToAttrs, 0);
    Dispose(AKF.TileDS[i]);
  end;

  SetLength(AKF.TileDS, 0);
  AKF.TileDS := nil;
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

procedure TMainForm.DoFrameTiling(AFrame: TFrame; Y: Integer; AFTGamma: Integer; AUseWavelets: Boolean;
  AAddlTilesThres: TFloat; AFTBlend: Integer);
var
  i, bestIdx, PalIdx, bucketIdx, idx: Integer;
  attrs, bestBlendCur, bestBlendPrev: Byte;
  x, oy, ox, bestX, bestY: Integer;
  bestErr: TFloat;

  tmiO, prevTMI: PTileMapItem;
  TmpTMI: TTileMapItem;
  prevTile, addlTile: PTile;
  plan: TMixingPlan;

  idxs: array[0 .. cFrameFTBucketSize - 1] of Integer;
  errs: array[0 .. cFrameFTBucketSize - 1] of TANNFloat;
  DS: array of PTilingDataset;
  DCT: TFloatDynArray;
  CurPrevDCT: TFloatDynArray2;
  ANNDCT: TANNFloatDynArray;

  ATList: TList;

  procedure AccumulateErr;
  begin
    SpinEnter(@FLock);
    DS[PalIdx]^.DistErrCml += bestErr;
    Inc(DS[PalIdx]^.DistErrCnt);
    SpinLeave(@FLock);
  end;

  procedure TestBestErr(err: TANNFloat; bc, bp: Integer);
  begin
    if err < bestErr then
    begin
      bestErr := err;
      bestIdx := idx;
      bestBlendCur := bc;
      bestBlendPrev := bp;
      bestX := ox;
      bestY := oy;
    end;
  end;

  procedure SearchBlending2P(Plain: array of ArbFloat; CurPrev: TFloatDynArray2);
  var
    term, bc, bp: Integer;
    fcp: array[0 .. 1] of ArbFloat;
    fc, fp, err: TFloat;
  begin
    slegls(CurPrev[2, 0], cTileDCTSize, 2, 2, Plain[0], fcp[0], term);
    if term = 1 then
    begin
      bc := EnsureRange(round(fcp[0] * (cMaxFTBlend - 1)), 0, cMaxFTBlend - 1);
      fc := bc * (1.0 / (cMaxFTBlend - 1));

      // try to compensate for rounding to 16 levels by sending rounding error to other parameter

      fp := fcp[1] + fcp[0] - fc;
      bp := EnsureRange(round(fp * (cMaxFTBlend - 1)), 0, cMaxFTBlend - 1);
      fp := bp * (1.0 / (cMaxFTBlend - 1));

      err := ComputeBlendingError(@Plain[0], @CurPrev[0, 0], @CurPrev[1, 0], fc, fp);
      TestBestErr(err, bc, bp);
    end;
  end;

  procedure SearchBlending1P(Plain, Cur: array of ArbFloat);
  var
    term, bc: Integer;
    fc: ArbFloat;
    err: TFloat;
  begin
    slegls(Cur[0], cTileDCTSize, 1, 1, Plain[0], fc, term);
    if term = 1 then
    begin
      bc := EnsureRange(round(fc * (cMaxFTBlend - 1)), 0, cMaxFTBlend - 1);
      fc := bc * (1.0 / (cMaxFTBlend - 1));
      err := ComputeBlendingError(@Plain[0], @Cur[0], @Cur[0], fc, 0.0);
      TestBestErr(err, bc, 0);
    end;
  end;

begin
  DS := AFrame.PKeyFrame.TileDS;

  // map AFrame tilemap items to reduced tiles and mirrors and choose best corresponding palette

  SetLength(DCT, cTileDCTSize);
  SetLength(ANNDCT, cTileDCTSize);
  SetLength(CurPrevDCT, 3, cTileDCTSize * 2);

  for x := 0 to FTileMapWidth - 1 do
  begin
    PalIdx := AFrame.TileMap[Y, x].PalIdx;
    if Length(DS[PalIdx]^.Dataset) <= 0 then
      Continue;

    ComputeTilePsyVisFeatures(AFrame.Tiles[Y * FTileMapWidth + x]^, False, AUseWavelets, False, False, False, False, AFTGamma, nil, DCT);
    for i := 0 to cTileDCTSize - 1 do
      ANNDCT[i] := DCT[i];

    ann_kdtree_pri_search_multi(DS[PalIdx]^.KDT, @idxs[0], @errs[0], cFrameFTBucketSize, @ANNDCT[0], 0.0);

    bestBlendCur := cMaxFTBlend - 1;
    bestBlendPrev := 0;
    bestX := x;
    bestY := Y;
    if (AFTBlend >= 0) and not IsZero(errs[0]) then
    begin
      // try to blend a local tile of the previous frame to improve likeliness

      bestIdx := idxs[0];
      bestErr := errs[0];
      for bucketIdx := 0 to cFrameFTBucketSize - 1 do
      begin
        idx := idxs[bucketIdx];
        if idx < 0 then
          Continue;

        TmpTMI := FFrames[AFrame.Index].TileMap[Y, x];

        attrs := DS[PalIdx]^.TRToAttrs[idx];
        TmpTMI.TileIdx := DS[PalIdx]^.TRToTileIdx[idx];
        TmpTMI.PalIdx := DS[PalIdx]^.TRToPalIdx[idx];
        TmpTMI.HMirror := (attrs and 1) <> 0;
        TmpTMI.VMirror := (attrs and 2) <> 0;

        ComputeTilePsyVisFeatures(FTiles[TmpTMI.TileIdx]^, True, AUseWavelets, False, False, TmpTMI.HMirror, TmpTMI.VMirror, AFTGamma, AFrame.PKeyFrame.PaletteRGB[TmpTMI.PalIdx], CurPrevDCT[0]);
        for i := 0 to cTileDCTSize - 1 do
          CurPrevDCT[2, i * 2] := CurPrevDCT[0, i];

        for oy := Y - AFTBlend to Y + AFTBlend do
        begin
          if not InRange(oy, 0, FTileMapHeight - 1) then
            Continue;

          if AFrame.Index > 0 then
            while not FFrames[AFrame.Index - 1].FTYDone[oy] do
              Sleep(10); // wait for previous frame to compute tilemap line

          for ox := x - AFTBlend to x + AFTBlend do
          begin
            if not InRange(ox, 0, FTileMapWidth - 1) then
              Continue;

            if AFrame.Index > 0 then
            begin
              prevTile := nil;
              prevTMI := @FFrames[AFrame.Index - 1].TileMap[oy, ox];
              if prevTMI^.TileIdx < Length(FTiles) then
              begin
                prevTile := FTiles[prevTMI^.TileIdx];
              end
              else
              begin
                ATList := FAdditionalTiles.LockList;
                try
                  prevTile := PTile(ATList[prevTMI^.TileIdx - Length(FTiles)]);
                finally
                  FAdditionalTiles.UnlockList;
                end;
              end;

              ComputeTilePsyVisFeatures(prevTile^, True, AUseWavelets, False, False, prevTMI^.HMirror, prevTMI^.VMirror, AFTGamma, FFrames[AFrame.Index - 1].PKeyFrame.PaletteRGB[prevTMI^.PalIdx], CurPrevDCT[1]);
              for i := 0 to cTileDCTSize - 1 do
                CurPrevDCT[2, i * 2 + 1] := CurPrevDCT[1, i];

              SearchBlending2P(DCT, CurPrevDCT);
            end
            else
            begin
              SearchBlending1P(DCT, CurPrevDCT[0]);
            end;
          end;
        end;
      end;
    end
    else
    begin
      // in case no blending, first element from KNN is best

      bestIdx := idxs[0];
      bestErr := errs[0];
    end;

    tmiO := @FFrames[AFrame.Index].TileMap[Y, x];

    if IsZero(AAddlTilesThres) or (bestErr < AAddlTilesThres) then
    begin
      attrs := DS[PalIdx]^.TRToAttrs[bestIdx];
      tmiO^.TileIdx := DS[PalIdx]^.TRToTileIdx[bestIdx];
      tmiO^.PalIdx := DS[PalIdx]^.TRToPalIdx[bestIdx];
      tmiO^.HMirror := (attrs and 1) <> 0;
      tmiO^.VMirror := (attrs and 2) <> 0;
      tmiO^.BlendCur := bestBlendCur;
      tmiO^.BlendPrev := bestBlendPrev;
      tmiO^.BlendX := bestX - x;
      tmiO^.BlendY := bestY - Y;

      AccumulateErr;
    end
    else
    begin
      ATList := FAdditionalTiles.LockList;
      try
        addlTile := TTile.New(True, True);
        addlTile^.CopyFrom(AFrame.Tiles[Y * FTileMapWidth + x]^);
        ATList.Add(addlTile);

        // redither tile (frame tiles don't have the paletted version)
        PreparePlan(plan, FY2MixedColors, AFrame.PKeyFrame.PaletteRGB[tmiO^.PalIdx]);
        DitherTile(addlTile^, plan);
        TerminatePlan(plan);

        AccumulateErr;

        tmiO^.TileIdx := Length(FTiles) + ATList.Count - 1;
        tmiO^.HMirror := False;
        tmiO^.VMirror := False;
        tmiO^.BlendCur := cMaxFTBlend - 1;
        tmiO^.BlendPrev := 0;
        tmiO^.BlendX := 0;
        tmiO^.BlendY := 0;
      finally
        FAdditionalTiles.UnlockList;
      end;
    end;
  end;

  if Y = FTileMapHeight - 1 then
    WriteLn('Frame: ', AFrame.Index, #9'FramesLeft: ', AFrame.PKeyFrame.FramesLeft - 1);
end;

procedure TMainForm.DoTemporalSmoothing(AFrame, APrevFrame: TFrame; Y: Integer; Strength: TFloat);
const
  cSqrtFactor = 1 / cTileDCTSize;
var
  sx: Integer;
  cmp: TFloat;
  TMI, PrevTMI: PTileMapItem;
  DCT, PrevDCT: TFloatDynArray;
begin
  if AFrame.PKeyFrame <> APrevFrame.PKeyFrame then
    Exit;

  SetLength(DCT, cTileDCTSize);
  SetLength(PrevDCT, cTileDCTSize);

  for sx := 0 to FTileMapWidth - 1 do
  begin
    // compare DCT of current tile with tile from prev frame tilemap

    PrevTMI := @APrevFrame.TileMap[Y, sx];
    ComputeTilePsyVisFeatures(FTiles[PrevTMI^.SmoothedTileIdx]^, True, False, False, True, PrevTMI^.SmoothedHMirror, PrevTMI^.SmoothedVMirror, -1, APrevFrame.PKeyFrame.PaletteRGB[PrevTMI^.SmoothedPalIdx], PrevDCT);

    TMI := @AFrame.TileMap[Y, sx];
    ComputeTilePsyVisFeatures(FTiles[TMI^.SmoothedTileIdx]^, True, False, False, True, TMI^.SmoothedHMirror, TMI^.SmoothedVMirror, -1, AFrame.PKeyFrame.PaletteRGB[TMI^.SmoothedPalIdx], DCT);

    cmp := CompareEuclideanDCT(DCT, PrevDCT);
    cmp := sqrt(cmp * cSqrtFactor);

    // if difference is low enough, mark the tile as smoothed for tilemap compression use

    if cmp <= Strength then
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
end;

function TMainForm.GetTileUseCount(ATileIndex: Integer): Integer;
var
  i, sx, sy: Integer;
begin
  Result := 0;
  for i := 0 to High(FFrames) do
    for sy := 0 to FTileMapHeight - 1 do
      for sx := 0 to FTileMapWidth - 1 do
        Inc(Result, Ord(FFrames[i].TileMap[sy, sx].TileIdx = ATileIndex));
end;

function TMainForm.GetTileZoneSum(const ATile: TTile; x, y, w, h: Integer): Integer;
var
  i, j: Integer;
begin
  Result := 0;
  for j := y to y + h - 1 do
    for i := x to x + w - 1 do
      Result += ATile.PalPixels[j, i];
end;

function TMainForm.GetTilePalZoneThres(const ATile: TTile; ZoneCount: Integer; Zones: PByte): Integer;
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
      Inc(acc[b * ZoneCount div FTilePaletteSize]);
    end;

  Result := sqr(cTileWidth);
  for i := 0 to ZoneCount - 1 do
  begin
    Result := Min(Result, sqr(cTileWidth) - acc[i]);
    signi := acc[i] > (FTilePaletteSize div ZoneCount);
    Zones^ := Ord(signi);
    Inc(Zones);
  end;
end;

function TMainForm.WriteTileDatasetLine(const ATile: TTile; DataLine: PByte; out PalSigni: Integer): Integer;
var
  x, y: Integer;
  hsv: array[0..3, 0..2] of Word;
  ph, ps, pv: Byte;
begin
  Result := 0;

  // tile pixels

  for y := 0 to cTileWidth - 1 do
    for x := 0 to cTileWidth - 1 do
    begin
      DataLine[Result] := ATile.PalPixels[y, x];
      Inc(Result);
    end;

  // HSV for 4x4 quadrants

  FillChar(hsv, SizeOf(hsv), 0);
  for y := 0 to cTileWidth div 2 - 1 do
    for x := 0 to cTileWidth div 2 - 1 do
    begin
      RGBToHSV(ATile.RGBPixels[y, x], ph, ps, pv);
      hsv[0, 0] += ph; hsv[0, 1] += ps; hsv[0, 2] += pv;

      RGBToHSV(ATile.RGBPixels[y, x + cTileWidth div 2], ph, ps, pv);
      hsv[1, 0] += ph; hsv[1, 1] += ps; hsv[1, 2] += pv;

      RGBToHSV(ATile.RGBPixels[y + cTileWidth div 2, x], ph, ps, pv);
      hsv[2, 0] += ph; hsv[2, 1] += ps; hsv[2, 2] += pv;

      RGBToHSV(ATile.RGBPixels[y + cTileWidth div 2, x + cTileWidth div 2], ph, ps, pv);
      hsv[3, 0] += ph; hsv[3, 1] += ps; hsv[3, 2] += pv;
    end;

  for y := 0 to 3 do
  begin
    DataLine[Result] := hsv[y, 0] shr 8; // to 4 bits from 8 bits * sqr(cTileWidth div 2)=16
    Inc(Result);
    DataLine[Result] := hsv[y, 1] shr 8;
    Inc(Result);
    DataLine[Result] := hsv[y, 2] shr 8;
    Inc(Result);
  end;

  // count of most used index per 4x4 quadrant

  PalSigni := GetTilePalZoneThres(ATile, sqr(cTileWidth) div 16, @DataLine[Result]);
  Inc(Result, sqr(cTileWidth) div 16);

  Assert(Result = cKModesFeatureCount);
end;

type
  TKModesBin = record
    Dataset: TByteDynArray2;
    TileIndices: TIntegerDynArray;
    ClusterCount: Integer;
    StartingPoint: Integer;
  end;

  PKModesBin = ^TKModesBin;
  TKModesBinArray = array of TKModesBin;
  PKModesBinArray = ^TKModesBinArray;

procedure TMainForm.DoGlobalKModes(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
var
  KModes: TKModes;
  LocCentroids: TByteDynArray2;
  LocClusters: TIntegerDynArray;
  lineIdx, clusterIdx, clusterLineCount, DSLen, tx, ty, j, k, bestIdx, bestVal: Integer;
  ToMergeIdxs: TIntegerDynArray;
  KMBin: PKModesBin;
  median: array[0 .. Sqr(cTileWidth) - 1, 0 .. Sqr(cTileWidth) - 1] of Integer;
begin
  if not InRange(AIndex, 0, FPaletteCount - 1) then
    Exit;

  KMBin := @PKModesBinArray(AData)^[AIndex];
  DSLen := Length(KMBin^.Dataset);
  if DSLen <= KMBin^.ClusterCount then
    Exit;

  SetLength(LocClusters, DSLen);
  SetLength(LocCentroids, KMBin^.ClusterCount, cTileDCTSize);

  KModes := TKModes.Create(1, -1, True, 'Bin: ' + IntToStr(AIndex) + #9, @FConcurrentKModesBins);
  try
    KMBin^.ClusterCount := KModes.ComputeKModes(KMBin^.Dataset, round(KMBin^.ClusterCount), -KMBin^.StartingPoint, FTilePaletteSize, LocClusters, LocCentroids);
    Assert(Length(LocCentroids) = KMBin^.ClusterCount);
    Assert(MaxIntValue(LocClusters) = KMBin^.ClusterCount - 1);
  finally
    KModes.Free;
    InterLockedDecrement(FConcurrentKModesBins);
  end;

  Assert(Length(LocCentroids) = KMBin^.ClusterCount);
  Assert(MaxIntValue(LocClusters) = KMBin^.ClusterCount - 1);

  SetLength(ToMergeIdxs, DSLen);

  // build a list of this centroid tiles

  for clusterIdx := 0 to KMBin^.ClusterCount - 1 do
  begin
    clusterLineCount := 0;
    for lineIdx := 0 to High(KMBin^.TileIndices) do
    begin
      if LocClusters[lineIdx] = clusterIdx then
      begin
        ToMergeIdxs[clusterLineCount] := KMBin^.TileIndices[lineIdx];
        Inc(clusterLineCount);
      end;
    end;

    // devise a single tile for the cluster

    if clusterLineCount >= 2 then
    begin
      FillChar(median[0, 0], Sizeof(median), 0);

      // build a median of the cluster

      for j := 0 to clusterLineCount - 1 do
      begin
        k := 0;
        for ty := 0 to cTileWidth - 1 do
          for tx := 0 to cTileWidth - 1 do
          begin
            Inc(median[k, FTiles[ToMergeIdxs[j]]^.PalPixels[ty, tx]], FTiles[ToMergeIdxs[j]]^.UseCount);
            Inc(k);
          end;
      end;

      // use it as centroid

      FTiles[ToMergeIdxs[0]]^.ClearRGBPixels;
      k := 0;
      for ty := 0 to cTileWidth - 1 do
        for tx := 0 to cTileWidth - 1 do
        begin
          bestIdx := -1;
          bestVal := -1;
          for j := 0 to FTilePaletteSize - 1 do
            if median[k, j] > bestVal then
            begin
              bestIdx := j;
              bestVal := median[k, j];
            end;

          FTiles[ToMergeIdxs[0]]^.PalPixels[ty, tx] := bestIdx;
          Inc(k);
        end;



      // apply to cluster
      SpinEnter(@FLock);
      MergeTiles(ToMergeIdxs, clusterLineCount, ToMergeIdxs[0], nil, nil);
      SpinLeave(@FLock);
    end;
  end;

  // free up memory

  SetLength(KMBin^.TileIndices, 0);
end;


procedure TMainForm.DoGlobalTiling(DesiredNbTiles, RestartCount: Integer);
const
  cBinCount = 16;

  function GetTileBin(ATile: PTile): Byte;
  var
    tx, ty, rr, gg, bb: Integer;
    r, g, b, h, s, v, sh: Byte;
  begin
    rr := 0; gg := 0; bb := 0;
    for ty := 0 to cTileWidth - 1 do
      for tx := 0 to cTileWidth - 1 do
      begin
        FromRGB(ATile^.RGBPixels[ty, tx], r, g, b);
        rr += r; gg += g; bb += b;
      end;
    rr := rr div Sqr(cTileWidth); gg := gg div Sqr(cTileWidth); bb := bb div Sqr(cTileWidth);

    RGBToHSV(ToRGB(rr, gg, bb), h, s, v);

    sh := 8 - Round(Log2(cBinCount));
    Result := (h shr sh) xor (s shr sh) xor (v shr sh);
  end;

var
  KMBins: TKModesBinArray;
  acc, i, j, disCnt, bin, dummy: Integer;
  cnt: TIntegerDynArray;
  best: TIntegerDynArray;
  DataLine: TByteDynArray;
  share: TFloat;
begin

  // prepare KModes dataset, one line per tile, psychovisual model as features
  // also choose KModes starting point

  FConcurrentKModesBins := cBinCount;
  SetLength(KMBins, cBinCount);
  SetLength(cnt, Length(KMBins));
  SetLength(best, Length(KMBins));
  SetLength(DataLine, cKModesFeatureCount);

  // precompute dataset size

  FillDWord(cnt[0], Length(cnt), 0);
  for i := 0 to High(FTiles) do
  begin
    if not FTiles[i]^.Active then
      Continue;

    bin := GetTileBin(FTiles[i]);

    Inc(cnt[bin]);
  end;

  for i := 0 to High(KMBins) do
  begin
    SetLength(KMBins[i].TileIndices, cnt[i]);
    SetLength(KMBins[i].Dataset, cnt[i], cKModesFeatureCount);
  end;
  FillDWord(cnt[0], Length(cnt), 0);
  FillDWord(best[0], Length(best), 0);

  // bin tiles by PalSigni (highest number of pixels the same color from the tile)

  for i := 0 to High(FTiles) do
  begin
    if not FTiles[i]^.Active then
      Continue;

    WriteTileDatasetLine(FTiles[i]^, @DataLine[0], dummy);
    bin := GetTileBin(FTiles[i]);

    KMBins[bin].TileIndices[cnt[bin]] := i;

    acc := 0;
    for j := 0 to cKModesFeatureCount - 1 do
    begin
      KMBins[bin].Dataset[cnt[bin], j] := DataLine[j];
      acc += KMBins[bin].Dataset[cnt[bin], j];
    end;
    if acc <= best[bin] then
    begin
      KMBins[bin].StartingPoint := cnt[bin];
      best[bin] := acc;
    end;

    Inc(cnt[bin]);
  end;

  // share DesiredNbTiles among bins, proportional to amount of tiles

  disCnt := 0;
  for i := 0 to High(cnt) do
    disCnt += EqualQualityTileCount(cnt[i]);
  share := DesiredNbTiles / disCnt;

  for i := 0 to High(cnt) do
    KMBins[i].ClusterCount := ceil(EqualQualityTileCount(cnt[i]) * share);

  InitMergeTiles;

  ProgressRedraw(1);

  // run the KMeans algorithm, which will group similar tiles until it reaches a fixed amount of groups

  ProcThreadPool.DoParallel(@DoGlobalKModes, 0, High(KMBins), @KMBins);

  ProgressRedraw(2);

  FinishMergeTiles;

  // ensure inter block tile unicity

  MakeTilesUnique(0, Length(FTiles));

  ProgressRedraw(3);

  // put most probable tiles first and remove unused tiles

  ReindexTiles;

  ProgressRedraw(4);
end;

procedure TMainForm.ReloadPreviousTiling(AFN: String);
var
  SigniDataset: TByteDynArray3;
  Dataset: TByteDynArray2;
  ParallelCount: PtrUInt;

  procedure DoFindBest(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    last, bin, signi, i, tidx: Integer;
    DataLine: TByteDynArray;
    dis: UInt64;
  begin
    if not InRange(AIndex, 0, ParallelCount - 1) then
      Exit;

    SetLength(DataLine, cKModesFeatureCount);

    bin := Length(FTiles) div ParallelCount;
    last := (AIndex + 1) * bin - 1;
    if AIndex >= ParallelCount - 1 then
      last := High(FTiles);

    for i := bin * AIndex to last do
    begin
      if FTiles[i]^.Active then
      begin
        WriteTileDatasetLine(FTiles[i]^, @DataLine[0], signi);
        if Length(SigniDataset[signi]) > 0 then
        begin
          tidx := GetMinMatchingDissim(SigniDataset[signi], DataLine, Length(SigniDataset[signi]), dis);
          FTiles[i]^.CopyPalPixels(SigniDataset[signi, tidx]);
        end
        else
        begin
          tidx := GetMinMatchingDissim(Dataset, DataLine, Length(Dataset), dis);
          FTiles[i]^.CopyPalPixels(Dataset[tidx]);
        end;
      end;

      if i mod 10000 = 0 then
        WriteLn('Thread: ', GetCurrentThreadId, #9'TileIdx: ', i);
    end;
  end;

var
  signi, i, y, x: Integer;
  fs: TFileStream;
  T: TTile;
  SigniIndices: TIntegerDynArray2;
  TilingPaletteSize: Integer;
  hasUseCount: Boolean;
  PalPixels: TPalPixels;
begin
  fs := TFileStream.Create(AFN, fmOpenRead or fmShareDenyNone);
  try
    FillChar(T, SizeOf(T), 0);
    T.Active := True;

    SetLength(SigniIndices, High(Word) + 1, 0);
    SetLength(Dataset, fs.Size div sqr(cTileWidth), cKModesFeatureCount);

    hasUseCount := fs.Size mod sqr(cTileWidth) = 2;
    if hasUseCount then
      fs.ReadByte; // version

    TilingPaletteSize := sqr(cTileWidth);
    if hasUseCount or (fs.Size mod sqr(cTileWidth) <> 0) then
      TilingPaletteSize := fs.ReadByte;

    for i := 0 to High(Dataset) do
    begin
      fs.ReadBuffer(PalPixels[0, 0], SizeOf(TPalPixels));
      for y := 0 to cTileWidth - 1 do
        for x := 0 to cTileWidth - 1 do
          PalPixels[y, x] := (PalPixels[y, x] * FTilePaletteSize) div TilingPaletteSize;
      T.CopyPalPixels(PalPixels);

      WriteTileDatasetLine(T, @Dataset[i, 0], signi);

      SetLength(SigniIndices[signi], Length(SigniIndices[signi]) + 1);
      SigniIndices[signi][High(SigniIndices[signi])] := i;
    end;

    SetLength(SigniDataset, High(Word) + 1, 0);
    for signi := 0 to High(Word) do
      if Length(SigniIndices[signi]) > 0 then
      begin
        SetLength(SigniDataset[signi], Length(SigniIndices[signi]));
        for i := 0 to High(SigniIndices[signi]) do
          SigniDataset[signi, i] := Dataset[SigniIndices[signi, i]];
      end;

    SetLength(SigniIndices, 0);

    ProgressRedraw(1);

    ParallelCount := ProcThreadPool.MaxThreadCount * 10;
    ProcThreadPool.DoParallelLocalProc(@DoFindBest, 0, ParallelCount - 1);

    ProgressRedraw(4);

    MakeTilesUnique(0, Length(FTiles));

    ProgressRedraw(5);
  finally
    fs.Free;
  end;
end;

function CompareTileUseCountRev(Item1, Item2, UserParameter:Pointer):Integer;
var
  t1, t2: PTile;
begin
  t1 := PPTile(Item1)^;
  t2 := PPTile(Item2)^;
  Result := CompareValue(t2^.UseCount, t1^.UseCount);
  if Result = 0 then
    Result := CompareValue(t1^.TmpIndex, t2^.TmpIndex);
end;

procedure TMainForm.ReindexTiles;
var
  i, j, x, y, cnt, idx: Integer;
  IdxMap: TIntegerDynArray;
  Frame: ^TFrame;
  Tiles: PTileDynArray;
begin
  cnt := 0;
  for i := 0 to High(FTiles) do
  begin
    FTiles[i]^.KFSoleIndex := -1;
    FTiles[i]^.TmpIndex := i;
    if FTiles[i]^.Active then
      Inc(cnt);
  end;

  // pack the global tiles, removing inactive ones

  Tiles := TTile.Array1DNew(cnt, False, True);
  j := 0;
  for i := 0 to High(FTiles) do
    if FTiles[i]^.Active then
    begin
      Tiles[j]^.CopyFrom(FTiles[i]^);
      Inc(j);
    end;

  SetLength(IdxMap, Length(FTiles));
  FillDWord(IdxMap[0], Length(FTiles), $ffffffff);

  TTile.Array1DDispose(FTiles);
  FTiles := Tiles;
  Tiles := nil;

  // sort global tiles by use count descending (to make smoothing work better) then by tile index (to make tile indexes compression work better)

  QuickSort(FTiles[0], 0, High(FTiles), SizeOf(PTile), @CompareTileUseCountRev);

  // point tilemap items on new tiles indexes

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
          if FTiles[idx]^.KFSoleIndex < 0 then
            FTiles[idx]^.KFSoleIndex := Frame^.PKeyFrame.StartFrame
          else if FTiles[idx]^.KFSoleIndex <> Frame^.PKeyFrame.StartFrame then
            FTiles[idx]^.KFSoleIndex := MaxInt;
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

procedure TMainForm.SaveStream(AStream: TStream; AFTBlend: Integer);
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
  begin
    Assert((TMI.SmoothedPalIdx >= 0) and (TMI.SmoothedPalIdx < FPaletteCount));

    attrs := (TMI.SmoothedPalIdx shl 2) or (Ord(TMI.SmoothedVMirror) shl 1) or Ord(TMI.SmoothedHMirror);
    blend := (Word(TMI.BlendY and $f) shl 12) or (Word(TMI.BlendX and $f) shl 8) or (Word(TMI.BlendPrev and $f) shl 4) or Word(TMI.BlendCur and $f);

    if TMI.TileIdx < (1 shl 16) then
    begin
      if (AFTBlend < 0) or (TMI.BlendPrev = 0) and (TMI.BlendCur = cMaxFTBlend - 1) then
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
      if (AFTBlend < 0) or (TMI.BlendPrev = 0) and (TMI.BlendCur = cMaxFTBlend - 1) then
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
      for i := 0 to FTilePaletteSize - 1 do
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
          DoCmd(gtTileSet, FTilePaletteSize);
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

          WriteLn('KF: ', FKeyFrames[kf].StartFrame, #9'FCnt: ', KFCount, #9'Written: ', KFSize, #9'Bitrate: ', FormatFloat('0.00', KFSize / 1024.0 * 8.0 / KFCount) + ' kbpf  '#9'(' + FormatFloat('0.00', KFSize / 1024.0 * 8.0 / KFCount * FFramesPerSecond)+' kbps)');
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

procedure TMainForm.SaveRawTiles(OutFN: String);
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
      fs.WriteByte(FTilePaletteSize);
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

function TMainForm.DoExternalFFMpeg(AFN: String; var AVidPath: String; AStartFrame, AFrameCount: Integer; AScale: Double; out AFPS: Double): String;
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
  Process.Parameters.Add('-y -i "' + AFN + '" ' + vfl + ' -compression_level 0 -pix_fmt rgb24 "' + Result + '%04d.png' + '"');
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

procedure TMainForm.FormCreate(Sender: TObject);
var
  col, i, sr, luma: Integer;
  es: TEncoderStep;
begin
  FormatSettings.DecimalSeparator := '.';
  InitializeCriticalSection(FCS);
  SpinLeave(@FLock);

{$ifdef DEBUG}
  //ProcThreadPool.MaxThreadCount := 1;
{$else}
  SetPriorityClass(GetCurrentProcess(), IDLE_PRIORITY_CLASS);
{$endif}

  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  pcPages.ActivePage := tsSettings;
  ReframeUI(80, 45);
  FFramesPerSecond := 24.0;

  cbxYilMixChange(nil);
  chkUseTKChange(nil);

  for es := Succ(Low(TEncoderStep)) to High(TEncoderStep) do
  begin
    cbxStartStep.AddItem(Copy(GetEnumName(TypeInfo(TEncoderStep), Ord(es)), 3), TObject(PtrInt(Ord(es))));
    cbxEndStep.AddItem(Copy(GetEnumName(TypeInfo(TEncoderStep), Ord(es)), 3), TObject(PtrInt(Ord(es))));
  end;
  cbxStartStep.ItemIndex := Ord(Succ(Low(TEncoderStep)));
  cbxEndStep.ItemIndex := Ord(High(TEncoderStep));

  sr := (1 shl cRGBBitsPerComp) - 1;

  for i := 0 to cRGBColors - 1 do
  begin
    col :=
       ((((i shr (cRGBBitsPerComp * 0)) and sr) * 255 div sr) and $ff) or //R
      (((((i shr (cRGBBitsPerComp * 1)) and sr) * 255 div sr) and $ff) shl 8) or //G
      (((((i shr (cRGBBitsPerComp * 2)) and sr) * 255 div sr) and $ff) shl 16);  //B

    FromRGB(col, FColorMap[i, 0], FColorMap[i, 1], FColorMap[i, 2]);
    RGBToHSV(col, FColorMap[i, 3], FColorMap[i, 4], FColorMap[i, 5]);
    luma := (FColorMap[i, 0] * cRedMul + FColorMap[i, 1] * cGreenMul + FColorMap[i, 2] * cBlueMul) div cLumaDiv;
    Assert(InRange(luma, 0, 255));
    FColorMap[i, 6] := luma;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DeleteCriticalSection(FCS);

  ClearAll;
end;

end.

