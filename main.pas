unit main;

{$mode objfpc}{$H+}

{$define ASM_DBMP}

interface

uses
  LazLogger, Classes, SysUtils, windows, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, typinfo,
  StdCtrls, ComCtrls, Spin, Menus, Math, types, Process, strutils, kmodes, MTProcs, extern,
  xalglib, IntfGraphics, FPimage, FPWritePNG, zstream;

type
  TEncoderStep = (esNone = -1, esLoad = 0, esDither, esMakeUnique, esGlobalTiling, esFrameTiling, esReindex, esSmooth, esSave);

const
  cPhi = (1 + sqrt(5)) / 2;
  cInvPhi = 1 / cPhi;

  // Tweakable params
  cRandomKModesCount = 7;
  cKeyframeFixedColors = 4;
  cGamma: array[0..1{YUV,LAB}] of TFloat = (2.0, 1.0);
  cGammaCorrectSmoothing = -1;
  cKFFromPal = True;
  cKFGamma = 0;
  cKFQWeighting = True;

  cRedMultiplier = 299;
  cGreenMultiplier = 587;
  cBlueMultiplier = 114;
  cLumaMultiplier = cRedMultiplier + cGreenMultiplier + cBlueMultiplier;

  // SMS consts

{$if true}
  cTileMapWidth = 160;
  cTileMapHeight = 66;
  cPaletteCount = 16;
  cBitsPerComp = 8;
{$else}
  cTileMapWidth = 32;
  cTileMapHeight = 24;
  cPaletteCount = 8;
  cBitsPerComp = 4;
{$endif}

  cPreDitherMixedColors = 4;
  cVecInvWidth = 16;
  cTotalColors = 1 shl (cBitsPerComp * 3);
  cTileWidth = 8;
  cTilePaletteSize = 32;
  cTileMapSize = cTileMapWidth * cTileMapHeight;
  cScreenWidth = cTileMapWidth * cTileWidth;
  cScreenHeight = cTileMapHeight * cTileWidth;
  cBankSize = 16384;
  cTileSize = cTileWidth * cTileWidth div 2;
  cTilesPerBank = cBankSize div cTileSize;
  cZ80Clock = 3546893;
  cLineCount = 313;
  cRefreshRate = 50;
  cCyclesPerLine = cZ80Clock / (cLineCount * cRefreshRate);
  cCyclesPerDisplayPhase : array[Boolean{VBlank?}] of Integer = (
    Round(cScreenHeight * cCyclesPerLine),
    Round((cLineCount - cScreenHeight) * cCyclesPerLine)
  );

  // Video player consts
  cMaxTilesPerFrame = 207;
  cSmoothingPrevFrame = 1;
  cTileIndexesTileOffset = cTilesPerBank + 1;
  cTileIndexesMaxDiff = 223;
  cTileIndexesRepeatStart = 224;
  cTileIndexesMaxRepeat = 30;
  cTileIndexesDirectValue = 223;
  cTileIndexesTerminator = 254;
  cTileIndexesVBlankSwitch = 255;
  cTileMapIndicesOffset : array[0..1] of Integer = (49, 256 + 49);
  cTileMapCacheBits = 5;
  cTileMapCacheSize = 1 shl cTileMapCacheBits;
  cTileMapMaxRepeat : array[Boolean{Raw?}] of Integer = (6, 4);
  cTileMapMaxSkip = 31;
  cTileMapCommandCache : array[1..6{Rpt}] of Byte = ($01, $40, $41, $80, $81, $c0);
  cTileMapCommandSkip = $00;
  cTileMapCommandRaw : array[1..4{Rpt}] of Byte = ($c1, $d1, $e1, $f1);
  cTileMapTerminator = $00; // skip 0
  cClocksPerSample = 344;
  cFrameSoundSize = cZ80Clock / cClocksPerSample / 2 / (cRefreshRate / 4);

  // number of Z80 cycles to execute a function
  cTileIndexesTimings : array[Boolean{VBlank?}, 0..4 {Direct/Std/RptFix/RptVar/VBlankSwitch}] of Integer = (
    (343 + 688, 289 + 17 + 688, 91 + 5, 213 + 12 + 688, 113 + 7),
    (347 + 344, 309 + 18 + 344, 91 + 5, 233 + 14 + 344, 113 + 7)
  );
  cLineJitterCompensation = 4;
  cTileIndexesInitialLine = 201; // algo starts in VBlank

  cUV = 7;
  cDCTQuantization: array[0..2{YUV}, 0..7, 0..7] of TFloat = (
    (
      (16, 16, 16, 16, 16, 16, 16, 16),
      (16, 16, 16, 16, 16, 16, 16, 16),
      (16, 16, 16, 16, 16, 16, 16, 16),
      (16, 16, 16, 16, 16, 16, 16, 16),
      (16, 16, 16, 16, 16, 16, 16, 16),
      (16, 16, 16, 16, 16, 16, 16, 16),
      (16, 16, 16, 16, 16, 16, 16, 16),
      (16, 16, 16, 16, 16, 16, 16, 16)
    ),
    (
      (cUV, cUV, cUV, cUV, cUV, cUV, cUV, cUV),
      (cUV, cUV, cUV, cUV, cUV, cUV, cUV, cUV),
      (cUV, cUV, cUV, cUV, cUV, cUV, cUV, cUV),
      (cUV, cUV, cUV, cUV, cUV, cUV, cUV, cUV),
      (cUV, cUV, cUV, cUV, cUV, cUV, cUV, cUV),
      (cUV, cUV, cUV, cUV, cUV, cUV, cUV, cUV),
      (cUV, cUV, cUV, cUV, cUV, cUV, cUV, cUV),
      (cUV, cUV, cUV, cUV, cUV, cUV, cUV, cUV)
    ),
    (
      (cUV, cUV, cUV, cUV, cUV, cUV, cUV, cUV),
      (cUV, cUV, cUV, cUV, cUV, cUV, cUV, cUV),
      (cUV, cUV, cUV, cUV, cUV, cUV, cUV, cUV),
      (cUV, cUV, cUV, cUV, cUV, cUV, cUV, cUV),
      (cUV, cUV, cUV, cUV, cUV, cUV, cUV, cUV),
      (cUV, cUV, cUV, cUV, cUV, cUV, cUV, cUV),
      (cUV, cUV, cUV, cUV, cUV, cUV, cUV, cUV),
      (cUV, cUV, cUV, cUV, cUV, cUV, cUV, cUV)
    )
  );

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

  cEncoderStepLen: array[TEncoderStep] of Integer = (0, 2, 2, 1, 3, 2, 2, 1, 1);

  cPalettePattern : array[0 .. 15, 0 .. cTilePaletteSize - 1] of TFloat = (
    (0.000000, 0.000176, 0.000311, 0.000525, 0.000776, 0.001106, 0.001521, 0.002051, 0.002724, 0.003581, 0.004671, 0.006057, 0.007820, 0.010062, 0.012915, 0.016543, 0.021159, 0.027030, 0.034499, 0.043998, 0.056082, 0.071453, 0.091006, 0.115876, 0.147513, 0.187754, 0.238943, 0.304056, 0.386880, 0.492235, 0.626248, 0.796715),
    (0.000088, 0.000243, 0.000418, 0.000651, 0.000941, 0.001314, 0.001786, 0.002388, 0.003153, 0.004126, 0.005364, 0.006938, 0.008941, 0.011489, 0.014729, 0.018851, 0.024095, 0.030765, 0.039249, 0.050040, 0.063768, 0.081230, 0.103441, 0.131695, 0.167634, 0.213349, 0.271499, 0.345468, 0.439557, 0.559241, 0.711481, 0.905134),
    (0.000044, 0.000210, 0.000364, 0.000588, 0.000859, 0.001210, 0.001654, 0.002219, 0.002939, 0.003853, 0.005017, 0.006497, 0.008380, 0.010775, 0.013822, 0.017697, 0.022627, 0.028897, 0.036874, 0.047019, 0.059925, 0.076341, 0.097223, 0.123786, 0.157573, 0.200552, 0.255221, 0.324762, 0.413219, 0.525738, 0.668864, 0.850924),
    (0.000132, 0.000277, 0.000471, 0.000713, 0.001024, 0.001417, 0.001919, 0.002556, 0.003367, 0.004398, 0.005710, 0.007379, 0.009502, 0.012202, 0.015636, 0.020005, 0.025563, 0.032632, 0.041624, 0.053061, 0.067611, 0.086118, 0.109659, 0.139604, 0.177694, 0.226146, 0.287777, 0.366174, 0.465896, 0.592744, 0.754098, 0.959343),
    (0.000022, 0.000193, 0.000338, 0.000556, 0.000818, 0.001158, 0.001587, 0.002135, 0.002831, 0.003717, 0.004844, 0.006277, 0.008100, 0.010419, 0.013368, 0.017120, 0.021893, 0.027964, 0.035686, 0.045509, 0.058004, 0.073897, 0.094114, 0.119831, 0.152543, 0.194153, 0.247082, 0.314409, 0.400049, 0.508986, 0.647556, 0.823819),
    (0.000066, 0.000226, 0.000391, 0.000619, 0.000900, 0.001262, 0.001720, 0.002304, 0.003046, 0.003990, 0.005190, 0.006718, 0.008661, 0.011132, 0.014276, 0.018274, 0.023361, 0.029831, 0.038061, 0.048530, 0.061847, 0.078785, 0.100332, 0.127740, 0.162603, 0.206950, 0.263360, 0.335115, 0.426388, 0.542489, 0.690173, 0.878029),
    (0.000119, 0.000267, 0.000455, 0.000695, 0.000999, 0.001386, 0.001879, 0.002506, 0.003303, 0.004317, 0.005606, 0.007247, 0.009333, 0.011988, 0.015364, 0.019659, 0.025122, 0.032071, 0.040911, 0.052155, 0.066458, 0.084651, 0.107793, 0.137231, 0.174676, 0.222307, 0.282894, 0.359962, 0.457994, 0.582693, 0.741313, 0.943080),
    (0.000154, 0.000294, 0.000498, 0.000745, 0.001065, 0.001469, 0.001985, 0.002640, 0.003474, 0.004534, 0.005883, 0.007599, 0.009782, 0.012558, 0.016090, 0.020582, 0.026296, 0.033565, 0.042811, 0.054572, 0.069532, 0.088562, 0.112768, 0.143558, 0.182724, 0.232544, 0.295916, 0.376527, 0.479065, 0.609496, 0.775406, 0.986448),
    (0.000011, 0.000184, 0.000324, 0.000541, 0.000797, 0.001132, 0.001554, 0.002093, 0.002778, 0.003649, 0.004757, 0.006167, 0.007960, 0.010241, 0.013142, 0.016832, 0.021526, 0.027497, 0.035092, 0.044754, 0.057043, 0.072675, 0.092560, 0.117854, 0.150028, 0.190954, 0.243012, 0.309232, 0.393465, 0.500610, 0.636902, 0.810267),
    (0.000033, 0.000201, 0.000351, 0.000572, 0.000838, 0.001184, 0.001621, 0.002177, 0.002885, 0.003785, 0.004930, 0.006387, 0.008240, 0.010597, 0.013595, 0.017409, 0.022260, 0.028431, 0.036280, 0.046264, 0.058964, 0.075119, 0.095669, 0.121808, 0.155058, 0.197352, 0.251152, 0.319585, 0.406634, 0.517362, 0.658210, 0.837372),
    (0.000055, 0.000218, 0.000378, 0.000603, 0.000879, 0.001236, 0.001687, 0.002262, 0.002992, 0.003922, 0.005104, 0.006608, 0.008520, 0.010954, 0.014049, 0.017986, 0.022994, 0.029364, 0.037467, 0.047775, 0.060886, 0.077563, 0.098778, 0.125763, 0.160088, 0.203751, 0.259291, 0.329938, 0.419803, 0.534114, 0.679519, 0.864477),
    (0.000077, 0.000235, 0.000404, 0.000635, 0.000921, 0.001288, 0.001753, 0.002346, 0.003099, 0.004058, 0.005277, 0.006828, 0.008801, 0.011310, 0.014502, 0.018563, 0.023728, 0.030298, 0.038655, 0.049285, 0.062807, 0.080008, 0.101887, 0.129717, 0.165118, 0.210149, 0.267430, 0.340291, 0.432973, 0.550865, 0.700827, 0.891581),
    (0.000099, 0.000252, 0.000431, 0.000666, 0.000962, 0.001340, 0.001819, 0.002430, 0.003206, 0.004194, 0.005450, 0.007048, 0.009081, 0.011667, 0.014956, 0.019140, 0.024462, 0.031231, 0.039842, 0.050796, 0.064729, 0.082452, 0.104995, 0.133672, 0.170149, 0.216548, 0.275569, 0.350644, 0.446142, 0.567617, 0.722135, 0.918686),
    (0.000112, 0.000262, 0.000447, 0.000685, 0.000987, 0.001371, 0.001859, 0.002480, 0.003271, 0.004276, 0.005554, 0.007181, 0.009249, 0.011881, 0.015228, 0.019486, 0.024902, 0.031791, 0.040555, 0.051702, 0.065881, 0.083918, 0.106861, 0.136045, 0.173167, 0.220387, 0.280452, 0.356856, 0.454044, 0.577668, 0.734920, 0.934949),
    (0.000143, 0.000286, 0.000485, 0.000729, 0.001044, 0.001443, 0.001952, 0.002598, 0.003420, 0.004466, 0.005797, 0.007489, 0.009642, 0.012380, 0.015863, 0.020294, 0.025930, 0.033098, 0.042217, 0.053817, 0.068571, 0.087340, 0.111213, 0.141581, 0.180209, 0.229345, 0.291847, 0.371351, 0.472481, 0.601120, 0.764752, 0.972895),
    (0.000165, 0.000302, 0.000511, 0.000761, 0.001086, 0.001495, 0.002018, 0.002682, 0.003528, 0.004603, 0.005970, 0.007709, 0.009922, 0.012737, 0.016317, 0.020871, 0.026663, 0.034032, 0.043405, 0.055327, 0.070493, 0.089784, 0.114322, 0.145535, 0.185239, 0.235744, 0.299986, 0.381704, 0.485650, 0.617872, 0.786061, 1.000000)
  );

type
  TFloatFloatFunction = function(x: TFloat; Data: Pointer): TFloat of object;

  PTile = ^TTile;
  PPTile = ^PTile;

  TPalPixels = array[0..(cTileWidth - 1),0..(cTileWidth - 1)] of Byte;
  PPalPixels = ^TPalPixels;

  TTile = record
    RGBPixels: array[0..(cTileWidth - 1),0..(cTileWidth - 1)] of Integer;
    PalPixels: TPalPixels;

    PaletteIndexes: TIntegerDynArray;
    PaletteRGB: TIntegerDynArray;

    Active: Boolean;
    UseCount, AveragedCount, TmpIndex: Integer;
  end;

  PTileMapItem = ^TTileMapItem;

  TTileMapItem = record
    GlobalTileIndex, TmpIndex: Integer;
    HMirror,VMirror,Smoothed: Boolean;
    PalIdx: Integer;
  end;

  TTileMapItems = array of TTileMapItem;

  PKeyFrame = ^TKeyFrame;

  TFrame = record
    Index: Integer;
    Tiles: array[0..(cTileMapSize - 1)] of TTile;
    TileMap: array[0..(cTileMapHeight - 1),0..(cTileMapWidth - 1)] of TTileMapItem;
    KeyFrame: PKeyFrame;
  end;

  PFrame = ^TFrame;

  TTileDataset = record
    Tags: TIVector;
    Dataset: TFloatDynArray2;
    FrameDataset: array of TFloatDynArray;
    TRToTileIdx: TIntegerDynArray;
    KDT: Tkdtree;
  end;

  PTileDataset = ^TTileDataset;

  TMixingPlan = record
    // static
    LumaPal: array of Integer;
    Y2Palette: array of array[0..3] of Integer;
    Y2MixedColors: Integer;
    // dynamic
    CacheCS: TRTLCriticalSection;
    ListCache: TByteDynArray2;
    CountCache: TByteDynArray;
  end;

  TKeyFrame = record
    PaletteIndexes: array[0 .. cPaletteCount - 1] of TIntegerDynArray;
    PaletteRGB: array[0 .. cPaletteCount - 1] of TIntegerDynArray;
    StartFrame, EndFrame, FrameCount: Integer;
    TileDS: PTileDataset;
    MixingPlans: array[0 .. cPaletteCount - 1] of TMixingPlan;
    FrameDitheringLeft: Integer;
  end;

  { T8BitPortableNetworkGraphic }

  T8BitPortableNetworkGraphic = class(TPortableNetworkGraphic)
    procedure InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter); override;
  end;

  { TFastPortableNetworkGraphic }

  TFastPortableNetworkGraphic = class(TPortableNetworkGraphic)
    procedure InitializeWriter(AImage: TLazIntfImage; AWriter: TFPCustomImageWriter); override;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    btnRunAll: TButton;
    cbxStep: TComboBox;
    cbxYilMix: TComboBox;
    chkGamma: TCheckBox;
    chkLAB: TCheckBox;
    chkReduced: TCheckBox;
    chkMirrored: TCheckBox;
    chkDithered: TCheckBox;
    chkPlay: TCheckBox;
    edInput: TEdit;
    edOutputDir: TEdit;
    edWAV: TEdit;
    imgPalette: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    lblCorrel: TLabel;
    lblPct: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    lblTileCount: TLabel;
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
    pbProgress: TProgressBar;
    seAvgTPF: TSpinEdit;
    seColReach: TFloatSpinEdit;
    sedPalIdx: TSpinEdit;
    seMaxTiles: TSpinEdit;
    seTempoSmoo: TSpinEdit;
    sePage: TSpinEdit;
    IdleTimer: TIdleTimer;
    imgTiles: TImage;
    imgSource: TImage;
    imgDest: TImage;
    seFrameCount: TSpinEdit;
    tbFrame: TTrackBar;

    procedure btnLoadClick(Sender: TObject);
    procedure btnDitherClick(Sender: TObject);
    procedure btnDoMakeUniqueClick(Sender: TObject);
    procedure btnDoGlobalTilingClick(Sender: TObject);
    procedure btnDoKeyFrameTilingClick(Sender: TObject);
    procedure btnReindexClick(Sender: TObject);
    procedure btnSmoothClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);

    procedure btnRunAllClick(Sender: TObject);
    procedure btnDebugClick(Sender: TObject);
    procedure cbxYilMixChange(Sender: TObject);
    procedure chkLABChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure IdleTimerTimer(Sender: TObject);
    procedure seAvgTPFEditingDone(Sender: TObject);
    procedure seMaxTilesEditingDone(Sender: TObject);
    procedure tbFrameChange(Sender: TObject);
  private
    FKeyFrames: array of TKeyFrame;
    FFrames: array of TFrame;
    FColorMap: array[0..cTotalColors - 1] of Integer;
    FTiles: array of PTile;
    FUseLAB: Boolean;
    FY2MixedColors: Integer;
    FProgressStep: TEncoderStep;
    FProgressPosition, FOldProgressPosition, FProgressStartTime, FProgressPrevTime: Integer;

    FCS: TRTLCriticalSection;

    function ComputeCorrelation(a: TIntegerDynArray; b: TIntegerDynArray): TFloat;

    procedure LoadFrame(AFrame: PFrame; ABitmap: TBitmap);
    procedure ProgressRedraw(CurFrameIdx: Integer = -1; ProgressStep: TEncoderStep = esNone);
    procedure Render(AFrameIndex: Integer; playing, dithered, mirrored, reduced, gamma: Boolean; palIdx: Integer;
      ATilePage: Integer);

    procedure RGBToHSL(RGBCol : Integer; var h, s, l: integer);
    function HSLToRGB(H, S, L: integer): Integer;
    procedure RGBToYUV(r, g, b: Integer; GammaCor: Integer; out y, u, v: TFloat); overload;
    procedure RGBToYUV(fr, fg, fb: TFloat; out y, u, v: TFloat); overload;
    procedure RGBToLAB(ir, ig, ib: Integer; out ol, oa, ob: TFloat); overload;
    procedure RGBToLAB(r, g, b: TFloat; out ol, oa, ob: TFloat); overload;
    procedure RGBToColSpc(r, g, b: TFloat; LAB: Boolean; out ol, oa, ob: TFloat); overload;
    procedure RGBToColSpc(col: Integer; LAB: Boolean; GammaCor: Integer; out ol, oa, ob: TFloat); overload;

    procedure ComputeTileDCT(const ATile: TTile; FromPal, QWeighting, LAB, HMirror, VMirror: Boolean; GammaCor: Integer; const pal: array of Integer;
      var DCT: TFloatDynArray);

    // Dithering algorithms ported from http://bisqwit.iki.fi/story/howto/dither/jy/

    function ColorCompare(r1, g1, b1, r2, g2, b2: Integer): Int64;
    procedure PreparePlan(var Plan: TMixingPlan; MixedColors: Integer; const pal: array of Integer);
    procedure TerminatePlan(var Plan: TMixingPlan);
    function DeviseBestMixingPlan(var Plan: TMixingPlan; col: Integer; List: TByteDynArray): Integer;

    procedure LoadTiles;
    function GetGlobalTileCount: Integer;
    function GetFrameTileCount(AFrame: PFrame): Integer;
    procedure CopyTile(const Src: TTile; var Dest: TTile);

    procedure DitherTile(var ATile: TTile; var Plan: TMixingPlan);
    procedure FindBestKeyframePalette(AKeyFrame: PKeyFrame; ColorReach: TFloat);
    procedure FinalDitherTiles(AFrame: PFrame);

    function GetTileZoneMedian(const ATile: TTile; x, y, w, h: Integer): Integer;
    function GetTileGridMedian(const ATile: TTile; other: Boolean): Integer;
    procedure MakeTilesUnique(FirstTileIndex, TileCount: Integer);
    procedure MergeTiles(const TileIndexes: array of Integer; TileCount: Integer; BestIdx: Integer; NewTile: PPalPixels);
    function WriteTileDatasetLine(const ATile: TTile; var DataLine: TByteDynArray; out PxlAccum: Integer): Integer;
    procedure DoGlobalTiling(DesiredNbTiles, RestartCount: Integer);

    function GoldenRatioSearch(Func: TFloatFloatFunction; Mini, Maxi: TFloat; ObjectiveY: TFloat = 0.0; Epsilon: TFloat = 1e-12; Data: Pointer = nil): TFloat;
    procedure HMirrorPalTile(var ATile: TTile);
    procedure VMirrorPalTile(var ATile: TTile);
    function GetMaxTPF(AKF: PKeyFrame): Integer;
    procedure DoKeyFrameTiling(AKF: PKeyFrame);
    procedure DoFrameTiling(AFrame: PFrame);

    function GetTileUseCount(ATileIndex: Integer): Integer;
    procedure ReindexTiles;
    procedure DoTemporalSmoothing(AFrame, APrevFrame: PFrame; Y: Integer; Strength: TFloat);
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

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

function SwapRB(c: Integer): Integer; inline;
begin
  Result := ((c and $ff) shl 16) or ((c shr 16) and $ff) or (c and $ff00ff00);
end;

function ToRGB(r, g, b: Byte): Integer; inline;
begin
  Result := (b shl 16) or (g shl 8) or r;
end;

procedure FromRGB(col: Integer; out r, g, b: Integer); inline;
begin
  r := col and $ff;
  g := (col shr 8) and $ff;
  b := (col shr 16) and $ff;
end;

var
  gGammaCorLut: array[0..High(cGamma), 0..High(Byte)] of TFloat;
  gVecInv: array[0..256 * 4 - 1] of Cardinal;
  gDCTLut:array[0..cTileWidth - 1, 0..cTileWidth - 1,0..cTileWidth - 1,0..cTileWidth - 1] of TFloat;

procedure InitLuts;
var
  g, i, v, u, y, x: Integer;
begin
  for g := 0 to High(cGamma) do
    for i := 0 to High(Byte) do
      gGammaCorLut[g, i] := power(i / 255.0, cGamma[g]);

  for i := 0 to High(gVecInv) do
    gVecInv[i] := iDiv0(1 shl cVecInvWidth, i shr 2);

  for v := 0 to (cTileWidth - 1) do
    for u := 0 to (cTileWidth - 1) do
      for y := 0 to (cTileWidth - 1) do
        for x := 0 to (cTileWidth - 1) do
		      gDCTLut[v, u, y, x] := cos((x + 0.5) * u * PI / 16.0) * cos((y + 0.5) * v * PI / 16.0);
end;

function GammaCorrect(lut: Integer; x: Byte): TFloat; inline;
begin
  Result := gGammaCorLut[lut, x];
end;

function lerp(x, y, alpha: TFloat): TFloat; inline;
begin
  Result := x + (y - x) * alpha;
end;

function CompareEuclidean192Ptr(pa, pb: PFloat): TFloat;
var
  i: Integer;
begin
  Result := 0;
  for i := 192 div 8 - 1 downto 0 do
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

function CompareManhattan192Ptr(pa, pb: PFloat): TFloat;
var
  i: Integer;
begin
  Result := 0;
  for i := 192 div 8 - 1 downto 0 do
  begin
    Result += abs(pa^ - pb^); Inc(pa); Inc(pb);
    Result += abs(pa^ - pb^); Inc(pa); Inc(pb);
    Result += abs(pa^ - pb^); Inc(pa); Inc(pb);
    Result += abs(pa^ - pb^); Inc(pa); Inc(pb);
    Result += abs(pa^ - pb^); Inc(pa); Inc(pb);
    Result += abs(pa^ - pb^); Inc(pa); Inc(pb);
    Result += abs(pa^ - pb^); Inc(pa); Inc(pb);
    Result += abs(pa^ - pb^); Inc(pa); Inc(pb);
  end;
end;

function CompareEuclidean192(const a, b: TFloatDynArray): TFloat; inline;
begin
  Result := CompareEuclidean192Ptr(@a[0], @b[0]);
end;

function CompareManhattan192(const a, b: TFloatDynArray): TFloat; inline;
begin
  Result := CompareManhattan192Ptr(@a[0], @b[0]);
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

function TMainForm.ComputeCorrelation(a: TIntegerDynArray; b: TIntegerDynArray): TFloat;
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
    ya[i] := fr; ya[i + Length(a)] := fg; ya[i + Length(a) * 2] := fb;

    fr := (b[i] shr 16) and $ff; fg := (b[i] shr 8) and $ff; fb := b[i] and $ff;
    yb[i] := fr; yb[i + Length(a)] := fg; yb[i + Length(a) * 2] := fb;
  end;

  Result := PearsonCorrelation(ya, yb, Length(a) * 3);
end;

{ TMainForm }

procedure TMainForm.btnDoGlobalTilingClick(Sender: TObject);
var
  dnt: Integer;
begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(-1, esGlobalTiling);

  dnt := seMaxTiles.Value;
  dnt -= Ord(odd(dnt));

  DoGlobalTiling(dnt, cRandomKModesCount);

  tbFrameChange(nil);
end;

procedure TMainForm.btnDitherClick(Sender: TObject);

  procedure DoFindBest(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    FindBestKeyframePalette(@FKeyFrames[AIndex], seColReach.Value);
  end;

  procedure DoFinal(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    FinalDitherTiles(@FFrames[AIndex]);
  end;

begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(-1, esDither);
  ProcThreadPool.DoParallelLocalProc(@DoFindBest, 0, High(FKeyFrames));
  ProgressRedraw(1);
  ProcThreadPool.DoParallelLocalProc(@DoFinal, 0, High(FFrames));
  ProgressRedraw(2);

  tbFrameChange(nil);
end;

procedure TMainForm.btnDoMakeUniqueClick(Sender: TObject);
const
  cTilesAtATime = 100 * cTileMapSize;

  procedure DoMakeUnique(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    MakeTilesUnique(AIndex * cTilesAtATime, Min(Length(FTiles) - AIndex * cTilesAtATime, cTilesAtATime));
  end;

begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(-1, esMakeUnique);

  ProcThreadPool.DoParallelLocalProc(@DoMakeUnique, 0, High(FTiles) div cTilesAtATime);

  ProgressRedraw(1);

  tbFrameChange(nil);
end;

function CompareFrames(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PInteger(Item2)^, PInteger(Item1)^);
end;

procedure TMainForm.btnDoKeyFrameTilingClick(Sender: TObject);

  procedure DoFrm(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    DoFrameTiling(@FFrames[AIndex]);
  end;

  procedure DoKF(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    DoKeyFrameTiling(@FKeyFrames[AIndex]);
  end;

var
  i: Integer;
begin
  if Length(FKeyFrames) = 0 then
    Exit;

  ProgressRedraw(-1, esFrameTiling);
  ProcThreadPool.DoParallelLocalProc(@DoKF, 0, High(FKeyFrames));
  ProgressRedraw(1);
  ProcThreadPool.DoParallelLocalProc(@DoFrm, 0, High(FFrames));
  ProgressRedraw(2);

  for i := 0 to High(FKeyFrames) do
  begin
    FreeAndNil(FKeyFrames[i].TileDS^.KDT);
    FreeMemAndNil(FKeyFrames[i].TileDS);
  end;

  tbFrameChange(nil);
end;

procedure TMainForm.chkLABChange(Sender: TObject);
begin
  FUseLAB := chkLAB.Checked;
end;

procedure TMainForm.btnLoadClick(Sender: TObject);
var
  inPath: String;

  procedure DoLoadFrame(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    bmp: TPicture;
  begin
    bmp := TPicture.Create;
    try
      EnterCriticalSection(FCS);
      bmp.Bitmap.PixelFormat:=pf32bit;
      bmp.LoadFromFile(Format(inPath, [AIndex]));
      LeaveCriticalSection(FCS);

      LoadFrame(@FFrames[AIndex], bmp.Bitmap);

      FFrames[AIndex].Index := AIndex;
    finally
      bmp.Free;
    end;
  end;

var
  i, j: Integer;
  fn: String;
  kfCnt, frc: Integer;
  isKf: Boolean;
  kfSL: TStringList;
  sfr, efr: Integer;
begin
  ProgressRedraw;

  for i := 0 to High(FTiles) do
    Dispose(FTiles[i]);

  SetLength(FFrames, 0);
  SetLength(FKeyFrames, 0);
  SetLength(FTiles, 0);

  ProgressRedraw(-1, esLoad);

  inPath := edInput.Text;
  frc := seFrameCount.Value;

  if frc <= 0 then
  begin
    if Pos('%', inPath) > 0 then
    begin
      i := 0;
      repeat
        fn := Format(inPath, [i]);
        Inc(i);
      until not FileExists(fn);

      frc := i - 1;
    end
    else
    begin
      frc := 1;
    end;

    seFrameCount.Value := frc;
    seFrameCount.Repaint;
  end;

  SetLength(FFrames, frc);
  tbFrame.Max := High(FFrames);

  for i := 0 to High(FFrames) do
  begin
    fn := Format(inPath, [i]);
    if not FileExists(fn) then
    begin
      SetLength(FFrames, 0);
      tbFrame.Max := 0;
      raise EFileNotFoundException.Create('File not found: ' + fn);
    end;
  end;

  ProcThreadPool.DoParallelLocalProc(@DoLoadFrame, 0, High(FFrames));

  kfSL := TStringList.Create;
  try
    fn := ChangeFileExt(Format(inPath, [0]), '.kf');
    if FileExists(fn) then
      kfSL.LoadFromFile(fn);

    kfCnt := 0;
    for i := 0 to High(FFrames) do
    begin
      fn := ChangeFileExt(Format(inPath, [i]), '.kf');
      isKf := FileExists(fn) or (i = 0) or (i < kfSL.Count) and (Pos('I', kfSL[i]) <> 0);
      if isKf then
      begin
        WriteLn('KF: ', kfCnt, #9'Frame: ', i);
        Inc(kfCnt);
      end;
    end;

    SetLength(FKeyFrames, kfCnt);
    kfCnt := -1;
    for i := 0 to High(FFrames) do
    begin
      fn := ChangeFileExt(Format(inPath, [i]), '.kf');
      isKf := FileExists(fn) or (i = 0) or (i < kfSL.Count) and (Pos('I', kfSL[i]) <> 0);
      if isKf then
        Inc(kfCnt);
      FFrames[i].KeyFrame := @FKeyFrames[kfCnt];
    end;
  finally
    kfSL.Free;
  end;

  for j := 0 to High(FKeyFrames) do
  begin
    sfr := High(Integer);
    efr := Low(Integer);

    for i := 0 to High(FFrames) do
      if FFrames[i].KeyFrame = @FKeyFrames[j] then
      begin
        sfr := Min(sfr, i);
        efr := Max(efr, i);
      end;

    FKeyFrames[j].StartFrame := sfr;
    FKeyFrames[j].EndFrame := efr;
    FKeyFrames[j].FrameCount := efr - sfr + 1;
    FKeyFrames[j].FrameDitheringLeft := -1;
  end;

  ProgressRedraw(1);

  LoadTiles;

  ProgressRedraw(2);

  tbFrameChange(nil);
end;

procedure TMainForm.btnReindexClick(Sender: TObject);

  procedure DoPruneUnusedTiles(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    if FTiles[AIndex]^.Active then
    begin
      FTiles[AIndex]^.UseCount := GetTileUseCount(AIndex);
      FTiles[AIndex]^.Active := FTiles[AIndex]^.UseCount <> 0;
    end
    else
    begin
      FTiles[AIndex]^.UseCount := 0;
    end;
  end;

begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(-1, esReindex);

  ProcThreadPool.DoParallelLocalProc(@DoPruneUnusedTiles, 0, High(FTiles));
  ProgressRedraw(1);
  ReindexTiles;
  ProgressRedraw(2);

  tbFrameChange(nil);
end;

procedure TMainForm.btnRunAllClick(Sender: TObject);
var
  lastStep: TEncoderStep;
begin
  lastStep := TEncoderStep(cbxStep.ItemIndex);

  if lastStep >= esLoad then
    btnLoadClick(nil);

  if lastStep >= esDither then
    btnDitherClick(nil);

  if lastStep >= esMakeUnique then
    btnDoMakeUniqueClick(nil);

  if lastStep >= esGlobalTiling then
    btnDoGlobalTilingClick(nil);

  if lastStep >= esFrameTiling then
    btnDoKeyFrameTilingClick(nil);

  if lastStep >= esReindex then
    btnReindexClick(nil);

  if lastStep >= esSmooth then
    btnSmoothClick(nil);

  if lastStep >= esSave then
    btnSaveClick(nil);

  ProgressRedraw;
  tbFrameChange(nil);
end;

procedure TMainForm.btnDebugClick(Sender: TObject);
var
  i: Integer;
  seed: Cardinal;
  pal: array[0.. 5] of Integer;
  list: TByteDynArray;
  plan: TMixingPlan;
begin
  seed := 42;
  for i := 0 to 15 do
    pal[i] := RandInt(1 shl 24, seed);
  PreparePlan(plan, 4, pal);

  SetLength(list, cDitheringListLen);
  DeviseBestMixingPlan(plan, $ffffff, list);
  DeviseBestMixingPlan(plan, $ff8000, list);
  DeviseBestMixingPlan(plan, $808080, list);
  DeviseBestMixingPlan(plan, $000000, list);
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
var
  i: Integer;
  palPict: TPortableNetworkGraphic;
begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(-1, esSave);

{$if cTilePaletteSize * cPaletteCount <= 256}
  if chkDithered.Checked and Assigned(FKeyFrames[0].PaletteRGB[0]) then
    palPict := T8BitPortableNetworkGraphic.Create
  else
{$endif}
     palPict := TFastPortableNetworkGraphic.Create;

  palPict.Width := cScreenWidth;
  palPict.Height := cScreenHeight;
  //palPict.PixelFormat := pf24bit;

  try
    for i := 0 to High(FFrames) do
    begin
      Render(i, True, chkDithered.Checked, chkMirrored.Checked, chkReduced.Checked,  chkGamma.Checked, sedPalIdx.Value, sePage.Value);
      imgDest.Repaint;

      palPict.Canvas.Draw(0, 0, imgDest.Picture.Bitmap);
      palPict.SaveToFile(Format(edOutputDir.Text, [i]));
    end;
  finally
    palPict.Free;
  end;

  ProgressRedraw(1);

  tbFrameChange(nil);
end;

procedure TMainForm.btnSmoothClick(Sender: TObject);
  procedure DoSmoothing(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var i: Integer;
  begin
    for i := cSmoothingPrevFrame to High(FFrames) do
      DoTemporalSmoothing(@FFrames[i], @FFrames[i - cSmoothingPrevFrame], AIndex, seTempoSmoo.Value / 10.0);
  end;
begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(-1, esSmooth);

  ProcThreadPool.DoParallelLocalProc(@DoSmoothing, 0, cTileMapHeight - 1);

  ProgressRedraw(1);

  tbFrameChange(nil);
end;

procedure TMainForm.cbxYilMixChange(Sender: TObject);
begin
  FY2MixedColors := StrToIntDef(cbxYilMix.Text, 16);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_F2: btnLoadClick(nil);
    VK_F3: btnDitherClick(nil);
    VK_F4: btnDoMakeUniqueClick(nil);
    VK_F5: btnDoGlobalTilingClick(nil);
    VK_F6: btnDoKeyFrameTilingClick(nil);
    VK_F7: btnReindexClick(nil);
    VK_F8: btnSmoothClick(nil);
    VK_F9: btnSaveClick(nil);
    VK_F10: btnRunAllClick(nil);
    VK_F12: btnDebugClick(nil);
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

procedure TMainForm.seAvgTPFEditingDone(Sender: TObject);
begin
  if Length(FFrames) = 0 then Exit;
  seMaxTiles.Value := seAvgTPF.Value * Length(FFrames);
end;

procedure TMainForm.seMaxTilesEditingDone(Sender: TObject);
begin
  if Length(FFrames) = 0 then Exit;
  seAvgTPF.Value := seMaxTiles.Value div Length(FFrames);
end;

procedure TMainForm.tbFrameChange(Sender: TObject);
begin
  Screen.Cursor := crDefault;
  seAvgTPFEditingDone(nil);
  Render(tbFrame.Position, chkPlay.Checked, chkDithered.Checked, chkMirrored.Checked, chkReduced.Checked,  chkGamma.Checked, sedPalIdx.Value, sePage.Value);
end;

function TMainForm.GoldenRatioSearch(Func: TFloatFloatFunction; Mini, Maxi: TFloat; ObjectiveY: TFloat;
  Epsilon: TFloat; Data: Pointer): TFloat;
var
  x, y: TFloat;
begin
  if SameValue(Mini, Maxi, Epsilon) then
  begin
    DebugLn('GoldenRatioSearch failed!');
    Result := NaN;
    Exit;
  end;

  x := lerp(Mini, Maxi, 1.0 - cInvPhi);
  y := Func(x, Data);

  //EnterCriticalSection(FCS);
  //WriteLn('X: ', FormatFloat('0.000', x), #9'Y: ', FormatFloat('0.000', y), #9'Mini: ', FormatFloat('0.000', Mini), #9'Maxi: ', FormatFloat('0.000', Maxi));
  //LeaveCriticalSection(FCS);

  case CompareValue(y, ObjectiveY, Epsilon) of
    LessThanValue:
      Result := GoldenRatioSearch(Func, x, Maxi, ObjectiveY, Epsilon, Data);
    GreaterThanValue:
      Result := GoldenRatioSearch(Func, Mini, x, ObjectiveY, Epsilon, Data);
  else
      Result := x;
  end;
end;


procedure TMainForm.PreparePlan(var Plan: TMixingPlan; MixedColors: Integer; const pal: array of Integer);
var
  i, col, r, g, b: Integer;
begin
  FillChar(Plan, SizeOf(Plan), 0);

  Plan.Y2MixedColors := MixedColors;
  SetLength(Plan.LumaPal, Length(pal));
  SetLength(Plan.Y2Palette, Length(pal));

  SetLength(Plan.CountCache, cTotalColors);
  SetLength(Plan.ListCache, cTotalColors);
  FillByte(Plan.CountCache[0], cTotalColors, $ff);

  InitializeCriticalSection(Plan.CacheCS);

  for i := 0 to High(pal) do
  begin
    col := pal[i];
    r := col and $ff;
    g := (col shr 8) and $ff;
    b := (col shr 16) and $ff;

    Plan.LumaPal[i] := r*cRedMultiplier + g*cGreenMultiplier + b*cBlueMultiplier;

    Plan.Y2Palette[i][0] := r;
    Plan.Y2Palette[i][1] := g;
    Plan.Y2Palette[i][2] := b;
    Plan.Y2Palette[i][3] := Plan.LumaPal[i] div cLumaMultiplier;
  end
end;

procedure TMainForm.TerminatePlan(var Plan: TMixingPlan);
begin
  DeleteCriticalSection(Plan.CacheCS);

  SetLength(Plan.LumaPal, 0);
  SetLength(Plan.Y2Palette, 0);
  SetLength(Plan.CountCache, 0);
  SetLength(Plan.ListCache, 0);
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

function TMainForm.ColorCompare(r1, g1, b1, r2, g2, b2: Integer): Int64;
var
  luma1, luma2, lumadiff, diffR, diffG, diffB: Int64;
begin
  luma1 := r1 * cRedMultiplier + g1 * cGreenMultiplier + b1 * cBlueMultiplier;
  luma2 := r2 * cRedMultiplier + g2 * cGreenMultiplier + b2 * cBlueMultiplier;
  lumadiff := luma1 - luma2;
  diffR := r1 - r2;
  diffG := g1 - g2;
  diffB := b1 - b2;
  Result := diffR * diffR * cRedMultiplier * 3 div 4; // 3 div 4 for 0.75 chroma importance reduction
  Result += diffG * diffG * cGreenMultiplier * 3 div 4;
  Result += diffB * diffB * cBlueMultiplier * 3 div 4;
  Result += lumadiff * lumadiff;
end;

function TMainForm.DeviseBestMixingPlan(var Plan: TMixingPlan; col: Integer; List: TByteDynArray): Integer;
label
  pal_loop, inner_loop, worst;
var
  r, g, b: Integer;
  t, index, max_test_count, plan_count, y2pal_len: Integer;
  chosen_amount, chosen, least_penalty, penalty: Int64;
  so_far, sum, add: array[0..3] of Integer;
  VecInv: PCardinal;
  y2pal: PInteger;
begin
  EnterCriticalSection(Plan.CacheCS);
  if Plan.CountCache[col] < $80 then
  begin
    Result := Plan.CountCache[col];
    Move(Plan.ListCache[col, 0], List[0], Result);
    LeaveCriticalSection(Plan.CacheCS);
    Exit;
  end
  else
  begin
    LeaveCriticalSection(Plan.CacheCS);
  end;

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

    mov eax, r
    mov ebx, g
    mov ecx, b

    pinsrd xmm4, eax, 0
    pinsrd xmm4, ebx, 1
    pinsrd xmm4, ecx, 2

    imul eax, cRedMultiplier
    imul ebx, cGreenMultiplier
    imul ecx, cBlueMultiplier

    add eax, ebx
    add eax, ecx
    imul eax, (1 shl 22) / cLumaMultiplier
    shr eax, 22

    pinsrd xmm4, eax, 3

    mov rax, 1 or (1 shl 32)
    pinsrq xmm5, rax, 0
    pinsrq xmm5, rax, 1

    mov rax, (cRedMultiplier * 3 / 16) or ((cGreenMultiplier * 3 / 16) shl 32)
    pinsrq xmm6, rax, 0
    mov rax, (cBlueMultiplier * 3 / 16) or ((cLumaMultiplier * 4 / 16) shl 32)
    pinsrq xmm6, rax, 1

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

  EnterCriticalSection(Plan.CacheCS);
  Plan.ListCache[col] := Copy(List, 0, Result);
  Plan.CountCache[col] := Result;
  LeaveCriticalSection(Plan.CacheCS);
end;

procedure TMainForm.DitherTile(var ATile: TTile; var Plan: TMixingPlan);
var
  x, y: Integer;
  count, map_value: Integer;
  list: TByteDynArray;
begin
  SetLength(list, cDitheringListLen);

  for y := 0 to (cTileWidth - 1) do
    for x := 0 to (cTileWidth - 1) do
    begin
      map_value := cDitheringMap[(y shl 3) + x];
      count := DeviseBestMixingPlan(Plan, ATile.RGBPixels[y,x], list);
      map_value := (map_value * count) shr 6;
      ATile.PalPixels[y, x] := List[map_value];
    end;
end;

type
  TCountIndex = (ciCount, ciIndex, ciHue, ciSat, ciLit);
  TCountIndexArray = array[Low(TCountIndex)..High(TCountIndex)] of Integer;
  PCountIndexArray = ^TCountIndexArray;


function CompareCMUCntHSL(Item1,Item2:Pointer):Integer;
begin
  Result := CompareValue(PCountIndexArray(Item2)^[ciCount], PCountIndexArray(Item1)^[ciCount]);
  if Result = 0 then
    Result := CompareValue(PCountIndexArray(Item1)^[ciHue], PCountIndexArray(Item2)^[ciHue]);
  if Result = 0 then
    Result := CompareValue(PCountIndexArray(Item1)^[ciLit], PCountIndexArray(Item2)^[ciLit]);
  if Result = 0 then
    Result := CompareValue(PCountIndexArray(Item1)^[ciSat], PCountIndexArray(Item2)^[ciSat]);
end;

procedure TMainForm.FindBestKeyframePalette(AKeyFrame: PKeyFrame; ColorReach: TFloat);
const
  cRGShift = (8 - cBitsPerComp);
  cBShift = (8 - cBitsPerComp) * 2;
  cRMask = ((1 shl cBitsPerComp) - 1);
  cGMask = ((1 shl cBitsPerComp) - 1) shl 8;
  cBMask = ((1 shl cBitsPerComp) - 1) shl 16;
var
  col, cnt, sx, sy, tx, ty, i, PalIdx, LastUsed, CmlPct, acc: Integer;
  GTile: PTile;
  CMUsage: TList;
  CMItem: PCountIndexArray;
  TrueColorUsage: TCardinalDynArray;
  CmlReach: TFloat;
begin
  Assert(cPaletteCount <= Length(cPalettePattern));

  SetLength(TrueColorUsage, 1 shl (24 - cRGShift));
  FillDWord(TrueColorUsage[0], Length(TrueColorUsage), 0);

  CMUsage := TList.Create;
  try
    CMUsage.Count := cTotalColors;
    for i := 0 to cTotalColors - 1 do
    begin
      New(CMItem);
      CMItem^[ciCount] := 0;
      CMItem^[ciIndex] := i;
      RGBToHSL(FColorMap[i], CMItem^[ciHue], CMItem^[ciSat], CMItem^[ciLit]);
      CMUsage[i] := CMItem;
    end;

    // get color usage stats

    for i := AKeyFrame^.StartFrame to AKeyFrame^.EndFrame do
    begin
      for sy := 0 to cTileMapHeight - 1 do
        for sx := 0 to cTileMapWidth - 1 do
        begin
          GTile := FTiles[FFrames[i].TileMap[sy, sx].GlobalTileIndex];

          for ty := 0 to cTileWidth - 1 do
            for tx := 0 to cTileWidth - 1 do
            begin
              col := GTile^.RGBPixels[ty, tx];
              col := col shr cRGShift;
              Inc(TrueColorUsage[col]);
            end;
        end;
    end;

    for i := 0 to High(TrueColorUsage) do
    begin
      cnt := TrueColorUsage[i];
      col := (i and cRMask) or ((i and cGMask) shr cRGShift) or ((i and cBMask) shr cBShift);
      Inc(PCountIndexArray(CMUsage[col])^[ciCount], cnt);
    end;

    // sort colors by use count

    CMUsage.Sort(@CompareCMUCntHSL);

    LastUsed := -1;
    for i := cTotalColors - 1 downto 0 do
      if PCountIndexArray(CMUsage[i])^[ciCount] <> 0 then
      begin
        LastUsed := i;
        Break;
      end;

    CmlPct := 0;
    acc := AKeyFrame^.FrameCount * cTileMapSize * sqr(cTileWidth);
    acc := acc - (acc div 20); // 95% point
    for i := 0 to cTotalColors - 1 do
    begin
      acc -= PCountIndexArray(CMUsage[i])^[ciCount];
      if acc <= 0 then
      begin
        CmlPct := i;
        Break;
      end;
    end;

    EnterCriticalSection(FCS);
    WriteLn('KF: ', AKeyFrame^.StartFrame, #9'LastUsed: ', LastUsed, #9'CmlPct: ', CmlPct);
    LeaveCriticalSection(FCS);

    // after LastUsed put 'weird' colors (ie. colors not in frames, but relevant for Y2)

    for i := LastUsed + 1 to LastUsed + (LastUsed shr 1) do
    begin
      CMItem := PCountIndexArray(CMUsage[i]);
      CMItem^ := PCountIndexArray(CMUsage[(i - LastUsed) shl 1])^;
      CMItem^[ciCount] := 0;
      col := HSLToRGB(CMItem^[ciHue], 255 - CMItem^[ciSat], CMItem^[ciLit]);
      col := col shr cRGShift;
      col := (col and cRMask) or ((col and cGMask) shr cRGShift) or ((col and cBMask) shr cBShift);
      CMItem^[ciIndex] := col;
    end;

    // split most used colors into 16 color palettes, with the best few repeated everywhere

    for PalIdx := 0 to cPaletteCount - 1 do
    begin
      SetLength(AKeyFrame^.PaletteIndexes[PalIdx], cTilePaletteSize);
      SetLength(AKeyFrame^.PaletteRGB[PalIdx], cTilePaletteSize);
    end;

    CmlReach := EnsureRange((CmlPct + 1) * ColorReach, 0, cTotalColors);
    for i := 0 to cTilePaletteSize - 1 do
      for PalIdx := 0 to cPaletteCount - 1 do
        AKeyFrame^.PaletteIndexes[PalIdx, i] := PCountIndexArray(CMUsage[Round(cPalettePattern[PalIdx, i] * CmlReach)])^[ciIndex];

    for i := 0 to cTilePaletteSize - 1 do
      for PalIdx := 0 to cPaletteCount - 1 do
        AKeyFrame^.PaletteRGB[PalIdx, i] := FColorMap[AKeyFrame^.PaletteIndexes[PalIdx, i]];

    for i := 0 to CMUsage.Count - 1 do
      Dispose(PCountIndexArray(CMUsage[i]));

  finally
    CMUsage.Free;
  end;
end;

procedure TMainForm.FinalDitherTiles(AFrame: PFrame);
var
  i, PalIdx: Integer;
  sx, sy: Integer;
  best, cmp: TFloat;
  BestTile: TTile;
  OrigTile: PTile;
  TileDCT, OrigTileDCT: TFloatDynArray;
begin
  EnterCriticalSection(FCS);
  if AFrame^.KeyFrame^.FrameDitheringLeft < 0 then
  begin
    for i := 0 to cPaletteCount - 1 do
      PreparePlan(AFrame^.KeyFrame^.MixingPlans[i], FY2MixedColors, AFrame^.KeyFrame^.PaletteRGB[i]);
    AFrame^.KeyFrame^.FrameDitheringLeft := AFrame^.KeyFrame^.FrameCount;
  end;
  LeaveCriticalSection(FCS);

  for sy := 0 to cTileMapHeight - 1 do
    for sx := 0 to cTileMapWidth - 1 do
    begin
      OrigTile := FTiles[AFrame^.TileMap[sy, sx].GlobalTileIndex];

      if not OrigTile^.Active then
        Exit;

      // choose best palette from the keyframe by comparing DCT of the tile colored with either palette

      ComputeTileDCT(OrigTile^, False, False, False, False, False, 0, OrigTile^.PaletteRGB, OrigTileDCT);

      PalIdx := -1;
      best := MaxDouble;
      for i := 0 to cPaletteCount - 1 do
      begin
        DitherTile(OrigTile^, AFrame^.KeyFrame^.MixingPlans[i]);
        ComputeTileDCT(OrigTile^, True, False, False, False, False, 0, AFrame^.KeyFrame^.PaletteRGB[i], TileDCT);
        cmp := CompareEuclidean192(TileDCT, OrigTileDCT);
        if cmp < best then
        begin
          PalIdx := i;
          best := cmp;
          CopyTile(OrigTile^, BestTile);
        end;
      end;

      // now that the palette is chosen, keep only one version of the tile

      CopyTile(BestTile, AFrame^.Tiles[sx + sy * cTileMapWidth]);

      AFrame^.TileMap[sy, sx].PalIdx := PalIdx;
      AFrame^.Tiles[sx + sy * cTileMapWidth].PaletteRGB := Copy(AFrame^.KeyFrame^.PaletteRGB[PalIdx]);
      AFrame^.Tiles[sx + sy * cTileMapWidth].PaletteIndexes := Copy(AFrame^.KeyFrame^.PaletteIndexes[PalIdx]);
      SetLength(BestTile.PaletteIndexes, 0);
      SetLength(BestTile.PaletteRGB, 0);
      OrigTile^ := BestTile;
    end;

  EnterCriticalSection(FCS);
  Dec(AFrame^.KeyFrame^.FrameDitheringLeft);
  if AFrame^.KeyFrame^.FrameDitheringLeft <= 0 then
    for i := 0 to cPaletteCount - 1 do
      TerminatePlan(AFrame^.KeyFrame^.MixingPlans[i]);
  LeaveCriticalSection(FCS);
end;

function CompareTilePalPixels(Item1, Item2, UserParameter:Pointer):Integer;
var
  t1, t2: PTile;
begin
  t1 := PPTile(Item1)^;
  t2 := PPTile(Item2)^;
  Result := CompareByte(t1^.PalPixels[0, 0], t2^.PalPixels[0, 0], sqr(cTileWidth));
end;

procedure TMainForm.MakeTilesUnique(FirstTileIndex, TileCount: Integer);
var
  i, firstSameIdx: Integer;
  sortArr: array of PTile;
  sameIdx: array of Integer;

  procedure DoOneMerge;
  var
    j: Integer;
  begin
    if i - firstSameIdx >= 2 then
    begin
      for j := firstSameIdx to i - 1 do
        sameIdx[j - firstSameIdx] := sortArr[j]^.TmpIndex;
      MergeTiles(sameIdx, i - firstSameIdx, sameIdx[0], nil);
    end;
    firstSameIdx := i;
  end;

begin
  // sort global tiles by palette indexes (L to R, T to B)

  SetLength(sameIdx, TileCount);
  sortArr := Copy(FTiles, FirstTileIndex, TileCount);

  for i := 0 to High(sortArr) do
    sortArr[i]^.TmpIndex := i + FirstTileIndex;

  QuickSort(sortArr[0], 0, High(sortArr), SizeOf(PTile), @CompareTilePalPixels);

  // merge exactly similar tiles (so, consecutive after prev code)

  firstSameIdx := 0;
  for i := 1 to High(sortArr) do
    if CompareByte(sortArr[i - 1]^.PalPixels[0, 0], sortArr[i]^.PalPixels[0, 0], sqr(cTileWidth)) <> 0 then
      DoOneMerge;

  i := High(sortArr);
  DoOneMerge;
end;

procedure TMainForm.LoadTiles;
var
  i,j,x,y: Integer;
  tileCnt: Integer;
begin
  // free memory from a prev run
  for i := 0 to High(FTiles) do
    Dispose(FTiles[i]);

  tileCnt := Length(FFrames) * cTileMapSize;

  SetLength(FTiles, tileCnt);

  // allocate tiles
  for i := 0 to High(FTiles) do
    FTiles[i] := New(PTile);

  // copy frame tiles to global tiles, point tilemap on proper global tiles
  for i := 0 to High(FFrames) do
  begin
    tileCnt := i * cTileMapSize;
    for j := 0 to cTileMapSize - 1 do
      CopyTile(FFrames[i].Tiles[j], FTiles[tileCnt+j]^);
    for y := 0 to (cTileMapHeight - 1) do
      for x := 0 to (cTileMapWidth - 1) do
        Inc(FFrames[i].TileMap[y,x].GlobalTileIndex, tileCnt);
  end;
end;

procedure TMainForm.RGBToYUV(r, g, b: Integer;  GammaCor: Integer; out y, u, v: TFloat); inline;
var
  fr, fg, fb: TFloat;
  yy, uu, vv: TFloat;
begin
  if GammaCor >= 0 then
  begin
    fr := GammaCorrect(GammaCor, r);
    fg := GammaCorrect(GammaCor, g);
    fb := GammaCorrect(GammaCor, b);
  end
  else
  begin
    fr := r / 255.0;
    fg := g / 255.0;
    fb := b / 255.0;
  end;

  yy := 0.299*fr + 0.587*fg + 0.114*fb;
  uu := -0.147*fr - 0.289*fg + 0.436*fb;
  vv := 0.615*fr - 0.515*fg - 0.100*fb;

  y := yy; u := uu; v := vv; // for safe "out" param
end;

procedure TMainForm.RGBToYUV(fr, fg, fb: TFloat; out y, u, v: TFloat); inline;
var
  yy, uu, vv: TFloat;
begin
  yy := 0.299*fr + 0.587*fg + 0.114*fb;
  uu := -0.147*fr - 0.289*fg + 0.436*fb;
  vv := 0.615*fr - 0.515*fg - 0.100*fb;

  y := yy; u := uu; v := vv; // for safe "out" param
end;

procedure TMainForm.RGBToLAB(ir, ig, ib: Integer; out ol, oa, ob: TFloat); inline;
var
  r, g, b, x, y, z: TFloat;
begin
  r := GammaCorrect(1, ir);
  g := GammaCorrect(1, ig);
  b := GammaCorrect(1, ib);

  if r > 0.04045 then r := power((r + 0.055) / 1.055, 2.4) else r := r / 12.92;
  if g > 0.04045 then g := power((g + 0.055) / 1.055, 2.4) else g := g / 12.92;
  if b > 0.04045 then b := power((b + 0.055) / 1.055, 2.4) else b := b / 12.92;

  // CIE XYZ color space from the Wright–Guild data
  x := (r * 0.49000 + g * 0.31000 + b * 0.20000) / 0.17697;
  y := (r * 0.17697 + g * 0.81240 + b * 0.01063) / 0.17697;
  z := (r * 0.00000 + g * 0.01000 + b * 0.99000) / 0.17697;

{$if false}
  // Illuminant D50
  x /= 96.6797;
  y /= 100.000;
  z /= 82.5188;
{$else}
  // Illuminant D65
  x /= 95.0470;
  y /= 100.000;
  z /= 108.883;
{$endif}

  if x > 0.008856 then x := power(x, 1/3) else x := (7.787 * x) + 16/116;
  if y > 0.008856 then y := power(y, 1/3) else y := (7.787 * y) + 16/116;
  if z > 0.008856 then z := power(z, 1/3) else z := (7.787 * z) + 16/116;

  ol := (116 * y) - 16;
  oa := 500 * (x - y);
  ob := 200 * (y - z);
end;

procedure TMainForm.RGBToLAB(r, g, b: TFloat; out ol, oa, ob: TFloat); inline;
var
  ll, aa, bb: TFloat;
begin
  RGBToLAB(Integer(round(r * 255.0)), round(g * 255.0), round(b * 255.0), ll, aa, bb);
  ol := ll;
  oa := aa;
  ob := bb;
end;

procedure TMainForm.RGBToColSpc(r, g, b: TFloat; LAB: Boolean; out ol, oa, ob: TFloat);
begin
  if LAB then
    RGBToLAB(r, g, b, ol, oa, ob)
  else
    RGBToYUV(r, g, b, ol, oa, ob);
end;

procedure TMainForm.RGBToColSpc(col: Integer; LAB: Boolean; GammaCor: Integer; out ol, oa, ob: TFloat);
var
  ir, ig, ib: Integer;
begin
  FromRGB(col, ir, ig, ib);
  if LAB then
    RGBToLAB(ir, ig, ib, ol, oa, ob)
  else
    RGBToYUV(ir, ig, ib, GammaCor, ol, oa, ob);
end;

procedure TMainForm.ComputeTileDCT(const ATile: TTile; FromPal, QWeighting, LAB, HMirror, VMirror: Boolean;
  GammaCor: Integer; const pal: array of Integer; var DCT: TFloatDynArray);
const
  cUVRatio: array[0..cTileWidth-1] of TFloat = (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1);
var
  u, v, x, y, xx, yy, di, cpn: Integer;
  vRatio, z: TFloat;
  YUVPixels: array[0..2, 0..cTileWidth-1,0..cTileWidth-1] of TFloat;
  pYuv, pLut: PFloat;
begin
  SetLength(DCT, 3 * sqr(cTileWidth));

  if FromPal then
  begin
    for y := 0 to (cTileWidth - 1) do
      for x := 0 to (cTileWidth - 1) do
      begin
        xx := x;
        yy := y;
        if HMirror then xx := cTileWidth - 1 - x;
        if VMirror then yy := cTileWidth - 1 - y;

        RGBToColSpc(pal[ATile.PalPixels[yy,xx]], LAB, GammaCor, YUVPixels[0,y,x], YUVPixels[1,y,x], YUVPixels[2,y,x]);
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

        RGBToColSpc(ATile.RGBPixels[yy,xx], LAB, GammaCor, YUVPixels[0,y,x], YUVPixels[1,y,x], YUVPixels[2,y,x]);
      end;
  end;

  di := 0;
  for cpn := 0 to 2 do
    for v := 0 to (cTileWidth - 1) do
    begin
      vRatio := cUVRatio[v];

      for u := 0 to (cTileWidth - 1) do
      begin
		    z := 0.0;

        for y := 0 to (cTileWidth - 1) do
        begin
          pYuv := @YUVPixels[cpn, y, 0];
          pLut := @gDCTLut[v, u, y, 0];

          // unroll x by cTileWidth
          z += pYuv^ * pLut^; Inc(pYuv); Inc(pLut);
          z += pYuv^ * pLut^; Inc(pYuv); Inc(pLut);
          z += pYuv^ * pLut^; Inc(pYuv); Inc(pLut);
          z += pYuv^ * pLut^; Inc(pYuv); Inc(pLut);
          z += pYuv^ * pLut^; Inc(pYuv); Inc(pLut);
          z += pYuv^ * pLut^; Inc(pYuv); Inc(pLut);
          z += pYuv^ * pLut^; Inc(pYuv); Inc(pLut);
          z += pYuv^ * pLut^;
        end;

        DCT[di] := cUVRatio[u] * vRatio * z;

        if QWeighting then
           DCT[di] *= 16.0 / sqrt(cDCTQuantization[cpn, v, u]);

        Inc(di);
	    end;
    end;
end;

procedure TMainForm.VMirrorPalTile(var ATile: TTile);
var
  j, i: Integer;
  v: Integer;
begin
  // hardcode vertical mirror into the tile

  for j := 0 to cTileWidth div 2 - 1  do
    for i := 0 to cTileWidth - 1 do
    begin
      v := ATile.PalPixels[j, i];
      ATile.PalPixels[j, i] := ATile.PalPixels[cTileWidth - 1 - j, i];
      ATile.PalPixels[cTileWidth - 1 - j, i] := v;
    end;
end;

procedure TMainForm.HMirrorPalTile(var ATile: TTile);
var
  i, j: Integer;
  v: Integer;
begin
  // hardcode horizontal mirror into the tile

  for j := 0 to cTileWidth - 1 do
    for i := 0 to cTileWidth div 2 - 1  do
    begin
      v := ATile.PalPixels[j, i];
      ATile.PalPixels[j, i] := ATile.PalPixels[j, cTileWidth - 1 - i];
      ATile.PalPixels[j, cTileWidth - 1 - i] := v;
    end;
end;

procedure TMainForm.LoadFrame(AFrame: PFrame; ABitmap: TBitmap);
var
  i, j, px, r, g, b, ti, tx, ty: Integer;
  pcol: PInteger;
begin
  for j := 0 to (cTileMapHeight - 1) do
    for i := 0 to (cTileMapWidth - 1) do
    begin
      AFrame^.TileMap[j, i].GlobalTileIndex := cTileMapWidth * j + i;
      AFrame^.TileMap[j, i].HMirror := False;
      AFrame^.TileMap[j, i].VMirror := False;
      AFrame^.TileMap[j, i].PalIdx := 0;
      AFrame^.TileMap[j, i].Smoothed := False;
      AFrame^.TileMap[j, i].TmpIndex := -1;
    end;

  Assert(ABitmap.Width = cScreenWidth, 'Wrong video width!');
  Assert(ABitmap.Height = cScreenHeight, 'Wrong video height!');

  ABitmap.BeginUpdate;
  try
    for j := 0 to (cScreenHeight - 1) do
    begin
      pcol := ABitmap.ScanLine[j];
      for i := 0 to (cScreenWidth - 1) do
        begin
          px := pcol^;
          Inc(pcol);

          b := px and $ff;
          g := (px shr 8) and $ff;
          r := (px shr 16) and $ff;

          ti := cTileMapWidth * (j shr 3) + (i shr 3);
          tx := i and (cTileWidth - 1);
          ty := j and (cTileWidth - 1);

          AFrame^.Tiles[ti].RGBPixels[ty, tx] := ToRGB(r, g, b);
        end;
    end;

    for i := 0 to (cTileMapSize - 1) do
    begin
      for ty := 0 to (cTileWidth - 1) do
        for tx := 0 to (cTileWidth - 1) do
          AFrame^.Tiles[i].PalPixels[ty, tx] := 0;

      AFrame^.Tiles[i].Active := True;
      AFrame^.Tiles[i].AveragedCount := 1;
      AFrame^.Tiles[i].TmpIndex := -1;
    end;

  finally
    ABitmap.EndUpdate;
  end;
end;

procedure TMainForm.Render(AFrameIndex: Integer; playing, dithered, mirrored, reduced, gamma: Boolean; palIdx: Integer;
  ATilePage: Integer);

  procedure DrawTile(bitmap: TBitmap; sx, sy: Integer; tilePtr: PTile; pal: TIntegerDynArray; hmir, vmir: Boolean);
  var
    r, g, b, tx, ty, txm, tym: Integer;
    psl: PInteger;
  begin
    for ty := 0 to cTileWidth - 1 do
    begin
      psl := bitmap.ScanLine[ty + sy * cTileWidth];
      Inc(psl, sx * cTileWidth);

      tym := ty;
      if vmir and mirrored then tym := cTileWidth - 1 - tym;

      for tx := 0 to cTileWidth - 1 do
      begin
        txm := tx;
        if hmir and mirrored then txm := cTileWidth - 1 - txm;

        if dithered and Assigned(pal) then
          FromRGB(pal[tilePtr^.PalPixels[tym, txm]], r, g, b)
        else
          FromRGB(tilePtr^.RGBPixels[tym, txm], r, g, b);

        if gamma then
        begin
          r := round(GammaCorrect(0, r) * 255.0);
          g := round(GammaCorrect(0, g) * 255.0);
          b := round(GammaCorrect(0, b) * 255.0);
        end;

        if not tilePtr^.Active then
        begin
          r := 255;
          g := 0;
          b := 255;
        end;

        psl^ := SwapRB(ToRGB(r, g, b));
        Inc(psl);
      end;
    end;
  end;

var
  i, j, sx, sy, ti: Integer;
  p: PInteger;
  tilePtr: PTile;
  TMItem: TTileMapItem;
  Frame: PFrame;
  pal: TIntegerDynArray;
  oriCorr, chgCorr: TIntegerDynArray;
begin
  if Length(FFrames) <= 0 then
    Exit;

  AFrameIndex := EnsureRange(AFrameIndex, 0, high(FFrames));

  Frame := @FFrames[AFrameIndex];

  if not Assigned(Frame) or not Assigned(Frame^.KeyFrame) then
    Exit;

  if not playing then
  begin
    lblTileCount.Caption := 'Global: ' + IntToStr(GetGlobalTileCount) + ' / Frame: ' + IntToStr(GetFrameTileCount(Frame));

    imgTiles.Picture.Bitmap.BeginUpdate;
    try
      imgTiles.Picture.Bitmap.Canvas.Brush.Color := clAqua;
      imgTiles.Picture.Bitmap.Canvas.Brush.Style := bsSolid;
      imgTiles.Picture.Bitmap.Canvas.Clear;

      for sy := 0 to cTileMapHeight - 1 do
        for sx := 0 to cTileMapWidth div 2 - 1 do
        begin
          ti := cTileMapWidth div 2 * sy + sx + cTileMapSize div 2 * ATilePage;

          if ti < Length(FTiles) then
          begin
            tilePtr := FTiles[ti];
            pal := Frame^.KeyFrame^.PaletteRGB[Max(0, palIdx)];

            DrawTile(imgTiles.Picture.Bitmap, sx, sy, tilePtr, pal, False, False);
          end;
        end;
    finally
      imgTiles.Picture.Bitmap.EndUpdate;
    end;
  end;

  imgSource.Picture.Bitmap.BeginUpdate;
  try
    for sy := 0 to cTileMapHeight - 1 do
      for sx := 0 to cTileMapWidth - 1 do
      begin
        tilePtr :=  @Frame^.Tiles[sy * cTileMapWidth + sx];
        DrawTile(imgSource.Picture.Bitmap, sx, sy, tilePtr, nil, False, False);
      end;
  finally
    imgSource.Picture.Bitmap.EndUpdate;
  end;

  imgDest.Picture.Bitmap.BeginUpdate;
  try
    SetLength(oriCorr, cScreenHeight * cScreenWidth * 2);
    SetLength(chgCorr, cScreenHeight * cScreenWidth * 2);

    for sy := 0 to cTileMapHeight - 1 do
      for sx := 0 to cTileMapWidth - 1 do
      begin
        TMItem := Frame^.TileMap[sy, sx];
        ti := TMItem.GlobalTileIndex;

        if ti < Length(FTiles) then
        begin
          tilePtr :=  @Frame^.Tiles[sy * cTileMapWidth + sx];
          pal := tilePtr^.PaletteRGB;

          if reduced then
          begin
            tilePtr := FTiles[ti];
            if palIdx < 0 then
              pal := Frame^.KeyFrame^.PaletteRGB[TMItem.PalIdx]
            else
              pal := Frame^.KeyFrame^.PaletteRGB[palIdx];
          end;

          DrawTile(imgDest.Picture.Bitmap, sx, sy, tilePtr, pal, TMItem.HMirror, TMItem.VMirror);
        end;
      end;
  finally
    imgDest.Picture.Bitmap.EndUpdate;
  end;

  imgPalette.Picture.Bitmap.BeginUpdate;
  try
    for j := 0 to imgPalette.Height - 1 do
    begin
      p := imgPalette.Picture.Bitmap.ScanLine[j];
      for i := 0 to imgPalette.Width - 1 do
      begin
        if Assigned(Frame^.KeyFrame^.PaletteRGB[j div 16]) then
          p^ := SwapRB(Frame^.KeyFrame^.PaletteRGB[j div 16, i * cTilePaletteSize div imgPalette.Width])
        else
          p^ := $00ff00ff;

        Inc(p);
      end;
    end;
  finally
    imgPalette.Picture.Bitmap.EndUpdate;
  end;

  if not playing then
  begin
    for i := 0 to cScreenHeight - 1 do
    begin
      Move(PInteger(imgSource.Picture.Bitmap.ScanLine[i])^, oriCorr[i * cScreenWidth], cScreenWidth * SizeOf(Integer));
      Move(PInteger(imgDest.Picture.Bitmap.ScanLine[i])^, chgCorr[i * cScreenWidth], cScreenWidth * SizeOf(Integer));
    end;

    lblCorrel.Caption := FormatFloat('0.0000', ComputeCorrelation(oriCorr, chgCorr));
  end;
end;

// from https://www.delphipraxis.net/157099-fast-integer-rgb-hsl.html
procedure TMainForm.RGBToHSL(RGBCol: Integer; var h, s, l: integer);
var
  rr, gg, bb: Integer;

  function RGBMaxValue: byte;
  begin
    Result := rr;
    if (Result < gg) then Result := gg;
    if (Result < bb) then Result := bb;
  end;

  function RGBMinValue : byte;
  begin
    Result := rr;
    if (Result > gg) then Result := gg;
    if (Result > bb) then Result := bb;
  end;

var
  Delta, Min: Integer;
begin
  FromRGB(RGBCol, rr, gg, bb);

  L := RGBMaxValue;
  Min := RGBMinValue;
  Delta := L-Min;
  if (L = Min) then
  begin
    H := 0;
    S := 0;
  end
  else
  begin
    S := MulDiv(Delta, 255, L);

    if (rr = L) then
      H := MulDiv(60, gg-bb, Delta)
    else if (gg = L) then
      H := MulDiv(60, bb-rr, Delta) + 120
    else if (bb = L) then
      H := MulDiv(60, rr-gg, Delta) + 240;

    if (H < 0) then H := H + 360;
  end;
end;

function TMainForm.HSLToRGB(H, S, L: integer): Integer;
const
  MaxHue: integer = 360;
  MaxSat: integer = 255;
  MaxLum: integer = 255;
  Divisor = 255*60;
var
 hTemp, f, LS, p, q, r: integer;
begin
 H := H mod MaxHue;
 S := EnsureRange(S, 0, MaxSat);
 L := EnsureRange(L, 0, MaxLum);
 if (S = 0) then
   Result := ToRGB(L, L, L)
 else
  begin
   hTemp := H mod MaxHue;
   f := hTemp mod 60;
   hTemp := hTemp div 60;
   LS := L*S;
   p := L - LS div MaxLum;
   q := L - (LS*f) div Divisor;
   r := L - (LS*(60 - f)) div Divisor;
   case hTemp of
    0: Result := ToRGB(L, r, p);
    1: Result := ToRGB(q, L, p);
    2: Result := ToRGB(p, L, r);
    3: Result := ToRGB(p, q, L);
    4: Result := ToRGB(r, p, L);
    5: Result := ToRGB(L, p, q);
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
  t: Integer;
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
    FProgressPrevTime := GetTickCount;
    FProgressStartTime := FProgressPrevTime;
  end;

  if (CurFrameIdx < 0) and (ProgressStep = esNone) then
  begin
    FProgressPosition := 0;
    FOldProgressPosition := 0;
    FProgressStep := esNone;
    FProgressPosition := 0;
    FProgressPrevTime := GetTickCount;
    FProgressStartTime := FProgressPrevTime;
  end;

  pbProgress.Position := pbProgress.Position + (FProgressPosition - FOldProgressPosition);
  pbProgress.Invalidate;
  lblPct.Caption := IntToStr(pbProgress.Position * 100 div pbProgress.Max) + '%';
  lblPct.Invalidate;
  Application.ProcessMessages;

  t := GetTickCount;
  if CurFrameIdx >= 0 then
  begin
    WriteLn('Step: ', GetEnumName(TypeInfo(TEncoderStep), Ord(FProgressStep)), ' / ', FProgressPosition,
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

function TMainForm.GetFrameTileCount(AFrame: PFrame): Integer;
var
  Used: TIntegerDynArray;
  i, j: Integer;
begin
  Result := 0;

  if Length(FTiles) = 0 then
    Exit;

  SetLength(Used, Length(FTiles));
  FillDWord(Used[0], Length(FTiles), 0);

  for j := 0 to cTileMapHeight - 1 do
    for i := 0 to cTileMapWidth - 1 do
      Used[AFrame^.TileMap[j, i].GlobalTileIndex] := 1;

  for i := 0 to High(Used) do
    Inc(Result, Used[i]);
end;

procedure TMainForm.CopyTile(const Src: TTile; var Dest: TTile);
var x,y: Integer;
begin
  Dest.Active := Src.Active;
  Dest.AveragedCount := Src.AveragedCount;
  Dest.TmpIndex := Src.TmpIndex;
  Dest.UseCount := Src.UseCount;

  SetLength(Dest.PaletteIndexes, Length(Src.PaletteIndexes));
  SetLength(Dest.PaletteRGB, Length(Src.PaletteRGB));

  if Assigned(Dest.PaletteIndexes) then
    move(Src.PaletteIndexes[0], Dest.PaletteIndexes[0], Length(Src.PaletteIndexes) * SizeOf(Integer));
  if Assigned(Dest.PaletteRGB) then
    move(Src.PaletteRGB[0], Dest.PaletteRGB[0], Length(Src.PaletteRGB) * SizeOf(Integer));

  for y := 0 to cTileWidth - 1 do
    for x := 0 to cTileWidth - 1 do
    begin
      Dest.RGBPixels[y, x] := Src.RGBPixels[y, x];
      Dest.PalPixels[y, x] := Src.PalPixels[y, x];
    end;
end;

procedure TMainForm.MergeTiles(const TileIndexes: array of Integer; TileCount: Integer; BestIdx: Integer;
  NewTile: PPalPixels);
var
  i, j, k: Integer;
begin
  if TileCount <= 0 then
    Exit;

  if Assigned(NewTile) then
    Move(NewTile^[0, 0], FTiles[BestIdx]^.PalPixels[0, 0], sizeof(TPalPixels));

  for k := 0 to TileCount - 1 do
  begin
    j := TileIndexes[k];

    if j = BestIdx then
      Continue;

    Inc(FTiles[BestIdx]^.AveragedCount, FTiles[j]^.AveragedCount);

    FTiles[j]^.Active := False;
    FTiles[j]^.TmpIndex := BestIdx;

    FillChar(FTiles[j]^.RGBPixels, SizeOf(FTiles[j]^.RGBPixels), 0);
    FillChar(FTiles[j]^.PalPixels, SizeOf(FTiles[j]^.PalPixels), 0);
  end;

  for k := 0 to High(FFrames) do
    for j := 0 to (cTileMapHeight - 1) do
        for i := 0 to (cTileMapWidth - 1) do
          if FTiles[FFrames[k].TileMap[j, i].GlobalTileIndex]^.TmpIndex = BestIdx then
            FFrames[k].TileMap[j, i].GlobalTileIndex := BestIdx;
end;

function TMainForm.GetMaxTPF(AKF: PKeyFrame): Integer;
var
  frame: Integer;
begin
  Result := 0;
  for frame := AKF^.StartFrame to AKF^.EndFrame do
    Result := Max(Result, GetFrameTileCount(@FFrames[frame]));
end;

procedure TMainForm.DoKeyFrameTiling(AKF: PKeyFrame);
var
  TRSize, di, i, frame, sy, sx: Integer;
  frm: PFrame;
  T: PTile;
  DS: PTileDataset;
  used: TBooleanDynArray;
  vmir, hmir: Boolean;
  palIdx: Integer;
begin
  DS := New(PTileDataset);
  AKF^.TileDS := DS;

  // make a list of all used tiles
  SetLength(DS^.FrameDataset, AKF^.FrameCount * cTileMapSize);

  SetLength(used, Length(FTiles));
  FillByte(used[0], Length(FTiles), 0);

  di := 0;
  for frame := 0 to AKF^.FrameCount - 1 do
  begin
    frm := @FFrames[AKF^.StartFrame + frame];
    for sy := 0 to cTileMapHeight - 1 do
      for sx := 0 to cTileMapWidth - 1 do
      begin
        ComputeTileDCT(frm^.Tiles[sy * cTileMapWidth + sx], cKFFromPal, cKFQWeighting, FUseLAB, False, False, cKFGamma, frm^.Tiles[sy * cTileMapWidth + sx].PaletteRGB, DS^.FrameDataset[di]);
        Inc(di);

        used[frm^.TileMap[sy, sx].GlobalTileIndex] := True;
      end;
  end;

  Assert(di = AKF^.FrameCount * cTileMapSize);

  TRSize := 0;
  for i := 0 to High(used) do
    TRSize += Ord(used[i]);

  SetLength(DS^.TRToTileIdx, TRSize);
  SetLength(DS^.Dataset, TRSize * cPaletteCount * 4);

  di := 0;
  for i := 0 to High(FTiles) do
    if used[i] then
    begin
      DS^.TRToTileIdx[di div (cPaletteCount * 4)] := i;
      T := FTiles[i];

      for palIdx := 0 to cPaletteCount - 1 do
        for vmir := False to True do
          for hmir := False to True do
          begin
            ComputeTileDCT(T^, True, cKFQWeighting, FUseLAB, hmir, vmir, cKFGamma, AKF^.PaletteRGB[palIdx], DS^.Dataset[di]);
            Inc(di);
          end;
    end;

  Assert(di = TRSize * cPaletteCount * 4);

  SetLength(DS^.Tags, Length(DS^.Dataset));
  for i := 0 to High(DS^.Dataset) do
    DS^.Tags[i] := i;

  KDTreeBuildTagged(DS^.Dataset, DS^.Tags, Length(DS^.Dataset[0]), 0, 2, DS^.KDT);
end;

procedure TMainForm.DoFrameTiling(AFrame: PFrame);
var
  sy, sx: Integer;
  DS: PTileDataset;
  tmiO: PTileMapItem;

  TPF, MaxTPF, i, tri: Integer;
  Used: TBooleanDynArray;
  DCT: TFloatDynArray;
  Tags: TIVector;
  KDBuf: Tkdtreerequestbuffer;
begin
  DS := AFrame^.KeyFrame^.TileDS;

  // map frame tilemap items to reduced tiles and mirrors and choose best corresponding palette

  SetLength(Used, Length(FTiles));
  SetLength(Tags, 1);

  MaxTPF := 0;
  FillChar(Used[0], Length(FTiles) * SizeOf(Boolean), 0);

  kdtreecreaterequestbuffer(DS^.KDT, KDBuf);

  for sy := 0 to cTileMapHeight - 1 do
    for sx := 0 to cTileMapWidth - 1 do
    begin
      DCT := DS^.FrameDataset[AFrame^.Index * cTileMapSize + sy * cTileMapWidth + sx];

      i := kdtreetsqueryknn(DS^.KDT, KDBuf, DCT, 1, True);
      Assert(i = 1);
      kdtreetsqueryresultstags(DS^.KDT, KDBuf, Tags);
      Assert(i = 1);
      tri := Tags[0];

      tmiO := @FFrames[AFrame^.Index].TileMap[sy, sx];

      tmiO^.GlobalTileIndex := DS^.TRToTileIdx[tri div (cPaletteCount * 4)];
      tmiO^.HMirror := (tri and 1) <> 0;
      tmiO^.VMirror := (tri and 2) <> 0;
      tmiO^.PalIdx := (tri shr 2) and (cPaletteCount - 1);

      Used[tmiO^.GlobalTileIndex] := True;
    end;

  FreeAndNil(KDBuf);

  TPF := 0;
  for i := 0 to High(Used) do
    Inc(TPF, Ord(Used[i]));

  MaxTPF := max(MaxTPF, TPF);

  EnterCriticalSection(FCS);
  WriteLn('KF: ', AFrame^.Index, #9'MaxTPF: ', MaxTPF, #9'TileCnt: ', Length(DS^.Dataset));
  LeaveCriticalSection(FCS);
end;

procedure TMainForm.DoTemporalSmoothing(AFrame, APrevFrame: PFrame; Y: Integer; Strength: TFloat);
const
  cSqrtFactor = 1 / (sqr(cTileWidth) * 3);
var
  sx: Integer;
  cmp: TFloat;
  TMI, PrevTMI: PTileMapItem;
  Tile_, PrevTile: TTile;
  TileDCT, PrevTileDCT: TFloatDynArray;
begin
  for sx := 0 to cTileMapWidth - 1 do
  begin
    PrevTMI := @APrevFrame^.TileMap[Y, sx];
    TMI := @AFrame^.TileMap[Y, sx];

    // compare DCT of current tile with tile from prev frame tilemap

    PrevTile := FTiles[PrevTMI^.GlobalTileIndex]^;
    Tile_ := FTiles[TMI^.GlobalTileIndex]^;

    ComputeTileDCT(PrevTile, True, True, FUseLAB, PrevTMI^.HMirror, PrevTMI^.VMirror, cGammaCorrectSmoothing, AFrame^.KeyFrame^.PaletteRGB[PrevTMI^.PalIdx], PrevTileDCT);
    ComputeTileDCT(Tile_, True, True, FUseLAB, TMI^.HMirror, TMI^.VMirror, cGammaCorrectSmoothing, AFrame^.KeyFrame^.PaletteRGB[TMI^.PalIdx], TileDCT);

    cmp := CompareEuclidean192(TileDCT, PrevTileDCT);
    cmp := sqrt(cmp * cSqrtFactor);

    // if difference is low enough, mark the tile as smoothed for tilemap compression use

    if Abs(cmp) <= Strength then
    begin
      TMI^.GlobalTileIndex := PrevTMI^.GlobalTileIndex;
      TMI^.HMirror := PrevTMI^.HMirror;
      TMI^.VMirror := PrevTMI^.VMirror;
      TMI^.PalIdx := PrevTMI^.PalIdx;
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
    for sy := 0 to cTileMapHeight - 1 do
      for sx := 0 to cTileMapWidth - 1 do
        Inc(Result, Ord(FFrames[i].TileMap[sy, sx].GlobalTileIndex = ATileIndex));
end;

function TMainForm.GetTileZoneMedian(const ATile: TTile; x, y, w, h: Integer): Integer;
var i, j: Integer;
    px: Byte;
    cntL, cntH: array [0..cTilePaletteSize - 1] of Integer;
    highest: Integer;
begin
  FillDWord(cntL[0], cTilePaletteSize, DWORD(Low(Integer)));
  FillDWord(cntH[0], cTilePaletteSize, High(Integer));

  for j := y to y + h - 1 do
    for i := x to x + w - 1 do
    begin
      px := ATile.PalPixels[j, i];

      if cntL[px] = Low(Integer) then
        cntL[px] := 1
      else
        Inc(cntL[px]);

      if cntH[px] = High(Integer) then
        cntH[px] := 1
      else
        Inc(cntH[px]);
    end;

  Result := cTilePaletteSize - 1;

  if MaxIntValue(cntL) = MinIntValue(cntH) then // "lack of significativity" test
    Exit;

  highest := -1;
  for i := 0 to cTilePaletteSize - 1 do
    if cntL[i] >= highest then
    begin
      Result := i;
      highest := cntL[i];
    end;
end;

function TMainForm.GetTileGridMedian(const ATile: TTile; other: Boolean): Integer;
var i, j: Integer;
    px: Byte;
    cntL, cntH: array [0..cTilePaletteSize - 1] of Integer;
    highest: Integer;
begin
  FillDWord(cntL[0], cTilePaletteSize, DWORD(Low(Integer)));
  FillDWord(cntH[0], cTilePaletteSize, High(Integer));

  for j := 0 to cTileWidth - 1 do
    for i := 0 to cTileWidth - 1 do
      if other xor (odd(i) = not odd(j)) then
      begin
        px := ATile.PalPixels[j, i];

        if cntL[px] = Low(Integer) then
          cntL[px] := 1
        else
          Inc(cntL[px]);

        if cntH[px] = High(Integer) then
          cntH[px] := 1
        else
          Inc(cntH[px]);
      end;

  Result := cTilePaletteSize - 1;

  if MaxIntValue(cntL) = MinIntValue(cntH) then // "lack of significativity" test
    Exit;

  highest := -1;
  for i := 0 to cTilePaletteSize - 1 do
    if cntL[i] >= highest then
    begin
      Result := i;
      highest := cntL[i];
    end;
end;

function TMainForm.WriteTileDatasetLine(const ATile: TTile; var DataLine: TByteDynArray; out PxlAccum: Integer): Integer;
var
  acc, x, y: Integer;
  b: Byte;
begin
  Result := 0;
  acc := 0;
  for y := 0 to cTileWidth - 1 do
    for x := 0 to cTileWidth - 1 do
    begin
      b :=ATile.PalPixels[y, x];
      Inc(acc, b);
      DataLine[Result] := b;
      Inc(Result);
    end;

  //DataLine[Result] := GetTileZoneMedian(ATile, 0, 0, 4, 4);
  //Inc(Result);
  //DataLine[Result] := GetTileZoneMedian(ATile, 4, 0, 4, 4);
  //Inc(Result);
  //DataLine[Result] := GetTileZoneMedian(ATile, 0, 4, 4, 4);
  //Inc(Result);
  //DataLine[Result] := GetTileZoneMedian(ATile, 4, 4, 4, 4);
  //Inc(Result);
  //
  //DataLine[Result] := GetTileZoneMedian(ATile, 0, 0, 8, 8);
  //Inc(Result);
  //
  //DataLine[Result] := GetTileGridMedian(ATile, True);
  //Inc(Result);
  //
  //DataLine[Result] := GetTileGridMedian(ATile, False);
  //Inc(Result);

  PxlAccum := acc;

  // clear remaining bytes
  //FillByte(DataLine[Result], cKModesFeatureCount - Result, 0);
end;

procedure TMainForm.DoGlobalTiling(DesiredNbTiles, RestartCount: Integer);
var
  Dataset, Centroids: TByteDynArray2;
  Clusters: TIntegerDynArray;
  i, j, k, x, y, di, acc, best, StartingPoint, ActualNbTiles: Integer;
  ToMerge: array of Integer;
  WasActive: TBooleanDynArray;
  KModes: TKModes;
  NewTile: TPalPixels;
begin
  SetLength(Dataset, Length(FTiles), cKModesFeatureCount);
  SetLength(WasActive, Length(FTiles));

  // prepare KModes dataset, one line per tile, 64 palette indexes per line
  // also choose KModes starting point

  StartingPoint := -RestartCount; // by default, random starting point
  di := 0;
  best := MaxInt;
  for i := 0 to High(FTiles) do
  begin
    WasActive[i] := FTiles[i]^.Active;

    if not FTiles[i]^.Active then
      Continue;

    WriteTileDatasetLine(FTiles[i]^, Dataset[di], acc);

    if acc <= best then
    begin
      best := acc;
      StartingPoint := di;
    end;

    Inc(di);
  end;

  SetLength(Dataset, di);

  ProgressRedraw(1);

  // run the KModes algorithm, which will group similar tiles until it reaches a fixed amount of groups

  KModes := TKModes.Create(0, 0, True);
  try
    ActualNbTiles := KModes.ComputeKModes(Dataset, DesiredNbTiles, -StartingPoint, cTilePaletteSize, Clusters, Centroids);
    Assert(Length(Centroids) = ActualNbTiles);
    Assert(MaxIntValue(Clusters) = ActualNbTiles - 1);
  finally
    KModes.Free;
  end;

  ProgressRedraw(2);

  // for each group, merge the tiles

  SetLength(ToMerge, Length(FTiles));

  for j := 0 to ActualNbTiles - 1 do
  begin
    di := 0;
    k := 0;
    for i := 0 to High(FTiles) do
    begin
      if not WasActive[i] then
        Continue;

      if Clusters[k] = j then
      begin
        ToMerge[di] := i;
        Inc(di);
      end;

      Inc(k);
    end;

    // recreate a tile from the centroids

    i := 0;
    for y := 0 to cTileWidth - 1 do
      for x := 0 to cTileWidth - 1 do
      begin
        NewTile[y, x] := Centroids[j, i];
        Inc(i);
      end;

    if di >= 2 then
      MergeTiles(ToMerge, di, ToMerge[0], @NewTile)
  end;

  ProgressRedraw(3);
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
  i, x, y, cnt: Integer;
  IdxMap: TIntegerDynArray;
begin
  cnt := 0;
  for i := 0 to High(FTiles) do
  begin
    FTiles[i]^.TmpIndex := i;
    if FTiles[i]^.Active then
      Inc(cnt);
  end;

  // pack the global tiles, removing inactive ones

  for i := High(FTiles) - 1 downto 0 do
    if not FTiles[i]^.Active then
    begin
      Dispose(FTiles[i]);
      Move(FTiles[i + 1], FTiles[i], (Length(FTiles) - 1 - i) * SizeOf(PTile));
    end;

  SetLength(IdxMap, Length(FTiles));
  FillDWord(IdxMap[0], Length(FTiles), $ffffffff);

  // sort global tiles by use count descending (to make smoothing work better) then by tile index (to make tile indexes compression work better)

  SetLength(FTiles, cnt);
  QuickSort(FTiles[0], 0, High(FTiles), SizeOf(PTile), @CompareTileUseCountRev);

  // point tilemap items on new tiles indexes

  for i := 0 to High(FTiles) do
    IdxMap[FTiles[i]^.TmpIndex] := i;

  for i := 0 to High(FFrames) do
    for y := 0 to (cTileMapHeight - 1) do
      for x := 0 to (cTileMapWidth - 1) do
        FFrames[i].TileMap[y,x].GlobalTileIndex := IdxMap[FFrames[i].TileMap[y,x].GlobalTileIndex];
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  col, i, sr: Integer;
  es: TEncoderStep;
begin
  InitializeCriticalSection(FCS);

  FormatSettings.DecimalSeparator := '.';

{$ifdef DEBUG}
  ProcThreadPool.MaxThreadCount := 1;
{$else}
  SetPriorityClass(GetCurrentProcess(), IDLE_PRIORITY_CLASS);
{$endif}

  imgSource.Picture.Bitmap.Width:=cScreenWidth;
  imgSource.Picture.Bitmap.Height:=cScreenHeight;
  imgSource.Picture.Bitmap.PixelFormat:=pf32bit;

  imgDest.Picture.Bitmap.Width:=cScreenWidth;
  imgDest.Picture.Bitmap.Height:=cScreenHeight;
  imgDest.Picture.Bitmap.PixelFormat:=pf32bit;

  imgTiles.Picture.Bitmap.Width:=cScreenWidth div 2;
  imgTiles.Picture.Bitmap.Height:=cScreenHeight;
  imgTiles.Picture.Bitmap.PixelFormat:=pf32bit;

  imgPalette.Picture.Bitmap.Width := cScreenWidth;
  imgPalette.Picture.Bitmap.Height := 16 * cPaletteCount;
  imgPalette.Picture.Bitmap.PixelFormat:=pf32bit;

  imgTiles.Width := cScreenWidth div 2;
  imgTiles.Height := cScreenHeight;
  imgSource.Width := cScreenWidth;
  imgSource.Height := cScreenHeight;
  imgDest.Width := cScreenWidth;
  imgDest.Height := cScreenHeight;
  imgPalette.Height := 16 * cPaletteCount;

  imgTiles.Left := imgSource.Left - imgTiles.Width;
  if imgTiles.Left < 0 then
  begin
    imgDest.Left := imgDest.Left - imgTiles.Left;
    imgSource.Left := imgSource.Left - imgTiles.Left;
    imgTiles.Left := 0;
  end;

  imgDest.Top := imgSource.Top + imgSource.Height;
  imgPalette.Top := imgTiles.Top + imgTiles.Height;

  sedPalIdx.MaxValue := cPaletteCount - 1;

  cbxYilMixChange(nil);
  chkLABChange(nil);

  for es := esLoad to High(TEncoderStep) do
    cbxStep.AddItem(GetEnumName(TypeInfo(TEncoderStep), Ord(es)), TObject(PtrInt(Ord(es))));
  cbxStep.ItemIndex := Ord(es);

  sr := (1 shl cBitsPerComp) - 1;

  for i := 0 to cTotalColors - 1 do
  begin
    col :=
       ((((i shr (cBitsPerComp * 0)) and sr) * 255 div sr) and $ff) or //R
      (((((i shr (cBitsPerComp * 1)) and sr) * 255 div sr) and $ff) shl 8) or //G
      (((((i shr (cBitsPerComp * 2)) and sr) * 255 div sr) and $ff) shl 16);  //B

    FColorMap[i] := col;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  DeleteCriticalSection(FCS);

  for i := 0 to High(FTiles) do
    Dispose(FTiles[i]);
end;

initialization
  InitLuts;
end.

