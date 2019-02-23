unit main;

{$mode objfpc}{$H+}

{$define ASM_DBMP}

interface

uses
  LazLogger, Classes, SysUtils, windows, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, typinfo,
  StdCtrls, ComCtrls, Spin, Menus, Math, types, strutils, kmodes, MTProcs, extern,
  ap, correlation, IntfGraphics, FPimage, FPWritePNG, zstream;

type
  TEncoderStep = (esNone = -1, esLoad = 0, esDither, esMakeUnique, esGlobalTiling, esFrameTiling, esReindex, esSmooth, esSave);

const
  cTileMapWidth = 160;
  cTileMapHeight = 66;
  cPaletteCount = 32;
  cBitsPerComp = 8;
  cTilePaletteSize = 32;
  cRandomKModesCount = 7;
  cGamma: array[0..1{YUV,LAB}] of TFloat = (2.0, 1.0);
  cSmoothingGamma = -1;
  cKFGamma = -1;
  cKFFromPal = True;
  cKFQWeighting = True;

  cRedMultiplier = 299;
  cGreenMultiplier = 587;
  cBlueMultiplier = 114;
  cLumaMultiplier = cRedMultiplier + cGreenMultiplier + cBlueMultiplier;

  cSmoothingPrevFrame = 1;
  cVecInvWidth = 16;
  cTotalColors = 1 shl (cBitsPerComp * 3);
  cTileWidth = 8;
  cTileMapSize = cTileMapWidth * cTileMapHeight;
  cScreenWidth = cTileMapWidth * cTileWidth;
  cScreenHeight = cTileMapHeight * cTileWidth;
  cTileDCTSize = 3 * sqr(cTileWidth);

  cPhi = (1 + sqrt(5)) / 2;
  cInvPhi = 1 / cPhi;

  cUV = 10;
  cDCTQuantization: array[0..2{YUV}, 0..7, 0..7] of TFloat = (
    (
      // optimized
      (16, 11, 12, 15,  21,  32,  50,  66),
      (11, 12, 13, 18,  24,  46,  62,  73),
      (12, 13, 16, 23,  38,  56,  73,  75),
      (15, 18, 23, 29,  53,  75,  83,  80),
      (21, 24, 38, 53,  68,  95, 103,  94),
      (32, 46, 56, 75,  95, 104, 117,  96),
      (50, 62, 73, 83, 103, 117, 120, 128),
      (66, 73, 75, 80,  94,  96, 128, 144)
    ),
    (
      // Improved (reduced high frequency chroma importance)
      (17*cUV,  18*cUV,  24*cUV,  47*cUV,  99*cUV,  99*cUV,  99*cUV,  99*cUV),
      (18*cUV,  21*cUV,  26*cUV,  66*cUV,  99*cUV,  99*cUV,  99*cUV, 112*cUV),
      (24*cUV,  26*cUV,  56*cUV,  99*cUV,  99*cUV,  99*cUV, 112*cUV, 128*cUV),
      (47*cUV,  66*cUV,  99*cUV,  99*cUV,  99*cUV, 112*cUV, 128*cUV, 144*cUV),
      (99*cUV,  99*cUV,  99*cUV,  99*cUV, 112*cUV, 128*cUV, 144*cUV, 160*cUV),
      (99*cUV,  99*cUV,  99*cUV, 112*cUV, 128*cUV, 144*cUV, 160*cUV, 176*cUV),
      (99*cUV,  99*cUV, 112*cUV, 128*cUV, 144*cUV, 160*cUV, 176*cUV, 192*cUV),
      (99*cUV, 112*cUV, 128*cUV, 144*cUV, 160*cUV, 176*cUV, 192*cUV, 208*cUV)
    ),
    (
      // Improved (reduced high frequency chroma importance)
      (17*cUV,  18*cUV,  24*cUV,  47*cUV,  99*cUV,  99*cUV,  99*cUV,  99*cUV),
      (18*cUV,  21*cUV,  26*cUV,  66*cUV,  99*cUV,  99*cUV,  99*cUV, 112*cUV),
      (24*cUV,  26*cUV,  56*cUV,  99*cUV,  99*cUV,  99*cUV, 112*cUV, 128*cUV),
      (47*cUV,  66*cUV,  99*cUV,  99*cUV,  99*cUV, 112*cUV, 128*cUV, 144*cUV),
      (99*cUV,  99*cUV,  99*cUV,  99*cUV, 112*cUV, 128*cUV, 144*cUV, 160*cUV),
      (99*cUV,  99*cUV,  99*cUV, 112*cUV, 128*cUV, 144*cUV, 160*cUV, 176*cUV),
      (99*cUV,  99*cUV, 112*cUV, 128*cUV, 144*cUV, 160*cUV, 176*cUV, 192*cUV),
      (99*cUV, 112*cUV, 128*cUV, 144*cUV, 160*cUV, 176*cUV, 192*cUV, 208*cUV)
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
    Tags: TInteger1DArray;
    Dataset: TFloatDynArray2;
    TRToTileIdx: TIntegerDynArray;
    TRToPalIdx: TByteDynArray;
    KDT: PANNkdtree;
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
    FramesLeft: Integer;
    CS: TRTLCriticalSection;
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
    chkHSL: TCheckBox;
    chkTransPalette: TCheckBox;
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
    Label13: TLabel;
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
    seStartFrame: TSpinEdit;
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
    procedure btnDoFrameTilingClick(Sender: TObject);
    procedure btnReindexClick(Sender: TObject);
    procedure btnSmoothClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);

    procedure btnRunAllClick(Sender: TObject);
    procedure btnDebugClick(Sender: TObject);
    procedure cbxYilMixChange(Sender: TObject);
    procedure chkHSLChange(Sender: TObject);
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
    FUseHSL: Boolean;
    FY2MixedColors: Integer;
    FProgressStep: TEncoderStep;
    FProgressPosition, FOldProgressPosition, FProgressStartTime, FProgressPrevTime: Integer;

    FCS: TRTLCriticalSection;

    function ComputeCorrelation(a: TIntegerDynArray; b: TIntegerDynArray): TFloat;

    procedure LoadFrame(AFrame: PFrame; ABitmap: TBitmap);
    procedure ClearAll;
    procedure ProgressRedraw(CurFrameIdx: Integer = -1; ProgressStep: TEncoderStep = esNone);
    procedure Render(AFrameIndex: Integer; playing, dithered, mirrored, reduced, gamma: Boolean; palIdx: Integer;
      ATilePage: Integer);

    function HSLToRGB(h, s, l: Byte): Integer;
    procedure RGBToHSL(col: Integer; out h, s, l: Byte); overload;
    procedure RGBToHSL(col: Integer; out h, s, l: TFloat); overload;
    procedure RGBToYUV(col: Integer; GammaCor: Integer; out y, u, v: TFloat);

    procedure ComputeTileDCT(const ATile: TTile; FromPal, QWeighting, HSL, HMirror, VMirror: Boolean; GammaCor: Integer;
      const pal: array of Integer; var DCT: TFloatDynArray);

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

    function GetTileZoneMedian(const ATile: TTile; x, y, w, h: Integer; out UseCount: Integer): Integer;
    function GetTileGridMedian(const ATile: TTile; other: Boolean; out UseCount: Integer): Integer;
    procedure MakeTilesUnique(FirstTileIndex, TileCount: Integer);
    procedure MergeTiles(const TileIndexes: array of Integer; TileCount: Integer; BestIdx: Integer; NewTile: PPalPixels);
    function WriteTileDatasetLine(const ATile: TTile; DataLine: TByteDynArray; out PxlAccum: Integer): Integer;
    procedure DoGlobalTiling(DesiredNbTiles, RestartCount: Integer);

    function GoldenRatioSearch(Func: TFloatFloatFunction; Mini, Maxi: TFloat; ObjectiveY: TFloat = 0.0; Epsilon: TFloat = 1e-12; Data: Pointer = nil): TFloat;
    procedure HMirrorPalTile(var ATile: TTile);
    procedure VMirrorPalTile(var ATile: TTile);
    function GetMaxTPF(AKF: PKeyFrame): Integer;
    procedure PrepareFrameTiling(AKF: PKeyFrame; TransPalette: Boolean);
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
  gPalettePattern : array[0 .. cPaletteCount - 1, 0 .. cTilePaletteSize - 1] of TFloat;

procedure InitLuts;
var
  g, i, j, v, u, y, x: Int64;
  f, fp: TFloat;
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

  for j := 0 to cPaletteCount - 1 do
  begin
    x := 1;
    v := 1;
    u := 0;
    f := 0;
    for i := 0 to cTilePaletteSize do
    begin
      x := u + v;
      u := v;
      v := x;

      fp := f;
      f := sqrt(x) - 1.0;

      if i > 0 then
        gPalettePattern[j, i - 1] := ((j + 1) / cPaletteCount) * (f - fp) + fp;
    end;
  end;

  for j := 0 to cPaletteCount - 1 do
    for i := 0 to cTilePaletteSize - 1 do
      gPalettePattern[j, i] /= gPalettePattern[cPaletteCount - 1, cTilePaletteSize - 1];
end;

function GammaCorrect(lut: Integer; x: Byte): TFloat; inline;
begin
  Result := gGammaCorLut[lut, x];
end;

function lerp(x, y, alpha: TFloat): TFloat; inline;
begin
  Result := x + (y - x) * alpha;
end;

function revlerp(x, y, alpha: TFloat): TFloat; inline;
begin
  Result := (alpha - x) / (y - x);
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
  cTilesAtATime = 12 * cTileMapSize;

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

procedure TMainForm.btnDoFrameTilingClick(Sender: TObject);

  procedure DoFrm(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    DoFrameTiling(@FFrames[AIndex]);
  end;

  procedure DoKF(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    PrepareFrameTiling(@FKeyFrames[AIndex], chkTransPalette.Checked);
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
    FreeMemAndNil(FKeyFrames[i].TileDS);

  tbFrameChange(nil);
end;

procedure TMainForm.chkHSLChange(Sender: TObject);
begin
  FUseHSL := chkHSL.Checked;
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
      bmp.LoadFromFile(Format(inPath, [AIndex + PtrInt(AData)]));
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
  kfCnt, frc, startFrame: Integer;
  isKf: Boolean;
  kfSL: TStringList;
  sfr, efr: Integer;
begin
  ProgressRedraw;

  ClearAll;

  ProgressRedraw(-1, esLoad);

  inPath := edInput.Text;
  startFrame := seStartFrame.Value;
  frc := seFrameCount.Value;

  if frc <= 0 then
  begin
    if Pos('%', inPath) > 0 then
    begin
      i := 0;
      repeat
        fn := Format(inPath, [i + startFrame]);
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
    fn := Format(inPath, [i + startFrame]);
    if not FileExists(fn) then
    begin
      SetLength(FFrames, 0);
      tbFrame.Max := 0;
      raise EFileNotFoundException.Create('File not found: ' + fn);
    end;
  end;

  ProcThreadPool.DoParallelLocalProc(@DoLoadFrame, 0, High(FFrames), Pointer(startFrame));

  kfSL := TStringList.Create;
  try
    fn := ChangeFileExt(Format(inPath, [0]), '.kf');
    if FileExists(fn) then
    begin
      kfSL.LoadFromFile(fn);
      kfSL.Insert(0, 'I'); // fix format shifted 1 frame in the past
      for i := 0 to startFrame - 1 do
        kfSL.Delete(0);
    end;

    kfCnt := 0;
    for i := 0 to High(FFrames) do
    begin
      fn := ChangeFileExt(Format(inPath, [i + startFrame]), '.kf');
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
      fn := ChangeFileExt(Format(inPath, [i + startFrame]), '.kf');
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
    FKeyFrames[j].FramesLeft := -1;
    InitializeCriticalSection(FKeyFrames[j].CS);
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
    btnDoFrameTilingClick(nil);

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
  i, j: Integer;
  seed: Cardinal;
  pal: array[0..15] of Integer;
  list: TByteDynArray;
  plan: TMixingPlan;

  hh,ss,ll: Byte;
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


  for i := 0 to 255 do
    for j := 0 to 255 do
    begin
      hh := 0; ss := 0; ll := 0;
      RGBToHSL(HSLToRGB(i,j,255), hh, ss, ll);
      imgDest.Canvas.Pixels[i,j] := SwapRB(HSLToRGB(hh,ss,ll));
    end;
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
  palPict.PixelFormat := pf24bit;

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
    VK_F6: btnDoFrameTilingClick(nil);
    VK_F7: btnReindexClick(nil);
    VK_F8: btnSmoothClick(nil);
    VK_F9: btnSaveClick(nil);
    VK_F10: btnRunAllClick(nil);
    VK_F11: chkPlay.Checked := not chkPlay.Checked;
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

  //DebugLn(['X: ', FormatFloat('0.000', x), #9'Y: ', FormatFloat('0.000', y), #9'Mini: ', FormatFloat('0.000', Mini), #9'Maxi: ', FormatFloat('0.000', Maxi)]);

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

  SetLength(Plan.CountCache, 1 shl 24);
  SetLength(Plan.ListCache, 1 shl 24);
  FillByte(Plan.CountCache[0], 1 shl 24, $ff);

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
  Result := diffR * diffR * cRedMultiplier div 2; // div 2 for 0.5 chroma importance reduction
  Result += diffG * diffG * cGreenMultiplier div 2;
  Result += diffB * diffB * cBlueMultiplier div 2;
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
  if Plan.CountCache[col] < $80 then
  begin
    Result := Plan.CountCache[col];
    Move(Plan.ListCache[col, 0], List[0], Result);
    Exit;
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

    mov rax, (cRedMultiplier * 2 / 16) or ((cGreenMultiplier * 2 / 16) shl 32)
    pinsrq xmm6, rax, 0
    mov rax, (cBlueMultiplier * 2 / 16) or ((cLumaMultiplier * 4 / 16) shl 32)
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
  if Plan.CountCache[col] >= $80 then
  begin
    Plan.ListCache[col] := Copy(List, 0, Result);
    ReadWriteBarrier;
    Plan.CountCache[col] := Result;
  end;
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
  TCountIndexArray = packed record
    Count, Index: Integer;
    Hue, Sat, Lit, Dummy: Byte;
  end;

  PCountIndexArray = ^TCountIndexArray;


function CompareCMUCntHSL(Item1,Item2:Pointer):Integer;
begin
  Result := PCountIndexArray(Item2)^.Count - PCountIndexArray(Item1)^.Count;
  if Result = 0 then
    Result := CompareValue(PCountIndexArray(Item1)^.Hue, PCountIndexArray(Item2)^.Hue);
  if Result = 0 then
    Result := CompareValue(PCountIndexArray(Item1)^.Lit, PCountIndexArray(Item2)^.Lit);
  if Result = 0 then
    Result := CompareValue(PCountIndexArray(Item1)^.Sat, PCountIndexArray(Item2)^.Sat);
end;

function CompareCMUHSL(Item1,Item2:Pointer):Integer;
begin
  Result := CompareValue(PCountIndexArray(Item1)^.Lit, PCountIndexArray(Item2)^.Lit);
  if Result = 0 then
    Result := CompareValue(PCountIndexArray(Item1)^.Hue, PCountIndexArray(Item2)^.Hue);
  if Result = 0 then
    Result := CompareValue(PCountIndexArray(Item1)^.Sat, PCountIndexArray(Item2)^.Sat);
end;

procedure TMainForm.FindBestKeyframePalette(AKeyFrame: PKeyFrame; ColorReach: TFloat);
{$if cBitsPerComp <> 8}
const
  cRGShift = (8 - cBitsPerComp);
  cBShift = (8 - cBitsPerComp) * 2;
  cRMask = ((1 shl cBitsPerComp) - 1);
  cGMask = ((1 shl cBitsPerComp) - 1) shl 8;
  cBMask = ((1 shl cBitsPerComp) - 1) shl 16;
{$endif}
var
  col, sx, sy, tx, ty, i, PalIdx, LastUsed, CmlPct, acc: Integer;
  GTile: PTile;
  CMUsage, CMPal: TList;
  CMItem: PCountIndexArray;
{$if cBitsPerComp <> 8}
  cnt: Integer;
  TrueColorUsage: TCardinalDynArray;
{$endif}
  CmlReach: TFloat;
begin
  Assert(cPaletteCount <= Length(gPalettePattern));

{$if cBitsPerComp <> 8}
  SetLength(TrueColorUsage, 1 shl (24 - cRGShift));
  FillDWord(TrueColorUsage[0], Length(TrueColorUsage), 0);
{$endif}

  CMUsage := TList.Create;
  CMPal := TList.Create;
  try
    CMUsage.Count := cTotalColors;
    for i := 0 to cTotalColors - 1 do
    begin
      New(CMItem);
      CMItem^.Count := 0;
      CMItem^.Index := i;
      RGBToHSL(FColorMap[i], CMItem^.Hue, CMItem^.Sat, CMItem^.Lit);
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
{$if cBitsPerComp <> 8}
              col := col shr cRGShift;
              Inc(TrueColorUsage[col]);
{$else}
              Inc(PCountIndexArray(CMUsage[col])^.Count);
{$endif}
            end;
        end;
    end;

{$if cBitsPerComp <> 8}
    for i := 0 to High(TrueColorUsage) do
    begin
      cnt := TrueColorUsage[i];

      if cnt = 0 then
        Continue;

      col := (i and cRMask) or ((i and cGMask) shr cRGShift) or ((i and cBMask) shr cBShift);
      Inc(PCountIndexArray(CMUsage[col])^.Count, cnt);
    end;
{$endif}

    // sort colors by use count

    CMUsage.Sort(@CompareCMUCntHSL);

    LastUsed := -1;
    for i := cTotalColors - 1 downto 0 do    //TODO: rev algo
      if PCountIndexArray(CMUsage[i])^.Count <> 0 then
      begin
        LastUsed := i;
        Break;
      end;

    CmlPct := 0;
    acc := AKeyFrame^.FrameCount * cTileMapSize * sqr(cTileWidth);
    acc := acc - (acc div 20); // 95% point
    for i := 0 to cTotalColors - 1 do
    begin
      acc -= PCountIndexArray(CMUsage[i])^.Count;
      if acc <= 0 then
      begin
        CmlPct := i;
        Break;
      end;
    end;

    DebugLn(['KF: ', AKeyFrame^.StartFrame, #9'LastUsed: ', LastUsed, #9'CmlPct: ', CmlPct]);

    // after LastUsed put 'weird' colors (ie. colors not in frames, but relevant for Y2)

    for i := LastUsed + 1 to min(cTotalColors, LastUsed + (LastUsed shr 1)) - 1 do
    begin
      CMItem := PCountIndexArray(CMUsage[i]);
      CMItem^ := PCountIndexArray(CMUsage[EnsureRange((i - LastUsed - 1) shl 1, 0, cTotalColors - 1)])^;
      CMItem^.Count := 0;
      col := HSLToRGB(CMItem^.Hue, 255 - CMItem^.Sat, CMItem^.Lit);
{$if cBitsPerComp <> 8}
      col := col shr cRGShift;
      col := (col and cRMask) or ((col and cGMask) shr cRGShift) or ((col and cBMask) shr cBShift);
{$endif}
      CMItem^.Index := col;
    end;

    // split most used colors into tile palettes

    CmlReach := EnsureRange((CmlPct + 1) * ColorReach, 0, cTotalColors - 1);
    for PalIdx := 0 to cPaletteCount - 1 do
    begin
      CMPal.Clear;
      for i := 0 to cTilePaletteSize - 1 do
        CMPal.Add(CMUsage[Round(gPalettePattern[PalIdx, i] * CmlReach)]);

      CMPal.Sort(@CompareCMUHSL);

      SetLength(AKeyFrame^.PaletteIndexes[PalIdx], cTilePaletteSize);
      SetLength(AKeyFrame^.PaletteRGB[PalIdx], cTilePaletteSize);
      for i := 0 to cTilePaletteSize - 1 do
      begin
        AKeyFrame^.PaletteIndexes[PalIdx, i] := PCountIndexArray(CMPal[i])^.Index;
        AKeyFrame^.PaletteRGB[PalIdx, i] := FColorMap[AKeyFrame^.PaletteIndexes[PalIdx, i]];
      end;
    end;

    for i := 0 to CMUsage.Count - 1 do
      Dispose(PCountIndexArray(CMUsage[i]));

  finally
    CMPal.Free;
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
  EnterCriticalSection(AFrame^.KeyFrame^.CS);
  if AFrame^.KeyFrame^.FramesLeft < 0 then
  begin
    for i := 0 to cPaletteCount - 1 do
      PreparePlan(AFrame^.KeyFrame^.MixingPlans[i], FY2MixedColors, AFrame^.KeyFrame^.PaletteRGB[i]);
    AFrame^.KeyFrame^.FramesLeft := AFrame^.KeyFrame^.FrameCount;
  end;
  LeaveCriticalSection(AFrame^.KeyFrame^.CS);

  SetLength(OrigTileDCT, cTileDCTSize);
  SetLength(TileDCT, cTileDCTSize);

  for sy := 0 to cTileMapHeight - 1 do
    for sx := 0 to cTileMapWidth - 1 do
    begin
      OrigTile := FTiles[AFrame^.TileMap[sy, sx].GlobalTileIndex];

      if not OrigTile^.Active then
        Exit;

      // choose best palette from the keyframe by comparing DCT of the tile colored with either palette

      ComputeTileDCT(OrigTile^, False, True, False, False, False, -1, OrigTile^.PaletteRGB, OrigTileDCT);

      PalIdx := -1;
      best := MaxDouble;
      for i := 0 to cPaletteCount - 1 do
      begin
        DitherTile(OrigTile^, AFrame^.KeyFrame^.MixingPlans[i]);
        ComputeTileDCT(OrigTile^, True, True, False, False, False, -1, AFrame^.KeyFrame^.PaletteRGB[i], TileDCT);
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

  EnterCriticalSection(AFrame^.KeyFrame^.CS);
  Dec(AFrame^.KeyFrame^.FramesLeft);
  if AFrame^.KeyFrame^.FramesLeft <= 0 then
    for i := 0 to cPaletteCount - 1 do
      TerminatePlan(AFrame^.KeyFrame^.MixingPlans[i]);
  LeaveCriticalSection(AFrame^.KeyFrame^.CS);
end;

function CompareTilePalPixels(Item1, Item2:Pointer):Integer;
var
  t1, t2: PTile;
begin
  t1 := PTile(Item1);
  t2 := PTile(Item2);
  Result := CompareDWord(t1^.PalPixels[0, 0], t2^.PalPixels[0, 0], sqr(cTileWidth) div SizeOf(DWORD));
end;

procedure TMainForm.MakeTilesUnique(FirstTileIndex, TileCount: Integer);
var
  i, firstSameIdx: Integer;
  sortList: TList;
  sameIdx: array of Integer;

  procedure DoOneMerge;
  var
    j: Integer;
  begin
    if i - firstSameIdx >= 2 then
    begin
      for j := firstSameIdx to i - 1 do
        sameIdx[j - firstSameIdx] := PTile(sortList[j])^.TmpIndex;
      MergeTiles(sameIdx, i - firstSameIdx, sameIdx[0], nil);
    end;
    firstSameIdx := i;
  end;

begin
  sortList := TList.Create;
  try

    // sort global tiles by palette indexes (L to R, T to B)

    SetLength(sameIdx, TileCount);

    sortList.Count := TileCount;
    for i := 0 to TileCount - 1 do
    begin
      sortList[i] := FTiles[i + FirstTileIndex];
      PTile(sortList[i])^.TmpIndex := i + FirstTileIndex;
    end;

    sortList.Sort(@CompareTilePalPixels);

    // merge exactly similar tiles (so, consecutive after prev code)

    firstSameIdx := 0;
    for i := 1 to sortList.Count - 1 do
      if CompareDWord(PTile(sortList[i - 1])^.PalPixels[0, 0], PTile(sortList[i])^.PalPixels[0, 0], sqr(cTileWidth) div SizeOf(DWORD)) <> 0 then
        DoOneMerge;

    i := sortList.Count - 1;
    DoOneMerge;

  finally
    sortList.Free;
  end;
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

procedure TMainForm.RGBToYUV(col: Integer; GammaCor: Integer; out y, u, v: TFloat); inline;
var
  fr, fg, fb: TFloat;
  yy, uu, vv: TFloat;
  r, g, b: Integer;
begin
  FromRGB(col, r, g, b);

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

  yy :=  0.299*fr + 0.587*fg + 0.114*fb;
  uu := -0.147*fr - 0.289*fg + 0.436*fb;
  vv :=  0.615*fr - 0.515*fg - 0.100*fb;

  y := yy; u := uu; v := vv; // for safe "out" param
end;

procedure TMainForm.RGBToHSL(col: Integer; out h, s, l: TFloat);
var
  bh, bs, bl: Byte;
begin
  bh := 0; bs := 0; bl := 0;
  RGBToHSL(col, bh, bs, bl);
  h := bh / 255.0;
  s := bs / 255.0;
  l := bl / 255.0;
end;

procedure TMainForm.ComputeTileDCT(const ATile: TTile; FromPal, QWeighting, HSL, HMirror, VMirror: Boolean;
  GammaCor: Integer; const pal: array of Integer; var DCT: TFloatDynArray);
const
  cUVRatio: array[0..cTileWidth-1] of TFloat = (sqrt(0.5), 1, 1, 1, 1, 1, 1, 1);
var
  u, v, x, y, xx, yy, di, cpn: Integer;
  vRatio, z: TFloat;
  CpnPixels: array[0..2, 0..cTileWidth-1,0..cTileWidth-1] of TFloat;
  pYuv, pLut: PFloat;
begin
  Assert(Length(DCT) >= cTileDCTSize, 'DCT too small!');

  if FromPal then
  begin
    if HSL then
    begin
      for y := 0 to (cTileWidth - 1) do
        for x := 0 to (cTileWidth - 1) do
        begin
          xx := x;
          yy := y;
          if HMirror then xx := cTileWidth - 1 - x;
          if VMirror then yy := cTileWidth - 1 - y;
          RGBToHSL(pal[ATile.PalPixels[yy,xx]], CpnPixels[2,y,x], CpnPixels[1,y,x], CpnPixels[0,y,x])
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
          RGBToYUV(pal[ATile.PalPixels[yy,xx]], GammaCor, CpnPixels[0,y,x], CpnPixels[1,y,x], CpnPixels[2,y,x]);
        end;
    end;
  end
  else
  begin
    if HSL then
    begin
      for y := 0 to (cTileWidth - 1) do
        for x := 0 to (cTileWidth - 1) do
        begin
          xx := x;
          yy := y;
          if HMirror then xx := cTileWidth - 1 - x;
          if VMirror then yy := cTileWidth - 1 - y;
          RGBToHSL(ATile.RGBPixels[yy,xx], CpnPixels[2,y,x], CpnPixels[1,y,x], CpnPixels[0,y,x])
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
          RGBToYUV(ATile.RGBPixels[yy,xx], GammaCor, CpnPixels[0,y,x], CpnPixels[1,y,x], CpnPixels[2,y,x]);
        end;
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
          pYuv := @CpnPixels[cpn, y, 0];
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
           DCT[di] *= 16.0 / sqrt(cDCTQuantization[ifthen(not HSL, cpn), v, u]);

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

procedure TMainForm.ClearAll;
var
  i: Integer;
begin
  for i := 0 to High(FTiles) do
    Dispose(FTiles[i]);

  SetLength(FFrames, 0);

  for i := 0 to High(FKeyFrames) do
    DeleteCriticalSection(FKeyFrames[i].CS);

  SetLength(FKeyFrames, 0);
  SetLength(FTiles, 0);
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

  mirrored := mirrored and reduced;

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
    for j := 0 to imgPalette.Picture.Bitmap.Height - 1 do
    begin
      p := imgPalette.Picture.Bitmap.ScanLine[j];
      for i := 0 to imgPalette.Picture.Bitmap.Width - 1 do
      begin
        if Assigned(Frame^.KeyFrame^.PaletteRGB[j]) then
          p^ := SwapRB(Frame^.KeyFrame^.PaletteRGB[j, i])
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
    for j := 0 to cScreenHeight - 1 do
    begin
      Move(PInteger(imgSource.Picture.Bitmap.ScanLine[j])^, oriCorr[j * cScreenWidth], cScreenWidth * SizeOf(Integer));
      Move(PInteger(imgDest.Picture.Bitmap.ScanLine[j])^, chgCorr[j * cScreenWidth], cScreenWidth * SizeOf(Integer));
    end;

    for j := 0 to cScreenHeight - 1 do
      for i := 0 to cScreenWidth - 1 do
      begin
        oriCorr[cScreenWidth * cScreenHeight + i * cScreenHeight + j] := oriCorr[j * cScreenWidth + i];
        chgCorr[cScreenWidth * cScreenHeight + i * cScreenHeight + j] := chgCorr[j * cScreenWidth + i];
      end;

    lblCorrel.Caption := FormatFloat('0.0000000', ComputeCorrelation(oriCorr, chgCorr));
  end;
end;

// from https://www.delphipraxis.net/157099-fast-integer-rgb-hsl.html
procedure TMainForm.RGBToHSL(col: Integer; out h, s, l: Byte);
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

  h := hh;
  s := ss;
  l := ll;
end;

function TMainForm.HSLToRGB(h, s, l: Byte): Integer;
const
  MaxHue: Integer = 252;
  MaxSat: Integer = 255;
  MaxLum: Integer = 255;
  Divisor: Integer = 42;
var
 f, LS, p, q, r: integer;
begin
 if (s = 0) then
   Result := ToRGB(l, l, l)
 else
  begin
   h := h mod MaxHue;
   s := EnsureRange(s, 0, MaxSat);
   l := EnsureRange(l, 0, MaxLum);

   f := h mod Divisor;
   h := h div Divisor;
   LS := l*s;
   p := l - LS div MaxLum;
   q := l - (LS*f) div (255 * Divisor);
   r := l - (LS*(Divisor - f)) div (255 * Divisor);
   case h of
    0: Result := ToRGB(l, r, p);
    1: Result := ToRGB(q, l, p);
    2: Result := ToRGB(p, l, r);
    3: Result := ToRGB(p, q, l);
    4: Result := ToRGB(r, p, l);
    5: Result := ToRGB(l, p, q);
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

procedure TMainForm.PrepareFrameTiling(AKF: PKeyFrame; TransPalette: Boolean);
var
  TRSize, di, i, frame, sy, sx: Integer;
  frm: PFrame;
  T: PTile;
  DS: PTileDataset;
  used: array[0 .. cPaletteCount - 1] of TBooleanDynArray;
  vmir, hmir: Boolean;
  palIdx: Integer;
begin
  DS := New(PTileDataset);
  AKF^.TileDS := DS;

  for palIdx := 0 to cPaletteCount - 1 do
  begin
    SetLength(used[palIdx], Length(FTiles));
    FillByte(used[palIdx, 0], Length(FTiles), 0);
  end;

  for frame := 0 to AKF^.FrameCount - 1 do
  begin
    frm := @FFrames[AKF^.StartFrame + frame];

    if TransPalette then
    begin
      for palIdx := 0 to cPaletteCount - 1 do
        for sy := 0 to cTileMapHeight - 1 do
          for sx := 0 to cTileMapWidth - 1 do
            used[palIdx, frm^.TileMap[sy, sx].GlobalTileIndex] := True;
    end
    else
    begin
      for sy := 0 to cTileMapHeight - 1 do
        for sx := 0 to cTileMapWidth - 1 do
          used[frm^.TileMap[sy, sx].PalIdx, frm^.TileMap[sy, sx].GlobalTileIndex] := True;
    end;
  end;

  TRSize := 0;
  for palIdx := 0 to cPaletteCount - 1 do
    for i := 0 to High(FTiles) do
      TRSize += Ord(used[palIdx, i]);

  SetLength(DS^.TRToTileIdx, TRSize);
  SetLength(DS^.TRToPalIdx, TRSize);
  SetLength(DS^.Dataset, TRSize * 4, cTileDCTSize);

  di := 0;
  for i := 0 to High(FTiles) do
  begin
    T := FTiles[i];

    for palIdx := 0 to cPaletteCount - 1 do
      if used[palIdx, i] then
      begin
        DS^.TRToTileIdx[di shr 2] := i;
        DS^.TRToPalIdx[di shr 2] := palIdx;

        for vmir := False to True do
          for hmir := False to True do
          begin
            ComputeTileDCT(T^, True, cKFQWeighting, FUseHSL, hmir, vmir, cKFGamma, AKF^.PaletteRGB[palIdx], DS^.Dataset[di]);
            Inc(di);
          end;
      end;
  end;

  assert(di = TRSize * 4);

  DebugLn(['KF: ', AKF^.StartFrame, #9'TRSize: ', TRSize, #9'DSSize: ', Length(DS^.Dataset)]);

  SetLength(DS^.Tags, di);
  for i := 0 to High(DS^.Tags) do
    DS^.Tags[i] := i;

  AKF^.FramesLeft := -1;
end;

procedure TMainForm.DoFrameTiling(AFrame: PFrame);
var
  sy, sx: Integer;
  DS: PTileDataset;
  tmiO: PTileMapItem;

  TPF, MaxTPF, i, tri: Integer;
  Used: TBooleanDynArray;
  DCT: TFloatDynArray;
begin
  DS := AFrame^.KeyFrame^.TileDS;

  EnterCriticalSection(AFrame^.KeyFrame^.CS);
  if AFrame^.KeyFrame^.FramesLeft < 0 then
  begin
    DS^.KDT := ann_kdtree_create(PPFloat(DS^.Dataset), Length(DS^.Dataset), cTileDCTSize, 1, ANN_KD_STD);
    AFrame^.KeyFrame^.FramesLeft := AFrame^.KeyFrame^.FrameCount;
  end;
  LeaveCriticalSection(AFrame^.KeyFrame^.CS);

  // map frame tilemap items to reduced tiles and mirrors and choose best corresponding palette

  SetLength(Used, Length(FTiles));
  SetLength(DCT, cTileDCTSize);

  MaxTPF := 0;
  FillChar(Used[0], Length(FTiles) * SizeOf(Boolean), 0);

  for sy := 0 to cTileMapHeight - 1 do
    for sx := 0 to cTileMapWidth - 1 do
    begin
      ComputeTileDCT(AFrame^.Tiles[sy * cTileMapWidth + sx], cKFFromPal, cKFQWeighting, FUseHSL, False, False, cKFGamma, AFrame^.Tiles[sy * cTileMapWidth + sx].PaletteRGB, DCT);

      tri := ann_kdtree_search(DS^.KDT, PFloat(DCT), 0.0);

      tmiO := @FFrames[AFrame^.Index].TileMap[sy, sx];

      tmiO^.GlobalTileIndex := DS^.TRToTileIdx[tri shr 2];
      tmiO^.PalIdx :=  DS^.TRToPalIdx[tri shr 2];
      tmiO^.HMirror := (tri and 1) <> 0;
      tmiO^.VMirror := (tri and 2) <> 0;

      Used[tmiO^.GlobalTileIndex] := True;
    end;

  TPF := 0;
  for i := 0 to High(Used) do
    Inc(TPF, Ord(Used[i]));

  MaxTPF := max(MaxTPF, TPF);

  EnterCriticalSection(AFrame^.KeyFrame^.CS);
  Dec(AFrame^.KeyFrame^.FramesLeft);
  if AFrame^.KeyFrame^.FramesLeft <= 0 then
  begin
    ann_kdtree_destroy(DS^.KDT);
    DS^.KDT := nil;
  end;
  LeaveCriticalSection(AFrame^.KeyFrame^.CS);

  DebugLn(['KF: ', AFrame^.Index, #9'MaxTPF: ', MaxTPF, #9'TileCnt: ', Length(DS^.Dataset), #9'FramesLeft: ', AFrame^.KeyFrame^.FramesLeft]);
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
  SetLength(PrevTileDCT, cTileDCTSize);
  SetLength(TileDCT, cTileDCTSize);

  for sx := 0 to cTileMapWidth - 1 do
  begin
    PrevTMI := @APrevFrame^.TileMap[Y, sx];
    TMI := @AFrame^.TileMap[Y, sx];

    // compare DCT of current tile with tile from prev frame tilemap

    PrevTile := FTiles[PrevTMI^.GlobalTileIndex]^;
    Tile_ := FTiles[TMI^.GlobalTileIndex]^;

    ComputeTileDCT(PrevTile, True, True, FUseHSL, PrevTMI^.HMirror, PrevTMI^.VMirror, cSmoothingGamma, AFrame^.KeyFrame^.PaletteRGB[PrevTMI^.PalIdx], PrevTileDCT);
    ComputeTileDCT(Tile_, True, True, FUseHSL, TMI^.HMirror, TMI^.VMirror, cSmoothingGamma, AFrame^.KeyFrame^.PaletteRGB[TMI^.PalIdx], TileDCT);

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

function TMainForm.GetTileZoneMedian(const ATile: TTile; x, y, w, h: Integer; out UseCount: Integer): Integer;
var uc, i, j: Integer;
    px: Byte;
    cnt: array [0..cTilePaletteSize - 1] of Integer;
    highest: Integer;
begin
  FillDWord(cnt[0], cTilePaletteSize, DWORD(Low(Integer)));

  for j := y to y + h - 1 do
    for i := x to x + w - 1 do
    begin
      px := ATile.PalPixels[j, i];

      if cnt[px] = Low(Integer) then
        cnt[px] := 1
      else
        Inc(cnt[px]);
    end;

  Result := 0;
  highest := -1;
  uc := 0;
  for i := 0 to cTilePaletteSize - 1 do
    if cnt[i] >= highest then
    begin
      Result := i;
      highest := cnt[i];
      Inc(uc, Ord(cnt[i] <> 0));
    end;

  UseCount := uc;
end;

function TMainForm.GetTileGridMedian(const ATile: TTile; other: Boolean; out UseCount: Integer): Integer;
var uc, i, j: Integer;
    px: Byte;
    cnt: array [0..cTilePaletteSize - 1] of Integer;
    highest: Integer;
begin
  FillDWord(cnt[0], cTilePaletteSize, DWORD(Low(Integer)));

  for j := 0 to cTileWidth - 1 do
    for i := 0 to cTileWidth - 1 do
      if other xor (odd(i) = not odd(j)) then
      begin
        px := ATile.PalPixels[j, i];

        if cnt[px] = Low(Integer) then
          cnt[px] := 1
        else
          Inc(cnt[px]);
      end;

  Result := 0;
  highest := -1;
  uc := 0;
  for i := 0 to cTilePaletteSize - 1 do
    if cnt[i] >= highest then
    begin
      Result := i;
      highest := cnt[i];
      Inc(uc, Ord(cnt[i] <> 0));
    end;

  UseCount := uc;
end;

function TMainForm.WriteTileDatasetLine(const ATile: TTile; DataLine: TByteDynArray; out PxlAccum: Integer): Integer;
var
  acc, x, y, UseCount: Integer;
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

  DataLine[Result] := GetTileZoneMedian(ATile, 0, 0, 4, 4, UseCount);
  Inc(Result);
  DataLine[Result] := UseCount;
  Inc(Result);

  DataLine[Result] := GetTileZoneMedian(ATile, 4, 0, 4, 4, UseCount);
  Inc(Result);
  DataLine[Result] := UseCount;
  Inc(Result);

  DataLine[Result] := GetTileZoneMedian(ATile, 0, 4, 4, 4, UseCount);
  Inc(Result);
  DataLine[Result] := UseCount;
  Inc(Result);

  DataLine[Result] := GetTileZoneMedian(ATile, 4, 4, 4, 4, UseCount);
  Inc(Result);
  DataLine[Result] := UseCount;
  Inc(Result);

  DataLine[Result] := GetTileZoneMedian(ATile, 0, 0, 8, 8, UseCount);
  Inc(Result);
  DataLine[Result] := UseCount;
  Inc(Result);

  DataLine[Result] := GetTileGridMedian(ATile, True, UseCount);
  Inc(Result);
  DataLine[Result] := UseCount;
  Inc(Result);

  DataLine[Result] := GetTileGridMedian(ATile, False, UseCount);
  Inc(Result);
  DataLine[Result] := UseCount;
  Inc(Result);

  // clear remaining bytes
  FillByte(DataLine[Result], cKModesFeatureCount - Result, 0);
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

  imgPalette.Picture.Bitmap.Width := cTilePaletteSize;
  imgPalette.Picture.Bitmap.Height := cPaletteCount;
  imgPalette.Picture.Bitmap.PixelFormat:=pf32bit;

  imgTiles.Width := cScreenWidth div 2;
  imgTiles.Height := cScreenHeight;
  imgSource.Width := cScreenWidth;
  imgSource.Height := cScreenHeight;
  imgDest.Width := cScreenWidth;
  imgDest.Height := cScreenHeight;
  imgPalette.Height := min(256, 16 * cPaletteCount);

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
  chkHSLChange(nil);

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
begin
  DeleteCriticalSection(FCS);

  ClearAll;
end;

initialization
  InitLuts;
end.

