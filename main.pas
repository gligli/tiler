unit main;

{$mode delphi}

{$define ASM_DBMP}

interface

uses
  LazLogger, Classes, SysUtils, windows, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, typinfo,
  StdCtrls, ComCtrls, Spin, Menus, Math, types, Process, strutils, kmodes, MTProcs, extern,
  xalglib;

type
  TEncoderStep = (esNone = -1, esLoad = 0, esDither, esGlobalTiling, esFrameTiling, esReindex, esSmooth, esSave);

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
  cPaletteCount = 8;
  cBitsPerComp = 6;
  cPreDitherMixedColors = 2;
  cVecInvWidth = 16;
  cTotalColors = 1 shl (cBitsPerComp * 3);
  cTileWidth = 8;
  cTilePaletteSize = 16;
  cTileMapWidth = 160;
  cTileMapHeight = 66;
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

  cEncoderStepLen: array[TEncoderStep] of Integer = (0, 2, 3, 3, 2, 3, 1, 2);

type
  TFloatFloatFunction = function(x: TFloat; Data: Pointer): TFloat of object;

  PTile = ^TTile;
  PPTile = ^PTile;

{$if cTotalColors <= 256}
  TPalPixel = Byte;
{$else}
  TPalPixel = Integer;
{$endif}
  PPalPixel = ^TPalPixel;
  TPalPixels = array[0..(cTileWidth - 1),0..(cTileWidth - 1)] of TPalPixel;

  TTile = record
    RGBPixels: array[0..(cTileWidth - 1),0..(cTileWidth - 1),0..2{RGB}] of Integer;
    PalPixels: TPalPixels;

    PaletteIndexes: TIntegerDynArray;
    PaletteRGB: TIntegerDynArray;

    Active: Boolean;
    UseCount, AveragedCount, TmpIndex: Integer;
  end;

  PTileMapItem = ^TTileMapItem;

  TTileMapItem = record
    GlobalTileIndex, FrameTileIndex, TmpIndex: Integer;
    HMirror,VMirror,Smoothed: Boolean;
    PalIdx: Integer;
  end;

  TTileMapItems = array of TTileMapItem;

  PKeyFrame = ^TKeyFrame;

  TFrame = record
    Index: Integer;
    Tiles: array[0..(cTileMapSize - 1)] of TTile;
    TilesIndexes: array of Integer;
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

  TKeyFrame = record
    PaletteIndexes: array[0 .. cPaletteCount - 1] of TIntegerDynArray;
    PaletteRGB: array[0 .. cPaletteCount - 1] of TIntegerDynArray;
    StartFrame, EndFrame, FrameCount: Integer;
    TileDS: PTileDataset;
  end;

  TMixingPlan = record
    List: array[0..cTotalColors - 1] of TPalPixel;
    Count: Integer;
    LumaPal: array of Integer;
    Y2Palette: array of array[0..3] of Integer;
    Y2MixedColors: Integer;
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
    miLoad: TMenuItem;
    MenuItem1: TMenuItem;
    pmProcesses: TPopupMenu;
    PopupMenu1: TPopupMenu;
    pbProgress: TProgressBar;
    seAvgTPF: TSpinEdit;
    sedPalIdx: TSpinEdit;
    seMaxTiles: TSpinEdit;
    seTempoSmoo: TSpinEdit;
    seMaxTPF: TSpinEdit;
    sePage: TSpinEdit;
    IdleTimer: TIdleTimer;
    imgTiles: TImage;
    imgSource: TImage;
    imgDest: TImage;
    seFrameCount: TSpinEdit;
    tbFrame: TTrackBar;

    procedure btnLoadClick(Sender: TObject);
    procedure btnDitherClick(Sender: TObject);
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
    FColorMapLuma: array[0..cTotalColors - 1] of Integer;
    FColorMapHue: array[0..cTotalColors - 1] of Integer;
    FColorMapImportance: array[0..cTotalColors - 1] of Integer;
    FTiles: array of PTile;
    FUseLAB: Boolean;
    FY2MixedColors: Integer;
    FProgressStep: TEncoderStep;
    FProgressPosition, FOldProgressPosition, FProgressStartTime, FProgressPrevTime: Integer;

    FCS: TRTLCriticalSection;

    function ComputeYUVCorrelation(a: TIntegerDynArray; b: TIntegerDynArray): TFloat;
    procedure DoFinal(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
    procedure DoFindBest(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
    procedure DoFrm(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
    procedure DoKF(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
    procedure DoPre(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);

    procedure LoadFrame(AFrame: PFrame; ABitmap: TBitmap);
    procedure ProgressRedraw(CurFrameIdx: Integer = -1; ProgressStep: TEncoderStep = esNone);
    procedure Render(AFrameIndex: Integer; dithered, mirrored, reduced, gamma: Boolean; spritePal: Integer; ATilePage: Integer);

    procedure RGBToYUV(r, g, b: Integer; GammaCor: Integer; out y, u, v: TFloat); overload;
    procedure RGBToYUV(fr, fg, fb: TFloat; out y, u, v: TFloat); overload;
    procedure RGBToLAB(ir, ig, ib: Integer; out ol, oa, ob: TFloat); overload;
    procedure RGBToLAB(r, g, b: TFloat; out ol, oa, ob: TFloat); overload;
    procedure RGBToColSpc(r, g, b: TFloat; LAB: Boolean; out ol, oa, ob: TFloat); overload;
    procedure RGBToColSpc(ir, ig, ib: Integer; LAB: Boolean; GammaCor: Integer; out ol, oa, ob: TFloat); overload;

    procedure ComputeTileDCT(const ATile: TTile; FromPal, QWeighting, LAB, HMirror, VMirror: Boolean; GammaCor: Integer; const pal: array of Integer;
      var DCT: TFloatDynArray);

    // Dithering algorithms ported from http://bisqwit.iki.fi/story/howto/dither/jy/

    function ColorCompare(r1, g1, b1, r2, g2, b2: Integer): Int64;
    procedure PreparePlan(var Plan: TMixingPlan; MixedColors: Integer; const pal: array of Integer);
    procedure DeviseBestMixingPlan(var Plan: TMixingPlan; r, g, b: Integer);

    procedure LoadTiles;
    function GetGlobalTileCount: Integer;
    function GetFrameTileCount(AFrame: PFrame): Integer;
    procedure CopyTile(const Src: TTile; var Dest: TTile);

    procedure DitherTile(var ATile: TTile; var Plan: TMixingPlan);
    procedure DitherTileFloydSteinberg(var ATile: TTile);
    procedure PreDitherTiles(AFrame: PFrame);
    procedure FindBestKeyframePalette(AKeyFrame: PKeyFrame);
    procedure FinalDitherTiles(AFrame: PFrame);

    function GetTileZoneMedian(const ATile: TTile; x, y, w, h: Integer): Integer;
    function GetTileGridMedian(const ATile: TTile; other: Boolean): Integer;
    procedure MergeTiles(const TileIndexes: array of Integer; TileCount: Integer; BestIdx: Integer; NewTile: TPalPixels);
    function WriteTileDatasetLine(const ATile: TTile; var DataLine: TByteDynArray; out PxlAccum: Integer): Integer;
    procedure DoGlobalTiling(DesiredNbTiles, RestartCount: Integer);

    function GoldenRatioSearch(Func: TFloatFloatFunction; Mini, Maxi: TFloat; ObjectiveY: TFloat = 0.0; Epsilon: TFloat = 1e-12; Data: Pointer = nil): TFloat;
    procedure HMirrorPalTile(var ATile: TTile);
    procedure VMirrorPalTile(var ATile: TTile);
    function GetMaxTPF(AKF: PKeyFrame): Integer;
    procedure DoKeyFrameTiling(AKF: PKeyFrame);
    procedure DoFrameTiling(AFrame: PFrame; DesiredNbTiles: Integer);

    function GetTileUseCount(ATileIndex: Integer): Integer;
    procedure ReindexTiles;
    procedure IndexFrameTiles(AFrame: PFrame);
    procedure DoTemporalSmoothing(AFrame, APrevFrame: PFrame; Y: Integer; Strength: TFloat);

    function DoExternalPCMEnc(AFN: String; Volume: Integer): String;

    procedure SaveTileIndexes(ADataStream: TStream; AFrame: PFrame);
    procedure SaveTilemap(ADataStream: TStream; AFrame: PFrame; AFrameIdx: Integer; ASkipFirst: Boolean);

    procedure Save(ADataStream, ASoundStream: TStream);

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
  Result := (R shl 16) or (G shl 8) or B;
end;

var
  gGammaCorLut: array[0..High(cGamma), 0..High(Byte)] of TFloat;
  gVecInv: array[0..256 * 4 - 1] of Cardinal;

procedure InitLuts;
var
  g, i: Integer;
begin
  for g := 0 to High(cGamma) do
    for i := 0 to High(Byte) do
      gGammaCorLut[g, i] := power(i / 255.0, cGamma[g]);

  for i := 0 to High(gVecInv) do
    gVecInv[i] := iDiv0(1 shl cVecInvWidth, i shr 2);
end;

function GammaCorrect(lut: Integer; x: Byte): TFloat; inline;
begin
  Result := gGammaCorLut[lut, x];
end;

Const
  READ_BYTES = 65536; // not too small to avoid fragmentation when reading large files.

// helperfunction that does the bulk of the work.
// We need to also collect stderr output in order to avoid
// lock out if the stderr pipe is full.
function internalRuncommand(p:TProcess;var outputstring:string;
                            var stderrstring:string; var exitstatus:integer):integer;
var
    numbytes,bytesread,available : integer;
    outputlength, stderrlength : integer;
    stderrnumbytes,stderrbytesread : integer;
begin
  result:=-1;
  try
    try
    p.Options :=  [poUsePipes];
    bytesread:=0;
    outputlength:=0;
    stderrbytesread:=0;
    stderrlength:=0;
    p.Execute;
    while p.Running do
      begin
        // Only call ReadFromStream if Data from corresponding stream
        // is already available, otherwise, on  linux, the read call
        // is blocking, and thus it is not possible to be sure to handle
        // big data amounts bboth on output and stderr pipes. PM.
        available:=P.Output.NumBytesAvailable;
        if  available > 0 then
          begin
            if (BytesRead + available > outputlength) then
              begin
                outputlength:=BytesRead + READ_BYTES;
                Setlength(outputstring,outputlength);
              end;
            NumBytes := p.Output.Read(outputstring[1+bytesread], available);
            if NumBytes > 0 then
              Inc(BytesRead, NumBytes);
          end
        // The check for assigned(P.stderr) is mainly here so that
        // if we use poStderrToOutput in p.Options, we do not access invalid memory.
        else if assigned(P.stderr) and (P.StdErr.NumBytesAvailable > 0) then
          begin
            available:=P.StdErr.NumBytesAvailable;
            if (StderrBytesRead + available > stderrlength) then
              begin
                stderrlength:=StderrBytesRead + READ_BYTES;
                Setlength(stderrstring,stderrlength);
              end;
            StderrNumBytes := p.StdErr.Read(stderrstring[1+StderrBytesRead], available);
            if StderrNumBytes > 0 then
              Inc(StderrBytesRead, StderrNumBytes);
          end
        else
          Sleep(100);
      end;
    // Get left output after end of execution
    available:=P.Output.NumBytesAvailable;
    while available > 0 do
      begin
        if (BytesRead + available > outputlength) then
          begin
            outputlength:=BytesRead + READ_BYTES;
            Setlength(outputstring,outputlength);
          end;
        NumBytes := p.Output.Read(outputstring[1+bytesread], available);
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes);
        available:=P.Output.NumBytesAvailable;
      end;
    setlength(outputstring,BytesRead);
    while assigned(P.stderr) and (P.Stderr.NumBytesAvailable > 0) do
      begin
        available:=P.Stderr.NumBytesAvailable;
        if (StderrBytesRead + available > stderrlength) then
          begin
            stderrlength:=StderrBytesRead + READ_BYTES;
            Setlength(stderrstring,stderrlength);
          end;
        StderrNumBytes := p.StdErr.Read(stderrstring[1+StderrBytesRead], available);
        if StderrNumBytes > 0 then
          Inc(StderrBytesRead, StderrNumBytes);
      end;
    setlength(stderrstring,StderrBytesRead);
    exitstatus:=p.exitstatus;
    result:=0; // we came to here, document that.
    except
      on e : Exception do
         begin
           result:=1;
           setlength(outputstring,BytesRead);
         end;
     end;
  finally
    p.free;
  end;
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

function TMainForm.ComputeYUVCorrelation(a: TIntegerDynArray; b: TIntegerDynArray): TFloat;
var
  i: Integer;
  ya, yb: TDoubleDynArray;
  y, u, v: TFloat;
begin
  SetLength(ya, Length(a) * 3);
  SetLength(yb, Length(a) * 3);

  for i := 0 to High(a) do
  begin
    RGBToYUV((a[i] shr 16) and $ff, (a[i] shr 8) and $ff, a[i] and $ff, y, u, v);
    ya[i] := y; ya[i + Length(a)] := u; ya[i + Length(a) * 2] := v;

    RGBToYUV((b[i] shr 16) and $ff, (b[i] shr 8) and $ff, b[i] and $ff, y, u, v);
    yb[i] := y; yb[i + Length(a)] := u; yb[i + Length(a) * 2] := v;
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

procedure TMainForm.DoPre(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
begin
  PreDitherTiles(@FFrames[AIndex]);
end;

procedure TMainForm.DoFindBest(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
begin
  FindBestKeyframePalette(@FKeyFrames[AIndex]);
end;

procedure TMainForm.DoFinal(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
begin
  FinalDitherTiles(@FFrames[AIndex]);
end;

procedure TMainForm.btnDitherClick(Sender: TObject);
begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(-1, esDither);
  ProcThreadPool.DoParallel(DoPre, 0, High(FFrames));
  ProgressRedraw(1);
  ProcThreadPool.DoParallel(DoFindBest, 0, High(FKeyFrames));
  ProgressRedraw(2);
  ProcThreadPool.DoParallel(DoFinal, 0, High(FFrames));
  ProgressRedraw(3);

  tbFrameChange(nil);
end;

function CompareFrames(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PInteger(Item2)^, PInteger(Item1)^);
end;

procedure TMainForm.DoFrm(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
begin
  DoFrameTiling(@FFrames[AIndex], seMaxTPF.Value);
end;

procedure TMainForm.DoKF(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
begin
  DoKeyFrameTiling(@FKeyFrames[AIndex]);
end;

procedure TMainForm.btnDoKeyFrameTilingClick(Sender: TObject);
var
  i: Integer;
begin
  if Length(FKeyFrames) = 0 then
    Exit;

  ProgressRedraw(-1, esFrameTiling);
  ProcThreadPool.DoParallel(DoKF, 0, High(FKeyFrames));
  ProgressRedraw(1);
  ProcThreadPool.DoParallel(DoFrm, 0, High(FFrames));
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
  i: Integer;
  fn: String;
  kfCnt, frc: Integer;
  isKf: Boolean;
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

  kfCnt := 0;
  for i := 0 to High(FFrames) do
  begin
    fn := Format(inPath, [i]);
    isKf := FileExists(ChangeFileExt(fn, '.kf')) or (i = 0);
    if isKf then
      Inc(kfCnt);
  end;

  SetLength(FKeyFrames, kfCnt);
  kfCnt := -1;
  for i := 0 to High(FFrames) do
  begin
    fn := Format(inPath, [i]);
    isKf := FileExists(ChangeFileExt(fn, '.kf')) or (i = 0);
    if isKf then
      Inc(kfCnt);
    FFrames[i].KeyFrame := @FKeyFrames[kfCnt];
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

  procedure DoIndexFrameTiles(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    IndexFrameTiles(@FFrames[AIndex]);
  end;

begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(-1, esReindex);

  ProcThreadPool.DoParallelLocalProc(@DoPruneUnusedTiles, 0, High(FTiles));
  ProgressRedraw(1);
  ReindexTiles;
  ProgressRedraw(2);
  ProcThreadPool.DoParallelLocalProc(@DoIndexFrameTiles, 0, High(FFrames));
  ProgressRedraw(3);

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
  pal: array[0 .. 15] of Integer;
  plan: TMixingPlan;
begin
  seed := 42;
  for i := 0 to 15 do
    pal[i] := RandInt(1 shl 24, seed);
  PreparePlan(plan, 4, pal);
  DeviseBestMixingPlan(plan, 255, 255, 255);
  DeviseBestMixingPlan(plan, 0, 128, 255);
  DeviseBestMixingPlan(plan, 128, 128, 128);
  DeviseBestMixingPlan(plan, 0, 0, 0);
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
var
  dataFS, soundFS: TFileStream;
begin
  if Length(FFrames) = 0 then
    Exit;

  ProgressRedraw(-1, esSave);

  dataFS := TFileStream.Create(IncludeTrailingPathDelimiter(edOutputDir.Text) + 'data.bin', fmCreate);
  soundFS := nil;
  if Trim(edWAV.Text) <> '' then
    soundFS := TFileStream.Create(DoExternalPCMEnc(edWAV.Text, 100), fmOpenRead or fmShareDenyWrite);

  ProgressRedraw(1);

  try
    Save(dataFS, soundFS);
  finally
    dataFS.Free;
    if Assigned(soundFS) then
      soundFS.Free;
  end;

  ProgressRedraw(2);

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
    VK_F4: btnDoGlobalTilingClick(nil);
    VK_F5: btnDoKeyFrameTilingClick(nil);
    VK_F6: btnReindexClick(nil);
    VK_F7: btnSmoothClick(nil);
    VK_F8: btnSaveClick(nil);
    VK_F9: btnRunAllClick(nil);
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
  Render(tbFrame.Position, chkDithered.Checked, chkMirrored.Checked, chkReduced.Checked,  chkGamma.Checked, sedPalIdx.Value, sePage.Value);
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
  Plan.Count := 0;
  SetLength(Plan.LumaPal, Length(pal));
  SetLength(Plan.Y2Palette, Length(pal));

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

function PlanCompareLuma(Item1,Item2,UserParameter:Pointer):Integer;
var
  pi1, pi2: PInteger;
begin
  pi1 := PInteger(UserParameter);
  pi2 := PInteger(UserParameter);

  Inc(pi1, PPalPixel(Item1)^);
  Inc(pi2, PPalPixel(Item2)^);

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

procedure TMainForm.DeviseBestMixingPlan(var Plan: TMixingPlan; r, g, b: Integer);
label
  pal_loop, inner_loop, worst;
var
  t, index, max_test_count, plan_count, y2pal_len: Integer;
  chosen_amount, chosen, least_penalty, penalty: Int64;
  so_far, sum, add: array[0..3] of Integer;
  VecInv: PCardinal;
  y2pal: PInteger;
begin
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

    chosen_amount := Min(chosen_amount, Length(Plan.List) - plan_count);
{$if cTotalColors <= 256}
    FillByte(Plan.List[plan_count], chosen_amount, chosen);
{$else}
    FillDWord(Plan.List[plan_count], chosen_amount, chosen);
{$endif}
    Inc(plan_count, chosen_amount);

    so_far[0] += Plan.Y2Palette[chosen][0] * chosen_amount;
    so_far[1] += Plan.Y2Palette[chosen][1] * chosen_amount;
    so_far[2] += Plan.Y2Palette[chosen][2] * chosen_amount;
    so_far[3] += Plan.Y2Palette[chosen][3] * chosen_amount;
  end;

  QuickSort(Plan.List[0], 0, plan_count - 1, SizeOf(TPalPixel), @PlanCompareLuma, @Plan.LumaPal[0]);

  Plan.Count := plan_count;

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

procedure TMainForm.DitherTile(var ATile: TTile; var Plan: TMixingPlan);
var
  x, y: Integer;
  map_value: Integer;
begin
  for y := 0 to (cTileWidth - 1) do
    for x := 0 to (cTileWidth - 1) do
    begin
      map_value := cDitheringMap[(y shl 3) + x];
      DeviseBestMixingPlan(Plan, ATile.RGBPixels[y,x,0], ATile.RGBPixels[y,x,1], ATile.RGBPixels[y,x,2]);
      map_value := (map_value * Plan.Count) shr 6;
      ATile.PalPixels[y, x] := Plan.List[map_value];
    end;
end;

procedure TMainForm.DitherTileFloydSteinberg(var ATile: TTile);
var
  r, g, b, x, y, c, yp, xm, xp: Integer;
  OldPixel, NewPixel, QuantError: Integer;
  Pixels: array[0..(cTileWidth - 1), 0..(cTileWidth - 1), 0..2{RGB}] of Integer;
begin
  for y := 0 to (cTileWidth - 1) do
    for x := 0 to (cTileWidth - 1) do
      for c := 0 to 2 do
        Pixels[y, x, c] := ATile.RGBPixels[y, x, c];

  for y := 0 to (cTileWidth - 1) do
    for x := 0 to (cTileWidth - 1) do
      for c := 0 to 2 do
      begin
        OldPixel := Pixels[y, x, c];
        NewPixel := ((OldPixel * ((1 shl cBitsPerComp) - 1)) div 255) shl (8 - cBitsPerComp);
        QuantError := OldPixel - NewPixel;

        yp := (y + 1) and (cTileWidth - 1);
        xp := (x + 1) and (cTileWidth - 1);
        xm := (x - 1) and (cTileWidth - 1);

        Pixels[y,  x,  c] := NewPixel;

        Pixels[yp, xp, c] += (QuantError * 1) shr 4;
        Pixels[yp, xm, c] += (QuantError * 3) shr 4;
        Pixels[yp, x,  c] += (QuantError * 5) shr 4;
        Pixels[y,  xp, c] += (QuantError * 7) shr 4;
      end;

  for y := 0 to (cTileWidth - 1) do
    for x := 0 to (cTileWidth - 1) do
    begin
      r := EnsureRange(Pixels[y, x, 0] shr (8 - cBitsPerComp), 0, (1 shl cBitsPerComp) - 1);
      g := EnsureRange(Pixels[y, x, 1] shr (8 - cBitsPerComp), 0, (1 shl cBitsPerComp) - 1);
      b := EnsureRange(Pixels[y, x, 2] shr (8 - cBitsPerComp), 0, (1 shl cBitsPerComp) - 1);

      ATile.PalPixels[y, x] := r or (g shl cBitsPerComp) or (b shl (cBitsPerComp * 2));
    end;
end;

type
  TCountIndex = (ciCount, ciIndex, ciImportance, ciLuma, ciHue);
  TCountIndexArray = array[Low(TCountIndex)..High(TCountIndex)] of Integer;
  PCountIndexArray = ^TCountIndexArray;


function CompareCMUCntHueLuma(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PCountIndexArray(Item2)^[ciCount], PCountIndexArray(Item1)^[ciCount]);
  if Result = 0 then
    Result := CompareValue(PCountIndexArray(Item1)^[ciHue], PCountIndexArray(Item2)^[ciHue]);
  if Result = 0 then
    Result := CompareValue(PCountIndexArray(Item1)^[ciLuma], PCountIndexArray(Item2)^[ciLuma]);
end;

function CompareCMUHueLuma(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PCountIndexArray(Item1)^[ciHue], PCountIndexArray(Item2)^[ciHue]);
  if Result = 0 then
    Result := CompareValue(PCountIndexArray(Item1)^[ciLuma], PCountIndexArray(Item2)^[ciLuma]);
end;

function CompareCMULumaHueInv(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PCountIndexArray(Item2)^[ciLuma], PCountIndexArray(Item1)^[ciLuma]);
  if Result = 0 then
    Result := CompareValue(PCountIndexArray(Item2)^[ciHue], PCountIndexArray(Item1)^[ciHue]);
end;

procedure TMainForm.PreDitherTiles(AFrame: PFrame);
var
  sx, sy, i, tx, ty: Integer;

{$if cBitsPerComp > 4}
  r, g, b: Integer;
{$endif}

  CMUsage: array of TCountIndexArray;
  Tile_: PTile;
  FullPalTile: TTile;
  Plan, CMPlan: TMixingPlan;
begin
{$if cBitsPerComp <= 4}
  PreparePlan(CMPlan, cPreDitherMixedColors, FColorMap);
{$endif}

  SetLength(CMUsage, cTotalColors);

  for sy := 0 to cTileMapHeight - 1 do
    for sx := 0 to cTileMapWidth - 1 do
    begin
      Tile_ := FTiles[AFrame^.TileMap[sy, sx].GlobalTileIndex];

      if not Tile_^.Active then
        Exit;

      CopyTile(Tile_^, FullPalTile);

      for i := 0 to High(CMUsage) do
      begin
        CMUsage[i][ciCount] := 0;
        CMUsage[i][ciIndex] := i;
        CMUsage[i][ciImportance] := FColorMapImportance[i];
        CMUsage[i][ciLuma] := FColorMapLuma[i];
        CMUsage[i][ciHue] := FColorMapHue[i];
      end;

      // dither using full RGB palette
{$if cBitsPerComp <= 4}
      DitherTile(FullPalTile, CMPlan);
{$else}
      DitherTileFloydSteinberg(FullPalTile);
{$endif}
      for ty := 0 to (cTileWidth - 1) do
        for tx := 0 to (cTileWidth - 1) do
          Inc(CMUsage[FullPalTile.PalPixels[ty, tx]][ciCount], FullPalTile.AveragedCount);

      // keep the 16 most used colors

      QuickSort(CMUsage[0], 0, High(CMUsage), SizeOf(CMUsage[0]), @CompareCMUCntHueLuma);

      // sort those by importance and luma
      QuickSort(CMUsage[0], 0, cTilePaletteSize - 1, SizeOf(CMUsage[0]), @CompareCMULumaHueInv);

      SetLength(Tile_^.PaletteIndexes, cTilePaletteSize);
      SetLength(Tile_^.PaletteRGB, cTilePaletteSize);
      for i := 0 to cTilePaletteSize - 1 do
      begin
        Tile_^.PaletteIndexes[i] := CMUsage[cTilePaletteSize - 1 - i][ciIndex];
        Tile_^.PaletteRGB[i] := FColorMap[Tile_^.PaletteIndexes[i]];
      end;

      // dither again using that 16 color palette
      PreparePlan(Plan, FY2MixedColors, Tile_^.PaletteRGB);

      DitherTile(Tile_^, Plan);
    end;
end;

procedure TMainForm.FindBestKeyframePalette(AKeyFrame: PKeyFrame);
const
  cPalettePattern : array[0 .. 8 - 1, 0 .. cTilePaletteSize - 1] of Integer = (
{$if cBitsPerComp <= 4}
    // 4bpc
    (0, 1, 2, 3, 5, 8, 13, 20, 30, 45, 68, 102, 154, 233, 350, 528),
    (0, 1, 2, 4, 7, 11, 16, 25, 37, 56, 85, 128, 193, 291, 439, 661),
    (0, 1, 2, 3, 6, 9, 14, 22, 33, 51, 76, 115, 174, 262, 395, 594),
    (0, 1, 2, 5, 8, 12, 18, 27, 41, 62, 94, 141, 213, 321, 483, 728),
    (0, 1, 2, 3, 6, 9, 14, 21, 32, 48, 72, 109, 164, 247, 373, 561),
    (0, 1, 2, 4, 7, 11, 17, 26, 39, 59, 89, 135, 203, 306, 461, 695),
    (0, 1, 2, 4, 6, 10, 15, 23, 35, 53, 81, 122, 184, 277, 417, 628),
    (0, 1, 2, 5, 8, 12, 19, 28, 43, 65, 98, 148, 223, 336, 506, 761)
{$else}
    // 5+bpc
    (0, 1, 2, 3, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597),
    (0, 1, 2, 5, 10, 17, 27, 44, 72, 116, 188, 305, 493, 798, 1292, 2090),
    (0, 1, 2, 4, 9, 15, 24, 39, 63, 102, 166, 269, 435, 704, 1139, 1843),
    (0, 1, 2, 6, 11, 19, 30, 49, 80, 130, 210, 341, 551, 892, 1444, 2337),
    (0, 1, 2, 3, 8, 14, 22, 36, 59, 95, 155, 251, 406, 657, 1063, 1720),
    (0, 1, 2, 6, 11, 18, 29, 47, 76, 123, 199, 323, 522, 845, 1368, 2213),
    (0, 1, 2, 4, 9, 16, 25, 41, 67, 109, 177, 287, 464, 751, 1215, 1967),
    (0, 1, 2, 7, 12, 20, 32, 52, 84, 137, 221, 359, 580, 939, 1520, 2460)
{$endif}
    );
var
  sx, sy, tx, ty, i, PalIdx: Integer;
  GTile: PTile;
  CMUsage: array of TCountIndexArray;
  sfr, efr: Integer;
begin
  Assert(cPaletteCount <= Length(cPalettePattern));

  SetLength(CMUsage, cTotalColors);

  for i := 0 to High(CMUsage) do
  begin
    CMUsage[i][ciCount] := 0;
    CMUsage[i][ciIndex] := i;
    CMUsage[i][ciImportance] := FColorMapImportance[i];
    CMUsage[i][ciLuma] := FColorMapLuma[i];
    CMUsage[i][ciHue] := FColorMapHue[i];
  end;

  // get color usage stats

  sfr := High(Integer);
  efr := Low(Integer);
  for i := 0 to High(FFrames) do
    if FFrames[i].KeyFrame = AKeyFrame then
    begin
      sfr := Min(sfr, i);
      efr := Max(efr, i);

      for sy := 0 to cTileMapHeight - 1 do
        for sx := 0 to cTileMapWidth - 1 do
        begin
          GTile := FTiles[FFrames[i].TileMap[sy, sx].GlobalTileIndex];

          for ty := 0 to cTileWidth - 1 do
            for tx := 0 to cTileWidth - 1 do
              Inc(CMUsage[GTile^.PaletteIndexes[GTile^.PalPixels[ty, tx]]][ciCount]);
        end;
    end;

  AKeyFrame^.StartFrame := sfr;
  AKeyFrame^.EndFrame := efr;
  AKeyFrame^.FrameCount := efr - sfr + 1;

  // sort colors by use count

  QuickSort(CMUsage[0], 0, High(CMUsage), SizeOf(CMUsage[0]), @CompareCMUCntHueLuma);

  // split most used colors into 16 color palettes, with the best few repeated everywhere

  for PalIdx := 0 to cPaletteCount - 1 do
  begin
    SetLength(AKeyFrame^.PaletteIndexes[PalIdx], cTilePaletteSize);
    SetLength(AKeyFrame^.PaletteRGB[PalIdx], cTilePaletteSize);
  end;

  for i := 0 to cTilePaletteSize - 1 do
    for PalIdx := 0 to cPaletteCount - 1 do
      AKeyFrame^.PaletteIndexes[PalIdx, i] := CMUsage[cPalettePattern[PalIdx, i]][ciIndex];

  for i := 0 to cTilePaletteSize - 1 do
    for PalIdx := 0 to cPaletteCount - 1 do
      AKeyFrame^.PaletteRGB[PalIdx, i] := FColorMap[AKeyFrame^.PaletteIndexes[PalIdx, i]];
end;

procedure TMainForm.FinalDitherTiles(AFrame: PFrame);
var
  sx, sy: Integer;
  KF: PKeyFrame;
  OrigTile: TTile;
  OrigTileDCT: TFloatDynArray;
  Plan: TMixingPlan;

  i, PalIdx: Integer;
  best: TFloat;

  cmp: array[0 .. cPaletteCount - 1] of TFloat;
  Tile_: array[0 .. cPaletteCount - 1] of TTile;
  TileDCT: array[0 .. cPaletteCount - 1] of TFloatDynArray;
begin
  for sy := 0 to cTileMapHeight - 1 do
    for sx := 0 to cTileMapWidth - 1 do
    begin
      if not FTiles[AFrame^.TileMap[sy, sx].GlobalTileIndex]^.Active then
        Exit;

      for PalIdx := 0 to cPaletteCount - 1 do
        CopyTile(FTiles[AFrame^.TileMap[sy, sx].GlobalTileIndex]^, Tile_[PalIdx]);
      CopyTile(FTiles[AFrame^.TileMap[sy, sx].GlobalTileIndex]^, OrigTile);

      // choose best palette from the keyframe by comparing DCT of the tile colored with either palette

      ComputeTileDCT(OrigTile, False, True, False, False, False, 0, OrigTile.PaletteRGB, OrigTileDCT);

      KF := AFrame^.KeyFrame;

      for PalIdx := 0 to cPaletteCount - 1 do
      begin
        PreparePlan(Plan, FY2MixedColors, KF^.PaletteRGB[PalIdx]);
        DitherTile(Tile_[PalIdx], Plan);

        ComputeTileDCT(Tile_[PalIdx], True, True, False, False, False, 0, KF^.PaletteRGB[PalIdx], TileDCT[PalIdx]);
        cmp[PalIdx] := CompareEuclidean192(TileDCT[PalIdx], OrigTileDCT);
      end;

      PalIdx := -1;
      best := MaxDouble;
      for i := 0 to cPaletteCount - 1 do
        if cmp[i] < best then
        begin
          PalIdx := i;
          best := cmp[i];
        end;

      // now that the palette is chosen, keep only one version of the tile

      AFrame^.TileMap[sy, sx].PalIdx := PalIdx;

      Move(KF^.PaletteIndexes[PalIdx][0], Tile_[PalIdx].PaletteIndexes[0], cTilePaletteSize * SizeOf(Integer));
      Move(KF^.PaletteRGB[PalIdx][0], Tile_[PalIdx].PaletteRGB[0], cTilePaletteSize * SizeOf(Integer));

      CopyTile(Tile_[PalIdx], AFrame^.Tiles[sx + sy * cTileMapWidth]);

      SetLength(Tile_[PalIdx].PaletteIndexes, 0);
      SetLength(Tile_[PalIdx].PaletteRGB, 0);
      CopyTile(Tile_[PalIdx], FTiles[AFrame^.TileMap[sy, sx].GlobalTileIndex]^);

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

procedure TMainForm.RGBToColSpc(ir, ig, ib: Integer; LAB: Boolean; GammaCor: Integer; out ol, oa, ob: TFloat);
begin
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
  col, u, v, x, y, xx, yy, di, cpn: Integer;
  s, q, vRatio, z: TFloat;
  YUVPixels: array[0..2, 0..cTileWidth-1,0..cTileWidth-1] of TFloat;
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

        col := pal[ATile.PalPixels[yy,xx]];
        RGBToColSpc(col and $ff, (col shr 8) and $ff, (col shr 16) and $ff, LAB, GammaCor, YUVPixels[0,y,x], YUVPixels[1,y,x], YUVPixels[2,y,x]);
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

        RGBToColSpc(ATile.RGBPixels[yy,xx,0], ATile.RGBPixels[yy,xx,1], ATile.RGBPixels[yy,xx,2], LAB, GammaCor, YUVPixels[0,y,x], YUVPixels[1,y,x], YUVPixels[2,y,x]);
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
          for x := 0 to (cTileWidth - 1) do
		      begin
			      s := YUVPixels[cpn,y,x];

			      q := s * cos((x + 0.5) * u * PI / 16.0) * cos((y + 0.5) * v * PI / 16.0);

			      z += q;
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

          AFrame^.Tiles[ti].RGBPixels[ty, tx, 0] := r;
          AFrame^.Tiles[ti].RGBPixels[ty, tx, 1] := g;
          AFrame^.Tiles[ti].RGBPixels[ty, tx, 2] := b;
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

procedure TMainForm.Render(AFrameIndex: Integer; dithered, mirrored, reduced, gamma: Boolean; spritePal: Integer;
  ATilePage: Integer);
var
  i, j, r, g, b, ti, tx, ty, col: Integer;
  pTiles, pDest, p: PInteger;
  tilePtr: PTile;
  TMItem: TTileMapItem;
  Frame: PFrame;
  fn: String;
  pal: TIntegerDynArray;
  oriCorr, chgCorr: TIntegerDynArray;
begin
  if Length(FFrames) <= 0 then
    Exit;

  AFrameIndex := EnsureRange(AFrameIndex, 0, high(FFrames));
  ATilePage := EnsureRange(ATilePage, 0, high(FFrames));

  Frame := @FFrames[AFrameIndex];

  if not Assigned(Frame) or not Assigned(Frame^.KeyFrame) then
    Exit;

  lblTileCount.Caption := 'Global: ' + IntToStr(GetGlobalTileCount) + ' / Frame: ' + IntToStr(GetFrameTileCount(Frame));

  imgTiles.Picture.Bitmap.BeginUpdate;
  imgDest.Picture.Bitmap.BeginUpdate;
  try
    // tile pages

    for j := 0 to cScreenHeight - 1 do
    begin
      pTiles := imgTiles.Picture.Bitmap.ScanLine[j];

      for i := 0 to cScreenWidth div 2 - 1 do
      begin
        ti := cTileMapWidth div 2 * (j shr 3) + (i shr 3) + cTileMapSize div 2 * ATilePage;

        tx := i and (cTileWidth - 1);
        ty := j and (cTileWidth - 1);

        if ti >= Length(FTiles) then
        begin
          r := 0;
          g := 255;
          b := 255;
        end
        else
        begin
          tilePtr := FTiles[ti];

          pal := Frame^.KeyFrame^.PaletteRGB[Max(0, spritePal)];

          if dithered and Assigned(pal) then
          begin
            col := pal[tilePtr^.PalPixels[ty, tx]];

            b := (col shr 16) and $ff; g := (col shr 8) and $ff; r := col and $ff;
          end
          else
          begin
            r := tilePtr^.RGBPixels[ty, tx, 0];
            g := tilePtr^.RGBPixels[ty, tx, 1];
            b := tilePtr^.RGBPixels[ty, tx, 2];
          end;

          if not tilePtr^.Active then
          begin
            r := 255;
            g := 0;
            b := 255;
		     end;
        end;

        p := pTiles;
        Inc(p, i);
        p^ := ToRGB(r, g, b);
      end;
    end;

    // dest screen

    SetLength(oriCorr, cScreenHeight * cScreenWidth * 2);
    SetLength(chgCorr, cScreenHeight * cScreenWidth * 2);

    for j := 0 to cScreenHeight - 1 do
    begin
      pDest := imgDest.Picture.Bitmap.ScanLine[j];

      for i := 0 to cScreenWidth - 1 do
      begin
        tx := i and (cTileWidth - 1);
        ty := j and (cTileWidth - 1);

        TMItem := Frame^.TileMap[j shr 3, i shr 3];
        if TMItem.Smoothed then
        begin
          // emulate smoothing (use previous frame tilemap item)
          TMItem := FFrames[AFrameIndex - cSmoothingPrevFrame].TileMap[j shr 3, i shr 3];
          TMItem.GlobalTileIndex := Frame^.TilesIndexes[TMItem.FrameTileIndex];
        end;
        ti := TMItem.GlobalTileIndex;

        if ti < Length(FTiles) then
        begin
          tilePtr :=  @Frame^.Tiles[(j shr 3) * cTileMapWidth + (i shr 3)];
          pal := tilePtr^.PaletteRGB;
          oriCorr[j * cScreenWidth + i] := ToRGB(tilePtr^.RGBPixels[ty, tx, 0], tilePtr^.RGBPixels[ty, tx, 1], tilePtr^.RGBPixels[ty, tx, 2]);
          oriCorr[i * cScreenHeight + j + cScreenWidth * cScreenHeight] := oriCorr[j * cScreenWidth + i];

          if reduced then
          begin
            tilePtr := FTiles[ti];
            if mirrored and TMItem.HMirror then tx := cTileWidth - 1 - tx;
            if mirrored and TMItem.VMirror then ty := cTileWidth - 1 - ty;
            if spritePal < 0 then
              pal := Frame^.KeyFrame^.PaletteRGB[TMItem.PalIdx]
            else
              pal := Frame^.KeyFrame^.PaletteRGB[spritePal];
          end;

          if dithered and Assigned(pal) then
          begin
            col := pal[tilePtr^.PalPixels[ty, tx]];
            b := (col shr 16) and $ff; g := (col shr 8) and $ff; r := col and $ff;
          end
          else
          begin
            r := tilePtr^.RGBPixels[ty, tx, 0];
            g := tilePtr^.RGBPixels[ty, tx, 1];
            b := tilePtr^.RGBPixels[ty, tx, 2];
          end;

          if gamma then
          begin
            r := round(GammaCorrect(0, r) * 255.0);
            g := round(GammaCorrect(0, g) * 255.0);
            b := round(GammaCorrect(0, b) * 255.0);
          end;

          p := pDest;
          Inc(p, i);
          p^ := ToRGB(r, g, b);

          chgCorr[j * cScreenWidth + i] := ToRGB(r, g, b);
          chgCorr[i * cScreenHeight + j + cScreenWidth * cScreenHeight] := chgCorr[j * cScreenWidth + i];
        end;
      end;
    end;

    lblCorrel.Caption := FormatFloat('0.0000', ComputeYUVCorrelation(oriCorr, chgCorr));
  finally
    imgTiles.Picture.Bitmap.EndUpdate;
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

  fn := Format(edInput.Text, [AFrameIndex]);
  if FileExists(fn) then
    imgSource.Picture.LoadFromFile(fn);

  imgSource.Invalidate;
  imgDest.Invalidate;
  imgTiles.Invalidate;
  imgPalette.Invalidate;
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
  end;

  if (CurFrameIdx < 0) and (ProgressStep = esNone) then
  begin
    FProgressPosition := 0;
    FOldProgressPosition := 0;
    FProgressStep := esNone;
    FProgressPosition := 0;
    FProgressPrevTime := GetTickCount;
    FProgressStartTime := GetTickCount;
  end;

  pbProgress.Position := pbProgress.Position + (FProgressPosition - FOldProgressPosition);
  pbProgress.Invalidate;
  lblPct.Caption := IntToStr(pbProgress.Position * 100 div pbProgress.Max) + '%';
  lblPct.Invalidate;
  Application.ProcessMessages;

  t := GetTickCount;
  WriteLn('Step: ', GetEnumName(TypeInfo(TEncoderStep), Ord(FProgressStep)), ' / ', FProgressPosition, #9'Time: ', FormatFloat('0.000', (t - FProgressPrevTime) / 1000), #9'All: ', FormatFloat('0.000', (t - FProgressStartTime) / 1000));
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
  Used: array of Boolean;
  i, j: Integer;
begin
  Result := 0;

  if Length(FTiles) = 0 then
    Exit;

  SetLength(Used, Length(FTiles));
  FillChar(Used[0], Length(FTiles) * SizeOf(Boolean), 0);

  for j := 0 to cTileMapHeight - 1 do
    for i := 0 to cTileMapWidth - 1 do
      Used[AFrame^.TileMap[j, i].GlobalTileIndex] := True;

  for i := 0 to High(Used) do
    Inc(Result, ifthen(Used[i], 1));
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
      Dest.RGBPixels[y, x, 0] := Src.RGBPixels[y, x, 0];
      Dest.RGBPixels[y, x, 1] := Src.RGBPixels[y, x, 1];
      Dest.RGBPixels[y, x, 2] := Src.RGBPixels[y, x, 2];
      Dest.PalPixels[y, x] := Src.PalPixels[y, x];
    end;
end;

procedure TMainForm.MergeTiles(const TileIndexes: array of Integer; TileCount: Integer; BestIdx: Integer; NewTile: TPalPixels);
var
  i, j, k: Integer;
begin
  if TileCount <= 0 then
    Exit;

  Move(NewTile[0, 0], FTiles[BestIdx]^.PalPixels[0, 0], sizeof(TPalPixels));

  for k := 0 to TileCount - 1 do
  begin
    j := TileIndexes[k];

    if FTiles[j]^.TmpIndex = -2 then // -2 as TmpIndex means tile is a centroid
      Continue;

    assert(j <> BestIdx, 'Malformed MergeTiles() params!');

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

procedure TMainForm.DoFrameTiling(AFrame: PFrame; DesiredNbTiles: Integer);
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

    if PrevTMI^.FrameTileIndex >= Length(AFrame^.TilesIndexes) then
      Continue;

    // compare DCT of current tile with tile from prev frame tilemap

    PrevTile := FTiles[AFrame^.TilesIndexes[PrevTMI^.FrameTileIndex]]^;
    Tile_ := FTiles[AFrame^.TilesIndexes[TMI^.FrameTileIndex]]^;

    if PrevTMI^.HMirror then HMirrorPalTile(PrevTile);
    if PrevTMI^.VMirror then VMirrorPalTile(PrevTile);
    if TMI^.HMirror then HMirrorPalTile(Tile_);
    if TMI^.VMirror then VMirrorPalTile(Tile_);

    ComputeTileDCT(PrevTile, True, True, FUseLAB, False, False, cGammaCorrectSmoothing, AFrame^.KeyFrame^.PaletteRGB[PrevTMI^.PalIdx], PrevTileDCT);
    ComputeTileDCT(Tile_, True, True, FUseLAB, False, False, cGammaCorrectSmoothing, AFrame^.KeyFrame^.PaletteRGB[TMI^.PalIdx], TileDCT);

    cmp := CompareEuclidean192(TileDCT, PrevTileDCT);
    cmp := sqrt(cmp * cSqrtFactor);

    // if difference is low enough, mark the tile as smoothed for tilemap compression use

    if Abs(cmp) <= Strength then
    begin
      TMI^.GlobalTileIndex := AFrame^.TilesIndexes[PrevTMI^.FrameTileIndex];
      TMI^.FrameTileIndex := PrevTMI^.FrameTileIndex;
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
    px: TPalPixel;
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
    px: TPalPixel;
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
  dissim: UInt64;
  DsTileIdxs: TIntegerDynArray;
  Found: Boolean;
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

  // match centroid to an existing tile in the dataset

  SetLength(DsTileIdxs, ActualNbTiles);

  for j := 0 to ActualNbTiles - 1 do
  begin
    DsTileIdxs[j] := GetMinMatchingDissim(Dataset, Centroids[j], Length(Dataset), dissim);

    Found := False;
    k := 0;
    for i := 0 to High(FTiles) do
    begin
      if not WasActive[i] then
        Continue;

      if k = DsTileIdxs[j] then
      begin
        DsTileIdxs[j] := i;
        FTiles[i]^.TmpIndex := -2;
        Found := True;
      end;

      Inc(k);
    end;

    Assert(Found, 'DsTileIdx not found!');
  end;

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

    MergeTiles(ToMerge, di, DsTileIdxs[j], NewTile)
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

function CompareTilesIndexes(Item1, Item2, UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PInteger(Item1)^, PInteger(Item2)^);
end;

procedure TMainForm.IndexFrameTiles(AFrame: PFrame);
var
  x, y, i, cnt, UseCount: Integer;
begin
  // build a list of tiles indexes used by the frame

  SetLength(AFrame^.TilesIndexes, cTileMapSize);

  cnt := 0;
  for i := 0 to High(FTiles) do
    if FTiles[i]^.Active then
    begin
      UseCount := 0;
      for y := 0 to (cTileMapHeight - 1) do
        for x := 0 to (cTileMapWidth - 1) do
          if AFrame^.TileMap[y, x].GlobalTileIndex = i then
          begin
            AFrame^.TilesIndexes[cnt] := i;
            Inc(UseCount);
          end;

      if UseCount <> 0 then
        Inc(cnt);
    end;

  SetLength(AFrame^.TilesIndexes, cnt);

  // sort it

  QuickSort(AFrame^.TilesIndexes[0], 0, High(AFrame^.TilesIndexes), SizeOf(AFrame^.TilesIndexes[0]), @CompareTilesIndexes);

  // point tilemap on those indexes

  for i := 0 to High(AFrame^.TilesIndexes) do
    for y := 0 to (cTileMapHeight - 1) do
      for x := 0 to (cTileMapWidth - 1) do
        if AFrame^.TileMap[y, x].GlobalTileIndex = AFrame^.TilesIndexes[i] then
          AFrame^.TileMap[y, x].FrameTileIndex := i;
end;

function TMainForm.DoExternalPCMEnc(AFN: String; Volume: Integer): String;
var
  i: Integer;
  Output, ErrOut: String;
  Process: TProcess;
begin
  Process := TProcess.Create(nil);

  Process.CurrentDirectory := ExtractFilePath(ParamStr(0));
  Process.Executable := 'pcmenc.exe';
  Process.Parameters.Add('-p 4 -dt1 ' + IntToStr(cClocksPerSample) + ' -dt2 ' + IntToStr(cClocksPerSample) + ' -dt3 ' +
    IntToStr(cClocksPerSample) + ' -cpuf ' + IntToStr(cZ80Clock) + ' -rto 3 -a ' + IntToStr(Volume) + ' -r 4096 -precision 8 "' + AFN + '"');
  Process.ShowWindow := swoHIDE;
  Process.Priority := ppIdle;

  i := 0;
  internalRuncommand(Process, Output, ErrOut, i); // destroys Process

  Result := AFN + '.pcmenc';
end;

procedure TMainForm.SaveTileIndexes(ADataStream: TStream; AFrame: PFrame);
var
  j, prevTileIndex, diffTileIndex, prevDTI, sameCount, tiZ80Cycles: Integer;
  tiVBlank, vbl: Boolean;
  tileIdxStream: TMemoryStream;

  function CurrentLineJitter: Integer;
  begin
    Result := round(IfThen(tiVBlank, -cLineJitterCompensation * cCyclesPerLine, cLineJitterCompensation * cCyclesPerLine));
  end;

  procedure CountZ80Cycles(ACycleAdd: Integer);
  var
    cyPh: Integer;
  begin
    cyPh := cCyclesPerDisplayPhase[tiVBlank];
    tiZ80Cycles += ACycleAdd;
    if tiZ80Cycles > cyPh + CurrentLineJitter then
    begin
      tiZ80Cycles -= cyPh;
      tiVBlank := not tiVBlank;
      tileIdxStream.WriteByte(cTileIndexesVBlankSwitch);
      tiZ80Cycles += cTileIndexesTimings[tiVBlank, 4];
    end;
  end;

  procedure DoTileIndex;
  var
    priorCount, cyAdd: Integer;
    vbl: Boolean;
  begin
    vbl := tiVBlank;
    if sameCount = 1 then
    begin
      if vbl then
        CountZ80Cycles(cTileIndexesTimings[tiVBlank, 1]);
      tileIdxStream.WriteByte(prevDTI - 1);
      if not vbl then
        CountZ80Cycles(cTileIndexesTimings[tiVBlank, 1]);
    end
    else if sameCount <> 0 then
    begin
      // split upload in case we need to switch VBlank in the midlde of it
      priorCount := sameCount;
      cyAdd := cTileIndexesTimings[tiVBlank, 2] + priorCount * cTileIndexesTimings[tiVBlank, 3];

      while (tiZ80Cycles + cyAdd > cCyclesPerDisplayPhase[tiVBlank] + CurrentLineJitter) and (priorCount > 0) do
      begin
        Dec(priorCount);
        cyAdd := cTileIndexesTimings[tiVBlank, 2] + priorCount * cTileIndexesTimings[tiVBlank, 3];
      end;

      // compute cycles
      cyAdd := cTileIndexesTimings[tiVBlank, 2] + priorCount * cTileIndexesTimings[tiVBlank, 3] +
               cTileIndexesTimings[not tiVBlank, 2] + (sameCount - priorCount) * cTileIndexesTimings[not tiVBlank, 3];

      // in case we need to switch VBlank in the middle of the upload, add one more tile in "slow" not VBlank state
      if tiZ80Cycles + cyAdd > cCyclesPerDisplayPhase[tiVBlank] + CurrentLineJitter then
      begin
        if (priorCount > 0) and tiVBlank then
        begin
          Dec(priorCount);
          cyAdd -= cTileIndexesTimings[tiVBlank, 3];
          cyAdd += cTileIndexesTimings[not tiVBlank, 3];
        end
        else if (priorCount < sameCount) and not tiVBlank then
        begin
          Inc(priorCount);
          cyAdd += cTileIndexesTimings[tiVBlank, 3];
          cyAdd -= cTileIndexesTimings[not tiVBlank, 3];
        end;
      end;

      // if we output only one command, only one fixed cost should have been added
      if priorCount = 0 then
        cyAdd -= cTileIndexesTimings[tiVBlank, 2]
      else if priorCount = sameCount then
        cyAdd -= cTileIndexesTimings[not tiVBlank, 2];

      if priorCount > 0 then
        tileIdxStream.WriteByte(cTileIndexesRepeatStart + priorCount - 1);

      CountZ80Cycles(cyAdd);

      if sameCount - priorCount > 0 then
        tileIdxStream.WriteByte(cTileIndexesRepeatStart + sameCount - priorCount - 1);
    end;
  end;

begin
  tileIdxStream := TMemoryStream.Create;
  try
    tiZ80Cycles :=  Round((cTileIndexesInitialLine - cScreenHeight) * cCyclesPerLine);
    tiVBlank := True;

    prevTileIndex := -1;
    prevDTI := -1;
    sameCount := 0;
    for j := 0 to High(AFrame^.TilesIndexes) do
    begin
      diffTileIndex := AFrame^.TilesIndexes[j] - prevTileIndex;

      if (diffTileIndex <> 1) or (diffTileIndex <> prevDTI) or
          (diffTileIndex >= cTileIndexesMaxDiff) or (sameCount >= cTileIndexesMaxRepeat) or
          ((AFrame^.TilesIndexes[j] + cTileIndexesTileOffset) mod cTilesPerBank = 0) then // don't change bank while repeating
      begin
        DoTileIndex;
        sameCount := 1;
      end
      else
      begin
        Inc(sameCount);
      end;

      if (prevTileIndex = -1) or (diffTileIndex >= cTileIndexesMaxDiff) or (diffTileIndex < 0) or
          ((AFrame^.TilesIndexes[j] + cTileIndexesTileOffset) div cTilesPerBank <>
           (prevTileIndex + cTileIndexesTileOffset) div cTilesPerBank) then // any bank change must be a direct value
      begin
        vbl := tiVBlank;
        if vbl then
          CountZ80Cycles(cTileIndexesTimings[tiVBlank, 0]);

        tileIdxStream.WriteByte(cTileIndexesDirectValue);
        tileIdxStream.WriteWord(AFrame^.TilesIndexes[j] + cTileIndexesTileOffset);

        if not vbl then
          CountZ80Cycles(cTileIndexesTimings[tiVBlank, 0]);

        diffTileIndex := -1;
        sameCount := 0;
      end;

      prevTileIndex := AFrame^.TilesIndexes[j];
      prevDTI := diffTileIndex;
    end;

    DoTileIndex;
    tileIdxStream.WriteByte(cTileIndexesTerminator);
    tileIdxStream.WriteByte(cMaxTilesPerFrame - Length(AFrame^.TilesIndexes));

    // the whole tiles indices should stay in the same bank
    if ADataStream.Size div cBankSize <> (ADataStream.Size + tileIdxStream.Size) div cBankSize then
    begin
      ADataStream.WriteByte(1);
      while ADataStream.Size mod cBankSize <> 0 do
        ADataStream.WriteByte(0);
      DebugLn('Crossed bank limit!');
    end
    else
    begin
      ADataStream.WriteByte(0);
    end;

    tileIdxStream.Position := 0;
    ADataStream.CopyFrom(tileIdxStream, tileIdxStream.Size);

  finally
    tileIdxStream.Free;
  end;
end;

function CompareTMICache(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PInteger(Item2)^, PInteger(Item1)^);
end;

procedure TMainForm.SaveTilemap(ADataStream: TStream; AFrame: PFrame; AFrameIdx: Integer; ASkipFirst: Boolean);
var
  j, k, x, y, skipCount: Integer;
  rawTMI, tmiCacheIdx, awaitingCacheIdx, awaitingCount: Integer;
  smoothed: Boolean;
  tmi: PTileMapItem;
  rawTMIs: array[0..cTileMapSize] of Integer;
  tmiCache: array[0..4095] of packed record
    UsedCount: Integer;
    RawTMI: Integer;
  end;

  procedure DoTilemapTileCommand(DoCache, DoSkip: Boolean);
  begin
    if DoSkip and (skipCount > 0) then
    begin
      ADataStream.WriteByte(cTileMapCommandSkip or (skipCount shl 1));
      skipCount := 0;
    end;

    if DoCache and (awaitingCacheIdx <> cTileMapCacheSize) then
      if awaitingCacheIdx < 0 then
      begin
        ADataStream.WriteByte(cTileMapCommandRaw[awaitingCount] or ((-awaitingCacheIdx) shr 8));
        ADataStream.WriteByte((-awaitingCacheIdx) and $ff);
      end
      else
      begin
        ADataStream.WriteByte(cTileMapCommandCache[awaitingCount] or (awaitingCacheIdx shl 1));
      end;
  end;

begin
  for j := 0 to High(tmiCache) do
  begin
    tmiCache[j].UsedCount := 0;
    tmiCache[j].RawTMI := j;
  end;

  for y := 0 to cTileMapHeight - 1 do
    for x := 0 to cTileMapWidth - 1 do
    begin
      tmi := @AFrame^.TileMap[y, x];
      rawTMI := (tmi^.FrameTileIndex + cTileMapIndicesOffset[AFrameIdx and 1]) and $1ff;
      if tmi^.HMirror then rawTMI := rawTMI or $200;
      if tmi^.VMirror then rawTMI := rawTMI or $400;
      //if tmi^.SpritePal then rawTMI := rawTMI or $800; //TODO: binary stream
      rawTMIs[x + y * cTileMapWidth] := rawTMI;
      Inc(tmiCache[rawTMI].UsedCount);
    end;

  QuickSort(tmiCache[0], 0, High(tmiCache), SizeOf(tmiCache[0]), @CompareTMICache);

  for j := 0 to cTileMapCacheSize div 2 - 1 do
  begin
    k := j * 2;

    // 2:3 compression: high nibble b / high nibble a / low byte a / low byte b
    ADataStream.WriteByte(((tmiCache[k + 1].RawTMI shr 8) shl 4) or (tmiCache[k].RawTMI shr 8));
    ADataStream.WriteByte(tmiCache[k].RawTMI and $ff);
    ADataStream.WriteByte(tmiCache[k + 1].RawTMI and $ff);
  end;

  awaitingCacheIdx := cTileMapCacheSize;
  awaitingCount := 0;
  skipCount := 0;
  for j := 0 to cTileMapSize - 1 do
  begin
    smoothed := AFrame^.TileMap[j div cTileMapWidth, j mod cTileMapWidth].Smoothed;
    rawTMI := rawTMIs[j];

    tmiCacheIdx := -1;
    for k := 0 to cTileMapCacheSize -1 do
      if tmiCache[k].RawTMI = rawTMI then
      begin
        tmiCacheIdx := k;
        Break;
      end;
    if tmiCacheIdx = -1  then
      tmiCacheIdx := -rawTMI;

    if ASkipFirst then
    begin
      if smoothed and (skipCount < cTileMapMaxSkip) then
      begin
        DoTilemapTileCommand(True, False);

        Inc(skipCount);

        awaitingCacheIdx := cTileMapCacheSize;
        awaitingCount := 0;
      end
      else
      begin
        DoTilemapTileCommand(False, True);

        if tmiCacheIdx = awaitingCacheIdx then
        begin
          Inc(awaitingCount);

          if awaitingCount >= cTileMapMaxRepeat[awaitingCacheIdx < 0] then
          begin
            DoTilemapTileCommand(True, False);

            awaitingCacheIdx := cTileMapCacheSize;
            awaitingCount := 0;
          end;
        end
        else
        begin
          DoTilemapTileCommand(True, False);

          awaitingCacheIdx := tmiCacheIdx;
          awaitingCount := 1;
        end;
      end;
    end
    else
    begin
      if tmiCacheIdx = awaitingCacheIdx then
      begin
        Inc(awaitingCount);

        if awaitingCount >= cTileMapMaxRepeat[awaitingCacheIdx < 0] then
        begin
          DoTilemapTileCommand(True, False);

          awaitingCacheIdx := cTileMapCacheSize;
          awaitingCount := 0;
        end;
      end
      else
      begin
        DoTilemapTileCommand(True, False);

        if smoothed and (skipCount < cTileMapMaxSkip) then
        begin
          Inc(skipCount);

          awaitingCacheIdx := cTileMapCacheSize;
          awaitingCount := 0;
        end
        else
        begin
          DoTilemapTileCommand(False, True);

          awaitingCacheIdx := tmiCacheIdx;
          awaitingCount := 1;
        end;
      end;
    end;
  end;

  DoTilemapTileCommand(True, True);

  ADataStream.WriteByte(cTileMapTerminator);
end;

procedure TMainForm.Save(ADataStream, ASoundStream: TStream);
var pp, pp2, i, j, x, y, frameStart, palIdx: Integer;
    palpx: Byte;
    prevKF: PKeyFrame;
    SkipFirst: Boolean;
    tilesPlanes: array[0..cTileWidth - 1, 0..3] of Byte;
    TMStream: array[Boolean] of TMemoryStream;
begin
  // leave the size of one tile for index

  for x := 0 to cTileWidth - 1 do
    ADataStream.WriteDWord(0);

  // tiles

  for i := 0 to High(FTiles) do
  begin
    FillByte(tilesPlanes[0, 0], Sizeof(tilesPlanes), 0);
    for y := 0 to cTileWidth - 1 do
    begin
      for x := 0 to cTileWidth - 1 do
      begin
        palpx := FTiles[i]^.PalPixels[y, x];
        tilesPlanes[y, 0] := tilesPlanes[y, 0] or (((palpx and 1) shl 7) shr x);
        tilesPlanes[y, 1] := tilesPlanes[y, 1] or (((palpx and 2) shl 6) shr x);
        tilesPlanes[y, 2] := tilesPlanes[y, 2] or (((palpx and 4) shl 5) shr x);
        tilesPlanes[y, 3] := tilesPlanes[y, 3] or (((palpx and 8) shl 4) shr x);
      end;
      for x := 0 to 3 do
        ADataStream.WriteByte(tilesPlanes[y, x]);
    end;
  end;

  DebugLn(['Total tiles size: ', ADataStream.Position]);
  pp2 := ADataStream.Position;

  // index

  frameStart := ADataStream.Position + cBankSize;
  i := ADataStream.Position;
  ADataStream.Position := 0;
  ADataStream.WriteWord(Length(FFrames));
  ADataStream.WriteWord(frameStart div cBankSize);
  ADataStream.WriteWord(frameStart mod cBankSize);
  ADataStream.Position := i;

  prevKF := nil;
  for i := 0 to High(FFrames) do
  begin
    // palette

    if FFrames[i].KeyFrame <> prevKF then
    begin
      ADataStream.WriteByte(cTilePaletteSize * 2);
      for palIdx := 0 to cPaletteCount - 1 do
        for j := 0 to cTilePaletteSize - 1 do
          ADataStream.WriteByte(FFrames[i].KeyFrame^.PaletteIndexes[palIdx, j]);
    end
    else
    begin
      ADataStream.WriteByte(0);
    end;

    // tiles indexes

    pp := ADataStream.Position;
    SaveTileIndexes(ADataStream, @FFrames[i]);
    DebugLn(['TileIndexes size: ', ADataStream.Position - pp]);

    // tilemap

    for SkipFirst := False to True do
    begin
      TMStream[SkipFirst] := TMemoryStream.Create;
      SaveTileMap(TMStream[SkipFirst], @FFrames[i], i, SkipFirst);
      DebugLn(['TM size: ', TMStream[SkipFirst].Size, ' ', SkipFirst]);
    end;

    SkipFirst := True;
    if TMStream[SkipFirst].Size > TMStream[not SkipFirst].Size then
      SkipFirst := not SkipFirst;

    TMStream[SkipFirst].Position := 0;
    ADataStream.CopyFrom(TMStream[SkipFirst], TMStream[SkipFirst].Size);

    for SkipFirst := False to True do
      TMStream[SkipFirst].Free;

    // sound

    if Assigned(ASoundStream) then
    begin
      ASoundStream.Position := 2 + Floor(cFrameSoundSize * i);
      ADataStream.CopyFrom(ASoundStream, Ceil(cFrameSoundSize));
    end;

    prevKF := FFrames[i].KeyFrame;
  end;

  DebugLn(['Total frames size: ', ADataStream.Position - pp2]);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  r,g,b,i,mx,mn,col,prim_col,sr: Integer;
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

  imgSource.Left := imgTiles.Left + imgTiles.Width;
  imgDest.Top := imgSource.Top + imgSource.Height;
  imgDest.Left := imgTiles.Left + imgTiles.Width;
  imgPalette.Top := imgTiles.Top + imgTiles.Height;

  sedPalIdx.MaxValue := cPaletteCount - 1;

  cbxYilMixChange(nil);
  chkLABChange(nil);

  for es := esLoad to High(TEncoderStep) do
    cbxStep.AddItem(GetEnumName(TypeInfo(TEncoderStep), Ord(es)), Pointer(Ord(es)));
  cbxStep.ItemIndex := Ord(es);

  sr := (1 shl cBitsPerComp) - 1;

  for i := 0 to cTotalColors - 1 do
  begin
    col :=
       ((((i shr (cBitsPerComp * 0)) and sr) * 255 div sr) and $ff) or //R
      (((((i shr (cBitsPerComp * 1)) and sr) * 255 div sr) and $ff) shl 8) or //G
      (((((i shr (cBitsPerComp * 2)) and sr) * 255 div sr) and $ff) shl 16);  //B

    prim_col :=
      ((i and 1) * 255) or //R
      ((((i shr 2) and 1) * 255) shl 8) or //G
      ((((i shr 4) and 1) * 255) shl 16);  //B

    FColorMap[i] := col;
    FColorMapImportance[i] := Ord(col = prim_col);
  end;

  for i := 0 to cTotalColors - 1 do
  begin
    col := FColorMap[i];
    r := col and $ff;
    g := (col shr 8) and $ff;
    b := col shr 16;

    FColorMapLuma[i] := ((r*cRedMultiplier + g*cGreenMultiplier + b*cBlueMultiplier) shl 7) div cLumaMultiplier;

    mx := MaxIntValue([r, g, b]);
    mn := MinIntValue([r, g, b]);

    if r = mx then FColorMapHue[i] := iDiv0((g - b) shl 7, mx - mn);
    if g = mx then FColorMapHue[i] := iDiv0((b - r) shl 7, mx - mn);
    if b = mx then FColorMapHue[i] := iDiv0((r - g) shl 7, mx - mn);
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

