unit main;

{$mode objfpc}{$H+}

interface

uses
  LazLogger, Classes, SysUtils, windows, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Spin, Menus, Math, MTProcs, syncobjs, types, Process, strutils, kmodes, extern;

type
  TEncoderStep = (esNone = -1, esLoad = 0, esDither, esMakeUnique, esGlobalTiling, esFrameTiling, esReindex, esSmooth, esSave);

const
  cPhi = (1 + sqrt(5)) / 2;

  // Tweakable params
  cRandomKModesCount = 26;
  cKeyframeFixedColors = 4;
  cGamma = 1.8;
  cInvertSpritePalette = False;
  cGammaCorrectSmoothing = False;
  cRedMultiplier = 299;
  cGreenMultiplier = 587;
  cBlueMultiplier = 114;
  cKFMaxTPFSearchRatio = 0.9;
  cKFYakmoRestarts = 1;

  cLumaMultiplier = cRedMultiplier + cGreenMultiplier + cBlueMultiplier;

  // SMS consts
  cBitsPerComp = 2;
  cTotalColors = 1 shl (cBitsPerComp * 3);
  cTileWidth = 8;
  cTilePaletteSize = 16;
  cTileMapWidth = 32;
  cTileMapHeight = 24;
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
  cSmoothingPrevFrame = 2;
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

  cDCTQuantization: array[0..2{YUV}, 0..7, 0..7] of Double = (
    (
      // optimized
      (16, 11, 12, 15,  21,  32,  50,  66),
      (11, 12, 13, 18,  24,  46,  62,  73),
      (12, 13, 16, 23,  38,  56,  73,  75),
      (15, 18, 23, 29,  53,  75,  83,  80),
      (21, 24, 38, 53,  68,  95, 103,  94),
      (32, 46, 56, 75,  95, 104, 117,  96),
      (50, 62, 73, 83, 103, 117, 120, 128),
      (66, 73, 75, 80,  94,  96, 128, 160)
    ),
    (
      // Improved (reduced high frequency chroma importance)
      (17,  18,  24,  47,  99,  99,  99,  99),
      (18,  21,  26,  66,  99,  99,  99, 112),
      (24,  26,  56,  99,  99,  99, 112, 128),
      (47,  66,  99,  99,  99, 112, 128, 144),
      (99,  99,  99,  99, 112, 128, 144, 160),
      (99,  99,  99, 112, 128, 144, 160, 192),
      (99,  99, 112, 128, 144, 160, 192, 224),
      (99, 112, 128, 144, 160, 192, 224, 256)
    ),
    (
      // Improved (reduced high frequency chroma importance)
      (17,  18,  24,  47,  99,  99,  99,  99),
      (18,  21,  26,  66,  99,  99,  99, 112),
      (24,  26,  56,  99,  99,  99, 112, 128),
      (47,  66,  99,  99,  99, 112, 128, 144),
      (99,  99,  99,  99, 112, 128, 144, 160),
      (99,  99,  99, 112, 128, 144, 160, 192),
      (99,  99, 112, 128, 144, 160, 192, 224),
      (99, 112, 128, 144, 160, 192, 224, 256)
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

  cEncoderStepLen: array[TEncoderStep] of Integer = (0, 2, 3, 1, 3, 1, 3, 1, 2);

type
  PTile = ^TTile;
  PPTile = ^PTile;

  TDCTCoeffs = array[0..2{YUV},0..(cTileWidth - 1),0..(cTileWidth - 1)] of Double;

{$if cTotalColors <= 256}
  TPalPixel = Byte;
{$else}
  TPalPixel = Integer;
{$endif}
  PPalPixel = ^TPalPixel;

  TTile = record
    RGBPixels: array[0..(cTileWidth - 1),0..(cTileWidth - 1),0..2{RGB}] of Integer;
    DCTCoeffs: TDCTCoeffs;

    PalPixels: array[0..(cTileWidth - 1),0..(cTileWidth - 1)] of TPalPixel;

    // for dithering algos
    PaletteIndexes: TIntegerDynArray;
    PaletteRGB: TIntegerDynArray;

    Active: Boolean;
    UseCount, AveragedCount, TmpIndex: Integer;
  end;

  PTileMapItem = ^TTileMapItem;

  TTileMapItem = record
    GlobalTileIndex, FrameTileIndex, TmpIndex: Integer;
    HMirror,VMirror,SpritePal,Smoothed: Boolean;
  end;

  TTileMapItems = array of TTileMapItem;

  TKeyFrame = record
    PaletteIndexes: array[Boolean{SpritePal?}, 0..(cTilePaletteSize - 1)] of Integer;
    PaletteRGB: array[Boolean{SpritePal?}, 0..(cTilePaletteSize - 1)] of Integer;
    StartFrame, EndFrame, FrameCount: Integer;
  end;

  PKeyFrame = ^TKeyFrame;

  TFrame = record
    Tiles: array[0..(cTileMapSize - 1)] of TTile;
    TilesIndexes: array of Integer;
    TileMap: array[0..(cTileMapHeight - 1),0..(cTileMapWidth - 1)] of TTileMapItem;
    KeyFrame: PKeyFrame;
  end;

  PFrame = ^TFrame;

  TTileRepo = record
    Dataset: TDoubleDynArray2;
    DatasetTMIs: TTileMapItems;
    OutputTMIs: array of array[0..(cTileMapHeight - 1), 0..(cTileMapWidth - 1)] of TTileMapItem;
    FrameDataset: TDoubleDynArray2;
    Tl2Tr: TIntegerDynArray;
    pKF: PKeyFrame;
    DesiredNbTiles: Integer;
  end;

  PTileRepo = ^TTileRepo;

  TMixingPlan = record
    IsYiluoma2: Boolean;

    Y1Palette: array of array[0..2{RGB}] of Integer;

    Colors: array[Boolean] of TPalPixel;
    Ratio: Integer; // 0 = always index1, 63 = always index2, 32 = 50% of both

    List: array[0..cTotalColors - 1] of TPalPixel;
    Count: Integer;
    LumaPal: array of Integer;
    Y2Palette: array of array[0..2] of Double;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    btnRunAll: TButton;
    btnDebug: TButton;
    chkGamma: TCheckBox;
    chkReduced: TCheckBox;
    chkSprite: TCheckBox;
    chkMirrored: TCheckBox;
    chkUseOldDithering: TCheckBox;
    chkDithered: TCheckBox;
    chkPlay: TCheckBox;
    edInput: TEdit;
    edOutputDir: TEdit;
    edWAV: TEdit;
    imgPalette: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
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
    procedure chkUseOldDitheringChange(Sender: TObject);
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
    FColorMapLuma: array[0..cTotalColors - 1] of SmallInt;
    FColorMapHue: array[0..cTotalColors - 1] of SmallInt;
    FColorMapImportance: array[0..cTotalColors - 1] of ShortInt;
    FTiles: array of PTile;
    FUseOldDithering: Boolean;
    FProgressStep: TEncoderStep;
    FProgressPosition, FOldProgressPosition: Integer;

    FCS: TRTLCriticalSection;

    class function CompareEuclidean192(pa, pb: PDouble): Double;

    procedure LoadFrame(AFrame: PFrame; ABitmap: TBitmap);
    procedure ProgressRedraw(CurFrameIdx: Integer = -1; ProgressStep: TEncoderStep = esNone);
    procedure Render(AFrameIndex: Integer; dithered, mirrored, reduced: Boolean; spritePal, gamma: Integer; ATilePage: Integer);

    procedure RGBToYUV(r,g,b: Integer; GammaCor: Boolean; out y,u,v: Double); overload;
    procedure RGBToYUV(r,g,b: Double; out y,u,v: Double); overload;

    function ComputeTileDCT(var ATile: TTile; FromPal, GammaCor: Boolean; const pal: array of Integer): TDCTCoeffs;
    class function CompareTilesDCT(const ATileA, ATileB: TTile): Double;

    // Dithering algorithms ported from http://bisqwit.iki.fi/story/howto/dither/jy/

    function ColorCompareRGB(r1, g1, b1, r2, g2, b2: Double): Double;
    function ColorCompareRGBYUV(r, g, b, y, u, v: Double): Double;

    function EvaluateMixingError(r, g, b, r0, g0, b0, r1, g1, b1, r2, g2, b2: Integer; ratio: Double): Double;
    procedure DeviseBestMixingPlan(var Plan: TMixingPlan; r, g, b: Integer);

    procedure PreparePlan(var Plan: TMixingPlan; const pal: TIntegerDynArray; IsYiluoma2: Boolean);
    procedure DeviseBestMixingPlan2(var Plan: TMixingPlan; r, g, b: Integer);

    procedure LoadTiles;
    function GetGlobalTileCount: Integer;
    function GetFrameTileCount(AFrame: PFrame): Integer;
    procedure CopyTile(const Src: TTile; var Dest: TTile);

    procedure DitherTile(var ATile: TTile; var Plan: TMixingPlan);
    procedure PreDitherTiles(AFrame: PFrame);
    procedure FindBestKeyframePalette(AKeyFrame: PKeyFrame);
    procedure FinalDitherTiles(AFrame: PFrame);

    procedure MergeTiles(const TileIndexes: array of Integer; TileCount: Integer; BestIdx: Integer);
    procedure DoGlobalTiling(DesiredNbTiles, RestartCount: Integer);

    procedure HMirrorPalTile(var ATile: TTile);
    procedure VMirrorPalTile(var ATile: TTile);
    function GetMaxTPF(AKF: PKeyFrame): Integer;
    function TestTMICount(Iteration, PassTileCount: Integer; TR: PTileRepo): Integer;
    procedure DoKeyframeTiling(AKF: PKeyFrame; DesiredNbTiles: Integer);

    function GetTileUseCount(ATileIndex: Integer): Integer;
    procedure ReindexTiles;
    procedure IndexFrameTiles(AFrame: PFrame);
    procedure DoTemporalSmoothing(AFrame, APrevFrame: PFrame; Y: Integer; Strength: Double);

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
  GammaCorLut: array[0..High(Byte)] of Double;

procedure InitGammaLuts;
var i: Integer;
begin
  for i := 0 to High(GammaCorLut) do
    GammaCorLut[i] := power(i / 255.0, cGamma);
end;

function GammaCorrect(x: Byte): Double; inline;
begin
  Result := GammaCorLut[x];
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

procedure DoPre(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
begin
  TMainForm(AData).PreDitherTiles(@TMainForm(AData).FFrames[AIndex]);
end;

procedure DoFindBest(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
begin
  TMainForm(AData).FindBestKeyframePalette(@TMainForm(AData).FKeyFrames[AIndex]);
end;

procedure DoFinal(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
begin
  TMainForm(AData).FinalDitherTiles(@TMainForm(AData).FFrames[AIndex]);
end;

procedure TMainForm.btnDitherClick(Sender: TObject);
begin
  ProgressRedraw(-1, esDither);
  ProcThreadPool.DoParallel(@DoPre, 0, High(FFrames), Self);
  ProgressRedraw(1);
  ProcThreadPool.DoParallel(@DoFindBest, 0, High(FKeyFrames), Self);
  ProgressRedraw(2);
  ProcThreadPool.DoParallel(@DoFinal, 0, High(FFrames), Self);
  ProgressRedraw(3);

  tbFrameChange(nil);
end;

function CompareFrames(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PInteger(Item2)^, PInteger(Item1)^);
end;

procedure DoKF(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
begin
  TMainForm(AData).DoKeyframeTiling(@TMainForm(AData).FKeyFrames[AIndex], TMainForm(AData).seMaxTPF.Value);
end;

procedure TMainForm.btnDoKeyFrameTilingClick(Sender: TObject);
begin
  ProgressRedraw(-1, esFrameTiling);

  if Length(FKeyFrames) = 0 then
    Exit;

  ProcThreadPool.DoParallel(@DoKF, 0, High(FKeyFrames), Self);

  ProgressRedraw(1);

  tbFrameChange(nil);
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
      bmp.LoadFromFile(Format(inPath, [AIndex]));
      LeaveCriticalSection(FCS);

      LoadFrame(@FFrames[AIndex], bmp.Bitmap);
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
  ProgressRedraw(-1, esReindex);

  if Length(FFrames) = 0 then
    Exit;

  ProcThreadPool.DoParallelLocalProc(@DoPruneUnusedTiles, 0, High(FTiles));
  ProgressRedraw(1);
  ReindexTiles;
  ProgressRedraw(2);
  ProcThreadPool.DoParallelLocalProc(@DoIndexFrameTiles, 0, High(FFrames));
  ProgressRedraw(3);

  tbFrameChange(nil);
end;

procedure TMainForm.btnRunAllClick(Sender: TObject);
begin
  btnLoadClick(nil);
  btnDitherClick(nil);
  //btnDoMakeUniqueClick(nil); // useless and may alter tiles weighting
  btnDoGlobalTilingClick(nil);
  btnDoKeyFrameTilingClick(nil);
  btnReindexClick(nil);
  btnSmoothClick(nil);
  btnSaveClick(nil);

  ProgressRedraw;
  tbFrameChange(nil);
end;

procedure TMainForm.btnDebugClick(Sender: TObject);
var t: PTile;
    p:TMixingPlan;
    i,j: Integer;
    pali:array[0..15] of Integer;
    pal:array[0..15] of Integer;
begin
  new(t);
  FillChar(t^, sizeof(TTile), 0);
  t^.Active := True;
  t^.AveragedCount := 1;

  for j := 0 to 7 do
    for i := 0 to 7 do
    begin
      t^.RGBPixels[j,i,0] := j * 32;
      t^.RGBPixels[j,i,1] := i * 32;
      t^.RGBPixels[j,i,2] := (14 - j - i) * 16;
    end;

  for i := 0 to 15 do
  begin
    pali[i] := i * 3;
    pal[i] := FColorMap[pali[i]];
  end;

  PreparePlan(p, pal, True);
  DitherTile(t^, p);

  SetLength(FTiles, 4);
  SetLength(FKeyFrames, 1);
  SetLength(FFrames, 1);
  FFrames[0].KeyFrame := @FKeyFrames[0];

  move(pal[0], FKeyFrames[0].PaletteRGB[False, 0], 64);
  move(pal[0], FKeyFrames[0].PaletteRGB[True, 0], 64);

  move(pali[0], t^.PaletteIndexes[0], 64);
  move(pali[0], FKeyFrames[0].PaletteIndexes[False, 0], 64);
  move(pali[0], FKeyFrames[0].PaletteIndexes[True, 0], 64);

  ftiles[0] := t;

  new(t);
  t^ := ftiles[0]^;
  VMirrorPalTile(t^);
  ftiles[1] := t;

  new(t);
  t^ := ftiles[0]^;
  HMirrorPalTile(t^);
  ftiles[2] := t;

  new(t);
  t^ := ftiles[0]^;
  HMirrorPalTile(t^);
  VMirrorPalTile(t^);
  ftiles[3] := t;

  tbFrameChange(nil);
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
var
  dataFS, soundFS: TFileStream;
begin
  ProgressRedraw(-1, esSave);

  if Length(FFrames) = 0 then
    Exit;

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
      DoTemporalSmoothing(@FFrames[i], @FFrames[i - cSmoothingPrevFrame], AIndex, seTempoSmoo.Value / 80.0);
  end;
begin
  ProgressRedraw(-1, esSmooth);

  if Length(FFrames) = 0 then
    Exit;

  ProcThreadPool.DoParallelLocalProc(@DoSmoothing, 0, cTileMapHeight - 1);

  ProgressRedraw(1);

  tbFrameChange(nil);
end;

procedure TMainForm.chkUseOldDitheringChange(Sender: TObject);
begin
  FUseOldDithering := chkUseOldDithering.Checked;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_F1: btnLoadClick(nil);
    VK_F2: btnDitherClick(nil);
    VK_F3: btnDoGlobalTilingClick(nil);
    VK_F4: btnDoKeyFrameTilingClick(nil);
    VK_F5: btnReindexClick(nil);
    VK_F6: btnSmoothClick(nil);
    VK_F7: btnSaveClick(nil);
    VK_F8: btnRunAllClick(nil);
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
  Render(tbFrame.Position, chkDithered.Checked, chkMirrored.Checked, chkReduced.Checked, Ord(chkSprite.State), Ord(chkGamma.State), sePage.Value);
end;

procedure TMainForm.PreparePlan(var Plan: TMixingPlan; const pal: TIntegerDynArray; IsYiluoma2: Boolean);
var
  i, col, r, g, b: Integer;
begin
  Plan.Count := 0;
  Plan.IsYiluoma2 := IsYiluoma2;
  Plan.Ratio := 0;
  SetLength(Plan.LumaPal, Length(pal));
  SetLength(Plan.Y1Palette, Length(pal));
  SetLength(Plan.Y2Palette, Length(pal));

  for i := 0 to High(pal) do
  begin
    col := pal[i];
    r := col and $ff;
    g := (col shr 8) and $ff;
    b := (col shr 16) and $ff;

    Plan.LumaPal[i] := ((r*cRedMultiplier + g*cGreenMultiplier + b*cBlueMultiplier) shl 7) div cLumaMultiplier;

    Plan.Y2Palette[i][0] := GammaCorrect(r);
    Plan.Y2Palette[i][1] := GammaCorrect(g);
    Plan.Y2Palette[i][2] := GammaCorrect(b);

    Plan.Y1Palette[i][0] := round(Plan.Y2Palette[i][0] * 16383.0);
    Plan.Y1Palette[i][1] := round(Plan.Y2Palette[i][1] * 16383.0);
    Plan.Y1Palette[i][2] := round(Plan.Y2Palette[i][2] * 16383.0);
  end
end;

function PlanCompareLuma(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PInteger(UserParameter)[PPalPixel(Item1)^], PInteger(UserParameter)[PPalPixel(Item2)^]);
end;

procedure TMainForm.DeviseBestMixingPlan2(var Plan: TMixingPlan; r, g, b: Integer);
var
  p, t, index, max_test_count, chosen_amount, chosen: Integer;
  least_penalty, penalty: Double;
  cc0, cc1, cc2, t_inv: Double;
  so_far: array[0..2] of Double;
  sum: array[0..2] of Double;
  add: array[0..2] of Double;
begin
  Plan.Count := 0;

  cc0 := GammaCorrect(r);
  cc1 := GammaCorrect(g);
  cc2 := GammaCorrect(b);
  RGBToYUV(cc0, cc1, cc2, cc0, cc1, cc2);

  so_far[0] := 0; so_far[1] := 0; so_far[2] := 0;

  while (Plan.Count < cTilePaletteSize) do
  begin
    chosen_amount := 1;
    chosen := 0;
    max_test_count := IfThen(Plan.Count = 0, 1, Plan.Count);
    least_penalty := MaxDouble;
    for index := 0 to High(Plan.Y2Palette) do
    begin
      sum[0] := so_far[0]; sum[1] := so_far[1]; sum[2] := so_far[2];
      add[0] := Plan.Y2Palette[index][0]; add[1] := Plan.Y2Palette[index][1];  add[2] := Plan.Y2Palette[index][2];

      p := 1;
      while p <= max_test_count do
      begin
        sum[0] += add[0];
        sum[1] += add[1];
        sum[2] += add[2];

        add[0] *= 2;
        add[1] *= 2;
        add[2] *= 2;

        t := Plan.Count + p;
        t_inv := 1.0 / t;

        penalty := ColorCompareRGBYUV(sum[0] * t_inv, sum[1] * t_inv, sum[2] * t_inv, cc0, cc1, cc2);

        if penalty < least_penalty then
        begin
          least_penalty := penalty;
          chosen        := index;
          chosen_amount := p;
        end;

        p *= 2;
      end;
    end;

    chosen_amount := Min(chosen_amount,  High(Plan.List) - Plan.Count);
{$if cTotalColors <= 256}
    FillByte(Plan.List[Plan.Count], chosen_amount, chosen);
{$else}
    FillDWord(Plan.List[Plan.Count], chosen_amount, chosen);
{$endif}
    Inc(Plan.Count, chosen_amount);

    so_far[0] += Plan.Y2Palette[chosen][0] * chosen_amount;
    so_far[1] += Plan.Y2Palette[chosen][1] * chosen_amount;
    so_far[2] += Plan.Y2Palette[chosen][2] * chosen_amount;
  end;

  QuickSort(Plan.List[0], 0, Plan.Count - 1, SizeOf(TPalPixel), @PlanCompareLuma, @Plan.LumaPal[0]);
end;

procedure TMainForm.DitherTile(var ATile: TTile; var Plan: TMixingPlan);
var
  x, y: Integer;
  map_value: Integer;
begin
  for y := 0 to (cTileWidth - 1) do
    for x := 0 to (cTileWidth - 1) do
    begin
      map_value := cDitheringMap[(x and (cTileWidth - 1)) + ((y and (cTileWidth - 1)) shl 3)];
      if not Plan.IsYiluoma2 then
      begin
        DeviseBestMixingPlan(Plan, ATile.RGBPixels[y,x,0], ATile.RGBPixels[y,x,1], ATile.RGBPixels[y,x,2]);
        ATile.PalPixels[y, x] := Plan.Colors[map_value < Plan.Ratio];
      end
      else
      begin
        DeviseBestMixingPlan2(Plan, ATile.RGBPixels[y,x,0], ATile.RGBPixels[y,x,1], ATile.RGBPixels[y,x,2]);
        map_value := map_value * Plan.Count div 64;
        ATile.PalPixels[y, x] := Plan.List[map_value];
      end;
    end;
end;

type
  TCountIndex = (ciCount, ciIndex, ciImportance, ciLuma, ciHue);
  TCountIndexArray = array[Low(TCountIndex)..High(TCountIndex)] of Integer;
  PCountIndexArray = ^TCountIndexArray;


function CompareCMU(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PCountIndexArray(Item2)^[ciCount], PCountIndexArray(Item1)^[ciCount]) shl 16;
  Result += CompareValue(PCountIndexArray(Item2)^[ciImportance], PCountIndexArray(Item1)^[ciImportance]);
end;

function CompareCMUHueLuma(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PCountIndexArray(Item1)^[ciHue], PCountIndexArray(Item2)^[ciHue]) shl 16;
  Result += CompareValue(PCountIndexArray(Item1)^[ciLuma], PCountIndexArray(Item2)^[ciLuma]) ;
end;

function CompareCMULumaHue(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PCountIndexArray(Item2)^[ciLuma], PCountIndexArray(Item1)^[ciLuma]) shl 16;
  Result += CompareValue(PCountIndexArray(Item2)^[ciHue], PCountIndexArray(Item1)^[ciHue]) ;
end;

procedure TMainForm.PreDitherTiles(AFrame: PFrame);
var
  sx, sy, i, tx, ty: Integer;
  CMUsage: array of TCountIndexArray;
  Tile_: PTile;
  FullPalTile: TTile;
  Plan, CMPlan: TMixingPlan;
begin
  PreparePlan(CMPlan, FColorMap, True);

  SetLength(CMUsage, cTotalColors);

  for sy := 0 to cTileMapHeight - 1 do
    for sx := 0 to cTileMapWidth - 1 do
    begin
      Tile_ := FTiles[AFrame^.TileMap[sy, sx].GlobalTileIndex];

      if not Tile_^.Active then
        Exit;

      // dither using full RGB palette

      CopyTile(Tile_^, FullPalTile);

      DitherTile(FullPalTile, CMPlan);

      for i := 0 to High(CMUsage) do
      begin
        CMUsage[i][ciCount] := 0;
        CMUsage[i][ciIndex] := i;
        CMUsage[i][ciImportance] := FColorMapImportance[i];
        CMUsage[i][ciLuma] := FColorMapLuma[i];
        CMUsage[i][ciHue] := FColorMapHue[i];
      end;

      // keep the 16 most used color

      for ty := 0 to (cTileWidth - 1) do
        for tx := 0 to (cTileWidth - 1) do
          Inc(CMUsage[FullPalTile.PalPixels[ty, tx]][ciCount], FullPalTile.AveragedCount);

      QuickSort(CMUsage[0], 0, High(CMUsage), SizeOf(CMUsage[0]), @CompareCMU);

      // sort those by importance and luma
      QuickSort(CMUsage[0], 0, cTilePaletteSize - 1, SizeOf(CMUsage[0]), @CompareCMULumaHue);

      SetLength(Tile_^.PaletteIndexes, cTilePaletteSize);
      SetLength(Tile_^.PaletteRGB, cTilePaletteSize);
      for i := 0 to cTilePaletteSize - 1 do
      begin
        Tile_^.PaletteIndexes[i] := CMUsage[cTilePaletteSize - 1 - i][ciIndex];
        Tile_^.PaletteRGB[i] := FColorMap[Tile_^.PaletteIndexes[i]];
      end;

      // dither again using that 16 color palette
      PreparePlan(Plan, Tile_^.PaletteRGB, True);

      DitherTile(Tile_^, Plan);
    end;
end;

procedure TMainForm.FindBestKeyframePalette(AKeyFrame: PKeyFrame);
var sx, sy, tx, ty, i, idx, idx2: Integer;
    GTile: PTile;
    CMUsage: array of TCountIndexArray;
    sfr, efr: Integer;
begin
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

  QuickSort(CMUsage[0], 0, High(CMUsage), SizeOf(CMUsage[0]), @CompareCMU);

  // split most used colors into two 16 color palettes, with brightest and darkest colors repeated in both

  QuickSort(CMUsage[0], 0, cKeyframeFixedColors - 1, SizeOf(CMUsage[0]), @CompareCMULumaHue);
  QuickSort(CMUsage[0], cKeyframeFixedColors, cTilePaletteSize * 2 - 1, SizeOf(CMUsage[0]), @CompareCMUHueLuma);
  for i := 0 to cTilePaletteSize - 1 do
  begin
    if i >= cTilePaletteSize - cKeyframeFixedColors then
    begin
      idx := i - (cTilePaletteSize - cKeyframeFixedColors);
      idx2 := idx;
    end
    else
    begin
      idx := (i shl 1) + cKeyframeFixedColors;
      idx2 := idx + 1;
    end;

    AKeyFrame^.PaletteIndexes[False, i] := CMUsage[idx][ciIndex];
    AKeyFrame^.PaletteIndexes[True, i] := CMUsage[idx2][ciIndex];
  end;

{$if cInvertSpritePalette}
  for i := 0 to (cTilePaletteSize - cKeyframeFixedColors) div 2 - 1 do
    Exchange(AKeyFrame^.PaletteIndexes[True, cTilePaletteSize - cKeyframeFixedColors - 1 - i], AKeyFrame^.PaletteIndexes[True, i]);

  for i := 0 to cKeyframeFixedColors div 2 - 1 do
    Exchange(AKeyFrame^.PaletteIndexes[True, cTilePaletteSize - 1 - i], AKeyFrame^.PaletteIndexes[True, cTilePaletteSize - cKeyframeFixedColors + i]);
{$endif}

  for i := 0 to cTilePaletteSize - 1 do
  begin
    AKeyFrame^.PaletteRGB[False, i] := FColorMap[AKeyFrame^.PaletteIndexes[False, i]];
    AKeyFrame^.PaletteRGB[True, i] := FColorMap[AKeyFrame^.PaletteIndexes[True, i]];
  end;
end;

procedure TMainForm.FinalDitherTiles(AFrame: PFrame);
var
  sx, sy: Integer;
  KF: PKeyFrame;
  SpritePal: Boolean;
  cmp: array[Boolean] of Double;
  OrigTile: TTile;
  Tile_: array[Boolean] of TTile;
  Plan: TMixingPlan;
begin
  for sy := 0 to cTileMapHeight - 1 do
    for sx := 0 to cTileMapWidth - 1 do
    begin
      if not FTiles[AFrame^.TileMap[sy, sx].GlobalTileIndex]^.Active then
        Exit;

      CopyTile(FTiles[AFrame^.TileMap[sy, sx].GlobalTileIndex]^, Tile_[False]);
      CopyTile(FTiles[AFrame^.TileMap[sy, sx].GlobalTileIndex]^, Tile_[True]);
      CopyTile(FTiles[AFrame^.TileMap[sy, sx].GlobalTileIndex]^, OrigTile);

      // choose best palette from the keyframe by comparing DCT of the tile colored with either palette

      OrigTile.DCTCoeffs := ComputeTileDCT(OrigTile, False, True, OrigTile.PaletteRGB);

      KF := AFrame^.KeyFrame;

      for SpritePal := False to True do
      begin
        PreparePlan(Plan, KF^.PaletteRGB[SpritePal], not FUseOldDithering);
        DitherTile(Tile_[SpritePal], Plan);

        Tile_[SpritePal].DCTCoeffs := ComputeTileDCT(Tile_[SpritePal], True, True, KF^.PaletteRGB[SpritePal]);
        cmp[SpritePal] := CompareTilesDCT(Tile_[SpritePal], OrigTile);
      end;

      SpritePal := cmp[True] < cmp[False];

      // now that the palette is chosen, keep only one version of the tile

      AFrame^.TileMap[sy, sx].SpritePal := SpritePal;

      Move(KF^.PaletteIndexes[SpritePal][0], Tile_[SpritePal].PaletteIndexes[0], cTilePaletteSize * SizeOf(Integer));
      Move(KF^.PaletteRGB[SpritePal][0], Tile_[SpritePal].PaletteRGB[0], cTilePaletteSize * SizeOf(Integer));

      CopyTile(Tile_[SpritePal], AFrame^.Tiles[sx + sy * cTileMapWidth]);

      SetLength(Tile_[SpritePal].PaletteIndexes, 0);
      SetLength(Tile_[SpritePal].PaletteRGB, 0);
      CopyTile(Tile_[SpritePal], FTiles[AFrame^.TileMap[sy, sx].GlobalTileIndex]^);

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

function TMainForm.EvaluateMixingError(r, g, b, r0, g0, b0, r1, g1, b1, r2, g2, b2: Integer; ratio: Double): Double;
begin
  Result := ColorCompareRGB(r,g,b, r0,g0,b0) + ColorCompareRGB(r1,g1,b1, r2,g2,b2) * 0.1 * (abs(ratio-0.5)+0.5);
end;

procedure TMainForm.DeviseBestMixingPlan(var Plan: TMixingPlan; r, g, b: Integer);
var
  r0,r1,r2,g0,g1,g2,b0,b1,b2,index1,index2,ratio,t1,t2,t3,d1,d2,d3: Integer;
  least_penalty, penalty: Double;
begin
  Plan.Colors[False] := 0;
  Plan.Colors[True] := 0;
  Plan.Ratio := 32;

  r := round(GammaCorrect(r) * 16383.0);
  g := round(GammaCorrect(g) * 16383.0);
  b := round(GammaCorrect(b) * 16383.0);

  least_penalty := MaxDouble;
  // Loop through every unique combination of two colors from the palette,
  // and through each possible way to mix those two colors. They can be
  // mixed in exactly 64 ways, when the threshold matrix is 8x8.
  for index1 := 0 to High(Plan.Y1Palette) do
  begin
    r1 := Plan.Y1Palette[index1][0];
    g1 := Plan.Y1Palette[index1][1];
    b1 := Plan.Y1Palette[index1][2];
    for index2 := 0 to High(Plan.Y1Palette) do
    begin
      // Determine the two component colors
      r2 := Plan.Y1Palette[index2][0];
      g2 := Plan.Y1Palette[index2][1];
      b2 := Plan.Y1Palette[index2][2];
      ratio := 32;

      // Determine the ratio of mixing for each channel.
      //   solve(r1 + ratio*(r2-r1)/64 = r, ratio)
      // Take a weighed average of these three ratios according to the
      // perceived luminosity of each channel (according to CCIR 601).
      t1 := 0; t2 := 0; t3 := 0; d1 := 0; d2 := 0; d3 := 0;
      if r2 <> r1 then
      begin
        t1 := cRedMultiplier * 64 * (r - r1) div (r2-r1);
        d1 := cRedMultiplier;
      end;
      if g2 <> g1 then
      begin
        t2 := cGreenMultiplier * 64 * (g - g1) div (g2-g1);
        d2 := cGreenMultiplier;
      end;
      if b2 <> b1 then
      begin
        t3 := cBlueMultiplier * 64 * (b - b1) div (b2-b1);
        d3 := cBlueMultiplier;
      end;
      ratio := iDiv0(t1+t2+t3, d1+d2+d3);
      if(ratio < 0) then ratio := 0 else if(ratio > 63) then ratio := 63;

      // Determine what mixing them in this proportion will produce
      r0 := r1 + (ratio * (r2-r1)) shr 6;
      g0 := g1 + (ratio * (g2-g1)) shr 6;
      b0 := b1 + (ratio * (b2-b1)) shr 6;
      // Determine how well that matches what we want to accomplish
      penalty := EvaluateMixingError(r,g,b, r0,g0,b0, r1,g1,b1, r2,g2,b2, ratio/64.0);
      if penalty < least_penalty then
      begin
        // Keep the result that has the smallest error
        least_penalty := penalty;
        Plan.colors[False] := index1;
        Plan.colors[True] := index2;
        Plan.ratio := ratio;
      end;
    end;
  end;
end;

procedure TMainForm.RGBToYUV(r, g, b: Integer;  GammaCor: Boolean; out y, u, v: Double); inline;
var
  sr, sg, sb: Double;
begin
  if GammaCor then
  begin
    sr := GammaCorrect(r);
    sg := GammaCorrect(g);
    sb := GammaCorrect(b);
  end
  else
  begin
    sr := r / 255.0;
    sg := g / 255.0;
    sb := b / 255.0;
  end;

  y := 0.299*sr + 0.587*sg + 0.114*sb;
  u := -0.147*sr - 0.289*sg + 0.436*sb;
  v := 0.615*sr - 0.515*sg - 0.100*sb;
end;

procedure TMainForm.RGBToYUV(r,g,b: Double; out y,u,v: Double); inline;
var
  sr, sg, sb: Double;
begin
  sr := r;
  sg := g;
  sb := b;

  y := 0.299*sr + 0.587*sg + 0.114*sb;
  u := -0.147*sr - 0.289*sg + 0.436*sb;
  v := 0.615*sr - 0.515*sg - 0.100*sb;
end;

function TMainForm.ComputeTileDCT(var ATile: TTile; FromPal, GammaCor: Boolean; const pal: array of Integer): TDCTCoeffs;
const
  cUVRatio: array[0..cTileWidth-1] of Double = (sqrt(0.5)*0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5);
var
  col, u, v, x, y, cpn: Integer;
  coeff, s, q, vRatio, z: Double;
  YUVPixels: array[0..2, 0..cTileWidth-1,0..cTileWidth-1] of Double;
begin
  if FromPal then
  begin
    for y := 0 to (cTileWidth - 1) do
      for x := 0 to (cTileWidth - 1) do
      begin
        col := pal[ATile.PalPixels[y,x]];
        RGBToYUV(col and $ff, (col shr 8) and $ff, (col shr 16) and $ff, GammaCor,
                  YUVPixels[0,y,x], YUVPixels[1,y,x], YUVPixels[2,y,x]);
      end;
  end
  else
  begin
    for y := 0 to (cTileWidth - 1) do
      for x := 0 to (cTileWidth - 1) do
        RGBToYUV(ATile.RGBPixels[y,x,0], ATile.RGBPixels[y,x,1], ATile.RGBPixels[y,x,2], GammaCor,
                  YUVPixels[0,y,x], YUVPixels[1,y,x], YUVPixels[2,y,x]);
  end;

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

			      q := s * cos(Double((2*x+1) * u) * PI/16.0) * cos(Double((2*y+1) * v) * PI/16.0);

			      z += q;
          end;

		    coeff := cUVRatio[u] * vRatio * z * 256.0 / cDCTQuantization[cpn, v, u];

        Result[cpn,v,u] := coeff;
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

{$if defined(CPUX86_64)}
function SumOf8Squares(pa, pb: PDouble): Double; register; assembler; nostackframe;
asm
  // load 8 Doubles from pa
  movupd xmm0, oword ptr [pa]
  movupd xmm9, oword ptr [pa + $10]
  movupd xmm10, oword ptr [pa + $20]
  movupd xmm11, oword ptr [pa + $30]

  // load 8 Doubles from pb
  movupd xmm12, oword ptr [pb]
  movupd xmm13, oword ptr [pb + $10]
  movupd xmm14, oword ptr [pb + $20]
  movupd xmm15, oword ptr [pb + $30]

  // pa - pb for 8 Doubles
  subpd xmm0, xmm12
  subpd xmm9, xmm13
  subpd xmm10, xmm14
  subpd xmm11, xmm15

  // result of prev operation squared
  mulpd xmm0, xmm0
  mulpd xmm9, xmm9
  mulpd xmm10, xmm10
  mulpd xmm11, xmm11

  // sum the 8 squared differences
  addpd xmm10, xmm11
  addpd xmm0, xmm9

  addpd xmm0, xmm10

  haddpd xmm0, xmm0
end ['xmm0', 'xmm9', 'xmm10', 'xmm11', 'xmm12', 'xmm13', 'xmm14', 'xmm15'];
{$endif}

class function TMainForm.CompareEuclidean192(pa, pb: PDouble): Double;
begin
  // unroll by cTileWidth * 3
  Result := SumOf8Squares(@pa[$00], @pb[$00]);
  Result += SumOf8Squares(@pa[$08], @pb[$08]);
  Result += SumOf8Squares(@pa[$10], @pb[$10]);
  Result += SumOf8Squares(@pa[$18], @pb[$18]);
  Result += SumOf8Squares(@pa[$20], @pb[$20]);
  Result += SumOf8Squares(@pa[$28], @pb[$28]);
  Result += SumOf8Squares(@pa[$30], @pb[$30]);
  Result += SumOf8Squares(@pa[$38], @pb[$38]);

  Result += SumOf8Squares(@pa[$40], @pb[$40]);
  Result += SumOf8Squares(@pa[$48], @pb[$48]);
  Result += SumOf8Squares(@pa[$50], @pb[$50]);
  Result += SumOf8Squares(@pa[$58], @pb[$58]);
  Result += SumOf8Squares(@pa[$60], @pb[$60]);
  Result += SumOf8Squares(@pa[$68], @pb[$68]);
  Result += SumOf8Squares(@pa[$70], @pb[$70]);
  Result += SumOf8Squares(@pa[$78], @pb[$78]);

  Result += SumOf8Squares(@pa[$80], @pb[$80]);
  Result += SumOf8Squares(@pa[$88], @pb[$88]);
  Result += SumOf8Squares(@pa[$90], @pb[$90]);
  Result += SumOf8Squares(@pa[$98], @pb[$98]);
  Result += SumOf8Squares(@pa[$a0], @pb[$a0]);
  Result += SumOf8Squares(@pa[$a8], @pb[$a8]);
  Result += SumOf8Squares(@pa[$b0], @pb[$b0]);
  Result += SumOf8Squares(@pa[$b8], @pb[$b8]);
end;

class function TMainForm.CompareTilesDCT(const ATileA, ATileB: TTile): Double;
var
{$if not defined(CPUX86_64)}
  j, i, c: Integer;
{$else}
  pa, pb: PDouble;
{$endif}
begin

{$if not defined(CPUX86_64)}
  Result := 0;
  for c := 0 to 2 do
    for j := 0 to cTileWidth - 1 do
      for i := 0 to cTileWidth - 1 do
          Result += sqr(ATileA.DCTCoeffs[c, j, i] - ATileB.DCTCoeffs[c, j, i]);
{$else}
  pa := @ATileA.DCTCoeffs[0, 0, 0];
  pb := @ATileB.DCTCoeffs[0, 0, 0];

  Result := CompareEuclidean192(pa, pb);
{$endif}
end;

function TMainForm.ColorCompareRGB(r1, g1, b1, r2, g2, b2: Double): Double;
var
  y1, u1, v1, y2, u2, v2: Double;
begin
  RGBToYUV(r1, g1, b1, y1, u1, v1);
  RGBToYUV(r2, g2, b2, y2, u2, v2);

  Result := sqr(y1 - y2) + (sqr(u1 - u2) + sqr(v1 - v2)) * (1.0 / sqrt(2.0));
end;

function TMainForm.ColorCompareRGBYUV(r, g, b, y, u, v: Double): Double;
var
  y1, u1, v1: Double;
begin
  RGBToYUV(r, g, b, y1, u1, v1);

  Result := sqr(y1 - y) + (sqr(u1 - u) + sqr(v1 - v)) * (1.0 / sqrt(2.0));
end;

procedure TMainForm.LoadFrame(AFrame: PFrame; ABitmap: TBitmap);
var
  i, j, px, r, g, b, ti, tx, ty: Integer;
  pcol: PInteger;
begin
  for j := 0 to (cTileMapHeight - 1) do
    for i := 0 to (cTileMapWidth - 1) do
    begin
      AFrame^.TileMap[j, i].GlobalTileIndex := 32 * j + i;
      AFrame^.TileMap[j, i].HMirror := False;
      AFrame^.TileMap[j, i].VMirror := False;
      AFrame^.TileMap[j, i].SpritePal := False;
      AFrame^.TileMap[j, i].Smoothed := False;
      AFrame^.TileMap[j, i].TmpIndex := -1;
    end;

  ABitmap.BeginUpdate;
  try

  finally
    ABitmap.EndUpdate;
  end;

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

        ti := 32 * (j shr 3) + (i shr 3);
        tx := i and (cTileWidth - 1);
        ty := j and (cTileWidth - 1);

        AFrame^.Tiles[ti].RGBPixels[ty, tx, 0] := r;
        AFrame^.Tiles[ti].RGBPixels[ty, tx, 1] := g;
        AFrame^.Tiles[ti].RGBPixels[ty, tx, 2] := b;

        AFrame^.Tiles[ti].PalPixels[ty, tx] := 0;
        AFrame^.Tiles[ti].Active := True;
        AFrame^.Tiles[ti].AveragedCount := 1;
        AFrame^.Tiles[ti].TmpIndex := -1;
      end;
  end;
end;

procedure TMainForm.Render(AFrameIndex: Integer; dithered, mirrored, reduced: Boolean; spritePal, gamma: Integer;
  ATilePage: Integer);
var
  i, j, r, g, b, ti, tx, ty, col: Integer;
  pTiles, pDest, p: PInteger;
  tilePtr: PTile;
  TMItem: TTileMapItem;
  Frame: PFrame;
  fn: String;
  pal: TIntegerDynArray;
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
    for j := 0 to cScreenHeight * 2 - 1 do
    begin
      pTiles := imgTiles.Picture.Bitmap.ScanLine[j];
      pDest := imgDest.Picture.Bitmap.ScanLine[j];

      for i := 0 to (cScreenWidth - 1) do
        begin
          ti := 32 * (j shr 3) + (i shr 3) + cTileMapSize * ATilePage;

          // tile pages

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

            case spritePal of
              0, 2: pal := Frame^.KeyFrame^.PaletteRGB[False];
              1: pal := Frame^.KeyFrame^.PaletteRGB[True];
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

          // dest screen

          tx := i and (cTileWidth - 1);
          ty := (j shr 1) and (cTileWidth - 1);

          TMItem := Frame^.TileMap[j shr 4, i shr 3];
          if TMItem.Smoothed then
          begin
            // emulate smoothing (use previous frame tilemap item)
            TMItem := FFrames[AFrameIndex - cSmoothingPrevFrame].TileMap[j shr 4, i shr 3];
            TMItem.GlobalTileIndex := Frame^.TilesIndexes[TMItem.FrameTileIndex];
          end;
          ti := TMItem.GlobalTileIndex;

          if ti < Length(FTiles) then
          begin
            if reduced then
            begin
              tilePtr := FTiles[ti];
              if mirrored and TMItem.HMirror then tx := cTileWidth - 1 - tx;
              if mirrored and TMItem.VMirror then ty := cTileWidth - 1 - ty;
              case spritePal of
                0: pal := Frame^.KeyFrame^.PaletteRGB[False];
                1: pal := Frame^.KeyFrame^.PaletteRGB[True];
                2: pal := Frame^.KeyFrame^.PaletteRGB[TMItem.SpritePal];
              end
            end
            else
            begin
              tilePtr :=  @Frame^.Tiles[(j shr 4) * cTileMapWidth + (i shr 3)];
              pal := tilePtr^.PaletteRGB;
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

            if gamma = 1 then
            begin
              r := round(GammaCorrect(r) * 255.0);
              g := round(GammaCorrect(g) * 255.0);
              b := round(GammaCorrect(b) * 255.0);
            end;

            if j and 1 = 1 then
            begin
              // 25% scanlines
              r := r - r shr 2;
              g := g - g shr 2;
              b := b - b shr 2;
            end;

            p := pDest;
            Inc(p, i);
            p^ := ToRGB(r, g, b);
          end;
        end;
    end;
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
        p[i] := SwapRB(Frame^.KeyFrame^.PaletteRGB[j >= imgPalette.Height div 2, i * cTilePaletteSize div imgPalette.Width]);
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
  end;

  pbProgress.Position := pbProgress.Position + (FProgressPosition - FOldProgressPosition);
  pbProgress.Invalidate;
  lblPct.Caption := IntToStr(pbProgress.Position * 100 div pbProgress.Max) + '%';
  lblPct.Invalidate;
  Application.ProcessMessages;

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

  move(Src.RGBPixels[0,0,0], Dest.RGBPixels[0,0,0], SizeOf(Src.RGBPixels));
  move(Src.DCTCoeffs[0,0,0], Dest.DCTCoeffs[0,0,0], SizeOf(Src.DCTCoeffs));
  move(Src.PalPixels[0,0], Dest.PalPixels[0,0], SizeOf(Src.PalPixels));
end;

procedure TMainForm.MergeTiles(const TileIndexes: array of Integer; TileCount: Integer; BestIdx: Integer);
var
  i, j, k: Integer;
begin
  if TileCount <= 0 then
    Exit;

  for k := 0 to TileCount - 1 do
  begin
    j := TileIndexes[k];

    if FTiles[j]^.TmpIndex = -2 then // -2 as TmpIndex means tile is a centroid
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

function TMainForm.TestTMICount(Iteration, PassTileCount: Integer; TR: PTileRepo): Integer;
var
  Centroid: TDoubleDynArray;
  BestIdx, MaxTPF, dsi, ci, i, j, frame, sy, sx: Integer;
  best, cur: Double;
  tmiO: PTileMapItem;
  Ct2Tr: TIntegerDynArray;
  Centroids: TStringList;
  Clusters: TIntegerDynArray;
  Used: TBooleanDynArray;
begin
  Centroids := TStringList.Create;
  try
    // cluster all tiles duplicated fos SpritePal? / VMirror? / HMirror?

    DoExternalYakmo(TR^.Dataset, PassTileCount, cKFYakmoRestarts, False, False, False, Centroids, Clusters);

    // search tiles closest to centroids

    SetLength(Ct2Tr, PassTileCount);
    for i := 0 to PassTileCount - 1 do
    begin
      Centroid := GetSVMLightLine(i, Centroids);
      best := MaxDouble;
      BestIdx := -1;
      for j := 0 to High(TR^.Dataset) do
        if Clusters[j] = i then
        begin
          if best = MaxDouble then
            BestIdx := j;

          cur := CompareEuclidean192(@Centroid[0], @TR^.Dataset[j, 0]);
          if not IsNan(cur) and (cur < best) then
          begin
            best := cur;
            BestIdx := j;
          end;
        end;

      Ct2Tr[i] := BestIdx;
    end;

    i := PassTileCount;
    DoExternalYakmo(TR^.FrameDataset, i, 1, True, False, False, Centroids, Clusters);
    EnterCriticalSection(FCS);
    j := StrToInt(copy(Centroids[1], 1, Pos(' ', Centroids[1]) - 1));
    WriteLn('SF: ', TR^.pKF^.StartFrame, #9'Iter: ', Iteration, #9'PTC: ', i, #9, PassTileCount, #9, j);
    LeaveCriticalSection(FCS);

    // map frame tilemap items to "centroid" tiles

    SetLength(Used, Length(FTiles));

    MaxTPF := 0;
    ci := 0;
    for frame := 0 to TR^.pKF^.FrameCount - 1 do
    begin
      FillChar(Used[0], Length(FTiles) * SizeOf(Boolean), 0);

      for sy := 0 to cTileMapHeight - 1 do
        for sx := 0 to cTileMapWidth - 1 do
        begin
          dsi := Ct2Tr[Clusters[ci]];

          tmiO := @TR^.OutputTMIs[frame, sy, sx];
          tmiO^.GlobalTileIndex := TR^.DatasetTMIs[dsi].GlobalTileIndex;
          tmiO^.SpritePal := TR^.DatasetTMIs[dsi].SpritePal;
          tmiO^.VMirror := TR^.DatasetTMIs[dsi].VMirror;
          tmiO^.HMirror := TR^.DatasetTMIs[dsi].HMirror;

          Used[tmiO^.GlobalTileIndex] := True;

          Inc(ci);
        end;

      j := 0;
      for i := 0 to High(Used) do
        Inc(j, Ord(Used[i]));
      MaxTPF := max(MaxTPF, j);
    end;

  finally
    Centroids.Free;
  end;

  Result := MaxTPF;
end;

procedure TMainForm.DoKeyframeTiling(AKF: PKeyFrame; DesiredNbTiles: Integer);
var
  TRSize, MaxTPF, PassTileCount, fdi, i, j, fi, ty, tx, tc, frame, sy, sx, iter: Integer;
  sp, vm, hm: Boolean;
  frm: PFrame;
  Tile_: PTile;
  LocalTile: TTile;
  TilesRepo: PTileRepo;
  DCTCoeffs: TDCTCoeffs;
  tmiO, tmiI: PTileMapItem;
begin
  TilesRepo := New(PTileRepo);
  try
    TilesRepo^.pKF := AKF;
    TilesRepo^.DesiredNbTiles := DesiredNbTiles;

    // make a list of all active tiles

    TRSize := GetGlobalTileCount shl 3; // 3: sp/vm/hm

    SetLength(TilesRepo^.DatasetTMIs, TRSize);
    SetLength(TilesRepo^.Dataset, TRSize);
    SetLength(TilesRepo^.Tl2Tr, Length(FTiles));
    SetLength(TilesRepo^.OutputTMIs, AKF^.FrameCount);

    j := 0;
    for i := 0 to High(FTiles) do
    begin
      TilesRepo^.Tl2Tr[i] := -1;

      if FTiles[i]^.Active then
      begin
        TilesRepo^.Tl2Tr[i] := j;

        for sp := False to True do
          for vm := False to True do
            for hm := False to True do
            begin
              CopyTile(FTiles[i]^, LocalTile);

              if hm then HMirrorPalTile(LocalTile);
              if vm then VMirrorPalTile(LocalTile);
              DCTCoeffs := ComputeTileDCT(LocalTile, True, False, AKF^.PaletteRGB[sp]);

              SetLength(TilesRepo^.Dataset[j], sqr(cTileWidth) * 3);
              fi := 0;
              for tc := 0 to 2 do
                for ty := 0 to cTileWidth - 1 do
                  for tx := 0 to cTileWidth - 1 do
                  begin
                    TilesRepo^.Dataset[j, fi] := DCTCoeffs[tc, ty, tx];
                    Inc(fi);
                  end;

              TilesRepo^.DatasetTMIs[j].GlobalTileIndex := i;
              TilesRepo^.DatasetTMIs[j].SpritePal := sp;
              TilesRepo^.DatasetTMIs[j].VMirror := vm;
              TilesRepo^.DatasetTMIs[j].HMirror := hm;

              Inc(j);
            end;
      end;
    end;

    Assert(j = TRSize);

    // build a dataset from frame tiles

    SetLength(TilesRepo^.FrameDataset, AKF^.FrameCount * cTileMapSize, sqr(cTileWidth) * 3);

    fdi := 0;
    for frame := 0 to AKF^.FrameCount - 1 do
    begin
      frm := @FFrames[AKF^.StartFrame + frame];
      for sy := 0 to cTileMapHeight - 1 do
        for sx := 0 to cTileMapWidth - 1 do
        begin
          Tile_ := @frm^.Tiles[sy * cTileMapWidth + sx];
          DCTCoeffs := ComputeTileDCT(Tile_^, True, False, Tile_^.PaletteRGB);

          fi := 0;
          for tc := 0 to 2 do
            for ty := 0 to cTileWidth - 1 do
              for tx := 0 to cTileWidth - 1 do
              begin
                TilesRepo^.FrameDataset[fdi, fi] := DCTCoeffs[tc, ty, tx];
                Inc(fi);
              end;

          Inc(fdi);
        end;
    end;

    Assert(fdi = AKF^.FrameCount * cTileMapSize);

    // search of PassTileCount that gives MaxTPF closest to DesiredNbTiles

    iter := 0;
    PassTileCount := Length(TilesRepo^.Dataset);
    repeat
      MaxTPF := TestTMICount(iter, PassTileCount, TilesRepo);

      EnterCriticalSection(FCS);
      WriteLn('SF: ',AKF^.StartFrame,#9'Iter: ',iter,#9'MaxTPF: ',MaxTPF,#9'TileCnt: ',PassTileCount);
      LeaveCriticalSection(FCS);

      PassTileCount := round(PassTileCount * cKFMaxTPFSearchRatio);

      Inc(iter);
    until MaxTPF <= DesiredNbTiles;

    // update tilemap

    for frame := AKF^.StartFrame to AKF^.EndFrame do
      for sy := 0 to cTileMapHeight - 1 do
        for sx := 0 to cTileMapWidth - 1 do
        begin
          tmiO := @FFrames[frame].TileMap[sy, sx];
          tmiI := @TilesRepo^.OutputTMIs[frame - AKF^.StartFrame, sy, sx];

          tmiO^.GlobalTileIndex := tmiI^.GlobalTileIndex;
          tmiO^.SpritePal := tmiI^.SpritePal;
          tmiO^.VMirror := tmiI^.VMirror;
          tmiO^.HMirror := tmiI^.HMirror;
        end;
  finally
    Dispose(TilesRepo);
  end;
end;

procedure TMainForm.DoTemporalSmoothing(AFrame, APrevFrame: PFrame; Y: Integer; Strength: Double);
const
  cSqrtFactor = 1 / (sqr(cTileWidth) * 3);
var
  sx: Integer;
  cmp: Double;
  TMI, PrevTMI: PTileMapItem;
  Tile_, PrevTile: TTile;
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

    PrevTile.DCTCoeffs := ComputeTileDCT(PrevTile, True, cGammaCorrectSmoothing, AFrame^.KeyFrame^.PaletteRGB[PrevTMI^.SpritePal]);
    Tile_.DCTCoeffs := ComputeTileDCT(Tile_, True, cGammaCorrectSmoothing, AFrame^.KeyFrame^.PaletteRGB[TMI^.SpritePal]);

    cmp := CompareTilesDCT(Tile_, PrevTile);
    cmp := sqrt(cmp * cSqrtFactor);

    // if difference is low enough, mark the tile as smoothed for tilemap compression use

    if Abs(cmp) <= Strength then
    begin
      TMI^.GlobalTileIndex := AFrame^.TilesIndexes[PrevTMI^.FrameTileIndex];
      TMI^.FrameTileIndex := PrevTMI^.FrameTileIndex;
      TMI^.HMirror := PrevTMI^.HMirror;
      TMI^.VMirror := PrevTMI^.VMirror;
      TMI^.SpritePal := PrevTMI^.SpritePal;
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

procedure TMainForm.DoGlobalTiling(DesiredNbTiles, RestartCount: Integer);
var
  Dataset, Centroids: TByteDynArray2;
  Labels: TIntegerDynArray;
  i, j, k, x, y, Cnt, acc, best, StartingPoint, ActualNbTiles: Integer;
  DsTileIdxs: TIntegerDynArray;
  b: Byte;
  Found: Boolean;
  ToMerge: array of Integer;
  WasActive: TBooleanDynArray;
  KModes: TKModes;
begin
  SetLength(Dataset, Length(FTiles), sqr(cTileWidth));
  SetLength(WasActive, Length(FTiles));

  // prepare KModes dataset, one line per tile, 64 palette indexes per line
  // also choose KModes starting point

  StartingPoint := -RestartCount; // by default, random starting point
  Cnt := 0;
  best := MaxInt;
  for i := 0 to High(FTiles) do
  begin
    WasActive[i] := FTiles[i]^.Active;

    if not FTiles[i]^.Active then
      Continue;

    j := 0;
    acc := 0;
    for y := 0 to cTileWidth - 1 do
      for x := 0 to cTileWidth - 1 do
      begin
        b := FTiles[i]^.PalPixels[y, x];
        Inc(acc, b);
        Dataset[Cnt, j] := b;
        Inc(j);
      end;

    if (StartingPoint < 0) and (acc < best) then
    begin
      best := acc;
      StartingPoint := i;
    end;

    Inc(Cnt);
  end;

  SetLength(Dataset, Cnt);

  ProgressRedraw(1);

  // run the KModes algorithm, which will group similar tiles until it reaches a fixed amount of groups

  KModes := TKModes.Create;
  try
    ActualNbTiles := KModes.ComputeKModes(Dataset, DesiredNbTiles, -StartingPoint, cTilePaletteSize, Labels, Centroids);
  finally
    KModes.Free;
  end;

  ProgressRedraw(2);

  // match centroid to an existing tile in the dataset

  SetLength(DsTileIdxs, ActualNbTiles);

  for j := 0 to ActualNbTiles - 1 do
  begin
    DsTileIdxs[j] := GetMinMatchingDissim(Dataset, Centroids[j], acc);

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
    Cnt := 0;
    k := 0;
    for i := 0 to High(FTiles) do
    begin
      if not WasActive[i] then
        Continue;

      if Labels[k] = j then
      begin
        ToMerge[Cnt] := i;
        Inc(Cnt);
      end;

      Inc(k);
    end;

    MergeTiles(ToMerge, Cnt, DsTileIdxs[j])
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
      if tmi^.SpritePal then rawTMI := rawTMI or $800;
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
var pp, pp2, i, j, x, y, frameStart: Integer;
    palpx: Byte;
    prevKF: PKeyFrame;
    SkipFirst, b: Boolean;
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
      for b := False to True do
        for j := 0 to cTilePaletteSize - 1 do
          ADataStream.WriteByte(FFrames[i].KeyFrame^.PaletteIndexes[b, j]);
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
begin
  InitializeCriticalSection(FCS);

  FormatSettings.DecimalSeparator := '.';

{$ifdef DEBUG}
  ProcThreadPool.MaxThreadCount := 1;
  btnDebug.Visible := True;
{$else}
  SetPriorityClass(GetCurrentProcess(), IDLE_PRIORITY_CLASS);
{$endif}

  imgSource.Picture.Bitmap.Width:=cScreenWidth;
  imgSource.Picture.Bitmap.Height:=cScreenHeight;
  imgSource.Picture.Bitmap.PixelFormat:=pf32bit;

  imgDest.Picture.Bitmap.Width:=cScreenWidth;
  imgDest.Picture.Bitmap.Height:=cScreenHeight * 2;
  imgDest.Picture.Bitmap.PixelFormat:=pf32bit;

  imgTiles.Picture.Bitmap.Width:=cScreenWidth;
  imgTiles.Picture.Bitmap.Height:=cScreenHeight * 2;
  imgTiles.Picture.Bitmap.PixelFormat:=pf32bit;

  imgPalette.Picture.Bitmap.Width := cScreenWidth;
  imgPalette.Picture.Bitmap.Height := 32;
  imgPalette.Picture.Bitmap.PixelFormat:=pf32bit;

  chkUseOldDitheringChange(nil);

  sr := (1 shl cBitsPerComp) - 1;

  for i := 0 to cTotalColors - 1 do
  begin
    col :=
      (((i and sr) * 255 div sr) and $ff) or //R
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
  InitGammaLuts;
end.

