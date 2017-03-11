unit main;

{$mode objfpc}{$H+}

interface

uses
  LazLogger, Classes, SysUtils, windows, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Spin, Math, MTProcs, syncobjs, types, Process, strutils, kmodes;

const
  // Tweakable params
  cKeyframeFixedColors = 2;
  cGamma = 2.2;
  cInvertSpritePalette = True;
  cGammaCorrectFrameTiling = True;
  cGammaCorrectSmoothing = False;

  // SMS consts
  cTotalColors = 64;
  cTileWidth = 8;
  cTilePaletteSize = 16;
  cTileMapWidth = 32;
  cTileMapHeight = 24;
  cMaxTiles = cTileMapWidth * cTileMapHeight;
  cScreenWidth = cTileMapWidth * cTileWidth;
  cScreenHeight = cTileMapHeight * cTileWidth;
  cBankSize = 16384;
  cTileSize = cTileWidth * cTileWidth div 2;
  cTilesPerBank = cBankSize div cTileSize;
  cZ80Clock = 3546893;
  cLineCount = 313;
  cRefreshRate = 50;
  cCyclesPerLine = cZ80Clock / (cLineCount * cRefreshRate);
  cCyclesPerDisplayPhase : array[Boolean{VSync?}] of Integer = (
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
  cTileIndexesTimings : array[Boolean{VSync?}, 0..4 {Direct/Std/RptFix/RptVar/VBlank}] of Integer = (
    (343 + 688, 289 + 17 + 688, 91, 213 + 12 + 688, 113 + 7),
    (343 + 344, 309 + 18 + 344, 91, 233 + 14 + 344, 113 + 7)
  );
  cLineJitter = 5;
  cTileIndexesInitialLine = 201; // algo starts in VBlank

  // JPEG standard quantization tables

  cLumaQuantisation: array[0..7, 0..7] of Single = (
    (16,  11,  10,  16,  24,  40,  51,  61),
    (12,  12,  14,  19,  26,  58,  60,  55),
    (14,  13,  16,  24,  40,  57,  69,  56),
    (14,  17,  22,  29,  51,  87,  80,  62),
    (18,  22,  37,  56,  68, 109, 103,  77),
    (24,  35,  55,  64,  81, 104, 113,  92),
    (49,  64,  78,  87, 103, 121, 120, 101),
    (72,  92,  95,  98, 112, 100, 103,  99)
  );

  cChromaQuantisation: array[0..7, 0..7] of Single = (
    (17,  18,  24,  47,  99,  99,  99,  99),
    (18,  21,  26,  66,  99,  99,  99,  99),
    (24,  26,  56,  99,  99,  99,  99,  99),
    (47,  66,  99,  99,  99,  99,  99,  99),
    (99,  99,  99,  99,  99,  99,  99,  99),
    (99,  99,  99,  99,  99,  99,  99,  99),
    (99,  99,  99,  99,  99,  99,  99,  99),
    (99,  99,  99,  99,  99,  99,  99,  99)
  );

  cDitheringMap2 : array[0..8*8 - 1] of Byte = (
     0, 48, 12, 60,  3, 51, 15, 63,
    32, 16, 44, 28, 35, 19, 47, 31,
     8, 56,  4, 52, 11, 59,  7, 55,
    40, 24, 36, 20, 43, 27, 39, 23,
     2, 50, 14, 62,  1, 49, 13, 61,
    34, 18, 46, 30, 33, 17, 45, 29,
    10, 58,  6, 54,  9, 57,  5, 53,
    42, 26, 38, 22, 41, 25, 37, 21
  );

type
  PTile = ^TTile;

  TTile = record
    RGBPixels: array[0..(cTileWidth - 1),0..(cTileWidth - 1),0..2] of Integer;
    DCTCoeffs: array[Boolean, 0..(cTileWidth - 1),0..(cTileWidth - 1),0..2] of Single;

    PalPixels: array[Boolean, 0..(cTileWidth - 1),0..(cTileWidth - 1)] of Byte;

    PaletteIndexes: array[0..(cTilePaletteSize - 1)] of Byte;
    PaletteRGB: array[0..(cTilePaletteSize - 1)] of Integer;

    Active: Boolean;
    UseCount, AveragedCount, TmpIndex: Integer;
  end;

  PTileMapItem = ^TTileMapItem;

  TTileMapItem = record
    GlobalTileIndex, FrameTileIndex: Integer;
    HMirror,VMirror,SpritePal,Smoothed: Boolean;
  end;

  TKeyFrame = record
    PaletteIndexes: array[Boolean, 0..(cTilePaletteSize - 1)] of Byte;
    PaletteRGB: array[Boolean, 0..(cTilePaletteSize - 1)] of Integer;
  end;

  PKeyFrame = ^TKeyFrame;

  TFrame = record
    Tiles: array[0..(cMaxTiles - 1)] of TTile;
    TilesIndexes: array of Integer;
    TileMap: array[0..(cTileMapHeight - 1),0..(cTileMapWidth - 1)] of TTileMapItem;
    KeyFrame: PKeyFrame;
  end;

  PFrame = ^TFrame;

  TMixingPlan2 = record
    List: array[0..255] of Integer;
    Count: Integer;
    LumaPal: array of Integer;
    GammaPal: array of array[0..2] of Single;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    btnDoFrameTiling: TButton;
    btnSmooth: TButton;
    btnSave: TButton;
    btnDoGlobalTiling: TButton;
    btnLoad: TButton;
    btnDither: TButton;
    btnRunAll: TButton;
    btnReindex: TButton;
    chkDithered: TCheckBox;
    chkPlay: TCheckBox;
    edInput: TEdit;
    edOutputDir: TEdit;
    edWAV: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblTileCount: TLabel;
    pbPalette: TPaintBox;
    seAvgTPF: TSpinEdit;
    seTempoSmoo: TSpinEdit;
    seMaxTPF: TSpinEdit;
    seKMRest: TSpinEdit;
    sePage: TSpinEdit;
    IdleTimer: TIdleTimer;
    imgTiles: TImage;
    imgSource: TImage;
    imgDest: TImage;
    seFrameCount: TSpinEdit;
    tbFrame: TTrackBar;
    procedure btnDitherClick(Sender: TObject);
    procedure btnDoFrameTilingClick(Sender: TObject);
    procedure btnDoGlobalTilingClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnReindexClick(Sender: TObject);
    procedure btnRunAllClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSmoothClick(Sender: TObject);
    procedure chkPlayChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IdleTimerTimer(Sender: TObject);
    procedure pbPaletteClick(Sender: TObject);
    procedure pbPalettePaint(Sender: TObject);
    procedure tbFrameChange(Sender: TObject);
  private
    FKeyFrames: array of TKeyFrame;
    FFrames: array of TFrame;
    FColorMap: array[0..cTotalColors - 1] of Integer;
    FColorMapLuma: array[0..cTotalColors - 1] of Integer;
    FTiles: array of TTile;
    FCS: TCriticalSection;

    function GetMostFrequent(Arr: array of Byte): Integer;
    procedure RGBToYUV(r,g,b: Integer; GammaCor: Boolean; out y,u,v: Single);

    procedure ComputeTileDCT(ATile: PTile; FromPal, SpritePal, GammaCor: Boolean; const pal: array of Integer);
    function CompareTilesDCT(ATileA, ATileB: PTile; SpritePalA, SpritePalB: Boolean): Single;

    // Dithering algorithm ported from http://bisqwit.iki.fi/story/howto/dither/jy/
    function ColorCompare(r1, g1, b1, r2, g2, b2: Integer): Int64;
    procedure PreparePlan2(var Plan: TMixingPlan2; const pal: array of Integer);
    procedure DeviseBestMixingPlan2(var Plan: TMixingPlan2; r, g, b: Integer);

    procedure LoadTiles;
    procedure LoadFrame(AFrame: PFrame; ABitmap: TBitmap);
    procedure Render(AFrameIndex: Integer; dithered: Boolean; page: Integer);
    function GetGlobalTileCount: Integer;
    function GetFrameTileCount(AFrame: PFrame): Integer;
    function GetMaxFrameTileCount(AKeyFrame: PKeyFrame): Integer;
    function IsTileInFrame(AFrame: PFrame; ATile: Integer): Boolean;

    procedure DitherTile(ATile: PTile; const pal: array of Integer; SpritePal: Boolean);
    procedure PreDitherTiles(AFrame: PFrame);
    procedure FindBestKeyframePalette(AKeyFrame: PKeyFrame);
    procedure FinalDitherTiles(AFrame: PFrame);

    procedure MergeTilesByPaletteIndexes(const TileIndexes: array of Integer; TileCount: Integer);
    procedure DoGlobalTiling(DesiredNbTiles, RestartCount: Integer);

    procedure HMirrorPalTile(ATile: PTile; SpritePal: Boolean);
    procedure VMirrorPalTile(ATile: PTile; SpritePal: Boolean);
    procedure DoFrameTiling(AFrame: PFrame; DesiredNbTiles, RestartCount: Integer);

    function GetTileUseCount(ATileIndex: Integer): Integer;
    procedure ReindexTiles;
    procedure IndexFrameTiles(AFrame: PFrame);
    procedure DoTemporalSmoothing(AFrame, APrevFrame: PFrame; Y: Integer; Strength: Single);

    function DoExternalPCMEnc(AFN: String; Volume: Integer): String;

    procedure Save(ADataStream, ASoundStream: TStream);
 public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

function iDiv0(x,y:Integer):Integer;inline;
begin
  Result:=0;
  if y <> 0 then
    Result:=x div y;
end;

var
  GammaCorLut: array[0..High(Byte)] of Single;
  GammaUncorLut: array[0..High(Word)] of Byte;

procedure InitGammaLuts;
var i: Integer;
begin
  for i := 0 to High(GammaCorLut) do
    GammaCorLut[i] := power(i / 255.0, cGamma);

  for i := 0 to High(GammaUncorLut) do
    GammaUncorLut[i] := EnsureRange(Round(power(i / Single(High(GammaUncorLut)), 1 / cGamma) * 255.0), 0, 255);
end;

function GammaCorrect(x: Byte): Single; inline;
begin
  Result := GammaCorLut[x];
end;

function GammaUnCorrect(x: Single): Integer; inline;
begin
  Result := GammaUncorLut[EnsureRange(Round(x * Single(High(GammaUncorLut))), 0, High(GammaUncorLut))];
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
begin
  DoGlobalTiling(seAvgTPF.Value * Length(FFrames), seKMRest.Value);
  tbFrameChange(nil);
end;

procedure TMainForm.btnDitherClick(Sender: TObject);
  procedure DoPre(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    PreDitherTiles(@FFrames[AIndex]);
  end;

  procedure DoFindBest(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    FindBestKeyframePalette(@FKeyFrames[AIndex]);
  end;

  procedure DoFinal(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    FinalDitherTiles(@FFrames[AIndex]);
  end;

begin
  ProcThreadPool.DoParallelLocalProc(@DoPre, 0, High(FFrames));
  ProcThreadPool.DoParallelLocalProc(@DoFindBest, 0, High(FKeyFrames));
  ProcThreadPool.DoParallelLocalProc(@DoFinal, 0, High(FFrames));
  tbFrameChange(nil);
end;

function CompareFrames(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PInteger(Item2)^, PInteger(Item1)^);
end;

procedure TMainForm.btnDoFrameTilingClick(Sender: TObject);
  procedure DoDCT(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var
    SpritePal: Boolean;
  begin
    if FTiles[AIndex].Active then
      for SpritePal := False to True do
        ComputeTileDCT(@FTiles[AIndex], True, SpritePal, cGammaCorrectFrameTiling, PKeyFrame(AData)^.PaletteRGB[SpritePal]);
  end;

  procedure DoFrame(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    DoFrameTiling(@FFrames[AIndex], seMaxTPF.Value, seKMRest.Value);
  end;

var i, j, first, last: Integer;
begin
  for j := 0 to High(FKeyFrames) do
  begin
    ProcThreadPool.DoParallelLocalProc(@DoDCT, 0, High(FTiles), @FKeyFrames[j]);

    first := 0;
    for i := 0 to High(FFrames) do
      if FFrames[i].KeyFrame = @FKeyFrames[j] then
      begin
        first := i;
        Break;
      end;

    last := High(FFrames);
    for i := first to High(FFrames) do
      if FFrames[i].KeyFrame <> @FKeyFrames[j] then
      begin
        last := i - 1;
        Break;
      end;

    ProcThreadPool.DoParallelLocalProc(@DoFrame, first, last);
  end;

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
      FCS.Enter;
      try
        bmp.LoadFromFile(Format(inPath, [AIndex]));
      finally
        FCS.Leave;
      end;
      LoadFrame(@FFrames[AIndex], bmp.Bitmap);
    finally
      bmp.Free;
    end;
  end;

var
  i: Integer;
  fn: String;
  kfCnt: Integer;
  isKf: Boolean;
begin
  SetLength(FFrames, seFrameCount.Value);
  tbFrame.Max := High(FFrames);

  inPath := edInput.Text;

  for i := 0 to High(FFrames) do
  begin
    fn := Format(inPath, [i]);
    if not FileExists(fn) then
    begin
      Application.MessageBox(PChar('File not found: ' + fn), nil, MB_ICONERROR);
      Exit;
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

  LoadTiles;

  tbFrameChange(nil);
end;

procedure TMainForm.btnReindexClick(Sender: TObject);

  procedure DoPruneUnusedTiles(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    FTiles[AIndex].UseCount := GetTileUseCount(AIndex);
    FTiles[AIndex].Active := FTiles[AIndex].Active and (FTiles[AIndex].UseCount <> 0);
  end;

  procedure DoIndexFrameTiles(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  begin
    IndexFrameTiles(@FFrames[AIndex]);
  end;

begin
  ProcThreadPool.DoParallelLocalProc(@DoPruneUnusedTiles, 0, High(FTiles));
  ReindexTiles;
  ProcThreadPool.DoParallelLocalProc(@DoIndexFrameTiles, 0, High(FFrames));
  tbFrameChange(nil);
end;

procedure TMainForm.btnRunAllClick(Sender: TObject);
begin
  btnLoad.Click;
  btnDither.Click;
  btnDoGlobalTiling.Click;
  btnDoFrameTiling.Click;
  btnReindex.Click;
  btnSmooth.Click;
  btnSave.Click;
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
var
  dataFS, soundFS: TFileStream;
begin
  dataFS := TFileStream.Create(IncludeTrailingPathDelimiter(edOutputDir.Text) + 'data.bin', fmCreate);
  soundFS := nil;
  if Trim(edWAV.Text) <> '' then
    soundFS := TFileStream.Create(DoExternalPCMEnc(edWAV.Text, 100), fmOpenRead or fmShareDenyWrite);
  try
    Save(dataFS, soundFS);
  finally
    dataFS.Free;
    if Assigned(soundFS) then
      soundFS.Free;
  end;
end;

procedure TMainForm.btnSmoothClick(Sender: TObject);
  procedure DoSmoothing(AIndex: PtrInt; AData: Pointer; AItem: TMultiThreadProcItem);
  var i: Integer;
  begin
    for i := cSmoothingPrevFrame to High(FFrames) do
      DoTemporalSmoothing(@FFrames[i], @FFrames[i - cSmoothingPrevFrame], AIndex, seTempoSmoo.Value / 10);
  end;
begin
  ProcThreadPool.DoParallelLocalProc(@DoSmoothing, 0, cTileMapHeight - 1);
  tbFrameChange(nil);
end;

procedure TMainForm.chkPlayChange(Sender: TObject);
begin
  IdleTimer.Enabled := chkPlay.Checked;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  r,g,b,i,col: Integer;
begin
  SetPriorityClass(GetCurrentProcess, IDLE_PRIORITY_CLASS);
  FormatSettings.DecimalSeparator := '.';

  FCS := TCriticalSection.Create;

  imgDest.Picture.Bitmap.Width:=cScreenWidth;
  imgDest.Picture.Bitmap.Height:=cScreenHeight;
  imgDest.Picture.Bitmap.PixelFormat:=pf32bit;

  imgTiles.Picture.Bitmap.Width:=cScreenWidth;
  imgTiles.Picture.Bitmap.Height:=cScreenHeight * 2;
  imgTiles.Picture.Bitmap.PixelFormat:=pf32bit;

  for i := 0 to 63 do
  begin
    col :=
      ((i and $03) * 85) or    //R
      ((i and $0C) * 5440 ) or //G
      ((i and $30) * 348160);  //B

    FColorMap[i] := col;
  end;

  for i := 0 to cTotalColors - 1 do
  begin
    col := FColorMap[i];
    r := col and $ff;
    g := (col shr 8) and $ff;
    b := col shr 16;
    FColorMapLuma[i] := r*299 + g*587 + b*114;
  end;

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FCS.Free;
end;

procedure TMainForm.IdleTimerTimer(Sender: TObject);
begin
  if tbFrame.Position >= tbFrame.Max then
  begin
    tbFrame.Position := 0;
    Exit;
  end;

  tbFrame.Position := tbFrame.Position + 1;
end;

procedure TMainForm.pbPaletteClick(Sender: TObject);
begin

end;

procedure TMainForm.pbPalettePaint(Sender: TObject);
var x, y: Integer;
begin
  if Length(FFrames) > 0 then
    for y := 0 to pbPalette.Height - 1 do
      for x := 0 to pbPalette.Width - 1 do
        pbPalette.Canvas.Pixels[x, y] := FFrames[tbFrame.Position].KeyFrame^.PaletteRGB[y >= pbPalette.Height div 2, x * cTilePaletteSize div pbPalette.Width];
end;

procedure TMainForm.tbFrameChange(Sender: TObject);
var
  fn: String;
begin
  fn := Format(edInput.Text, [tbFrame.Position]);
  if FileExists(fn) then
    imgSource.Picture.LoadFromFile(fn);
  if Length(FFrames) > 0 then
    Render(tbFrame.Position, chkDithered.Checked, sePage.Value);

  pbPalette.Invalidate;

  Repaint;
end;

procedure TMainForm.PreparePlan2(var Plan: TMixingPlan2; const pal: array of Integer);
var
  i, col, r, g, b: Integer;
begin
  Plan.Count := 0;
  SetLength(Plan.LumaPal, Length(pal));
  SetLength(Plan.GammaPal, Length(pal));

  for i := 0 to High(pal) do
  begin
    col := pal[i];
    r := col and $ff;
    g := (col shr 8) and $ff;
    b := col shr 16;

    Plan.LumaPal[i] := r*299 + g*587 + b*114;

    Plan.GammaPal[i][0] := GammaCorrect(r);
    Plan.GammaPal[i][1] := GammaCorrect(g);
    Plan.GammaPal[i][2] := GammaCorrect(b);
  end;
end;

function PlanCompareLuma(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PInteger(UserParameter)[PInteger(Item1)^], PInteger(UserParameter)[PInteger(Item2)^]);
end;

procedure TMainForm.DeviseBestMixingPlan2(var Plan: TMixingPlan2; r, g, b: Integer);
var
  p, t, index, max_test_count, chosen_amount, chosen: Integer;
  least_penalty, penalty: Int64;
  so_far: array[0..2] of Single;
  sum: array[0..2] of Single;
  add: array[0..2] of Single;
begin
  Plan.Count := 0;

  so_far[0] := 0; so_far[1] := 0; so_far[2] := 0;

  while (Plan.Count < cTilePaletteSize) do
  begin
    chosen_amount := 1;
    chosen := 0;
    max_test_count := IfThen(Plan.Count = 0, 1, Plan.Count);
    least_penalty := High(Int64);
    for index := 0 to High(Plan.GammaPal) do
    begin
      sum[0] := so_far[0]; sum[1] := so_far[1]; sum[2] := so_far[2];
      add[0] := Plan.GammaPal[index][0]; add[1] := Plan.GammaPal[index][1];  add[2] := Plan.GammaPal[index][2];

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

        penalty := ColorCompare(r, g, b, GammaUnCorrect(sum[0] / t), GammaUnCorrect(sum[1] / t), GammaUnCorrect(sum[2] / t));

        if penalty < least_penalty then
        begin
          least_penalty := penalty;
          chosen        := index;
          chosen_amount := p;
        end;

        p *= 2;
      end;
    end;

    Assert(Plan.Count + chosen_amount < Length(Plan.List));
    FillDWord(Plan.List[Plan.Count], chosen_amount, chosen);
    Inc(Plan.Count, chosen_amount);

    so_far[0] += Plan.GammaPal[chosen][0] * chosen_amount;
    so_far[1] += Plan.GammaPal[chosen][1] * chosen_amount;
    so_far[2] += Plan.GammaPal[chosen][2] * chosen_amount;
  end;

  QuickSort(Plan.List[0], 0, Plan.Count - 1, SizeOf(Integer), @PlanCompareLuma, @Plan.LumaPal[0]);
end;

procedure TMainForm.DitherTile(ATile: PTile; const pal: array of Integer; SpritePal: Boolean);
var
  x, y: Integer;
  map_value: Integer;
  plan: TMixingPlan2;
begin
  PreparePlan2(plan, pal);

  for y := 0 to (cTileWidth - 1) do
    for x := 0 to (cTileWidth - 1) do
    begin
      map_value := cDitheringMap2[(x and (cTileWidth - 1)) + ((y and (cTileWidth - 1)) shl 3)];
      DeviseBestMixingPlan2(plan, ATile^.RGBPixels[y,x,0], ATile^.RGBPixels[y,x,1], ATile^.RGBPixels[y,x,2]);
      map_value := map_value * plan.Count div 64;
      ATile^.PalPixels[SpritePal, y,x] := plan.List[map_value];
    end;
end;

function CompareCMU(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PInteger(Item2)^, PInteger(Item1)^);
end;

function CompareCMULuma(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PInteger(UserParameter)[PInteger(Item1)[1]], PInteger(UserParameter)[PInteger(Item2)[1]]);
end;

procedure TMainForm.PreDitherTiles(AFrame: PFrame);
var
  sx, sy, i, tx, ty: Integer;
  CMUsage: array[0..cTotalColors - 1] of packed record
    Count, Index: Integer;
  end;
  Tile_: PTile;
begin
  for sy := 0 to cTileMapHeight - 1 do
    for sx := 0 to cTileMapWidth - 1 do
    begin
      Tile_ := @FTiles[AFrame^.TileMap[sy, sx].GlobalTileIndex];

      if not Tile_^.Active then
        Exit;

      DitherTile(Tile_, FColorMap, False);

      for i := 0 to High(CMUsage) do
      begin
        CMUsage[i].Count := 0;
        CMUsage[i].Index := i;
      end;

      for ty := 0 to (cTileWidth - 1) do
        for tx := 0 to (cTileWidth - 1) do
          Inc(CMUsage[Tile_^.PalPixels[False, ty,tx]].Count, Tile_^.AveragedCount);

      QuickSort(CMUsage[0], 0, High(CMUsage), SizeOf(CMUsage[0]), @CompareCMU);
      QuickSort(CMUsage[0], 0, cTilePaletteSize - 1, SizeOf(CMUsage[0]), @CompareCMULuma, @FColorMapLuma[0]);

      for i := 0 to cTilePaletteSize - 1 do
      begin
        Tile_^.PaletteIndexes[i] := CMUsage[cTilePaletteSize - 1 - i].Index;
        Tile_^.PaletteRGB[i] := FColorMap[CMUsage[cTilePaletteSize - 1 - i].Index];
      end;

      DitherTile(Tile_, Tile_^.PaletteRGB, False);

      Move(Tile_^.PalPixels[False, 0, 0], Tile_^.PalPixels[True, 0, 0], cTileWidth * cTileWidth);
    end;
end;

procedure TMainForm.FindBestKeyframePalette(AKeyFrame: PKeyFrame);
var sx, sy, tx, ty, i, idx, idx2: Integer;
    GTile: PTile;
    CMUsage: array[0..cTotalColors - 1] of packed record
      Count, Index: Integer;
    end;
begin
  for i := 0 to High(CMUsage) do
  begin
    CMUsage[i].Count := 0;
    CMUsage[i].Index := i;
  end;

  for i := 0 to High(FFrames) do
    if FFrames[i].KeyFrame = AKeyFrame then
    begin
      for sy := 0 to cTileMapHeight - 1 do
        for sx := 0 to cTileMapWidth - 1 do
        begin
          GTile := @FTiles[FFrames[i].TileMap[sy, sx].GlobalTileIndex];

          for ty := 0 to cTileWidth - 1 do
            for tx := 0 to cTileWidth - 1 do
              Inc(CMUsage[GTile^.PaletteIndexes[GTile^.PalPixels[False, ty, tx]]].Count);
        end;
    end;

  QuickSort(CMUsage[0], 0, High(CMUsage), SizeOf(CMUsage[0]), @CompareCMU);

  QuickSort(CMUsage[0], 0, (cTilePaletteSize - cKeyframeFixedColors) * 2 - 1, SizeOf(CMUsage[0]), @CompareCMULuma, @FColorMapLuma[0]);
  for i := 0 to cTilePaletteSize - 1 do
  begin
    if i < cKeyframeFixedColors then
    begin
      idx := i;
      idx2 := idx;
    end
    else if i >= (cTilePaletteSize - cKeyframeFixedColors) then
    begin
      idx := i + cTilePaletteSize - cKeyframeFixedColors * 2;
      idx2 := idx;
    end
    else
    begin
      idx := (i - cKeyframeFixedColors) * 2 + cKeyframeFixedColors;
      idx2 := idx + 1;
    end;

    if cInvertSpritePalette then
      idx2 := (cTilePaletteSize - cKeyframeFixedColors) * 2 - 1 - idx; // invert second palette

    AKeyFrame^.PaletteIndexes[False, i] := CMUsage[idx].Index;
    AKeyFrame^.PaletteIndexes[True, i] := CMUsage[idx2].Index;
    AKeyFrame^.PaletteRGB[False, i] := FColorMap[AKeyFrame^.PaletteIndexes[False, i]];
    AKeyFrame^.PaletteRGB[True, i] := FColorMap[AKeyFrame^.PaletteIndexes[True, i]];
  end;
end;

procedure TMainForm.FinalDitherTiles(AFrame: PFrame);
var
  OrigTile: TTile;
  sx, sy: Integer;
  KF: PKeyFrame;
  SpritePal: Boolean;
  cmp: array[Boolean] of Single;
  Tile_: PTile;
begin
  for sy := 0 to cTileMapHeight - 1 do
    for sx := 0 to cTileMapWidth - 1 do
    begin
      Tile_ := @FTiles[AFrame^.TileMap[sy, sx].GlobalTileIndex];

      if not Tile_^.Active then
        Exit;

      OrigTile := Tile_^;
      ComputeTileDCT(@OrigTile, True, False, False, OrigTile.PaletteRGB);

      KF := AFrame^.KeyFrame;

      for SpritePal := False to True do
      begin
        DitherTile(Tile_, KF^.PaletteRGB[SpritePal], SpritePal);

        ComputeTileDCT(Tile_, True, SpritePal, False, KF^.PaletteRGB[SpritePal]);
        cmp[SpritePal] := CompareTilesDCT(Tile_, @OrigTile, SpritePal, False);
      end;

      SpritePal := cmp[True] < cmp[False];

      Move(KF^.PaletteIndexes[SpritePal], Tile_^.PaletteIndexes, SizeOf(Tile_^.PaletteIndexes));
      Move(KF^.PaletteRGB[SpritePal], Tile_^.PaletteRGB, SizeOf(Tile_^.PaletteRGB));
      Move(Tile_^.PalPixels[SpritePal, 0, 0], Tile_^.PalPixels[not SpritePal, 0, 0], cTileWidth * cTileWidth);

      AFrame^.TileMap[sy, sx].SpritePal := SpritePal;
      AFrame^.Tiles[sx + sy * cTileMapWidth] := Tile_^;
    end;
end;

procedure TMainForm.LoadTiles;
var
  i,j,x,y: Integer;
  tileCnt: Integer;
begin
  tileCnt := Length(FFrames) * cMaxTiles;
  SetLength(FTiles, tileCnt);

  for i := 0 to High(FFrames) do
  begin
    tileCnt := i * cMaxTiles;
    for j := 0 to cMaxTiles - 1 do
      Move(FFrames[i].Tiles[j], FTiles[tileCnt+j], Sizeof(FFrames[i].Tiles[j]));
    for y := 0 to (cTileMapHeight - 1) do
      for x := 0 to (cTileMapWidth - 1) do
        Inc(FFrames[i].TileMap[y,x].GlobalTileIndex, tileCnt);
  end;
end;

function TMainForm.ColorCompare(r1, g1, b1, r2, g2, b2: Integer): Int64;
var
  luma1, luma2, lumadiff, diffR, diffG, diffB: Int64;
begin
  luma1 := r1 * 299 + g1 * 587 + b1 * 114;
  luma2 := r2 * 299 + g2 * 587 + b2 * 114;
  lumadiff := luma1 - luma2;
  diffR := r1 - r2;
  diffG := g1 - g2;
  diffB := b1 - b2;
  Result := diffR * diffR * (299 * 299 * 3 div 4);
  Result += diffG * diffG * (587 * 587 * 3 div 4);
  Result += diffB * diffB * (114 * 114 * 3 div 4);
  Result += lumadiff * lumadiff;
end;

procedure TMainForm.RGBToYUV(r, g, b: Integer;  GammaCor: Boolean; out y, u, v: Single); inline;
var
  sr, sg, sb: Single;
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

procedure TMainForm.ComputeTileDCT(ATile: PTile; FromPal, SpritePal, GammaCor: Boolean; const pal: array of Integer);
var
  col, u, v, x, y, cpn: Integer;
  Cu, Cv, z, s, q, coeff: Single;
  YUVPixels: array[0..cTileWidth-1,0..cTileWidth-1,0..2] of Single;
begin
  if FromPal then
  begin
    for y := 0 to (cTileWidth - 1) do
      for x := 0 to (cTileWidth - 1) do
      begin
        col := pal[ATile^.PalPixels[SpritePal,y,x]];
        RGBToYUV(col and $ff, (col shr 8) and $ff, (col shr 16) and $ff, GammaCor,
                 YUVPixels[y,x,0], YUVPixels[y,x,1], YUVPixels[y,x,2]);
      end;
  end
  else
  begin
    for y := 0 to (cTileWidth - 1) do
      for x := 0 to (cTileWidth - 1) do
        RGBToYUV(ATile^.RGBPixels[y,x,0], ATile^.RGBPixels[y,x,1], ATile^.RGBPixels[y,x,2], GammaCor,
                 YUVPixels[y,x,0], YUVPixels[y,x,1], YUVPixels[y,x,2]);
  end;

  for cpn := 0 to 2 do
    for v := 0 to (cTileWidth - 1) do
      for u := 0 to (cTileWidth - 1) do
      begin
		    z := 0.0;

      	if (u = 0) then Cu := 1.0 / sqrt(2.0) else Cu := 1.0;
      	if (v = 0) then Cv := 1.0 / sqrt(2.0) else Cv := 1.0;

        for y := 0 to (cTileWidth - 1) do
          for x := 0 to (cTileWidth - 1) do
		      begin
			      s := YUVPixels[y,x,cpn];

			      q := s * cos(Single((2*x+1) * u) * PI/16.0) * cos(Single((2*y+1) * v) * PI/16.0);

			      z += q;
          end;

		    coeff := 0.25 * Cu * Cv * z;

        if cpn = 0 then
          coeff := coeff * 255.0 / cLumaQuantisation[v,u]
        else
          coeff := coeff * 255.0 / cChromaQuantisation[v,u];

        ATile^.DCTCoeffs[SpritePal,v,u,cpn] := coeff;
	    end;
end;

procedure TMainForm.HMirrorPalTile(ATile: PTile; SpritePal: Boolean);
var
  i, j: Integer;
  v: Byte;
begin
  for i := 0 to cTileWidth div 2 - 1  do
    for j := 0 to cTileWidth - 1 do
    begin
      v := ATile^.PalPixels[SpritePal, i, j];
      ATile^.PalPixels[SpritePal, i, j] := ATile^.PalPixels[SpritePal, cTileWidth - 1 - i, j];
      ATile^.PalPixels[SpritePal, cTileWidth - 1 - i, j] := v;
    end;
end;

procedure TMainForm.VMirrorPalTile(ATile: PTile; SpritePal: Boolean);
var
  i, j: Integer;
  v: Byte;
begin
  for i := 0 to cTileWidth div 2 - 1  do
    for j := 0 to cTileWidth - 1 do
    begin
      v := ATile^.PalPixels[SpritePal, j, i];
      ATile^.PalPixels[SpritePal, j, i] := ATile^.PalPixels[SpritePal, j, cTileWidth - 1 - i];
      ATile^.PalPixels[SpritePal, j, cTileWidth - 1 - i] := v;
    end;
end;

function TMainForm.CompareTilesDCT(ATileA, ATileB: PTile; SpritePalA, SpritePalB: Boolean): Single;
var
  i: Integer;
  pa, pb: PSingle;
begin
  Result := 0;

  pa := @ATileA^.DCTCoeffs[SpritePalA, 0, 0, 0];
  pb := @ATileB^.DCTCoeffs[SpritePalB, 0, 0, 0];

  for i := 0 to cTileWidth * 3 - 1 do
  begin
    // unroll by cTileWidth
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
    Result += sqr(pa^ - pb^); Inc(pa); Inc(pb);
  end;

  Result := sqrt(Result / (sqr(cTileWidth) * 3));
end;

procedure TMainForm.LoadFrame(AFrame: PFrame; ABitmap: TBitmap);
var
  i, j, px, r, g, b, ti, tx, ty: Integer;
begin
  for j := 0 to (cTileMapHeight - 1) do
    for i := 0 to (cTileMapWidth - 1) do
    begin
      AFrame^.TileMap[j, i].GlobalTileIndex := 32 * j + i;
      AFrame^.TileMap[j, i].HMirror := False;
      AFrame^.TileMap[j, i].VMirror := False;
      AFrame^.TileMap[j, i].SpritePal := False;
      AFrame^.TileMap[j, i].Smoothed := False;
    end;

  for j := 0 to (cScreenHeight - 1) do
    for i := 0 to (cScreenWidth - 1) do
      begin
        px := ColorToRGB(ABitmap.Canvas.Pixels[i, j]);

        r := px and $ff;
        g := (px shr 8) and $ff;
        b := (px shr 16) and $ff;

        ti := 32 * (j shr 3) + (i shr 3);
        tx := i and (cTileWidth - 1);
        ty := j and (cTileWidth - 1);

        AFrame^.Tiles[ti].RGBPixels[tx, ty, 0] := r;
        AFrame^.Tiles[ti].RGBPixels[tx, ty, 1] := g;
        AFrame^.Tiles[ti].RGBPixels[tx, ty, 2] := b;
        AFrame^.Tiles[ti].PalPixels[False, tx, ty] := 0;
        AFrame^.Tiles[ti].PalPixels[True, tx, ty] := 0;
        AFrame^.Tiles[ti].Active := True;
        AFrame^.Tiles[ti].AveragedCount := 1;
        AFrame^.Tiles[ti].TmpIndex := -1;
      end;
end;

procedure TMainForm.Render(AFrameIndex: Integer; dithered: Boolean; page: Integer);
var
  i, j, r, g, b, ti, tx, ty, col: Integer;
  pTiles, pDest, p: PInteger;
  tilePtr: PTile;
  TMItem: TTileMapItem;
  Frame: PFrame;
begin
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
          ti := 32 * (j shr 3) + (i shr 3) + cMaxTiles * page;

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
            if dithered then
            begin
              col := FColorMap[FTiles[ti].PaletteIndexes[FTiles[ti].PalPixels[False, tx, ty]]];
              b := (col shr 16) and $ff; g := (col shr 8) and $ff; r := col and $ff;
            end
            else
            begin
              r := FTiles[ti].RGBPixels[tx, ty, 0];
              g := FTiles[ti].RGBPixels[tx, ty, 1];
              b := FTiles[ti].RGBPixels[tx, ty, 2];
            end;

            if not FTiles[ti].Active then
            begin
              r := 255;
              g := 0;
              b := 255;
				    end;
          end;

          p := pTiles;
          Inc(p, i);
          p^ := RGBToColor(b, g, r);

          if j >= cScreenHeight then Continue;

          TMItem := Frame^.TileMap[j shr 3, i shr 3];
          if TMItem.Smoothed then
          begin
            TMItem := FFrames[AFrameIndex - cSmoothingPrevFrame].TileMap[j shr 3, i shr 3];
            TMItem.GlobalTileIndex := Frame^.TilesIndexes[TMItem.FrameTileIndex];
          end;
{$if true}
          ti := TMItem.GlobalTileIndex;
          tilePtr := @FTiles[ti];
{$else}
          ti := (j shr 3) * cTileMapWidth + (i shr 3);
          tilePtr := @AFrame^.Tiles[ti];
{$endif}

          if TMItem.HMirror then tx := cTileWidth - 1 - tx;
          if TMItem.VMirror then ty := cTileWidth - 1 - ty;

          if dithered then
          begin
            col := FColorMap[Frame^.KeyFrame^.PaletteIndexes[TMItem.SpritePal, tilePtr^.PalPixels[False, tx, ty]]];
            b := (col shr 16) and $ff; g := (col shr 8) and $ff; r := col and $ff;
          end
          else
          begin
            r := tilePtr^.RGBPixels[tx, ty, 0];
            g := tilePtr^.RGBPixels[tx, ty, 1];
            b := tilePtr^.RGBPixels[tx, ty, 2];
          end;

          p := pDest;
          Inc(p, i);
          p^ := RGBToColor(b, g, r);
        end;
    end;
  finally
    imgTiles.Picture.Bitmap.EndUpdate;
    imgDest.Picture.Bitmap.EndUpdate;
  end;
end;

function TMainForm.GetGlobalTileCount: Integer;
var i: Integer;
begin
  Result := 0;
  for i := 0 to High(FTiles) do
    if FTiles[i].Active then
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

function TMainForm.GetMaxFrameTileCount(AKeyFrame: PKeyFrame): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(FFrames) do
    if FFrames[i].KeyFrame = AKeyFrame then
      Result := Max(Result, GetFrameTileCount(@FFrames[i]));
end;

procedure TMainForm.MergeTilesByPaletteIndexes(const TileIndexes: array of Integer; TileCount: Integer);
var
  i, j, k, tx, ty, Tile0: Integer;
  PalOccurences: array[0..cTilePaletteSize-1] of Integer;
begin
  if TileCount < 2 then
    Exit;

  Tile0 := TileIndexes[0];

  for ty := 0 to (cTileWidth - 1) do
    for tx := 0 to (cTileWidth - 1) do
    begin
      FillChar(PalOccurences, cTilePaletteSize * SizeOf(Integer), 0);
      for i := 0 to TileCount - 1 do
        Inc(PalOccurences[FTiles[TileIndexes[i]].PalPixels[False, ty, tx]]);

      j := -1;
      k := 0;
      for i := 0 to High(PalOccurences) do
      begin
        if PalOccurences[i] > k then
        begin
          k := PalOccurences[i];
          j := i;
        end;
      end;

      FTiles[Tile0].PalPixels[False, ty, tx] := j;
		end;

  for k := 1 to TileCount - 1 do
  begin
    j := TileIndexes[k];

    Inc(FTiles[Tile0].AveragedCount, FTiles[j].AveragedCount);

    FTiles[j].Active := False;
    FTiles[j].TmpIndex := Tile0;

    FillChar(FTiles[j].RGBPixels, SizeOf(FTiles[j].RGBPixels), 0);
    FillChar(FTiles[j].PalPixels, SizeOf(FTiles[j].PalPixels), 0);
  end;

  for k := 0 to High(FFrames) do
    for j := 0 to (cTileMapHeight - 1) do
        for i := 0 to (cTileMapWidth - 1) do
          if FTiles[FFrames[k].TileMap[j, i].GlobalTileIndex].TmpIndex = Tile0 then
            FFrames[k].TileMap[j, i].GlobalTileIndex := Tile0;
end;

function TMainForm.IsTileInFrame(AFrame: PFrame; ATile: Integer): Boolean;
var
  x, y: Integer;
begin
  Result := False;
  for y := 0 to cTileMapHeight - 1 do
    for x := 0 to cTileMapWidth - 1 do
      if AFrame^.TileMap[y, x].GlobalTileIndex = ATile then
      begin
        Result := True;
        Break;
      end;
end;

function CompareTRUseCountInv(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PInteger(Item2)^, PInteger(Item1)^);
end;

procedure TMainForm.DoFrameTiling(AFrame: PFrame; DesiredNbTiles, RestartCount: Integer);
var
  TilesRepo: array of packed record
    UseCount: Integer; // must stay first (cf CompareTRUseCountInv)
    GlobalIndex: Integer;
    Tile: PTile;
  end;

  function FindBestComparison(FrameTile: PTile; out GlobalIndex: Integer; out SpritePal: Boolean): Single;
  var
    i, gi: Integer;
    cmp: Single;
    isp, sp: Boolean;
  begin
    gi := -1;
    sp := False;
    Result := MaxSingle;
    for i := 0 to High(TilesRepo) do
      if TilesRepo[i].GlobalIndex >= 0 then
        for isp := False to True do
        begin
          cmp := CompareTilesDCT(FrameTile, TilesRepo[i].Tile, False, isp);
          if cmp < Result then
          begin
            Result := cmp;
            gi := TilesRepo[i].GlobalIndex;
            sp := isp;
          end;
        end;

    GlobalIndex := gi;
    SpritePal := sp;
  end;

var
  Cnt, i, j, k, sy, sx, ty, tx: Integer;
  FrameTile: TTile;
  cmp, cmpH, cmpV, cmpHV, rcmp: Single;
  idx, idxH, idxV, idxHV: Integer;
  pass, sp, spH, spV, spHV, isp: Boolean;
  Dataset: TByteDynArray2;
  XYC: TIntegerDynArray;
begin
  SetLength(TilesRepo, Length(FTiles));
  j := 0;
  for i := 0 to High(FTiles) do
    if FTiles[i].Active then
    begin
      TilesRepo[j].Tile := @FTiles[i];
      TilesRepo[j].GlobalIndex := i;
      TilesRepo[j].UseCount := 0;
      Inc(j);
    end;
  SetLength(TilesRepo, j);

  for pass := False to True do
  begin
    for sy := 0 to cTileMapHeight - 1 do
      for sx := 0 to cTileMapWidth - 1 do
      begin
        FrameTile := AFrame^.Tiles[sx + sy * cTileMapWidth];

        ComputeTileDCT(@FrameTile, True, False, cGammaCorrectFrameTiling, FrameTile.PaletteRGB);
        cmp := FindBestComparison(@FrameTile, idx, sp);

        HMirrorPalTile(@FrameTile, False);
        ComputeTileDCT(@FrameTile, True, False, cGammaCorrectFrameTiling, FrameTile.PaletteRGB);
        cmpH := FindBestComparison(@FrameTile, idxH, spH);

        VMirrorPalTile(@FrameTile, False);
        ComputeTileDCT(@FrameTile, True, False, cGammaCorrectFrameTiling, FrameTile.PaletteRGB);
        cmpHV := FindBestComparison(@FrameTile, idxHV, spHV);

        HMirrorPalTile(@FrameTile, False);
        ComputeTileDCT(@FrameTile, True, False, cGammaCorrectFrameTiling, FrameTile.PaletteRGB);
        cmpV := FindBestComparison(@FrameTile, idxV, spV);

        rcmp := minvalue([cmp, cmpH, cmpHV, cmpV]);

        AFrame^.TileMap[sy, sx].GlobalTileIndex := idx;
        AFrame^.TileMap[sy, sx].SpritePal := sp;
        AFrame^.TileMap[sy, sx].HMirror := False;
        AFrame^.TileMap[sy, sx].VMirror := False;
        if rcmp = cmpH then
        begin
          AFrame^.TileMap[sy, sx].GlobalTileIndex := idxH;
          AFrame^.TileMap[sy, sx].SpritePal := spH;
          AFrame^.TileMap[sy, sx].HMirror := True;
        end
        else if rcmp = cmpV then
        begin
          AFrame^.TileMap[sy, sx].GlobalTileIndex := idxV;
          AFrame^.TileMap[sy, sx].SpritePal := spV;
          AFrame^.TileMap[sy, sx].VMirror := True;
        end
        else if rcmp = cmpHV then
        begin
          AFrame^.TileMap[sy, sx].GlobalTileIndex := idxHV;
          AFrame^.TileMap[sy, sx].SpritePal := spHV;
          AFrame^.TileMap[sy, sx].HMirror := True;
          AFrame^.TileMap[sy, sx].VMirror := True;
        end;
      end;

    if pass or (GetFrameTileCount(AFrame) <= DesiredNbTiles) then
      Break;

    j := Length(TilesRepo);
    for i := 0 to High(TilesRepo) do
    begin
      TilesRepo[i].UseCount := 0;
      for sy := 0 to cTileMapHeight - 1 do
        for sx := 0 to cTileMapWidth - 1 do
          if TilesRepo[i].GlobalIndex = AFrame^.TileMap[sy, sx].GlobalTileIndex then
            Inc(TilesRepo[i].UseCount);

      if TilesRepo[i].UseCount = 0 then
      begin
        TilesRepo[i].GlobalIndex := -1;
        Dec(j);
      end;
    end;
    QuickSort(TilesRepo[0], 0, High(TilesRepo), SIzeOf(TilesRepo[0]), @CompareTRUseCountInv);

    SetLength(Dataset, j, sqr(cTileWidth));
    j := 0;
    for i := 0 to High(TilesRepo) do
    begin
      if TilesRepo[i].GlobalIndex < 0 then
        Continue;

      for ty := 0 to cTileWidth - 1 do
        for tx := 0 to cTileWidth - 1 do
          Dataset[j, tx + ty * cTileWidth] := TilesRepo[i].Tile^.PalPixels[False, ty, tx];
      Inc(j);
    end;

    Assert(j = Length(Dataset));

    ComputeKModes(Dataset, DesiredNbTiles, MaxInt, RestartCount, cTilePaletteSize, 1, XYC);

    for j := 0 to DesiredNbTiles - 1 do
    begin
      Cnt := 0;
      k := 0;
      for i := 0 to High(TilesRepo) do
      begin
        if TilesRepo[i].GlobalIndex = -1 then
          Continue;

        if XYC[k] = j then
        begin
          if Cnt <> 0 then // first has hightest use count, so keep that one
            TilesRepo[i].GlobalIndex := -2;
          Inc(Cnt);
        end;

        Inc(k);
      end;
    end;
  end;
end;

procedure TMainForm.DoTemporalSmoothing(AFrame, APrevFrame: PFrame; Y: Integer; Strength: Single);
var
  sx: Integer;
  cmp: Single;
  TMI, PrevTMI: PTileMapItem;
  Tile_, PrevTile: TTile;
begin
  for sx := 0 to cTileMapWidth - 1 do
  begin
    PrevTMI := @APrevFrame^.TileMap[Y, sx];
    TMI := @AFrame^.TileMap[Y, sx];

    if PrevTMI^.FrameTileIndex >= Length(AFrame^.TilesIndexes) then
      Continue;

    PrevTile := FTiles[AFrame^.TilesIndexes[PrevTMI^.FrameTileIndex]];
    Tile_ := FTiles[AFrame^.TilesIndexes[TMI^.FrameTileIndex]];

    if PrevTMI^.HMirror then HMirrorPalTile(@PrevTile, False);
    if PrevTMI^.VMirror then VMirrorPalTile(@PrevTile, False);
    if TMI^.HMirror then HMirrorPalTile(@Tile_, False);
    if TMI^.VMirror then VMirrorPalTile(@Tile_, False);

    ComputeTileDCT(@PrevTile, True, False, cGammaCorrectSmoothing, AFrame^.KeyFrame^.PaletteRGB[PrevTMI^.SpritePal]);
    ComputeTileDCT(@Tile_, True, False, cGammaCorrectSmoothing, AFrame^.KeyFrame^.PaletteRGB[TMI^.SpritePal]);

    cmp := CompareTilesDCT(@Tile_, @PrevTile, False, False);

    if Abs(cmp) <= Strength then
    begin
      TMI^.GlobalTileIndex := AFrame^.TilesIndexes[PrevTMI^.FrameTileIndex];
      TMI^.FrameTileIndex := PrevTMI^.FrameTileIndex;
      TMI^.HMirror := PrevTMI^.HMirror;
      TMI^.VMirror := PrevTMI^.VMirror;
      TMI^.SpritePal := PrevTMI^.SpritePal;
      TMI^.Smoothed := True;
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


function TMainForm.GetMostFrequent(Arr: array of Byte): Integer;
var
  Freq: array[0..255] of Integer;
  i: Integer;
  best: Integer;
begin
  FillDWord(Freq, 256, 0);

  for i := 0 to High(Arr) do
    Inc(Freq[Arr[i]]);

  Result := -1;
  best := 0;
  for i := 0 to 255 do
    if Freq[i] > best then
    begin
      best := Freq[i];
      Result := i;
    end;
end;

procedure TMainForm.DoGlobalTiling(DesiredNbTiles, RestartCount: Integer);
var
  Dataset: TByteDynArray2;
  XYC: TIntegerDynArray;
  i, j, k, x, y, Cnt: Integer;
  ToMerge: array of Integer;
  WasActive: TBooleanDynArray;
begin
  SetLength(Dataset, Length(FTiles), sqr(cTileWidth));
  SetLength(WasActive, Length(FTiles));

  Cnt := 0;
  for i := 0 to High(FTiles) do
  begin
    WasActive[i] := FTiles[i].Active;

    if not FTiles[i].Active then
      Continue;

    j := 0;
    for y := 0 to cTileWidth - 1 do
      for x := 0 to cTileWidth - 1 do
      begin
        Dataset[Cnt, j] := FTiles[i].PalPixels[False, y, x];
        Inc(j);
      end;

    Inc(Cnt);
  end;

  SetLength(Dataset, Cnt);

  ComputeKModes(Dataset, DesiredNbTiles, MaxInt, RestartCount, cTilePaletteSize, 0, XYC);

  SetLength(ToMerge, Length(FTiles));

  for j := 0 to DesiredNbTiles - 1 do
  begin
    Cnt := 0;
    k := 0;
    for i := 0 to High(FTiles) do
    begin
      if not WasActive[i] then
        Continue;

      if XYC[k] = j then
      begin
        ToMerge[Cnt] := i;
        Inc(Cnt);
      end;

      Inc(k);
    end;

    if Cnt >= 2 then
      MergeTilesByPaletteIndexes(ToMerge, Cnt);
  end;
end;

function CompareTileUseCountRev(Item1, Item2, UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PTile(Item2)^.UseCount, PTile(Item1)^.UseCount);
  if Result = 0 then
    Result := CompareValue(PTile(Item1)^.TmpIndex, PTile(Item2)^.TmpIndex);
end;

procedure TMainForm.ReindexTiles;
var
  i, j, x, y, cnt: Integer;
  IdxMap: TIntegerDynArray;
begin
  cnt := 0;
  for i := 0 to High(FTiles) do
  begin
    FTiles[i].TmpIndex := i;
    if FTiles[i].Active then
      Inc(cnt);
  end;

  for i := High(FTiles) downto 0 do
    if not FTiles[i].Active then
    begin
      for j := i to High(FTiles) - 1 do
        if FTiles[j + 1].Active then
          Move(FTiles[j + 1], FTiles[j], Sizeof(FTiles[j]))
        else
          FTiles[j].Active := False;
      FTiles[High(FTiles)].Active := False;
    end;

  SetLength(IdxMap, Length(FTiles));
  FillDWord(IdxMap[0], High(FTiles), $ffffffff);

  SetLength(FTiles, cnt);
  QuickSort(FTiles[0], 0, High(FTiles), SizeOf(TTile), @CompareTileUseCountRev);

  for i := 0 to High(FTiles) do
    IdxMap[FTiles[i].TmpIndex] := i;

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
  SetLength(AFrame^.TilesIndexes, cMaxTiles);

  cnt := 0;
  for i := 0 to High(FTiles) do
    if FTiles[i].Active then
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

  QuickSort(AFrame^.TilesIndexes[0], 0, High(AFrame^.TilesIndexes), SizeOf(AFrame^.TilesIndexes[0]), @CompareTilesIndexes);

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
    IntToStr(cClocksPerSample) + ' -cpuf ' + IntToStr(cZ80Clock) + ' -rto 3 -a ' + IntToStr(Volume) + ' -r 1024 -precision 8 "' + AFN + '"');
  Process.ShowWindow := swoHIDE;
  Process.Priority := ppIdle;

  i := 0;
  internalRuncommand(Process, Output, ErrOut, i); // destroys Process

  Result := AFN + '.pcmenc';
end;

function CompareTMICache(Item1,Item2,UserParameter:Pointer):Integer;
begin
  Result := CompareValue(PInteger(Item2)^, PInteger(Item1)^);
end;

procedure TMainForm.Save(ADataStream, ASoundStream: TStream);
var pp, pp2, i, j, k, sz, x, y, idx, frameStart, prevTileIndex, diffTileIndex, prevDTI, sameCount, skipCount: Integer;
    rawTMI, tmiCacheIdx, best, awaitingCacheIdx, awaitingCount, tiZ80Cycles: Integer;
    smoothed, tiVBlank, vbl: Boolean;
    prevKF: PKeyFrame;
    tilesPlanes: array[0..cTileWidth - 1, 0..3] of Byte;
    tmi: PTileMapItem;
    rawTMIs: array[0..cMaxTiles] of Integer;
    tmiCache: array[0..4095] of packed record
      UsedCount: Integer;
      RawTMI: Integer;
    end;
    tileIdxStream: TMemoryStream;

  function CurrentLineJitter: Integer;
  begin
    Result := round(IfThen(tiVBlank, -cLineJitter * cCyclesPerLine, cLineJitter * cCyclesPerLine));
  end;

  procedure HandleTileIndexZ80Cycles(ACycleAdd: Integer);
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
        HandleTileIndexZ80Cycles(cTileIndexesTimings[tiVBlank, 1]);
      tileIdxStream.WriteByte(prevDTI - 1);
      if not vbl then
        HandleTileIndexZ80Cycles(cTileIndexesTimings[tiVBlank, 1]);
    end
    else if sameCount <> 0 then
    begin
      priorCount := sameCount;
      cyAdd := cTileIndexesTimings[tiVBlank, 2] + priorCount * cTileIndexesTimings[tiVBlank, 3];

      while (tiZ80Cycles + cyAdd > cCyclesPerDisplayPhase[tiVBlank] + CurrentLineJitter) and (priorCount > 0) do
      begin
        Dec(priorCount);
        cyAdd := cTileIndexesTimings[tiVBlank, 2] + priorCount * cTileIndexesTimings[tiVBlank, 3];
      end;

      if priorCount > 0 then
      begin
        HandleTileIndexZ80Cycles(cTileIndexesTimings[tiVBlank, 2] + priorCount * cTileIndexesTimings[tiVBlank, 3]);
        tileIdxStream.WriteByte(cTileIndexesRepeatStart + priorCount - 1);
      end;

      sameCount -= priorCount;
      if sameCount > 0 then
      begin
        if vbl then
          HandleTileIndexZ80Cycles(cTileIndexesTimings[tiVBlank, 2] + sameCount * cTileIndexesTimings[tiVBlank, 3]);
        tileIdxStream.WriteByte(cTileIndexesRepeatStart + sameCount - 1);
        if not vbl then
          HandleTileIndexZ80Cycles(cTileIndexesTimings[tiVBlank, 2] + sameCount * cTileIndexesTimings[tiVBlank, 3]);
      end;
    end;
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
  // leave the size of one tile for index

  for x := 0 to cTileWidth - 1 do
    ADataStream.WriteDWord(0);

  // tiles

  for i := 0 to High(FTiles) do
  begin
    FillChar(tilesPlanes, Sizeof(tilesPlanes), 0);
    for y := 0 to cTileWidth - 1 do
    begin
      for x := 0 to cTileWidth - 1 do
      begin
        idx := FTiles[i].PalPixels[False, x, y];
        tilesPlanes[y, 0] := tilesPlanes[y, 0] or (((idx and 1) shl 7) shr x);
        tilesPlanes[y, 1] := tilesPlanes[y, 1] or (((idx and 2) shl 6) shr x);
        tilesPlanes[y, 2] := tilesPlanes[y, 2] or (((idx and 4) shl 5) shr x);
        tilesPlanes[y, 3] := tilesPlanes[y, 3] or (((idx and 8) shl 4) shr x);
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
      sz := SizeOf(FFrames[i].KeyFrame^.PaletteIndexes);
      ADataStream.WriteByte(sz);
      ADataStream.WriteBuffer(FFrames[i].KeyFrame^.PaletteIndexes, sz);
    end
    else
    begin
      ADataStream.WriteByte(0);
    end;

    pp := ADataStream.Position;
    // tiles indexes

    tileIdxStream := TMemoryStream.Create;
    try
      tiZ80Cycles :=  Round((cTileIndexesInitialLine - cScreenHeight) * cCyclesPerLine);
      tiVBlank := True;

      prevTileIndex := -1;
      prevDTI := -1;
      sameCount := 0;
      for j := 0 to High(FFrames[i].TilesIndexes) do
      begin
        diffTileIndex := FFrames[i].TilesIndexes[j] - prevTileIndex;

        if (diffTileIndex <> 1) or (diffTileIndex <> prevDTI) or
            (diffTileIndex >= cTileIndexesMaxDiff) or (sameCount >= cTileIndexesMaxRepeat) or
            ((FFrames[i].TilesIndexes[j] + cTileIndexesTileOffset) mod cTilesPerBank = 0) then // don't change bank while repeating
        begin
          DoTileIndex;
          sameCount := 1;
        end
        else
        begin
          Inc(sameCount);
        end;

        if (prevTileIndex = -1) or (diffTileIndex >= cTileIndexesMaxDiff) or (diffTileIndex < 0) or
            ((FFrames[i].TilesIndexes[j] + cTileIndexesTileOffset) div cTilesPerBank <>
             (prevTileIndex + cTileIndexesTileOffset) div cTilesPerBank) then // any bank change must be a direct value
        begin
          vbl := tiVBlank;
          if vbl then
            HandleTileIndexZ80Cycles(cTileIndexesTimings[tiVBlank, 0]);

          tileIdxStream.WriteByte(cTileIndexesDirectValue);
          tileIdxStream.WriteWord(FFrames[i].TilesIndexes[j] + cTileIndexesTileOffset);

          if not vbl then
            HandleTileIndexZ80Cycles(cTileIndexesTimings[tiVBlank, 0]);

          diffTileIndex := -1;
          sameCount := 0;
        end;

        prevTileIndex := FFrames[i].TilesIndexes[j];
        prevDTI := diffTileIndex;
      end;

      DoTileIndex;
      tileIdxStream.WriteByte(cTileIndexesTerminator);
      tileIdxStream.WriteByte(cMaxTilesPerFrame - Length(FFrames[i].TilesIndexes));

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

    DebugLn(['TileIndexes size: ', ADataStream.Position - pp]);
    pp := ADataStream.Position;
    // tilemap

    for j := 0 to High(tmiCache) do
    begin
      tmiCache[j].UsedCount := 0;
      tmiCache[j].RawTMI := j;
    end;

    for y := 0 to cTileMapHeight - 1 do
      for x := 0 to cTileMapWidth - 1 do
      begin
        tmi := @FFrames[i].TileMap[y, x];
        rawTMI := (tmi^.FrameTileIndex + cTileMapIndicesOffset[i and 1]) and $1ff;
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
    for j := 0 to cMaxTiles - 1 do
    begin
      smoothed := FFrames[i].TileMap[j div cTileMapWidth, j mod cTileMapWidth].Smoothed;
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
      end
    end;

    DoTilemapTileCommand(True, True);

    ADataStream.WriteByte(cTileMapTerminator);

    DebugLn(['TM size: ', ADataStream.Position - pp]);
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

initialization
  InitGammaLuts;
end.

