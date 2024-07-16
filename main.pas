unit main;

{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils, strutils, types, Math, FileUtil, typinfo, LazLogger,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Spin, Menus, IntfGraphics, Buttons,
  FPimage, FPCanvas, FPWritePNG, GraphType, MTProcs, extern, tilingencoder, utils;

type
  { TMainForm }

  TMainForm = class(TForm)
    btnInput: TButton;
    btnGTM: TButton;
    btnRunAll: TButton;
    btnPM: TButton;
    cbxClusMethod: TComboBox;
    cbxVisMode: TComboBox;
    cbxGlobMode: TComboBox;
    cbxDitheringMode: TComboBox;
    cbxFTMode: TComboBox;
    chkClusterFromPal: TCheckBox;
    chkPredicted: TCheckBox;
    chkDitheredI: TCheckBox;
    chkGlobGamma: TCheckBox;
    chkQuantize: TCheckBox;
    cbxScaling: TComboBox;
    cbxEndStep: TComboBox;
    cbxPalCount: TComboBox;
    cbxStartStep: TComboBox;
    cbxYilMix: TComboBox;
    cbxPalSize: TComboBox;
    chkDitheredO: TCheckBox;
    chkFTGamma: TCheckBox;
    chkGamma: TCheckBox;
    chkDitheringGamma: TCheckBox;
    chkSmoothed: TCheckBox;
    chkMirrored: TCheckBox;
    chkPlay: TCheckBox;
    chkFTFromPal: TCheckBox;
    chkUseTK: TCheckBox;
    edInput: TEdit;
    edOutput: TEdit;
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
    lblQuantizer: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    lblPct: TLabel;
    llPalTileDesc: TPanel;
    miGeneratePNGsOutput: TMenuItem;
    miGeneratePNGsInput: TMenuItem;
    miGenerateY4MOutput: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    miGenerateY4MInput: TMenuItem;
    miGenerateY4M: TMenuItem;
    miReload: TMenuItem;
    miLoadSettings: TMenuItem;
    miGeneratePNGs: TMenuItem;
    miSaveSettings: TMenuItem;
    odFFInput: TOpenDialog;
    odGTM: TOpenDialog;
    odSettings: TOpenDialog;
    pbProgress: TProgressBar;
    pcPages: TPageControl;
    pnLbl: TPanel;
    PsyVTimer: TTimer;
    sbTiles: TScrollBox;
    sdGTM: TSaveDialog;
    sdSettings: TSaveDialog;
    Separator1: TMenuItem;
    Separator3: TMenuItem;
    seShotTransDistHiThres: TFloatSpinEdit;
    seShotTransMaxSecondsPerKF: TFloatSpinEdit;
    seShotTransCorrelLoThres: TFloatSpinEdit;
    seMaxCores: TSpinEdit;
    seQbTiles: TFloatSpinEdit;
    seVisGamma: TFloatSpinEdit;
    seFrameCount: TSpinEdit;
    seMaxTiles: TSpinEdit;
    sePage: TSpinEdit;
    seStartFrame: TSpinEdit;
    seTempoSmoo: TFloatSpinEdit;
    seEncGamma: TFloatSpinEdit;
    seShotTransMinSecondsPerKF: TFloatSpinEdit;
    tbQuantizer: TTrackBar;
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
    miLoad: TMenuItem;
    MenuItem1: TMenuItem;
    pmProcesses: TPopupMenu;
    PopupMenu1: TPopupMenu;
    sedPalIdx: TSpinEdit;
    IdleTimer: TIdleTimer;
    tbFrame: TTrackBar;

    // processes
    procedure btnPredictMotionClick(Sender: TObject);
    procedure btnDitherClick(Sender: TObject);
    procedure btnGlobalLoadClick(Sender: TObject);
    procedure btnPreparePalettesClick(Sender: TObject);
    procedure btnClusterClick(Sender: TObject);
    procedure btnReconstructClick(Sender: TObject);
    procedure btnReindexClick(Sender: TObject);
    procedure btnSmoothClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);

    procedure btnPMClick(Sender: TObject);
    procedure btnGTMClick(Sender: TObject);
    procedure btnInputClick(Sender: TObject);
    procedure btnRunAllClick(Sender: TObject);
    procedure btnDebugClick(Sender: TObject);
    procedure btnDebug2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure IdleTimerTimer(Sender: TObject);
    procedure imgClick(Sender: TObject);
    procedure imgContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure imgPaletteClick(Sender: TObject);
    procedure btnGeneratePNGsInputClick(Sender: TObject);
    procedure imgPaintBackground(ASender: TObject; ACanvas: TCanvas; ARect: TRect);
    procedure imgPaletteMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgTilesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure miGeneratePNGsOutputClick(Sender: TObject);
    procedure miGenerateY4MInputClick(Sender: TObject);
    procedure miGenerateY4MOutputClick(Sender: TObject);
    procedure miLoadSettingsClick(Sender: TObject);
    procedure miReloadClick(Sender: TObject);
    procedure miSaveSettingsClick(Sender: TObject);
    procedure PsyVTimerTimer(Sender: TObject);
    procedure seMaxTilesEditingDone(Sender: TObject);
    procedure seQbTilesEditingDone(Sender: TObject);
    procedure UpdateVideo(Sender: TObject);
    procedure UpdateGUI(Sender: TObject);
  private
    FLastIOTabSheet: TTabSheet;
    FTilingEncoder: TTilingEncoder;
    FLockChanges: Boolean;

    procedure TilingEncoderProgress(ASender: TTilingEncoder; APosition, AMax: Integer; AHourGlass: Boolean);
    procedure LoadGUISettings;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

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

{ TMainForm }

procedure TMainForm.btnClusterClick(Sender: TObject);
begin
  FTilingEncoder.Run(esCluster);
  UpdateVideo(nil);
end;

procedure TMainForm.btnPreparePalettesClick(Sender: TObject);
begin
  FTilingEncoder.Run(esPreparePalettes);
  UpdateVideo(nil);
end;

procedure TMainForm.btnReconstructClick(Sender: TObject);
begin
  FTilingEncoder.Run(esReconstruct);
  UpdateVideo(nil);
end;

procedure TMainForm.btnGlobalLoadClick(Sender: TObject);
begin
  FTilingEncoder.Run(esLoad);
  seMaxTiles.Value := FTilingEncoder.GlobalTilingTileCount;
  UpdateVideo(nil);
end;

procedure TMainForm.btnPredictMotionClick(Sender: TObject);
begin
  FTilingEncoder.Run(esPredictMotion);
  UpdateVideo(nil);
end;

procedure TMainForm.btnDitherClick(Sender: TObject);
begin
  FTilingEncoder.Run(esDither);
  UpdateVideo(nil);
end;

procedure TMainForm.btnReindexClick(Sender: TObject);
begin
  FTilingEncoder.Run(esReindex);
  UpdateVideo(nil);
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
begin
  FTilingEncoder.Run(esSave);
  UpdateVideo(nil);
end;

procedure TMainForm.btnSmoothClick(Sender: TObject);
begin
  FTilingEncoder.Run(esSmooth);
  UpdateVideo(nil);
end;

procedure TMainForm.btnGeneratePNGsInputClick(Sender: TObject);
begin
  FTilingEncoder.GeneratePNGs(True);
  UpdateVideo(nil);
end;

procedure TMainForm.btnPMClick(Sender: TObject);
var
  pt: TPoint;
begin
  pt.X := btnPM.Left;
  pt.Y := btnPM.Top + btnPM.Height;
  pt := ClientToScreen(pt);
  pmProcesses.PopUp(pt.X, pt.Y);
end;

procedure TMainForm.imgPaintBackground(ASender: TObject; ACanvas: TCanvas; ARect: TRect);
begin
  ACanvas.Brush.Color := clBlack;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Clear;
end;

procedure TMainForm.imgPaletteMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  palIdx, useCount: Integer;
begin
  P := imgPalette.ScreenToClient(Mouse.CursorPos);
  palIdx := iDiv0(P.Y * FTilingEncoder.PaletteCount, imgPalette.Height);

  useCount := -1;
  if InRange(FTilingEncoder.RenderFrameIndex, 0, High(FTilingEncoder.Frames)) and
      Assigned(FTilingEncoder.Frames[FTilingEncoder.RenderFrameIndex].PKeyFrame) and
      InRange(palIdx, 0, High(FTilingEncoder.Frames[FTilingEncoder.RenderFrameIndex].PKeyFrame.Palettes)) then
    useCount := FTilingEncoder.Frames[FTilingEncoder.RenderFrameIndex].PKeyFrame.Palettes[palIdx].UseCount;

  llPalTileDesc.Caption := Format('Palette #: %3d, UseCount: %6d', [palIdx, useCount]);
end;

procedure TMainForm.imgTilesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  tileIdx: Integer;
begin
  P := imgTiles.ScreenToClient(Mouse.CursorPos);
  P.X := P.X * imgTiles.Picture.Width div imgTiles.Width;
  P.Y := P.Y * imgTiles.Picture.Height div imgTiles.Height;

  tileIdx := (sePage.Value * (imgTiles.Picture.Height shr cTileWidthBits) + (P.Y shr cTileWidthBits)) * (imgTiles.Picture.Width shr cTileWidthBits) + (P.X shr cTileWidthBits);

  if InRange(FTilingEncoder.RenderFrameIndex, 0, High(FTilingEncoder.Frames)) and
      Assigned(FTilingEncoder.Frames[FTilingEncoder.RenderFrameIndex].PKeyFrame) and
      InRange(tileIdx, 0, High(FTilingEncoder.Tiles)) then
    llPalTileDesc.Caption := Format('Tile #: %6d, UseCount: %6d%s%s%s', [
        tileIdx,
        FTilingEncoder.Tiles[tileIdx]^.UseCount,
        IfThen(FTilingEncoder.Tiles[tileIdx]^.Active, ', [Active]'),
        IfThen(FTilingEncoder.Tiles[tileIdx]^.HMirror_Initial, ', [H]'),
        IfThen(FTilingEncoder.Tiles[tileIdx]^.VMirror_Initial, ', [V]')])
  else
    llPalTileDesc.Caption := 'Invalid tile!';
end;

procedure TMainForm.miGeneratePNGsOutputClick(Sender: TObject);
begin
  FTilingEncoder.GeneratePNGs(False);
  UpdateVideo(nil);
end;

procedure TMainForm.miGenerateY4MInputClick(Sender: TObject);
begin
  FTilingEncoder.GenerateY4M(FTilingEncoder.OutputFileName + '.input.y4m', True);
  UpdateVideo(nil);
end;

procedure TMainForm.miGenerateY4MOutputClick(Sender: TObject);
begin
  FTilingEncoder.GenerateY4M(FTilingEncoder.OutputFileName + '.y4m', False);
  UpdateVideo(nil);
end;

procedure TMainForm.miLoadSettingsClick(Sender: TObject);
begin
  if FileExists(edInput.Text) then
    odSettings.FileName := ChangeFileExt(edInput.Text, '.gtm_settings');
  if odSettings.Execute then
  begin
    FTilingEncoder.LoadSettings(odSettings.FileName);
    LoadGUISettings;
    UpdateGUI(nil);
  end;
end;

procedure TMainForm.miReloadClick(Sender: TObject);
begin
  if odGTM.Execute then
  begin
    FTilingEncoder.ReloadGTM(odGTM.FileName);
    LoadGUISettings;
    UpdateGUI(nil);
  end;
end;

procedure TMainForm.miSaveSettingsClick(Sender: TObject);
begin
  if FileExists(edInput.Text) then
    sdSettings.FileName := ChangeFileExt(edInput.Text, '.gtm_settings');
  if sdSettings.Execute then
  begin
    UpdateGUI(nil);
    FTilingEncoder.SaveSettings(sdSettings.FileName);
  end;
end;

procedure TMainForm.PsyVTimerTimer(Sender: TObject);
begin
  UpdateVideo(Sender);
  PsyVTimer.Enabled := False;
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
    btnGlobalLoadClick(nil);

  if OkStep(esPredictMotion) then
    btnPredictMotionClick(nil);

  if OkStep(esPreparePalettes) then
    btnPreparePalettesClick(nil);

  if OkStep(esDither) then
    btnDitherClick(nil);

  if OkStep(esCluster) then
    btnClusterClick(nil);

  if OkStep(esReconstruct) then
    btnReconstructClick(nil);

  if OkStep(esSmooth) then
    btnSmoothClick(nil);

  if OkStep(esReindex) then
    btnReindexClick(nil);

  if OkStep(esSave) then
    btnSaveClick(nil);
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
    edInput.Text := odFFInput.FileName;
  end;
end;

procedure TMainForm.btnGTMClick(Sender: TObject);
begin
  if sdGTM.Execute then
    edOutput.Text := sdGTM.FileName;
end;

procedure TMainForm.btnDebugClick(Sender: TObject);
begin
  edInput.Text := ExtractFilePath(Application.ExeName) + '..\tiler_misc\Star.Wars.Despecialized.Edition.v2.5.avi';
  edOutput.Text := ExtractFilePath(Application.ExeName) + 'debug.gtm';
  seFrameCount.Value := IfThen(seFrameCount.Value >= 12, IfThen(seFrameCount.Value = 12, 48, 2), 12);
  cbxScaling.ItemIndex := 4;

  FTilingEncoder.Test;

  UpdateGUI(nil);
end;

procedure TMainForm.btnDebug2Click(Sender: TObject);
begin
  edInput.Text := ExtractFilePath(Application.ExeName) + '..\tiler_misc\sunflower_1080p25.y4m';
  edOutput.Text :=  ExtractFilePath(Application.ExeName) + 'debug.gtm';
  seFrameCount.Value := IfThen(seFrameCount.Value >= 12, IfThen(seFrameCount.Value = 12, 24, 1), 12);
  cbxScaling.ItemIndex := 2;

  FTilingEncoder.Test;

  UpdateGUI(nil);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  k: Word;
  i: Integer;
begin
  k := Key;
  if k in [VK_F10, VK_F11, VK_F12, VK_PRIOR, VK_NEXT] then
    Key := 0; // KLUDGE: workaround event called twice
  case k of
    VK_F10: btnRunAllClick(nil);
    VK_F11: chkPlay.Checked := not chkPlay.Checked;
    VK_F12:
      if ssCtrl in Shift then
      begin
        btnDebugClick(nil);
      end
      else if ssAlt in Shift then
      begin
        btnDebug2Click(nil);
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
        UpdateVideo(nil);
      end;
    VK_PRIOR, VK_NEXT:
      for i := 0 to FTilingEncoder.KeyFrameCount - 1 do
        if InRange(tbFrame.Position, FTilingEncoder.KeyFrames[i].StartFrame, FTilingEncoder.KeyFrames[i].EndFrame) then
        begin
          tbFrame.Position := FTilingEncoder.KeyFrames[(i + IfThen(k = VK_NEXT, 1, FTilingEncoder.KeyFrameCount - 1)) mod FTilingEncoder.KeyFrameCount].StartFrame;
          Break;
        end;
    VK_CONTROL:
      tbFrame.LineSize := round(FTilingEncoder.FramesPerSecond);
  end;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_CONTROL then
    tbFrame.LineSize := 1;
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

procedure TMainForm.imgClick(Sender: TObject);
var
  pt: TPoint;
begin
  pt := TImage(Sender).ScreenToClient(Mouse.CursorPos);

  pt.X -= (TImage(Sender).Width - FTilingEncoder.ScreenWidth) div 2;
  pt.Y -= (TImage(Sender).Height - FTilingEncoder.ScreenHeight) div 2;

  if Assigned(FTilingEncoder) and (tbFrame.Position < FTilingEncoder.FrameCount) and
    InRange(pt.X, 0, FTilingEncoder.ScreenWidth - 1) and InRange(pt.Y, 0, FTilingEncoder.ScreenHeight - 1) then
  begin
    pt.X := pt.X div cTileWidth;
    pt.Y := pt.Y div cTileWidth;

    sedPalIdx.Value := FTilingEncoder.Frames[tbFrame.Position].TileMap[pt.Y, pt.X].Smoothed.PalIdx;
  end;
end;

procedure TMainForm.imgContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  pt: TPoint;
begin
  pt := TImage(Sender).ScreenToClient(Mouse.CursorPos);

  pt.X -= (TImage(Sender).Width - FTilingEncoder.ScreenWidth) div 2;
  pt.Y -= (TImage(Sender).Height - FTilingEncoder.ScreenHeight) div 2;

  Handled := InRange(pt.X, 0, FTilingEncoder.ScreenWidth - 1) and InRange(pt.Y, 0, FTilingEncoder.ScreenHeight - 1) and (sedPalIdx.Value >= 0);

  if Handled then
    sedPalIdx.Value := -1;
end;

procedure TMainForm.imgPaletteClick(Sender: TObject);
var
  P: TPoint;
begin
  P := imgPalette.ScreenToClient(Mouse.CursorPos);
  sedPalIdx.Value := iDiv0(P.Y * FTilingEncoder.PaletteCount, imgPalette.Height);
  FTilingEncoder.RenderPaletteIndex := sedPalIdx.Value;
end;

procedure TMainForm.seMaxTilesEditingDone(Sender: TObject);
begin
  FTilingEncoder.GlobalTilingTileCount := seMaxTiles.Value;
  seQbTiles.Value := FTilingEncoder.GlobalTilingQualityBasedTileCount;
end;

procedure TMainForm.seQbTilesEditingDone(Sender: TObject);
begin
  FTilingEncoder.GlobalTilingQualityBasedTileCount := seQbTiles.Value;
  seMaxTiles.Value := FTilingEncoder.GlobalTilingTileCount;
  UpdateGUI(Sender);
end;

procedure TMainForm.UpdateVideo(Sender: TObject);
var
  fromTimer: Boolean;
begin
  UpdateGUI(Sender);

  fromTimer := Assigned(Sender) and (Sender is TTimer);

  FTilingEncoder.Render(not fromTimer);
  PsyVTimer.Enabled := False;
  PsyVTimer.Enabled := not fromTimer;

  imgSource.Picture.Bitmap := FTilingEncoder.InputBitmap;
  imgDest.Picture.Bitmap := FTilingEncoder.OutputBitmap;
  imgPalette.Picture.Bitmap := FTilingEncoder.PaletteBitmap;
  imgTiles.Picture.Bitmap := FTilingEncoder.TilesBitmap;

  UpdateGUI(Sender);
end;

procedure TMainForm.UpdateGUI(Sender: TObject);
var
  i: Integer;
begin
  if FLockChanges then
    Exit;

  tbFrame.Min := 0;
  tbFrame.Max := FTilingEncoder.FrameCount - 1;
  IdleTimer.Interval := round(1000 / FTilingEncoder.FramesPerSecond);
  FLastIOTabSheet := pcPages.ActivePage;

  tbFrame.HandleNeeded;
  for i := 0 to FTilingEncoder.FrameCount - 1 do
    if Assigned(FTilingEncoder.Frames[i]) and Assigned(FTilingEncoder.Frames[i].PKeyFrame) and
        (FTilingEncoder.Frames[i].Index = FTilingEncoder.Frames[i].PKeyFrame.StartFrame) then
      tbFrame.SetTick(i);

  FTilingEncoder.InputFileName := edInput.Text;
  FTilingEncoder.OutputFileName := edOutput.Text;
  FTilingEncoder.StartFrame := seStartFrame.Value;
  FTilingEncoder.FrameCountSetting := seFrameCount.Value;
  FTilingEncoder.PaletteCount := StrToIntDef(cbxPalCount.Text, 1);
  FTilingEncoder.PaletteSize := StrToIntDef(cbxPalSize.Text, 2);
  FTilingEncoder.Scaling := StrToFloatDef(cbxScaling.Text, 1.0, InvariantFormatSettings);

  FTilingEncoder.EncoderGammaValue := seEncGamma.Value;
  FTilingEncoder.RenderPlaying := chkPlay.Checked;
  FTilingEncoder.RenderFrameIndex := Max(0, tbFrame.Position);
  FTilingEncoder.RenderPredicted := chkPredicted.Checked;
  FTilingEncoder.RenderMirrored := chkMirrored.Checked;
  FTilingEncoder.RenderSmoothed := chkSmoothed.Checked;
  FTilingEncoder.RenderOutputDithered := chkDitheredO.Checked;
  FTilingEncoder.RenderInputDithered := chkDitheredI.Checked;
  FTilingEncoder.RenderUseGamma := chkGamma.Checked;
  FTilingEncoder.RenderMode := TPsyVisMode(cbxVisMode.ItemIndex);
  FTilingEncoder.RenderPaletteIndex := sedPalIdx.Value;
  FTilingEncoder.RenderTilePage := sePage.Value;
  FTilingEncoder.RenderGammaValue := seVisGamma.Value;

  FTilingEncoder.UseQuantizer := chkQuantize.Checked;
  FTilingEncoder.Quantizer := tbQuantizer.Position / 4;
  FTilingEncoder.DitheringUseGamma := chkDitheringGamma.Checked;
  FTilingEncoder.DitheringMode := TPsyVisMode(cbxDitheringMode.ItemIndex);
  FTilingEncoder.DitheringYliluoma2MixedColors := StrToIntDef(cbxYilMix.Text, 1);
  FTilingEncoder.DitheringUseThomasKnoll := chkUseTK.Checked;

  FTilingEncoder.GlobalTilingFromPalette := chkClusterFromPal.Checked;
  FTilingEncoder.GlobalTilingUseGamma := chkGlobGamma.Checked;
  FTilingEncoder.GlobalTilingMode := TPsyVisMode(cbxGlobMode.ItemIndex);
  FTilingEncoder.GlobalTilingQualityBasedTileCount := seQbTiles.Value;
  FTilingEncoder.GlobalTilingMethod := TClusteringMethod(cbxClusMethod.ItemIndex);

  FTilingEncoder.FrameTilingFromPalette := chkFTFromPal.Checked;
  FTilingEncoder.FrameTilingUseGamma := chkFTGamma.Checked;
  FTilingEncoder.FrameTilingMode := TPsyVisMode(cbxFTMode.ItemIndex);

  FTilingEncoder.SmoothingFactor := seTempoSmoo.Value;

  FTilingEncoder.ShotTransMinSecondsPerKF := seShotTransMinSecondsPerKF.Value;
  FTilingEncoder.ShotTransMaxSecondsPerKF := seShotTransMaxSecondsPerKF.Value;
  FTilingEncoder.ShotTransCorrelLoThres := seShotTransCorrelLoThres.Value;
  FTilingEncoder.ShotTransDistHiThres := seShotTransDistHiThres.Value;

  if pcPages.ActivePage = tsInput then
    FTilingEncoder.RenderPage := rpInput
  else if pcPages.ActivePage = tsOutput then
    FTilingEncoder.RenderPage := rpOutput
  else if pcPages.ActivePage = tsTilesPal then
    FTilingEncoder.RenderPage := rpTilesPalette
  else
    FTilingEncoder.RenderPage := rpNone;

  FTilingEncoder.MaxThreadCount := seMaxCores.Value;

  pnLbl.Caption := FTilingEncoder.RenderTitleText;
  lblCorrel.Caption := FormatFloat('##0.000000', FTilingEncoder.RenderPsychoVisualQuality);
  lblQuantizer.Caption := FormatFloat('##0.00', FTilingEncoder.Quantizer);
  sedPalIdx.MaxValue := FTilingEncoder.PaletteCount - 1;
  miReload.Enabled := FTilingEncoder.FrameCount > 0;
end;

procedure TMainForm.TilingEncoderProgress(ASender: TTilingEncoder; APosition, AMax: Integer; AHourGlass: Boolean);
begin
  pbProgress.Max := AMax;
  pbProgress.Position := APosition;
  lblPct.Caption := IntToStr(pbProgress.Position * 100 div pbProgress.Max) + '%';

  if AHourGlass then
    Screen.Cursor := crHourGlass
  else
    Screen.Cursor := crDefault;

  Invalidate;
  Repaint;
end;

procedure TMainForm.LoadGUISettings;
begin
  FLockChanges := True;
  try
   edInput.Text := FTilingEncoder.InputFileName;
   edOutput.Text := FTilingEncoder.OutputFileName;
   seStartFrame.Value := FTilingEncoder.StartFrame;
   seFrameCount.Value := FTilingEncoder.FrameCountSetting;
   cbxScaling.Text := FloatToStr(FTilingEncoder.Scaling);

   cbxPalSize.Text := IntToStr(FTilingEncoder.PaletteSize);
   cbxPalCount.Text := IntToStr(FTilingEncoder.PaletteCount);
   chkQuantize.Checked := FTilingEncoder.UseQuantizer;
   tbQuantizer.Position := round(FTilingEncoder.Quantizer * 4);
   chkDitheringGamma.Checked := FTilingEncoder.DitheringUseGamma;
   cbxDitheringMode.ItemIndex := Ord(FTilingEncoder.DitheringMode);
   chkUseTK.Checked := FTilingEncoder.DitheringUseThomasKnoll;
   cbxYilMix.Text := IntToStr(FTilingEncoder.DitheringYliluoma2MixedColors);

   chkClusterFromPal.Checked := FTilingEncoder.GlobalTilingFromPalette;
   chkGlobGamma.Checked := FTilingEncoder.GlobalTilingUseGamma;
   cbxGlobMode.ItemIndex := Ord(FTilingEncoder.GlobalTilingMode);
   seMaxTiles.Value := FTilingEncoder.GlobalTilingTileCount;
   seQbTiles.Value := FTilingEncoder.GlobalTilingQualityBasedTileCount;
   cbxClusMethod.ItemIndex := Ord(FTilingEncoder.GlobalTilingMethod);

   chkFTFromPal.Checked := FTilingEncoder.FrameTilingFromPalette;
   chkFTGamma.Checked := FTilingEncoder.FrameTilingUseGamma;
   cbxFTMode.ItemIndex := Ord(FTilingEncoder.FrameTilingMode);

   seTempoSmoo.Value := FTilingEncoder.SmoothingFactor;

   seEncGamma.Value := FTilingEncoder.EncoderGammaValue;
   seMaxCores.Value := FTilingEncoder.MaxThreadCount;

   cbxVisMode.ItemIndex := Ord(FTilingEncoder.RenderMode);
   seShotTransMinSecondsPerKF.Value := FTilingEncoder.ShotTransMinSecondsPerKF;
   seShotTransMaxSecondsPerKF.Value := FTilingEncoder.ShotTransMaxSecondsPerKF;
   seShotTransCorrelLoThres.Value := FTilingEncoder.ShotTransCorrelLoThres;
   seShotTransDistHiThres.Value := FTilingEncoder.ShotTransDistHiThres;
  finally
    FLockChanges := False;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  es: TEncoderStep;
begin
  FormatSettings := InvariantFormatSettings;
  FTilingEncoder := TTilingEncoder.Create;
  FTilingEncoder.OnProgress := @TilingEncoderProgress;

  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  pcPages.ActivePage := tsSettings;

  FLockChanges := True;
  try
    for es := Succ(Low(TEncoderStep)) to High(TEncoderStep) do
    begin
      cbxStartStep.AddItem(Copy(GetEnumName(TypeInfo(TEncoderStep), Ord(es)), 3), TObject(PtrInt(Ord(es))));
      cbxEndStep.AddItem(Copy(GetEnumName(TypeInfo(TEncoderStep), Ord(es)), 3), TObject(PtrInt(Ord(es))));
    end;
    cbxStartStep.ItemIndex := Ord(Succ(Low(TEncoderStep)));
    cbxEndStep.ItemIndex := Ord(High(TEncoderStep));

    seMaxCores.MaxValue := NumberOfProcessors;
    seMaxCores.Value := ProcThreadPool.MaxThreadCount;
  finally
    FLockChanges := False;
  end;

  LoadGUISettings;

  UpdateVideo(nil);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FTilingEncoder.Free;
end;

end.

