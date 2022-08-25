unit main;

{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils, strutils, types, Math, FileUtil, typinfo, LazLogger,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Spin, Menus, IntfGraphics,
  FPimage, FPCanvas, FPWritePNG, GraphType, MTProcs, extern, tilingencoder;

type
  { TMainForm }

  TMainForm = class(TForm)
    btnGTS: TButton;
    btnInput: TButton;
    btnGTM: TButton;
    btnRunAll: TButton;
    cbxScaling: TComboBox;
    cbxEndStep: TComboBox;
    cbxPalCount: TComboBox;
    cbxStartStep: TComboBox;
    cbxYilMix: TComboBox;
    cbxPalSize: TComboBox;
    cbxDLBPC: TComboBox;
    chkFTGamma: TCheckBox;
    chkUseKMQuant: TCheckBox;
    chkGamma: TCheckBox;
    chkDitheringGamma: TCheckBox;
    chkSmoothed: TCheckBox;
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
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    lblPct: TLabel;
    MenuItem8: TMenuItem;
    odFFInput: TOpenDialog;
    pbProgress: TProgressBar;
    pcPages: TPageControl;
    pnLbl: TPanel;
    sbTiles: TScrollBox;
    sdGTM: TSaveDialog;
    sdGTS: TSaveDialog;
    seFTBlendThres: TFloatSpinEdit;
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
    procedure btnDebug2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure IdleTimerTimer(Sender: TObject);
    procedure imgClick(Sender: TObject);
    procedure imgContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure imgPaletteClick(Sender: TObject);
    procedure imgPaletteDblClick(Sender: TObject);
    procedure btnGeneratePNGsClick(Sender: TObject);
    procedure imgPaintBackground(ASender: TObject; ACanvas: TCanvas; ARect: TRect);
    procedure seMaxTilesEditingDone(Sender: TObject);
    procedure seQbTilesEditingDone(Sender: TObject);
    procedure UpdateVideo(Sender: TObject);
    procedure UpdateGUI(Sender: TObject);
  private
    FLastIOTabSheet: TTabSheet;
    FTilingEncoder: TTilingEncoder;

    procedure TilingEncoderProgress(ASender: TTilingEncoder; APosition, AMax: Integer; AHourGlass: Boolean);
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

procedure TMainForm.btnDoGlobalTilingClick(Sender: TObject);
begin
  FTilingEncoder.GlobalTiling;
  UpdateVideo(nil);
end;

procedure TMainForm.btnDitherClick(Sender: TObject);
begin
  FTilingEncoder.Dither;
  UpdateVideo(nil);
end;

procedure TMainForm.btnDoMakeUniqueClick(Sender: TObject);
begin
  FTilingEncoder.MakeUnique;
  UpdateVideo(nil);
end;

procedure TMainForm.btnDoFrameTilingClick(Sender: TObject);
begin
  FTilingEncoder.FrameTiling;
  UpdateVideo(nil);
end;

procedure TMainForm.btnLoadClick(Sender: TObject);
begin
  FTilingEncoder.Load;
  seMaxTiles.Value := FTilingEncoder.GlobalTilingTileCount;
  UpdateVideo(nil);
end;

procedure TMainForm.btnReindexClick(Sender: TObject);
begin
  FTilingEncoder.Reindex;
  UpdateVideo(nil);
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
begin
  FTilingEncoder.Save;
  UpdateVideo(nil);
end;

procedure TMainForm.btnSmoothClick(Sender: TObject);
begin
  FTilingEncoder.Smooth;
  UpdateVideo(nil);
end;

procedure TMainForm.btnGeneratePNGsClick(Sender: TObject);
begin
  FTilingEncoder.GeneratePNGs;
  UpdateVideo(nil);
end;

procedure TMainForm.imgPaintBackground(ASender: TObject; ACanvas: TCanvas; ARect: TRect);
begin
  ACanvas.Brush.Color := clBlack;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Clear;
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

procedure TMainForm.btnDebugClick(Sender: TObject);
begin
  edInput.Text := 'C:\tiler_misc\Star.Wars.Despecialized.Edition.v2.5.avi';
  edOutput.Text := 'C:\tiler\debug.gtm';
  edReload.Text := '';
  seFrameCount.Value := IfThen(seFrameCount.Value >= 12, IfThen(seFrameCount.Value = 12, 48, 1), 12);
  seFTBlend.Value := 7;
  seFTBlendThres.Value := 1.0;

  FTilingEncoder.Test;
end;

procedure TMainForm.btnDebug2Click(Sender: TObject);
begin
  edInput.Text := 'C:\tiler_misc\factory_1080p30.y4m';
  edOutput.Text := 'C:\tiler\debug.gtm';
  edReload.Text := '';
  seFrameCount.Value := IfThen(seFrameCount.Value >= 12, IfThen(seFrameCount.Value = 12, 24, 1), 12);
  cbxScaling.ItemIndex := 2;
  seFTBlend.Value := 3;

  FTilingEncoder.Test;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  k: Word;
begin
  k := Key;
  if k in [VK_F2, VK_F3, VK_F4, VK_F5, VK_F6, VK_F7, VK_F8, VK_F9, VK_F10, VK_F11, VK_F12] then
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

    sedPalIdx.Value := FTilingEncoder.Frames[tbFrame.Position].TileMap[pt.Y, pt.X].PalIdx;
  end;
end;

procedure TMainForm.imgContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
  sedPalIdx.Value := -1;
end;

procedure TMainForm.imgPaletteClick(Sender: TObject);
var
  P: TPoint;
begin
  P := imgPalette.ScreenToClient(Mouse.CursorPos);
  sedPalIdx.Value := iDiv0(P.y * FTilingEncoder.PaletteCount, imgPalette.Height);
  FTilingEncoder.RenderPaletteIndex := sedPalIdx.Value;
end;

procedure TMainForm.imgPaletteDblClick(Sender: TObject);
begin
  sedPalIdx.Value := -1;
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
begin
  UpdateGUI(Sender);

  FTilingEncoder.Render;

  imgSource.Picture.Bitmap := FTilingEncoder.InputBitmap;
  imgDest.Picture.Bitmap := FTilingEncoder.OutputBitmap;
  imgPalette.Picture.Bitmap := FTilingEncoder.PaletteBitmap;
  imgTiles.Picture.Bitmap := FTilingEncoder.TilesBitmap;

  UpdateGUI(Sender);
end;

procedure TMainForm.UpdateGUI(Sender: TObject);
begin
  tbFrame.Min := 0;
  tbFrame.Max := FTilingEncoder.FrameCount - 1;
  IdleTimer.Interval := round(1000 / FTilingEncoder.FramesPerSecond);
  FLastIOTabSheet := pcPages.ActivePage;


  FTilingEncoder.InputFileName := edInput.Text;
  FTilingEncoder.OutputFileName := edOutput.Text;
  FTilingEncoder.StartFrame := seStartFrame.Value;
  FTilingEncoder.FrameCount := seFrameCount.Value;
  FTilingEncoder.PaletteCount := StrToIntDef(cbxPalCount.Text, 1);
  FTilingEncoder.PaletteSize := StrToIntDef(cbxPalSize.Text, 2);
  FTilingEncoder.Scaling := StrToFloatDef(cbxScaling.Text, 1.0, InvariantFormatSettings);

  FTilingEncoder.EncoderGammaValue := seEncGamma.Value;
  FTilingEncoder.RenderPlaying := chkPlay.Checked;
  FTilingEncoder.RenderFrameIndex := Max(0, tbFrame.Position);
  FTilingEncoder.RenderBlended := chkBlended.Checked;
  FTilingEncoder.RenderMirrored := chkMirrored.Checked;
  FTilingEncoder.RenderSmoothed := chkSmoothed.Checked;
  FTilingEncoder.RenderUseGamma := chkGamma.Checked;
  FTilingEncoder.RenderPaletteIndex := sedPalIdx.Value;
  FTilingEncoder.RenderTilePage := sePage.Value;
  FTilingEncoder.RenderGammaValue := seVisGamma.Value;

  FTilingEncoder.QuantizerUseYakmo := chkUseKMQuant.Checked;
  FTilingEncoder.QuantizerDennisLeeBitsPerComponent := StrToInt(cbxDLBPC.Text);
  FTilingEncoder.DitheringUseGamma := chkDitheringGamma.Checked;
  FTilingEncoder.DitheringYliluoma2MixedColors := StrToIntDef(cbxYilMix.Text, 1);
  FTilingEncoder.DitheringUseThomasKnoll := chkUseTK.Checked;

  FTilingEncoder.GlobalTilingTileCount := seMaxTiles.Value;
  FTilingEncoder.GlobalTilingQualityBasedTileCount := seQbTiles.Value;
  FTilingEncoder.ReloadTileset := chkReload.Checked;
  FTilingEncoder.ReloadTilesetFileName := edReload.Text;

  FTilingEncoder.FrameTilingUseGamma := chkFTGamma.Checked;
  FTilingEncoder.FrameTilingBlendingSize := seFTBlend.Value;
  FTilingEncoder.FrameTilingBlendingThreshold := seFTBlendThres.Value;

  FTilingEncoder.SmoothingFactor := seTempoSmoo.Value;
  FTilingEncoder.SmoothingAdditionalTilesThreshold := seAddlTiles.Value;

  if pcPages.ActivePage = tsInput then
    FTilingEncoder.RenderPage := rpInput
  else if pcPages.ActivePage = tsOutput then
    FTilingEncoder.RenderPage := rpOutput
  else if pcPages.ActivePage = tsTilesPal then
    FTilingEncoder.RenderPage := rpTilesPalette
  else
    FTilingEncoder.RenderPage := rpNone;

  pnLbl.Caption := FTilingEncoder.RenderTitleText;
  lblCorrel.Caption := FormatFloat('##0.000000', FTilingEncoder.RenderPsychoVisualQuality);
  sedPalIdx.MaxValue := FTilingEncoder.PaletteCount - 1;
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

  UpdateGUI(nil);
  Invalidate;
  Repaint;
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

  for es := Succ(Low(TEncoderStep)) to High(TEncoderStep) do
  begin
    cbxStartStep.AddItem(Copy(GetEnumName(TypeInfo(TEncoderStep), Ord(es)), 3), TObject(PtrInt(Ord(es))));
    cbxEndStep.AddItem(Copy(GetEnumName(TypeInfo(TEncoderStep), Ord(es)), 3), TObject(PtrInt(Ord(es))));
  end;
  cbxStartStep.ItemIndex := Ord(Succ(Low(TEncoderStep)));
  cbxEndStep.ItemIndex := Ord(High(TEncoderStep));

  UpdateVideo(nil);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FTilingEncoder.Free;
end;

end.

