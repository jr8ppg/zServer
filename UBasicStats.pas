unit UBasicStats;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids,
  UzLogGlobal, UzLogConst, UzLogQSO;

type
  TBandSet = set of TBand;

  TBandSummary = record
    qso : integer;
    points : integer;
    multi1 : integer;
    multi2 : integer;
    cwqso : integer;
    noncwqso : integer;
  end;

  TBasicStats = class(TForm)
    Grid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
  private
    { Private declarations }
  public
    { Public declarations }
    StatSummary : array[b19..b10g] of TBandSummary;
    UsedBands : array[b19..b10g] of boolean;
    Saved : Boolean;
    MasterLog : TLog;
    procedure InitStatSummary;
    procedure UpdateStatSummary;
    procedure Add(aQSO : TQSO);
    procedure AddNoUpdate(aQSO : TQSO);
    procedure Delete(aQSO : TQSO);
    procedure ClearAll;
    procedure UpdateStats; virtual; abstract;
    procedure SaveLogs(Filename : string);
    procedure InitGrid(LBand, HBand : TBand); virtual;
    procedure LoadFile(FileName : string); // resets current log and loads from file
    procedure MergeFile(FileName : string; BandSet : TBandSet); // will only update if band is in BandSet
  end;

var
  CurrentFileName : string;

implementation

uses
  UServerForm;

{$R *.DFM}

procedure TBasicStats.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);
   Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TBasicStats.FormCreate(Sender: TObject);
var
   b: TBand;
begin
   Saved := True;
   CurrentFileName := '';
   MasterLog := TLog.Create('Z-Server');
   for b := b19 to HiBand do begin
      UsedBands[b] := False;
   end;
end;

procedure TBasicStats.InitStatSummary;
var
   b: TBand;
begin
   for b := b19 to b10g do begin
      StatSummary[b].qso := 0;
      StatSummary[b].points := 0;
      StatSummary[b].multi1 := 0;
      StatSummary[b].multi2 := 0;
      StatSummary[b].cwqso := 0;
      StatSummary[b].noncwqso := 0;
   end;
end;

procedure TBasicStats.UpdateStatSummary;
var
   aQSO: TQSO;
   b: TBand;
   i: integer;
begin
   InitStatSummary;

   for i := 1 to MasterLog.TotalQSO do begin
      aQSO := TQSO(MasterLog.QSOList[i]);
      b := aQSO.Band;
      inc(StatSummary[b].qso);
      inc(StatSummary[b].points, aQSO.points);
      if aQSO.NewMulti1 then
         inc(StatSummary[b].multi1);
      if aQSO.NewMulti2 then
         inc(StatSummary[b].multi2);
      if aQSO.Mode = mCW then
         inc(StatSummary[b].cwqso)
      else
         inc(StatSummary[b].noncwqso);
   end;
end;

procedure TBasicStats.InitGrid(LBand, HBand: TBand);
var
   i: integer;
   b: TBand;
begin
   with Grid do begin
      i := 0;
      for b := LBand to HBand do
         if NotWARC(b) then
            inc(i);

      i := i + 3;
      RowCount := i;

      ColCount := 6;
      Cells[0, 0] := 'MHz';
      Cells[1, 0] := 'QSOs';
      Cells[2, 0] := 'Mult';
      Cells[3, 0] := 'CW';
      Cells[4, 0] := 'Ph';
      Cells[5, 0] := 'CW%';

      i := 1;
      for b := LBand to HBand do begin
         if NotWARC(b) then begin
            Cells[0, i] := MHzString[b];
            inc(i);
         end;
      end;

      Cells[0, i] := 'Total';
      Cells[0, i + 1] := 'Score';
      Height := DefaultRowHeight * RowCount + 2;
      Width := DefaultColWidth * ColCount + 2;
   end;
end;

procedure TBasicStats.SaveLogs(Filename: string);
var
   f: file of TQSOdata;
   D: TQSOdata;
   i: word;
   back: string;
begin
   back := Filename;
   back := copy(back, 1, length(back) - 4) + '.BAK'; // change the extension
   if FileExists(back) then begin
      DeleteFile(back);
   end;

   RenameFile(Filename, back);

   AssignFile(f, Filename);
   Rewrite(f);

   D.memo := 'ZServer';
   Write(f, D);

   MasterLog.SortByTime;
   ServerForm.MultiForm.RecalcAll;

   for i := 1 to MasterLog.TotalQSO do begin
      D := MasterLog.QSOList[i].FileRecord;
      Write(f, D);
   end;

   CloseFile(f);
   Saved := True;
end;

procedure TBasicStats.AddNoUpdate(aQSO: TQSO);
begin
   MasterLog.Add(aQSO);
   Saved := False;
end;

procedure TBasicStats.Add(aQSO: TQSO);
begin
   MasterLog.Add(aQSO);
   UpdateStats;
   Saved := False;
end;

procedure TBasicStats.Delete(aQSO: TQSO);
begin
   aQSO.Reserve := actDelete;
   MasterLog.AddQue(aQSO);
   MasterLog.ProcessQue;
   UpdateStats;
   Saved := False;
end;

procedure TBasicStats.ClearAll;
begin
   // MasterLog.Clear;
end;

procedure TBasicStats.LoadFile(Filename: string);
var
   f: file of TQSOdata;
   D: TQSOdata;
   Q: TQSO;
begin
   if FileExists(Filename) = False then
      exit;

   AssignFile(f, Filename);
   Reset(f);

   ClearAll;
   ServerForm.MultiForm.reset;
   Q := TQSO.Create;
   Read(f, D);
   while not(eof(f)) do begin
      Read(f, D);
      Q.FileRecord := D;
      MasterLog.Add(Q);
      ServerForm.MultiForm.Add(Q);
   end;
   Q.Free;
   UpdateStats;
   ServerForm.CommandQue.Add('999 ' + ZLinkHeader + ' FILELOADED');
   CloseFile(f);
end;

procedure TBasicStats.MergeFile(Filename: string; BandSet: TBandSet);
begin
   {
     if FileExists(FileName) = False then
     exit;
     System.assign(f, Filename);
     reset(f);
     for B := b19 to HiBand do
     if B in BandSet then
     ClearBand(B);
     Q := TQSO.Create;
     read(f, D);
     while not(eof(f)) do
     begin
     read(f, D);
     Q.QSO := D;
     if D.Band in BandSet then
     begin
     Logs[D.Band].Add(Q);
     //ServerForm.MultiForm.Add(Q);
     end;
     end;
     for B := b19 to HiBand do
     Logs[B].SortByTime;
     Q.Free;
     UpdateStats;
     ServerForm.MultiForm.RecalcAll;
     ServerForm.CommandQue.Add('999 '+ZLinkHeader+ ' FILELOADED');
   }
end;

procedure TBasicStats.GridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
   strText: string;
begin
   with TStringGrid(Sender).Canvas do begin
      if ARow = 0 then begin
         Font.Color := clGreen;
      end
      else if ACol = 0 then begin
         if ARow < TStringGrid(Sender).RowCount - 2 then begin
            Font.Color := clBlue
         end
         else begin
            Font.Color := clNavy;
         end;
      end
      else begin
         Font.Color := clBlack;
      end;

      strText := TStringGrid(Sender).Cells[ACol, ARow];
      TextRect(Rect, strText, [tfRight,tfVerticalCenter,tfSingleLine]);
   end;
end;

end.
