unit USimpleStats;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids,
  UBasicStats, UzLogGlobal, UzLogConst, UzLogQSO;

type
  TSimpleStats = class(TBasicStats)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    LowBand, HighBand : TBand;
    procedure UpdateStats; override;
    procedure InitGrid(LBand, HBand: TBand); override;
  end;

implementation

{$R *.DFM}

procedure TSimpleStats.UpdateStats;
var
   i, _totalqso, _totalcw, _totalph: integer;
   temp: string;
   B: TBand;
   R: double;
begin
   _totalqso := 0;
   _totalcw := 0;
   _totalph := 0;
   i := 1;
   UpdateStatSummary;
   for B := LowBand to HighBand do begin
      if NotWARC(B) then begin
         Grid.Cells[1, i] := IntToStr(StatSummary[B].qso);
         Grid.Cells[2, i] := IntToStr(StatSummary[B].cwqso);
         Grid.Cells[3, i] := IntToStr(StatSummary[B].noncwqso);
         if StatSummary[B].qso = 0 then
            R := 0
         else
            R := 100.0 * (StatSummary[B].cwqso) / (StatSummary[B].qso);

         temp := Format('%3.1f', [R]);
         Grid.Cells[4, i] := temp;

         inc(_totalqso, StatSummary[B].qso);
         inc(_totalcw, StatSummary[B].cwqso);
         inc(_totalph, StatSummary[B].noncwqso);
         inc(i);
      end;
   end;
   Grid.Cells[1, i] := IntToStr(_totalqso);
   Grid.Cells[2, i] := IntToStr(_totalcw);
   Grid.Cells[3, i] := IntToStr(_totalph);
   if _totalqso = 0 then
      R := 0
   else
      R := 100 * _totalcw / _totalqso;

   temp := Format('%3.1f', [R]);
   Grid.Cells[4, i] := temp;
end;

procedure TSimpleStats.FormCreate(Sender: TObject);
begin
   inherited;
   LowBand := b19;
   HighBand := b10g;
   InitGrid(LowBand, HighBand);
   UpdateStats;
   UsedBands[b19] := True;
   UsedBands[b35] := True;
   UsedBands[b7] := True;
   UsedBands[b14] := True;
   UsedBands[b21] := True;
   UsedBands[b28] := True;
   UsedBands[b50] := True;
end;

procedure TSimpleStats.InitGrid(LBand, HBand: TBand);
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

      ColCount := 5;
      Cells[0, 0] := 'MHz';
      Cells[1, 0] := 'QSOs';
      Cells[2, 0] := 'CW';
      Cells[3, 0] := 'Ph';
      Cells[4, 0] := 'CW%';

      i := 1;
      for b := LBand to HBand do begin
         if NotWARC(b) then begin
            Cells[0, i] := MHzString[b];
            inc(i);
         end;
      end;

      Cells[0, i] := 'Total';
      Cells[0, i + 1] := 'Score';
   end;

   ClientHeight := Grid.DefaultRowHeight * Grid.RowCount + 2;
   ClientWidth := Grid.DefaultColWidth * Grid.ColCount + 2;
end;

end.
