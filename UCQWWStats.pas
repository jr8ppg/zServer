unit UCQWWStats;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, Cologrid,
  UBasicStats, UzLogGlobal, UzLogConst;

type
  TCQWWStats = class(TBasicStats)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    LowBand, HighBand : TBand;
    procedure InitGrid(LBand, HBand : TBand); override;
    procedure UpdateStats; override;
  end;

var
  CQWWStats: TCQWWStats;

implementation

{$R *.DFM}

procedure TCQWWStats.UpdateStats;
var
   i, _totalqso, _totalcty, _totalzone, _totalpoints: integer;
   B: TBand;
begin
   _totalqso := 0;
   _totalcty := 0;
   _totalzone := 0;
   _totalpoints := 0;
   i := 1;
   UpdateStatSummary;
   for B := LowBand to HighBand do begin
      if NotWARC(B) then begin
         Grid.Cells[1, i] := IntToStr(StatSummary[B].qso);
         Grid.Cells[2, i] := IntToStr(StatSummary[B].points);
         Grid.Cells[3, i] := IntToStr(StatSummary[B].multi1);
         Grid.Cells[4, i] := IntToStr(StatSummary[B].multi2);
         inc(_totalqso, StatSummary[B].qso);
         inc(_totalzone, StatSummary[B].multi1);
         inc(_totalcty, StatSummary[B].multi2);
         inc(_totalpoints, StatSummary[B].points);
         inc(i);
      end;
   end;
   Grid.Cells[1, i] := IntToStr(_totalqso);
   Grid.Cells[2, i] := IntToStr(_totalpoints);
   Grid.Cells[3, i] := IntToStr(_totalzone);
   Grid.Cells[4, i] := IntToStr(_totalcty);
   Grid.Cells[2, i + 1] := IntToStr(_totalpoints * (_totalcty + _totalzone));
end;

procedure TCQWWStats.InitGrid(LBand, HBand: TBand);
var
   i: integer;
   B: TBand;
begin
   with Grid do begin
      i := 0;
      for B := LBand to HBand do
         if NotWARC(B) then
            inc(i);
      i := i + 3;
      RowCount := i;

      ColCount := 5;
      Cells[0, 0] := 'MHz';
      Cells[1, 0] := 'QSOs';
      Cells[2, 0] := 'Points';
      Cells[3, 0] := 'Zones';
      Cells[4, 0] := 'Cty';
      // Cells[5,0] := '';

      i := 1;
      for B := LBand to HBand do
         if NotWARC(B) then begin
            Cells[0, i] := MHzString[B];
            inc(i);
         end;
      Cells[0, i] := 'Total';
      Cells[0, i + 1] := 'Score';
      Height := DefaultRowHeight * RowCount + 2;
      // Width := DefaultColWidth*ColCount + 2;
   end;
end;

procedure TCQWWStats.FormCreate(Sender: TObject);
begin
   inherited;
   LowBand := b19;
   HighBand := b28;
   InitGrid(LowBand, HighBand);
   UpdateStats;
   Height := 185;
   UsedBands[b35] := True;
   UsedBands[b7] := True;
   UsedBands[b14] := True;
   UsedBands[b21] := True;
   UsedBands[b28] := True;
   UsedBands[b19] := True;
end;

end.
