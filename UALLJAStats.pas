unit UALLJAStats;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids,
  UBasicStats, UzLogGlobal, UzLogConst, UzLogQSO;

type
  TAllJAStats = class(TBasicStats)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    LowBand, HighBand : TBand;
    procedure UpdateStats; override;
    procedure InitACAG;
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TAllJAStats.UpdateStats;
var
   i, _totalqso, _totalmulti, _totalcw, _totalph, _totalpts: integer;
   temp: string;
   B: TBand;
   R: double;
begin
   _totalqso := 0;
   _totalpts := 0;
   _totalmulti := 0;
   _totalcw := 0;
   _totalph := 0;
   i := 1;
   UpdateStatSummary;
   for B := LowBand to HighBand do begin
      if NotWARC(B) then begin
         Grid.Cells[1, i] := IntToStr(StatSummary[B].qso);
         Grid.Cells[2, i] := IntToStr(StatSummary[B].points);
         Grid.Cells[3, i] := IntToStr(StatSummary[B].multi1);
         Grid.Cells[4, i] := IntToStr(StatSummary[B].cwqso);
         Grid.Cells[5, i] := IntToStr(StatSummary[B].noncwqso);
         if StatSummary[B].qso = 0 then
            R := 0
         else
            R := 100.0 * (StatSummary[B].cwqso) / (StatSummary[B].qso);

         temp := Format('%3.1f', [R]);
         Grid.Cells[6, i] := temp;

         inc(_totalqso, StatSummary[B].qso);
         inc(_totalpts, StatSummary[B].points);
         inc(_totalmulti, StatSummary[B].multi1);
         inc(_totalcw, StatSummary[B].cwqso);
         inc(_totalph, StatSummary[B].noncwqso);
         inc(i);
      end;
   end;
   Grid.Cells[1, i] := IntToStr(_totalqso);
   Grid.Cells[2, i] := IntToStr(_totalpts);
   Grid.Cells[3, i] := IntToStr(_totalmulti);
   Grid.Cells[4, i] := IntToStr(_totalcw);
   Grid.Cells[5, i] := IntToStr(_totalph);
   if _totalqso = 0 then
      R := 0
   else
      R := 100 * _totalcw / _totalqso;

   temp := Format('%3.1f', [R]);
   Grid.Cells[6, i] := temp;
   Grid.Cells[3, i + 1] := IntToStr(_totalpts * _totalmulti);
end;

procedure TAllJAStats.FormCreate(Sender: TObject);
begin
   inherited;
   LowBand := b19;
   HighBand := b50;
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

procedure TAllJAStats.InitACAG;
begin
   LowBand := b19;
   HighBand := b10G;
   InitGrid(LowBand, HighBand);
   UpdateStats;
end;

end.
