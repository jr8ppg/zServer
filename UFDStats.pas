unit UFDStats;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids,
  UALLJAStats, UzLogGlobal, UzLogConst;

type
  TFDStats = class(TAllJAStats)
    Portable: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure PortableClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateStats; override;
  end;

implementation

{$R *.DFM}

procedure TFDStats.UpdateStats;
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

   if Portable.Checked then
      Grid.Cells[3, i + 1] := IntToStr(_totalpts * _totalmulti * 2)
   else
      Grid.Cells[3, i + 1] := IntToStr(_totalpts * _totalmulti);
end;

procedure TFDStats.FormCreate(Sender: TObject);
begin
   inherited;
   LowBand := b19;
   HighBand := b10G;
   InitGrid(LowBand, HighBand);
   ClientHeight := ClientHeight + Portable.Height + 8;
   Portable.Top := ClientHeight - Portable.Height - 4;
   UpdateStats;
   UsedBands[b19] := True;
   UsedBands[b35] := True;
   UsedBands[b7] := True;
   UsedBands[b14] := True;
   UsedBands[b21] := True;
   UsedBands[b28] := True;
   UsedBands[b50] := True;
   UsedBands[b144] := True;
   UsedBands[b430] := True;
   UsedBands[b1200] := True;
   UsedBands[b2400] := True;
   UsedBands[b5600] := True;
   UsedBands[b10G] := True;
end;

procedure TFDStats.PortableClick(Sender: TObject);
begin
   inherited;
   UpdateStats;
end;

end.
