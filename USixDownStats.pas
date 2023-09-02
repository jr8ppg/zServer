unit USixDownStats;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, System.Math,
  UBasicStats, UzLogGlobal, UzLogConst;

type
  TSixDownStats = class(TBasicStats)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateStats; override;
  end;

implementation

{$R *.DFM}

procedure TSixDownStats.FormCreate(Sender: TObject);
begin
   inherited;
   LowBand := b50;
   HighBand := HiBand;
   InitGrid(LowBand, HighBand);
   UpdateStats;
   UsedBands[b50] := True;
   UsedBands[b144] := True;
   UsedBands[b430] := True;
   UsedBands[b1200] := True;
   UsedBands[b2400] := True;
   UsedBands[b5600] := True;
   UsedBands[b10g] := True;
end;

procedure TSixDownStats.UpdateStats;
var
   band : TBand;
   TotQSO, TotPts, TotMulti, TotCW, TotPH: Integer;
   row: Integer;
   i: Integer;
   h: Integer;
   w: Integer;
   strScore: string;
begin
   UpdateStatSummary;

   TotQSO := 0;
   TotPts := 0;
   TotMulti := 0;
   TotCW := 0;
   TotPH := 0;
   row := 1;

   // ���o���s
   Grid.Cells[0, 0] := 'MHz';
   Grid.Cells[1, 0] := 'QSOs';
   Grid.Cells[2, 0] := 'Points';
   Grid.Cells[3, 0] := 'Multi';
   Grid.Cells[4, 0] := 'CW';
   Grid.Cells[5, 0] := 'Ph';
   Grid.Cells[6, 0] := 'CW %';

   for band := LowBand to HighBand do begin
      // WARC���O
      if NotWARC(band) = False then begin
         Continue;
      end;

      TotQSO := TotQSO + StatSummary[band].qso;
      TotPts := TotPts + StatSummary[band].points;
      TotMulti := TotMulti + StatSummary[band].multi1;
      TotCW := TotCW + StatSummary[band].cwqso;
      TotPH := TotPH + StatSummary[band].noncwqso;

      Grid.Cells[0, row] := '*' + MHzString[band];
      Grid.Cells[1, row] := IntToStr3(StatSummary[band].qso);
      Grid.Cells[2, row] := IntToStr3(StatSummary[band].points);
      Grid.Cells[3, row] := IntToStr3(StatSummary[band].multi1);
      Grid.Cells[4, row] := IntToStr3(StatSummary[band].cwqso);
      Grid.Cells[5, row] := IntToStr3(StatSummary[band].noncwqso);

      if StatSummary[band].qso > 0 then begin
         Grid.Cells[6, row] := FloatToStrF(100 * (StatSummary[band].cwqso / StatSummary[band].qso), ffFixed, 1000, 1);
      end
      else begin
         Grid.Cells[6, row] := '-';
      end;

      Inc(row);
   end;

   // ���v�s
   Grid.Cells[0, row] := 'Total';
   Grid.Cells[1, row] := IntToStr3(TotQSO);
   Grid.Cells[2, row] := IntToStr3(TotPts);
   Grid.Cells[3, row] := IntToStr3(TotMulti);
   Grid.Cells[4, row] := IntToStr3(TotCW);
   Grid.Cells[5, row] := IntToStr3(TotPH);
   Grid.Cells[6, row] := '';
   Inc(row);

   // �X�R�A�s
   strScore:= IntToStr3(TotPts * TotMulti);
   Grid.Cells[0, row] := 'Score';
   Grid.Cells[1, row] := '';
   Grid.Cells[2, row] := '';
   Grid.Cells[3, row] := strScore;
   Grid.Cells[4, row] := '';
   Grid.Cells[5, row] := '';
   Grid.Cells[6, row] := '';
   Inc(row);

   // �s�����Z�b�g
   Grid.ColCount := 7;
   Grid.RowCount := row;

   // �J���������Z�b�g
   w := Grid.Canvas.TextWidth('9');
   Grid.ColWidths[0] := w * 6;
   Grid.ColWidths[1] := w * 7;
   Grid.ColWidths[2] := w * 7;
   Grid.ColWidths[3] := w * Max(8, Length(strScore)+1);
   Grid.ColWidths[4] := w * 7;
   Grid.ColWidths[5] := w * 7;
   Grid.ColWidths[6] := w * 7;

   // ������
   w := 0;
   for i := 0 to Grid.ColCount - 1 do begin
      w := w + Grid.ColWidths[i];
   end;
   w := w + (Grid.ColCount * Grid.GridLineWidth) + 2;
   ClientWidth := Max(w, 200);

   // ��������
   h := 0;
   for i := 0 to Grid.RowCount - 1 do begin
      h := h + Grid.RowHeights[i];
   end;
   h := h + (Grid.RowCount * Grid.GridLineWidth) + 4;
   ClientHeight := h;
end;

end.
