unit UALLJAStats;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, System.Math,
  UBasicStats, UzLogGlobal, UzLogConst, UzLogQSO;

type
  TAllJAStats = class(TBasicStats)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateStats; override;
    procedure InitACAG;
  end;

implementation

{$R *.DFM}

procedure TAllJAStats.FormCreate(Sender: TObject);
begin
   inherited;
   LowBand := b19;
   HighBand := b50;
//   InitGrid(LowBand, HighBand);
//   UpdateStats;
   UsedBands[b19] := True;
   UsedBands[b35] := True;
   UsedBands[b7] := True;
   UsedBands[b14] := True;
   UsedBands[b21] := True;
   UsedBands[b28] := True;
   UsedBands[b50] := True;
end;

procedure TAllJAStats.UpdateStats;
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

procedure TAllJAStats.InitACAG;
begin
   LowBand := b19;
   HighBand := b10G;
//   InitGrid(LowBand, HighBand);
   UpdateStats;
end;

end.
