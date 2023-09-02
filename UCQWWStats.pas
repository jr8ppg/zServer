unit UCQWWStats;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, System.Math,
  UBasicStats, UzLogGlobal, UzLogConst;

type
  TCQWWStats = class(TBasicStats)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateStats; override;
  end;

implementation

{$R *.DFM}

procedure TCQWWStats.FormCreate(Sender: TObject);
begin
   inherited;
   LowBand := b19;
   HighBand := b28;
   //InitGrid(LowBand, HighBand);
   //UpdateStats;
   //Height := 185;
   UsedBands[b35] := True;
   UsedBands[b7] := True;
   UsedBands[b14] := True;
   UsedBands[b21] := True;
   UsedBands[b28] := True;
   UsedBands[b19] := True;
end;

procedure TCQWWStats.UpdateStats;
var
   band : TBand;
   TotQSO, TotPts, TotMulti, TotMulti2: Integer;
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
   TotMulti2 := 0;
   row := 1;

   // ���o���s
   Grid.Cells[0, 0] := 'MHz';
   Grid.Cells[1, 0] := 'QSOs';
   Grid.Cells[2, 0] := 'Points';
   Grid.Cells[3, 0] := 'Multi';
   Grid.Cells[4, 0] := 'Multi2';

   for band := b19 to b28 do begin
      // WARC���O
      if NotWARC(band) = False then begin
         Continue;
      end;

      TotQSO := TotQSO + StatSummary[band].qso;
      TotPts := TotPts + StatSummary[band].points;
      TotMulti := TotMulti + StatSummary[band].multi1;
      TotMulti2 := TotMulti2 + StatSummary[band].multi2;

      Grid.Cells[0, row] := '*' + MHzString[band];
      Grid.Cells[1, row] := IntToStr3(StatSummary[band].qso);
      Grid.Cells[2, row] := IntToStr3(StatSummary[band].points);
      Grid.Cells[3, row] := IntToStr3(StatSummary[band].multi1);
      Grid.Cells[4, row] := IntToStr3(StatSummary[band].multi2);

      Inc(row);
   end;

   // ���v�s
   Grid.Cells[0, row] := 'Total';
   Grid.Cells[1, row] := IntToStr3(TotQSO);
   Grid.Cells[2, row] := IntToStr3(TotPts);
   Grid.Cells[3, row] := IntToStr3(TotMulti);
   Grid.Cells[4, row] := IntToStr3(TotMulti2);
   Inc(row);

   // �X�R�A�s
   strScore:= IntToStr3(TotPts * (TotMulti + TotMulti2));
   Grid.Cells[0, row] := 'Score';
   Grid.Cells[1, row] := '';
   Grid.Cells[2, row] := '';
   Grid.Cells[3, row] := '';
   Grid.Cells[4, row] := strScore;
   Inc(row);

   // �s�����Z�b�g
   Grid.ColCount := 5;
   Grid.RowCount := row;

   // �J���������Z�b�g
   w := Grid.Canvas.TextWidth('9');
   Grid.ColWidths[0] := w * 6;
   Grid.ColWidths[1] := w * 7;
   Grid.ColWidths[2] := w * 7;
   Grid.ColWidths[3] := w * 7;
   Grid.ColWidths[4] := w * Max(8, Length(strScore)+1);

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
