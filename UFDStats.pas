unit UFDStats;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, System.Math,
  UBasicStats, UzLogGlobal, UzLogConst, UALLJAStats;

type
  TFDStats = class(TBasicStats)
    Portable: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure PortableClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateStats; override;
  end;

implementation

{$R *.DFM}

procedure TFDStats.FormCreate(Sender: TObject);
begin
   inherited;
   LowBand := b19;
   HighBand := b10G;
//   InitGrid(LowBand, HighBand);
//   ClientHeight := ClientHeight + Portable.Height + 8;
//   Portable.Top := ClientHeight - Portable.Height - 4;
//   UpdateStats;
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

procedure TFDStats.FormResize(Sender: TObject);
begin
   inherited;
   Portable.Top := ClientHeight - Portable.Height - 4;
   Portable.Left := ClientWidth - Portable.Width - 4;
end;

procedure TFDStats.UpdateStats;
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

   // 見出し行
   Grid.Cells[0, 0] := 'MHz';
   Grid.Cells[1, 0] := 'QSOs';
   Grid.Cells[2, 0] := 'Points';
   Grid.Cells[3, 0] := 'Multi';
   Grid.Cells[4, 0] := 'CW';
   Grid.Cells[5, 0] := 'Ph';
   Grid.Cells[6, 0] := 'CW %';

   for band := LowBand to HighBand do begin
      // WARC除外
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

   // 合計行
   Grid.Cells[0, row] := 'Total';
   Grid.Cells[1, row] := IntToStr3(TotQSO);
   Grid.Cells[2, row] := IntToStr3(TotPts);
   Grid.Cells[3, row] := IntToStr3(TotMulti);
   Grid.Cells[4, row] := IntToStr3(TotCW);
   Grid.Cells[5, row] := IntToStr3(TotPH);
   Grid.Cells[6, row] := '';
   Inc(row);

   // スコア行
   if Portable.Checked then begin
      strScore:= IntToStr3(TotPts * TotMulti * 2);
   end
   else begin
      strScore:= IntToStr3(TotPts * TotMulti);
   end;
   Grid.Cells[0, row] := 'Score';
   Grid.Cells[1, row] := '';
   Grid.Cells[2, row] := '';
   Grid.Cells[3, row] := strScore;
   Grid.Cells[4, row] := '';
   Grid.Cells[5, row] := '';
   Grid.Cells[6, row] := '';
   Inc(row);

   // 行数をセット
   Grid.ColCount := 7;
   Grid.RowCount := row;

   // カラム幅をセット
   w := Grid.Canvas.TextWidth('9');
   Grid.ColWidths[0] := w * 6;
   Grid.ColWidths[1] := w * 7;
   Grid.ColWidths[2] := w * 7;
   Grid.ColWidths[3] := w * Max(8, Length(strScore)+1);
   Grid.ColWidths[4] := w * 7;
   Grid.ColWidths[5] := w * 7;
   Grid.ColWidths[6] := w * 7;

   // 幅調整
   w := 0;
   for i := 0 to Grid.ColCount - 1 do begin
      w := w + Grid.ColWidths[i];
   end;
   w := w + (Grid.ColCount * Grid.GridLineWidth) + 2;
   ClientWidth := Max(w, 200);

   // 高さ調整
   h := 0;
   for i := 0 to Grid.RowCount - 1 do begin
      h := h + Grid.RowHeights[i];
   end;
   h := h + (Grid.RowCount * Grid.GridLineWidth) + 4;
   ClientHeight := h;
end;

procedure TFDStats.PortableClick(Sender: TObject);
begin
   inherited;
   UpdateStats;
end;

end.
