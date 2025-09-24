unit HelperLib;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.Math, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.Grids, Vcl.CategoryButtons;

type
  TListBoxHelper = class helper for TListBox
  public
    procedure ShowFirst();
    procedure ShowLast();
  end;

  TStringGridHelper = class helper for TStringGrid
  public
    procedure ShowFirst();
    procedure ShowLast(datarows: Integer = -1);
    function GetColBoundary(): Integer;
    procedure AdjustColWidth(C: Integer);
  end;

  TCategoryButtonsHelper = class helper for TCategoryButtons
  public
    procedure LoadFromFile(filename: string; Encoding: TEncoding);
    procedure SaveToFile(filename: string; Encoding: TEncoding);
    procedure LoadFromText(TXT: TStringList);
    procedure SaveToText(TXT: TStringList);
  end;

  TStringListHelper = class helper for TStringList
  public
    procedure Add2(S: string);
  end;

implementation

{ TListBoxHelper }

procedure TListBoxHelper.ShowFirst();
begin
   TopIndex := 0;
end;

procedure TListBoxHelper.ShowLast();
var
   line_height: Integer;
   lines: Integer;
begin
   if Style = lbOwnerDrawFixed then begin
      line_height := ItemHeight;
   end
   else begin
      line_height := Abs(Font.Height) + 2;
   end;
   lines := Height div line_height;

   if Items.Count < lines then begin
      TopIndex := 0;
   end
   else begin
      TopIndex := Items.Count - lines;
   end;
end;

{ TStringGridHelper }

procedure TStringGridHelper.ShowFirst();
begin
   TopRow := FixedRows;
end;

procedure TStringGridHelper.ShowLast(datarows: Integer);
begin
   if datarows <= VisibleRowCount then begin
      TopRow := FixedRows;
   end
   else begin
      if datarows = -1 then begin
         TopRow := RowCount - VisibleRowCount;
      end
      else begin
         TopRow := (datarows + FixedRows) - VisibleRowCount;
      end;
   end;
end;

function TStringGridHelper.GetColBoundary(): Integer;
var
   i: Integer;
   pt: TPoint;
   rect: TRect;
   w: Integer;
   coord: TGridCoord;
begin
   // �_�u���N���b�N�ʒu���擾
   GetCursorPos(pt);
   pt := Self.ScreenToClient(pt);

   // �P�s�ڈȊO�͖����������Ƃɂ���
   coord := Self.MouseCoord(pt.X, pt.Y);
   if (coord.Y > 0) then begin
      Result := -1;
      Exit;
   end;

   // �_�u���N���b�N���ꂽ�Z����T��
   w := 0;

   rect.Top := 0;
   rect.Bottom := Self.RowHeights[0];

   for i := 0 to Self.ColCount - 1 do begin
      // �ׂ̃Z���Ƃ̋��E���W
      w := w + Self.ColWidths[i];

      // �O��5pixel��L���N���b�N�͈͂Ƃ���
      rect.Left := w - 10;
      rect.Right := w + 10;

      // �͈͂ɂ���΂������Z���ʒu
      if PtInRect(rect, pt) = true then begin
         Result := i;
         Exit;
      end;
   end;

   // ��������
   Result := -1;
end;

procedure TStringGridHelper.AdjustColWidth(C: Integer);
var
   R: Integer;
   L: Integer;
   w: Integer;
begin
   // �ő咷�������߂�
   L := 0;
   for R := 1 to Self.RowCount - 1 do begin
      L := Max(L, Length(Self.Cells[C, R]));
   end;

   // �P����������̕�
   w := Self.Canvas.TextWidth('X');

   // ����ݒ�
   Self.ColWidths[C] := w * (L + 1);

   // �ĕ`��
   Self.Invalidate();
end;

{ TCategoryButtonsHelper }

procedure TCategoryButtonsHelper.LoadFromFile(filename: string; Encoding: TEncoding);
var
   L: TStringList;
begin
   L := TStringList.Create();
   try
      L.LoadFromFile(filename, Encoding);
      LoadFromText(L);
   finally
      L.Free();
   end;
end;

procedure TCategoryButtonsHelper.SaveToFile(filename: string; Encoding: TEncoding);
var
   L: TStringList;
begin
   L := TStringList.Create();
   try
      SaveToText(L);
      L.SaveToFile(filename, Encoding);
   finally
      L.Free();
   end;
end;

procedure TCategoryButtonsHelper.LoadFromText(TXT: TStringList);
var
   i: Integer;
   strLine: string;
   strText: string;
   C: TButtonCategory;
   B: TButtonItem;
begin
   C := nil;
   Categories.Clear();
   for i := 0 to TXT.Count - 1 do begin
      strLine := TXT[i];
      if strLine[1] = Chr($09) then begin
         if C = nil then begin
            C := Categories.Add();
            C.Caption := '�^�C�g������';
         end;
         strText := Trim(Copy(strLine, 2));
         B := C.Items.Add();
         B.Caption := strText;
         B.Hint := strText;
      end
      else begin
         C := Categories.Add();
         C.Caption := strLine;
      end;
   end;
end;

procedure TCategoryButtonsHelper.SaveToText(TXT: TStringList);
var
   i: Integer;
   j: Integer;
   C: TButtonCategory;
   B: TButtonItem;
begin
   for i := 0 to Categories.Count - 1 do begin
      C := Categories[i];
      TXT.Add(C.Caption);
      for j := 0 to C.Items.Count - 1 do begin
         B := C.Items[j];
         TXT.Add(Chr($09) + B.Caption);
      end;
   end;
end;

{ TStringListHelper }

procedure TStringListHelper.Add2(S: string);
begin
   if S = '' then begin
      Add(S);
   end
   else begin
      S := StringReplace(S, '"', '""', [rfReplaceAll]);
      Add('"' + S + '"');
   end;
end;

end.
