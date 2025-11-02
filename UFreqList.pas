unit UFreqList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  UCheckWin, UzLogGlobal, UzLogConst, Vcl.ComCtrls;

type
  TFreqList = class(TForm)
    ListView1: TListView;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FFreqArray : array[b19..HiBand] of string;
    procedure UpdateX;
    function GetPCName(S : string) : string;
  public
    { Public declarations }
    procedure ProcessFreqData(S : string);
    procedure Clear(B: TBand);
  end;

implementation

{$R *.DFM}

procedure TFreqList.FormCreate(Sender: TObject);
var
   B: TBand;
begin
   for B := b19 to HiBand do begin
      FFreqArray[B] := '';
   end;
end;

procedure TFreqList.FormShow(Sender: TObject);
begin
   UpdateX;
end;

procedure TFreqList.UpdateX;
var
   B: TBand;
   S: string;
   listitem: TListItem;
   str: string;
begin
   ListView1.Items.BeginUpdate();
   ListView1.Items.Clear;

   // 00000000011111111112222222222333333333344444444445
   // 12345678901234567890123456789012345678901234567890
   // 123123123451234567890 12345123123456789
   // 1  2  7        7114.5 SSB  SP 22:21:57  [PC01]
   for B := b19 to HiBand do begin
      if FFreqArray[B] <> '' then begin
         listitem := ListView1.Items.Add();
         S := FFreqArray[B];

         // 接続番号
         str := Trim(Copy(S, 1, 3));
         listitem.Caption := str;

         // バンドコード(TBand)
//         str := Trim(Copy(S, 4, 3));
//         listitem.SubItems.Add(str);

         // バンド名
         str := Trim(Copy(S, 7, 5));
         listitem.SubItems.Add(str);

         // 周波数
         str := Trim(Copy(S, 12, 10));
         listitem.SubItems.Add(str);

         // モード
         str := Trim(Copy(S, 23, 5));
         listitem.SubItems.Add(str);

         // CALL
         str := Trim(Copy(S, 28, 3));
         listitem.SubItems.Add(str);

         // 更新時刻
         // HH:MM:SS
         str := Trim(Copy(S, 31, 8));
         if (Length(str) = 7) then begin
            str := '0' + str;
         end;
         listitem.SubItems.Add(str);

         // PC名
         str := Trim(Copy(S, 40));
         str := StringReplace(str, '[', '', [rfReplaceAll]);
         str := StringReplace(str, ']', '', [rfReplaceAll]);
         listitem.SubItems.Add(str);
      end;
   end;

   ListView1.Items.EndUpdate();
end;

function TFreqList.GetPCName(S: string): string;
var
   ss: string;
   i, j: integer;
begin
   ss := '';
   i := pos('[', S);
   j := pos(']', S);
   if (i > 0) and (j > 0) and (j > i) then begin
      ss := S;
      Delete(ss, 1, i);
      j := pos(']', ss);
      ss := copy(ss, 1, j - 1);
   end;
   Result := ss;
end;

procedure TFreqList.ProcessFreqData(S: string);
var
   ss: string;
   pcname: string;
   B: TBand;
begin
   if length(S) < 30 then
      exit;

   // tx := GetTXNr(S);
   pcname := GetPCName(S);

   // 同じＰＣ名が他のバンドに有った時用にクリアする
   for B := b19 to HiBand do begin
      if FFreqArray[B] <> '' then begin
         if pcname = GetPCName(FFreqArray[B]) then begin
            FFreqArray[B] := '';
         end;
      end;
   end;

   // バンドコード取りだし
   ss := copy(S, 4, 2);
   ss := TrimRight(ss);
   B := TBand(StrToInt(ss));

   // バンド別のリストにセット
   FFreqArray[B] := S;

   // 再表示
   UpdateX();
end;

procedure TFreqList.Clear(B: TBand);
begin
   FFreqArray[B] := '';
   UpdateX();
end;

end.
