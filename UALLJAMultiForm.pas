unit UALLJAMultiForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, checklst, ExtCtrls, ComCtrls,
  UBasicMultiForm, UzLogGlobal, UzLogConst, UzLogQSO;

type
  TKen = (m101,m102,m103,m104,m105,m106,m107,m108,
  m109,m110,m111,m112,m113,m114,
  m02,m03,m04,m05,m06,m07,m08,m09,m10,m11,
  m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,
  m22,m23,m24,m25,m26,m27,m28,m29,m30,m31,
  m32,m33,m34,m35,m36,m37,m38,m39,m40,m41,
  m42,m43,m44,m45,m46,m47,m48);

  TALLJAMultiForm = class(TBasicMultiForm)
    TabControl: TTabControl;
    CheckListBox: TCheckListBox;
    ListBox: TListBox;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure CheckListBoxClickCheck(Sender: TObject);
  private
    { Private declarations }
    MultiArray : array[b19..HiBand, m101..m48] of boolean;
  public
    { Public declarations }
    procedure Reset; override;
    procedure Add(aQSO : TQSO); override;
    procedure UpdateCheckListBox;
    procedure UpdateListBox;
  end;

const KenNames : array[m101..m48] of string =
('101 @’J','102 —¯–G','103 ãì','104 –Ô‘–','105 ‹ó’m','106 Îë','107 ªº',
 '108 Œãu','109 \Ÿ','110 ‹ú˜H','111 “ú‚','112 ’_U','113 •OR','114 “n“‡',
 '02  ÂX','03  Šâè','04  H“c','05  RŒ`','06  ‹{é','07  •Ÿ“‡','08  VŠƒ',
 '09  ’·–ì','10  “Œ‹','11  _“Şì','12  ç—t','13  é‹Ê','14  ˆïé','15  “È–Ø',
 '16  ŒQ”n','17  R—œ','18  Ã‰ª','19  Šò•Œ','20  ˆ¤’m','21  Od','22  ‹“s',
 '23   ‰ê','24  “Ş—Ç','25  ‘åã','26  ˜a‰ÌR','27  •ºŒÉ','28  •xR','29  •Ÿˆä',
 '30  Îì','31  ‰ªR','32  “‡ª','33  RŒû','34  ’¹æ','35  L“‡','36  ì',
 '37  “¿“‡','38  ˆ¤•Q','39  ‚’m','40  •Ÿ‰ª','41  ²‰ê','42  ’·è','43  ŒF–{',
 '44  ‘å•ª','45  ‹{è','46  ­™“‡','47  ‰«“ê','48  ¬Š}Œ´'
);

implementation

uses
  UServerForm;

{$R *.DFM}

procedure TALLJAMultiForm.UpdateListBox;
var
   K: TKen;
   BB: TBand;
   str: string;
begin
   ListBox.Clear;
   for K := m101 to m48 do begin
      str := '';
      for BB := b19 to b50 do begin
         if NotWARC(BB) then begin
            if MultiArray[BB, K] then
               str := str + '* '
            else
               str := str + '. ';
         end;
      end;

      str := FillRight(KenNames[K], 16) + str;
      ListBox.Items.Add(str);
   end;
end;

procedure TALLJAMultiForm.Reset;
var
   B: TBand;
   K: TKen;
   str: string;
begin
   for B := b19 to HiBand do begin
      for K := m101 to m48 do begin
         MultiArray[B, K] := False;
      end;
   end;

   ListBox.Clear;
   for K := m101 to m48 do begin
      str := FillRight(KenNames[K], 16) + '. . . . . . .';
      ListBox.Items.Add(str);
   end;

   Update;
end;

procedure TALLJAMultiForm.Add(aQSO: TQSO);
var
   M: TKen;
   str: string;
   i: integer;
   B: TBand;
begin
   if aQSO.Dupe then begin
      exit;
   end;

   str := aQSO.NrRcvd;
   Delete(str, length(str), 1); // delete the last char

   aQSO.NewMulti1 := False;
   try
      i := StrToInt(str);
   except
      on EConvertError do
         exit;
   end;

   case i of
      101 .. 114:
         M := TKen(i - 101);
      2 .. 50:
         M := TKen(i - 2 + ord(m02));
      else
         exit;
   end;

   if MultiArray[aQSO.Band, M] = False then begin
      MultiArray[aQSO.Band, M] := True;
      aQSO.NewMulti1 := True;

      str := '';
      for B := b19 to b50 do begin
         if NotWARC(B) then begin
            if MultiArray[B, M] then
               str := str + '* '
            else
               str := str + '. ';
         end;
      end;

      str := FillRight(KenNames[M], 16) + str;
      ListBox.Items.Delete(ord(M));
      ListBox.Items.Insert(ord(M), str);
      UpdateCheckListBox;
   end;
end;

procedure TALLJAMultiForm.UpdateCheckListBox;
var
   B: TBand;
   K: TKen;
begin
   if TabControl.TabIndex = 7 then begin
   end
   else begin
      B := b19;
      case TabControl.TabIndex of
         0:
            B := b19;
         1:
            B := b35;
         2:
            B := b7;
         3:
            B := b14;
         4:
            B := b21;
         5:
            B := b28;
         6:
            B := b50;
      end;
      for K := m101 to m48 do begin
         CheckListBox.Checked[ord(K)] := MultiArray[B, K];
      end;
   end;
end;

procedure TALLJAMultiForm.FormCreate(Sender: TObject);
var
   K: TKen;
begin
   inherited;

   TabControl.TabIndex := 7;
   ListBox.Visible := True;
   CheckListBox.Visible := False;
   ListBox.Align := alClient;
   CheckListBox.Align := alNone;

   for K := m101 to m48 do begin
      CheckListBox.Items.Add(KenNames[K]);
   end;

   Reset;
end;

procedure TALLJAMultiForm.TabControlChange(Sender: TObject);
begin
   inherited;

   if TabControl.TabIndex = 7 then begin
      CheckListBox.Align := alNone;
      CheckListBox.Visible := False;
      ListBox.Align := alClient;
      ListBox.Visible := True;
   end
   else begin
      ListBox.Align := alNone;
      ListBox.Visible := False;
      CheckListBox.Align := alClient;
      CheckListBox.Visible := True;
      UpdateCheckListBox;
   end;
end;

procedure TALLJAMultiForm.CheckListBoxClickCheck(Sender: TObject);
begin
   inherited;
   UpdateCheckListBox;
end;

end.
