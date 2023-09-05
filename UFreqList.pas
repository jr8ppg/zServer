unit UFreqList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  UCheckWin, UzLogGlobal, UzLogConst;

type
  TFreqList = class(TCheckWin)
    ClearBtn: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
  private
    { Private declarations }
    FreqArray : array[b19..HiBand] of string;
    //function GetTXNr(S : string) : integer;
    function GetPCName(S : string) : string;
  public
    procedure UpdateX;
    procedure ProcessFreqData(S : string);
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TFreqList.UpdateX;
var
   B: TBand;
   S: string;
begin
   ListBox.Items.Clear;
   for B := b19 to HiBand do
      if FreqArray[B] <> '' then begin
         S := FreqArray[B];
         Delete(S, 1, 3);
         ListBox.Items.Add(S);
      end;
end;

{ function TFreqList.GetTXNr(S : string) : integer;
  var ss : string;
  i : integer;
  begin
  ss := copy(S, 7, 2);
  ss := TrimRight(ss);
  try
  i := StrToInt(ss);
  except
  on EConvertError do
  i := 0;
  end;
  Result := i;
  end; }

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

   for B := b19 to HiBand do
      if FreqArray[B] <> '' then
         if pcname = GetPCName(FreqArray[B]) { GetTXNr(FreqArray[B]) = tx } then
            FreqArray[B] := '';

   ss := copy(S, 1, 2);
   ss := TrimRight(ss);
   B := TBand(StrToInt(ss));
   FreqArray[B] := S;
   UpdateX;
end;

procedure TFreqList.FormShow(Sender: TObject);
begin
   // inherited;
   UpdateX;
end;

procedure TFreqList.FormCreate(Sender: TObject);
var
   B: TBand;
begin
   for B := b19 to HiBand do begin
      FreqArray[B] := '';
   end;
end;

procedure TFreqList.ClearBtnClick(Sender: TObject);
var
   B: TBand;
begin
   // inherited;
   for B := b19 to HiBand do begin
      FreqArray[B] := '';
   end;

   UpdateX;
end;

end.
