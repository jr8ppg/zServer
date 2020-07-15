unit UWWMultiForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  UBasicMultiForm, StdCtrls, Grids, Cologrid, ExtCtrls, JLLabel,
  UzLogGlobal, UzLogConst, UzLogQSO;

const testCQWW = $03;
      MAXCQZONE = 40;
      testIARU = $09;
      testDXCCWWZone = $05;

type
  TCountry = class
    Country : string; {JA, KH6 etc}
    CountryName : string; {Japan, Hawaii, etc}
    Zone : integer;
    Continent : string;
    Worked : array[b19..HiBand] of boolean;
    GridIndex : integer;  // where it is listed in the Grid (row)
    constructor Create;
    function Summary : string;
    function Summary2 : string;
    function JustInfo : string; // returns cty name, px and continent
  end;

  TCountryList = class
    List : TList;
    constructor Create;
    destructor Destroy; override;
  end;

  TPrefix = class
    Prefix : string;
    Index : integer;
    Length : integer;
    OvrZone : integer;         // override zone
    OvrContinent : string;     // override continent
    constructor Create;
  end;

  TPrefixList = class
    List : TList;
    constructor Create;
    destructor Destroy; override;
  end;

type
  TWWMultiForm = class(TBasicMultiForm)
    Panel: TPanel;
    RotateLabel1: TRotateLabel;
    RotateLabel2: TRotateLabel;
    RotateLabel3: TRotateLabel;
    RotateLabel4: TRotateLabel;
    RotateLabel5: TRotateLabel;
    RotateLabel6: TRotateLabel;
    SortBy: TRadioGroup;
    Grid: TMgrid;
    Panel1: TPanel;
    Button1: TButton;
    GoButton: TButton;
    Edit1: TEdit;
    cbStayOnTop: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure SortByClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GoButtonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbStayOnTopClick(Sender: TObject);
    procedure GridSetting(ARow, Acol: Integer; var Fcolor: Integer;
      var Bold, Italic, underline: Boolean);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    GridReverse : array[0..500] of integer; {pointer from grid row to countrylist index}
    CountryList : TCountryList;
    PrefixList : TPrefixList;
    Zone : array[b19..HiBand, 1..MAXCQZONE] of boolean;
    procedure ResetBand(B : TBand); override;
    procedure Reset; override;
    procedure Add(aQSO : TQSO); override;
    procedure SortDefault;
    procedure SortZone;
    //procedure RecalcBand(B : TBand); override;
    procedure RecalcAll; override;
    { Public declarations }
  end;

procedure LoadCTY_DAT(TEST : byte; var L : TCountryList; var PL : TPrefixList);
procedure LoadCountryDataFromFile(filename : string; var L : TCountryList; var PL : TPrefixList);

var
  WWMultiForm: TWWMultiForm;

implementation

uses
  UWWZone, UServerForm;

{$R *.DFM}

procedure LoadCTY_DAT(TEST: byte; var L: TCountryList; var PL: TPrefixList);
var
   f: textfile;
   str, temp, temp2: string;
   C: TCountry;
   P: TPrefix;
   i, mii, j, k, m: integer;
begin
   AssignFile(f, 'CTY.DAT');
   try
      Reset(f);
   except
      on EFOpenError do begin
         exit;
      end;
   end;

   C := TCountry.Create;
   C.CountryName := 'Unknown';
   L.List.Add(C);
   while not(eof(f)) do begin
      ReadLn(f, str);

      if (Pos('*', str) > 0) and (TEST <> testCQWW) then // Cty only for CQWW
      begin
         repeat
            readln(f, str);
         until (eof(f)) or (Pos(':', str) > 0);
         if eof(f) then
            exit;
      end;

      C := TCountry.Create;

      i := Pos(':', str);
      if i > 0 then begin
         C.CountryName := copy(str, 1, i - 1);
         Delete(str, 1, i);
         str := TrimLeft(str);
      end;

      i := Pos(':', str);
      if i > 0 then begin
         temp := copy(str, 1, i - 1);
         j := StrToIntDef(temp, 0);

         if (TEST in [testCQWW, testDXCCWWZone]) then begin
            C.Zone := j;
         end;

         Delete(str, 1, i);
         str := TrimLeft(str);
      end;

      i := Pos(':', str);
      if i > 0 then begin
         temp := copy(str, 1, i - 1);
         j := StrToIntDef(temp, 0);

         if (TEST = testIARU) then begin
            C.Zone := j;
         end;

         Delete(str, 1, i);
         str := TrimLeft(str);
      end;

      i := Pos(':', str);
      if i > 0 then begin
         temp := copy(str, 1, i - 1);
         if Pos(temp + ';', 'AS;AF;EU;NA;SA;OC;') > 0 then
            C.Continent := temp;
         Delete(str, 1, i);
         str := TrimLeft(str);
      end;

      i := Pos(':', str); // latitude
      if i > 0 then begin
         Delete(str, 1, i);
         str := TrimLeft(str);
      end;

      i := Pos(':', str); // longitude
      if i > 0 then begin
         Delete(str, 1, i);
         str := TrimLeft(str);
      end;

      i := Pos(':', str); // utc offset
      if i > 0 then begin
         Delete(str, 1, i);
         str := TrimLeft(str);
      end;

      i := Pos(':', str);
      if i > 0 then begin
         temp := copy(str, 1, i - 1);
         if temp[1] = '*' then
            Delete(temp, 1, 1);
         C.Country := temp;
         // Delete(str, 1, i);
         // str := TrimLeft(str);
      end;

      L.List.Add(C);
      i := L.List.Count - 1;
      C.GridIndex := i;

      repeat
         mii := 1;
         readln(f, str);
         str := TrimLeft(str);
         repeat
            temp := '';
            repeat
               temp := temp + str[mii];
               inc(mii)
            until (str[mii] = ',') or (str[mii] = ';') or (mii > Length(str));

            P := TPrefix.Create;

            if (Pos('(', temp) > 0) then begin
               j := Pos('(', temp);
               k := Pos(')', temp);
               if k > j + 1 then begin
                  temp2 := copy(temp, j + 1, k - j - 1);
                  m := StrToIntDef(temp2, 0);

                  if (m > 0) and (TEST in [testCQWW, testDXCCWWZone]) then begin
                     P.OvrZone := m;
                  end;
               end;

               Delete(temp, j, k - j + 1);
            end;

            if (Pos('[', temp) > 0) then begin
               j := Pos('[', temp);
               k := Pos(']', temp);
               if k > j + 1 then begin
                  temp2 := copy(temp, j + 1, k - j - 1);
                  m := StrToIntDef(temp2, 0);

                  if (m > 0) and (TEST = testIARU) then begin
                     P.OvrZone := m;
                  end;
               end;

               Delete(temp, j, k - j + 1);
            end;

            if (Pos('{', temp) > 0) then begin
               j := Pos('{', temp);
               k := Pos('}', temp);
               if k > j + 1 then begin
                  temp2 := copy(temp, j + 1, k - j - 1);
                  if Pos(temp2 + ';', 'AS;AF;EU;NA;SA;OC;') > 0 then
                     P.OvrContinent := temp2;
               end;
               Delete(temp, j, k - j + 1);
            end;

            if (Pos('<', temp) > 0) then // lat, long override. ignore
            begin
               j := Pos('<', temp);
               k := Pos('>', temp);
               Delete(temp, j, k - j + 1);
            end;

            P.Prefix := temp;
            P.Index := i;
            P.Length := Length(temp);
            j := 0;
            if PL.List.Count > 0 then
               for j := 0 to PL.List.Count - 1 do begin
                  if TPrefix(PL.List[j]).Length <= P.Length then
                     break;
               end;
            PL.List.Insert(j, P);
            inc(mii);
         until (mii >= Length(str) + 1);
      until str[mii - 1] = ';';
   end;
end;

procedure LoadCountryDataFromFile(filename: string; var L: TCountryList; var PL: TPrefixList);
var
   f: textfile;
   str, temp: string;
   C: TCountry;
   P: TPrefix;
   i, mii, j: integer;
begin
   AssignFile(f, filename);
   try
      Reset(f);
   except
      on EFOpenError do begin
         exit;
      end;
   end;

   ReadLn(f, str);
   C := TCountry.Create;
   C.CountryName := 'Unknown';
   L.List.Add(C);
   while not(eof(f)) do begin
      readln(f, str);
      if Pos('end of file', LowerCase(str)) > 0 then
         break;
      C := TCountry.Create;
      C.CountryName := TrimRight(copy(str, 1, 26));
      temp := TrimLeft(TrimRight(copy(str, 27, 2)));
      i := StrToIntDef(temp, 0);

      if (i < 0) or (i > 90 { maxzone } ) then begin
         i := 0;
      end;

      C.Zone := i;
      C.Country := TrimRight(copy(str, 32, 7));
      case C.Zone of
         1 .. 8:
            C.Continent := 'NA';
         9 .. 13:
            C.Continent := 'SA';
         14 .. 16, 40:
            C.Continent := 'EU';
         17 .. 26:
            C.Continent := 'AS';
         27 .. 32:
            C.Continent := 'OC';
         33 .. 39:
            C.Continent := 'AF';
      end;

      if CharInSet(str[39], ['A', 'O', 'E']) then begin
         temp := str[39] + str[40];
         if temp = 'AS' then
            C.Continent := 'AS';
         if temp = 'AN' then
            C.Continent := 'AN';
         if temp = 'AF' then
            C.Continent := 'AF';
         if temp = 'EU' then
            C.Continent := 'EU';
         if temp = 'OC' then
            C.Continent := 'OC';
         if temp = 'NA' then
            C.Continent := 'NA';
         if temp = 'SA' then
            C.Continent := 'SA';
      end;

      L.List.Add(C);
      i := L.List.Count - 1;
      C.GridIndex := i;

      repeat
         mii := 3;
         readln(f, str);
         repeat
            temp := '';
            repeat
               temp := temp + str[mii];
               inc(mii)
            until (str[mii] = ',') or (str[mii] = ';');
            P := TPrefix.Create;
            P.Prefix := temp;
            P.Index := i;
            P.Length := Length(temp);
            j := 0;
            if PL.List.Count > 0 then
               for j := 0 to PL.List.Count - 1 do begin
                  if TPrefix(PL.List[j]).Length <= P.Length then
                     break;
               end;
            PL.List.Insert(j, P);
            inc(mii);
         until mii = Length(str) + 1;
      until str[mii - 1] = ';';
   end;
end;

constructor TCountryList.Create;
begin
   List := TList.Create;
end;

destructor TCountryList.Destroy;
var
   i: integer;
begin
   List.Pack;
   for i := 0 to List.Count - 1 do
      TCountry(List[i]).Free;
   List.Free;
end;

function TCountry.Summary: string;
var
   temp: string;
   B: TBand;
begin
   if CountryName = 'Unknown' then begin
      Result := 'Unknown';
      exit;
   end;
   temp := '';
   temp := FillRight(Country, 7) + FillRight(CountryName, 28) + FillRight(IntToStr(Zone), 2) + ' ' + // ver 0.23
      Continent + '  ';
   for B := b19 to b28 do
      if NotWARC(B) then
         if Worked[B] then
            temp := temp + '* '
         else
            temp := temp + '. ';
   Result := temp;
end;

function TCountry.Summary2: string;
var
   temp: string;
   B: TBand;
   i: integer;
begin
   if CountryName = 'Unknown' then begin
      Result := 'Unknown';
      exit;
   end;
   temp := '';
   temp := FillRight(Country, 7) + FillRight(CountryName, 28) + Continent + '  ';
   temp := temp + 'worked on : ';
   for B := b19 to b28 do
      if NotWARC(B) then
         if Worked[B] then
            temp := temp + MHzString[B] + ' '
         else
            for i := 1 to Length(MHzString[B]) do
               temp := temp + ' ';
   Result := temp;
end;

function TCountry.JustInfo: string;
var
   temp: string;
begin
   if CountryName = 'Unknown' then begin
      Result := 'Unknown';
      exit;
   end;
   temp := '';
   temp := FillRight(Country, 7) + FillRight(CountryName, 28) + Continent + '  ';
   Result := temp;
end;

constructor TCountry.Create;
var
   B: TBand;
begin
   for B := b19 to HiBand do
      Worked[B] := False;
   Country := '';
   CountryName := '';
   Zone := 0;
   Continent := '';
end;

constructor TPrefix.Create;
begin
   Prefix := '';
   Index := 0;
   Length := 0;
   OvrZone := 0;
   OvrContinent := '';
end;

constructor TPrefixList.Create;
begin
   List := TList.Create;
end;

destructor TPrefixList.Destroy;
var
   i: integer;
begin
   List.Pack;
   for i := 0 to List.Count - 1 do
      TPrefix(List[i]).Free;
   List.Free;
end;

procedure TWWMultiForm.FormCreate(Sender: TObject);
begin
   inherited;
   CountryList := TCountryList.Create;
   PrefixList := TPrefixList.Create;

   if FileExists('CTY.DAT') then begin
      LoadCTY_DAT(testCQWW, CountryList, PrefixList);
      // MainForm.StatusLine.SimpleText := 'Loaded CTY.DAT';
   end
   else
      LoadCountryDataFromFile('CQWW.DAT', CountryList, PrefixList);

   Reset; // WWZone.Reset is also called from Reset

end;

procedure TWWMultiForm.ResetBand(B: TBand);
var
   i: integer;
begin
   for i := 0 to CountryList.List.Count - 1 do begin
      TCountry(CountryList.List[i]).Worked[B] := False;
   end;

   for i := 1 to MAXCQZONE do
      Zone[B, i] := False;
   WWZone.ResetBand(B);

   case SortBy.ItemIndex of
      0:
         SortDefault;
      1:
         SortZone;
   end;
end;

procedure TWWMultiForm.Reset;
var
   B: TBand;
   i: integer;
begin
   WWZone.Reset;
   for B := b19 to HiBand do
      for i := 1 to MAXCQZONE do
         Zone[B, i] := False;

   if CountryList.List.Count = 0 then
      exit;

   for i := 0 to CountryList.List.Count - 1 do begin
      for B := b19 to HiBand do
         TCountry(CountryList.List[i]).Worked[B] := False;
   end;

   case SortBy.ItemIndex of
      0:
         SortDefault;
      1:
         SortZone;
   end;
end;

procedure TWWMultiForm.SortDefault;
var
   i, j: integer;
begin
   if CountryList.List.Count = 0 then
      exit;
   j := Grid.TopRow;
   Grid.RowCount := 0;
   Grid.RowCount := CountryList.List.Count;

   for i := 0 to CountryList.List.Count - 1 do begin
      Grid.Cells[0, i] := TCountry(CountryList.List[i]).Summary;
      TCountry(CountryList.List[i]).GridIndex := i;
      GridReverse[i] := i;
   end;
   Grid.TopRow := j;
end;

procedure TWWMultiForm.SortZone;
var
   i, j, x, _top: integer;
begin
   if CountryList.List.Count = 0 then
      exit;
   _top := Grid.TopRow;

   Grid.RowCount := 0;
   Grid.RowCount := CountryList.List.Count;

   Grid.Cells[0, 0] := TCountry(CountryList.List[0]).Summary; // unknown

   x := 1;
   for i := 1 to 40 do begin
      for j := 1 to CountryList.List.Count - 1 do begin
         if TCountry(CountryList.List[j]).Zone = i then begin
            Grid.Cells[0, x] := TCountry(CountryList.List[j]).Summary;
            TCountry(CountryList.List[j]).GridIndex := x;
            GridReverse[x] := j;
            inc(x);
         end;
      end;
   end;
   Grid.TopRow := _top;
end;

procedure TWWMultiForm.Add(aQSO: TQSO);
var
   i: integer;
   C: TCountry;
begin
   if aQSO.Dupe then
      exit;

   if aQSO.NewMulti1 then begin
      try
         i := StrToInt(aQSO.Multi1);
      except
         on EConvertError do
            i := 0;
      end;
      if i in [1 .. 40] then
         WWZone.Mark(aQSO.Band, i);
   end;

   if aQSO.NewMulti2 then begin
      for i := 0 to CountryList.List.Count - 1 do
         if TCountry(CountryList.List[i]).Country = aQSO.Multi2 then begin
            C := TCountry(CountryList.List[i]);
            C.Worked[aQSO.Band] := True;
            Grid.Cells[0, C.GridIndex] := C.Summary;
            exit;
         end;
   end;
end;

{ procedure TWWMultiForm.RecalcBand(B : TBand);
  var Log : TQSOList;
  i : integer;
  aQSO : TQSO;
  begin
  if NotWARC(B) and (B in [b19..b28]) = False then
  exit;
  Log := ServerForm.Stats.Logs[B];
  ResetBand(B);
  aQSO := TQSO.Create;
  for i := 1 to Log.TotalQSO do
  begin
  aQSO.QSO := TQSO(Log.List[i]).QSO;
  Add(aQSO);
  TQSO(Log.List[i]).QSO := aQSO.QSO;
  end;
  aQSO.Free;
  end; }

procedure TWWMultiForm.RecalcAll;
var
   i: integer;
   aQSO: TQSO;
begin
   Reset;
   for i := 1 to ServerForm.Stats.MasterLog.TotalQSO do begin
      aQSO := TQSO(ServerForm.Stats.MasterLog.QSOList[i]);
      Add(aQSO);
   end;
   { aQSO := TQSO.Create;
     for B := b19 to HiBand do
     begin
     if NotWARC(B) then
     begin
     Log := ServerForm.Stats.Logs[B];
     for i := 1 to Log.TotalQSO do
     begin
     aQSO.QSO := TQSO(Log.List[i]).QSO;
     Add(aQSO);
     TQSO(Log.List[i]).QSO := aQSO.QSO;
     end;
     end;
     end;
     aQSO.Free; }
end;

procedure TWWMultiForm.SortByClick(Sender: TObject);
begin
   case SortBy.ItemIndex of
      0:
         SortDefault;
      1:
         SortZone;
   end;
end;

procedure TWWMultiForm.FormShow(Sender: TObject);
begin
   inherited;
   WWZone.Show;
   WWMultiForm.SetFocus;
end;

procedure TWWMultiForm.GoButtonClick(Sender: TObject);
var
   temp: string;
   i: integer;
begin
   temp := Edit1.Text;
   for i := 0 to CountryList.List.Count - 1 do begin
      if Pos(temp, TCountry(CountryList.List[i]).Country) = 1 then begin
         Grid.TopRow := TCountry(CountryList.List[i]).GridIndex;
         break;
      end;
   end;
end;

procedure TWWMultiForm.Button1Click(Sender: TObject);
begin
   Close;
end;

procedure TWWMultiForm.cbStayOnTopClick(Sender: TObject);
begin
   inherited;
   if cbStayOnTop.Checked then
      FormStyle := fsStayOnTop
   else
      FormStyle := fsNormal;
end;

procedure TWWMultiForm.GridSetting(ARow, Acol: integer; var Fcolor: integer; var Bold, Italic, underline: boolean);
begin
   inherited;
   {
     B := Main.CurrentQSO.QSO.Band;
     if TCountry(CountryList.List[GridReverse[ARow]]).Worked[B] then
     FColor := clRed
     else
     FColor := clBlack; }
   Fcolor := clBlack;
end;

procedure TWWMultiForm.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
   inherited;
   if Key = Chr($0D) then begin
      GoButtonClick(Self);
      Key := #0;
   end;
end;

end.
