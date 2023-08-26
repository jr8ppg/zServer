unit UMultipliers;

interface

uses
  SysUtils, Windows, Classes, Dialogs, Forms, UITypes,
  Generics.Collections, Generics.Defaults,
  UzLogConst, UzLogGlobal, UzLogQSO;

const
  MAXCQZONE = 40;

type
  TCountry = class(TObject)
    FName: string;            // Japan, Hawaii, etc
    FCQZone: string;          // CQ Zone
    FITUZone: string;         // ITU Zone
    FContinent: string;       // �嗤
    FLatitude: string;        // �ܓx
    FLongitude: string;       // �o�x
    FUTCOffset: Integer;      // UTC�ɑ΂��鎞��
    FCode: string;            // JA, KH6 etc
    FPrefixes: string;        // ��\�v���t�B�b�N�X

    FWorked: array[b19..HiBand] of Boolean;
    FIndex: Integer;
    FGridIndex: Integer;  // where it is listed in the Grid (row)

    function GetWorked(Index: TBand): Boolean;
    procedure SetWorked(Index: TBand; Value: Boolean);
  public
    constructor Create(); overload;
    constructor Create(strText: string); overload;

    function Summary : string;
    function SummaryWAE : string;
    function Summary2 : string;
    function SummaryARRL10 : string;
    function SummaryGeneral : string;
    function JustInfo : string; // returns cty name, px and continent

    procedure Parse(strText: string);
    property CountryName: string read FName write FName;
    property CQZone: string read FCQZone write FCQZone;
    property ITUZone: string read FITUZone write FITUZone;
    property Continent: string read FContinent write FContinent;
    property Latitude: string read FLatitude write FLatitude;
    property Longitude: string read FLongitude write FLongitude;
    property UTCOffset: Integer read FUTCOffset write FUTCOffset;
    property Country: string read FCode write FCode;
    property Prefixes: string read FPrefixes write FPrefixes;
    property Worked[Index: TBand]: Boolean read GetWorked write SetWorked;
    property Index: Integer read FIndex write FIndex;
    property GridIndex: Integer read FGridIndex write FGridIndex;
  end;

  TCountryList = class(TObjectList<TCountry>)
  private
  public
    constructor Create(OwnsObjects: Boolean = True);
    procedure LoadFromFile(strFileName: string);
    procedure Reset();
  end;

  TPrefix = class(TObject)
    FPrefix: string;
    FOvrCQZone: string;         // override zone
    FOvrITUZone: string;
    FOvrContinent: string;  // override continent
    FCountry: TCountry;
    FFullMatch: Boolean;
  public
    constructor Create();
    property Prefix: string read FPrefix write FPrefix;
    property OvrCQZone: string read FOvrCQZone write FOvrCQZone;
    property OvrITUZone: string read FOvrITUZone write FOvrITUZone;
    property OvrContinent: string read FOvrContinent write FOvrContinent;
    property Country: TCountry read FCountry write FCountry;
    property FullMatch: Boolean read FFullMatch write FFullMatch;
  end;

  TPrefixComparer = class(TComparer<TPrefix>)
  public
    function Compare(const Left, Right: TPrefix): Integer; override;
  end;

  TPrefixList = class(TObjectList<TPrefix>)
  private
    FPrefixComparer: TPrefixComparer;
  public
    constructor Create(OwnsObjects: Boolean = True);
    destructor Destroy(); override;
    procedure Parse(cty: TCountry);
    procedure Sort(); overload;
  end;

  TCity = class
    CityNumber : string;
    CityName : string;
    PrefNumber : string;
    PrefName : string;
    Worked : array[b19..HiBand] of boolean;
    Index : integer;
    constructor Create;
    function Abbrev : string;
    function Summary : string;
    function SummaryGeneral : string;
    function Summary2 : string;
    function FDSummary(LowBand : TBand) : string;
    function WorkedOn(): string;
  end;

  TCityList = class
  private
    FList: TList;
    FSortedMultiList: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    function GetCity(Name : string): TCity;
    procedure LoadFromFile(filename: string);
    function AddAndSort(C : TCity): integer; // returns the index inserted
    property List: TList read FList;
    property SortedMultiList: TStringList read FSortedMultiList;
  end;

function LoadCTY_DAT(CL: TCountryList; PL: TPrefixList): Boolean;
//function GetPrefix(aQSO : TQSO) : TPrefix;
//function GetArea(str : string) : integer;
//function GuessCQZone(aQSO : TQSO) : string;

implementation

constructor TCountryList.Create(OwnsObjects: Boolean);
begin
   Inherited Create(OwnsObjects);
end;

procedure TCountryList.LoadFromFile(strFileName: string);
var
   mem: TMemoryStream;
   i: Integer;
   ch: AnsiChar;
   buf: Byte;
   strLine: string;
   C: TCountry;
begin
   mem := TMemoryStream.Create();
   try
      C := TCountry.Create();
      C.CountryName := 'Unknown';
      Add(C);

      if FileExists(strFileName) = False then begin
         Exit;
      end;

      mem.LoadFromFile(strFileName);
      mem.Position := 0;

      strLine := '';
      for i := 0 to mem.Size - 1 do begin
         mem.Read(buf, 1);
         ch := AnsiChar(buf);

         if ch = AnsiChar($0d) then begin
            Continue;
         end;
         if ch = AnsiChar($0a) then begin
            Continue;
         end;

         if ch = ';' then begin
            C := TCountry.Create(strLine);
            C.Index := Count;
            Add(C);
            strLine := '';
         end
         else begin
            strLine := strLine + Char(ch);
         end;
      end;

   finally
      mem.Free();
   end;
end;

procedure TCountryList.Reset();
var
   i: Integer;
   B: TBand;
begin
   for i := 0 to Count - 1 do begin
      for B := b19 to HiBand do begin
         TCountry(List[i]).Worked[B] := False;
      end;
   end;
end;

function TCountry.Summary: string;
var
   temp: string;
   B: TBand;
begin

   if CountryName = 'Unknown' then begin
      Result := 'Unknown Country';
      exit;
   end;

   temp := '';
   temp := FillRight(Country, 7) +
           FillRight(CountryName, 28) +
           FillRight(CQZone, 2) + ' ' + // ver 0.23
           Continent + '  ';

   for B := b19 to b28 do begin
      if NotWARC(B) then begin
         if Worked[B] then
            temp := temp + '* '
         else
            temp := temp + '. ';
      end;
   end;

   Result := temp;
end;

function TCountry.SummaryWAE: string;
var
   temp: string;
   B: TBand;
begin
   if CountryName = 'Unknown' then begin
      Result := 'Unknown Country';
      exit;
   end;

   temp := '';
   temp := FillRight(Country, 7) +
           FillRight(CountryName, 28) + '   ' + Continent + '    ';

   for B := b35 to b28 do begin
      if NotWARC(B) then begin
         if Worked[B] then
            temp := temp + '* '
         else
            temp := temp + '. ';
      end;
   end;

   Result := temp;
end;

function TCountry.SummaryGeneral: string;
var
   temp: string;
   B: TBand;
   temp2: string;
begin
   if CountryName = 'Unknown' then begin
      Result := 'Unknown Country';
      exit;
   end;

   temp := '';
   temp2 := CountryName;
   temp := FillRight(Country, 6) +
           FillRight(temp2, 16) +
           FillRight(CQZone, 2) + ' ' + // ver 0.23
           Continent + '  ';

   for B := b19 to HiBand do begin
//      if (MainForm.BandMenu.Items[Ord(B)].Visible = True) and
//         (dmZlogGlobal.Settings._activebands[B] = True) then begin
         if Worked[B] then begin
            temp := temp + '* ';
         end
         else begin
            temp := temp + '. ';
         end;
//      end;
   end;

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

   for B := b19 to b28 do begin
      if NotWARC(B) then begin
         if Worked[B] then
            temp := temp + MHzString[B] + ' '
         else
            for i := 1 to Length(MHzString[B]) do begin
               temp := temp + ' ';
            end;
      end;
   end;

   Result := temp;
end;

function TCountry.SummaryARRL10: string;
var
   temp: string;
   B: TBand;
begin
   if CountryName = 'Unknown' then begin
      Result := ' Unknown country';
      exit;
   end;

   temp := ' ' + FillRight(Country, 7) +
                 FillRight(CountryName, 28) + Continent + '  ';

   if IsWVE(Country) then begin
      Result := temp + 'N/A';
      exit;
   end;

   for B := b19 to b35 do
      if Worked[B] then
         temp := temp + '*  '
      else
         temp := temp + '.  ';

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
   temp := FillRight(Country, 7) +
           FillRight(CountryName, 28) + Continent + '  ';

   Result := temp;
end;

constructor TCountry.Create();
var
   B: TBand;
begin
   Inherited;

   FName := '';
   FCQZone := '';
   FITUZone := '';
   FContinent := '';
   FLatitude := '';
   FLongitude := '';
   FUTCOffset := 0;
   FCode := '';
   FPrefixes := '';

   for B := b19 to HiBand do begin
      Worked[B] := false;
   end;

   FIndex := -1;
   FGridIndex := -1;
end;

constructor TCountry.Create(strText: string);
begin
   Inherited Create();
   Parse(strText);
end;

procedure TCountry.Parse(strText: string);
var
   slLine: TStringList;
   i: Integer;
begin
   slLine := TStringList.Create();
   slLine.StrictDelimiter := True;
   slLine.Delimiter := ':';
   try
      slLine.DelimitedText := strText;

      for i := 0 to slLine.Count - 1 do begin
         slLine[i] := Trim(slLine[i]);
      end;

      FName       := slLine[0];
      FCQZone     := slLine[1];
      FITUZone    := slLine[2];
      FContinent  := slLine[3];
      FLatitude   := slLine[4];
      FLongitude  := slLine[5];
      FUTCOffset  := StrToIntDef(slLine[6], 0);
      FCode       := slLine[7];
      FPrefixes   := slLine[8];

   finally
      slLine.Free();
   end;
end;

function TCountry.GetWorked(Index: TBand): Boolean;
begin
   Result := FWorked[Index];
end;

procedure TCountry.SetWorked(Index: TBand; Value: Boolean);
begin
   FWorked[Index] := Value;
end;

constructor TPrefix.Create();
begin
   Inherited;
   FPrefix := '';
   FOvrCQZone := '';
   FOvrITUZone := '';
   FOvrContinent := '';
   FCountry := nil;
   FFullMatch := False;
end;

constructor TPrefixList.Create(OwnsObjects: Boolean);
begin
   Inherited Create(OwnsObjects);
   FPrefixComparer := TPrefixComparer.Create();
end;

destructor TPrefixList.Destroy();
begin
   Inherited;
   FPrefixComparer.Free();
end;

procedure TPrefixList.Parse(cty: TCountry);
var
   i: Integer;
   slText: TStringList;
   P: TPrefix;
   strPrefix: string;
   strOvrCQZone: string;
   strOvrITUZone: string;
   strOvrContinent: string;
   strUnused: string;
   fFullMatch: Boolean;

   function ExtractNumber(var strPrefix: string; strBegin, strEnd: string): string;
   var
      p1, p2: Integer;
   begin
      p1 := Pos(strBegin, strPrefix);
      if p1 <= 0 then begin
         Result := '';
         Exit;
      end;

      p2 := Pos(strEnd, strPrefix, p1 + 1);
      if p2 <= 0 then begin
         p2 := Length(strPrefix);
      end;

      Result := Copy(strPrefix, p1 + 1, p2 - p1 - 1);
      System.Delete(strPrefix, p1, p2 - p1 + 1);
   end;
begin
   slText := TStringList.Create();
   slText.StrictDelimiter := True;
   try
      slText.CommaText := cty.Prefixes;

      for i := 0 to slText.Count - 1 do begin
         strPrefix := Trim(slText[i]);

         // =�Ŏn�܂镨�͊��S��v�R�[��
         if strPrefix[1] = '=' then begin
            strPrefix := Copy(strPrefix, 2);
            fFullMatch := True;
         end
         else begin
            fFullMatch := False;
         end;

         // ()��Override CQ Zone
         strOvrCQZone := ExtractNumber(strPrefix, '(', ')');

         // []��Override ITU Zone
         strOvrITUZone := ExtractNumber(strPrefix, '[', ']');

         // {}��Override Continent
         strOvrContinent := ExtractNumber(strPrefix, '{', '}');

         // <#/#>��Override latitude/longitude
         strUnused := ExtractNumber(strPrefix, '<', '>');

         // ~#~��Override UTCOffset
         strUnused := ExtractNumber(strPrefix, '~', '~');

         P := TPrefix.Create();
         P.Prefix := strPrefix;
         P.Country := cty;
         P.OvrCQZone := strOvrCQZone;
         P.OvrITUZone := strOvrITUZone;
         P.OvrContinent := strOvrContinent;
         P.FFullMatch := fFullMatch;
         Add(P);
      end;

   finally
      slText.Free();
   end;
end;

procedure TPrefixList.Sort();
begin
   Sort(FPrefixComparer);
end;

{ TPrefixComparer }

function TPrefixComparer.Compare(const Left, Right: TPrefix): Integer;
begin
   Result := CompareText(Right.Prefix, Left.Prefix);
end;

function LoadCTY_DAT(CL: TCountryList; PL: TPrefixList): Boolean;
var
   i: Integer;
   P: TPrefix;
   strFileName: string;
begin
   strFileName := ExtractFilePath(Application.ExeName) + 'CTY.DAT';

   // �J���g���[���X�g�����[�h
   CL.LoadFromFile(strFileName);

   // �e�J���g���[��prefix��W�J
   for i := 0 to CL.Count - 1 do begin
      PL.Parse(CL[i]);
   end;

   // ���ёւ��i�~���j
   PL.Sort();

   // �擪��Unknown Country�̃_�~�[���R�[�h�ǉ�
   P := TPrefix.Create();
   P.Prefix := 'Unknown';
   P.Country := CL[0];
   PL.Insert(0, P);

   Result := True;
end;

{
function GetPrefix(aQSO: TQSO): TPrefix;
var
   str: string;
   i: integer;
   P: TPrefix;
   strCallRight: string;
   strCallFirst: string;
begin
   str := aQSO.CallSign;
   if str = '' then begin
      Result := PrefixList[0];
      Exit;
   end;

   // �ŏ��̓R�[����v�m�F
   for i := 0 to PrefixList.Count - 1 do begin
      P := PrefixList[i];

      if (P.FullMatch = True) and (P.Prefix = str) then begin
         Result := P;
         Exit;
      end;
   end;

   i := Pos('/', str);
   if i > 0 then begin
      strCallFirst := Copy(str, 1, i - 1);
      strCallRight := Copy(str, i + 1);
   end
   else begin
      strCallFirst := str;
      strCallRight := '';
   end;

   // Marine Mobile
   if strCallRight = 'MM' then begin
      Result := PrefixList[0];
      Exit;
   end

   // �����������
   else if (strCallRight = 'AA') or (strCallRight = 'AT') or (strCallRight = 'AG') or
      (strCallRight = 'AA') or (strCallRight = 'AE') or (strCallRight = 'M') or
      (strCallRight = 'P') or (strCallRight = 'AM') or (strCallRight = 'QRP') or
      (strCallRight = 'A') or (strCallRight = 'KT') or (strCallRight = 'N') or
      (strCallRight = 'T') or
      (strCallRight = '0') or (strCallRight = '1') or (strCallRight = '2') or
      (strCallRight = '3') or (strCallRight = '4') or (strCallRight = '5') or
      (strCallRight = '6') or (strCallRight = '7') or (strCallRight = '8') or
      (strCallRight = '9') then begin
      str := Copy(str, 1, i - 1);
   end

   // ���ʂł��Ȃ�
   else if i = 5 then begin
      // �܂��͍�������O����v��
      for i := 1 to PrefixList.Count - 1 do begin
         P := PrefixList[i];

         if P.FullMatch = False then begin
            if Copy(strCallFirst, 1, Length(P.Prefix)) = P.Prefix then begin
               Result := P;
               Exit;
            end;
         end;
      end;

      // ������ΉE��
      strCallFirst := strCallRight;
   end;

   // �����đO����v��
   for i := 1 to PrefixList.Count - 1 do begin
      P := PrefixList[i];

      if P.FullMatch = False then begin
         if Copy(strCallFirst, 1, Length(P.Prefix)) = P.Prefix then begin
            Result := P;
            Exit;
         end;
      end;
   end;

   Result := PrefixList[0];
end;
}

{
function GetArea(str: string): integer;
var
   j, k: integer;
begin
   j := pos('/', str);
   if j > 4 then begin
      for k := Length(str) downto 1 do
         if CharInSet(str[k], ['0' .. '9']) = True then
            break;
   end
   else begin
      for k := 1 to Length(str) do
         if CharInSet(str[k], ['0' .. '9']) = True then
            break;
   end;

   if CharInSet(str[k], ['0' .. '9']) = True then
      k := ord(str[k]) - ord('0')
   else
      k := 6;

   Result := k;
end;
}

{
function expos(substr, str: string): integer;
var
   i, j: integer;
   bad: boolean;
begin
   Result := 0;
   if (Length(substr) > Length(str)) or (substr = '') then
      exit;
   for i := 1 to (Length(str) - Length(substr) + 1) do begin
      bad := false;
      for j := 1 to Length(substr) do begin
         if substr[j] <> '?' then
            if substr[j] <> str[i + j - 1] then
               bad := true;
      end;
      if bad = false then begin
         Result := i;
         exit;
      end;
   end;
end;
}

(*
function GuessCQZone(aQSO: TQSO): string;
var
   i, k: integer;
   C: TCountry;
   p: TPrefix;
   str: string;
begin
   p := GetPrefix(aQSO);
   if p = nil then begin
      Result := '';
      exit;
   end
   else begin
      C := P.Country;
   end;

   str := aQSO.CallSign;
   i := StrToIntDef(C.CQZone, 0);

   if (C.Country = 'W') or (C.Country = 'K') then begin
      k := GetArea(str);
      case k of
         1 .. 4:
            i := 5;
         5, 8, 9, 0:
            i := 4;
         6, 7:
            i := 3;
      end;
   end;

   if C.Country = 'VE' then begin
      k := GetArea(str);
      case k of
         1, 2, 9:
            i := 5;
         3 .. 6:
            i := 4;
         7:
            i := 3;
         8:
            i := 1;
         0:
            i := 2;
      end;
   end;

   if C.Country = 'VK' then begin
      k := GetArea(str);
      case k of
         1 .. 5, 7:
            i := 30;
         6, 8:
            i := 29;
         9, 0:
            i := 30; { Should not happen }
      end;
   end;

   if C.Country = 'BY' then begin
      k := GetArea(str);
      case k of
         1 .. 8:
            i := 24;
         9, 0:
            i := 23;
      end;
   end;

   if (C.Country = 'UA') or (C.Country = 'UA0') or (C.Country = 'UA9') then begin
      if (expos('U?0', str) > 0) or (pos('R?0', str) > 0) or (pos('R0', str) > 0) then begin
         k := pos('0', str);
         if Length(str) >= k + 1 then
            case str[k + 1] of
               'A', 'B', 'H', 'O', 'P', 'S', 'T', 'U', 'V', 'W':
                  i := 18;
               'Y':
                  i := 23;
               else
                  i := 19;
            end;
      end;

      if (expos('U?8', str) > 0) or (expos('R?8', str) > 0) then begin
         i := 18;
      end;
      if (expos('U?9', str) > 0) or (pos('R?9', str) > 0) then begin
         k := pos('9', str);
         if Length(str) >= k + 1 then
            case str[k + 1] of
               'S', 'T', 'W':
                  i := 16;
               'H', 'I', 'O', 'P', 'U', 'V', 'Y', 'Z':
                  i := 18;
               else
                  i := 17;
            end;
      end;
   end;

   if P.OvrCQZone <> '' then begin
      i := StrToIntDef(P.OvrCQZone, 0);
   end;

   if i = 0 then
      Result := ''
   else
      Result := IntToStr(i);
end;
*)

{ TCity }

constructor TCity.Create;
var
   B: TBand;
begin
   for B := b19 to HiBand do
      Worked[B] := false;

   CityNumber := '';
   CityName := '';
   PrefNumber := '';
   PrefName := '';
end;

function TCity.Abbrev: string;
var
   str: string;
begin
   str := CityNumber;
   if pos(',', str) > 0 then
      str := copy(str, 1, pos(',', str) - 1);
   Result := str;
end;

function TCity.Summary: string;
var
   temp, _cityname: string;
   B: TBand;
begin
   temp := '';
   if Length(CityName) > 20 then begin
      _cityname := copy(CityName, 1, 20);
   end
   else begin
      _cityname := CityName;
   end;

   temp := FillRight( { CityNumber } Abbrev, 7) +
           FillRight(_cityname, 20) + ' ';

   for B := b19 to HiBand do begin
      if NotWARC(B) then begin
         if Worked[B] then begin
            temp := temp + '* ';
         end
         else begin
            temp := temp + '. ';
         end;
      end;
   end;

   Result := ' ' + temp;
end;

function TCity.SummaryGeneral: string;
var
   temp, _cityname: string;
   B: TBand;
begin
   temp := '';
   if Length(CityName) > 20 then begin
      _cityname := copy(CityName, 1, 20);
   end
   else begin
      _cityname := CityName;
   end;

   temp := FillRight( { CityNumber } Abbrev, 7) +
           FillRight(_cityname, 24) + ' ';

   for B := b19 to HiBand do begin
      if Worked[B] then begin
         temp := temp + '* ';
      end
      else begin
         temp := temp + '. ';
      end;
   end;

   Result := ' ' + temp;
end;

function TCity.FDSummary(LowBand: TBand): string;
var
   temp: string;
   B: TBand;
begin
   temp := '';
   temp := FillRight(CityNumber, 7) +
           FillRight(CityName, 20) + ' ';

   for B := LowBand to HiBand do begin
      if NotWARC(B) then begin
         if B in [b19 .. b1200] then begin
            if Length(Self.CityNumber) <= 3 then
               if Worked[B] then
                  temp := temp + '* '
               else
                  temp := temp + '. '
            else
               temp := temp + '  ';
         end
         else begin // 2.4G and upper
            if Length(Self.CityNumber) > 3 then
               if Worked[B] then
                  temp := temp + '* '
               else
                  temp := temp + '. '
            else
               temp := temp + '  ';
         end;
      end;
   end;

   Result := ' ' + temp;
end;

function TCity.Summary2: string;
var
   temp: string;
   B: TBand;
begin
   temp := '';
   temp := FillRight( { CityNumber } Abbrev, 7) +
           FillRight(CityName, 20) + ' Worked on : ';

   for B := b35 to HiBand do begin
      if Worked[B] then
         temp := temp + ' ' + MHzString[B]
      else
         temp := temp + '';
   end;

   Result := temp;
end;

function TCity.WorkedOn(): string;
var
   temp: string;
   B: TBand;
begin
   temp := '';

   for B := b35 to HiBand do begin
      if Worked[B] then begin
         temp := temp + ' ' + MHzString[B];
      end;
   end;

   if temp <> '' then begin
      temp := 'Worked on:' + temp;
   end;

   Result := temp;
end;

{ TCityList }

constructor TCityList.Create;
begin
   FList := TList.Create;
   FSortedMultiList := TStringList.Create;
   FSortedMultiList.Sorted := true;
end;

procedure TCityList.Reset;
var
   i: integer;
   B: TBand;
begin
   for i := 0 to List.Count - 1 do
      for B := b19 to HiBand do
         TCity(List[i]).Worked[B] := false;
end;

function TCityList.GetCity(Name: string): TCity;
var
   i: integer;
begin
   Result := nil;
   i := FSortedMultiList.IndexOf(Name);
   if i >= 0 then
      Result := TCity(FSortedMultiList.Objects[i]);
end;

destructor TCityList.Destroy;
var
   i: integer;
begin
   for i := 0 to FList.Count - 1 do begin
      if FList[i] <> nil then
         TCity(FList[i]).Free;
   end;
   FList.Free;
   FSortedMultiList.Clear;
   FSortedMultiList.Free;
end;

procedure TCityList.LoadFromFile(filename: string);
var
   str: string;
   C: TCity;
   i: integer;
   fullpath: string;
   SL: TStringList;
   L: Integer;
begin
   fullpath := ExtractFilePath(Application.ExeName) + filename;
   if FileExists(fullpath) = False then begin
      SL := LoadFromResourceName(SysInit.HInstance, filename);
   end
   else begin
      SL := TStringList.Create();
      SL.LoadFromFile(fullpath);
   end;

   for L := 1 to SL.Count - 1 do begin

      str := SL[L];

      if pos('end of file', LowerCase(str)) > 0 then begin
         break;
      end;

      C := TCity.Create;

      i := pos(' ', str);
      if i > 1 then begin
         C.CityNumber := copy(str, 1, i - 1);
      end;

      Delete(str, 1, i);
      C.CityName := TrimRight(TrimLeft(str));

      C.Index := List.Count;

      FList.Add(C);
      FSortedMultiList.AddObject(C.CityNumber, C);
   end;

   SL.Free();
end;

function TCityList.AddAndSort(C: TCity): integer;
var
   i: integer;
begin
   if FList.Count = 0 then begin
      FList.Add(C);
      Result := 0;
      exit;
   end;

   for i := 0 to List.Count - 1 do begin
      if StrMore(TCity(List[i]).CityNumber, C.CityNumber) then begin
         FList.Insert(i, C);
         Result := i;
         exit;
      end;
   end;

   FList.Add(C);

   Result := List.Count - 1;
end;

end.
