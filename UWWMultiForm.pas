unit UWWMultiForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, ExtCtrls, StrUtils, JLLabel,
  UBasicMultiForm, UWWZone, UMultipliers,
  UzLogGlobal, UzLogConst, UzLogQSO;

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
    Grid: TStringGrid;
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
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure FormDestroy(Sender: TObject);
    procedure GridTopLeftChanged(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FWWZone: TWWZone;
    FCountryList: TCountryList;
    FPrefixList: TPrefixList;
    FGridReverse: array[0..500] of integer; {pointer from grid row to countrylist index}
    procedure SortDefault;
    procedure SortZone;
    procedure SortContinent;
    procedure RefreshGrid;
  public
    { Public declarations }
    procedure Reset; override;
    procedure Add(aQSO : TQSO); override;
  end;

implementation

{$R *.DFM}

procedure TWWMultiForm.FormCreate(Sender: TObject);
begin
   inherited;
   FCountryList := TCountryList.Create;
   FPrefixList := TPrefixList.Create;
   FWWZone := TWWZone.Create(Self);

   if FileExists('CTY.DAT') then begin
      LoadCTY_DAT(FCountryList, FPrefixList);
      // MainForm.StatusLine.SimpleText := 'Loaded CTY.DAT';
   end
   else begin
      // LoadCountryDataFromFile('CQWW.DAT', CountryList, PrefixList);
   end;

   Reset; // WWZone.Reset is also called from Reset
end;

procedure TWWMultiForm.FormDestroy(Sender: TObject);
begin
   inherited;
   FWWZone.Release();
   FCountryList.Free();
   FPrefixList.Free();
end;

procedure TWWMultiForm.FormResize(Sender: TObject);
begin
   inherited;
   Grid.ColWidths[0] := Grid.Width;
   RefreshGrid;
end;

procedure TWWMultiForm.Reset;
var
   B: TBand;
   i: integer;
begin
   FWWZone.Reset;

   if FCountryList.Count = 0 then
      exit;

   for i := 0 to FCountryList.Count - 1 do begin
      for B := b19 to HiBand do
         TCountry(FCountryList[i]).Worked[B] := False;
   end;

   case SortBy.ItemIndex of
      0: SortDefault;
      1: SortZone;
      2: SortContinent;
   end;

   Grid.RowCount := FCountryList.Count;
end;

procedure TWWMultiForm.SortDefault;
var
   i: integer;
begin
   if FCountryList.Count = 0 then
      exit;

   for i := 0 to FCountryList.Count - 1 do begin
      TCountry(FCountryList[i]).GridIndex := i;
      FGridReverse[i] := i;
   end;
end;

procedure TWWMultiForm.SortZone;
var
   i, j, x: integer;
   strZone: string;
begin
   if FCountryList.Count = 0 then begin
      exit;
   end;

   FGridReverse[0] := 0;
   x := 1;
   for i := 1 to 40 do begin
      strZone := RightStr('00' + IntToStr(i), 2);
      for j := 1 to FCountryList.Count - 1 do begin
         if TCountry(FCountryList.List[j]).CQZone = strZone then begin
            TCountry(FCountryList.List[j]).GridIndex := x;
            FGridReverse[x] := j;
            inc(x);
         end;
      end;
   end;
end;

procedure TWWMultiForm.SortContinent;
var
   i, j, x: integer;
   cont : array[0..5] of string;
begin
   cont[0] := 'AS';
   cont[1] := 'AF';
   cont[2] := 'EU';
   cont[3] := 'NA';
   cont[4] := 'SA';
   cont[5] := 'OC';
   if FCountryList.Count = 0 then exit;
   FGridReverse[0] := 0;
   x := 1;

   for i := 0 to 5 do begin
      for j := 1 to FCountryList.Count - 1 do begin
         if TCountry(FCountryList.List[j]).Continent = cont[i] then begin
            TCountry(FCountryList.List[j]).GridIndex := x;
            FGridReverse[x] := j;
            inc(x);
         end;
      end;
   end;
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
         FWWZone.Mark(aQSO.Band, i);
   end;

   if aQSO.NewMulti2 then begin
      for i := 0 to FCountryList.Count - 1 do begin
         if TCountry(FCountryList.List[i]).Country = aQSO.Multi2 then begin
            C := TCountry(FCountryList.List[i]);
            C.Worked[aQSO.Band] := True;
            Grid.Cells[0, C.GridIndex] := C.Summary;
            Break;
         end;
      end;
   end;

   RefreshGrid;
end;

procedure TWWMultiForm.SortByClick(Sender: TObject);
begin
   case SortBy.ItemIndex of
      0 : SortDefault;
      1 : SortZone;
      2 : SortContinent;
   end;

   RefreshGrid;
end;

procedure TWWMultiForm.FormShow(Sender: TObject);
begin
   inherited;
   FWWZone.Show;
   SetFocus;
end;

procedure TWWMultiForm.GoButtonClick(Sender: TObject);
var
   temp: string;
   i: integer;
begin
   temp := Edit1.Text;
   for i := 0 to FCountryList.Count - 1 do begin
      if Pos(temp, TCountry(FCountryList.List[i]).Country) = 1 then begin
         Grid.TopRow := TCountry(FCountryList.List[i]).GridIndex;
         break;
      end;
   end;
end;

procedure TWWMultiForm.GridTopLeftChanged(Sender: TObject);
begin
   inherited;
   RefreshGrid;
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

procedure TWWMultiForm.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
   inherited;
   if Key = Chr($0D) then begin
      GoButtonClick(Self);
      Key := #0;
   end;
end;

procedure TWWMultiForm.RefreshGrid;
var
   i , k : integer;
   C: TCountry;
begin
   for i := Grid.TopRow to Grid.TopRow + Grid.VisibleRowCount - 1 do begin
      if (i > Grid.RowCount - 1) then begin
         exit;
      end
      else begin
         k := FGridReverse[i];
         C := TCountry(FCountryList.List[k]);
         if (k >= 0) and (k < FCountryList.Count) then begin
            Grid.Cells[0, i] := C.Summary;
         end
         else begin
            Grid.Cells[0, i] := '';
         end;
      end;
   end;
end;

end.
