unit UWWMultiForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, ExtCtrls, JLLabel,
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
  private
    { Private declarations }
    FWWZone: TWWZone;
    FCountryList: TCountryList;
    FPrefixList: TPrefixList;
    FGridReverse: array[0..500] of integer; {pointer from grid row to countrylist index}
  public
    { Public declarations }
    Zone: array[b19..HiBand, 1..MAXCQZONE] of boolean;
    procedure ResetBand(B : TBand); override;
    procedure Reset; override;
    procedure Add(aQSO : TQSO); override;
    procedure SortDefault;
    procedure SortZone;
    procedure RecalcAll; override;
  end;

implementation

uses
  UServerForm;

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

procedure TWWMultiForm.ResetBand(B: TBand);
var
   i: integer;
begin
   for i := 0 to FCountryList.Count - 1 do begin
      TCountry(FCountryList.List[i]).Worked[B] := False;
   end;

   for i := 1 to MAXCQZONE do begin
      Zone[B, i] := False;
   end;

   FWWZone.ResetBand(B);

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
   FWWZone.Reset;
   for B := b19 to HiBand do
      for i := 1 to MAXCQZONE do
         Zone[B, i] := False;

   if FCountryList.Count = 0 then
      exit;

   for i := 0 to FCountryList.Count - 1 do begin
      for B := b19 to HiBand do
         TCountry(FCountryList[i]).Worked[B] := False;
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
   if FCountryList.Count = 0 then
      exit;
   j := Grid.TopRow;
   Grid.RowCount := 0;
   Grid.RowCount := FCountryList.Count;

   for i := 0 to FCountryList.Count - 1 do begin
      Grid.Cells[0, i] := TCountry(FCountryList[i]).Summary;
      TCountry(FCountryList[i]).GridIndex := i;
      FGridReverse[i] := i;
   end;
   Grid.TopRow := j;
end;

procedure TWWMultiForm.SortZone;
var
   i, j, x, _top: integer;
begin
   if FCountryList.Count = 0 then
      exit;
   _top := Grid.TopRow;

   Grid.RowCount := 0;
   Grid.RowCount := FCountryList.Count;

   Grid.Cells[0, 0] := TCountry(FCountryList[0]).Summary; // unknown

   x := 1;
   for i := 1 to 40 do begin
      for j := 1 to FCountryList.Count - 1 do begin
         if TCountry(FCountryList[j]).CQZone = IntToStr(i) then begin
            Grid.Cells[0, x] := TCountry(FCountryList[j]).Summary;
            TCountry(FCountryList.List[j]).GridIndex := x;
            FGridReverse[x] := j;
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
         FWWZone.Mark(aQSO.Band, i);
   end;

   if aQSO.NewMulti2 then begin
      for i := 0 to FCountryList.Count - 1 do
         if TCountry(FCountryList.List[i]).Country = aQSO.Multi2 then begin
            C := TCountry(FCountryList.List[i]);
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

end.
