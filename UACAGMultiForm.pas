unit UACAGMultiForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, JLLabel, ExtCtrls, System.UITypes,
  UBasicMultiForm, UzLogGlobal, UzLogConst, UzLogQSO;

type
  TCity = class
    CityNumber : string;
    CityName : string;
    PrefNumber : string;
    PrefName : string;
    Worked : array[b19..HiBand] of boolean;
    constructor Create;
    function Summary : string;
    function ACAGSummary : string;
    function Summary2 : string;
    function FDSummary(LowBand : TBand) : string;
  end;

  TCityList = class
    List : TList;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(filename : string);
  end;


type
  TACAGMultiForm = class(TBasicMultiForm)
    Panel: TPanel;
    Label1R9: TRotateLabel;
    Label3r5: TRotateLabel;
    Label7: TRotateLabel;
    Label14: TRotateLabel;
    Label21: TRotateLabel;
    Label28: TRotateLabel;
    Label50: TRotateLabel;
    Label144: TRotateLabel;
    Label430: TRotateLabel;
    Label1200: TRotateLabel;
    Label2400: TRotateLabel;
    Label5600: TRotateLabel;
    Label10G: TRotateLabel;
    Grid: TStringGrid;
    Panel1: TPanel;
    Button3: TButton;
    Edit: TEdit;
    Button1: TButton;
    BandCombo: TComboBox;
    cbStayOnTop: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure cbStayOnTopClick(Sender: TObject);
  private
    { Private declarations }
  public
    CityList : TCityList;
    function ReturnSummary(C : TCity) : string; virtual; //returns appropriate summary for each contest
    procedure ResetBand(B : TBand); override;
    procedure Reset; override;
    procedure Add(aQSO : TQSO); override;
    //procedure RecalcBand(B : TBand); override;
    procedure RecalcAll; override;
    { Public declarations }
  end;

implementation

uses
  UServerForm;

{$R *.DFM}

function TACAGMultiForm.ReturnSummary(C: TCity): string;
begin
   Result := C.ACAGSummary;
end;

procedure TACAGMultiForm.RecalcAll;
var
   i: Integer;
   aQSO: TQSO;
begin
   Reset;

   ServerForm.Stats.MasterLog.SetDupeFlags;

   for i := 1 to ServerForm.Stats.MasterLog.TotalQSO do begin
      aQSO := ServerForm.Stats.MasterLog.QSOList[i];

      if ServerForm.Stats.MasterLog.CountHigherPoints = True then begin
         ServerForm.Stats.MasterLog.IsDupe(aQSO); // called to set log.differentmodepointer
      end;

      Add(aQSO);
   end;
end;

procedure TACAGMultiForm.ResetBand(B: TBand);
var
   i: Integer;
begin
   for i := 0 to CityList.List.Count - 1 do begin
      TCity(CityList.List[i]).Worked[B] := False;
      Grid.Cells[0, i] := ReturnSummary(TCity(CityList.List[i]));
   end;
end;

procedure TACAGMultiForm.Reset;
var
   B: TBand;
   i: Integer;
begin
   for i := 0 to CityList.List.Count - 1 do begin
      for B := b19 to HiBand do
         TCity(CityList.List[i]).Worked[B] := False;
      Grid.Cells[0, i] := ReturnSummary(TCity(CityList.List[i]));
   end;
end;

procedure TACAGMultiForm.Add(aQSO: TQSO);
var
   i: Integer;
begin
   if aQSO.Dupe then
      exit;

   aQSO.NewMulti1 := False;
   for i := 0 to CityList.List.Count - 1 do begin
      if TCity(CityList.List[i]).CityNumber = aQSO.Multi1 then begin
         TCity(CityList.List[i]).Worked[aQSO.Band] := True;
         aQSO.NewMulti1 := True;
         Grid.Cells[0, i] := ReturnSummary(TCity(CityList.List[i]));
         Exit;
      end;
   end;
end;

constructor TCity.Create;
var
   B: TBand;
begin
   for B := b19 to HiBand do
      Worked[B] := False;
   CityNumber := '';
   CityName := '';
   PrefNumber := '';
   PrefName := '';
end;

function TCity.Summary: string;
var
   temp: string;
   B: TBand;
begin
   temp := '';
   temp := FillRight(CityNumber, 7) + FillRight(CityName, 20) + ' ';
   for B := b19 to HiBand do begin
      if NotWARC(B) then
         if Worked[B] then
            temp := temp + '* '
         else
            temp := temp + '. ';
   end;

   Result := ' ' + temp;
end;

function TCity.ACAGSummary: string;
var
   temp: string;
   B: TBand;
begin
   temp := '';
   temp := FillRight(CityNumber, 7) + FillRight(CityName, 20) + ' ';
   for B := b19 to HiBand do begin
      if NotWARC(B) then begin
         if Worked[B] then
            temp := temp + '* '
         else
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
   temp := FillRight(CityNumber, 7) + FillRight(CityName, 20) + ' ';
   for B := LowBand to HiBand do begin
      if NotWARC(B) then
         if B in [b19 .. b1200] then begin
            if length(Self.CityNumber) <= 3 then
               if Worked[B] then
                  temp := temp + '* '
               else
                  temp := temp + '. '
            else
               temp := temp + '  ';
         end
         else begin
            if length(Self.CityNumber) > 3 then
               if Worked[B] then
                  temp := temp + '* '
               else
                  temp := temp + '. '
            else
               temp := temp + '  ';
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
   temp := FillRight(CityNumber, 7) + FillRight(CityName, 20) + ' Worked on : ';
   for B := b19 to HiBand do begin
      if Worked[B] then
         temp := temp + ' ' + MHzString[B]
      else
         temp := temp + '';
   end;

   Result := temp;
end;

constructor TCityList.Create;
begin
   List := TList.Create;
end;

destructor TCityList.Destroy;
var
   i: Integer;
begin
   for i := 0 to List.Count - 1 do begin
      if List[i] <> nil then
         TCity(List[i]).Free;
   end;
   List.Free;
end;

procedure TCityList.LoadFromFile(filename: string);
var
   f: textfile;
   str: string;
   C: TCity;
begin
   assign(f, filename);
{$I-}
   Reset(f);
{$I+}
   if IOResult <> 0 then begin
      MessageDlg('DAT file ' + filename + ' cannot be opened', mtError, [mbOK], 0);
      exit; { Alert that the file cannot be opened \\ }
   end;

   {
     try
     reset(f);
     except
     on EFOpenError do
     begin
     MessageDlg('DAT file '+filename+' cannot be opened', mtError,
     [mbOK], 0);
     exit;
     end;
     end; }

   readln(f, str);
   while not(eof(f)) do begin
      readln(f, str);
      if Pos('end of file', LowerCase(str)) > 0 then
         break;
      C := TCity.Create;
      C.CityName := Copy(str, 12, 40);
      C.CityNumber := TrimRight(Copy(str, 1, 11));
      List.Add(C);
   end;
end;

procedure TACAGMultiForm.FormCreate(Sender: TObject);
begin
   inherited;
   CityList := TCityList.Create;
   CityList.LoadFromFile('ACAG.DAT');
   if CityList.List.Count = 0 then
      exit;
   Grid.RowCount := CityList.List.Count - 1;
   Reset;
end;

procedure TACAGMultiForm.Button3Click(Sender: TObject);
var
   i: Integer;
begin
   for i := 0 to CityList.List.Count - 1 do begin
      if Pos(Edit.Text, TCity(CityList.List[i]).CityNumber) = 1 then
         break;
   end;
   if i < Grid.RowCount - 1 - Grid.VisibleRowCount then
      Grid.TopRow := i
   else if CityList.List.Count <= Grid.VisibleRowCount then
      Grid.TopRow := 1
   else
      Grid.TopRow := Grid.RowCount - Grid.VisibleRowCount;
end;

procedure TACAGMultiForm.EditKeyPress(Sender: TObject; var Key: Char);
begin
   inherited;
   if Key = Chr($0D) then begin
      Button3Click(Self);
      Key := #0;
   end;
end;

procedure TACAGMultiForm.cbStayOnTopClick(Sender: TObject);
begin
   if cbStayOnTop.Checked then
      FormStyle := fsStayOnTop
   else
      FormStyle := fsNormal;
end;

end.
