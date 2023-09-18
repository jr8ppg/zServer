unit UACAGMultiForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, JLLabel, ExtCtrls, System.UITypes,
  UBasicMultiForm, UzLogGlobal, UzLogConst, UzLogQSO, UMultipliers;

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
    { Public declarations }
    CityList : TCityList;
    function ReturnSummary(C : TCity) : string; virtual; //returns appropriate summary for each contest
    procedure Reset; override;
    procedure Add(aQSO : TQSO); override;
  end;

implementation

uses
  UServerForm;

{$R *.DFM}

function TACAGMultiForm.ReturnSummary(C: TCity): string;
begin
   Result := C.Summary;
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

   for i := 0 to CityList.List.Count - 1 do begin
      if TCity(CityList.List[i]).CityNumber = aQSO.Multi1 then begin
         if TCity(CityList.List[i]).Worked[aQSO.Band] = False then begin
            TCity(CityList.List[i]).Worked[aQSO.Band] := True;
            Grid.Cells[0, i] := ReturnSummary(TCity(CityList.List[i]));
            Exit;
         end;
      end;
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
