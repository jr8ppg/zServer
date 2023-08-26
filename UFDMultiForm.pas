unit UFDMultiForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  UACAGMultiForm, StdCtrls, Grids, JLLabel, ExtCtrls,
  UzLogGlobal, UzLogConst, UMultipliers;


const testFD = $01;
      test6D = $02;
type
  TFDMultiForm = class(TACAGMultiForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    TEST : integer;
    function ReturnSummary(C : TCity) : string; override; //returns appropriate summary for each contest
    procedure Init6D; // initializes for 6m & Down Contest
    { Public declarations }
  end;

implementation

{$R *.DFM}

function TFDMultiForm.ReturnSummary(C: TCity): string;
begin
   if TEST = testFD then
      Result := C.FDSummary(b19)
   else
      Result := C.FDSummary(b50);
end;

procedure TFDMultiForm.Init6D;
const
   offset = 61;
begin
   TEST := test6D;
   Label1R9.Visible := False;
   Label3R5.Visible := False;
   Label7.Visible := False;
   Label14.Visible := False;
   Label21.Visible := False;
   Label28.Visible := False;
   Label50.Left := Label50.Left - offset;
   Label144.Left := Label144.Left - offset;
   Label430.Left := Label430.Left - offset;
   Label1200.Left := Label1200.Left - offset;
   Label2400.Left := Label2400.Left - offset;
   Label5600.Left := Label5600.Left - offset;
   Label10G.Left := Label10G.Left - offset;
   Edit.Left := Edit.Left - offset;
   Button3.Left := Button3.Left - offset;
   Width := Width - offset;
   Reset;
end;

procedure TFDMultiForm.FormCreate(Sender: TObject);
begin
   // inherited;
   TEST := testFD;
   Caption := 'Multipliers';
   CityList := TCityList.Create;
   CityList.LoadFromFile('XPO.DAT'); // different from acagmultiform
   CityList.LoadFromFile('ACAG.DAT');
   if CityList.List.Count = 0 then
      exit;
   Grid.RowCount := CityList.List.Count - 1;

   {
     BandCombo.Items.Clear;
     for B := b35 to HiBand do
     if NotWARC(B) then
     BandCombo.Items.Add(MHzString[B]);
     BandCombo.ItemIndex := 0;
   }
   Reset;

end;

end.
