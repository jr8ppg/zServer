unit UConnections;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls,
  UzLogGlobal, UzLogConst;

type
  TConnections = class(TForm)
    ListBox: TListBox;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ListBoxDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    CliNumber : array[0..99] of integer;
    procedure UpdateDisplay;
  end;

var
  Connections: TConnections;

implementation

uses
  UServerForm;

{$R *.DFM}

procedure TConnections.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);
   Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TConnections.UpdateDisplay;
var
   B: TBand;
   i: integer;
   str: string;
begin
   ListBox.Items.Clear;
   for B := b19 to HiBand do begin
      for i := 1 to 99 do begin
         if UServerForm.CliList[i] = nil then
            break;
         if UServerForm.CliList[i].CurrentBand = B then begin
            str := FillRight(BandString[UServerForm.CliList[i].CurrentBand], 9) + UServerForm.CliList[i].CurrentOperator;
            ListBox.Items.Add(str);
            CliNumber[ListBox.Items.Count - 1] := i;
         end;
      end;
   end;
end;

procedure TConnections.ListBoxDblClick(Sender: TObject);
begin
   UServerForm.CliList[CliNumber[ListBox.ItemIndex]].Show;
end;

end.
