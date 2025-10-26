unit UConnections;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, System.StrUtils,
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
    procedure UpdateDisplay;
  end;

implementation

uses
  UServerForm, UCliForm;

{$R *.DFM}

procedure TConnections.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);
   Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TConnections.UpdateDisplay;
var
   str: string;
   F: TCliForm;
   i: Integer;
begin
   ListBox.Items.Clear;

   // 1234 123456789012345 12345678 123456789 1234567890
   // [1]  127.000.000.100 HH:MM:SS 1.9MHz    JR8PPG
   for i := Low(ServerForm.ClientList) to High(ServerForm.ClientList) do begin
      if ServerForm.ClientList[i].FUse = False then begin
         Continue;
      end;
      F := ServerForm.ClientList[i].FForm;
      str := FillRight('[' + IntToStr(i) + ']', 4) + ' ';
      str := str + FillRight(ServerForm.ClientList[i].FIPAddress, 15) + ' ';
      str := str + FormatDateTime('hh:nn:ss', ServerForm.ClientList[i].FConnectTime) + ' ';
      str := str + FillRight(BandString[F.CurrentBand], 9) + F.CurrentOperator;
      ListBox.Items.AddObject(str, F);
   end;
end;

procedure TConnections.ListBoxDblClick(Sender: TObject);
var
   F: TCliForm;
begin
   F := TCliForm(ListBox.Items.Objects[ListBox.ItemIndex]);
   F.Show;
end;

end.
