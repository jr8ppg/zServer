unit UConnections;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, System.StrUtils,
  UzLogGlobal, UzLogConst, Vcl.ComCtrls;

type
  TConnections = class(TForm)
    ListView1: TListView;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ListView1DblClick(Sender: TObject);
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
   listitem: TListItem;
begin
   ListView1.Items.BeginUpdate();
   ListView1.Items.Clear;

   // 1234 123456789012345 12345678 123456789 1234567890
   // [1]  127.000.000.100 HH:MM:SS 1.9MHz    JR8PPG
   for i := Low(ServerForm.ClientList) to High(ServerForm.ClientList) do begin
      if ServerForm.ClientList[i].FUse = False then begin
         Continue;
      end;

      F := ServerForm.ClientList[i].FForm;

      listitem := ListView1.Items.Add();

      listitem.Caption := IntToStr(i);
      listitem.SubItems.Add(ServerForm.ClientList[i].FIPAddress);
      str := FormatDateTime('hh:nn:ss', ServerForm.ClientList[i].FConnectTime);
      listitem.SubItems.Add(str);

      str := BandString[F.CurrentBand];
      listitem.SubItems.Add(str);

      str := F.CurrentOperator;
      listitem.SubItems.Add(str);

      listitem.Data := F;
   end;

   ListView1.Items.EndUpdate();
end;

procedure TConnections.ListView1DblClick(Sender: TObject);
var
   F: TCliForm;
begin
   if ListView1.Selected = nil then begin
      Exit;
   end;

   F := TCliForm(ListView1.Selected.Data);
   F.Show;
end;

end.
