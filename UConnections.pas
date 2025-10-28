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
    FLongDateTime: Boolean;
    procedure SetLongDateTime(v: Boolean);
  public
    { Public declarations }
    procedure UpdateDisplay;
    property LongDateTime: Boolean read FLongDateTime write SetLongDateTime;
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
   fmt: string;
begin
   ListView1.Items.BeginUpdate();
   ListView1.Items.Clear;

   if FLongDateTime = True then begin
      fmt := 'yyyy/mm/dd hh:nn:ss';
   end
   else begin
      fmt := 'hh:nn:ss';
   end;

   // 1234 123456789012345 12345678 123456789 1234567890
   // [1]  127.000.000.100 HH:MM:SS 1.9MHz    JR8PPG
   for i := Low(ServerForm.ClientList) to High(ServerForm.ClientList) do begin
      // 未使用でIPアドレスの記録が無いスロットは出さない
      if (ServerForm.ClientList[i].FUse = False) and (ServerForm.ClientList[i].FIPAddress = '') then begin
         Continue;
      end;

      listitem := ListView1.Items.Add();

      // 接続番号
      listitem.Caption := IntToStr(i);

      // IPアドレス
      listitem.SubItems.Add(ServerForm.ClientList[i].FIPAddress);

      // 接続時刻
      str := FormatDateTime(fmt, ServerForm.ClientList[i].FConnectTime);
      listitem.SubItems.Add(str);

      if (ServerForm.ClientList[i].FUse = False) then begin
         F := nil;

         // 切断時刻
         str := FormatDateTime(fmt, ServerForm.ClientList[i].FDisconnectTime);
         listitem.SubItems.Add(str);

         // バンド
         listitem.SubItems.Add('');

         // オペレーター
         listitem.SubItems.Add('');
      end
      else begin
         F := ServerForm.ClientList[i].FForm;

         // 切断時刻
         listitem.SubItems.Add('');

         // バンド
         str := BandString[F.CurrentBand];
         listitem.SubItems.Add(str);

         // オペレーター
         str := F.CurrentOperator;
         listitem.SubItems.Add(str);
      end;

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
   if F <> nil then begin
      F.Show;
   end;
end;

procedure TConnections.SetLongDateTime(v: Boolean);
begin
   FLongDateTime := v;
   if v = True then begin
      ListView1.Columns[2].Width := 160;
      ListView1.Columns[3].Width := 160;
   end
   else begin
      ListView1.Columns[2].Width := 80;
      ListView1.Columns[3].Width := 80;
   end;
   ListView1.Refresh();
end;

end.
