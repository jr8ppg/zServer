unit UOptions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TformOptions = class(TForm)
    panelFooter: TPanel;
    buttonOK: TButton;
    buttonCancel: TButton;
    groupSecureOptions: TGroupBox;
    checkUseSecure: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    editUserId: TEdit;
    editUserPassword: TEdit;
    groupNetworkOptions: TGroupBox;
    Label3: TLabel;
    comboPort: TComboBox;
    groupOtherOptions: TGroupBox;
    checkTakeChatLog: TCheckBox;
    checkTakeCommandLog: TCheckBox;
    checkLongDateTime: TCheckBox;
    checkUseAutoSave: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private êÈåæ }
    function GetPort(): string;
    procedure SetPort(v: string);
    function GetUseSecure(): Boolean;
    procedure SetUseSecure(v: Boolean);
    function GetLoginID(): string;
    procedure SetLoginID(v: string);
    function GetLoginPassword(): string;
    procedure SetLoginPassword(v: string);
    function GetTakeChatLog(): Boolean;
    procedure SetTakeChatLog(v: Boolean);
    function GetTakeCommandLog(): Boolean;
    procedure SetTakeCommandLog(v: Boolean);
    function GetLongDateTime(): Boolean;
    procedure SetLongDateTime(v: Boolean);
    function GetUseAutoSave(): Boolean;
    procedure SetUseAutoSave(v: Boolean);
  public
    { Public êÈåæ }
    property Port: string read GetPort write SetPort;
    property UseSecure: Boolean read GetUseSecure write SetUseSecure;
    property LoginID: string read GetLoginID write SetLoginID;
    property LoginPassword: string read GetLoginPassword write SetLoginPassword;
    property TakeChatLog: Boolean read GetTakeChatLog write SetTakeChatLog;
    property TakeCommandLog: Boolean read GetTakeCommandLog write SetTakeCommandLog;
    property LongDateTime: Boolean read GetLongDateTime write SetLongDateTime;
    property UseAutoSave: Boolean read GetUseAutoSave write SetUseAutoSave;
  end;

implementation

{$R *.dfm}

procedure TformOptions.FormCreate(Sender: TObject);
begin
//
end;

procedure TformOptions.FormDestroy(Sender: TObject);
begin
//
end;

function TformOptions.GetPort(): string;
begin
   Result := comboPort.Text;
end;

procedure TformOptions.SetPort(v: string);
var
   Index: Integer;
begin
   Index := comboPort.Items.IndexOf(v);
   if Index = -1 then begin
      comboPort.Text := v;
   end
   else begin
      comboPort.ItemIndex := Index;
   end;
end;

function TformOptions.GetUseSecure(): Boolean;
begin
   Result := checkUseSecure.Checked;
end;

procedure TformOptions.SetUseSecure(v: Boolean);
begin
   checkUseSecure.Checked := v;
end;

function TformOptions.GetLoginID(): string;
begin
   Result := editUserId.Text;
end;

procedure TformOptions.SetLoginID(v: string);
begin
   editUserId.Text := v;
end;

function TformOptions.GetLoginPassword(): string;
begin
   Result := editUserPassword.Text;
end;

procedure TformOptions.SetLoginPassword(v: string);
begin
   editUserPassword.Text := v;
end;

function TformOptions.GetTakeChatLog(): Boolean;
begin
   Result := checkTakeChatLog.Checked;
end;

procedure TformOptions.SetTakeChatLog(v: Boolean);
begin
   checkTakeChatLog.Checked := v;
end;

function TformOptions.GetTakeCommandLog(): Boolean;
begin
   Result := checkTakeCommandLog.Checked;
end;

procedure TformOptions.SetTakeCommandLog(v: Boolean);
begin
   checkTakeCommandLog.Checked := v;
end;

function TformOptions.GetLongDateTime(): Boolean;
begin
   Result := checkLongDateTime.Checked;
end;

procedure TformOptions.SetLongDateTime(v: Boolean);
begin
   checkLongDateTime.Checked := v;
end;

function TformOptions.GetUseAutoSave(): Boolean;
begin
   Result := checkUseAutoSave.Checked;
end;

procedure TformOptions.SetUseAutoSave(v: Boolean);
begin
   checkUseAutoSave.Checked := v;
end;

end.
