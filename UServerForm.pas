{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Yokobayashi Yohei, JARL Contest Committee, JR8PPG
Description:  This program acts as a server when using zLog for multiple operations.
EMail:        zlog@zlog.org, jr8ppg@jarl.com
Creation:     1 August 2021
Version:      2.8
WebSite:      https://www.zlog.org/
              https://github.com/jr8ppg/zServer
Support:      Use the mailing list on zlog-reiwa@cq-test.net
              Use the @zLog_support on X(twitter)
              See website for details below.
              https://zlog.org/
Legal issues: Copyright 1997-2002 by Yohei Yokobayashi
              Portions created by JR8PPG are Copyright (C) 2020-2022 JR8PPG
              This software is released under the MIT License.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit UServerForm;

interface

uses
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Buttons,
  Vcl.Dialogs, Vcl.StdCtrls, System.IniFiles, Vcl.Menus, Vcl.ExtCtrls,
  System.UITypes, System.SyncObjs,
  System.Generics.Collections, System.Generics.Defaults,
  JclFileUtils,
  OverbyteIcsWndControl, OverbyteIcsWSocket, OverbyteIcsUtils,
  OverbyteIcsTypes, OverbyteIcsSslBase, OverbyteIcsWSocketS,
  UBasicStats, UBasicMultiForm, UCliForm, UFreqList, UConnections, UOptions,
  UzLogGlobal, UzLogConst, UzLogQSO, UzLogMessages, HelperLib;

const
  IniFileName = 'ZServer.ini';
  LISTMAXLINES = 1000;

type
  TClientData = record
    FUse: Boolean;
    FForm: TCliForm;
    FIPAddress: string;
    FPort: string;
    FConnectTime: TDateTime;
    FDisconnectTime: TDateTime;
    FX: Integer;
    FY: Integer;
    FWidth: Integer;
    FHeight: Integer;
  end;

  TClientList = array[1..32] of TClientData;

  TServerForm = class(TForm)
    ClientListBox: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    Contents1: TMenuItem;
    N1: TMenuItem;
    About1: TMenuItem;
    SendButton: TButton;
    Button2: TButton;
    Timer1: TTimer;
    Windows1: TMenuItem;
    ScoreandStatistics1: TMenuItem;
    Multipliers1: TMenuItem;
    checkMonitorChatOnly: TCheckBox;
    SendEdit: TEdit;
    SaveDialog: TSaveDialog;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    N2: TMenuItem;
    Open1: TMenuItem;
    MergeFile1: TMenuItem;
    OpenDialog: TOpenDialog;
    Connections1: TMenuItem;
    menuTakeChatLog: TMenuItem;
    CurrentFrequencies1: TMenuItem;
    DeleteDupes1: TMenuItem;
    N3: TMenuItem;
    menuTakeCommandLog: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    buttonMergeLock: TSpeedButton;
    SrvSslContext: TSslContext;
    SrvSocket: TSslWSocketServer;
    N4: TMenuItem;
    menuOptions: TMenuItem;
    PopupMenu1: TPopupMenu;
    menuSaveToFile: TMenuItem;
    //procedure CreateParams(var Params: TCreateParams); override;
    procedure FormShow(Sender: TObject);
    procedure SrvSocketClientConnect(Sender: TObject; Client: TWSocketClient; Error: Word);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Exit1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ScoreandStatistics1Click(Sender: TObject);
    procedure Multipliers1Click(Sender: TObject);
    procedure checkMonitorChatOnlyClick(Sender: TObject);
    procedure SendEditKeyPress(Sender: TObject; var Key: Char);
    procedure Save1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SaveAs1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Connections1Click(Sender: TObject);
    procedure MergeFile1Click(Sender: TObject);
    procedure menuTakeChatLogClick(Sender: TObject);
    procedure CurrentFrequencies1Click(Sender: TObject);
    procedure DeleteDupes1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Windows1Click(Sender: TObject);
    procedure SrvSocketError(Sender: TObject);
    procedure SrvSocketException(Sender: TObject; SocExcept: ESocketException);
    procedure SrvSocketSocksError(Sender: TObject; Error: Integer; Msg: string);
    procedure menuTakeCommandLogClick(Sender: TObject);
    procedure buttonMergeLockClick(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure menuOptionsClick(Sender: TObject);
    procedure menuSaveToFileClick(Sender: TObject);
  private
    { Declarations privates }
    FInitialized  : Boolean;
    FTakeChatLog : Boolean;
    FTakeCommandLog : Boolean;
    FLongDateTime: Boolean;
    FChatLogFileName: string;

    FFreqList: TFreqList;
    FConnections: TConnections;

    // 接続中のクライアントリスト（最大３２台）
    FClientList: TClientList;

    FStats: TBasicStats;
    FMultiForm: TBasicMultiForm;

    FCurrentFileName: string;
    FLastPath: string;
    FLastTickCount: DWORD;
    FUseAutoSave: Boolean;

    FPortNumber: string;
    FSecure: Boolean;
    FLoginUser: string;
    FLoginPass: string;

    procedure OnClientClosed(var msg: TMessage); message WM_USER_CLIENT_CLOSED;
    procedure OnFreqData(var msg: TMessage); message WM_ZCMD_FREQDATA;
    procedure OnPutQSO(var msg: TMessage); message WM_ZCMD_PUTQSO;
    procedure OnPutLog(var msg: TMessage); message WM_ZCMD_PUTLOG;
    procedure OnExDelQSO(var msg: TMessage); message WM_ZCMD_EXDELQSO;
    procedure OnDelQSO(var msg: TMessage); message WM_ZCMD_DELQSO;
    procedure OnEditQsoTo(var msg: TMessage); message WM_ZCMD_EDITQSOTO;
    procedure OnRenew(var msg: TMessage); message WM_ZCMD_RENEW;
    procedure OnInsQso(var msg: TMessage); message WM_ZCMD_INSQSO;
    procedure OnAddConsole(var msg: TMessage); message WM_ZCMD_ADDCONSOLE;
    procedure OnSendAll(var msg: TMessage); message WM_ZCMD_SENDALL;
    procedure OnUpdateDisplay(var msg: TMessage); message WM_ZCMD_UPDATE_DISPLAY;
    procedure StartServer();
    procedure StopServer();
    procedure LoadSettings();
    procedure SaveSettings();
    function GetMasterLog(): TLog;

    procedure RestoreWindowsPos();
    procedure RestoreWindowStates;
    procedure RecordWindowStates;

    procedure AddToChatLog(str : string);
    procedure IdleEvent(Sender: TObject; var Done: Boolean);
    procedure SendAll(str : string);
    procedure ApplyTakeCommandLog();
    procedure SetCaption();
    function GetClientSlot(ip: string): Integer;
    procedure SetClientList(no: Integer; form: TCliForm; ip: string; port: string);
    procedure UnsetClientList(no: Integer);
  public
    ChatOnly : boolean;
    procedure SaveCliFormPos(no: Integer; x, y, w, h: Integer);
    procedure RestoreCliFormPos(no: Integer; Form: TCliForm);
    function GetClientFormByBand(B: TBand): TCliForm;

    procedure AddConsole(S : string); // adds string to clientlistbox
    function GetConsole(): TStringList;
    function GetWhoList(): TStringList;
    function GetQSObyID(id : integer) : TQSO;
    function IsBandUsed(b: TBand): Boolean;
    function IsBandUsed2(from: Integer; b: TBand): Boolean;
    procedure SendAllButFrom(str : string; NotThisCli : integer);
    procedure MergeFile(FileName : string; BandSet : TBandSet);
    procedure RecalcAll();

    property LoginUser: string read FLoginUser;
    property LoginPass: string read FLoginPass;

    property ClientList: TClientList read FClientList;
    property MasterLog: TLog read GetMasterLog;
  end;

var
  ServerForm: TServerForm;

  MasterLogLock: TCriticalSection;

implementation

uses
  UAbout, UChooseContest, UMergeBand,
  UALLJAMultiForm, UALLJAStats, USixDownStats,
  UACAGMultiForm, UFDMultiForm, UFDStats, UCQWWStats, UWWMultiForm, USimpleStats;

{$R *.DFM}

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

{ procedure TServerForm.CreateParams(var Params: TCreateParams);
  begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
  end; }

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TServerForm.FormCreate(Sender: TObject);
var
   SL: TSTringList;
   i: Integer;
begin
   for i := Low(FClientList) to High(FClientList) do begin
      FClientList[i].FUse := False;
      FClientList[i].FForm := nil;
      FClientList[i].FIPAddress := '';
      FClientList[i].FPort := '';
      FClientList[i].FConnectTime := 0;
      FClientList[i].FDisconnectTime := 0;
      FClientList[i].FX := -1;
      FClientList[i].FY := -1;
      FClientList[i].FWidth := 0;
      FClientList[i].FHeight := 0;
   end;

   FCurrentFileName := '';
   FLastPath := '';
   FLongDateTime := False;
   FLastTickCount := 0;
   LoadSettings();

   // SSLコンテキストパラメータセット
   SL := LoadFromResourceName(SysInit.HInstance, 'SERVER_CRT');
   SrvSslContext.SslCertLines := SL;
   SL.Free();

   SL := LoadFromResourceName(SysInit.HInstance, 'SERVER_KEY');
   SrvSslContext.SslPrivKeyLines := SL;
   SL.Free();

   SetCaption();

   FFreqList := TFreqList.Create(Self);
   FConnections := TConnections.Create(Self);
   FConnections.LongDateTime := FLongDateTime;

   RestoreWindowsPos();
   checkMonitorChatOnly.Checked := ChatOnly;

   Application.OnIdle := IdleEvent;

   FChatLogFileName := StringReplace(Application.ExeName, '.exe', '_chat_' + FormatDateTime('yyyymmdd', Now) + '.txt', [rfReplaceAll]);
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TServerForm.FormShow(Sender: TObject);
var
   F: TChooseContest;
begin
   if FInitialized then begin
      Exit;
   end;

   {$IFDEF DEBUG}
   buttonMergeLock.Visible := True;
   {$ENDIF}

   FInitialized := True;

   FTakeChatLog := menuTakeChatLog.Checked;

   F := TChooseContest.Create(Self);
   try
      F.ShowModal();
      case F.ContestNumber of
         // ALL JA
         0: begin
            FStats := TALLJAStats.Create(Self);
            FMultiForm := TALLJAMultiForm.Create(Self);
         end;

         // 6m&DOWN
         1: begin
            FStats := TSixDownStats.Create(Self);
            FMultiForm := TFDMultiForm.Create(Self);
            TFDMultiForm(FMultiForm).Init6D;
         end;

         // Field Day
         2: begin
            FStats := TFDStats.Create(Self);
            FMultiForm := TFDMultiForm.Create(Self);
         end;

         // ACAG
         3: begin
            FStats := TALLJAStats.Create(Self);
            FMultiForm := TACAGMultiForm.Create(Self);
            TALLJAStats(FStats).InitACAG;
         end;

         // CQWW
         4: begin
            FStats := TCQWWStats.Create(Self);
            FMultiForm := TWWMultiForm.Create(Self);
         end;

         // Simple
         5: begin
            FStats := TSimpleStats.Create(Self);
            FMultiForm := nil;
         end;
      end;

      RestoreWindowStates;

      if ((FSecure = True) and ((FLoginUser = '') or (FLoginPass = ''))) then begin
         MessageBox(Handle, PChar('セキュアモードの場合は、ログイン名とパスワードの設定が必要です。'), PChar(Application.Title), MB_OK or MB_ICONEXCLAMATION);
         Exit;
      end;

      StartServer();
   finally
      F.Release();
   end;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TServerForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
   R: Word;
begin
   if FStats.Saved = False then begin
      R := MessageDlg('Save changes to ' + FCurrentFileName + ' ?', mtConfirmation, [mbYes, mbNo, mbCancel], 0); { HELP context 0 }
      case R of
         mrYes:
            Save1Click(self);
         mrCancel:
            CanClose := False;
      end;
   end;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TServerForm.File1Click(Sender: TObject);
begin
   Save1.Enabled := not FStats.Saved;
   menuTakeChatLog.Checked := FTakeChatLog;
   menuTakeCommandLog.Checked := FTakeCommandLog;
end;

procedure TServerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   SrvSocket.Close;
   SaveSettings();
   if FInitialized then begin
      RecordWindowStates;
      dmZlogGlobal.WriteMainFormState(Left, top, Width, Height, False, False);
   end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

procedure TServerForm.FormDestroy(Sender: TObject);
var
   i: Integer;
begin
   FFreqList.Release();
   FConnections.Release();

   for i := Low(FClientList) to High(FClientList) do begin
      UnsetClientList(i);
   end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

function TServerForm.GetQSObyID(id: Integer): TQSO;
var
   j: Integer;
begin
   j := id div 100;
   Result := FStats.MasterLog.ObjectOf(j);
end;

procedure TServerForm.AddConsole(S: string);
begin
   ClientListBox.Items.BeginUpdate();
   ClientListBox.Items.Add(S);
   ClientListBox.Items.EndUpdate();
   ClientListBox.ShowLast();
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TServerForm.IdleEvent(Sender: TObject; var Done: Boolean);
var
   dwTick: DWORD;
begin
   if ClientListBox.Items.Count > LISTMAXLINES then begin
      ClientListBox.Items.BeginUpdate();
      while ClientListBox.Items.Count > LISTMAXLINES do begin
         ClientListBox.Items.Delete(0);
      end;
      ClientListBox.Items.EndUpdate();
      ClientListBox.ShowLast();
   end;

   // 前回セーブ時から未セーブありかつ１分以上経過で自動セーブ
   if (FStats <> nil) and (MasterLog.Saved = False) and (FUseAutoSave = True) then begin
      dwTick := GetTickCount();
      if (dwTick - FLastTickCount) > (60 * 1000) then begin
         if FCurrentFileName = '' then begin
            FCurrentFileName := CreateTempLogFileName();
         end;

         FStats.SaveLogs(FCurrentFileName);
         FLastPath := ExtractFilePath(FCurrentFileName);
         AddConsole('ファイル ' + ExtractFileName(FCurrentFileName) + ' に自動保存しました');

         FLastTickCount := dwTick;
      end;
   end;

end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TServerForm.StartServer();
begin
   SrvSocket.Close;

   // ソケットにSSLコンテキストを割り当て
   if FSecure = True then begin
      SrvSocket.SslEnable := True;
      SrvSocket.SslContext := SrvSslContext;
   end
   else begin
      SrvSocket.SslEnable := False;
      SrvSocket.SslContext := nil;
   end;

   SrvSocket.Addr := '0.0.0.0';
   SrvSocket.Port := FPortNumber;
   SrvSocket.Proto := 'tcp';
   SrvSocket.Listen;
end;

procedure TServerForm.StopServer();
begin
   SrvSocket.Close;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TServerForm.SrvSocketError(Sender: TObject);
var
   S: string;
   t: string;
begin
   t := FormatDateTime('hh:nn', Now);
   S := t + ' SrvSocketError';
   AddToChatLog(S);
end;

procedure TServerForm.SrvSocketException(Sender: TObject; SocExcept: ESocketException);
var
   S: string;
   t: string;
begin
   t := FormatDateTime('hh:nn', Now);
   S := t + ' [' + SocExcept.IPStr + '] ' + IntToStr(SocExcept.ErrorCode) + ':' + SocExcept.ErrorMessage;
   AddToChatLog(S);
end;

procedure TServerForm.SrvSocketClientConnect(Sender: TObject; Client: TWSocketClient; Error: Word);
var
   Form: TCliForm;
   i: Integer;
   no: Integer;
   ip: string;
   port: string;
begin
   ip := Client.PeerAddr;
   port := Client.PeerPort;
   no := GetClientSlot(ip);
   if no = 0 then begin
      AddConsole('接続数MAXなので受け入れできません。');
      Client.Close();
      Exit;
   end;

   Form := TCliForm.Create(Self);
   Form.Caption := 'Client ' + IntToStr(no);
   Form.ClientNumber := no;
   Form.Socket := Client;
   Form.Secure := FSecure;
   Form.LoginUser := FLoginUser;
   Form.LoginPass := FLoginPass;
   Form.TakeLog := FTakeCommandLog;

   RestoreCliFormPos(no, Form);

   Form.Show;

   SetClientList(no, Form, ip, port);
end;

procedure TServerForm.SrvSocketSocksError(Sender: TObject; Error: Integer; Msg: string);
var
   S: string;
   t: string;
begin
   t := FormatDateTime('hh:nn', Now);
   S := t + IntToStr(Error) + ':' + Msg;
   AddToChatLog(S);
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TServerForm.OnClientClosed(var msg: TMessage);
var
   Form: TCliForm;
   i: Integer;
begin
   Form := TCliForm(msg.lParam);

   // クライアントリストから消去
   for i := Low(FClientList) to High(FClientList) do begin
      if LongInt(FClientList[i].FForm) = LongInt(Form) then begin
         FFreqList.Clear(FClientList[i].FForm.CurrentBand);
         UnsetClientList(i);
         Break;
      end;
   end;

   // コネクションリストを更新
   FConnections.UpdateDisplay();
end;

// ****************************************************************************

procedure TServerForm.OnFreqData(var msg: TMessage);
var
   S: string;
   param_atom: ATOM;
   szBuffer: array[0..1023] of Char;
begin
   ZeroMemory(@szBuffer, SizeOf(szBuffer));
   param_atom := msg.LParam;
   if GetAtomName(param_atom, @szBuffer, SizeOf(szBuffer)) = 0 then begin
      Exit;
   end;

   S := StrPas(szBuffer);

   FFreqList.ProcessFreqData(S);

   DeleteAtom(param_atom);
end;

// ****************************************************************************

procedure TServerForm.OnPutQSO(var msg: TMessage);
var
   from: Integer;
   aQSO: TQSO;
   t: string;
begin
   t := FormatDateTime('hh:nn', Now);
   from := msg.WParam;
   aQSO := TQSO(msg.LParam);

   if (FStats.MasterLog.CheckQSOID(aQSO.Reserve3) = True) then begin
      AddConsole(t + ' duplicate QSO detected! ==>' + aQSO.QSOinText);
      Exit;
   end;

   FStats.Add(aQSO);
   if Assigned(FMultiForm) then begin
      FMultiForm.Add(aQSO);
   end;
end;

// ****************************************************************************

procedure TServerForm.OnPutLog(var msg: TMessage);
var
   from: Integer;
   aQSO: TQSO;
   t: string;
begin
   t := FormatDateTime('hh:nn', Now);
   from := msg.WParam;
   aQSO := TQSO(msg.LParam);

   if (FStats.MasterLog.CheckQSOID(aQSO.Reserve3) = True) then begin
      AddConsole(t + ' duplicate QSO detected! ==>' + aQSO.QSOinText);
      Exit;
   end;

   FStats.AddNoUpdate(aQSO);
   if Assigned(FMultiForm) then begin
      FMultiForm.Add(aQSO);
   end;
end;

// ****************************************************************************

procedure TServerForm.OnExDelQSO(var msg: TMessage);
var
   from: Integer;
   aQSO: TQSO;
begin
   from := msg.WParam;
   aQSO := TQSO(msg.LParam);

   FStats.Delete(aQSO, False);
   aQSO.Free;
end;

// ****************************************************************************

procedure TServerForm.OnDelQSO(var msg: TMessage);
var
   from: Integer;
   aQSO: TQSO;
begin
   from := msg.WParam;
   aQSO := TQSO(msg.LParam);

   FStats.Delete(aQSO);
   RecalcAll;
   FStats.UpdateStats;
   aQSO.Free;
end;

// ****************************************************************************

procedure TServerForm.OnEditQsoTo(var msg: TMessage);
var
   from: Integer;
   aQSO: TQSO;
begin
   from := msg.WParam;
   aQSO := TQSO(msg.LParam);

   FStats.MasterLog.AddQue(aQSO);
   FStats.MasterLog.ProcessQue;

   RecalcAll;

   FStats.UpdateStats;
   aQSO.Free;
end;

// ****************************************************************************

procedure TServerForm.OnRenew(var msg: TMessage);
var
   from: Integer;
begin
   from := msg.WParam;

   RecalcAll;
   FStats.UpdateStats;
   FStats.Refresh();
end;

// ****************************************************************************

procedure TServerForm.OnInsQso(var msg: TMessage);
var
   from: Integer;
   aQSO: TQSO;
begin
   from := msg.WParam;
   aQSO := TQSO(msg.LParam);

   FStats.MasterLog.AddQue(aQSO);
   FStats.MasterLog.ProcessQue;
   RecalcAll;
   FStats.UpdateStats;
   aQSO.Free;
end;

// ****************************************************************************

procedure TServerForm.OnAddConsole(var msg: TMessage);
var
   from: Integer;
   S: string;
   szBuffer: array[0..1023] of Char;
   param_atom: ATOM;
begin
   from := msg.WParam;
   param_atom := msg.LParamLo;
   if GetAtomName(param_atom, @szBuffer, SizeOf(szBuffer)) = 0 then begin
      Exit;
   end;

   S := StrPas(szBuffer);

   if msg.LParamHi = 0 then begin
      AddConsole(S);
   end;

   if msg.LParamHi = 1 then begin
      AddConsole(S);
      AddToChatLog(S);
   end;

   DeleteAtom(param_atom);
end;

// ****************************************************************************

procedure TServerForm.OnSendAll(var msg: TMessage);
var
   from: Integer;
   S: string;
   szBuffer: array[0..1023] of Char;
   param_atom: ATOM;
begin
   from := msg.WParam;
   param_atom := msg.LParamLo;
   if GetAtomName(param_atom, @szBuffer, SizeOf(szBuffer)) = 0 then begin
      Exit;
   end;

   S := StrPas(szBuffer);

   SendAll(ZLinkHeader + ' PUTMESSAGE ' + S);

   AddToChatLog(S);

   DeleteAtom(param_atom);
end;

// ****************************************************************************

procedure TServerForm.OnUpdateDisplay(var msg: TMessage);
begin
   if Assigned(FConnections) then begin
      FConnections.UpdateDisplay;
   end;
end;

// ****************************************************************************

function TServerForm.GetConsole(): TStringList;
var
   i: Integer;
   SL: TStringList;
   S: string;
begin
   SL := TStringList.Create();
   try
      for i := 0 to ClientListBox.Items.Count - 1 do begin
         S := ZLinkHeader + ' PUTMESSAGE ';
         S := ClientListBox.Items[i];
         SL.Add(S);
      end;
   finally
      Result := SL;
   end;
end;

// ****************************************************************************

function TServerForm.GetWhoList(): TStringList;
var
   SL: TStringList;
   S: string;
   B: TBand;
   F: TCliForm;
begin
   SL := TStringList.Create();
   try
      for B := b19 to HiBand do begin
         F := GetClientFormByBand(B);
         if F <> nil then begin
            S := ZLinkHeader + ' PUTMESSAGE ';
            S := S + FillRight(BandString[F.CurrentBand], 9) + F.CurrentOperator;
            SL.Add(S);
         end;
      end;
   finally
      Result := SL;
   end;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TServerForm.SendAll(str: string);
var
   i: Integer;
begin
   for i := Low(FClientList) to High(FClientList) do begin
      if FClientList[i].FUse = True then begin
         FClientList[i].FForm.SendStr(str);
      end;
   end;
end;

procedure TServerForm.SendAllButFrom(str: string; NotThisCli: Integer);
var
   i: Integer;
begin
   for i := Low(FClientList) to High(FClientList) do begin
      if FClientList[i].FUse = True then begin
         if FClientList[i].FForm.ClientNumber = NotThisCli then begin
            Continue;
         end;
         FClientList[i].FForm.SendStr(str);
      end;
   end;
end;

procedure TServerForm.Exit1Click(Sender: TObject);
begin
   Close;
end;

procedure TServerForm.About1Click(Sender: TObject);
var
   F: TAboutBox;
begin
   F := TAboutBox.Create(Self);
   try
      F.ShowModal();
   finally
      F.Release();
   end;
end;

procedure TServerForm.AddToChatLog(str: string);
var
   F: TextFile;
begin
   if FTakeChatLog = False then begin
      Exit;
   end;

   str := StringReplace(str, #13, '', [rfReplaceAll]);
   str := StringReplace(str, #10, '', [rfReplaceAll]);

   AssignFile(F, FChatLogFileName);
   if FileExists(FChatLogFileName) then begin
      Append(F);
   end
   else begin
      Rewrite(F);
   end;

   WriteLn(F, str);

   CloseFile(F);
end;

procedure TServerForm.SendButtonClick(Sender: TObject);
var
   t, S: string;
begin
   t := FormatDateTime('hh:nn', Now);
   S := t + '  ZServer> ' + SendEdit.Text;

   // SendALL(ZLinkHeader + ' PUTMESSAGE '+'ZServer> '+SendEdit.Text + LBCODE);
   SendAll(ZLinkHeader + ' PUTMESSAGE ' + S + LBCODE);

   AddConsole(S);

   AddToChatLog(S);

   SendEdit.Clear;
   ActiveControl := SendEdit;
end;

procedure TServerForm.Button2Click(Sender: TObject);
begin
   ClientListBox.Clear;
end;

procedure TServerForm.buttonMergeLockClick(Sender: TObject);
begin
   if TSpeedButton(Sender).Down = True then begin
      MasterLogLock.Enter();
   end
   else begin
      MasterLogLock.Leave();
   end;
end;

procedure TServerForm.Timer1Timer(Sender: TObject);
begin
   Timer1.Enabled := False;
   try
      if ClientListBox.Items.Count > LISTMAXLINES then begin
         ClientListBox.Items.BeginUpdate();
         while ClientListBox.Items.Count > LISTMAXLINES do begin
            ClientListBox.Items.Delete(0);
         end;
         ClientListBox.Items.EndUpdate();
         ClientListBox.ShowLast();
      end;
   finally
      Timer1.Enabled := True;
   end;
end;

procedure TServerForm.Windows1Click(Sender: TObject);
begin
   Multipliers1.Enabled := Assigned(FMultiForm);
end;

procedure TServerForm.ScoreandStatistics1Click(Sender: TObject);
begin
   FStats.Show;
end;

procedure TServerForm.Multipliers1Click(Sender: TObject);
begin
   if Assigned(FMultiForm) then begin
      FMultiForm.Show;
   end;
end;

procedure TServerForm.checkMonitorChatOnlyClick(Sender: TObject);
begin
   ChatOnly := checkMonitorChatOnly.Checked;
end;

procedure TServerForm.SendEditKeyPress(Sender: TObject; var Key: Char);
begin
   case Key of
      Chr($0D): begin
         SendButtonClick(self);
         Key := #0;
      end;
   end;
end;

procedure TServerForm.Save1Click(Sender: TObject);
begin
   if FStats.Saved = False then begin
      // ファイル名が不明の場合は入力させる
      if FCurrentFileName = '' then begin
         SaveDialog.InitialDir := FLastPath;
         if SaveDialog.Execute then begin
            FCurrentFileName := SaveDialog.FileName;
         end;
      end;

      // ファイルに保存
      FStats.SaveLogs(FCurrentFileName);
      FLastPath := ExtractFilePath(FCurrentFileName);
      AddConsole('ファイル ' + FCurrentFileName + ' に保存しました');
   end;
end;

procedure TServerForm.SaveAs1Click(Sender: TObject);
begin
   SaveDialog.InitialDir := FLastPath;
   If SaveDialog.Execute then begin
      FCurrentFileName := SaveDialog.FileName;
      FStats.SaveLogs(FCurrentFileName);
      FLastPath := ExtractFilePath(FCurrentFileName);
      AddConsole('新しいファイル ' + FCurrentFileName + ' で保存しました');
   end;
end;

procedure TServerForm.Open1Click(Sender: TObject);
begin
   OpenDialog.InitialDir := FLastPath;
   if OpenDialog.Execute then begin
      if MessageBox(Handle, PChar('すべてのデータがクリアされ、指定のファイルからロードされます。よろしいですか？'), PChar(Application.Title), MB_YESNO or MB_DEFBUTTON2 or MB_ICONEXCLAMATION) = IDNO then begin
         Exit;
      end;

      FCurrentFileName := OpenDialog.FileName;

      if FileExists(FCurrentFileName) = False then begin
         MessageBox(Handle, PChar(FCurrentFileName + 'がありません。'), PChar(Application.Title), MB_OK or MB_ICONEXCLAMATION);
         Exit;
      end;

      AddConsole(FCurrentFileName + ' を開いています...');
      FStats.MasterLog.Clear2();
      FStats.MasterLog.LoadFromFile(FCurrentFileName);
      RecalcAll();
      FStats.UpdateStats();
      AddConsole(FCurrentFileName + ' ロード終了');

      FLastPath := ExtractFilePath(FCurrentFileName);

      // 拡張子がZLOの場合は付け替える
      if ExtractFileExt(FCurrentFileName) = '.ZLO' then begin
         FCurrentFileName := ChangeFileExt(FCurrentFileName, '.ZLOX');
      end;
   end;
end;

procedure TServerForm.Connections1Click(Sender: TObject);
begin
   FConnections.Show;
end;

procedure TServerForm.MergeFile1Click(Sender: TObject);
var
   F: TMergeBand;
begin
   F := TMergeBand.Create(Self);
   try
      OpenDialog.InitialDir := FLastPath;
      if OpenDialog.Execute then begin
         F.FileName := OpenDialog.FileName;
         F.ShowModal;

         FLastPath := ExtractFilePath(FCurrentFileName);
      end;
   finally
      F.Release();
   end;
end;

// チャットログを記録
procedure TServerForm.menuTakeChatLogClick(Sender: TObject);
begin
   FTakeChatLog := menuTakeChatLog.Checked;
end;

// コマンドログを記録
procedure TServerForm.menuTakeCommandLogClick(Sender: TObject);
begin
   FTakeCommandLog := menuTakeCommandLog.Checked;
   ApplyTakeCommandLog();
end;

// 設定
procedure TServerForm.menuOptionsClick(Sender: TObject);
var
   f: TformOptions;
begin
   StopServer();
   f := TformOptions.Create(Self);
   try

      f.Port := FPortNumber;
      f.UseSecure := FSecure;
      f.LoginID := FLoginUser;
      f.LoginPassword := FLoginPass;
      f.TakeChatLog := FTakeChatLog;
      f.TakeCommandLog := FTakeCommandLog;
      f.LongDateTime := FLongDateTime;
      f.UseAutoSave := FUseAutoSave;

      if f.ShowModal() <> mrOK then begin
         Exit;
      end;

      FPortNumber := f.Port;
      FSecure := f.UseSecure;
      FLoginUser := f.LoginID;
      FLoginPass := f.LoginPassword;
      FTakeChatLog := f.TakeChatLog;
      FTakeCommandLog := f.TakeCommandLog;
      FLongDateTime := f.LongDateTime;
      FUseAutoSave := f.UseAutoSave;

      ApplyTakeCommandLog();
      FConnections.LongDateTime := FLongDateTime;

      SaveSettings();
      SetCaption();
   finally
      StartServer();
      f.Release();
   end;
end;

procedure TServerForm.menuSaveToFileClick(Sender: TObject);
var
   fname: string;
begin
   fname := ChangeFileExt(Application.ExeName, '') + '_sf.txt';
   ClientListBox.Items.SaveToFile(fname);
end;

procedure TServerForm.ApplyTakeCommandLog();
var
   i: Integer;
begin
   for i := Low(FClientList) to High(FClientList) do begin
      if FClientList[i].FUse = True then begin
         FClientList[i].FForm.TakeLog := FTakeCommandLog;
      end;
   end;
end;

procedure TServerForm.CurrentFrequencies1Click(Sender: TObject);
begin
   FFreqList.Show;
end;

procedure TServerForm.DeleteDupes1Click(Sender: TObject);
begin
   FStats.MasterLog.RemoveDupes;
   RecalcAll;
   FStats.UpdateStats;
end;

procedure TServerForm.LoadSettings();
var
   IniFile: TIniFile;
   FileName: string;
   i: Integer;
   section: string;
begin
   FileName := ChangeFileExt(Application.ExeName, '.ini');
   IniFile := TIniFile.Create(FileName);
   try
      Top := IniFile.ReadInteger('Window', 'Top', Top);
      Left := IniFile.ReadInteger('Window', 'Left', Left);
      Width := IniFile.ReadInteger('Window', 'Width', Width);
      Height := IniFile.ReadInteger('Window', 'Height', Height);
      ChatOnly := IniFile.ReadBool('Options', 'ChatOnly', True);
      FLastPath := IniFile.ReadString('Path', 'Last', '');
      if FLastPath = '' then begin
         FLastPath := ExtractFilePath(Application.ExeName);
      end;
      FLastPath := IncludeTrailingPathDelimiter(FLastPath);

      FPortNumber := IniFile.ReadString('Server', 'Port', 'telnet');
      FSecure := IniFile.ReadBool('Server', 'Secure', False);
      FLoginUser := IniFile.ReadString('Login', 'User', 'zloguser');
      FLoginPass := IniFile.ReadString('Login', 'Password', '');
      FTakeChatLog := IniFile.ReadBool('Options', 'TakeChatLog', True);
      FTakeCommandLog := IniFile.ReadBool('Options', 'TakeCommandLog', False);
      FLongDateTime := IniFile.ReadBool('Options', 'LongDateTime', False);
      FUseAutoSave := IniFile.ReadBool('Options', 'UseAutoSave', False);

      ApplyTakeCommandLog();

      for i := Low(FClientList) to High(FClientList) do begin
         section := 'CliForm#' + IntToStr(i);
         FClientList[i].FY := IniFile.ReadInteger(section, 'Top', -1);
         FClientList[i].FX := IniFile.ReadInteger(section, 'Left', -1);
         FClientList[i].FWidth := IniFile.ReadInteger(section, 'Width', 0);
         FClientList[i].FHeight := IniFile.ReadInteger(section, 'Heigth', 0);
      end;
   finally
      IniFile.Free;
   end;
end;

procedure TServerForm.SaveSettings();
var
   IniFile: TIniFile;
   FileName: string;
   i: Integer;
   section: string;
begin
   FileName := ChangeFileExt(Application.ExeName, '.ini');
   IniFile := TIniFile.Create(FileName);
   try
      IniFile.WriteInteger('Window', 'Top', Top);
      IniFile.WriteInteger('Window', 'Left', Left);
      IniFile.WriteInteger('Window', 'Width', Width);
      IniFile.WriteInteger('Window', 'Height', Height);
      IniFile.WriteBool('Options', 'ChatOnly', ChatOnly);
      IniFile.WriteString('Path', 'Last', FLastPath);

      IniFile.WriteString('Server', 'Port', FPortNumber);
      IniFile.WriteBool('Server', 'Secure', FSecure);
      IniFile.WriteString('Login', 'User', FLoginUser);
      IniFile.WriteString('Login', 'Password', FLoginPass);
      IniFile.WriteBool('Options', 'TakeChatLog', FTakeChatLog);
      IniFile.WriteBool('Options', 'TakeCommandLog', FTakeCommandLog);
      IniFile.WriteBool('Options', 'LongDateTime', FLongDateTime);
      IniFile.WriteBool('Options', 'UseAutoSave', FUseAutoSave);

      for i := Low(FClientList) to High(FClientList) do begin
         if (FClientList[i].FX = -1) and (FClientList[i].FY = -1) then begin
            Continue;
         end;
         section := 'CliForm#' + IntToStr(i);
         IniFile.WriteInteger(section, 'Top', FClientList[i].FY);
         IniFile.WriteInteger(section, 'Left', FClientList[i].FX);
         IniFile.WriteInteger(section, 'Width', FClientList[i].FWidth);
         IniFile.WriteInteger(section, 'Heigth', FClientList[i].FHeight);
      end;
   finally
      IniFile.Free;
   end;
end;

function TServerForm.IsBandUsed(b: TBand): Boolean;
begin
   Result := FStats.UsedBands[b];
end;

function TServerForm.IsBandUsed2(from: Integer; b: TBand): Boolean;
var
   i: Integer;
begin
   for i := Low(FClientList) to High(FClientList) do begin
      if FClientList[i].FUse = True then begin
         if (i <> from) and (FClientList[i].FForm.CurrentBand = b) then begin
            Result := True;
            Exit;
         end;
      end;
   end;

   Result := False;
end;

procedure TServerForm.MergeFile(FileName : string; BandSet : TBandSet);
begin
   FStats.MergeFile(FileName, BandSet);
end;

function TServerForm.GetMasterLog(): TLog;
begin
   Result := FStats.MasterLog;
end;

procedure TServerForm.RecalcAll();
var
   i : integer;
   aQSO : TQSO;
begin
   if Assigned(FMultiForm) then begin
      FMultiForm.Reset;
   end;

   FStats.MasterLog.SetDupeFlags;

   for i := 1 to FStats.MasterLog.TotalQSO do begin
      aQSO := FStats.MasterLog.QSOList[i];

      if FStats.MasterLog.CountHigherPoints = True then begin
         FStats.MasterLog.IsDupe(aQSO); // called to set log.differentmodepointer
      end;

      if Assigned(FMultiForm) then begin
         FMultiForm.Add(aQSO);
      end;
   end;
end;

procedure TServerForm.RestoreWindowsPos();
var
   X, Y, W, H: Integer;
   B, BB: Boolean;
   mon: TMonitor;
   pt: TPoint;
begin
   dmZlogGlobal.ReadMainFormState(X, Y, W, H, B, BB);

   if (W > 0) and (H > 0) then begin
      pt.X := X;
      pt.Y := Y;
      mon := Screen.MonitorFromPoint(pt, mdNearest);
      if X < mon.Left then begin
         X := mon.Left;
      end;
      if X > (mon.Left + mon.Width) then begin
         X := (mon.Left + mon.Width) - W;
      end;
      if Y < mon.Top then begin
         Y := mon.Top;
      end;
      if Y > (mon.Top + mon.Height) then begin
         Y := (mon.Top + mon.Height) - H;
      end;

      Position := poDesigned;
      Left := X;
      top := Y;
      Width := W;
      Height := H;
   end
   else begin
      Position := poScreenCenter;
   end;
end;

procedure TServerForm.RestoreWindowStates;
begin
   dmZLogGlobal.ReadWindowState(FConnections);
   dmZLogGlobal.ReadWindowState(FStats);
   if Assigned(FMultiForm) then begin
      dmZLogGlobal.ReadWindowState(FMultiForm);
   end;
   dmZLogGlobal.ReadWindowState(FFreqList);
end;

procedure TServerForm.RecordWindowStates;
begin
   dmZLogGlobal.WriteWindowState(FConnections);
   dmZLogGlobal.WriteWindowState(FStats);
   if Assigned(FMultiForm) then begin
      dmZLogGlobal.WriteWindowState(FMultiForm);
   end;
   dmZLogGlobal.WriteWindowState(FFreqList);
end;

procedure TServerForm.SetCaption();
var
   ver: TJclFileVersionInfo;
   S: string;
begin
   ver := TJclFileVersionInfo.Create(Self.Handle);
   S := Application.Title + ' Version ' + ver.FileVersion;
   ver.Free();

   if FSecure = True then begin
      S := S + ' [SECURE]';
   end;

   if FUseAutoSave = True then begin
      S := S + ' [自動保存]';
   end;

   Caption := S;
end;

function TServerForm.GetClientSlot(ip: string): Integer;
var
   i: Integer;
begin
   // 同じIPアドレスの空スロットを探す
   for i := Low(FClientList) to High(FClientList) do begin
      if (FClientList[i].FUse = False) and (FClientList[i].FIPAddress = ip) then begin
         Result := i;
         Exit;
      end;
   end;

   // 無ければIPアドレスの記録が無い空スロットを探す
   for i := Low(FClientList) to High(FClientList) do begin
      if (FClientList[i].FUse = False) and (FClientList[i].FIPAddress = '') then begin
         Result := i;
         Exit;
      end;
   end;

   // さらに無ければ空スロットを探す
   for i := Low(FClientList) to High(FClientList) do begin
      if (FClientList[i].FUse = False) then begin
         Result := i;
         Exit;
      end;
   end;

   Result := 0;
end;

procedure TServerForm.SetClientList(no: Integer; form: TCliForm; ip: string; port: string);
begin
   if FClientList[no].FUse = True then begin
      AddConsole('ClientNo:' + IntToStr(no) + 'は使用中です。');
      Exit;
   end;

   FClientList[no].FUse := True;
   FClientList[no].FForm := form;
   FClientList[no].FIPAddress := ip;
   FClientList[no].FPort := port;
   FClientList[no].FConnectTime := Now;
end;

procedure TServerForm.UnsetClientList(no: Integer);
begin
   if FClientList[no].FUse = True then begin
      FClientList[no].FUse := False;
      if FClientList[no].FForm <> nil then begin
         FClientList[no].FForm.Release();
      end;
      FClientList[no].FForm := nil;
//      FClientList[no].FIPAddress := '';
      FClientList[no].FPort := '';
      FClientList[no].FDisconnectTime := Now;
   end;
end;

function TServerForm.GetClientFormByBand(B: TBand): TCliForm;
var
   i: Integer;
begin
   for i := Low(FClientList) to High(FClientList) do begin
      if FClientList[i].FUse = True then begin
         if FClientList[i].FForm.CurrentBand = B then begin
            Result := FClientList[i].FForm;
            Exit;
         end;
      end;
   end;

   Result := nil;
end;

procedure TServerForm.SaveCliFormPos(no: Integer; x, y, w, h: Integer);
begin
   if (no < Low(FClientList)) or (no > High(FClientList)) then begin
      Exit;
   end;
   FClientList[no].FX := x;
   FClientList[no].FY := y;
   FClientList[no].FWidth := w;
   FClientList[no].FHeight := h;
end;

procedure TServerForm.RestoreCliFormPos(no: Integer; Form: TCliForm);
begin
   if (FClientList[no].FX = -1) and (FClientList[no].FY = -1) then begin
      Form.Position := poDefaultPosOnly;
   end
   else begin
      Form.Position := poDesigned;
      Form.Left := FClientList[no].FX;
      Form.Top := FClientList[no].FY;
      Form.Width := FClientList[no].FWidth;
      Form.Height := FClientList[no].FHeight;
   end;
end;

initialization
   MasterLogLock := TCriticalSection.Create();

finalization
   MasterLogLock.Free();

end.
