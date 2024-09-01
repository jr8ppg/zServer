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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IniFiles, Menus, ExtCtrls, System.UITypes,
  OverbyteIcsWndControl, OverbyteIcsWSocket, OverbyteIcsUtils, System.SyncObjs,
  System.Generics.Collections, System.Generics.Defaults, Winsock,
  UBasicStats, UBasicMultiForm, UCliForm, UFreqList, UConnections,
  UzLogGlobal, UzLogConst, UzLogQSO, UzLogMessages, JclFileUtils, Vcl.Buttons;

const
  IniFileName = 'ZServer.ini';
  MAXPACKETLEN = 2048;

type
  TServerForm = class(TForm)
    SrvSocket: TWSocket;
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
    CheckBox2: TCheckBox;
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
    //procedure CreateParams(var Params: TCreateParams); override;
    procedure FormShow(Sender: TObject);
    procedure SrvSocketSessionAvailable(Sender: TObject; Error: Word);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Exit1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ScoreandStatistics1Click(Sender: TObject);
    procedure Multipliers1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
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
  protected
    procedure ReadBroadcastPacket( var Message: TMessage ); message WM_USER_N1MM_BROADCAST;
  private
    { Declarations privates }
    FInitialized  : Boolean;
    FClientNumber : Integer;
    FTakeChatLog : Boolean;
    FChatLogFileName: string;

    FFreqList: TFreqList;
    FConnections: TConnections;

    FClientList: TList<TCliForm>;

    FStats: TBasicStats;
    FMultiForm: TBasicMultiForm;

    FCurrentFileName: string;
    FLastPath: string;

    FUdpServer: TSocket;
    FServerAddr: TSockAddrIn;
    FClientAddr: TSockAddrIn;

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
    procedure StartServer;
    procedure LoadSettings();
    procedure SaveSettings();
    function GetMasterLog(): TLog;

    procedure RestoreWindowsPos();
    procedure RestoreWindowStates;
    procedure RecordWindowStates;

    procedure AddToChatLog(str : string);
    procedure IdleEvent(Sender: TObject; var Done: Boolean);
    procedure SendAll(str : string);

    function GetXmlValue(xml: string; tag: string): string;
  public
    ChatOnly : boolean;

    procedure AddConsole(S : string); // adds string to clientlistbox
    function GetConsole(): TStringList;
    function GetWhoList(): TStringList;
    function GetQSObyID(id : integer) : TQSO;
    function IsBandUsed(b: TBand): Boolean;
    function IsBandUsed2(from: Integer; b: TBand): Boolean;
    procedure SendAllButFrom(str : string; NotThisCli : integer);
    procedure MergeFile(FileName : string; BandSet : TBandSet);
    procedure RecalcAll();

    property ClientList: TList<TCliForm> read FClientList;
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
   ver: TJclFileVersionInfo;
   optval: Integer;
   nResult: Integer;
begin
   FClientList := TList<TCliForm>.Create();

   ChatOnly := True;
   FClientNumber := 0;
   FCurrentFileName := '';
   FLastPath := '';

   ver := TJclFileVersionInfo.Create(Self.Handle);
   Caption := Application.Title + ' Version ' + ver.FileVersion;
   ver.Free();

   FFreqList := TFreqList.Create(Self);
   FConnections := TConnections.Create(Self);

   LoadSettings();

   RestoreWindowsPos();
   CheckBox2.Checked := ChatOnly;

   Application.OnIdle := IdleEvent;

   FChatLogFileName := StringReplace(Application.ExeName, '.exe', '_chat_' + FormatDateTime('yyyymmdd', Now) + '.txt', [rfReplaceAll]);

   // UDPソケットを準備
   FUdpServer := Socket(AF_INET, SOCK_DGRAM, 0);
   optval := 1;
   SetSockOpt(FUdpServer, SOL_SOCKET, SO_REUSEADDR, @optval, SizeOf(Integer));

   // アドレス情報
   FServerAddr.sin_family := AF_INET;
   FServerAddr.sin_addr.s_addr := htonl(INADDR_ANY);
   FServerAddr.sin_port := htons(12060);

   // ソケットにアドレス情報をバインド
   Bind(FUdpServer, FServerAddr, SizeOf(TSockAddrIn) );

   // 受信開始
   nResult := WSAAsyncSelect(FUdpServer, Handle, WM_USER_N1MM_BROADCAST, FD_READ);
   if nResult = SOCKET_ERROR then begin
      ClientListBox.Items.Add('WSAAsyncSelect() errorcode=' + IntToStr(WSAGetLastError()));
   end;
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

      StartServer;
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
end;

procedure TServerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   CloseSocket(FUdpServer);
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

   for i := 0 to FClientList.Count - 1 do begin
      FClientList[i].Release();
   end;
   FClientList.Free();
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
var
   _VisRows: Integer;
   _TopRow: Integer;
begin
   ClientListBox.Items.Add(S);
   _VisRows := ClientListBox.ClientHeight div ClientListBox.ItemHeight;
   _TopRow := ClientListBox.Items.Count - _VisRows + 1;
   if _TopRow > 0 then
      ClientListBox.TopIndex := _TopRow
   else
      ClientListBox.TopIndex := 0;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TServerForm.IdleEvent(Sender: TObject; var Done: Boolean);
begin
   while ClientListBox.Items.Count > 400 do begin
      ClientListBox.Items.Delete(0);
   end;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TServerForm.StartServer;
begin
   SrvSocket.Close;
   SrvSocket.Addr := '0.0.0.0';
   SrvSocket.Port := 'telnet';
   SrvSocket.Proto := 'tcp';
   SrvSocket.Listen;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TServerForm.SrvSocketError(Sender: TObject);
var
   S: string;
   t: string;
begin
   t := FormatDateTime('hh:nn', SysUtils.Now);
   S := t + ' SrvSocketError';
   AddToChatLog(S);
end;

procedure TServerForm.SrvSocketException(Sender: TObject; SocExcept: ESocketException);
var
   S: string;
   t: string;
begin
   t := FormatDateTime('hh:nn', SysUtils.Now);
   S := t + ' [' + SocExcept.IPStr + '] ' + IntToStr(SocExcept.ErrorCode) + ':' + SocExcept.ErrorMessage;
   AddToChatLog(S);
end;

procedure TServerForm.SrvSocketSessionAvailable(Sender: TObject; Error: Word);
var
   Form: TCliForm;
begin
   FClientNumber := FClientList.Count + 1;

   Form := TCliForm.Create(Self);
   Form.Caption := 'Client ' + IntToStr(FClientNumber);
   Form.ClientNumber := FClientNumber;
   Form.Socket := SrvSocket.Accept;
   Form.Show;
   FClientList.Add(Form);
end;

procedure TServerForm.SrvSocketSocksError(Sender: TObject; Error: Integer; Msg: string);
var
   S: string;
   t: string;
begin
   t := FormatDateTime('hh:nn', SysUtils.Now);
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
   for i := 0 to FClientList.Count - 1 do begin
      if LongInt(FClientList[i]) = LongInt(Form) then begin
         FClientList[i].Release();
         FClientList.Delete(i);
         Break;
      end;
   end;

   // リナンバー
   for i := 0 to FClientList.Count - 1 do begin
      FClientList[i].ClientNumber := i + 1;
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
   t := FormatDateTime('hh:nn', SysUtils.Now);
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
   t := FormatDateTime('hh:nn', SysUtils.Now);
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
   FConnections.UpdateDisplay;
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
   i: Integer;
   SL: TStringList;
   S: string;
   B: TBand;
begin
   SL := TStringList.Create();
   try
      for B := b19 to HiBand do begin
         for i := 0 to FClientList.Count - 1 do begin
            if FClientList[i].CurrentBand = B then begin
               S := ZLinkHeader + ' PUTMESSAGE ';
               S := S + FillRight(BandString[FClientList[i].CurrentBand], 9) + FClientList[i].CurrentOperator;
               SL.Add(S);
            end;
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
   for i := 0 to FClientList.Count - 1 do begin
      FClientList[i].SendStr(str);
   end;
end;

procedure TServerForm.SendAllButFrom(str: string; NotThisCli: Integer);
var
   i: Integer;
begin
   for i := 0 to FClientList.Count - 1 do begin
      if FClientList[i].ClientNumber = NotThisCli then begin
         Continue;
      end;
      FClientList[i].SendStr(str);
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
   t := FormatDateTime('hh:nn', SysUtils.Now);
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
      while ClientListBox.Items.Count > 400 do begin
         ClientListBox.Items.Delete(0);
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

procedure TServerForm.CheckBox2Click(Sender: TObject);
begin
   ChatOnly := CheckBox2.Checked;
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
   end;
end;

procedure TServerForm.SaveAs1Click(Sender: TObject);
begin
   SaveDialog.InitialDir := FLastPath;
   If SaveDialog.Execute then begin
      FCurrentFileName := SaveDialog.FileName;
      FStats.SaveLogs(FCurrentFileName);
      FLastPath := ExtractFilePath(FCurrentFileName);
   end;
end;

procedure TServerForm.Open1Click(Sender: TObject);
begin
   OpenDialog.InitialDir := FLastPath;
   if OpenDialog.Execute then begin
      if MessageDlg('This will clear all data and reload from new file.', mtWarning, [mbOK, mbCancel], 0) <> mrOK then begin
         Exit;
      end;

      FCurrentFileName := OpenDialog.FileName;

      if FileExists(FCurrentFileName) = False then begin
         exit;
      end;

      FStats.MasterLog.LoadFromFile(FCurrentFileName);
      RecalcAll();
      FStats.UpdateStats();

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

procedure TServerForm.menuTakeChatLogClick(Sender: TObject);
begin
   FTakeChatLog := menuTakeChatLog.Checked;
end;

procedure TServerForm.menuTakeCommandLogClick(Sender: TObject);
var
   i: Integer;
begin
   for i := 0 to FClientList.Count - 1 do begin
      FClientList[i].TakeLog := menuTakeCommandLog.Checked;
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
   finally
      IniFile.Free;
   end;
end;

procedure TServerForm.SaveSettings();
var
   IniFile: TIniFile;
   FileName: string;
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
   for i := 0 to FClientList.Count - 1 do begin
      if (i <> from) and (FClientList[i].CurrentBand = b) then begin
         Result := True;
         Exit;
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
   dmZlogGlobal.ReadWindowState(FConnections);
   dmZlogGlobal.ReadWindowState(FStats);
   if Assigned(FMultiForm) then begin
      dmZlogGlobal.ReadWindowState(FMultiForm);
   end;
   dmZlogGlobal.ReadWindowState(FFreqList);
end;

procedure TServerForm.RecordWindowStates;
begin
   dmZlogGlobal.WriteWindowState(FConnections);
   dmZlogGlobal.WriteWindowState(FStats);
   if Assigned(FMultiForm) then begin
      dmZlogGlobal.WriteWindowState(FMultiForm);
   end;
   dmZlogGlobal.WriteWindowState(FFreqList);
end;

procedure TServerForm.ReadBroadcastPacket(var Message: TMessage);
var
   nResult: Integer;
   nLen: Integer;
   Buffer: array [0..MAXPACKETLEN-1] of AnsiChar;
   info_utf8: UTF8String;
   qso: TQSO;
   S: string;
   defrst: Integer;
begin
   // エラーがあったか確認
   if WSAGetSelectError(Message.lParam) <> 0 then begin
      ClientListBox.Items.Add('WSAGetSelectError() errorcode=' + IntToStr(WSAGetLastError()));
      Exit;
   end;

   // 受信バッファクリア
   ZeroMemory(@Buffer, SizeOf(Buffer));

   // UDPから受信
   nLen := SizeOf(TSockAddrIn);
   nResult := RecvFrom(FUdpServer, Buffer, MAXPACKETLEN, 0, FClientAddr, nLen);
   if nResult = SOCKET_ERROR then begin
      ClientListBox.Items.Add('RecvFrom() errorcode=' + IntToStr(WSAGetLastError()));
      Exit;
   end;

   // 受信データの処理
   Buffer[nResult] := #0;

   // XMLをパースする
   info_utf8 := UTF8String(Buffer);

   // contactinfo
   //<?xml version="1.0" encoding="utf-8"?>
   //<contactinfo>
   //    <app>N1MM</app>
   //    <contestname>CWOPS</contestname>
   //    <contestnr>73</contestnr>
   //    <timestamp>2020-01-17 16 :43:38</timestamp>
   //    <mycall>W2XYZ</mycall>
   //    <band>3.5</band>
   //    <rxfreq>352519</rxfreq>
   //    <txfreq>352519</txfreq>
   //    <operator></operator>
   //    <mode>CW</mode>
   //    <call>WlAW</call>
   //    <countryprefix>K</countryprefix>
   //    <wpxprefix>Wl</wpxprefix>
   //    <stationprefix>W2XYZ</stationprefix>
   //    <continent>NA</continent>
   //    <snt>599</snt>
   //    <sntnr>5</sntnr>
   //    <rcv>599</rcv>
   //    <rcvnr>0</rcvnr>
   //    <gridsquare></gridsquare>
   //    <exchangel></exchangel>
   //    <section></section>
   //    <comment></comment>
   //    <qth></qth>
   //    <name></name>
   //    <power></power>
   //    <misctext></misctext>
   //    <zone>0</zone>
   //    <prec></prec>
   //    <ck>0</ck>
   //    <ismultiplierl>l</ismultiplierl>
   //    <ismultiplier2>0</ismultiplier2>
   //    <ismultiplier3>0</ismultiplier3>
   //    <points>l</points>
   //    <radionr>l</radionr>
   //    <run1run2>1<run1run2>
   //    <RoverLocation></RoverLocation>
   //    <RadioInterfaced>l</RadioInterfaced>
   //    <NetworkedCompNr>0</NetworkedCompNr>
   //    <IsOriginal>False</IsOriginal>
   //    <NetBiosName></NetBiosName>
   //    <IsRunQSO>0</IsRunQSO>
   //    <StationName>CONTEST-PC</StationName>
   //    <ID>f9ff ac4f cd3e 479c a86e 137d f133 8531</ID>
   //    <IsClaimedQso>1</IsClaimedQso>
   //</contactinfo>
   if Pos('<contactinfo>', info_utf8) > 0 then begin
      // ZLINKのQSOデータに変換して再送信
      qso := TQSO.Create();
      qso.Callsign := GetXmlValue(info_utf8, 'call');

      S := GetXmlValue(info_utf8, 'timestamp');   // <timestamp>2020-01-17 16:43:38</timestamp>
      qso.Time := StrToDate(S);

      qso.Band := StrToBandDef(GetXmlValue(info_utf8, 'band'), bUnknown);
      qso.Mode := StrToModeDef(GetXmlValue(info_utf8, 'mode'), mCW);

      if qso.Mode in [mCW, mRTTY] then begin
         defrst := 599;
      end
      else begin
         defrst := 59;
      end;

      qso.NrSent := GetXmlValue(info_utf8, 'sntnr');
      qso.NrRcvd := GetXmlValue(info_utf8, 'rcvnr');

      qso.RSTSent := StrToIntDef(GetXmlValue(info_utf8, 'snt'), defrst);
      qso.RSTRcvd := StrToIntDef(GetXmlValue(info_utf8, 'rcv'), defrst);

      qso.TX := StrToIntDef(GetXmlValue(info_utf8, 'run1run2'), 1);

      if GetXmlValue(info_utf8, 'IsRunQSO') = '1' then begin
         qso.CQ := True;
      end
      else begin
         qso.CQ := False;
      end;

      // QSOIDを発番する必要あり
      qso.Reserve3 := 99999;

      // N1MMのIDも保存したい
      qso.guid := GetXmlValue(info_utf8, 'ID');

      PostMessage(Handle, WM_ZCMD_PUTQSO, 0, LPARAM(qso));
   end;

end;

//
// XML風なので簡易にデータを取得する。
//
// 00000000011111111
// 12345678901234567
// <call>WlAW</call>
//
// in= 1 + 6 = 7
// out = 11 - 7 = 4
//
function TServerForm.GetXmlValue(xml: string; tag: string): string;
var
   tag_in: string;
   tag_out: string;
   Index_in: Integer;
   Index_out: Integer;
begin
   tag_in := '<' + tag + '>';
   tag_out := '</' + tag + '>';

   Index_in := Pos(tag_in, xml) + Length(tag_in);

   Index_out := Pos(tag_out, xml);

   Result := Copy(xml, Index_in, Index_out - Index_in + 1);
end;

initialization
   MasterLogLock := TCriticalSection.Create();

finalization
   MasterLogLock.Free();

end.
