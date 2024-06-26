unit UCliForm;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.WinSock,
  System.SysUtils, System.Classes, System.SyncObjs, System.StrUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Generics.Collections, Generics.Defaults, System.NetEncoding,
  OverbyteIcsWndControl, OverbyteIcsWSocket,
  UzLogGlobal, UzLogConst, UzLogQSO, UzLogMessages;

const LBCODE = #13#10;

type
  TClientThread = class;

  TCliForm = class(TForm)
    Panel1: TPanel;
    SendEdit: TEdit;
    SendButton: TButton;
    Panel2: TPanel;
    DisconnectButton: TButton;
    ListBox: TListBox;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SendButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure ClientThreadTerminate(Sender: TObject);
  private
    FInitialized : Boolean;
    FServerSocket: TSocket;
    FClientNumber: Integer;
    FCurrentBand : TBand;
    FCurrentOperator : string;
    FClientThread: TClientThread;
    FPcName: string;
    FTakeLog: Boolean;
    procedure AddChat(S: string);
    procedure SetCurrentBand(v: TBand);
    procedure SetCurrentOperator(v: string);
    procedure SetClientNumber(v: Integer);
    procedure SetTakeLog(v: Boolean);
    procedure SetCaption;
    procedure SetPcName(v: string);
  public
    procedure AddConsole(S : string);
    procedure SendStr(S: string);

    property CurrentBand: TBand read FCurrentBand write SetCurrentBand;
    property CurrentOperator: string read FCurrentOperator write SetCurrentOperator;
    property PcName: string read FPcName write SetPcName;
    property ClientNumber: Integer read FClientNumber write SetClientNumber;
    property Socket: TSocket read FServerSocket write FServerSocket;
    property TakeLog: Boolean read FTakeLog write SetTakeLog;
  end;

  TClientThread = class(TThread)
  private
    FClientSocket: TWSocket;
    FClientHSocket: TSocket;
    FClientForm: TCliForm;
    FClientNumber: Integer;
    FCurrentBand : TBand;

    FCommandLogFileName: string;
    FTakeLog: Boolean;

    FInMergeProc: Boolean;

    FCommandQue: TStringList;
    FFileData: TStringList;
    FInProcessing: Boolean;
    procedure ServerWSocketDataAvailable(Sender: TObject; Error: Word);
    procedure ServerWSocketSessionClosed(Sender: TObject; Error: Word);
    procedure CliSocketError(Sender: TObject);
    procedure CliSocketException(Sender: TObject; SocExcept: ESocketException);
    procedure CliSocketSocksError(Sender: TObject; Error: Integer; Msg: string);
    procedure ProcessCommand(S: string);

    procedure Process_Freq(S: string; from: Integer);
    procedure Process_GetConsole();
    procedure Process_SendRenew();
    procedure Process_Who();
    procedure Process_Operator(S: string; from: Integer);
    procedure Process_Band(S: string; from: Integer);
    procedure Process_PcName(S: string; from: Integer);
    procedure Process_PutMessage(S: string; from: Integer);
    procedure Process_SendLog();
    procedure Process_GetQsoIDs();
    procedure Process_GetLogQsoID(str: string);
    procedure Process_PutQso(S: string; from: Integer);
    procedure Process_PutLog(S: string; from: Integer);
    procedure Process_ExDelQso(S: string; from: Integer);
    procedure Process_DelQso(S: string; from: Integer);
    procedure Process_EditQsoTo(S: string; from: Integer);
    procedure Process_Renew(S: string; from: Integer);
    procedure Process_InsQso(S: string; from: Integer);
    procedure Process_GetFile(S: string; from: Integer);
    procedure Process_PutFile(S: string; from: Integer);

    procedure AddServerConsole(S: string);
    procedure AddToCommandLog(direction: string; str: string);

    function GetZLogFilesPath(): string;
  protected
    procedure Execute(); override;
  public
    constructor Create(ClientHSocket: TSocket; AClientForm: TCliForm; AClientNumber: Integer);
    destructor Destroy(); override;
    procedure Release();

    procedure SendStr(str: string);

    property ClientNumber: Integer read FClientNumber write FClientNumber;
    property TakeLog: Boolean read FTakeLog write FTakeLog;
  end;

implementation

uses
  UServerForm;

{$R *.DFM}

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);
   Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.FormCreate(Sender: TObject);
begin
   FInitialized := False;
   FClientThread := nil;
   FServerSocket := 0;
   FTakeLog := False;
   FPcName := '';
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.FormShow(Sender: TObject);
begin
   if FInitialized then begin
      Exit;
   end;

   FInitialized := TRUE;
   SendEdit.Text := '';
   ActiveControl := SendEdit;
   FCurrentBand := b35;
   FCurrentOperator := '';

   { Create a new thread to handle client request                          }
   FClientThread := TClientThread.Create(FServerSocket, Self, FClientNumber);
   FClientThread.TakeLog := FTakeLog;

   { Assign the thread's OnTerminate event                                 }
   FClientThread.OnTerminate := ClientThreadTerminate;

   { Then start the client thread work                                     }
   { because it was created in the blocked state                           }
   FClientThread.Start();
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   FClientThread.Release();
   PostMessage(TForm(Owner).Handle, WM_USER_CLIENT_CLOSED, 0, LongInt(Self));
end;

procedure TCliForm.ClientThreadTerminate(Sender: TObject);
begin
   //
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.AddConsole(S: string);
var
   _VisRows: Integer;
   _TopRow: Integer;
begin
   ListBox.Items.Add(S);
   _VisRows := ListBox.ClientHeight div ListBox.ItemHeight;
   _TopRow := ListBox.Items.Count - _VisRows + 1;
   if _TopRow > 0 then
      ListBox.TopIndex := _TopRow
   else
      ListBox.TopIndex := 0;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.SetCaption;
var
   S: string;
begin
   S := BandString[FCurrentBand];

   if FCurrentOperator <> '' then begin
      S := S + ' by ' + FCurrentOperator;
   end;

   if FPcname <> '' then begin
      S := S + ' on ' + FPcName;
   end;

   Caption := S;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.SetPcName(v: string);
begin
   FPcName := v;
   SetCaption();
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.AddChat(S: string);
var
   param_atom: ATOM;
begin
//   S := FillRight(IntToStr(ClientNumber), 3) + ' ' + S;

   param_atom := AddAtom(PChar(S));
   PostMessage(ServerForm.Handle, WM_ZCMD_ADDCONSOLE, ClientNumber, MAKELPARAM(param_atom,1));
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.SendButtonClick(Sender: TObject);
var
   S: string;
   t: string;
begin
   t := FormatDateTime('hh:nn', Now);
   S := t + ' ' + ' ZServer> ' + SendEdit.Text;

   AddChat(S);
   AddConsole(S);
   FClientThread.SendStr(ZLinkHeader + ' PUTMESSAGE ' + S + LBCODE);

   SendEdit.Clear;
   ActiveControl := SendEdit;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.DisconnectButtonClick(Sender: TObject);
begin
   Close;
end;

procedure TCliForm.SetCurrentBand(v: TBand);
begin
   FCurrentBand := v;
   SetCaption();
end;

procedure TCliForm.SetCurrentOperator(v: string);
begin
   FCurrentOperator := v;
   SetCaption();
end;

procedure TCliForm.SetClientNumber(v: Integer);
begin
   FClientNumber := v;
   if FClientThread <> nil then begin
      FCLientThread.ClientNumber := v;
   end;
end;

procedure TCliForm.SetTakeLog(v: Boolean);
begin
   FTakeLog := v;
   FClientThread.TakeLog := v;
end;

procedure TCliForm.SendStr(S: string);
begin
   FClientThread.SendStr(S);
end;

{ TClientThread }

constructor TClientThread.Create(ClientHSocket: TSocket; AClientForm: TCliForm; AClientNumber: Integer);
begin
   FClientHSocket  := ClientHSocket;
   FreeOnTerminate := True;
   FClientForm := AClientForm;
   FClientNumber := AClientNumber;
   FCurrentBand := bUnknown;
   FTakeLog := False;
   FInMergeProc := False;
   FCommandQue := TStringList.Create();
   FFileData := TStringList.Create();
   inherited Create(True);
end;

destructor TClientThread.Destroy();
begin
   if Assigned(FClientSocket) then begin
      FClientSocket.Destroy;
      FClientSocket := nil;
   end;

   FFileData.Free();
   FCommandQue.Free();
   inherited Destroy;
end;

procedure TClientThread.Release();
begin
   PostMessage(FClientSocket.Handle, WM_QUIT, 0, 0);
end;

procedure TClientThread.Execute();
begin
   FCommandLogFileName := StringReplace(Application.ExeName, '.exe', '_#' + IntToHex(Integer(Self), 8) + '_' + FormatDateTime('yyyymmdd', Now) + '.txt', [rfReplaceAll]);

   FClientSocket                 := TWSocket.Create(nil);
   FClientSocket.SocketErrs      := wsErrTech;
   FClientSocket.SocksLevel      := '5';
   FClientSocket.MultiThreaded   := True;
   FClientSocket.LineMode        := True;
   FClientSocket.LineEnd         := #13#10;
   FClientSocket.OnDataAvailable := ServerWSocketDataAvailable;
   FClientSocket.OnSessionClosed := ServerWSocketSessionClosed;
   FClientSocket.OnError         := CliSocketError;
   FClientSocket.onException     := CliSocketException;
   FClientSocket.OnSocksError    := CliSocketSocksError;
   FClientSocket.HSocket         := FClientHSocket;

   { Message loop to handle TWSocket messages                              }
   { The loop is exited when WM_QUIT message is received                   }
   FClientSocket.MessageLoop;

   { Returning from the Execute function effectively terminate the thread  }
   ReturnValue := 0;

   if FInMergeProc = True then begin
      MasterLogLock.Release();
   end;
end;

procedure TClientThread.ServerWSocketSessionClosed(Sender: TObject; Error: Word);
var
   S: string;
   param_atom: ATOM;
   t: string;
begin
   t := FormatDateTime('hh:nn', Now);
   S := t + ' ' + FillRight(IntToStr(FClientNumber), 3) + ' ' + MHzString[FCurrentBand] + ' client disconnected from network.';

   param_atom := AddAtom(PChar(S));
   SendMessage(ServerForm.Handle, WM_ZCMD_ADDCONSOLE, FClientNumber, MAKELPARAM(param_atom,0));

   param_atom := AddAtom(PChar(S + LBCODE));
   SendMessage(ServerForm.Handle, WM_ZCMD_SENDALL, FClientNumber, MAKELPARAM(param_atom,0));

   PostMessage(FClientForm.Handle, WM_CLOSE, 0, 0);
end;

procedure TClientThread.ServerWSocketDataAvailable(Sender: TObject; Error: Word);
var
   str: string;
   SL: TStringList;
   i: Integer;
   st: Integer;
   line: string;
begin
   SL := TStringList.Create();

   str := FClientSocket.ReceiveStr();

   st := 1;
   for i := 1 to Length(str) do begin
      if str[i] = #10 then begin
         line := TrimCRLF(Copy(str, st, i - st + 1));
         FCommandQue.Add(FillRight(IntToStr(FClientNumber), 3) + ' ' + line);
         st := i + 1;
      end;
   end;

   line := TrimCRLF(Copy(str, st));
   if line <> '' then begin
      FCommandQue.Add(FillRight(IntToStr(FClientNumber), 3) + ' ' + line);
   end;

   if FInProcessing = False then begin
      ProcessCommand('');
   end;

   SL.Free();
end;

// *****************************************************************************

procedure TClientThread.ProcessCommand(S: string);
var
   from: Integer;
   temp: string;
   i: Integer;
begin
   FInProcessing := True;
   try
      while FCommandQue.Count > 0 do begin
         S := FCommandQue.Strings[0];
         FCommandQue.Delete(0);

         from := StrToIntDef(TrimRight(copy(S, 1, 3)), -1) - 1;
         if from < 0 then begin
            Exit;
         end;

         Delete(S, 1, 4);
         Delete(S, 1, Length(ZLinkHeader) + 1);

         temp := S;

         if Pos('FREQ', temp) = 1 then begin
            Process_Freq(temp, from);
         end;

         if Pos('GETCONSOLE', UpperCase(temp)) = 1 then begin
            Process_GetConsole();
            Exit;
         end;

         if Pos('SENDRENEW', temp) = 1 then begin
            Process_SendRenew();
            Exit;
         end;

         {
           if Pos('FILELOADED', UpperCase(temp)) = 1 then
           begin
           sendbuf := ZLinkHeader + ' PROMPTUPDATE';
           SendAll(sendbuf+LBCODE);    end;
         }

         if Pos('WHO', UpperCase(temp)) = 1 then begin
            Process_Who();
            Exit;
         end;

         if Pos('OPERATOR', temp) = 1 then begin
            Process_Operator(temp, from);
            Exit;
         end;

         if Pos('BAND ', temp) = 1 then begin
            Process_Band(temp, from);
            Exit;
         end;

         if Pos('PCNAME', temp) = 1 then begin
            Process_PcName(temp, from);
            Exit;
         end;

         if Pos('RESET', temp) = 1 then begin
            Exit;
         end;

         if Pos('ENDLOG', temp) = 1 then // received when zLog finishes uploading
         begin
            Exit;
         end;

         if Pos('PUTMESSAGE', temp) = 1 then begin
            Process_PutMessage(temp, from);
         end;

         if Pos('SPOT', temp) = 1 then begin
         end;

         if Pos('SENDLOG', temp) = 1 then // will send all qsos in server's log and renew command
         begin
            Process_SendLog();
            Exit;
         end;

         if Pos('GETQSOIDS', temp) = 1 then // will send all qso ids in server's log
         begin
            Process_GetQsoIDs();
            Exit;
         end;

         if Pos('GETLOGQSOID', temp) = 1 then // will send all qso ids in server's log
         begin
            Process_GetLogQsoID(temp);
            Exit;
         end;

         if Pos('SENDCURRENT', temp) = 1 then begin
            Exit;
         end;

         if Pos('PUTQSO', temp) = 1 then begin
            Process_PutQso(temp, from);
         end;

         if Pos('PUTLOG ', temp) = 1 then begin
            Process_PutLog(temp, from);
         end;

         if Pos('EXDELQSO', temp) = 1 then begin
            Process_ExDelQso(temp, from);
         end;

         if Pos('DELQSO', temp) = 1 then begin
            Process_DelQso(temp, from);
         end;

         if Pos('EDITQSOFROM', temp) = 1 then begin
            Exit;
         end;

         if Pos('EDITQSOTO ', temp) = 1 then begin
            Process_EditQsoTo(temp, from);
         end;

         if Pos('INSQSOAT ', temp) = 1 then begin
            Exit;
         end;

         if Pos('RENEW', temp) = 1 then begin
            Process_Renew(temp, from);
         end;

         if Pos('INSQSO ', temp) = 1 then begin
            Process_InsQso(temp, from);
         end;

         if Pos('BEGINMERGE', temp) = 1 then begin
            i := 0;
            while(MasterLogLock.TryEnter() = False) do begin
               if i >= 3000 then begin       // 30sec待つ
                  S := ZLinkHeader + ' BEGINMERGE-NG';
                  SendStr(S + LBCODE);
                  Exit;
               end;

               if (i mod 50) = 0 then begin  // 0.5secに１度メッセージ出力
                  S := Format('%.1f', [(i * 10) / 1000]);
                  FClientForm.AddConsole('*** マージ処理の開始待ち... ' + S + 'sec ***');
               end;
               Sleep(10);
               Inc(i);
            end;

            FClientForm.AddConsole('*** マージ処理を開始します ***');
            FInMergeProc := True;

            S := ZLinkHeader + ' BEGINMERGE-OK';
            SendStr(S + LBCODE);
            Exit;
         end;

         if Pos('ENDMERGE', temp) = 1 then begin
            FClientForm.AddConsole('*** マージ処理を終了しました ***');
            FInMergeProc := False;
            MasterLogLock.Release();
            Exit;
         end;

         if Pos('GETFILE', temp) = 1 then begin
            Process_GetFile(temp, from);
         end;

         if Pos('PUTFILE', temp) = 1 then begin
            Process_PutFile(temp, from);
         end;

         if Pos('FILEDATA', temp) = 1 then begin
            Delete(temp, 1, 9);
            FFileData.Add(temp);
            Continue;
         end;

         S := ZLinkHeader + ' ' + temp;

         if ServerForm.ChatOnly = False then begin
            FClientForm.AddConsole(S);
            AddServerConsole(S);
         end;

         AddToCommandLog('R', S);

         ServerForm.SendAllButFrom(S + LBCODE, FClientNumber);
      end;
   finally
      FInProcessing := False;
   end;
end;

procedure TClientThread.Process_Freq(S: string; from: Integer);
var
   temp2: string;
   param_atom: ATOM;
begin
   temp2 := copy(S, 6, 255);
   param_atom := AddAtom(PChar(temp2));
   PostMessage(ServerForm.Handle, WM_ZCMD_FREQDATA, from, param_atom);
end;

procedure TClientThread.Process_GetConsole();
var
   SL: TStringList;
   i: Integer;
begin
   SL := ServerForm.GetConsole();
   try
      for i := 0 to SL.Count -1 do begin
         SendStr(SL[i] + LBCODE);
      end;
   finally
      SL.Free();
   end;
end;

procedure TClientThread.Process_SendRenew();
var
   S: string;
begin
   S := ZLinkHeader + ' RENEW';
   SendStr(S + LBCODE);
end;

procedure TClientThread.Process_Who();
var
   SL: TStringList;
   i: Integer;
begin
   SL := ServerForm.GetWhoList();
   try
      for i := 0 to SL.Count -1 do begin
         SendStr(SL[i] + LBCODE);
      end;
   finally
      SL.Free();
   end;
end;

procedure TClientThread.Process_Operator(S: string; from: Integer);
begin
   Delete(S, 1, 9);

   FClientForm.CurrentOperator := S;

   PostMessage(ServerForm.Handle, WM_ZCMD_UPDATE_DISPLAY, from, 0);
end;

procedure TClientThread.Process_Band(S: string; from: Integer);
var
   i: Integer;
   B: TBand;
begin
   Delete(S, 1, 5);

   i := StrToIntDef(S, 17);
   if not(i in [0 .. ord(HiBand), ord(bUnknown)]) then begin
      Exit;
   end;

   B := TBand(i);

   FClientForm.CurrentBand := B;

   PostMessage(ServerForm.Handle, WM_ZCMD_UPDATE_DISPLAY, from, 0);

   if ServerForm.IsBandUsed2(from, B) = True then begin
      S := ZLinkHeader + ' PUTMESSAGE ' + 'Band already in use!';
      SendStr(S + LBCODE);
   end;
end;

procedure TClientThread.Process_PcName(S: string; from: Integer);
begin
   Delete(S, 1, 7);

   FClientForm.PcName := S;

   PostMessage(ServerForm.Handle, WM_ZCMD_UPDATE_DISPLAY, from, 0);
end;


procedure TClientThread.Process_PutMessage(S: string; from: Integer);
var
   param_atom: ATOM;
begin
   Delete(S, 1, 11);
   param_atom := AddAtom(PChar(S));
   PostMessage(ServerForm.Handle, WM_ZCMD_ADDCONSOLE, from, MAKELPARAM(param_atom, 1));
end;

procedure TClientThread.Process_SendLog();
var
   i: Integer;
   S: string;
   C: Integer;
begin
   if ServerForm.MasterLog.TotalQSO = 0 then begin
      S := '*** MasterLog is empty ***';
      if ServerForm.ChatOnly = False then begin
         AddServerConsole(S);
      end;
      Exit;
   end;

   if ServerForm.ChatOnly = False then begin
      S := '*** BEGIN SENDLOG ***';
      AddServerConsole(S);
   end;

   C := 0;
   try
      for i := 1 to ServerForm.MasterLog.TotalQSO do begin

         if FClientSocket.State <> wsConnected then begin
            Exit;
         end;

         S := ZLinkHeader + ' PUTLOG ' + ServerForm.MasterLog.QSOList[i].QSOinText + LBCODE;
         SendStr(S);

         Sleep(0);
         Inc(C);
      end;

      if FClientSocket.State <> wsConnected then begin
         Exit;
      end;

      S := ZLinkHeader + ' RENEW' + LBCODE;
      SendStr(S);
   finally
      if ServerForm.ChatOnly = False then begin
         S := '*** END SENDLOG = ' + IntToStr(C) + ' QSOs sent ***';
         AddServerConsole(S);
      end;
   end;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TClientThread.Process_GetQsoIDs();
var
   Index: Integer;
   S: string;
   C: Integer;
begin
   Index := 1;
   while Index <= ServerForm.MasterLog.TotalQSO do begin

      C := 0;
      S := ZLinkHeader + ' QSOIDS ';
      repeat
         if ServerForm.MasterLog.QSOList[Index].Reserve3 <> 0 then begin
            S := S + IntToStr(ServerForm.MasterLog.QSOList[Index].Reserve3);
            S := S + ' ';
            Inc(C);
         end;
         Inc(Index);
      until (C = 20) or (Index > ServerForm.MasterLog.TotalQSO);

      SendStr(S + LBCODE);
   end;

   S := ZLinkHeader + ' ENDQSOIDS';
   SendStr(S + LBCODE);
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TClientThread.Process_GetLogQsoID(str: string);
var
   i: Integer;
   qsoid: Integer;
   S: string;
   temp: string;
   temp2: string;
   aQSO: TQSO;
begin
   temp := str + ' ';

   Delete(temp, 1, 12);
   i := Pos(' ', temp);
   while i > 1 do begin

      temp2 := copy(temp, 1, i - 1);
      Delete(temp, 1, i);
      qsoid := StrToInt(temp2);
      aQSO := ServerForm.GetQSObyID(qsoid);
      if aQSO <> nil then begin
         S := ZLinkHeader + ' PUTLOGEX ' + aQSO.QSOinText;
         SendStr(S + LBCODE);
      end;
      i := Pos(' ', temp);
   end;

   // 全部送ったら再計算させる
   S := ZLinkHeader + ' RENEW';
   SendStr(S + LBCODE);
end;

procedure TClientThread.Process_PutQso(S: string; from: Integer);
var
   aQSO: TQSO;
begin
   aQSO := TQSO.Create;
   Delete(S, 1, 7);
   aQSO.TextToQSO(S); // delete "PUTQSO "

   PostMessage(ServerForm.Handle, WM_ZCMD_PUTQSO, from, LPARAM(aQSO));
end;

procedure TClientThread.Process_PutLog(S: string; from: Integer);
var
   aQSO: TQSO;
begin
   aQSO := TQSO.Create;
   Delete(S, 1, 7);
   aQSO.TextToQSO(S);

   PostMessage(ServerForm.Handle, WM_ZCMD_PUTLOG, from, LPARAM(aQSO));
end;

procedure TClientThread.Process_ExDelQso(S: string; from: Integer);
var
   aQSO: TQSO;
begin
   aQSO := TQSO.Create;
   Delete(S, 1, 9);
   aQSO.TextToQSO(S);

   PostMessage(ServerForm.Handle, WM_ZCMD_EXDELQSO, from, LPARAM(aQSO));
end;

procedure TClientThread.Process_DelQso(S: string; from: Integer);
var
   aQSO: TQSO;
begin
   aQSO := TQSO.Create;
   Delete(S, 1, 7);
   aQSO.TextToQSO(S);

   PostMessage(ServerForm.Handle, WM_ZCMD_DELQSO, from, LPARAM(aQSO));
end;

procedure TClientThread.Process_EditQsoTo(S: string; from: Integer);
var
   aQSO: TQSO;
begin
   aQSO := TQSO.Create;
   Delete(S, 1, 10);
   aQSO.TextToQSO(S);
   aQSO.Reserve := actEdit;

   PostMessage(ServerForm.Handle, WM_ZCMD_EDITQSOTO, from, LPARAM(aQSO));
end;

procedure TClientThread.Process_Renew(S: string; from: Integer);
begin
   PostMessage(ServerForm.Handle, WM_ZCMD_RENEW, from, 0);
end;

procedure TClientThread.Process_InsQso(S: string; from: Integer);
var
   aQSO: TQSO;
begin
   aQSO := TQSO.Create;
   Delete(S, 1, 7);
   aQSO.TextToQSO(S);
   aQSO.Reserve := actInsert;

   PostMessage(ServerForm.Handle, WM_ZCMD_INSQSO, from, LPARAM(aQSO));
end;

// REQUEST
//   1234567812345678901234567890...
//   GETFILE file_name
//
// RESPONSE
//   FILEDATA file_data(base64) ...
//   PUTFILE file_size, line_count
procedure TClientThread.Process_GetFile(S: string; from: Integer);
var
   filename: string;
   mem: TMemoryStream;
   base64: TBase64Encoding;
   sl: TStringList;
   i: Integer;
   c: Integer;
   sendbuf: string;
   str: string;
begin
   if FClientSocket.State <> wsConnected then begin
      Exit;
   end;

   Delete(S, 1, 8);

   base64 := TBase64Encoding.Create();
   mem := TMemoryStream.Create();
   sl := TStringList.Create();
   try
      filename := GetZLogFilesPath() + S;
      if FileExists(filename) = False then begin
         FClientSocket.SendStr(ZLinkHeader + ' PUTFILE ERROR,FILE NOT FOUND' + LBCODE);
         Exit;
      end;

      mem.LoadFromFile(filename);
      mem.Position := 0;
      sl.Text := base64.EncodeBytesToString(mem.Memory, mem.Size);

      sendbuf := '';
      c := 0;
      for i := 0 to sl.Count - 1 do begin

         if c >= 5000 then begin
            FClientSocket.SendStr(sendbuf);

            c := 0;
            sendbuf := '';
         end;

         sendbuf := sendbuf + ZLinkHeader + ' FILEDATA ' + sl[i] + #13#10;
         Inc(c);
      end;

      FClientSocket.SendStr(sendbuf);

      filename := ExtractFileName(filename);
      str := ZLinkHeader + ' PUTFILE ' + filename + ',' + IntToStr(mem.Size) + ',' + IntToStr(sl.Count);
      FClientSocket.SendStr(str + LBCODE);
   finally
      mem.Free();
      base64.Free();
      sl.Free();
   end;
end;

procedure TClientThread.Process_PutFile(S: string; from: Integer);
var
   mem: TMemoryStream;
   base64: TBase64Encoding;
   sl: TStringList;
   i: Integer;
   str: string;
   file_name: string;
   file_size: Integer;
   line_count: Integer;
   bytes: TBytes;
   bakfile: string;
begin
   if FClientSocket.State <> wsConnected then begin
      Exit;
   end;

   Delete(S, 1, 8);

   base64 := TBase64Encoding.Create();
   mem := TMemoryStream.Create();
   sl := TStringList.Create();
   sl.StrictDelimiter := True;
   try
      sl.CommaText := Trim(S) + ',,,';
      file_name := sl[0];
      file_size := StrToIntDef(sl[1], 0);
      line_count := StrToIntDef(sl[2], 0);

      sl.Clear();
      for i := 0 to FFileData.Count - 1 do begin
         str := FFileData[i];
         sl.Add(str);
      end;

      bytes := base64.DecodeStringToBytes(sl.Text);

      mem.Size := Length(bytes);
      mem.Position := 0;
      mem.WriteBuffer(bytes, Length(bytes));

      file_name := GetZLogFilesPath() + file_name;
      if FileExists(file_name) = True then begin
         bakfile := ChangeFileExt(file_name, '.' + FormatDateTime('yyyymmddhhnnss', Now));
         CopyFile(PChar(file_name), PChar(bakfile), False);
      end;

      mem.SaveToFile(file_name);

      {$IFDEF DEBUG}
      FFileData.SaveToFile(ChangeFileExt(file_name, '.txt'));
      {$ENDIF}

      FFileData.Clear();

      FClientSocket.SendStr(ZLinkHeader + ' PUTFILE-OK' + LBCODE);
   finally
      mem.Free();
      base64.Free();
      sl.Free();
   end;
end;

procedure TClientThread.SendStr(str: string);
begin
   if FClientSocket.State <> wsConnected then begin
      Exit;
   end;

   FClientSocket.SendStr(str);

   AddToCommandLog('S', str);
end;


procedure TClientThread.AddServerConsole(S: string);
var
   param_atom: ATOM;
   t: string;
begin
   t := FormatDateTime('hh:nn', Now);
   S := t + ' ' + FillRight(IntToStr(FClientNumber), 3) + ' ' + S;

   param_atom := AddAtom(PChar(S));
   PostMessage(ServerForm.Handle, WM_ZCMD_ADDCONSOLE, FClientNumber, MAKELPARAM(param_atom,0));
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TClientThread.AddToCommandLog(direction: string; str: string);
var
   F: TextFile;
begin
   if FTakeLog = False then begin
      Exit;
   end;

   AssignFile(F, FCommandLogFileName);
   if FileExists(FCommandLogFileName) then begin
      Append(F);
   end
   else begin
      Rewrite(F);
   end;

   str := StringReplace(str, LBCODE, '', [rfReplaceAll]);

   WriteLn(F, direction + ' ' + str);

   CloseFile(F);
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TClientThread.CliSocketError(Sender: TObject);
var
   S: string;
begin
   S := 'CliSocketError error code = ' + IntToStr(FClientSocket.LastError);
   AddServerConsole(S);

   PostMessage(FClientSocket.Handle, WM_QUIT, 0, 0);
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TClientThread.CliSocketException(Sender: TObject; SocExcept: ESocketException);
var
   S: string;
begin
   S := '[' + SocExcept.IPStr + '] ' + IntToStr(SocExcept.ErrorCode) + ':' + SocExcept.ErrorMessage;
   AddServerConsole(S);

   PostMessage(FClientSocket.Handle, WM_QUIT, 0, 0);
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TClientThread.CliSocketSocksError(Sender: TObject; Error: Integer; Msg: string);
var
   S: string;
begin
   S := 'CliSocketSocksError: ' + msg + '(' + IntToStr(Error) + ')';
   AddServerConsole(S);

   PostMessage(FClientSocket.Handle, WM_QUIT, 0, 0);
end;

function TClientThread.GetZLogFilesPath(): string;
var
   path: string;
begin
   path := ExtractFilePath(Application.ExeName) + '\zlog_files\';

   if DirectoryExists(path) = False then begin
      ForceDirectories(path);
   end;

   Result := path;
end;

end.
