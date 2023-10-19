unit UCliForm;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, OverbyteIcsWndControl, OverbyteIcsWSocket,
  Generics.Collections, Generics.Defaults, StrUtils,  WinSock,
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
    FTakeLog: Boolean;
    procedure AddChat(S: string);
    procedure SetCurrentBand(v: TBand);
    procedure SetCurrentOperator(v: string);
    procedure SetClientNumber(v: Integer);
    procedure SetTakeLog(v: Boolean);
    procedure SetCaption;
  public
    Bands : array[b19..HiBand] of boolean;
    procedure AddConsole(S : string);
    procedure SendStr(S: string);

    property CurrentBand: TBand read FCurrentBand write SetCurrentBand;
    property CurrentOperator: string read FCurrentOperator write SetCurrentOperator;
    property ClientNumber: Integer read FClientNumber write SetClientNumber;
    property Socket: TSocket read FServerSocket write FServerSocket;
    property TakeLog: Boolean read FTakeLog write SetTakeLog;
  end;

  TClientThread = class(TThread)
  private
    FClientSocket: TWSocket;
    FClientHSocket: TSocket;
    FClientForm: TCliForm;

    FLineBuffer: TStringList;
    FCommTemp : string; //work string for parsing LineBuffer

    FClientNumber: Integer;
    FCurrentBand : TBand;

    FCommandLogFileName: string;
    FTakeLog: Boolean;

    procedure ServerWSocketDataAvailable(Sender: TObject; Error: Word);
    procedure ServerWSocketSessionClosed(Sender: TObject; Error: Word);
    procedure CliSocketError(Sender: TObject);
    procedure CliSocketException(Sender: TObject; SocExcept: ESocketException);
    procedure CliSocketSocksError(Sender: TObject; Error: Integer; Msg: string);
    procedure ParseLineBuffer();
    procedure ProcessCommand(S: string);
    procedure SendLog();
    procedure GetQsoIDs();
    procedure GetLogQsoID(str: string);
    procedure AddServerConsole(S: string);
    procedure AddToCommandLog(direction: string; str: string);
  protected
    procedure Execute(); override;
  public
    constructor Create(ClientHSocket: TSocket; AClientForm: TCliForm; AClientNumber: Integer);
    destructor Destroy(); override;
    procedure Release();

    procedure SendStr(str: string);

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
var
   ClientThread: TClientThread;
begin
   FInitialized := False;
   FServerSocket := 0;
   FTakeLog := False;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.FormShow(Sender: TObject);
var
   B: TBand;
begin
   if FInitialized then begin
      Exit;
   end;

   FInitialized := TRUE;
   // DisplayMemo.Clear;
   SendEdit.Text := '';
   ActiveControl := SendEdit;
   FCurrentBand := b35;
   FCurrentOperator := '';
   for B := b19 to HiBand do begin
      Bands[B] := False;
   end;

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
   FClientThread.WaitFor();
   FClientThread.Free();
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

   Caption := S;
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
   t := FormatDateTime('hh:nn', SysUtils.Now);
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
   FLineBuffer := TStringList.Create();
   FTakeLog := False;
   inherited Create(True);
end;

destructor TClientThread.Destroy();
begin
   if Assigned(FClientSocket) then begin
      FClientSocket.Destroy;
      FClientSocket := nil;
   end;

   FLineBuffer.Free();

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
   FClientSocket.MultiThreaded   := True;
   FClientSocket.HSocket         := FClientHSocket;
   FClientSocket.OnDataAvailable := ServerWSocketDataAvailable;
   FClientSocket.OnSessionClosed := ServerWSocketSessionClosed;
   FClientSocket.OnError         := CliSocketError;
   FClientSocket.onException     := CliSocketException;
   FClientSocket.OnSocksError    := CliSocketSocksError;


   { Send the welcome message                                              }
   //FClientSocket.SendStr('Hello !' + #13 + #10 + '> ');

   { Message loop to handle TWSocket messages                              }
   { The loop is exited when WM_QUIT message is received                   }
   FClientSocket.MessageLoop;

   { Returning from the Execute function effectively terminate the thread  }
   ReturnValue := 0;
end;

procedure TClientThread.ServerWSocketSessionClosed(Sender: TObject; Error: Word);
var
   S: string;
   param_atom: ATOM;
   t: string;
begin
   t := FormatDateTime('hh:nn', SysUtils.Now);
   S := t + ' ' + FillRight(IntToStr(FClientNumber), 3) + ' ' + MHzString[FCurrentBand] + ' client disconnected from network.';

   param_atom := AddAtom(PChar(S));
   SendMessage(ServerForm.Handle, WM_ZCMD_ADDCONSOLE, FClientNumber, MAKELPARAM(param_atom,0));

   param_atom := AddAtom(PChar(S + LBCODE));
   SendMessage(ServerForm.Handle, WM_ZCMD_SENDALL, FClientNumber, MAKELPARAM(param_atom,0));

   PostMessage(FClientSocket.Handle, WM_QUIT, 0, 0);
end;

procedure TClientThread.ServerWSocketDataAvailable(Sender: TObject; Error: Word);
var
   str: string;
   SL: TStringList;
   i: Integer;
begin
   SL := TStringList.Create();

   str := FClientSocket.ReceiveStr();

   SL.Text := str;

   for i := 0 to Sl.Count - 1 do begin
      str := Trim(SL[i]);
      if str = '' then begin
         Continue;
      end;

      if ServerForm.ChatOnly = False then begin
         FClientForm.AddConsole(str);
         AddServerConsole(str);
      end;

//      AddToCommandLog('R', str);

      str := str + LBCODE;
      FLineBuffer.Add(str);
   end;

   ParseLineBuffer();

   SL.Free();
end;

procedure TClientThread.ParseLineBuffer();
var
   max, i, j, x: Integer;
   str: string;
begin
   max := FLineBuffer.Count - 1;
   if max < 0 then
      exit;
   for i := 0 to max do begin
      str := FLineBuffer.Strings[0];
      for j := 1 to Length(str) do begin
         if str[j] = chr($0D) then begin
            x := Pos(ZLinkHeader, FCommTemp);
            if x > 0 then begin
               FCommTemp := Copy(FCommTemp, x, 255);

               ProcessCommand(FillRight(IntToStr(FClientNumber), 3) + ' ' + FCommTemp);

//               ServerForm.AddCommandQue(FillRight(IntToStr(ClientNumber), 3) + ' ' + CommTemp);
            end;
            FCommTemp := '';
         end
         else
            FCommTemp := FCommTemp + str[j];
      end;
      FLineBuffer.Delete(0);
   end;
end;

// *****************************************************************************

procedure TClientThread.ProcessCommand(S: string);
var
   temp, temp2, sendbuf: string;
   from: Integer;
   aQSO: TQSO;
   i: Integer;
   B: TBand;
   param_atom: ATOM;
   SL: TStringList;
begin
   from := StrToIntDef(TrimRight(copy(S, 1, 3)), -1) - 1;
   if from < 0 then begin
      Exit;
   end;

   Delete(S, 1, 4);

   Delete(S, 1, Length(ZLinkHeader) + 1);

   temp := S;

   if Pos('FREQ', temp) = 1 then begin
      temp2 := copy(temp, 6, 255);
      param_atom := AddAtom(PChar(temp2));
      PostMessage(ServerForm.Handle, WM_ZCMD_FREQDATA, from, param_atom);
   end;

   if Pos('GETCONSOLE', UpperCase(temp)) = 1 then begin
      SL := ServerForm.GetConsole();
      for i := 0 to SL.Count -1 do begin
         SendStr(SL[i] + LBCODE);
      end;
      SL.Free();
      Exit;
   end;

   if Pos('SENDRENEW', temp) = 1 then begin
      sendbuf := ZLinkHeader + ' RENEW';
      SendStr(sendbuf + LBCODE);
      Exit;
   end;

   {
     if Pos('FILELOADED', UpperCase(temp)) = 1 then
     begin
     sendbuf := ZLinkHeader + ' PROMPTUPDATE';
     SendAll(sendbuf+LBCODE);    end;
   }

   if Pos('WHO', UpperCase(temp)) = 1 then begin
      SL := ServerForm.GetWhoList();
      for i := 0 to SL.Count -1 do begin
         SendStr(SL[i] + LBCODE);
      end;
      SL.Free();
      Exit;
   end;

   if Pos('OPERATOR', temp) = 1 then begin
      Delete(temp, 1, 9);

      FClientForm.CurrentOperator := temp;

      PostMessage(ServerForm.Handle, WM_ZCMD_UPDATE_DISPLAY, from, 0);
      Exit;
   end;

   if Pos('BAND ', temp) = 1 then begin
      Delete(temp, 1, 5);

      i := StrToIntDef(temp, 17);
      if not(i in [0 .. ord(HiBand), ord(bUnknown)]) then begin
         Exit;
      end;

      B := TBand(i);

      FClientForm.CurrentBand := B;

      PostMessage(ServerForm.Handle, WM_ZCMD_UPDATE_DISPLAY, from, 0);

      if ServerForm.IsBandUsed2(from, B) = True then begin
         sendbuf := ZLinkHeader + ' PUTMESSAGE ' + 'Band already in use!';
         SendStr(sendbuf + LBCODE);
      end;

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
      temp2 := temp;
      Delete(temp2, 1, 11);
      param_atom := AddAtom(PChar(temp2));
      PostMessage(ServerForm.Handle, WM_ZCMD_ADDCONSOLE, from, MAKELPARAM(param_atom, 1));
   end;

   if Pos('SPOT', temp) = 1 then begin
   end;

   if Pos('SENDLOG', temp) = 1 then // will send all qsos in server's log and renew command
   begin
      SendLog();
      Exit;
   end;

   if Pos('GETQSOIDS', temp) = 1 then // will send all qso ids in server's log
   begin
      GetQsoIDs();
      Exit;
   end;

   if Pos('GETLOGQSOID', temp) = 1 then // will send all qso ids in server's log
   begin
      GetLogQsoID(temp);
      Exit;
   end;

   if Pos('SENDCURRENT', temp) = 1 then begin
      Exit;
   end;

   if Pos('PUTQSO', temp) = 1 then begin
      aQSO := TQSO.Create;
      temp2 := temp;
      Delete(temp2, 1, 7);
      aQSO.TextToQSO(temp2); // delete "PUTQSO "

      PostMessage(ServerForm.Handle, WM_ZCMD_PUTQSO, from, LPARAM(aQSO));
   end;

   if Pos('PUTLOG ', temp) = 1 then begin
      aQSO := TQSO.Create;
      temp2 := temp;
      Delete(temp2, 1, 7);
      aQSO.TextToQSO(temp2);

      PostMessage(ServerForm.Handle, WM_ZCMD_PUTLOG, from, LPARAM(aQSO));
   end;

   if Pos('EXDELQSO', temp) = 1 then begin
      aQSO := TQSO.Create;
      temp2 := temp;
      Delete(temp2, 1, 9);
      aQSO.TextToQSO(temp2);

      PostMessage(ServerForm.Handle, WM_ZCMD_EXDELQSO, from, LPARAM(aQSO));
   end;

   if Pos('DELQSO', temp) = 1 then begin
      aQSO := TQSO.Create;
      temp2 := temp;
      Delete(temp2, 1, 7);
      aQSO.TextToQSO(temp2);

      PostMessage(ServerForm.Handle, WM_ZCMD_DELQSO, from, LPARAM(aQSO));
   end;

   if Pos('EDITQSOFROM', temp) = 1 then begin
      Exit;
   end;

   if Pos('EDITQSOTO ', temp) = 1 then begin
      aQSO := TQSO.Create;
      temp2 := temp;
      Delete(temp2, 1, 10);
      aQSO.TextToQSO(temp2);
      aQSO.Reserve := actEdit;

      PostMessage(ServerForm.Handle, WM_ZCMD_EDITQSOTO, from, LPARAM(aQSO));
   end;

   if Pos('INSQSOAT ', temp) = 1 then begin
      Exit;
   end;

   if Pos('RENEW', temp) = 1 then begin
      PostMessage(ServerForm.Handle, WM_ZCMD_RENEW, from, 0);
   end;

   if Pos('INSQSO ', temp) = 1 then begin
      aQSO := TQSO.Create;
      temp2 := temp;
      Delete(temp2, 1, 7);
      aQSO.TextToQSO(temp2);
      aQSO.Reserve := actInsert;

      PostMessage(ServerForm.Handle, WM_ZCMD_INSQSO, from, LPARAM(aQSO));
   end;

   sendbuf := ZLinkHeader + ' ' + temp;
   ServerForm.SendAllButFrom(sendbuf + LBCODE, from);
end;

procedure TClientThread.SendLog();
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
         Application.ProcessMessages();

         if FClientSocket.LastError <> 0 then begin
            Exit;
         end;

         S := ZLinkHeader + ' PUTLOG ' + ServerForm.MasterLog.QSOList[i].QSOinText + LBCODE;
         SendStr(S);

         Sleep(0);
         Inc(C);
      end;

      if FClientSocket.LastError <> 0 then begin
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

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TClientThread.GetQsoIDs();
var
   Index: Integer;
   S: string;
   C: Integer;
begin
   Index := 1;
   while Index <= ServerForm.MasterLog.TotalQSO do begin
      Application.ProcessMessages();

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

procedure TClientThread.GetLogQsoID(str: string);
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
      Application.ProcessMessages();

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

   // ëSïîëóÇ¡ÇΩÇÁçƒåvéZÇ≥ÇπÇÈ
   S := ZLinkHeader + ' RENEW';
   SendStr(S + LBCODE);
end;

procedure TClientThread.SendStr(str: string);
begin
   if FClientSocket.LastError <> 0 then begin
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
   t := FormatDateTime('hh:nn', SysUtils.Now);
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

end.
