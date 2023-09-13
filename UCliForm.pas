unit UCliForm;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, OverbyteIcsWndControl, OverbyteIcsWSocket,
  Generics.Collections, Generics.Defaults,
  UzLogGlobal, UzLogConst, UzLogQSO, UzLogMessages;

const LBCODE = #13{+#10};

type
  TCliForm = class(TForm)
    CliSocket: TWSocket;
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
    procedure CliSocketDataAvailable(Sender: TObject; Error: Word);
    procedure CliSocketSessionClosed(Sender: TObject; Error: Word);
    procedure SendButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure CliSocketException(Sender: TObject; SocExcept: ESocketException);
    procedure CliSocketError(Sender: TObject);
    procedure CliSocketSocksError(Sender: TObject; Error: Integer; Msg: string);
    procedure CliSocketSessionConnected(Sender: TObject; ErrCode: Word);
  private
    FInitialized : Boolean;
    LineBuffer : TStringList;
    CommTemp : string; //work string for parsing LineBuffer
    FClientNumber: Integer;
    FCommandLogFileName: string;
    FTakeLog: Boolean;
    procedure AddServerConsole(msg: string);
    procedure SendLog();
    procedure GetQsoIDs();
    procedure GetLogQsoID(str: string);
    procedure ProcessCommand(S: string);
    procedure AddToCommandLog(str: string);
    procedure SetClientNumber(v: Integer);
  public
    Bands : array[b19..HiBand] of boolean;
    CurrentBand : TBand;
    CurrentOperator : string;
    procedure SendStr(str : string);
    procedure ParseLineBuffer;
    procedure SetCaption;
    procedure AddConsole(S : string);

    property ClientNumber: Integer read FClientNumber write SetClientNumber;
    property TakeLog: Boolean read FTakeLog write FTakeLog;
  end;

  TCliFormList = class(TObjectList<TCliForm>)
  private
  public
    constructor Create(OwnsObjects: Boolean = False);
    destructor Destroy(); override;
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
   FCommandLogFileName := StringReplace(Application.ExeName, '.exe', '_#' + IntToHex(Integer(Self), 8) + '_' + FormatDateTime('yyyymmdd', Now) + '.txt', [rfReplaceAll]);
   FTakeLog := False;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.FormShow(Sender: TObject);
var
   B: TBand;
begin
   if not FInitialized then begin
      FInitialized := TRUE;
      // DisplayMemo.Clear;
      SendEdit.Text := '';
      ActiveControl := SendEdit;
      LineBuffer := TStringList.Create;
      CurrentBand := b35;
      CurrentOperator := '';
      for B := b19 to HiBand do
         Bands[B] := False;
   end;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   LineBuffer.Clear;
   LineBuffer.Free;
   PostMessage(TForm(Owner).Handle, WM_USER_CLIENT_CLOSED, 0, LongInt(Self));
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
   S := BandString[CurrentBand];

   if CurrentOperator <> '' then begin
      S := S + ' by ' + CurrentOperator;
   end;

   Caption := S;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.SendStr(str: string);
begin
   if CliSocket.LastError <> 0 then begin
      Exit;
   end;

   CliSocket.SendStr(str);
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.ParseLineBuffer;
var
   max, i, j, x: Integer;
   str: string;
begin
   max := LineBuffer.Count - 1;
   if max < 0 then
      exit;
   for i := 0 to max do begin
      str := LineBuffer.Strings[0];
      for j := 1 to Length(str) do begin
         if str[j] = chr($0D) then begin
            x := Pos(ZLinkHeader, CommTemp);
            if x > 0 then begin
               CommTemp := Copy(CommTemp, x, 255);

               ProcessCommand(FillRight(IntToStr(ClientNumber), 3) + ' ' + CommTemp);

//               ServerForm.AddCommandQue(FillRight(IntToStr(ClientNumber), 3) + ' ' + CommTemp);
            end;
            CommTemp := '';
         end
         else
            CommTemp := CommTemp + str[j];
      end;
      LineBuffer.Delete(0);
   end;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.CliSocketDataAvailable(Sender: TObject; Error: Word);
var
   str: string;
   SL: TStringList;
   i: Integer;
begin
   SL := TStringList.Create();

   str := CliSocket.ReceiveStr;

   SL.Text := str;

   for i := 0 to Sl.Count - 1 do begin
      str := Trim(SL[i]);
      if str = '' then begin
         Continue;
      end;

      if ServerForm.ChatOnly = False then begin
         AddConsole(str);
      end;

      AddToCommandLog(str);

      str := str + LBCODE;
      LineBuffer.Add(str);
   end;

   ParseLineBuffer;

   SL.Free();
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.AddServerConsole(msg: string);
var
   S: string;
begin
   S := FillRight(IntToStr(ClientNumber), 3) + ' ' +
        ZLinkHeader + ' PUTMESSAGE ' +
        FormatDateTime('hh:nn', SysUtils.Now) + ' ' +
        msg;

   ProcessCommand(S);
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.CliSocketError(Sender: TObject);
var
   S: string;
begin
   S := 'CliSocketError error code = ' + IntToStr(CliSocket.LastError);
   AddServerConsole(S);

   CliSocket.Close();
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.CliSocketException(Sender: TObject; SocExcept: ESocketException);
var
   S: string;
begin
   S := '[' + SocExcept.IPStr + '] ' + IntToStr(SocExcept.ErrorCode) + ':' + SocExcept.ErrorMessage;
   AddServerConsole(S);

   CliSocket.Close();
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.CliSocketSessionClosed(Sender: TObject; Error: Word);
var
   S: string;
begin
   S := MHzString[CurrentBand] + ' client disconnected from network.';

   AddServerConsole(S);

   Close;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.CliSocketSessionConnected(Sender: TObject; ErrCode: Word);
begin
//
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.CliSocketSocksError(Sender: TObject; Error: Integer; Msg: string);
var
   S: string;
begin
   S := 'CliSocketSocksError: ' + msg + '(' + IntToStr(Error) + ')';
   AddServerConsole(S);

   CliSocket.Close();
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.SendButtonClick(Sender: TObject);
var
   S: string;
   t: string;
begin
   t := FormatDateTime('hh:nn', SysUtils.Now);
   S := ' ZServer> ' + SendEdit.Text;

   AddServerConsole(S);
   AddConsole(t + ' ' + S);

   SendEdit.Clear;
   ActiveControl := SendEdit;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.DisconnectButtonClick(Sender: TObject);
begin
   Close;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.SendLog();
var
   i: Integer;
   S: string;
   C: Integer;
begin
   if ServerForm.MasterLog.TotalQSO = 0 then begin
      S := '*** MasterLog is empty ***';
      AddServerConsole(S);
      Exit;
   end;

   S := '*** BEGIN SENDLOG ***';
   AddServerConsole(S);

   C := 0;
   try
      for i := 1 to ServerForm.MasterLog.TotalQSO do begin
         Application.ProcessMessages();

         if CliSocket.LastError <> 0 then begin
            Exit;
         end;

         S := ZLinkHeader + ' PUTLOG ' + ServerForm.MasterLog.QSOList[i].QSOinText + LBCODE;
         SendStr(S);

         Sleep(0);
         Inc(C);
      end;

      if CliSocket.LastError <> 0 then begin
         Exit;
      end;

      S := ZLinkHeader + ' RENEW' + LBCODE;
      SendStr(S);
   finally
      S := '*** END SENDLOG = ' + IntToStr(C) + ' QSOs sent ***';
      AddServerConsole(S);
   end;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.GetQsoIDs();
var
   i: Integer;
   S: string;
begin
   i := 1;
   while i <= ServerForm.MasterLog.TotalQSO do begin
      S := ZLinkHeader + ' QSOIDS ';
      repeat
         S := S + IntToStr(ServerForm.MasterLog.QSOList[i].Reserve3);
         S := S + ' ';
         inc(i);
      until (i mod 10 = 0) or (i > ServerForm.MasterLog.TotalQSO);

      SendStr(S + LBCODE);
   end;

   S := ZLinkHeader + ' ENDQSOIDS';
   SendStr(S + LBCODE);
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.GetLogQsoID(str: string);
var
   i: Integer;
   qsoid: Integer;
   S: string;
   temp: string;
   temp2: string;
   aQSO: TQSO;
begin
   temp := str;

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

   // ëSïîëóÇ¡ÇΩÇÁçƒåvéZÇ≥ÇπÇÈ
   S := ZLinkHeader + ' RENEW';
   SendStr(S + LBCODE);
end;

// *****************************************************************************

procedure TCliForm.ProcessCommand(S: string);
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
      param_atom := AddAtom(PChar(temp));
      PostMessage(ServerForm.Handle, WM_ZCMD_SETOPERATOR, from, param_atom);
      Exit;
   end;

   if Pos('BAND ', temp) = 1 then begin
      Delete(temp, 1, 5);

      i := StrToIntDef(temp, 17);
      if not(i in [0 .. ord(HiBand), ord(bUnknown)]) then begin
         Exit;
      end;

      B := TBand(i);

      PostMessage(ServerForm.Handle, WM_ZCMD_SETBAND, from, i);

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
      PostMessage(ServerForm.Handle, WM_ZCMD_ADDCONSOLE, from, param_atom);
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

procedure TCliForm.AddToCommandLog(str: string);
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

   WriteLn(F, str);

   CloseFile(F);
end;

procedure TCliForm.SetClientNumber(v: Integer);
begin
   FClientNumber := v;
end;


{ TCliFormList }

constructor TCliFormList.Create(OwnsObjects: Boolean);
begin
   Inherited Create(OwnsObjects);
end;

destructor TCliFormList.Destroy();
begin
   Inherited;
end;

end.
