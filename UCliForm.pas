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
    procedure OnClientSendLog(var msg: TMessage); message WM_USER_CLIENT_SENDLOG;
    procedure OnClientGetQsoIDs(var msg: TMessage); message WM_USER_CLIENT_GETQSOIDS;
    procedure OnClientGetLogQsoID(var msg: TMessage); message WM_USER_CLIENT_GETLOGQSOID;
  private
    FInitialized : Boolean;
    LineBuffer : TStringList;
    CommTemp : string; //work string for parsing LineBuffer
    procedure AddServerConsole(msg: string);
    procedure SendLog();
    procedure GetQsoIDs();
    procedure GetLogQsoID(p: PChar);
  public
    ClientNumber : Integer;
    Bands : array[b19..HiBand] of boolean;
    CurrentBand : TBand;
    CurrentOperator : string;
    procedure SendStr(str : string);
    procedure ParseLineBuffer;
    procedure SetCaption;
    procedure AddConsole(S : string);
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
               ServerForm.AddCommandQue(FillRight(IntToStr(ClientNumber), 3) + ' ' + CommTemp);
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
begin
   str := CliSocket.ReceiveStr;
   if ServerForm.ChatOnly = False then begin
      AddConsole(str);
   end;

   LineBuffer.Add(str);

   ParseLineBuffer;
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

   ServerForm.ProcessCommand(S);
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

procedure TCliForm.OnClientSendLog(var msg: TMessage);
begin
   SendLog();
end;

procedure TCliForm.OnClientGetQsoIDs(var msg: TMessage);
begin
   GetQsoIDs();
end;

procedure TCliForm.OnClientGetLogQsoID(var msg: TMessage);
begin
   GetLogQsoID(PChar(msg.LParam));
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCliForm.SendLog();
var
   i: Integer;
   S: string;
   C: Integer;
begin
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

procedure TCliForm.GetLogQsoID(p: PChar);
var
   i: Integer;
   qsoid: Integer;
   S: string;
   temp: string;
   temp2: string;
   aQSO: TQSO;
begin
   temp := p;

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
