{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Yokobayashi Yohei, JARL Contest Committee, JR8PPG
Description:  This program acts as a server when using zLog for multiple operations.
EMail:        zlog@zlog.org, jr8ppg@jarl.com
Creation:     1 August 2021
Version:      2.8
WebSite:      https://www.zlog.org/
              https://github.com/jr8ppg/zServer
Support:      Use the mailing list zlog-reiwa@cq-test.net
              See website for details below.
              https://github.com/jr8ppg/zLog/wiki/%E3%83%A1%E3%82%A4%E3%83%AA%E3%83%B3%E3%82%B0%E3%83%AA%E3%82%B9%E3%83%88
Legal issues: Copyright 1997-2002 by Yohei Yokobayashi
              Portions created by JR8PPG are Copyright (C) 2020-2022 JR8PPG
              This software is released under the MIT License.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit UServerForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IniFiles, Menus, ExtCtrls, System.UITypes,
  OverbyteIcsWndControl, OverbyteIcsWSocket, System.SyncObjs,
  UBasicStats, UBasicMultiForm, UCliForm, UFreqList, UConnections,
  UzLogGlobal, UzLogConst, UzLogQSO, JclFileUtils;

const
  IniFileName = 'ZServer.ini';
  WM_USER_CLIENT_CLOSED = (WM_USER + 0);

type
  TServerForm = class(TForm)
    SrvSocket: TWSocket;
    ClientListBox: TListBox;
    Panel1: TPanel;
    Button1: TButton;
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
    mLog: TMenuItem;
    CurrentFrequencies1: TMenuItem;
    Graph1: TMenuItem;
    DeleteDupes1: TMenuItem;
    //procedure CreateParams(var Params: TCreateParams); override;
    procedure FormShow(Sender: TObject);
    procedure SrvSocketSessionAvailable(Sender: TObject; Error: Word);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
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
    procedure mLogClick(Sender: TObject);
    procedure CurrentFrequencies1Click(Sender: TObject);
    procedure Graph1Click(Sender: TObject);
    procedure DeleteDupes1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Windows1Click(Sender: TObject);
    procedure SrvSocketError(Sender: TObject);
    procedure SrvSocketException(Sender: TObject; SocExcept: ESocketException);
    procedure SrvSocketSocksError(Sender: TObject; Error: Integer; Msg: string);
  private
    { D馗larations priv馥s }
    FInitialized  : Boolean;
    FClientNumber : Integer;
    FTakeLog : Boolean;
    FLogFileName: string;

    FFreqList: TFreqList;
    FConnections: TConnections;

    FClientList: TCliFormList;
    FStats: TBasicStats;
    FMultiForm: TBasicMultiForm;

    FCurrentFileName: string;

    FCommandQue: TStringList;

    procedure OnClientClosed(var msg: TMessage); message WM_USER_CLIENT_CLOSED;
    procedure StartServer;
    procedure LoadSettings();
    procedure SaveSettings();
    function GetMasterLog(): TLog;

    procedure RestoreWindowsPos();
    procedure RestoreWindowStates;
    procedure RecordWindowStates;
  public
    ChatOnly : boolean;
    procedure AddCommandQue(str: string);
    procedure AddToChatLog(str : string);
    procedure SendAll(str : string);
    procedure SendAllButFrom(str : string; NotThisCli : integer);
    procedure SendOnly(str : string; CliNo : integer);
    procedure ProcessCommand(S : string);
    procedure Idle;
    procedure IdleEvent(Sender: TObject; var Done: Boolean);
    procedure AddConsole(S : string); // adds string to clientlistbox
    function GetQSObyID(id : integer) : TQSO;
    function IsBandUsed(b: TBand): Boolean;
    procedure MergeFile(FileName : string; BandSet : TBandSet);
    procedure RecalcAll();

    property ClientList: TCliFormList read FClientList;
    property MasterLog: TLog read GetMasterLog;
  end;

var
  ServerForm: TServerForm;

  FQueLock: TCriticalSection;

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
begin
   FClientList := TCliFormList.Create();
   FCommandQue := TStringList.Create;
   FTakeLog := False;
   ChatOnly := True;
   FClientNumber := 0;
   FCurrentFileName := '';

   ver := TJclFileVersionInfo.Create(Self.Handle);
   Caption := Application.Title + ' Version ' + ver.FileVersion;
   ver.Free();

   FFreqList := TFreqList.Create(Self);
   FConnections := TConnections.Create(Self);

   LoadSettings();

   RestoreWindowsPos();
   CheckBox2.Checked := ChatOnly;

   Application.OnIdle := IdleEvent;

   FLogFileName := StringReplace(Application.ExeName, '.exe', '_' + FormatDateTime('yyyymmdd', Now) + '.txt', [rfReplaceAll]);
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TServerForm.FormShow(Sender: TObject);
var
   F: TChooseContest;
begin
   if FInitialized then begin
      Exit;
   end;

   FInitialized := True;

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

   for i := 0 to FClientList.Count - 1 do begin
      FClientList[i].Release();
   end;
   FClientList.Free();

   FCommandQue.Free();
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

function TServerForm.GetQSObyID(id: Integer): TQSO;
var
   i, j: Integer;
   aQSO: TQSO;
begin
   Result := nil;
   j := id div 100;
   for i := 1 to FStats.MasterLog.TotalQSO do begin
      aQSO := FStats.MasterLog.QSOList[i];
      if j = ((aQSO.Reserve3) div 100) then begin
         Result := aQSO;
         Exit;
      end;
   end;
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

procedure TServerForm.ProcessCommand(S: string);
var
   temp, temp2, sendbuf: string;
   from: Integer;
   aQSO: TQSO;
   i, j: Integer;
   B: TBand;
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
      FFreqList.ProcessFreqData(temp2);
   end;

   if Pos('GETCONSOLE', UpperCase(temp)) = 1 then begin
      for i := 0 to ClientListBox.Items.Count - 1 do begin
         sendbuf := ZLinkHeader + ' PUTMESSAGE ';
         sendbuf := sendbuf + ClientListBox.Items[i];
         SendOnly(sendbuf + LBCODE, from);
      end;
      Exit;
   end;

   if Pos('SENDRENEW', temp) = 1 then begin
      sendbuf := ZLinkHeader + ' RENEW';
      SendOnly(sendbuf + LBCODE, from);
      Exit;
   end;

   {
     if Pos('FILELOADED', UpperCase(temp)) = 1 then
     begin
     sendbuf := ZLinkHeader + ' PROMPTUPDATE';
     SendAll(sendbuf+LBCODE);    end;
   }

   if Pos('WHO', UpperCase(temp)) = 1 then begin
      for B := b19 to HiBand do
         for i := 0 to FClientList.Count - 1 do begin
            if FClientList[i].CurrentBand = B then begin
               sendbuf := ZLinkHeader + ' PUTMESSAGE ';
               sendbuf := sendbuf + FillRight(BandString[FClientList[i].CurrentBand], 9) + FClientList[i].CurrentOperator;
               SendOnly(sendbuf + LBCODE, from);
            end;
         end;
      Exit;
   end;

   if Pos('OPERATOR', temp) = 1 then begin
      Delete(temp, 1, 9);
      FClientList[from].CurrentOperator := temp;
      FClientList[from].SetCaption;
      FConnections.UpdateDisplay;
      Exit;
   end;

   if Pos('BAND ', temp) = 1 then begin
      Delete(temp, 1, 5);

      i := StrToIntDef(temp, -1);
      if not(i in [0 .. ord(HiBand)]) then begin
         Exit;
      end;

      B := TBand(i);

      FClientList[from].CurrentBand := B;

      for i := 0 to FClientList.Count - 1 do begin
         if (i <> from) and (FClientList[i].CurrentBand = B) then begin
            sendbuf := ZLinkHeader + ' PUTMESSAGE ' + 'Band already in use!';
            SendOnly(sendbuf + LBCODE, from);
            // CliList[from].Close;
         end;
      end;

      FClientList[from].SetCaption;
      FConnections.UpdateDisplay;
      Exit;
   end;

   if Pos('RESET', temp) = 1 then begin
      Exit;
      {
        temp := copy(temp, 7, 255);
        try
        i := StrToInt(temp);
        except
        on EConvertError do
        i := -1;
        end;
        if not(i in [0..ord(HiBand)]) then
        Exit;
        Stats.Logs[TBand(i)].Clear;
        Stats.UpdateStats;
        MultiForm.ResetBand(TBand(i)); }
   end;

   if Pos('ENDLOG', temp) = 1 then // received when zLog finishes uploading
   begin
      Exit;
      {
        temp := copy(temp, 8, 255);
        try
        i := StrToInt(temp);
        except
        on EConvertError do
        i := -1;
        end;
        if not(i in [0..ord(HiBand)]) then
        Exit;

        sendbuf := ZLinkHeader + ' RESETSUB ' +IntToStr(i);
        SendAllButFrom(sendbuf+LBCODE, from);

        for j := 1 to Stats.Logs[TBand(i)].TotalQSO do
        begin
        sendbuf := ZLinkHeader + ' PUTLOGSUB '+TQSO(Stats.Logs[TBand(i)].List[j]).QSOinText;
        SendAllButFrom(sendbuf+LBCODE, from);
        end;

        sendbuf := ZLinkHeader + ' RENEW ';
        SendAllButFrom(sendbuf+LBCODE, from);

        MultiForm.RecalcAll;
        Stats.UpdateStats; }
   end;

   if Pos('PUTMESSAGE', temp) = 1 then begin
      temp2 := temp;
      Delete(temp2, 1, 11);
      AddConsole(temp2);
      if FTakeLog then
         AddToChatLog(temp2);
   end;

   if Pos('SPOT', temp) = 1 then begin
   end;

   if Pos('SENDLOG', temp) = 1 then // will send all qsos in server's log and renew command
   begin
      if FStats.MasterLog.TotalQSO = 0 then
         Exit;

      for i := 1 to FStats.MasterLog.TotalQSO do begin
         sendbuf := ZLinkHeader + ' PUTLOG ' + FStats.MasterLog.QSOList[i].QSOinText;
         SendOnly(sendbuf + LBCODE, from);
      end;
      sendbuf := ZLinkHeader + ' RENEW';
      SendOnly(sendbuf + LBCODE, from);
      Exit;
   end;

   if Pos('GETQSOIDS', temp) = 1 then // will send all qso ids in server's log
   begin
      i := 1;
      while i <= FStats.MasterLog.TotalQSO do begin
         sendbuf := ZLinkHeader + ' QSOIDS ';
         repeat
            sendbuf := sendbuf + IntToStr(FStats.MasterLog.QSOList[i].Reserve3);
            sendbuf := sendbuf + ' ';
            inc(i);
         until (i mod 10 = 0) or (i > FStats.MasterLog.TotalQSO);
         SendOnly(sendbuf + LBCODE, from);
      end;
      sendbuf := ZLinkHeader + ' ENDQSOIDS';
      SendOnly(sendbuf + LBCODE, from);
      Exit;
   end;

   if Pos('GETLOGQSOID', temp) = 1 then // will send all qso ids in server's log
   begin
      Delete(temp, 1, 12);
      i := Pos(' ', temp);
      while i > 1 do begin
         temp2 := copy(temp, 1, i - 1);
         Delete(temp, 1, i);
         j := StrToInt(temp2);
         aQSO := GetQSObyID(j);
         if aQSO <> nil then begin
            sendbuf := ZLinkHeader + ' PUTLOGEX ' + aQSO.QSOinText;
            SendOnly(sendbuf + LBCODE, from);
         end;
         i := Pos(' ', temp);
      end;

      // 全部送ったら再計算させる
      sendbuf := ZLinkHeader + ' RENEW';
      SendOnly(sendbuf + LBCODE, from);

      Exit;
   end;

   if Pos('SENDCURRENT', temp) = 1 then begin
      Exit;
      { Delete(temp, 1, 12);
        try
        i := StrToInt(temp);
        except
        on EConvertError do
        i := -1;
        end;
        if not(i in [0..ord(HiBand)]) then
        Exit;

        B := TBand(i);

        if Stats.Logs[B].TotalQSO = 0 then
        Exit;

        for i := 1 to Stats.Logs[B].TotalQSO do
        begin
        sendbuf := ZLinkHeader + ' PUTLOG '+TQSO(Stats.Logs[B].List[i]).QSOinText;
        SendOnly(sendbuf+LBCODE, from);
        end;
        sendbuf := ZLinkHeader + ' RENEW ';
        SendOnly(sendbuf+LBCODE, from); }
   end;

   if Pos('PUTQSO', temp) = 1 then begin
      aQSO := TQSO.Create;
      temp2 := temp;
      Delete(temp2, 1, 7);
      aQSO.TextToQSO(temp2); // delete "PUTQSO "
      FStats.Add(aQSO);
      if Assigned(FMultiForm) then begin
         FMultiForm.Add(aQSO);
      end;
   end;

   if Pos('PUTLOG ', temp) = 1 then begin
      aQSO := TQSO.Create;
      temp2 := temp;
      Delete(temp2, 1, 7);
      aQSO.TextToQSO(temp2);
      FStats.AddNoUpdate(aQSO);
      if Assigned(FMultiForm) then begin
         FMultiForm.Add(aQSO);
      end;
   end;

   if Pos('EXDELQSO', temp) = 1 then begin
      aQSO := TQSO.Create;
      temp2 := temp;
      Delete(temp2, 1, 9);
      aQSO.TextToQSO(temp2);
      FStats.Delete(aQSO, False);
      aQSO.Free;
   end;

   if Pos('DELQSO', temp) = 1 then begin
      aQSO := TQSO.Create;
      temp2 := temp;
      Delete(temp2, 1, 7);
      aQSO.TextToQSO(temp2);
      FStats.Delete(aQSO);
      RecalcAll;
      FStats.UpdateStats;
      aQSO.Free;
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

      { Stats.Logs[aQSO.QSO.Band].AddQue(aQSO);
        Stats.Logs[aQSO.QSO.Band].ProcessQue; }
      FStats.MasterLog.AddQue(aQSO);
      FStats.MasterLog.ProcessQue;

      RecalcAll;

      FStats.UpdateStats;
      aQSO.Free;
   end;

   if Pos('INSQSOAT ', temp) = 1 then begin
      Exit;
   end;

   if Pos('RENEW', temp) = 1 then begin
      RecalcAll;
      FStats.UpdateStats;
      FStats.Refresh();
   end;

   if Pos('INSQSO ', temp) = 1 then begin
      aQSO := TQSO.Create;
      temp2 := temp;
      Delete(temp2, 1, 7);
      aQSO.TextToQSO(temp2);
      aQSO.Reserve := actInsert;
      FStats.MasterLog.AddQue(aQSO);
      FStats.MasterLog.ProcessQue;
      RecalcAll;
      FStats.UpdateStats;
      aQSO.Free;
   end;

   sendbuf := ZLinkHeader + ' ' + temp;
   SendAllButFrom(sendbuf + LBCODE, from);
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TServerForm.Idle;
var
   str: string;
begin
   while FCommandQue.Count > 0 do begin
      Application.ProcessMessages();
      FQueLock.Enter();
      str := FCommandQue[0];
      if not(ChatOnly) then begin
         AddConsole(str);
      end;
      ProcessCommand(str);
      FCommandQue.Delete(0);
      FQueLock.Leave();
   end;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TServerForm.IdleEvent(Sender: TObject; var Done: Boolean);
begin
   Idle;
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
begin
//
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

   Form := TCliForm.Create(self);
   Form.CliSocket.HSocket := SrvSocket.Accept;
   Form.Caption := 'Client ' + IntToStr(FClientNumber);
   Form.ClientNumber := FClientNumber;
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
      if i = NotThisCli then begin
         Continue;
      end;
      FClientList[i].SendStr(str);
   end;
end;

procedure TServerForm.SendOnly(str: string; CliNo: Integer);
begin
   FClientList[CliNo].SendStr(str);
end;

procedure TServerForm.Button1Click(Sender: TObject);
begin
   FConnections.UpdateDisplay;
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

procedure TServerForm.AddCommandQue(str: string);
begin
   FQueLock.Enter();
   FCommandQue.Add(str);
   FQueLock.Leave();
end;

procedure TServerForm.AddToChatLog(str: string);
var
   F: TextFile;
begin
   if FTakeLog = False then begin
      Exit;
   end;

   AssignFile(F, FLogFileName);
   if FileExists(FLogFileName) then begin
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

   if FTakeLog then begin
      AddToChatLog(S);
   end;

   SendEdit.Clear;
   ActiveControl := SendEdit;
end;

procedure TServerForm.Button2Click(Sender: TObject);
begin
   ClientListBox.Clear;
end;

procedure TServerForm.Timer1Timer(Sender: TObject);
begin
   Timer1.Enabled := False;
   try
      // Idle;
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
      if FCurrentFileName = '' then begin
         if SaveDialog.Execute then begin
            FCurrentFileName := SaveDialog.FileName;
         end
         else begin
            Exit;
         end;
      end;

      FStats.SaveLogs(FCurrentFileName);
   end;
end;

procedure TServerForm.SaveAs1Click(Sender: TObject);
begin
   If SaveDialog.Execute then begin
      FCurrentFileName := SaveDialog.FileName;
      FStats.SaveLogs(FCurrentFileName);
   end;
end;

procedure TServerForm.Open1Click(Sender: TObject);
begin
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
      FCommandQue.Add('999 ' + ZLinkHeader + ' FILELOADED');
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
      if OpenDialog.Execute then begin
         { if MessageDlg('This will clear all data and reload from new file.',
           mtWarning,
           [mbOK, mbCancel],
           0) = mrOK then }
         // CurrentFileName := OpenDialog.FileName;
         F.FileName := OpenDialog.FileName;
         F.ShowModal;
         // Stats.LoadFile(OpenDialog.FileName);
      end;
   finally
      F.Release();
   end;
end;

procedure TServerForm.mLogClick(Sender: TObject);
begin
   FTakeLog := not(FTakeLog);
   if FTakeLog then
      mLog.Caption := 'Stop &Log'
   else
      mLog.Caption := 'Start &Log';
end;

procedure TServerForm.CurrentFrequencies1Click(Sender: TObject);
begin
   FFreqList.Show;
end;

procedure TServerForm.Graph1Click(Sender: TObject);
begin
//   Graph.Show;
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
   finally
      IniFile.Free;
   end;
end;

function TServerForm.IsBandUsed(b: TBand): Boolean;
begin
   Result := FStats.UsedBands[b];
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

initialization
   FQueLock := TCriticalSection.Create();

finalization
   FQueLock.Free();

end.
