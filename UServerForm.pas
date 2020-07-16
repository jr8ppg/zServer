{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Fran輟is PIETTE
Description:  Demonstration for Server program using TWSocket.
EMail:        francois.piette@ping.be  http://www.rtfm.be/fpiette
              francois.piette@rtfm.be
Creation:     8 december 1997
Version:      1.01
WebSite:      http://www.rtfm.be/fpiette/indexuk.htm
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997 by Fran輟is PIETTE <francois.piette@ping.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

Updates:
Dec 09, 1997 V1.01 Made it compatible with Delphi 1

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit UServerForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IniFiles, Menus, ExtCtrls, System.UITypes,
  OverbyteIcsWndControl, OverbyteIcsWSocket,
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
  private
    { D馗larations priv馥s }
    FInitialized  : Boolean;
    FClientNumber : Integer;
    FTakeLog : Boolean;

    FFreqList: TFreqList;
    FConnections: TConnections;

    FClientList: TCliFormList;

    procedure OnClientClosed(var msg: TMessage); message WM_USER_CLIENT_CLOSED;
    procedure StartServer;
    procedure LoadSettings();
    procedure SaveSettings();
  public
    ChatOnly : boolean;
    CommandQue : TStringList;
    Stats : TBasicStats;
    MultiForm : TBasicMultiForm;
    procedure AddToChatLog(str : string);
    procedure SendAll(str : string);
    procedure SendAllButFrom(str : string; NotThisCli : integer);
    procedure SendOnly(str : string; CliNo : integer);
    procedure ProcessCommand(S : string);
    procedure Idle;
    procedure IdleEvent(Sender: TObject; var Done: Boolean);
    procedure AddConsole(S : string); // adds string to clientlistbox
    function GetQSObyID(id : integer) : TQSO;

    property ClientList: TCliFormList read FClientList;
  end;

var
  ServerForm: TServerForm;

implementation

uses
  UAbout, UChooseContest, UMergeBand,
  UALLJAMultiForm, UALLJAStats, USixDownStats,
  UACAGMultiForm, UFDMultiForm, UFDStats, UCQWWStats, UWWMultiForm;

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
   CommandQue := TStringList.Create;
   Application.OnIdle := IdleEvent;
   FTakeLog := False;
   ChatOnly := True;
   FClientNumber := 0;

   CheckBox2.Checked := ChatOnly;

   ver := TJclFileVersionInfo.Create(Self.Handle);
   Caption := Application.Title + ' Version ' + ver.FileVersion;
   ver.Free();

   FFreqList := TFreqList.Create(Self);
   FConnections := TConnections.Create(Self);

   LoadSettings();
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
         0: begin
            Stats := TALLJAStats.Create(Self);
            MultiForm := TALLJAMultiForm.Create(Self);
         end;

         1: begin
            Stats := TSixDownStats.Create(Self);
            MultiForm := TFDMultiForm.Create(Self);
            TFDMultiForm(MultiForm).Init6D;
         end;

         2: begin
            Stats := TFDStats.Create(Self);
            MultiForm := TFDMultiForm(Self);
         end;

         3: begin
            Stats := TALLJAStats.Create(Self);
            MultiForm := TACAGMultiForm.Create(Self);
            TALLJAStats(Stats).InitACAG;
         end;

         4: begin
            Stats := TCQWWStats.Create(Self);
            MultiForm := TWWMultiForm.Create(Self);
         end;
      end;

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
   if Stats.Saved = False then begin
      R := MessageDlg('Save changes to ' + CurrentFileName + ' ?', mtConfirmation, [mbYes, mbNo, mbCancel], 0); { HELP context 0 }
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

   CommandQue.Free();
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

function TServerForm.GetQSObyID(id: Integer): TQSO;
var
   i, j: Integer;
   aQSO: TQSO;
begin
   Result := nil;
   j := id div 100;
   for i := 1 to Stats.MasterLog.TotalQSO do begin
      aQSO := TQSO(Stats.MasterLog.QSOList[i]);
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
   from := StrToIntDef(TrimRight(copy(S, 1, 3)), 0);

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
      if Stats.MasterLog.TotalQSO = 0 then
         Exit;

      for i := 1 to Stats.MasterLog.TotalQSO do begin
         sendbuf := ZLinkHeader + ' PUTLOG ' + TQSO(Stats.MasterLog.QSOList[i]).QSOinText;
         SendOnly(sendbuf + LBCODE, from);
      end;
      sendbuf := ZLinkHeader + ' RENEW';
      SendOnly(sendbuf + LBCODE, from);
      Exit;
   end;

   if Pos('GETQSOIDS', temp) = 1 then // will send all qso ids in server's log
   begin
      i := 1;
      while i <= Stats.MasterLog.TotalQSO do begin
         sendbuf := ZLinkHeader + ' QSOIDS ';
         repeat
            sendbuf := sendbuf + IntToStr(TQSO(Stats.MasterLog.QSOList[i]).Reserve3);
            sendbuf := sendbuf + ' ';
            inc(i);
         until (i mod 10 = 0) or (i > Stats.MasterLog.TotalQSO);
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
            sendbuf := ZLinkHeader + ' PUTLOG ' + aQSO.QSOinText;
            SendOnly(sendbuf + LBCODE, from);
         end;
         i := Pos(' ', temp);
      end;
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
      Stats.Add(aQSO);
      MultiForm.Add(aQSO);
      aQSO.Free;
   end;

   if Pos('PUTLOG ', temp) = 1 then begin
      aQSO := TQSO.Create;
      temp2 := temp;
      Delete(temp2, 1, 7);
      aQSO.TextToQSO(temp2);
      Stats.AddNoUpdate(aQSO);
      MultiForm.Add(aQSO);
      aQSO.Free;
      // Exit;
   end;

   if Pos('DELQSO', temp) = 1 then begin
      aQSO := TQSO.Create;
      temp2 := temp;
      Delete(temp2, 1, 7);
      aQSO.TextToQSO(temp2);
      Stats.Delete(aQSO);
      // MultiForm.RecalcBand(aQSO.QSO.Band);
      MultiForm.RecalcAll;
      Stats.UpdateStats; // 0.24
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
      Stats.MasterLog.AddQue(aQSO);
      Stats.MasterLog.ProcessQue;

      MultiForm.RecalcAll;
      Stats.UpdateStats;
      aQSO.Free;
   end;

   if Pos('INSQSOAT ', temp) = 1 then begin
      Exit;
   end;

   if Pos('RENEW', temp) = 1 then begin
      Stats.UpdateStats;
   end;

   if Pos('INSQSO ', temp) = 1 then begin
      aQSO := TQSO.Create;
      temp2 := temp;
      Delete(temp2, 1, 7);
      aQSO.TextToQSO(temp2);
      aQSO.Reserve := actInsert;
      Stats.MasterLog.AddQue(aQSO);
      Stats.MasterLog.ProcessQue;
      MultiForm.RecalcAll;
      Stats.UpdateStats;
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
   { if CommandQue.Count = 0 then
     if Connections.Visible then
     Connections.Update; }
   {
     for i := 1 to 99 do
     begin
     if CliList[i] = nil then
     break
     else
     CliList[i].ParseLineBuffer;
     end;
   }
   while CommandQue.Count > 0 do begin
      str := CommandQue[0];
      if not(ChatOnly) then begin
         AddConsole(str);
      end;
      ProcessCommand(str);
      CommandQue.Delete(0);
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
      FClientList[i].SendStr(str);
   end;
end;

procedure TServerForm.SendOnly(str: string; CliNo: Integer);
begin
   if FClientList[CliNo] <> nil then begin
      FClientList[CliNo].SendStr(str);
   end;
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

procedure TServerForm.AddToChatLog(str: string);
var
   f: TextFile;
   // t : string;
begin
   if FTakeLog = False then begin
      Exit;
   end;

   AssignFile(f, 'log.txt');
   if FileExists('log.txt') then begin
      Append(f);
   end
   else begin
      Rewrite(f);
   end;

   {
     t := FormatDateTime(' (hh:nn)', SysUtils.Now);
   }

   WriteLn(f, str { + t } );

   CloseFile(f);
end;

procedure TServerForm.SendButtonClick(Sender: TObject);
var
   t, S: string;
begin
   t := FormatDateTime('hh:nn', SysUtils.Now);
   S := t + ' ZServer> ' + SendEdit.Text;

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
   // Idle;
   while ClientListBox.Items.Count > 400 do begin
      ClientListBox.Items.Delete(0);
   end;
end;

procedure TServerForm.ScoreandStatistics1Click(Sender: TObject);
begin
   Stats.Show;
end;

procedure TServerForm.Multipliers1Click(Sender: TObject);
begin
   MultiForm.Show;
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
   if Stats.Saved = False then begin
      if CurrentFileName = '' then begin
         if SaveDialog.Execute then begin
            CurrentFileName := SaveDialog.FileName;
         end
         else begin
            Exit;
         end;
      end;

      Stats.SaveLogs(CurrentFileName);
   end;
end;

procedure TServerForm.SaveAs1Click(Sender: TObject);
begin
   If SaveDialog.Execute then begin
      CurrentFileName := SaveDialog.FileName;
      Stats.SaveLogs(CurrentFileName);
   end;
end;

procedure TServerForm.Open1Click(Sender: TObject);
begin
   if OpenDialog.Execute then begin
      if MessageDlg('This will clear all data and reload from new file.', mtWarning, [mbOK, mbCancel], 0) = mrOK then begin
         CurrentFileName := OpenDialog.FileName;
      end;
      Stats.LoadFile(OpenDialog.FileName);
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
   Stats.MasterLog.RemoveDupes;
   MultiForm.RecalcAll;
   Stats.UpdateStats;
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

end.
