program ZServer;

{$R 'resource.res' 'resource.rc'}

uses
  Forms,
  Windows,
  UServerForm in 'UServerForm.pas' {ServerForm},
  UCliForm in 'UCliForm.pas' {CliForm},
  UAbout in 'UAbout.pas' {AboutBox},
  UBasicMultiForm in 'UBasicMultiForm.pas' {BasicMultiForm},
  UBasicStats in 'UBasicStats.pas' {BasicStats},
  UALLJAMultiForm in 'UALLJAMultiForm.pas' {ALLJAMultiForm},
  USimpleStats in 'USimpleStats.pas' {SimpleStats},
  UChooseContest in 'UChooseContest.pas' {ChooseContest},
  USixDownStats in 'USixDownStats.pas' {SixDownStats},
  UACAGMultiForm in 'UACAGMultiForm.pas' {ACAGMultiForm},
  UFDMultiForm in 'UFDMultiForm.pas' {FDMultiForm},
  UFDStats in 'UFDStats.pas' {FDStats},
  UCQWWStats in 'UCQWWStats.pas' {CQWWStats},
  UConnections in 'UConnections.pas' {Connections},
  UMergeBand in 'UMergeBand.pas' {MergeBand},
  UWWMultiForm in 'UWWMultiForm.pas' {WWMultiForm},
  UWWZone in 'UWWZone.pas' {WWZone},
  UCheckWin in 'UCheckWin.pas' {CheckWin},
  UFreqList in 'UFreqList.pas' {FreqList},
  UzlogConst in 'UzlogConst.pas',
  UzLogQSO in 'UzLogQSO.pas',
  UzLogGlobal in 'UzLogGlobal.pas' {dmZLogGlobal: TDataModule},
  UMultipliers in 'UMultipliers.pas',
  UALLJAStats in 'UALLJAStats.pas' {AllJAStats},
  UzLogExtension in 'UzLogExtension.pas',
  UzLogMessages in 'UzLogMessages.pas';

{$R *.RES}

const
  ZSERVER_MUTEX = 'Z-Server';

var
  hMutex: THandle;

begin
  hMutex := OpenMutex(MUTEX_ALL_ACCESS, False, ZSERVER_MUTEX);
  if hMutex <> 0 then begin
    CloseHandle( hMutex );
    Exit;
  end;
  hMutex := CreateMutex(nil, False, ZSERVER_MUTEX);

  Application.Title := 'Z-Server';
  Application.CreateForm(TdmZLogGlobal, dmZLogGlobal);
  Application.CreateForm(TServerForm, ServerForm);
  Application.Run;

  ReleaseMutex( hMutex );
end.
