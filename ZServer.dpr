program ZServer;

uses
  Forms,
  UServerForm in 'UServerForm.pas' {ServerForm},
  UCliForm in 'UCliForm.pas' {CliForm},
  UAbout in 'UAbout.pas' {AboutBox},
  UBasicMultiForm in 'UBasicMultiForm.pas' {BasicMultiForm},
  UBasicStats in 'UBasicStats.pas' {BasicStats},
  UALLJAMultiForm in 'UALLJAMultiForm.pas' {ALLJAMultiForm},
  UALLJAStats in 'UALLJAStats.pas' {AllJAStats},
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
  UzLogGlobal in 'UzLogGlobal.pas' {dmZLogGlobal: TDataModule};

{$R *.RES}

begin
  Application.Title := 'Z-Server';
  Application.CreateForm(TdmZLogGlobal, dmZLogGlobal);
  Application.CreateForm(TServerForm, ServerForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TBasicMultiForm, BasicMultiForm);
  Application.CreateForm(TBasicStats, BasicStats);
  Application.CreateForm(TALLJAMultiForm, ALLJAMultiForm);
  Application.CreateForm(TAllJAStats, AllJAStats);
  Application.CreateForm(TChooseContest, ChooseContest);
  Application.CreateForm(TSixDownStats, SixDownStats);
  Application.CreateForm(TACAGMultiForm, ACAGMultiForm);
  Application.CreateForm(TFDMultiForm, FDMultiForm);
  Application.CreateForm(TFDStats, FDStats);
  Application.CreateForm(TCQWWStats, CQWWStats);
  Application.CreateForm(TConnections, Connections);
  Application.CreateForm(TMergeBand, MergeBand);
  Application.CreateForm(TWWZone, WWZone);
  Application.CreateForm(TWWMultiForm, WWMultiForm);
  Application.CreateForm(TCheckWin, CheckWin);
  Application.CreateForm(TFreqList, FreqList);
  Application.Run;
end.
