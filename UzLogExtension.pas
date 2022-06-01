{*******************************************************************************
 * Amateur Radio Operational Logging Software 'ZyLO' since 2020 June 22
 * License: The MIT License since 2021 October 28 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************}
unit UzLogExtension;

interface

uses
	Classes,
	Dialogs,
	Windows,
	Math,
	Forms,
	Menus,
	IOUtils,
	UITypes,
	Controls,
	IniFiles,
	StdCtrls,
	StrUtils,
	SysUtils,
   UMultipliers,
   UzLogQSO,
	AnsiStrings,
	JclFileUtils,
	RegularExpressions,
	System.Notification,
	Generics.Collections;

type
	TzLogEvent = (evInsertQSO = 0, evUpdateQSO, evDeleteQSO);

(*zLog event handlers*)
procedure zyloRuntimeLaunch;
procedure zyloRuntimeFinish;
procedure zyloWindowMessage(var msg: TMsg);
procedure zyloContestSwitch(test, path: string);
procedure zyloContestOpened(test, path: string);
procedure zyloContestClosed;
procedure zyloLogUpdated(event: TzLogEvent; bQSO, aQSO: TQSO);

(*zLog contest rules*)
function zyloRequestTotal(Points, Multi: integer): integer;
function zyloRequestScore(aQSO: TQSO): boolean;
function zyloRequestMulti(aQSO: TQSO; var mul: string): boolean;
function zyloRequestValid(aQSO: TQSO; var val: boolean): boolean;
function zyloRequestTable(Path: string; List: TCityList): boolean;

implementation

procedure zyloRuntimeLaunch;
begin
end;

procedure zyloRuntimeFinish;
begin
end;

procedure zyloWindowMessage(var msg: TMsg);
begin
end;

procedure zyloContestSwitch(test, path: string);
begin
end;

procedure zyloContestOpened(test, path: string);
begin
end;

procedure zyloContestClosed;
begin
end;

procedure zyloLogUpdated(event: TzLogEvent; bQSO, aQSO: TQSO);
begin
end;

function zyloRequestTotal(Points, Multi: integer): Integer;
begin
   Result := -1;
end;

(*returns whether the QSO score is calculated by this handler*)
function zyloRequestScore(aQSO: TQSO): boolean;
begin
   Result := False;
end;

(*returns whether the multiplier is extracted by this handler*)
function zyloRequestMulti(aQSO: TQSO; var mul: string): boolean;
begin
   Result := False;
end;

(*returns whether the multiplier is validated by this handler*)
function zyloRequestValid(aQSO: TQSO; var val: boolean): boolean;
begin
   Result := False;
end;

(*returns whether the cities list is provided by this handler*)
function zyloRequestTable(Path: string; List: TCityList): boolean;
begin
   Result := False;
end;

initialization

finalization

end.
