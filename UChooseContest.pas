unit UChooseContest;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TChooseContest = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    ContestBox: TRadioGroup;
  private
    { Private declarations }
    function GetContestNumber(): Integer;
  public
    { Public declarations }
    property ContestNumber: Integer read GetContestNumber;
  end;

implementation

{$R *.DFM}

function TChooseContest.GetContestNumber(): Integer;
begin
   Result := ContestBox.ItemIndex;
end;

end.
