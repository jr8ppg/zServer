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
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ChooseContest: TChooseContest;

implementation

uses
  UServerForm, UALLJAMultiForm, UALLJAStats, USixDownStats,
  UACAGMultiForm, UFDMultiForm, UFDStats, UCQWWStats, UWWMultiForm;

{$R *.DFM}

procedure TChooseContest.OKBtnClick(Sender: TObject);
const
   cALLJA = 0;
   c6D = 1;
   cFD = 2;
   cACAG = 3;
   cCQWW = 4;
begin
   case ContestBox.ItemIndex of
      cALLJA: begin
         ServerForm.Stats := ALLJAStats;
         ServerForm.MultiForm := ALLJAMultiForm;
      end;

      c6D: begin
         ServerForm.Stats := SixDownStats;
         ServerForm.MultiForm := FDMultiForm;
         FDMultiForm.Init6D;
      end;

      cFD: begin
         ServerForm.Stats := FDStats;
         ServerForm.MultiForm := FDMultiForm;
      end;

      cACAG: begin
         ServerForm.Stats := ALLJAStats;
         ServerForm.MultiForm := ACAGMultiForm;
         ALLJAStats.InitACAG;
      end;

      cCQWW: begin
         ServerForm.Stats := CQWWStats;
         ServerForm.MultiForm := WWMultiForm;
      end;
   end;
end;

end.
