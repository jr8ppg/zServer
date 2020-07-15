unit UBasicMultiForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  UzLogGlobal, UzLogConst, UzLogQSO;

type
  TBasicMultiForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure CreateParams(var Params: TCreateParams); override;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ResetBand(B : TBand); virtual; abstract;
    //procedure RecalcBand(B : TBand); virtual; abstract;
    procedure RecalcAll; virtual; abstract;
    procedure Reset; virtual; abstract;
    procedure Add(aQSO : TQSO); virtual; abstract;
    //procedure AddNoUpdate(aQSO : TQSO); virtual; abstract;
    //procedure Update; virtual; abstract;
  end;

var
  BasicMultiForm: TBasicMultiForm;

implementation

{$R *.DFM}

procedure TBasicMultiForm.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);
   Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TBasicMultiForm.FormCreate(Sender: TObject);
begin
   Caption := 'Multipliers';
end;

end.
