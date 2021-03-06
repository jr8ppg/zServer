unit UCheckWin;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  UzLogGlobal, UzLogConst, UzLogQSO;

type
  TCheckWin = class(TForm)
    Panel1: TPanel;
    Button3: TButton;
    ListBox: TListBox;
    StayOnTop: TCheckBox;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Button3Click(Sender: TObject);
    procedure StayOnTopClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
   public
    { Public declarations }
    ListCWandPh : boolean;
    BandRow : array[b19..HiBand] of Integer;
    procedure ResetListBox;
    procedure Renew(aQSO : TQSO); virtual;
  end;

implementation

{$R *.DFM}

procedure TCheckWin.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);
   Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TCheckWin.Button3Click(Sender: TObject);
begin
   Close;
end;

procedure TCheckWin.StayOnTopClick(Sender: TObject);
begin
   If StayOnTop.Checked then
      FormStyle := fsStayOnTop
   else
      FormStyle := fsNormal;
end;

procedure TCheckWin.ResetListBox;
var
   B: TBand;
begin
   ListBox.Items.Clear;
   if ListCWandPh then begin
      for B := b19 to HiBand do
         if BandRow[B] >= 0 then begin
            ListBox.Items.Add(FillRight(MHzString[B], 5) + ' CW');
            ListBox.Items.Add(FillRight(MHzString[B], 5) + ' Ph');
         end;
   end
   else begin
      for B := b19 to HiBand do
         if BandRow[B] >= 0 then
            ListBox.Items.Add(MHzString[B]);
   end;
end;

procedure TCheckWin.Renew(aQSO: TQSO);
begin
end;

procedure TCheckWin.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
   { case Key of
     VK_ESCAPE : MainForm.LastFocus.SetFocus;
     end; }
end;

procedure TCheckWin.FormCreate(Sender: TObject);
begin
   ListCWandPh := False;
end;

end.
