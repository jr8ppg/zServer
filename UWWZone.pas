unit UWWZone;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Grids,
  UzLogGlobal, UzLogConst;

type
  TWWZone = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    cbStayOnTop: TCheckBox;
    Grid: TStringGrid;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbStayOnTopClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Reset;
    procedure ResetBand(B : TBand);
    procedure Mark(B : TBand; Zone : integer);
    { Public declarations }
  end;

var
  WWZone: TWWZone;

implementation

{$R *.DFM}

const
  MaxWidth = 592;

procedure TWWZone.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);
   Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TWWZone.Reset;
var
   B: TBand;
   i, j: integer;
begin
   j := 1;
   for B := b19 to b28 do begin
      if NotWARC(B) then begin
         for i := 1 to 40 do begin
            Grid.Cells[i, j] := '.';
         end;
         inc(j);
      end;
   end;
end;

procedure TWWZone.ResetBand(B: TBand);
var
   i: integer;
begin
   for i := 1 to 40 do begin
      Grid.Cells[i, OldBandOrd(B) + 1] := '.';
   end;
end;

procedure TWWZone.Mark(B: TBand; Zone: integer);
begin
   Grid.Cells[Zone, OldBandOrd(B) + 1] := '*';
end;

procedure TWWZone.FormResize(Sender: TObject);
begin
   if WWZone.Width > MaxWidth then begin
      WWZone.Width := MaxWidth;
   end;
end;

procedure TWWZone.FormCreate(Sender: TObject);
begin
   Width := MaxWidth;
end;

procedure TWWZone.cbStayOnTopClick(Sender: TObject);
begin
   if cbStayOnTop.Checked then
      FormStyle := fsStayOnTop
   else
      FormStyle := fsNormal;
end;

procedure TWWZone.Button1Click(Sender: TObject);
begin
   Close;
end;

end.
