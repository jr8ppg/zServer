unit UMergeBand;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls,
  UzLogConst;

type
  TMergeBand = class(TForm)
    cb19: TCheckBox;
    cb35: TCheckBox;
    cb7: TCheckBox;
    cb14: TCheckBox;
    cb21: TCheckBox;
    cb28: TCheckBox;
    cb50: TCheckBox;
    cb144: TCheckBox;
    cb430: TCheckBox;
    cb1200: TCheckBox;
    cb2400: TCheckBox;
    cb10g: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    CancelBtn: TButton;
    OKButton: TButton;
    cb5600: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  private
    { Private declarations }
    cbList : array[0..12] of TCheckBox;
  public
    FileName : string; // written by ServerForm.MergeFile1Click;
    { Public declarations }
  end;

implementation

uses
  UServerForm;

{$R *.DFM}

procedure TMergeBand.FormCreate(Sender: TObject);
var
   i: integer;
begin
   cbList[0] := cb19;
   cbList[1] := cb35;
   cbList[2] := cb7;
   cbList[3] := cb14;
   cbList[4] := cb21;
   cbList[5] := cb28;
   cbList[6] := cb50;
   cbList[7] := cb144;
   cbList[8] := cb430;
   cbList[9] := cb1200;
   cbList[10] := cb2400;
   cbList[11] := cb5600;
   cbList[12] := cb10g;
   for i := 0 to 12 do begin
      cbList[i].Visible := False;
   end;
end;

procedure TMergeBand.FormShow(Sender: TObject);
var
   i, count: integer;
begin
   count := 0;
   for i := 0 to 12 do begin
      if ServerForm.IsBandUsed(TBand(cbList[i].Tag)) = True then begin
         cbList[i].Top := 58 + 19 * count;
         cbList[i].Visible := True;
         inc(count);
      end
      else begin
         cbList[i].Visible := False;
      end;
   end;

   Height := 370 - (13 - count) * 19;
end;

procedure TMergeBand.OKButtonClick(Sender: TObject);
var
   BandSet: set of TBand;
   i: integer;
begin
   BandSet := [];
   for i := 0 to 12 do begin
      if cbList[i].Checked then begin
         BandSet := BandSet + [TBand(cbList[i].Tag)];
      end;
   end;

   ServerForm.MergeFile(FileName, BandSet);

   Close;
end;

procedure TMergeBand.CancelBtnClick(Sender: TObject);
begin
   Close;
end;

end.
