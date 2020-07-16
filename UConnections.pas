unit UConnections;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls,
  UzLogGlobal, UzLogConst;

type
  TConnections = class(TForm)
    ListBox: TListBox;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ListBoxDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateDisplay;
  end;

implementation

uses
  UServerForm;

{$R *.DFM}

procedure TConnections.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);
   Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TConnections.UpdateDisplay;
var
   B: TBand;
   i: integer;
   str: string;
begin
   ListBox.Items.Clear;
   for B := b19 to HiBand do begin
      for i := 0 to ServerForm.ClientList.Count - 1 do begin
         if ServerForm.ClientList[i].CurrentBand = B then begin
            str := FillRight(BandString[ServerForm.ClientList[i].CurrentBand], 9) + ServerForm.ClientList[i].CurrentOperator;
            ListBox.Items.AddObject(str, TObject(i));
         end;
      end;
   end;
end;

procedure TConnections.ListBoxDblClick(Sender: TObject);
var
   i: Integer;
begin
   i := Integer(ListBox.Items.Objects[ListBox.ItemIndex]);
   ServerForm.ClientList[i].Show;
end;

end.
