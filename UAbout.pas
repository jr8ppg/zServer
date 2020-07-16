unit UAbout;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ShellApi, JclFileUtils;

type
  TAboutBox = class(TForm)
    Panel1: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Copyright: TLabel;
    Comments: TLabel;
    OKButton: TButton;
    Label2: TLabel;
    Panel2: TPanel;
    label1: TLabel;
    LinkLabel1: TLinkLabel;
    Label7: TLabel;
    procedure OKButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LinkLabel1LinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TAboutBox.OKButtonClick(Sender: TObject);
begin
   Close;
end;

procedure TAboutBox.FormCreate(Sender: TObject);
var
   ver: TJclFileVersionInfo;
begin
   ver := TJclFileVersionInfo.Create(Self.Handle);
   Label1.Caption := Application.Title + ' Version ' + ver.FileVersion + ' —ß˜a Edition based on 1.3';
   ver.Free();
end;

procedure TAboutBox.LinkLabel1LinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
   ShellExecute(Handle, 'open', PChar(Link), nil, nil, SW_SHOW);
end;

end.

