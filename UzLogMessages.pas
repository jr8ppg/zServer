unit UzLogMessages;

interface

uses
  WinApi.Windows, WinApi.Messages;

const
  WM_USER_CLIENT_CLOSED = (WM_USER + 100);

  WM_ZCMD_FREQDATA = (WM_USER + 200);
  WM_ZCMD_SETOPERATOR = (WM_USER + 201);
  WM_ZCMD_SETBAND = (WM_USER + 202);
  WM_ZCMD_PUTQSO = (WM_USER + 203);
  WM_ZCMD_PUTLOG = (WM_USER + 204);
  WM_ZCMD_EXDELQSO = (WM_USER + 205);
  WM_ZCMD_DELQSO = (WM_USER + 206);
  WM_ZCMD_EDITQSOTO = (WM_USER + 207);
  WM_ZCMD_RENEW = (WM_USER + 208);
  WM_ZCMD_INSQSO = (WM_USER + 209);
  WM_ZCMD_SENDALL = (WM_USER + 210);

implementation

end.
