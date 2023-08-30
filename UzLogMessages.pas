unit UzLogMessages;

interface

uses
  WinApi.Windows, WinApi.Messages;

const
  WM_USER_CLIENT_CLOSED = (WM_USER + 100);
  WM_USER_CLIENT_SENDLOG = (WM_USER + 101);
  WM_USER_CLIENT_GETQSOIDS = (WM_USER + 102);
  WM_USER_CLIENT_GETLOGQSOID = (WM_USER + 103);


implementation

end.
