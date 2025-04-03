unit WinAPIWindow;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows, Winapi.Messages;

type
  TWinAPIWindow = class(TComponent)
  private
    FHandle: HWND;
    FCaption: string;
    FWidth, FHeight: Integer;
    FOnCreate, FOnDestroy: TNotifyEvent;
    procedure WndProc(var Msg: TMessage);
    procedure SetCaption(const Value: string);
  protected
    procedure CreateHandle; virtual;
    procedure DestroyHandle; virtual;
    procedure DoCreate; virtual;
    procedure DoDestroy; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show;
    procedure Close;
    property Handle: HWND read FHandle;
  published
    property Caption: string read FCaption write SetCaption;
    property Width: Integer read FWidth write FWidth default 800;
    property Height: Integer read FHeight write FHeight default 600;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TWinAPIWindow]);
end;

{ TWinAPIWindow }

constructor TWinAPIWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption := 'WinAPI Window';
  FWidth := 800;
  FHeight := 600;
end;

destructor TWinAPIWindow.Destroy;
begin
  DestroyHandle;
  inherited Destroy;
end;

procedure TWinAPIWindow.CreateHandle;
var
  WndClass: WNDCLASS;
  ClassName: string;
begin
  ClassName := 'TWinAPIWindow_' + IntToStr(Integer(Self));

  FillChar(WndClass, SizeOf(WndClass), 0);
  WndClass.lpfnWndProc := @DefWindowProc;
  WndClass.hInstance := HInstance;
  WndClass.lpszClassName := PChar(ClassName);
  WndClass.hCursor := LoadCursor(0, IDC_ARROW);

  if RegisterClass(WndClass) = 0 then
    RaiseLastOSError;

  FHandle := CreateWindow(
    PChar(ClassName),
    PChar(FCaption),
    WS_OVERLAPPEDWINDOW or WS_VISIBLE,
    CW_USEDEFAULT, CW_USEDEFAULT, FWidth, FHeight,
    0, 0, HInstance, nil
  );

  if FHandle = 0 then
    RaiseLastOSError;

  DoCreate;
end;

procedure TWinAPIWindow.DestroyHandle;
begin
  if FHandle <> 0 then
  begin
    DestroyWindow(FHandle);
    FHandle := 0;
    DoDestroy;
  end;
end;

procedure TWinAPIWindow.DoCreate;
begin
  if Assigned(FOnCreate) then
    FOnCreate(Self);
end;

procedure TWinAPIWindow.DoDestroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
end;

procedure TWinAPIWindow.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    if FHandle <> 0 then
      SetWindowText(FHandle, PChar(FCaption));
  end;
end;

procedure TWinAPIWindow.Show;
begin
  if FHandle = 0 then
    CreateHandle;
  ShowWindow(FHandle, SW_SHOW);
  UpdateWindow(FHandle);
end;

procedure TWinAPIWindow.Close;
begin
  if FHandle <> 0 then
    DestroyHandle;
end;

procedure TWinAPIWindow.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_DESTROY then
    FHandle := 0;

  Msg.Result := DefWindowProc(FHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

end.

