unit sys_mac;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec1}  // Enable Objective-C interoperability

interface

uses
  Classes, SysUtils, MacOSAll, CocoaAll, Process; // CocoaAll provides macOS API support

type
  TMacOSSystemInfo = record
    OSName: string;
    KernelVersion: string;
    MachineName: string;
  end;

  { SysMac }
  SysMac = class
  public
    class function GetSystemInfo: TMacOSSystemInfo;
    class function ExecuteAppleScript(const Script: string): string;
    class procedure SetSystemVolume(Volume: Integer);
  end;

implementation

{ SysMac }

class function SysMac.GetSystemInfo: TMacOSSystemInfo;
var
  ProcessInfo: NSProcessInfo;
begin
  try
    ProcessInfo := NSProcessInfo.processInfo;

    Result.OSName := UTF8String(ProcessInfo.operatingSystemVersionString.UTF8String);
    Result.MachineName := UTF8String(ProcessInfo.hostName.UTF8String);

    // Get Kernel Version using uname()
    Result.KernelVersion := '';
  except
    on E: Exception do
      raise Exception.Create('Error retrieving macOS system information: ' + E.Message);
  end;
end;

class function SysMac.ExecuteAppleScript(const Script: string): string;
var
  OutputList: TStringList;
  ScriptFile, Output: string;
begin
  try
    ScriptFile := '/tmp/temp_script.applescript';
    OutputList := TStringList.Create;
    try
      OutputList.Text := Script;
      OutputList.SaveToFile(ScriptFile);
    finally
      OutputList.Free;
    end;

    // Fix: Use `TProcess` instead of `ExecuteProcess`
    with TProcess.Create(nil) do
    try
      Executable := '/usr/bin/osascript';
      Parameters.Add(ScriptFile);
      Options := [poWaitOnExit, poUsePipes];
      Execute;

      // Read output
      SetLength(Output, OutputStream.Size);
      OutputStream.Read(Output[1], Length(Output));
      Result := Trim(Output);
    finally
      Free;
    end;
  except
    on E: Exception do
      raise Exception.Create('AppleScript execution failed: ' + E.Message);
  end;
end;

class procedure SysMac.SetSystemVolume(Volume: Integer);
var
  Script: string;
begin
  if (Volume < 0) or (Volume > 100) then
    raise Exception.Create('Volume must be between 0 and 100.');

  Script := Format('set volume output volume %d', [Volume]);
  ExecuteAppleScript(Script);
end;

end.

