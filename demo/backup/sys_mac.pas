unit sys_mac;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MacOSAll; // MacOSAll contains macOS-specific API definitions

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
  OSVersion: NSString;
  HostName: NSString;
begin
  try
    // Get macOS version and hostname
    OSVersion := TNSString.Wrap(NSProcessInfo.processInfo.operatingSystemVersionString);
    HostName := TNSString.Wrap(NSProcessInfo.processInfo.hostName);

    Result.OSName := OSVersion.UTF8String;
    Result.KernelVersion := ''; // NSProcessInfo doesn't provide kernel version directly
    Result.MachineName := HostName.UTF8String;
  except
    on E: Exception do
      raise Exception.Create('Error retrieving macOS system information: ' + E.Message);
  end;
end;

class function SysMac.ExecuteAppleScript(const Script: string): string;
var
  ScriptFile: TStringList;
  ScriptPath, Output: string;
begin
  try
    // Save the script to a temporary file
    ScriptFile := TStringList.Create;
    try
      ScriptFile.Text := Script;
      ScriptPath := '/tmp/temp_script.applescript';
      ScriptFile.SaveToFile(ScriptPath);
    finally
      ScriptFile.Free;
    end;

    // Execute the script using osascript command
    Output := ExecuteProcess('/usr/bin/osascript', [ScriptPath]);
    Result := Output;
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

