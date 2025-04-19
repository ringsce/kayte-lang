unit sys_mac;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, MacOSAll, CocoaAll, Process, BaseUnix, ctypes;

type
  TMacOSSystemInfo = record
    OSName: string;
    KernelVersion: string;
    MachineName: string;
    MajorVersion: Integer;
    MinorVersion: Integer;
    PatchVersion: Integer;
    IsAppleSilicon: Boolean;
  end;

  SysMac = class
  public
    class function GetSystemInfo: TMacOSSystemInfo;
  end;

function sysctlbyname(name: PChar; oldp: Pointer; oldlenp: Psize_t;
  newp: Pointer; newlen: size_t): cint; cdecl; external 'c';

implementation

class function SysMac.GetSystemInfo: TMacOSSystemInfo;
var
  ProcessInfo: NSProcessInfo;
  Version: NSOperatingSystemVersion;
  UtsName: BaseUnix.UtsName;
  CPUType: array[0..255] of Char;
  Size: csize_t;
begin
  try
    ProcessInfo := NSProcessInfo.processInfo;
    Version := ProcessInfo.operatingSystemVersion;

    Result.MajorVersion := Version.majorVersion;
    Result.MinorVersion := Version.minorVersion;
    Result.PatchVersion := Version.patchVersion;

    Result.OSName := Format('macOS %d.%d.%d',
      [Version.majorVersion, Version.minorVersion, Version.patchVersion]);
    Result.MachineName := UTF8String(ProcessInfo.hostName.UTF8String);

    // Kernel version
    if fpUname(UtsName) = 0 then
      Result.KernelVersion := StrPas(@UtsName.release[0])
    else
      Result.KernelVersion := 'Unknown';

    // Detect Apple Silicon (ARM64)
    Size := SizeOf(CPUType);
    if sysctlbyname('hw.machine', @CPUType, @Size, nil, 0) = 0 then
      Result.IsAppleSilicon := Pos('arm64', LowerCase(StrPas(CPUType))) > 0
    else
      Result.IsAppleSilicon := False;

  except
    on E: Exception do
      raise Exception.Create('Error retrieving macOS system information: ' + E.Message);
  end;
end;

end.

