# build_kayte_windows.ps1
#
# This script compiles the Kayte language project for Windows 11 (x64)
# using the MSVC (Microsoft Visual C++) compiler toolchain and LLVM tools.
#
# Prerequisites:
# - Visual Studio 2019 or 2022 (with "Desktop development with C++" workload)
# - LLVM tools (llc.exe) which are usually installed with VS C++ workload.
#   If llc.exe is not found, you might need to install LLVM separately or ensure
#   the Visual Studio installer included the "C++ Clang tools for Windows" component.
# - kayte_runtime.c in the same directory as this script.
# - kayte_program_x86_64.ll in the same directory as this script.

# Set PowerShell to stop on errors for robust execution
$ErrorActionPreference = "Stop"

# --- Configuration ---
$RUNTIME_C_FILE = "kayte_runtime.c"
$LL_FILE = "kayte_program_x86_64.ll" # Using the x86_64 LLVM IR as source for Windows x64 build
$OUTPUT_DIR = "build_windows_x64"
$EXECUTABLE_NAME = "kayte_app.exe"
$STATIC_LIB_NAME = "kayte_universal.lib" # .lib is the standard static library extension for MSVC
$DYNAMIC_LIB_NAME = "kayte_universal.dll" # .dll is the standard dynamic library extension for MSVC

$MSVC_ARCH = "x64" # Target architecture for MSVC (e.g., "x64", "x86", "arm64")
$MSVC_HOST_ARCH = "x64" # Host architecture for the build tools (e.g., "x64", "x86")

# --- Functions ---

function Log-Step {
    param (
        [string]$Message
    )
    Write-Host "--- $Message ---" -ForegroundColor Cyan
}

function Find-VcVarsAll {
    # Common Visual Studio installation paths to find vcvarsall.bat
    # Prioritize newer VS versions and "Community" edition for common setups
    $vsPaths = @(
        "$env:ProgramFiles\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvarsall.bat",
        "$env:ProgramFiles\Microsoft Visual Studio\2022\Professional\VC\Auxiliary\Build\vcvarsall.bat",
        "$env:ProgramFiles\Microsoft Visual Studio\2022\Enterprise\VC\Auxiliary\Build\vcvarsall.bat",
        "$env:ProgramFiles\Microsoft Visual Studio\2022\BuildTools\VC\Auxiliary\Build\vcvarsall.bat", # Build Tools
        "$env:ProgramFiles(x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvarsall.bat",
        "$env:ProgramFiles(x86)\Microsoft Visual Studio\2019\Professional\VC\Auxiliary\Build\vcvarsall.bat",
        "$env:ProgramFiles(x86)\Microsoft Visual Studio\2019\Enterprise\VC\Auxiliary\Build\vcvarsall.bat",
        "$env:ProgramFiles(x86)\Microsoft Visual Studio\2019\BuildTools\VC\Auxiliary\Build\vcvarsall.bat" # Build Tools
    )

    foreach ($path in $vsPaths) {
        if (Test-Path $path) {
            return $path
        }
    }
    throw "Error: vcvarsall.bat not found. Please ensure Visual Studio (or Build Tools) with 'Desktop development with C++' workload is installed."
}

function Set-MsvcEnvironment {
    param (
        [string]$VcVarsAllPath,
        [string]$Arch,
        [string]$HostArch
    )
    Log-Step "Setting up MSVC environment for $Arch (Host: $HostArch)..."
    # Execute vcvarsall.bat and capture environment changes
    # We use 'cmd /c' to run the batch file and then 'set' to list environment variables
    # This allows us to parse the changes and apply them to the current PowerShell session.
    $scriptBlock = [scriptblock]::Create("cmd /c `"$VcVarsAllPath`" $Arch $HostArch & set")
    $envChanges = (Invoke-Expression $scriptBlock) -split "`n" | Where-Object { $_ -match "=" }

    foreach ($line in $envChanges) {
        $parts = $line.Split('=', 2)
        if ($parts.Count -eq 2) {
            $varName = $parts[0].Trim()
            $varValue = $parts[1].Trim()
            # Set environment variable in current PowerShell session
            Set-Item Env:$varName $varValue
        }
    }
    Write-Host "MSVC environment configured." -ForegroundColor Green
}

# --- Main Script Execution ---

Log-Step "Starting Kayte Windows Build Script"

# Check for source files
if (-not (Test-Path $RUNTIME_C_FILE)) {
    throw "Error: $RUNTIME_C_FILE not found in the current directory. Please place it here."
}
if (-not (Test-Path $LL_FILE)) {
    throw "Error: $LL_FILE not found in the current directory. Please place it here."
}

# Find and set up MSVC environment
try {
    $vcvarsall = Find-VcVarsAll
    Set-MsvcEnvironment -VcVarsAllPath $vcvarsall -Arch $MSVC_ARCH -HostArch $MSVC_HOST_ARCH
} catch {
    Write-Error $_.Exception.Message
    exit 1
}

# Create output directory
try {
    New-Item -ItemType Directory -Force -Path $OUTPUT_DIR | Out-Null
    Write-Host "Output directory: $OUTPUT_DIR" -ForegroundColor Green
} catch {
    Write-Error "Failed to create output directory '$OUTPUT_DIR'. Error: $_.Exception.Message"
    exit 1
}


Log-Step "Compiling Kayte Runtime ($RUNTIME_C_FILE) for $MSVC_ARCH..."
# Use cl.exe (MSVC C/C++ compiler)
# /c: Compile only (no linking)
# /Fo: Specify output object file name
try {
    cl.exe /c /Fo:"$OUTPUT_DIR\kayte_runtime.obj" "$RUNTIME_C_FILE"
    Write-Host "Runtime compiled to $OUTPUT_DIR\kayte_runtime.obj" -ForegroundColor Green
} catch {
    Write-Error "Failed to compile runtime C file. Error: $_.Exception.Message"
    exit 1
}


Log-Step "Compiling Kayte LLVM IR ($LL_FILE) to object file for $MSVC_ARCH..."
# Use llc.exe (LLVM static compiler)
# -filetype=obj: Output object file
# -mtriple: Explicitly set the target triple for the LLVM backend.
#           This helps llc generate correct code for MSVC ABI, even if the .ll file's
#           internal target triple is different (e.g., from macOS generation).
#           Common MSVC triples: x86_64-pc-windows-msvc, i686-pc-windows-msvc, arm64-pc-windows-msvc
try {
    llc.exe -filetype=obj -mtriple "$MSVC_ARCH-pc-windows-msvc" -o "$OUTPUT_DIR\kayte_program.obj" "$LL_FILE"
    Write-Host "Kayte program compiled to $OUTPUT_DIR\kayte_program.obj" -ForegroundColor Green
} catch {
    Write-Error "Failed to compile LLVM IR file. Error: $_.Exception.Message"
    Write-Error "Ensure llc.exe is in your PATH after setting MSVC environment, or that 'C++ Clang tools for Windows' is installed in Visual Studio."
    exit 1
}


Log-Step "Creating Executable ($EXECUTABLE_NAME)..."
# Use link.exe (MSVC linker)
# /OUT: Specify output executable name
try {
    link.exe /OUT:"$OUTPUT_DIR\$EXECUTABLE_NAME" "$OUTPUT_DIR\kayte_program.obj" "$OUTPUT_DIR\kayte_runtime.obj"
    Write-Host "Executable created: $OUTPUT_DIR\$EXECUTABLE_NAME" -ForegroundColor Green
    Write-Host "You can run it with: .$OUTPUT_DIR\$EXECUTABLE_NAME" -ForegroundColor Green
} catch {
    Write-Error "Failed to link executable. Error: $_.Exception.Message"
    exit 1
}


Log-Step "Creating Static Library ($STATIC_LIB_NAME)..."
# Use lib.exe (MSVC librarian)
# /OUT: Specify output static library name
try {
    lib.exe /OUT:"$OUTPUT_DIR\$STATIC_LIB_NAME" "$OUTPUT_DIR\kayte_program.obj" "$OUTPUT_DIR\kayte_runtime.obj"
    Write-Host "Static library created: $OUTPUT_DIR\$STATIC_LIB_NAME" -ForegroundColor Green
} catch {
    Write-Error "Failed to create static library. Error: $_.Exception.Message"
    exit 1
}


Log-Step "Creating Dynamic Library ($DYNAMIC_LIB_NAME)..."
# Use link.exe for DLLs, specifying /DLL
# /OUT: Specify output dynamic library name
try {
    link.exe /DLL /OUT:"$OUTPUT_DIR\$DYNAMIC_LIB_NAME" "$OUTPUT_DIR\kayte_program.obj" "$OUTPUT_DIR\kayte_runtime.obj"
    Write-Host "Dynamic library created: $OUTPUT_DIR\$DYNAMIC_LIB_NAME" -ForegroundColor Green
} catch {
    Write-Error "Failed to create dynamic library. Error: $_.Exception.Message"
    exit 1
}


Log-Step "Cleaning up intermediate object files..."
# Remove intermediate .obj files
try {
    Remove-Item -Path "$OUTPUT_DIR\*.obj" -Force -ErrorAction SilentlyContinue | Out-Null
    Write-Host "Intermediate object files cleaned." -ForegroundColor Green
} catch {
    Write-Warning "Failed to clean up object files. Error: $_.Exception.Message"
}


Log-Step "Compilation complete!" -ForegroundColor Green
Write-Host "Your compiled binaries are in the '$OUTPUT_DIR' directory." -ForegroundColor Green

