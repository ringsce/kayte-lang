unit kayte_sdl3;
interface

uses SysUtils;

const
{$IFDEF WINDOWS}
    SDL3_Lib = 'SDL3.dll';
  {$ELSE}
    {$IFDEF DARWIN}
      SDL3_Lib = 'libSDL3.dylib';
    {$ELSE}
      SDL3_Lib = 'libSDL3.so';
    {$ENDIF}
  {$ENDIF}

// Manually link the core SDL3 functions you need for the backend
function SDL_Init(flags: UInt32): Integer; cdecl; external SDL3_Lib Name 'SDL_Init';
function SDL_CreateWindow(title: PChar; w, h: Integer; flags: UInt64): Pointer; cdecl; external SDL3_Lib Name 'SDL_CreateWindow';
procedure SDL_DestroyWindow(window: Pointer); cdecl; external SDL3_Lib Name 'SDL_DestroyWindow';
procedure SDL_Quit(); cdecl; external SDL3_Lib Name 'SDL_Quit';

implementation
end.
