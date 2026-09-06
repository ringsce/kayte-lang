unit kayte_sdl2;
interface

uses SysUtils;

const
{$IFDEF WINDOWS}
    SDL2_Lib = 'SDL2.dll';
  {$ELSE}
    {$IFDEF DARWIN}
      SDL2_Lib = 'libSDL2.dylib';
    {$ELSE}
      SDL2_Lib = 'libSDL2.so';
    {$ENDIF}
  {$ENDIF}

// Manually link the core SDL3 functions you need for the backend
function SDL_Init(flags: UInt32): Integer; cdecl; external SDL2_Lib Name 'SDL_Init';
function SDL_CreateWindow(title: PChar; w, h: Integer; flags: UInt64): Pointer; cdecl; external SDL2_Lib Name 'SDL_CreateWindow';
procedure SDL_DestroyWindow(window: Pointer); cdecl; external SDL2_Lib Name 'SDL_DestroyWindow';
procedure SDL_Quit(); cdecl; external SDL2_Lib Name 'SDL_Quit';

implementation
end.
