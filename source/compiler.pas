unit Compiler;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Lexer, Parser, Assembler, VirtualMachine,
  TokenDefs;

type
  TCompiler = class
  private
    FSourceCode: TStringList;
    FCommentFreeSource: TStringList;
    FCompilerSuccess: Boolean;

    function StripComments(const ASource: TStringList): TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function Compile(const ASourceFile: string): Boolean;
    function CompileSource(const ASource: TStringList): TStringList;

  end;

implementation

{ TCompiler }

function TCompiler.StripComments(const ASource: TStringList): TStringList;
var
  S: string;
  ResultList: TStringList;
  I: Integer;
begin
  ResultList := TStringList.Create;
  for I := 0 to ASource.Count - 1 do
  begin
    S := ASource[I];
    // Find the first occurrence of a single-line comment marker
    if Pos('//', S) > 0 then
    begin
      // Extract the part of the string before the comment
      S := Copy(S, 1, Pos('//', S) - 1);
    end;
    // Add the line to the result list only if it's not empty
    if Trim(S) <> '' then
    begin
      ResultList.Add(S);
    end;
  end;
  Result := ResultList;
end;

constructor TCompiler.Create;
begin
  inherited Create;
  FSourceCode := TStringList.Create;
  FCommentFreeSource := TStringList.Create;
  FCompilerSuccess := False;
end;

destructor TCompiler.Destroy;
begin
  FSourceCode.Free;
  FCommentFreeSource.Free;
  inherited Destroy;
end;

function TCompiler.Compile(const ASourceFile: string): Boolean;
var
  Lexer: TLexer;
  Parser: TParser;
  Assembler: TAssembler;
begin
  Result := False;
  FCompilerSuccess := False;

  if not FileExists(ASourceFile) then
  begin
    Writeln('Error: Input file not found: ', ASourceFile);
    Exit;
  end;

  // Load source and strip comments before compiling
  FSourceCode.LoadFromFile(ASourceFile);
  FCommentFreeSource.Assign(StripComments(FSourceCode));

  Writeln('Compiling ', ASourceFile, '...');

  Lexer := TLexer.Create(FCommentFreeSource);
  try
    Parser := TParser.Create(Lexer);
    try
      Assembler := TAssembler.Create;
      try
        // Parse the code and generate bytecode
        Parser.ParseProgram(Assembler);
        FCompilerSuccess := True;
        Result := True;
        // The bytecode is now in Assembler.Code
      finally
        Assembler.Free;
      end;
    finally
      Parser.Free;
    end;
  finally
    Lexer.Free;
  end;
end;

function TCompiler.CompileSource(const ASource: TStringList): TStringList;
var
  Lexer: TLexer;
  Parser: TParser;
  Assembler: TAssembler;
begin
  Result := TStringList.Create;

  // First, strip comments from the source
  FCommentFreeSource.Assign(StripComments(ASource));

  Lexer := TLexer.Create(FCommentFreeSource);
  try
    Parser := TParser.Create(Lexer);
    try
      Assembler := TAssembler.Create;
      try
        Parser.ParseProgram(Assembler);
        Result.Assign(Assembler.Code);
      finally
        Assembler.Free;
      end;
    finally
      Parser.Free;
    end;
  finally
    Lexer.Free;
  end;
end;

end.

