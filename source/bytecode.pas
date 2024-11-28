unit Bytecode;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type
  TBytecodeGenerator = class
  private
    FBytecode: TMemoryStream;
    procedure EmitByte(ByteValue: Byte);
    //function ParseInstruction(const Line: string): Byte;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GenerateBytecode(const SourceFile: string; const OutputFile: string);
    function ParseInstruction(const Line: string): Byte; // <- Make this public

  end;

implementation

{ TBytecodeGenerator }

constructor TBytecodeGenerator.Create;
begin
  FBytecode := TMemoryStream.Create;
end;

destructor TBytecodeGenerator.Destroy;
begin
  FBytecode.Free;
  inherited Destroy;
end;

procedure TBytecodeGenerator.EmitByte(ByteValue: Byte);
begin
  FBytecode.Write(ByteValue, SizeOf(Byte));
end;

function TBytecodeGenerator.ParseInstruction(const Line: string): Byte;
begin
  // Very basic instruction parsing, extend this to support Kayte Lang instructions
  if Line = 'NOP' then
    Result := $00
  else if Line = 'LOAD' then
    Result := $01
  else if Line = 'ADD' then
    Result := $02
  else if Line = 'SUB' then
    Result := $03
  else if Line = 'HALT' then
    Result := $FF
  else
    raise Exception.Create('Unknown instruction: ' + Line);
end;

procedure TBytecodeGenerator.GenerateBytecode(const SourceFile: string; const OutputFile: string);
var
  Source: TStringList;
  Line: string;
  Instruction: Byte;
  I: Integer;
begin
  Source := TStringList.Create;
  try
    // Read the Kayte Lang source code
    Source.LoadFromFile(SourceFile);

    // Use a traditional for loop to iterate over the lines
    for I := 0 to Source.Count - 1 do
    begin
      Line := Trim(Source[I]);
      if Line = '' then Continue; // Skip empty lines

      Instruction := ParseInstruction(Line);
      EmitByte(Instruction);
    end;

    // Save the generated bytecode to a file
    FBytecode.SaveToFile(OutputFile);
    Writeln('Bytecode generated and saved to ', OutputFile);
  finally
    Source.Free;
  end;
end;

end.
