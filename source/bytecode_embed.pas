unit bytecode_embed;

{$mode objfpc}{$H+}

interface

const
  BytecodeData: PByte = PByte(@_binary_bytecode_bin_start);
  BytecodeSize: PtrUInt = PtrUInt(@_binary_bytecode_bin_end) - PtrUInt(@_binary_bytecode_bin_start);

var
  _binary_bytecode_bin_start: Byte; external name '_binary_bytecode_bin_start';
  _binary_bytecode_bin_end: Byte; external name '_binary_bytecode_bin_end';

implementation

end.

