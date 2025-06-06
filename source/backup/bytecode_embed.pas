unit bytecode_embed;

{$mode objfpc}{$H+}

interface

const
  BytecodeData: array[0..0] of Byte; external name '_binary_bytecode_bin_start';

  BytecodeSize: PtrUInt = 0;

implementation

end.

