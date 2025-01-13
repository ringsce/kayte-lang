uses
  Kayte2PCE;

var
  KayteConverter: TKayte2PCE;
begin
  try
    KayteConverter := TKayte2PCE.Create('/path/to/pceas'); // Specify assembler path
    KayteConverter.ConvertKayteToROM('demo.kayte', 'output.pce'); // Source and output files
  finally
    KayteConverter.Free;
  end;
end.

