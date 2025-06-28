program FindMinMax;

var
  inputFile, outputFile: TextFile;
  number, minNumber, maxNumber: Integer;
  firstLine: Boolean;

begin
  AssignFile(inputFile, 'input.txt');
  AssignFile(outputFile, 'output.txt');

  Reset(inputFile);
  firstLine := True;

  while not Eof(inputFile) do
  begin
    ReadLn(inputFile, number);

    if firstLine then
    begin
      minNumber := number;
      maxNumber := number;
      firstLine := False;
    end
    else
    begin
      if number < minNumber then
        minNumber := number;
      if number > maxNumber then
        maxNumber := number;
    end;
  end;

  CloseFile(inputFile);

  Rewrite(outputFile);
  WriteLn(outputFile, 'Минимальное число: ', minNumber);
  WriteLn(outputFile, 'Максимальное число: ', maxNumber);
  CloseFile(outputFile);
end.
