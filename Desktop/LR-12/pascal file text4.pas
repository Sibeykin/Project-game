program InsertEmptyLineBeforeK;

var
  K, lineNumber: Integer;
  currentLine: string;
  inputFile, tempFile: TextFile;
  lineExists: Boolean;

begin
  Write('Введите номер строки K: ');
  ReadLn(K);

  AssignFile(inputFile, 'input.txt');
  AssignFile(tempFile, 'temp.txt');

  Reset(inputFile);
  Rewrite(tempFile);
  lineNumber := 0;
  lineExists := False;

  while not Eof(inputFile) do
  begin
    ReadLn(inputFile, currentLine);
    Inc(lineNumber);

    if lineNumber = K then
    begin
      lineExists := True;
      WriteLn(tempFile);
    end;

    WriteLn(tempFile, currentLine);
  end;

  CloseFile(inputFile);
  CloseFile(tempFile);

  if not lineExists then
  begin
    Erase(tempFile);
  end
  else
  begin
    Erase(inputFile);
    Rename(tempFile, 'input.txt');
  end;
end.
