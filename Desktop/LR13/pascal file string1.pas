var inputFile, outputFile: Text; currentChar: Char; fileContent: string; lastSpacePos: Integer;
begin
  Assign(inputFile, 'C:\PABCWork.NET\блокноты\b1.txt  ');
  Assign(outputFile, 'C:\PABCWork.NET\блокноты\b1.2.txt');
  Reset(inputFile);
  Rewrite(outputFile);
  fileContent := '';
  while not Eof(inputFile) do
  begin
    Read(inputFile, currentChar);
    fileContent := fileContent + currentChar;
  end;
  lastSpacePos := LastPos(' ', fileContent  );
  if lastSpacePos > 0 then
    fileContent := Copy(fileContent, 1, lastSpacePos - 1);
  Write(outputFile, fileContent);
  Close(inputFile);
  Close(outputFile);
  Writeln('Изменения завершены. Проверьте файл b1.2.txt.');
end.
