program WriteStars;

var
  N, K, i: Integer;
  f: TextFile;

begin
  Write('количество строк : ');
  ReadLn(N);
  Write('количество символов в строке : ');
  ReadLn(K);

  AssignFile(f, 'output.txt');
  Rewrite(f);

  for i := 1 to N do
  begin
    Write(f, StringOfChar('*', K)); // Запись строки из K символов '*'
    WriteLn(f);
  end;

  CloseFile(f);
end.
