program AppendStringToFile;
var
  S: string;
  f: TextFile;

begin
  Write('Введите строку S: ');
  ReadLn(S);

  AssignFile(f, 'output.txt');
  Append(f); 

  WriteLn(f, S);

  CloseFile(f);
end.
