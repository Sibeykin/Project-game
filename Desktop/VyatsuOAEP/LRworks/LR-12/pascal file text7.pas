program SumWithFiveDivisors;

var
  n, p, p4, sum: Int64;
  inputFile, outputFile: TextFile;

function IsPrime(num: Int64): Boolean;
var
  i: Int64;
begin
  if num < 2 then
    Exit(False);
  for i := 2 to Trunc(Sqrt(num)) do
  begin
    if num mod i = 0 then
      Exit(False);
  end;
  Exit(True);
end;

begin
  // Инициализация переменной суммы
  sum := 0;

  // Чтение входных данных
  AssignFile(inputFile, 'z7.txt');
  Reset(inputFile);
  ReadLn(inputFile, n);
  CloseFile(inputFile);

  p := 2;

  // Поиск всех простых p, таких что p^4 <= n
  while True do
  begin
    if IsPrime(p) then
    begin
      p4 := p * p * p * p; // p^4
      if p4 > n then
        Break;
      sum := sum + p4;
    end;
    Inc(p);
  end;

  // Запись результата в выходной файл
  AssignFile(outputFile, 'z3.out');
  Rewrite(outputFile);
  WriteLn(outputFile, sum);
  CloseFile(outputFile);
end.
