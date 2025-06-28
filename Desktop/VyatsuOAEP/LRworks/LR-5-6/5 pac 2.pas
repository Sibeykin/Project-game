program pac2;

uses crt;

const
  MAX_SIZE = 10;  

var
  arr: array[1..MAX_SIZE] of integer;
  count: array[1..MAX_SIZE] of integer; 
  n, i, maxCount, mostCommonNumber: integer;

begin
  clrscr;
  
  writeln('введите количество чисел для  массива (максимум ', MAX_SIZE, '): ');
  readln(n);
  
  if (n < 1) or (n > MAX_SIZE) then
  begin
    writeln('неверное количество элементов');
    exit;
  end;

  writeln('введите ', n, ' целых чисел:');
  for i := 1 to n do
  begin
    read(arr[i]);
    count[i] := 0; 
  end;

  for i := 1 to n do
  begin
    for var j := 1 to n do
    begin
      if arr[i] = arr[j] then
        count[i] := count[i] + 1; 
    end;
  end;

  maxCount := 0;
  mostCommonNumber := arr[1]; 

  for i := 1 to n do
  begin
    if count[i] > maxCount then
    begin
      maxCount := count[i]; 
      mostCommonNumber := arr[i];
    end;
  end;

  writeln('наиболее часто повторяющееся число: ', mostCommonNumber, ' (повторяеться ', maxCount, ' раз)');
  
  readln; 
end.
