program ArraysMultiply;

uses crt;

const
  N = 5;

var
  array1, array2: array[1..N] of Integer;
  i: Integer;
  sumPos1, sumPos2: Integer;

begin
  clrscr;

  Writeln('напишите числа для 1 массива:');
  for i := 1 to N do
  begin
    Write('числа [', i, ']: ');
    Readln(array1[i]);
  end;

  Writeln('напишите  числа для 2 массива:');
  for i := 1 to N do
  begin
    Write('числа [', i, ']: ');
    Readln(array2[i]);
  end;

  sumPos1 := 0;
  for i := 1 to N do
  begin
    if array1[i] > 0 then
      sumPos1 := sumPos1 + array1[i];
  end;

  sumPos2 := 0;
  for i := 1 to N do
  begin
    if array2[i] > 0 then
      sumPos2 := sumPos2 + array2[i];
  end;

  Writeln('массив 1:');
  for i := 1 to N do
    Write(array1[i], ' ');
  Writeln;

  Writeln('массив 2:');
  for i := 1 to N do
    Write(array2[i], ' ');
  Writeln;

  if sumPos1 < sumPos2 then
  begin
    Writeln('cумма 1 массива меньше.');
    for i := 1 to N do
      array1[i] := array1[i] * 10;
  end
  else
  begin
    Writeln('сумма 2 массива меньше или равна.');
    for i := 1 to N do
      array2[i] := array2[i] * 10;
  end;

  Writeln('умноженный массив:');
  if sumPos1 < sumPos2 then
  begin
    for i := 1 to N do
      Write(array1[i], ' ');
  end
  else
  begin
    for i := 1 to N do
      Write(array2[i], ' ');
  end;

  Readln;
end.
