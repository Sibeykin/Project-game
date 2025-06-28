program FindElements;

procedure FindArrayElements(arr: array of Integer; var maxElement, maxIndex, minAbsoluteElement, maxNegativeElement: Integer);
var
  i: Integer;
begin
  maxElement := arr[0];
  maxIndex := 0;
  minAbsoluteElement := arr[0];
  maxNegativeElement := -MaxInt;

  for i := 0 to High(arr) do
  begin
    if arr[i] > maxElement then
    begin
      maxElement := arr[i];
      maxIndex := i;
    end;

    if abs(arr[i]) < abs(minAbsoluteElement) then
    begin
      minAbsoluteElement := arr[i];
    end;

    if (arr[i] < 0) and (arr[i] > maxNegativeElement) then
    begin
      maxNegativeElement := arr[i];
    end;
  end;
end;

var
  numbers: array of Integer;
  maxElement, maxIndex, minAbsoluteElement, maxNegativeElement: Integer;
begin
  SetLength(numbers, 7);
  numbers[0] := 3;
  numbers[1] := -5;
  numbers[2] := 1;
  numbers[3] := 2;
  numbers[4] := -4;
  numbers[5] := 6;
  numbers[6] := 0;

  FindArrayElements(numbers, maxElement, maxIndex, minAbsoluteElement, maxNegativeElement);

  WriteLn('Максимальный элемент: ', maxElement, ' Индекс: ', maxIndex);
  WriteLn('Минимальный по модулю элемент: ', minAbsoluteElement);
  WriteLn('Максимальный отрицательный элемент: ', maxNegativeElement);

  ReadLn;
end.
