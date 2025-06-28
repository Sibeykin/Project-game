program Pack1;

const
  N = 10;

type
  TArray = array[1..N] of integer;

{ Процедура для нахождения максимального элемента массива и его индекса }
procedure FindMax(var arr: TArray; var maxValue: integer; var index: integer);
var
  i: integer; { Локальная переменная }
begin
  maxValue := arr[1]; { Инициализация максимального значения первым элементом }
  index := 1; { Инициализация индекса }
  
  for i := 2 to N do
  begin
    if arr[i] > maxValue then
    begin
      maxValue := arr[i]; { Обновляем максимальное значение }
      index := i; { Обновляем индекс }
    end;
  end;
end;

{ Процедура для нахождения минимального по модулю и максимального отрицательного элемента }
procedure FindMinMax(var arr: TArray; var minAbsValue: integer; var indexMinAbs: integer; var maxNegativeValue: integer);
var
  i: integer; { Локальная переменная }
begin
  if arr[1] < 0 then
    maxNegativeValue := arr[1] { Инициализация максимального отрицательного элемента }
  else
    maxNegativeValue := -MaxInt; { Начальное значение для максимального отрицательного числа }

  minAbsValue := Abs(arr[1]); { Инициализация минимального по модулю значения }
  indexMinAbs := 1; { Инициализация индекса для минимального по модулю }

  for i := 2 to N do
  begin
    { Проверка на минимальный по модулю элемент }
    if Abs(arr[i]) < minAbsValue then
    begin
      minAbsValue := Abs(arr[i]);
      indexMinAbs := i;
    end;

    { Проверка на максимальный отрицательный элемент }
    if (arr[i] < 0) and (arr[i] > maxNegativeValue) then
      maxNegativeValue := arr[i];
  end;
end;

{ Процедура формирования массива и суммирования положительных элементов }
procedure CreateArray(var arr: TArray);
var
  i: integer; { Локальная переменная }
begin
  for i := 1 to N do
    arr[i] := Random(21) - 10; { Заполнение массива случайными числами от -10 до 10 }
end;

{ Глобальные переменные }
var
  array1: TArray;
  maxValue, indexMax: integer;
  minAbsValue, indexMinAbs, maxNegativeValue: integer;

begin
  Randomize;

  { Создание и заполнение массива }
  CreateArray(array1);

  { Нахождение максимального элемента }
  FindMax(array1, maxValue, indexMax);
  WriteLn('макс элемент: ', maxValue, ' по счету  ', indexMax);

  { Нахождение минимального по модулю и максимального отрицательного элемента }
  FindMinMax(array1, minAbsValue, indexMinAbs, maxNegativeValue);
  WriteLn('мин элемент: ', minAbsValue, ' по счету  ', indexMinAbs);
  WriteLn('макси отриц элемент: ', maxNegativeValue);

  { Вывод массива }
  WriteLn('массив:');
  for indexMax := 1 to N do
    Write(array1[indexMax], ' ');
  WriteLn;
end.
