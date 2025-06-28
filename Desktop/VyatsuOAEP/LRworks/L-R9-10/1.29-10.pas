program ArrayElements;

const
  MAX_SIZE = 100; // Константа для максимального размера массива

var
  arr: array[1..MAX_SIZE] of Integer; // Глобальная переменная: массив целых чисел
  size: Integer; // Глобальная переменная: размер массива

procedure FindArrayElements(const a: array of Integer; len: Integer; var maxElem: Integer; var maxIndex: Integer; var minAbsElem: Integer; var maxNegativeElem: Integer);
var
  i: Integer; // Локальная переменная: индекс
begin
  maxElem := a[0]; // Инициализация максимального элемента
  maxIndex := 0;    // Инициализация индекса максимального элемента
  minAbsElem := a[0]; // Инициализация минимального по модулю элемента
  maxNegativeElem := -MaxInt; // Инициализация максимального отрицательного элемента

  for i := 0 to len - 1 do
  begin
    // Нахождение максимального элемента
    if a[i] > maxElem then
    begin
      maxElem := a[i];
      maxIndex := i; 
    end;

    // Нахождение минимального по модулю элемента
    if Abs(a[i]) < Abs(minAbsElem) then
      minAbsElem := a[i];

    // Нахождение максимального отрицательного элемента
    if (a[i] < 0) and (a[i] > maxNegativeElem) then
      maxNegativeElem := a[i];
  end;
end;

begin
  // Ввод размера массива
  Write('Введите количество элементов массива (1 до 100): ');
  ReadLn(size);
  
  // Ввод элементов массива
  WriteLn('Введите ', size, ' элементов:');
  for var i := 1 to size do
    Read(arr[i]);

  var maxElem, minAbsElem, maxNegativeElem, maxIndex: Integer;

  // Вызов процедуры для нахождения элементов
  FindArrayElements(arr, size, maxElem, maxIndex, minAbsElem, maxNegativeElem); // Формальные параметры = фактические параметры

  // Вывод результатов
  WriteLn('Максимальный элемент: ', maxElem);
  WriteLn('Индекс максимального элемента: ', maxIndex);
  WriteLn('Минимальный по модулю элемент: ', minAbsElem);
  WriteLn('Максимальный отрицательный элемент: ', maxNegativeElem);
end.
