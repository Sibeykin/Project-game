program FruitSales;

var
  apple_price, pear_price, orange_price: integer;
  x1, y1, z1, x2, y2, z2: integer;
  monday_revenue, tuesday_revenue, total_revenue: real;

begin
  // Стоимость фруктов за штуку
  apple_price := 5;
  pear_price := 7;
  orange_price := 8;

  write('Введите количество проданных яблок в понедельник: ');
  readln(x1);
  write('Введите количество проданных груш в понедельник: ');
  readln(y1);
  write('Введите количество проданных апельсинов в понедельник: ');
  readln(z1);

  write('Введите количество проданных яблок во вторник: ');
  readln(x2);
  write('Введите количество проданных груш во вторник: ');
  readln(y2);
  write('Введите количество проданных апельсинов во вторник: ');
  readln(z2);

  monday_revenue := x1 * apple_price + y1 * pear_price + z1 * orange_price;

  tuesday_revenue := x2 * apple_price + y2 * pear_price + z2 * orange_price;

  total_revenue := monday_revenue + tuesday_revenue;

  writeln('Выручка в понедельник: ', monday_revenue:0:2, ' рублей');
  writeln('Выручка во вторник: ', tuesday_revenue:0:2, ' рублей');
  writeln('Общая выручка за два дня: ', total_revenue:0:2, ' рублей');

end.
