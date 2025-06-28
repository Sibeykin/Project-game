program RaznostChisel;

var
  num, a, b, c, reverse_num, difference: integer;

begin

  write('Введите трехзначное число: ');
  readln(num);

  a := num div 100;
  b := (num div 10) mod 10;
  c := num mod 10;

  reverse_num := c * 100 + b * 10 + a;

  difference := num - reverse_num;

  writeln('Разность между числом и числом, составленным из тех же цифр в обратном порядке: ', difference);
end.
