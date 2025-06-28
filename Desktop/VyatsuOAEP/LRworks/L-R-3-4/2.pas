program twoo;

var
  N, i: integer;
  length, width, area, totalArea: real;

begin
  write('Введите количество комнат в доме: ');
  readln(N);

  totalArea := 0;

  for i := 1 to N do
  begin
    writeln('Комната ', i, ':');

    write('  Введите длину: ');
    readln(length);
    write('  Введите ширину: ');
    readln(width);

    area := length * width;

    writeln('  Площадь комнаты: ', Format('%8.2f', [area]), ' кв.м');

    totalArea := totalArea + area;
  end;

  writeln('Суммарная площадь комнат в доме: ', Format('%8.2f', [totalArea]), ' кв.м');
end.
