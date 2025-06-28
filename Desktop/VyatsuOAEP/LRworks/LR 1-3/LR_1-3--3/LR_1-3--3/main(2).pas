program VisokosnyiGod;

var
  year: integer;

begin
  write('Введите год: ');
  readln(year);

  if (year mod 4 = 0) and ((year mod 100 <> 0) or (year mod 400 = 0)) then
    writeln(year, ' - високосный год')
  else
    writeln(year, ' - не високосный год');
end.

