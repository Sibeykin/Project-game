program LugoPasetsya;

var
  n: integer;
  ending: string;

begin
  write('Введите число n (n<100): ');
  readln(n);

  if (n mod 10 = 1) and (n mod 100 <> 11) then
    ending := 'korova'
  else if ((n mod 10 >= 2) and (n mod 10 <= 4)) and ((n mod 100 < 10) or (n mod 100 >= 20)) then
    ending := 'korovy'
  else
    ending := 'korov';

  writeln('На лугу пасется ', n, ' ', ending);
end.

