program one;

var
  A, B, H, x, y: real;

begin
  writeln('Введите начальное значение x (A):');
  readln(A);
  writeln('Введите конечное значение x (B):');
  readln(B);
  writeln('Введите шаг изменения x (H):');
  readln(H);

  writeln('x      y');
  writeln('-----------------');

  x := A;
  while x <= B do
  begin
    y := 3 * x * x + 5 * x / 13;
    writeln(Format('%6.2f', [x]), '   ', Format('%8.2f', [y]));
    x := x + H;
  end;
end.
