program fore;
var
  text, result: string;
begin
  writeln('напиши предложения:');
  readln(text);
  if Copy(text, 1, 3) = 'abc' then
    result := 'www' + Copy(text, 4, Length(text) - 3)
  else
    result := text + 'zzz';
  writeln('результат вашего ввода : ', result);
end.
