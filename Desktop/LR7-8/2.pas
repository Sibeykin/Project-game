program two;
var
  text: string;
  digit_count, i: integer;
begin
  writeln('напишите предложение состоящие токо из числе:');
  readln(text);
  digit_count := 0; 
  for i := 1 to Length(text) do
    if text[i] in ['0'..'9'] then
      digit_count := digit_count + 1;
  
  writeln('количество цифр в строке: ', digit_count);
end.
