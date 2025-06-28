program six;
var
  inputString: string;
  i: Integer;
begin
  Write('напишите предложение : ');
  ReadLn(inputString);
  Write('символы 3,6,9: ');
  for i := 3 to Length(inputString) do
  begin
    if (i mod 3 = 0) then 
    begin
      Write(inputString[i]);
    end;
  end;
  WriteLn; 
end.
