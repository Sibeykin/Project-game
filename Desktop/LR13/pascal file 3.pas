program lbrtrn13tsk4;
var  f: Text;   nmr, smm: Real;   chsl: Integer; 
begin
  smm := 0;
  chsl := 0;
  Assign(f, 'C:\PABCWork.NET\блокноты\4.txt'); 
  Reset(f);
  while not Eof(f) do
  begin
    Readln(f, nmr); 
    Inc(chsl); 
    if chsl mod 2 = 0 then 
      smm := smm + nmr; 
  end;
  Close(f);
  Writeln('сумма элементов с четными номерами: ', smm:0:2);
end.

