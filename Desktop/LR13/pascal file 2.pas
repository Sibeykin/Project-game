program lbrtrn13tsk3;
var inputFile, nchtnFile, chtnFile: Text; tkshnmr: Real; smvl: Integer;
begin
  Assign(inputFile, 'C:\PABCWork.NET\блокноты\3.txt');
  Reset(inputFile);
  Assign(nchtnFile, 'C:\PABCWork.NET\блокноты\3.1.txt');
  Rewrite(nchtnFile);
  Assign(chtnFile, 'C:\PABCWork.NET\блокноты\3.2.txt');
  Rewrite(chtnFile);
  smvl := 1;
  while not Eof(inputFile) do
  begin
    Readln(inputFile, tkshnmr);
    if smvl mod 2 <> 0 then
      Writeln(nchtnFile, tkshnmr)
    else
      Writeln(chtnFile, tkshnmr);
    Inc(smvl);
  end;
  Close(inputFile);
  Close(nchtnFile);
  Close(chtnFile);
  Writeln('гуд');
end.
