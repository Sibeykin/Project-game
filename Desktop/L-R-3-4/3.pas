 program StudentScores;

var
  scores: array [1..4] of real;
  failedStudents: integer;
  totalScore, averageScore: real;
  numStudents: integer;

begin
  failedStudents := 0;
  totalScore := 0;
  numStudents := 0;

  while true do
  begin
    writeln('Студент ', numStudents + 1, ':');

    for var i := 1 to 4 do
    begin
      write('  Оценка ', i, ': ');
      readln(scores[i]);
      if scores[i] = 0 then
        break;
    end;

    if scores[1] = 0 then
      break;

    if (scores[1] < 3) or (scores[2] < 3) or (scores[3] < 3) or (scores[4] < 3) then
      failedStudents := failedStudents + 1;

    totalScore := totalScore + scores[1] + scores[2] + scores[3] + scores[4];

    numStudents := numStudents + 1;
  end;

  averageScore := totalScore / (numStudents * 4);

  writeln('Количество неуспевающих студентов: ', failedStudents);
  writeln('Средний балл группы: ', Format('%6.2f', [averageScore]));
end.
