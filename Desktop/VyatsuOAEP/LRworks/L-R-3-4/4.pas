program 4;

var
  numJudges: integer;
  scores: array of real;
  maxScore, minScore, finalScore: real;
  i: integer;

begin
  repeat
    write('Введите количество судей (не менее 3): ');
    readln(numJudges);
  until numJudges >= 3;

  SetLength(scores, numJudges);
  for i := 1 to numJudges do
  begin
    write('Введите оценку судьи ', i, ': ');
    readln(scores[i - 1]);
  end;

  maxScore := scores[0];
  minScore := scores[0];
  for i := 1 to numJudges - 1 do
  begin
    if scores[i] > maxScore then
      maxScore := scores[i];
    if scores[i] < minScore then
      minScore := scores[i];
  end;

  finalScore := 0;
  for i := 0 to numJudges - 1 do
  begin
    if (scores[i] <> maxScore) and (scores[i] <> minScore) then
      finalScore := finalScore + scores[i];
  end;
  finalScore := finalScore / (numJudges - 2);

  writeln('Итоговая оценка спортсмена: ', Format('%6.2f', [finalScore]));
end.
