program ManageArray;

uses crt;

const
    N = 20;

var
    arr: array[1..N] of Integer;
    i, j, minIndex: Integer;
    firstPositiveFound: Boolean;

begin
    Randomize;
    
    for i := 1 to N do
    begin
        arr[i] := Random(101) - 50;
    end;
    
    WriteLn('Исходный массив:');
    for i := 1 to N do
    begin
        Write(arr[i], ' ');
    end;
    WriteLn;

    firstPositiveFound := False;
    for i := 1 to N do
    begin
        if (not firstPositiveFound) and (arr[i] > 0) then
        begin
            for j := i to N-1 do
            begin
                arr[j] := arr[j+1];
            end;
            arr[N] := 0;
            firstPositiveFound := True;
            Break;
        end;
    end;

    if firstPositiveFound then

    WriteLn('Массив после удаления первого положительного элемента:');
    for i := 1 to N do
    begin
        Write(arr[i], ' ');
    end;
    WriteLn;

    minIndex := 1;
    for i := 2 to N do
    begin
        if arr[i] < arr[minIndex] then
            minIndex := i;
    end;

    for i := minIndex to N-1 do
    begin
        arr[i] := arr[i+1];
    end;

    arr[N] := 0; 

    WriteLn('Массив после удаления наименьшего элемента:');
    for i := 1 to N do
    begin
        Write(arr[i], ' ');
    end;
    WriteLn;

    ReadLn;
end.
