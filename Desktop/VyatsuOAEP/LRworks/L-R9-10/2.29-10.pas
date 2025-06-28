program ArrayExample;

const
  ROWS = 8;   
  COLS = 6;   

type
  T2DArray = array[1..ROWS, 1..COLS] of Integer;
  T1DArray = array[1..ROWS] of Integer;

var
  arr2D: T2DArray;  
  arr1D: T1DArray;   
  hasNegative: Boolean;

procedure Create2DArray(var arr: T2DArray);
var
  i, j: Integer;
begin
  Randomize;
  for i := 1 to ROWS do
    for j := 1 to COLS do
      arr[i, j] := Random(21) - 15;  
end;

procedure ProcessArray(var arr2D: T2DArray; var arr1D: T1DArray);
var
  i, j: Integer;
  hasNegative: Boolean;
begin
  for i := 1 to ROWS do
  begin
    hasNegative := False;
    for j := 1 to COLS do
    begin
      if arr2D[i, j] < 0 then
      begin
        hasNegative := True;
        Break;  
      end;
    end;
    if hasNegative then
      arr1D[i] := -1
    else
      arr1D[i] := 1;
  end;
end;

procedure Print2DArray(var arr: T2DArray);
var
  i, j: Integer;
begin
  for i := 1 to ROWS do
  begin
    for j := 1 to COLS do
      Write(arr[i, j]:4);
    Writeln;
  end;
end;

procedure Print1DArray(var arr: T1DArray);
var
  i: Integer;
begin
  for i := 1 to ROWS do
    Write(arr[i]:4);
  Writeln;
end;

begin
  Create2DArray(arr2D);
  
  WriteLn('Двумерный массив:');
  Print2DArray(arr2D);
  
  ProcessArray(arr2D, arr1D);
  
  WriteLn('Одномерный массив:');
  Print1DArray(arr1D);
  
  ReadLn;
end.