program Aoc2020Day15;

uses SysUtils;

const
    Capacity = 30 * 1000 * 1000 + 10; 

var
    input : array[0..0] of String = (
    {'0,3,6', 
        '1,3,2',
        '2,1,3',
        '1,2,3',
        '2,3,1',
        '3,2,1',
        '3,1,2',
        }
        '6,3,15,13,1,0'
    );
    sample : array of Int64;
    hashtable : array of Int64;
    i, j, sampleSize : Int64;

procedure SavePrevPos(value: Int64; pos: Int64);
begin
    Assert(Value < Capacity);
    hashtable[value] := pos;
end;

function GetPrevPos(value: Int64): Int64;
begin
    Assert(Value < Capacity);
    GetPrevPos := hashtable[Value];
end;

{
function FindPrev(size, prev: Int64): Int64;
begin
   for i := size - 1 downto 0 do
      if sample[i] = prev then
         Exit(i);
   Exit(-1);
end;
}

procedure SolveSample(n: Int64);
var
    prevIndex : Int64;
begin
    while sampleSize < n do
    begin
        prevIndex := GetPrevPos(sample[sampleSize - 1]);
        SavePrevPos(sample[sampleSize - 1], sampleSize - 1);
        if prevIndex < 0 then
            sample[sampleSize] := 0
        else
            sample[sampleSize] := sampleSize - (prevIndex + 1);

        inc(sampleSize);
    end;

    WriteLn(n, 'th: ', sample[sampleSize - 1]);
end;

procedure InitSample(input : String);
var
    start, finish : Int64;
begin
    start := 1;
    finish := 1;
    sampleSize := 0;
    while start <= Length(input) do
    begin
        while (finish <= Length(input)) and (input[finish] <> ',') do
        begin
            inc(finish);
        end;
        
        if sampleSize > 0 then 
            SavePrevPos(sample[sampleSize - 1], sampleSize - 1);

        sample[sampleSize] := StrToInt64(copy(input, start, finish - start));
        inc(sampleSize);

        start := finish + 1;
        finish := start;
    end;
end;

begin
    setLength(sample, Capacity);
    setLength(hashtable, Capacity);
    for i := 0 to Length(input) - 1 do
    begin
        WriteLn('Sample: ', input[i]);
        for j := 0 to Capacity - 1 do hashtable[j] := -1;
        InitSample(input[i]);
        {SolveSample(2020);}
        SolveSample(30 * 1000 * 1000);
    end;
end.
