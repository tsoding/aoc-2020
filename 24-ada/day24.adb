with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with System.Assertions; use System.Assertions;
with Ada.Containers.Hashed_Maps; use Ada.Containers;

procedure Day24 is

    type Tile is record
        X : Integer;
        Y : Integer;
        Z : Integer;
    end record;

    function Tile_Hash(T: Tile) return Hash_Type is
    begin
        return Hash_Type(T.X + T.Y + T.Z);
    end;

    package Floor is new Ada.Containers.Hashed_Maps
        (Key_Type => Tile,
         Element_Type => Boolean,
         Hash => Tile_Hash,
         Equivalent_Keys => "=");

    procedure Flip_Tile(F: in out Floor.Map; T: Tile) is
        D: Floor.Cursor := Floor.Find(F, T);
    begin
        if not Floor.Has_Element(D) then
            Floor.Insert(F, T, True);
        else
            Floor.Replace_Element(F, D, not Floor.Element(D));
        end if;
    end;

    function Count_Black(F: in Floor.Map) return Integer is
        C: Floor.Cursor := Floor.First(F);
        Result: Integer := 0;
    begin
        while Floor.Has_Element(C) loop
            if Floor.Element(C) then
                Result := Result + 1;
            end if;
            C := Floor.Next(C);
        end loop;
        return Result;
    end;

    function Tile_Image(T: Tile) return String is
    begin
        return "(" & Integer'Image(T.X) & ", " & Integer'Image(T.Y) & ", " & Integer'Image(T.Z) & ")";
    end;

    function Tile_Add(T1: Tile; T2: Tile) return Tile is
    begin
        return (T1.X + T2.X, T1.Y + T2.Y, T1.Z + T2.Z);
    end;

    Dir_Names : constant array (1..6) of Unbounded_String := (
        To_Unbounded_String("e"), 
        To_Unbounded_String("se"), 
        To_Unbounded_String("sw"), 
        To_Unbounded_String("w"),
        To_Unbounded_String("nw"),
        To_Unbounded_String("ne")
    );

    Dirs : constant array (1..6) of Tile := (
        (X =>  1, Y => -1, Z =>  0), --("e"), 
        (X =>  0, Y => -1, Z =>  1), --("se"), 
        (X => -1, Y =>  0, Z =>  1), --("sw"), 
        (X => -1, Y =>  1, Z =>  0), --("w"),
        (X =>  0, Y =>  1, Z => -1), --("nw"),
        (X =>  1, Y =>  0, Z => -1)  --("ne")
    );

    function Dir_To_Delta(Dir_Name: Unbounded_String) return Tile is
    begin
        for Index in Dir_Names'Range loop
            if Dir_Name = Dir_Names(Index) then
                return Dirs(Index);
            end if;
        end loop;
        Raise_Assert_Failure("Unknown direction `" & To_String(Dir_Name) & "`");
    end Dir_To_Delta;

    function Next_Dir(Desc: Unbounded_String) return Unbounded_String is
    begin
        for Dir of Dir_Names loop
            if Length(Dir) <= Length(Desc) then
                if Slice(Desc, 1, Length(Dir)) = Dir then
                    return Dir;
                end if;
            end if;
        end loop;

        Raise_Assert_Failure("Unreachable. Could not get the next direction");
    end Next_Dir;

    function Parse_Tile(Desc: Unbounded_String) return Tile is
        Result : Tile := (X => 0, Y => 0, Z => 0);
        Dir: Unbounded_String;
        Input: Unbounded_String := Desc;
    begin
        while Length(Input) > 0 loop
            Dir := Next_Dir(Input);
            Input := Unbounded_Slice(Input, Length(Dir) + 1, Length(Input));
            Result := Tile_Add(Result, Dir_To_Delta(Dir));
        end loop;

        return Result;
    end Parse_Tile;

    procedure Solve_File(File_Path: String) is
        File : File_Type;
        T : Tile;
        F : Floor.Map;
    begin
        Put_Line("Input file: " & File_Path);
        Open(File => File,
             Mode => In_File,
             Name => File_Path);
        while not End_Of_File(File) loop
            T := Parse_Tile(To_Unbounded_String(Get_Line(File)));
            Flip_Tile(F, T);
        end loop;
        Put_Line(Integer'Image(Count_Black(F)));
    end Solve_File;

begin
    Put_Line("Amount of args: " & Integer'Image(Argument_Count));
    for Arg in 1..Argument_Count loop
        Solve_File(Argument(Arg));
    end loop;
end Day24;
