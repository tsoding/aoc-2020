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


    function Tile_Hash(T: Tile) return Hash_Type is
        Result : Long_Integer := Long_Integer(T.X);
    begin
        Result := Result * 31 + Long_Integer(T.Y);
        Result := Result * 97 + Long_Integer(T.Z);
        return Hash_Type(Result mod 1_000_000_000);
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

    function Is_Black(F: in Floor.Map; T: Tile) return Boolean is
        C: Floor.Cursor := Floor.Find(F, T);
    begin
        return Floor.Has_Element(C) and then Floor.Element(C);
    end;

    function Count_Neighbours(F: in Floor.Map; T: Tile) return Integer is
        Result: Integer := 0;
    begin
        for Dir of Dirs loop
            if Is_Black(F, Tile_Add(T, Dir)) then
                Result := Result + 1;
            end if;
        end loop;
        return Result;
    end;

    procedure Set_Tile(F: in out Floor.Map; T: Tile; Value: Boolean) is
        C: Floor.Cursor := Floor.Find(F, T);
    begin
        if Floor.Has_Element(C) then
            Floor.Replace_Element(F, C, Value);
        else
            Floor.Insert(F, T, Value);
        end if;
    end;

    procedure Next_Floor(F1: in Floor.Map; F2: out Floor.Map) is
        T : Tile;
        Neighbours : Integer;
    begin
        Floor.Clear(F2);
        for C in Floor.Iterate(F1) loop
            for Dir of Dirs loop
                T := Tile_Add(Floor.Key(C), Dir);
                Neighbours := Count_Neighbours(F1, T);
                if Is_Black(F1, T) then
                    Set_Tile(F2, T, not (Neighbours = 0 or Neighbours > 2));
                else
                    Set_Tile(F2, T, Neighbours = 2);
                end if;
            end loop;
        end loop;
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

    procedure Floor_From_File(File_Path: String; F: out Floor.Map) is
        File : File_Type;
        T : Tile;
    begin
        Open(File => File,
             Mode => In_File,
             Name => File_Path);
        while not End_Of_File(File) loop
            T := Parse_Tile(To_Unbounded_String(Get_Line(File)));
            Flip_Tile(F, T);
        end loop;
        Close(File);
    end;

    function Part1(File_Path: String) return Integer is
        F : Floor.Map;
    begin
        Floor_From_File(File_Path, F);
        return Count_Black(F);
    end;

    function Part2(File_Path: String) return Integer is
        F : array (0..1) of Floor.Map;
        Current : Integer := 0;
    begin
        Floor_From_File(File_Path, F(Current));
        for i in 1..100 loop
            Next_Floor(F(Current), F(1 - Current));
            Current := 1 - Current;
        end loop;
        return Count_Black(F(Current));
    end;

    procedure Solve_File(File_Path: String) is
    begin
        Put_Line("Input file: " & File_Path);
        Put_Line("  Part 1:" & Integer'Image(Part1(File_Path)));
        Put_Line("  Part 2:" & Integer'Image(Part2(File_Path)));
    end Solve_File;

begin
    Put_Line("Amount of args: " & Integer'Image(Argument_Count));
    for Arg in 1..Argument_Count loop
        Solve_File(Argument(Arg));
    end loop;
end Day24;
