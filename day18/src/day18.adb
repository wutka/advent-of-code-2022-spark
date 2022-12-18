pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Day18 is
   type Coord_Range is range 0 .. 19;
   type Lava_Cube_Type is
      array (Coord_Range, Coord_Range, Coord_Range) of Boolean;

   type Coord is record
      X, Y, Z : Coord_Range := 0;
   end record;

   --  I added this because Gnatprove was hanging or crashing trying to prove
   --  my code with a bunch of nested loops. By generatings the coords
   --  serially I am able to get gnatprove to work
   type All_Coords_Range is range 0 .. 8000;
   type All_Coords_Array is array (All_Coords_Range) of Coord;

   procedure Generate_All_Coords (All_Coords : out All_Coords_Array)
   is
      Pos : All_Coords_Range;
      X, Y, Z : Coord_Range;
   begin
      All_Coords := (others => <>);
      X := 0;
      Y := 0;
      Z := 0;

      --  Generate the coords serially instead of a loop
      Pos := 0;
      loop
         All_Coords (Pos) := (X => X, Y => Y, Z => Z);
         if Pos < All_Coords_Range'Last then
            Pos := Pos + 1;
         else
            exit;
         end if;
         if X < Coord_Range'Last then
            X := X + 1;
         else
            X := 0;
            if Y < Coord_Range'Last then
               Y := Y + 1;
            else
               Y := 0;
               if Z < Coord_Range'Last then
                  Z := Z + 1;
               end if;
            end if;
         end if;
      end loop;
   end Generate_All_Coords;

   function Count_Exposed (X, Y, Z : Coord_Range;
      Lava_Cube : Lava_Cube_Type) return Natural
   with Post => Count_Exposed'Result <= 6
   is
      Count : Natural;
   begin
      Count := 0;

      if X = 0 or else (X > 0 and then not Lava_Cube (X - 1, Y, Z)) then
         Count := Count + 1;
      end if;

      if X = Coord_Range'Last or else
         (X < Coord_Range'Last and then not Lava_Cube (X + 1, Y, Z))
      then
         Count := Count + 1;
      end if;

      if Y = 0 or else (Y > 0 and then not Lava_Cube (X, Y - 1, Z)) then
         Count := Count + 1;
      end if;

      if Y = Coord_Range'Last or else
         (Y < Coord_Range'Last and then not Lava_Cube (X, Y + 1, Z))
      then
         Count := Count + 1;
      end if;

      if Z = 0 or else (Z > 0 and then not Lava_Cube (X, Y, Z - 1)) then
         Count := Count + 1;
      end if;

      if Z = Coord_Range'Last or else
         (Z < Coord_Range'Last and then not Lava_Cube (X, Y, Z + 1))
      then
         Count := Count + 1;
      end if;

      return Count;
   end Count_Exposed;

   procedure Compute_External_Air (Lava_Cube : Lava_Cube_Type;
      All_Coords : All_Coords_Array;
      External_Air : out Lava_Cube_Type)
   is
      Changed : Boolean;
      x, y, z : Coord_Range;
      c1, c2 : Coord_Range;
   begin
      External_Air := (others => (others => (others => False)));

      --  This is quite inefficient because it repeats these
      --  assignments for each Z coordinate, but it still runs
      --  quickly and this is only executed once
      --
      for i in All_Coords_Range loop
         c1 := All_Coords (i).X;
         c2 := All_Coords (i).Y;
         if not Lava_Cube (Coord_Range'First, c1, c2) then
            External_Air (Coord_Range'First, c1, c2) := True;
         end if;
         if not Lava_Cube (Coord_Range'Last, c1, c2) then
            External_Air (Coord_Range'Last, c1, c2) := True;
         end if;
         if not Lava_Cube (c1, Coord_Range'First, c2) then
            External_Air (c1, Coord_Range'First, c2) := True;
         end if;
         if not Lava_Cube (c1, Coord_Range'Last, c2) then
            External_Air (c1, Coord_Range'Last, c2) := True;
         end if;
         if not Lava_Cube (c1, c2, Coord_Range'First) then
            External_Air (c1, c2, Coord_Range'First) := True;
         end if;
         if not Lava_Cube (c1, c2, Coord_Range'Last) then
            External_Air (c1, c2, Coord_Range'Last) := True;
         end if;
      end loop;

      Changed := True;
      while Changed loop
         Changed := False;
         for i in All_Coords_Range loop
            x := All_Coords (i).X;
            y := All_Coords (i).Y;
            z := All_Coords (i).Z;

            if not Lava_Cube (x, y, z) and then
               not External_Air (x, y, z)
            then
               if x > 0 and then
                  not Lava_Cube (x - 1, y, z) and then
                  External_Air (x - 1, y, z)
               then
                  Changed := True;
                  External_Air (x, y, z) := True;
               end if;

               if x < Coord_Range'Last and then
                  not Lava_Cube (x + 1, y, z) and then
                  External_Air (x + 1, y, z)
               then
                  Changed := True;
                  External_Air (x, y, z) := True;
               end if;

               if y > 0 and then
                  not Lava_Cube (x, y - 1, z) and then
                  External_Air (x, y - 1, z)
               then
                  Changed := True;
                  External_Air (x, y, z) := True;
               end if;

               if y < Coord_Range'Last and then
                  not Lava_Cube (x, y + 1, z) and then
                  External_Air (x, y + 1, z)
               then
                  Changed := True;
                  External_Air (x, y, z) := True;
               end if;

               if z > 0 and then
                  not Lava_Cube (x, y, z - 1) and then
                  External_Air (x, y, z - 1)
               then
                  Changed := True;
                  External_Air (x, y, z) := True;
               end if;

               if z < Coord_Range'Last and then
                  not Lava_Cube (x, y, z + 1) and then
                  External_Air (x, y, z + 1)
               then
                  Changed := True;
                  External_Air (x, y, z) := True;
               end if;
            end if;
         end loop;
      end loop;
   end Compute_External_Air;

   function Count_External_Exposed (X, Y, Z : Coord_Range;
      Lava_Cube : Lava_Cube_Type; External_Air : Lava_Cube_Type)
      return Natural
      with Post => Count_External_Exposed'Result <= 6
   is
      Count : Natural;
   begin
      Count := 0;

      if X = 0 or else (X > 0 and then
         not Lava_Cube (X - 1, Y, Z) and then
         External_Air (X - 1, Y, Z))
      then
         Count := Count + 1;
      end if;

      if X = Coord_Range'Last or else
         (X < Coord_Range'Last and then
         not Lava_Cube (X + 1, Y, Z) and then
         External_Air (X + 1, Y, Z))
      then
         Count := Count + 1;
      end if;

      if Y = 0 or else (Y > 0 and then
         not Lava_Cube (X, Y - 1, Z) and then
         External_Air (X, Y - 1, Z))
      then
         Count := Count + 1;
      end if;

      if Y = Coord_Range'Last or else
         (Y < Coord_Range'Last and then
         not Lava_Cube (X, Y + 1, Z) and then
         External_Air (X, Y + 1, Z))
      then
         Count := Count + 1;
      end if;

      if Z = 0 or else (Z > 0 and then
         not Lava_Cube (X, Y, Z - 1) and then
         External_Air (X, Y, Z - 1))
      then
         Count := Count + 1;
      end if;

      if Z = Coord_Range'Last or else
         (Z < Coord_Range'Last and then
         not Lava_Cube (X, Y, Z + 1) and then
         External_Air (X, Y, Z + 1))
      then
         Count := Count + 1;
      end if;

      pragma Assert (Count <= 6);
      return Count;
   end Count_External_Exposed;

   Data_File : File_Type;
   Dummy : Character;
   Val : Integer;
   X, Y, Z : Coord_Range;

   Lava_Cube : Lava_Cube_Type;
   External_Air : Lava_Cube_Type;
   All_Coords : All_Coords_Array;

   Num_Sides : Natural;
   Num_B_Sides : Natural;
   Num_Exposed : Natural;

begin
   Open (File => Data_File,
         Mode => In_File,
--         Name => "test.txt");
         Name => "data/day18.txt");

   Lava_Cube := (others => (others => (others => False)));

   Generate_All_Coords (All_Coords);

   while not End_Of_File (Data_File) loop
      Get (Data_File, Val);
      if Val >= 0 and then Val <= Integer (Coord_Range'Last) then
         X := Coord_Range (Val);
      else
         Put_Line ("Invalid X" & Integer'Image (Val));
         return;
      end if;
      Get (Data_File, Dummy);

      Get (Data_File, Val);
      if Val >= 0 and then Val <= Integer (Coord_Range'Last) then
         Y := Coord_Range (Val);
      else
         Put_Line ("Invalid Y" & Integer'Image (Val));
         return;
      end if;
      Get (Data_File, Dummy);

      Get (Data_File, Val);
      if Val >= 0 and then Val <= Integer (Coord_Range'Last) then
         Z := Coord_Range (Val);
      else
         Put_Line ("Invalid Z - " & Integer'Image (Val));
         return;
      end if;

      Lava_Cube (X, Y, Z) := True;
   end loop;

   Num_Sides := 0;
   Num_B_Sides := 0;

   Compute_External_Air (Lava_Cube, All_Coords, External_Air);

   for i in All_Coords_Range loop
      X := All_Coords (i).X;
      Y := All_Coords (i).Y;
      Z := All_Coords (i).Z;
      if Lava_Cube (X, Y, Z) then
         Num_Exposed := Count_Exposed (X, Y, Z, Lava_Cube);
         if Natural'Last - Num_Exposed > Num_Sides then
            Num_Sides := Num_Sides + Num_Exposed;
         end if;
         Num_Exposed := Count_External_Exposed (X, Y, Z,
            Lava_Cube, External_Air);
         if Natural'Last - Num_Exposed > Num_B_Sides then
            Num_B_Sides := Num_B_Sides + Num_Exposed;
         end if;
      end if;
   end loop;

   Put_Line ("Part A count: " & Natural'Image (Num_Sides));
   Put_Line ("Part B count: " & Natural'Image (Num_B_Sides));
end Day18;
