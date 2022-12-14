pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Formal_Ordered_Sets;

procedure Day9 is
   type Pos_Val is range -10000 .. 10000;
   type Tails_Range is range 1 .. 9;

   type Position is record
      X : Pos_Val := 0;
      Y : Pos_Val := 0;
   end record;

   function Pos_Less (A, B : Position) return Boolean;

   package Position_Set is new Formal_Ordered_Sets (
      Element_Type => Position, "<" => Pos_Less);
   use Position_Set;

   function Pos_Less (A, B : Position) return Boolean is
   begin
      return (A.Y < B.Y) or else (A.Y = B.Y and then A.X < B.X);
   end Pos_Less;

   function Is_Adjacent (A, B : Position) return Boolean;

   function Is_Adjacent (A, B : Position) return Boolean
   is
   begin
      return (abs (Integer (A.X) - Integer (B.X)) <= 1)
         and then (abs (Integer (A.Y) - Integer (B.Y)) <= 1);
   end Is_Adjacent;

   Data_File : File_Type;
   Dir, Dummy : Character;
   N : Integer;
   Num_Steps : Natural;
   Head_Pos, Prev_Head_Pos : Position;
   Tail_Pos : array (Tails_Range) of Position;
   Num_Visited : Natural;
   Num_Visited_10 : Natural;
   Visited : Set (40000);
   Visited_10 : Set (40000);
   New_X_Pos : Pos_Val;
   New_Y_Pos : Pos_Val;

begin
   Head_Pos := (0, 0);
   Prev_Head_Pos := (0, 0);
   Tail_Pos := (others => (0, 0));

   if not Contains (Visited, Head_Pos) then
      Insert (Visited, Head_Pos);
   end if;

   if not Contains (Visited, Head_Pos) then
      Insert (Visited_10, Head_Pos);
   end if;

   Num_Visited := 1;
   Num_Visited_10 := 1;

   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day9.txt");

   while not End_Of_File (Data_File) loop
      Get (Data_File, Dir);
      Get (Data_File, Dummy);
      Get (Data_File, N);

      if N >= 0 then
         Num_Steps := N;
      else
         Put_Line ("Invalid number of steps");
         exit;
      end if;

      for i in 1 .. Num_Steps loop
         case Dir is
            when 'L' =>
               if Head_Pos.X > Pos_Val'First then
                  Head_Pos := (X => Head_Pos.X - 1, Y => Head_Pos.Y);
               else
                  Put_Line ("Head ran off left boundary");
                  return;
               end if;
            when 'R' =>
               if Head_Pos.X < Pos_Val'Last then
                  Head_Pos := (X => Head_Pos.X + 1, Y => Head_Pos.Y);
               else
                  Put_Line ("Head ran off right boundary");
                  return;
               end if;
            when 'U' =>
               if Head_Pos.Y < Pos_Val'Last then
                  Head_Pos := (X => Head_Pos.X, Y => Head_Pos.Y + 1);
               else
                  Put_Line ("Head ran off top boundary");
                  return;
               end if;
            when 'D' =>
               if Head_Pos.Y > Pos_Val'First then
                  Head_Pos := (X => Head_Pos.X, Y => Head_Pos.Y - 1);
               else
                  Put_Line ("Head ran off bottom boundary");
                  return;
               end if;
            when others =>
               Put_Line ("Unexpected direction - " & Character'Image (Dir));
         end case;

         for j in Tails_Range loop
            --  First tail just follows the head's previous position
            if j = 1 then
               if not Is_Adjacent (Head_Pos, Tail_Pos (1)) then
                  Tail_Pos (1) := Prev_Head_Pos;

                  --  See if this spot has been visited before
                  if not Contains (Visited, Tail_Pos (1)) then
                     if Visited.Capacity > Length (Visited) then
                        Insert (Visited, Tail_Pos (1));
                     else
                        Put_Line ("Too many items in the visited set");
                        return;
                     end if;

                     if Num_Visited < Natural'Last then
                        Num_Visited := Num_Visited + 1;
                     end if;
                  end if;
               end if;
            else
               if not Is_Adjacent (Tail_Pos (j), Tail_Pos (j - 1)) then
                  --  Move toward previous knot
                  New_X_Pos := Tail_Pos (j).X;
                  if Tail_Pos (j - 1).X > Tail_Pos (j).X then
                     New_X_Pos := Tail_Pos (j).X + 1;
                  elsif Tail_Pos (j - 1).X < Tail_Pos (j).X then
                     New_X_Pos := Tail_Pos (j).X - 1;
                  end if;

                  New_Y_Pos := Tail_Pos (j).Y;
                  if Tail_Pos (j - 1).Y > Tail_Pos (j).Y then
                     New_Y_Pos := Tail_Pos (j).Y + 1;
                  elsif Tail_Pos (j - 1).Y < Tail_Pos (j).Y then
                     New_Y_Pos := Tail_Pos (j).Y - 1;
                  end if;
                  Tail_Pos (j) := (X => New_X_Pos, Y => New_Y_Pos);

                  --  If this is the last knot, see if this spot
                  --  has been visited before
                  if j = Tails_Range'Last then
                     if not Contains (Visited_10, Tail_Pos (j)) then
                        if Visited_10.Capacity > Length (Visited_10) then
                           Insert (Visited_10, Tail_Pos (j));
                        else
                           Put_Line ("Too many items in the visited set");
                           return;
                        end if;
                        if Num_Visited_10 < Natural'Last then
                           Num_Visited_10 := Num_Visited_10 + 1;
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end loop;

         Prev_Head_Pos := Head_Pos;
      end loop;
   end loop;

   Put_Line ("Part A count: " & Natural'Image (Num_Visited));
   Put_Line ("Part B count: " & Natural'Image (Num_Visited_10));
end Day9;
