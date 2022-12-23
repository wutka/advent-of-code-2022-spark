pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Formal_Ordered_Sets;
with Ada.Containers.Formal_Ordered_Maps;

procedure Day23 is
   type Direction is (North, South, West, East);

   type Coord_Type is record
      X : Integer := 0;
      Y : Integer := 0;
   end record;

   function Coord_Less (A, B : Coord_Type) return Boolean
   is (A.X < B.X or else (A.X = B.X and then A.Y < B.Y));

   package Coord_Set is new Formal_Ordered_Sets (
      Element_Type => Coord_Type,
      "<" => Coord_Less);

   package Coord_Map is new Formal_Ordered_Maps (
      Key_Type => Coord_Type, Element_Type => Natural,
      "<" => Coord_Less);

   function Coord_In_Dir (Coord : Coord_Type; Dir : Direction)
      return Coord_Type
   is
   begin
      case Dir is
         when North =>
            if Coord.Y > Integer'First then
               return (X => Coord.X, Y => Coord.Y - 1);
            else
               return Coord;
            end if;
         when South =>
            if Coord.Y < Integer'Last then
               return (X => Coord.X, Y => Coord.Y + 1);
            else
               return Coord;
            end if;
         when East =>
            if Coord.X < Integer'Last then
               return (X => Coord.X + 1, Y => Coord.Y);
            else
               return Coord;
            end if;
         when West =>
            if Coord.X > Integer'First then
               return (X => Coord.X - 1, Y => Coord.Y);
            else
               return Coord;
            end if;
      end case;
   end Coord_In_Dir;

   function Is_Alone (Coord : Coord_Type;
      Elves : Coord_Set.Set) return Boolean
   is
   begin
      if Coord.X > Integer'First and then
         Coord.X < Integer'Last and then
         Coord.Y > Integer'First and then
         Coord.Y < Integer'Last
      then
         return
            not Coord_Set.Contains
               (Elves, (X => Coord.X - 1, Y => Coord.Y - 1))
            and then not Coord_Set.Contains
               (Elves, (X => Coord.X, Y => Coord.Y - 1))
            and then not Coord_Set.Contains
               (Elves, (X => Coord.X + 1, Y => Coord.Y - 1))
            and then not Coord_Set.Contains
               (Elves, (X => Coord.X - 1, Y => Coord.Y))
            and then not Coord_Set.Contains
               (Elves, (X => Coord.X + 1, Y => Coord.Y))
            and then not Coord_Set.Contains
               (Elves, (X => Coord.X - 1, Y => Coord.Y + 1))
            and then not Coord_Set.Contains
               (Elves, (X => Coord.X, Y => Coord.Y + 1))
            and then not Coord_Set.Contains
               (Elves, (X => Coord.X + 1, Y => Coord.Y + 1));
      else
         return True;
      end if;
   end Is_Alone;

   function Is_Clear (Coord : Coord_Type; Dir : Direction;
      Elves : Coord_Set.Set) return Boolean
   is
   begin
      case Dir is
         when North | South =>
            if Coord.X > Integer'First and then
               Coord.X < Integer'Last
            then
               return not Coord_Set.Contains
                     (Elves, (X => Coord.X - 1, Y => Coord.Y)) and then
                  not Coord_Set.Contains
                     (Elves, (X => Coord.X, Y => Coord.Y)) and then
                  not Coord_Set.Contains
                     (Elves, (X => Coord.X + 1, Y => Coord.Y));
            else
               return False;
            end if;
         when East | West =>
            if Coord.Y > Integer'First and then
               Coord.Y < Integer'Last
            then
               return not Coord_Set.Contains
                     (Elves, (X => Coord.X, Y => Coord.Y - 1)) and then
                  not Coord_Set.Contains
                     (Elves, (X => Coord.X, Y => Coord.Y)) and then
                  not Coord_Set.Contains
                     (Elves, (X => Coord.X, Y => Coord.Y + 1));
            else
               return False;
            end if;
      end case;
   end Is_Clear;

   procedure Compute_Proposal (Elf_Coord : Coord_Type;
      Start_Dir : Direction;
      Elves : Coord_Set.Set; Can_Propose : out Boolean;
      Proposed_Coord : out Coord_Type)
   is
      Curr_Dir : Direction;
   begin
      Curr_Dir := Start_Dir;

      if Is_Alone (Elf_Coord, Elves) then
         Can_Propose := False;
         Proposed_Coord := (others => <>);
         return;
      end if;

      loop
         Proposed_Coord := Coord_In_Dir (Elf_Coord, Curr_Dir);
         if Is_Clear (Proposed_Coord, Curr_Dir, Elves) then
            Can_Propose := True;
            return;
         end if;

         if Curr_Dir = Direction'Last then
            Curr_Dir := Direction'First;
         else
            Curr_Dir := Direction'Succ (Curr_Dir);
         end if;
         if Curr_Dir = Start_Dir then
            Can_Propose := False;
            Proposed_Coord := (others => <>);
            return;
         end if;
      end loop;
   end Compute_Proposal;

   Data_File : File_Type;
   Ch : Character;

   Elves, New_Elves : Coord_Set.Set (10000);
   Proposals : Coord_Map.Map (10000);
   X, Y : Integer;
   Next_Dir : Direction;
   Elves_Cursor : Coord_Set.Cursor;
   Elf_Coord : Coord_Type;
   Num_Proposed : Integer;
   Can_Propose : Boolean;
   Proposed_Coord : Coord_Type;
   Min_X, Min_Y, Max_X, Max_Y : Integer;
   Width, Height : Integer;
   Moved : Boolean;
   Round : Natural;

begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day23.txt");

   X := 1;
   Y := 1;
   while not End_Of_File (Data_File) loop
      while not End_Of_Line (Data_File) loop
         Get (Data_File, Ch);
         if Ch = '#' then
            if not Coord_Set.Contains (Elves, (X => X, Y => Y))
               and then Coord_Set.Length (Elves) < Elves.Capacity
            then
               Coord_Set.Insert (Elves, (X => X, Y => Y));
            end if;
         end if;
         if X < Integer'Last then
            X := X + 1;
         end if;
      end loop;
      Skip_Line (Data_File);

      if Y < Integer'Last then
         Y := Y + 1;
      end if;
      X := 1;
   end loop;

   Next_Dir := North;

   Round := 1;
   loop
      Coord_Map.Clear (Proposals);
      Coord_Set.Clear (New_Elves);
      Moved := False;

      Elves_Cursor := Coord_Set.First (Elves);
      while Coord_Set.Has_Element (Elves, Elves_Cursor) loop
         Elf_Coord := Coord_Set.Element (Elves, Elves_Cursor);
         Compute_Proposal (Elf_Coord, Next_Dir, Elves, Can_Propose,
            Proposed_Coord);
         if Can_Propose then
            if Coord_Map.Contains (Proposals, Proposed_Coord) then
               Num_Proposed := Coord_Map.Element (Proposals, Proposed_Coord);
               if Num_Proposed < Integer'Last then
                  Coord_Map.Replace (Proposals, Proposed_Coord,
                     Num_Proposed + 1);
               end if;
            else
               if Coord_Map.Length (Proposals) < Proposals.Capacity then
                  Coord_Map.Insert (Proposals, Proposed_Coord, 1);
               end if;
            end if;
         end if;
         Elves_Cursor := Coord_Set.Next (Elves, Elves_Cursor);
      end loop;

      Elves_Cursor := Coord_Set.First (Elves);
      while Coord_Set.Has_Element (Elves, Elves_Cursor) loop
         Elf_Coord := Coord_Set.Element (Elves, Elves_Cursor);
         Compute_Proposal (Elf_Coord, Next_Dir, Elves, Can_Propose,
            Proposed_Coord);
         if Can_Propose then
            if Coord_Map.Contains (Proposals, Proposed_Coord) then
               Num_Proposed := Coord_Map.Element (Proposals, Proposed_Coord);
               if Num_Proposed = 1 then
                  if not Coord_Set.Contains (New_Elves, Proposed_Coord)
                     and then Coord_Set.Length (New_Elves) <
                        New_Elves.Capacity
                  then
                     Coord_Set.Insert (New_Elves, Proposed_Coord);
                     Moved := True;
                  end if;
               else
                  if not Coord_Set.Contains (New_Elves, Elf_Coord) and then
                     Coord_Set.Length (New_Elves) < New_Elves.Capacity
                  then
                     Coord_Set.Insert (New_Elves, Elf_Coord);
                  end if;
               end if;
            else
               Put_Line ("Error - proposal map should contain proposed coord");
            end if;
         else
            if not Coord_Set.Contains (New_Elves, Elf_Coord) and then
               Coord_Set.Length (New_Elves) < New_Elves.Capacity
            then
               Coord_Set.Insert (New_Elves, Elf_Coord);
            end if;
         end if;
         Elves_Cursor := Coord_Set.Next (Elves, Elves_Cursor);
      end loop;

      Coord_Set.Move (Elves, New_Elves);
--      Print_Elves (-10, -10, 20, 20, Elves);

      if Next_Dir = Direction'Last then
         Next_Dir := Direction'First;
      else
         Next_Dir := Direction'Succ (Next_Dir);
      end if;

      if not Moved then
         Put_Line ("Part B, round = " & Natural'Image (Round));
         exit;
      end if;

      if Round = 10 then
         Elves_Cursor := Coord_Set.First (Elves);
         Min_X := Integer'Last;
         Min_Y := Integer'Last;
         Max_X := Integer'First;
         Max_Y := Integer'First;

         while Coord_Set.Has_Element (Elves, Elves_Cursor) loop
            Elf_Coord := Coord_Set.Element (Elves, Elves_Cursor);
            Min_X := Integer'Min (Min_X, Elf_Coord.X);
            Min_Y := Integer'Min (Min_Y, Elf_Coord.Y);
            Max_X := Integer'Max (Max_X, Elf_Coord.X);
            Max_Y := Integer'Max (Max_Y, Elf_Coord.Y);
            Elves_Cursor := Coord_Set.Next (Elves, Elves_Cursor);
         end loop;

         if Min_X > -10_000 and then
            Min_X < 10_000 and then
            Max_X > -10_000 and then
            Max_X < 10_000 and then
            Max_X > Min_X
         then
            Width := Max_X - Min_X + 1;
         else
            Put_Line ("Too Wide");
            return;
         end if;

         if Min_Y > -10_000 and then
            Min_Y < 10_000 and then
            Max_Y > -10_000 and then
            Max_Y < 10_000 and then
            Max_Y > Min_Y
         then
            Height := Max_Y - Min_Y + 1;
         else
            Put_Line ("Too Wide");
            return;
         end if;

         if Width <= 0 or else Height <= 0 then
            Put_Line ("Invalid dimensions");
            return;
         end if;

         if Width > 0 and then Width < 30_000 and then
            Height > 0 and then Height < 30_000 and then
            Natural (Coord_Set.Length (Elves)) < 100_000
         then
            Put_Line ("Part A sum is " &
               Long_Integer'Image (
               Long_Integer (Width) * Long_Integer (Height) -
               Long_Integer (Coord_Set.Length (Elves))));
         else
            Put_Line ("Dimension overflow");
         end if;
      end if;

      if Round < Natural'Last then
         Round := Round + 1;
      end if;

   end loop;

end Day23;
