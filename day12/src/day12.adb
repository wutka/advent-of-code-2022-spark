pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Formal_Ordered_Sets;

procedure Day12 is
   type Row_Range is range 1 .. 50;
   type Col_Range is range 1 .. 100;

   type Grid_Val is new Character range 'a' .. 'z';

   type Grid_Type is array (Row_Range, Col_Range) of Grid_Val;
   type Grid_Distance_Type is array (Row_Range, Col_Range) of Natural;

   type Queue_Entry is record
      Row : Row_Range;
      Col : Col_Range;
      Dist : Natural;
   end record;

   function Queue_Entry_Less (A, B : Queue_Entry) return Boolean;
   function Adjacent (A, B : Grid_Val) return Boolean;

   package Queue_Set is new Formal_Ordered_Sets (
      Element_Type => Queue_Entry,
      "<" => Queue_Entry_Less);
   use Queue_Set;

   function Queue_Entry_Less (A, B : Queue_Entry) return Boolean is
   begin
      return A.Dist < B.Dist or else
         (A.Dist = B.Dist and then A.Col < B.Col) or else
         (A.Dist = B.Dist and then A.Col = B.Col and then
          A.Row < B.Row);
   end Queue_Entry_Less;

   function Adjacent (A, B : Grid_Val) return Boolean is
   begin
      return B <= A or else (B > 'a' and then A = Grid_Val'Pred (B));
   end Adjacent;

   procedure Compute_Grid_Distances (End_Row : Row_Range;
                           End_Col : Col_Range;
                           Num_Rows : Row_Range;
                           Num_Cols : Col_Range;
                           Grid : Grid_Type;
                           Distances : out Grid_Distance_Type;
                           Best_Start_Row : out Row_Range;
                           Best_Start_Col : out Col_Range)
   is
      Work_Queue : Set (10000);
      Work_Entry : Queue_Entry;
      New_Entry : Queue_Entry;
      Shortest_Start : Natural;
   begin
      Distances := (others => (others => Natural'Last));

      Distances (End_Row, End_Col) := 0;
      New_Entry := (End_Row, End_Col, 0);

      Shortest_Start := Natural'Last;

      Best_Start_Row := 1;
      Best_Start_Col := 1;

      if not Contains (Work_Queue, New_Entry) then
         Insert (Work_Queue, New_Entry);
      end if;

      while not Is_Empty (Work_Queue) loop
         Work_Entry := First_Element (Work_Queue);
         Delete (Work_Queue, Work_Entry);

         Distances (Work_Entry.Row, Work_Entry.Col) := Work_Entry.Dist;

         if Grid (Work_Entry.Row, Work_Entry.Col) = 'a' and then
            Work_Entry.Dist < Shortest_Start
         then
            Best_Start_Row := Work_Entry.Row;
            Best_Start_Col := Work_Entry.Col;
            Shortest_Start := Work_Entry.Dist;
         end if;

         --  See if we can move "up" (up means higher row)
         if Work_Entry.Row > 1 and then
            Distances (Work_Entry.Row - 1, Work_Entry.Col) = Natural'Last
            and then Adjacent (Grid (Work_Entry.Row - 1, Work_Entry.Col),
                               Grid (Work_Entry.Row, Work_Entry.Col))
         then
            if Work_Entry.Dist < Natural'Last then
               New_Entry := (Work_Entry.Row - 1, Work_Entry.Col,
                             Work_Entry.Dist + 1);
               if not Contains (Work_Queue, New_Entry) and then
                  Length (Work_Queue) < Work_Queue.Capacity
               then
                  Insert (Work_Queue, New_Entry);
               end if;
            end if;
         end if;

         --  See if we can move right
         if Work_Entry.Col > 1 and then
            Distances (Work_Entry.Row, Work_Entry.Col - 1) = Natural'Last
            and then Adjacent (Grid (Work_Entry.Row, Work_Entry.Col - 1),
                               Grid (Work_Entry.Row, Work_Entry.Col))
         then
            if Work_Entry.Dist < Natural'Last then
               New_Entry := (Work_Entry.Row, Work_Entry.Col - 1,
                             Work_Entry.Dist + 1);
               if not Contains (Work_Queue, New_Entry) and then
                  Length (Work_Queue) < Work_Queue.Capacity
               then
                  Insert (Work_Queue, New_Entry);
               end if;
            end if;
         end if;

         --  See if we can move "down" (down means lower row)
         if Work_Entry.Row < Num_Rows and then
            Distances (Work_Entry.Row + 1, Work_Entry.Col) = Natural'Last
            and then Adjacent (Grid (Work_Entry.Row + 1, Work_Entry.Col),
                               Grid (Work_Entry.Row, Work_Entry.Col))
         then
            if Work_Entry.Dist < Natural'Last then
               New_Entry := (Work_Entry.Row + 1, Work_Entry.Col,
                             Work_Entry.Dist + 1);
               if not Contains (Work_Queue, New_Entry) and then
                  Length (Work_Queue) < Work_Queue.Capacity
               then
                  Insert (Work_Queue, New_Entry);
               end if;
            end if;
         end if;

         --  See if we can move left
         if Work_Entry.Col < Num_Cols and then
            Distances (Work_Entry.Row, Work_Entry.Col + 1) = Natural'Last
            and then Adjacent (Grid (Work_Entry.Row, Work_Entry.Col + 1),
                               Grid (Work_Entry.Row, Work_Entry.Col))
         then
            if Work_Entry.Dist < Natural'Last then
               New_Entry := (Work_Entry.Row, Work_Entry.Col + 1,
                             Work_Entry.Dist + 1);
               if not Contains (Work_Queue, New_Entry) and then
                  Length (Work_Queue) < Work_Queue.Capacity
               then
                  Insert (Work_Queue, New_Entry);
               end if;
            end if;
         end if;
      end loop;
   end Compute_Grid_Distances;

   Data_File : File_Type;
   Line : String (1 .. 100);
   Line_Len : Natural;
   Ch : Character;

   Grid : Grid_Type;
   Distances : Grid_Distance_Type;

   Num_Rows : Row_Range;
   Num_Cols : Col_Range;
   Start_Row : Row_Range;
   Start_Col : Col_Range;
   End_Row : Row_Range;
   End_Col : Col_Range;
   Best_Start_Row : Row_Range;
   Best_Start_Col : Col_Range;

   Path_Len : Natural;

begin

   Open (File => Data_File,
         Mode => In_File,
--         Name => "test.txt");
         Name => "data/day12.txt");

   Num_Rows := 1;

   Start_Row := 1;
   Start_Col := 1;
   End_Row := 1;
   End_Col := 1;
   Num_Cols := 1;

   Grid := (others => (others => 'a'));

   while not End_Of_File (Data_File) loop
      Get_Line (Data_File, Line, Line_Len);

      if Line_Len > 0 and then Line_Len <= Natural (Col_Range'Last) then
         Num_Cols := Col_Range (Line_Len);
      else
         Put_Line ("Too many columns");
         return;
      end if;

      for i in 1 .. Line_Len loop
         Ch := Line (i);
         if Ch in 'a' .. 'z' then
            Grid (Num_Rows, Col_Range (i)) := Grid_Val (Ch);
         elsif Ch = 'S' then
            Start_Row := Num_Rows;
            Start_Col := Col_Range (i);
            Grid (Start_Row, Start_Col) := 'a';
         elsif Ch = 'E' then
            End_Row := Num_Rows;
            End_Col := Col_Range (i);
            Grid (End_Row, End_Col) := 'z';
         else
            Put_Line ("Unknown character in grid - " & Character'Image (Ch));
            return;
         end if;
      end loop;

      if Num_Rows < Row_Range'Last then
         Num_Rows := Num_Rows + 1;
      else
         Put_Line ("Too many rows");
         return;
      end if;
   end loop;

   if Num_Rows > 1 then
      Num_Rows := Num_Rows - 1;
   else
      Put_Line ("Not enough rows");
      return;
   end if;

   Compute_Grid_Distances (End_Row, End_Col, Num_Rows, Num_Cols,
                           Grid, Distances, Best_Start_Row, Best_Start_Col);

   Path_Len := Distances (Start_Row, Start_Col);
   Put_Line ("Part A length: " & Natural'Image (Path_Len));

   Path_Len := Distances (Best_Start_Row, Best_Start_Col);
   Put_Line ("Part B length: " & Natural'Image (Path_Len));
end Day12;
