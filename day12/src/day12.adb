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

   type Visited_Grid_Type is array (Row_Range, Col_Range) of Boolean;

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

   function Shortest_Path (Start_Row : Row_Range;
                           Start_Col : Col_Range;
                           End_Row : Row_Range;
                           End_Col : Col_Range;
                           Num_Rows : Row_Range;
                           Num_Cols : Col_Range;
                           Grid : Grid_Type) return Natural
   is
      Visited_Grid : Visited_Grid_Type;
      Work_Queue : Set (10000);
      Work_Entry : Queue_Entry;
      New_Entry : Queue_Entry;
   begin
      Visited_Grid := (others => (others => False));

      Visited_Grid (Start_Row, Start_Col) := True;
      New_Entry := (Start_Row, Start_Col, 0);

      if not Contains (Work_Queue, New_Entry) then
         Insert (Work_Queue, New_Entry);
      end if;

      while not Is_Empty (Work_Queue) loop
         Work_Entry := First_Element (Work_Queue);
         Delete (Work_Queue, Work_Entry);

         --  If we hit the end spot, we are done
         if Work_Entry.Row = End_Row and then Work_Entry.Col = End_Col then
            return Work_Entry.Dist;
         end if;

         Visited_Grid (Work_Entry.Row, Work_Entry.Col) := True;

         --  See if we can move "up" (up means higher row)
         if Work_Entry.Row > 1 and then
            not Visited_Grid (Work_Entry.Row - 1, Work_Entry.Col) and then
            Adjacent (Grid (Work_Entry.Row, Work_Entry.Col),
                      Grid (Work_Entry.Row - 1, Work_Entry.Col))
         then
            if Work_Entry.Dist < Natural'Last then
               New_Entry := (Work_Entry.Row - 1, Work_Entry.Col,
                             Work_Entry.Dist + 1);
               if not Contains (Work_Queue, New_Entry) and then
                  Length (Work_Queue) < Work_Queue.Capacity
               then
                  Insert (Work_Queue, New_Entry);
               end if;
            else
               return Natural'Last;
            end if;
         end if;

         --  See if we can move right
         if Work_Entry.Col > 1 and then
            not Visited_Grid (Work_Entry.Row, Work_Entry.Col - 1) and then
            Adjacent (Grid (Work_Entry.Row, Work_Entry.Col),
                      Grid (Work_Entry.Row, Work_Entry.Col - 1))
         then
            if Work_Entry.Dist < Natural'Last then
               New_Entry := (Work_Entry.Row, Work_Entry.Col - 1,
                             Work_Entry.Dist + 1);
               if not Contains (Work_Queue, New_Entry) and then
                  Length (Work_Queue) < Work_Queue.Capacity
               then
                  Insert (Work_Queue, New_Entry);
               end if;
            else
               return Natural'Last;
            end if;
         end if;

         --  See if we can move "down" (down means lower row)
         if Work_Entry.Row < Num_Rows and then
            not Visited_Grid (Work_Entry.Row + 1, Work_Entry.Col) and then
            Adjacent (Grid (Work_Entry.Row, Work_Entry.Col),
                      Grid (Work_Entry.Row + 1, Work_Entry.Col))
         then
            if Work_Entry.Dist < Natural'Last then
               New_Entry := (Work_Entry.Row + 1, Work_Entry.Col,
                             Work_Entry.Dist + 1);
               if not Contains (Work_Queue, New_Entry) and then
                  Length (Work_Queue) < Work_Queue.Capacity
               then
                  Insert (Work_Queue, New_Entry);
               end if;
            else
               return Natural'Last;
            end if;
         end if;

         --  See if we can move left
         if Work_Entry.Col < Num_Cols and then
            not Visited_Grid (Work_Entry.Row, Work_Entry.Col + 1) and then
            Adjacent (Grid (Work_Entry.Row, Work_Entry.Col),
                      Grid (Work_Entry.Row, Work_Entry.Col + 1))
         then
            if Work_Entry.Dist < Natural'Last then
               New_Entry := (Work_Entry.Row, Work_Entry.Col + 1,
                             Work_Entry.Dist + 1);
               if not Contains (Work_Queue, New_Entry) and then
                  Length (Work_Queue) < Work_Queue.Capacity
               then
                  Insert (Work_Queue, New_Entry);
               end if;
            else
               return Natural'Last;
            end if;
         end if;
      end loop;

      return Natural'Last;
   end Shortest_Path;

   Data_File : File_Type;
   Line : String (1 .. 100);
   Line_Len : Natural;
   Ch : Character;

   Grid : Grid_Type;

   Num_Rows : Row_Range;
   Num_Cols : Col_Range;
   Start_Row : Row_Range;
   Start_Col : Col_Range;
   End_Row : Row_Range;
   End_Col : Col_Range;

   Best_B_Path : Natural;
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

   Path_Len := Shortest_Path (Start_Row, Start_Col, End_Row, End_Col,
                              Num_Rows, Num_Cols, Grid);
   Put_Line ("Part A length: " & Natural'Image (Path_Len));

   Best_B_Path := Natural'Last;

   for row in 1 .. Num_Rows loop
      for col in 1 .. Num_Cols loop
         if Grid (row, col) = 'a' then
            Path_Len := Shortest_Path (row, col, End_Row, End_Col,
                                       Num_Rows, Num_Cols, Grid);
            if Path_Len < Best_B_Path then
               Best_B_Path := Path_Len;
            end if;
         end if;
      end loop;
   end loop;

   Put_Line ("Part B length: " & Natural'Image (Best_B_Path));
end Day12;
