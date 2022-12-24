pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;

procedure Day24 is
   type Col_Range is range 1 .. 120;
   type Row_Range is range 1 .. 25;

   type Grid_Type is array (Col_Range, Row_Range) of Boolean;

   type Direction is (Up, Right, Down, Left);

   type Blizzard_Range is range 1 .. 3000;
   type Blizzard is record
      Col : Col_Range := 1;
      Row : Row_Range := 1;
      Dir : Direction := Up;
   end record;

   type Blizzard_Array is array (Blizzard_Range) of Blizzard;

   procedure Compute_Grid_And_Advance (Grid : out Grid_Type;
      Num_Cols : Col_Range; Num_Rows : Row_Range;
      Blizzards : in out Blizzard_Array; Num_Blizzards : Blizzard_Range) is
   begin
      Grid := (others => (others => False));
      for i in 1 .. Num_Blizzards loop
         Grid (Blizzards (i).Col, Blizzards (i).Row) := True;

         case Blizzards (i).Dir is
            when Up =>
               if Blizzards (i).Row = 1 then
                  Blizzards (i).Row := Num_Rows;
               else
                  Blizzards (i).Row := Blizzards (i).Row - 1;
               end if;
            when Right =>
               if Blizzards (i).Col >= Num_Cols then
                  Blizzards (i).Col := 1;
               else
                  Blizzards (i).Col := Blizzards (i).Col + 1;
               end if;
            when Down =>
               if Blizzards (i).Row >= Num_Rows then
                  Blizzards (i).Row := 1;
               else
                  Blizzards (i).Row := Blizzards (i).Row + 1;
               end if;
            when Left =>
               if Blizzards (i).Col = 1 then
                  Blizzards (i).Col := Num_Cols;
               else
                  Blizzards (i).Col := Blizzards (i).Col - 1;
               end if;
         end case;
      end loop;
   end Compute_Grid_And_Advance;

   Data_File : File_Type;
   Start_Col, End_Col : Col_Range;
   Num_Rows : Row_Range;
   Num_Cols : Col_Range;
   Ch : Character;
   Curr_Col : Col_Range;
   Curr_Row : Row_Range;
   Blizzards : Blizzard_Array;
   Num_Blizzards : Blizzard_Range;
   Grid : Grid_Type;
   Turn : Natural;
   Presence : Grid_Type;
   New_Presence : Grid_Type;

begin
   Blizzards := (others => (others => <>));
   Num_Blizzards := 1;

   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day24.txt");

   Start_Col := 1;

   if not End_Of_File (Data_File) then
      Get (Data_File, Ch);
      Start_Col := 1;
      while not End_Of_Line (Data_File) loop
         Get (Data_File, Ch);
         if Ch = '.' then
            Skip_Line (Data_File);
            exit;
         end if;
         if Start_Col < Col_Range'Last then
            Start_Col := Start_Col + 1;
         end if;
      end loop;
   end if;

   Num_Cols := 1;
   Num_Rows := 1;
   Curr_Row := 1;
   End_Col := 1;

   while not End_Of_File (Data_File) loop
      Curr_Col := 1;

      Get (Data_File, Ch);

      while not End_Of_Line (Data_File) loop
         Get (Data_File, Ch);
         if Curr_Col = 1 and then Ch = '#' then
            End_Col := 1;
            while not End_Of_Line (Data_File) loop
               Get (Data_File, Ch);
               if End_Col < Col_Range'Last then
                  End_Col := End_Col + 1;
               end if;
               if Ch = '.' then
                  Skip_Line (Data_File);
                  exit;
               end if;
            end loop;
            exit;
         end if;
         case Ch is
            when '^' =>
               Blizzards (Num_Blizzards) := (Col => Curr_Col,
                                             Row => Curr_Row,
                                             Dir => Up);
               if Num_Blizzards < Blizzard_Range'Last then
                  Num_Blizzards := Num_Blizzards + 1;
               end if;
            when '>' =>
               Blizzards (Num_Blizzards) := (Col => Curr_Col,
                                             Row => Curr_Row,
                                             Dir => Right);
               if Num_Blizzards < Blizzard_Range'Last then
                  Num_Blizzards := Num_Blizzards + 1;
               end if;
            when 'v' =>
               Blizzards (Num_Blizzards) := (Col => Curr_Col,
                                             Row => Curr_Row,
                                             Dir => Down);
               if Num_Blizzards < Blizzard_Range'Last then
                  Num_Blizzards := Num_Blizzards + 1;
               end if;
            when '<' =>
               Blizzards (Num_Blizzards) := (Col => Curr_Col,
                                             Row => Curr_Row,
                                             Dir => Left);
               if Num_Blizzards < Blizzard_Range'Last then
                  Num_Blizzards := Num_Blizzards + 1;
               end if;
            when '#' => exit;
            when others => null;
         end case;
         if Curr_Col < Col_Range'Last then
            Curr_Col := Curr_Col + 1;
            if Curr_Col > Num_Cols then
               Num_Cols := Curr_Col;
            end if;
         end if;
      end loop;

      if Curr_Row < Row_Range'Last then
         Curr_Row := Curr_Row + 1;
         if Curr_Row > Num_Rows then
            Num_Rows := Curr_Row;
         end if;
      end if;
   end loop;

   if Num_Blizzards = 1 then
      Put_Line ("No blizzards");
      return;
   end if;

   Num_Blizzards := Num_Blizzards - 1;

   if Num_Cols = 1 then
      Put_Line ("No columns");
      return;
   end if;

   if Num_Rows < 3 then
      Put_Line ("No rows found");
      return;
   end if;

   if Num_Cols < Col_Range'Last then
      Num_Cols := Num_Cols - 1;
   end if;

   if Num_Rows < Row_Range'Last then
      Num_Rows := Num_Rows - 2;
   end if;

   Turn := 1;

   Presence := (others => (others => False));

   loop
      Compute_Grid_And_Advance (Grid, Num_Cols, Num_Rows,
         Blizzards, Num_Blizzards);

      New_Presence := (others => (others => False));
      for r in 1 .. Num_Rows loop
         for c in 1 .. Num_Cols loop
            if not Grid (c, r) and then
               ((r > 1 and then Presence (c, r - 1)) or else
                (r < Num_Rows and then Presence (c, r + 1)) or else
                (c > 1 and then Presence (c - 1, r)) or else
                (c < Num_Cols and then Presence (c + 1, r)) or else
                (Presence (c, r)))
            then
               New_Presence (c, r) := True;
            end if;
         end loop;
      end loop;

      if not Grid (Start_Col, 1) then
         New_Presence (Start_Col, 1) := True;
      end if;

      if New_Presence (End_Col, Num_Rows) then
         exit;
      end if;

      Presence := New_Presence;

      if Turn < Natural'Last then
         Turn := Turn + 1;
      end if;
   end loop;

   Put_Line ("Part A path len = " & Natural'Image (Turn));

   Presence := (others => (others => False));

   if Turn < Natural'Last then
      Turn := Turn + 1;
   end if;

   loop
      Compute_Grid_And_Advance (Grid, Num_Cols, Num_Rows,
         Blizzards, Num_Blizzards);

      New_Presence := (others => (others => False));
      for r in 1 .. Num_Rows loop
         for c in 1 .. Num_Cols loop
            if not Grid (c, r) and then
               ((r > 1 and then Presence (c, r - 1)) or else
                (r < Num_Rows and then Presence (c, r + 1)) or else
                (c > 1 and then Presence (c - 1, r)) or else
                (c < Num_Cols and then Presence (c + 1, r)) or else
                (Presence (c, r)))
            then
               New_Presence (c, r) := True;
            end if;
         end loop;
      end loop;

      if not Grid (End_Col, 1) then
         New_Presence (End_Col, 1) := True;
      end if;

      if New_Presence (Start_Col, Num_Rows) then
         exit;
      end if;

      Presence := New_Presence;

      if Turn < Natural'Last then
         Turn := Turn + 1;
      end if;
   end loop;

   Presence := (others => (others => False));

   if Turn < Natural'Last then
      Turn := Turn + 1;
   end if;

   loop
      Compute_Grid_And_Advance (Grid, Num_Cols, Num_Rows,
         Blizzards, Num_Blizzards);

      New_Presence := (others => (others => False));
      for r in 1 .. Num_Rows loop
         for c in 1 .. Num_Cols loop
            if not Grid (c, r) and then
               ((r > 1 and then Presence (c, r - 1)) or else
                (r < Num_Rows and then Presence (c, r + 1)) or else
                (c > 1 and then Presence (c - 1, r)) or else
                (c < Num_Cols and then Presence (c + 1, r)) or else
                (Presence (c, r)))
            then
               New_Presence (c, r) := True;
            end if;
         end loop;
      end loop;

      if not Grid (Start_Col, 1) then
         New_Presence (Start_Col, 1) := True;
      end if;

      if New_Presence (End_Col, Num_Rows) then
         exit;
      end if;

      Presence := New_Presence;

      if Turn < Natural'Last then
         Turn := Turn + 1;
      end if;
   end loop;
   Put_Line ("Part A path len = " & Natural'Image (Turn));
end Day24;
