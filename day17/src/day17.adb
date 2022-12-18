pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;

procedure Day17 is

   Long_Num_Rocks : constant  := 1_000_000_000_000;

   type Row_Range is range 1 .. 7;
   type Row_Array is array (Row_Range) of Boolean;

   type Col_Range is range 1 .. 50000;
   type Col_Type is array (Col_Range) of Row_Array;

   type Coord_Offset_Range is range 0 .. 3;

   type Coord_Offset is record
      X : Coord_Offset_Range := 0;
      Y : Coord_Offset_Range := 0;
   end record;

   type Coord_Range is range 1 .. 5;
   type Coord_Array is array (Coord_Range) of Coord_Offset;

   type Depth_Type is array (Row_Range) of Col_Range;
   type Depth_Range is range 1 .. 500;
   type Depth_Array is array (Depth_Range) of Depth_Type;
   type Height_Array is array (Depth_Range) of Natural;

   type Shape_Type is record
      Num_Coords : Coord_Range := 1;
      Max_X : Coord_Offset_Range := 0;
      Coords : Coord_Array := (others => (others => <>));
   end record;

   type Shape_Range is range 1 .. 5;
   type Shape_Array is array (Shape_Range) of Shape_Type;

   function "+" (Row : Row_Range; Offset : Coord_Offset_Range) return Row_Range
      with Pre => Natural (Row_Range'Last) - Natural (Row) >= Natural (Offset)
   is
   begin
      return (Row_Range (Natural (Row) + Natural (Offset)));
   end "+";

   function "+" (Col : Col_Range; Offset : Coord_Offset_Range) return Col_Range
      with Pre => Natural (Col_Range'Last) - Natural (Col) >= Natural (Offset)
   is
   begin
      return (Col_Range (Natural (Col) + Natural (Offset)));
   end "+";

   function Can_Move (Shape : Shape_Type; X : Row_Range; Y : Col_Range;
      Columns : Col_Type) return Boolean
      with
         Pre => Y < Col_Range'Last - 3
   is
   begin
      for i in 1 .. Shape.Num_Coords loop
         if Natural (Row_Range'Last) - Natural (X) <
            Natural (Shape.Coords (i).X)
         then
            return False;
         end if;
         if Columns (Y + Shape.Coords (i).Y)(X + Shape.Coords (i).X) then
            return False;
         end if;
      end loop;
      return True;
   end Can_Move;

   procedure Place (Shape : Shape_Type; X : Row_Range; Y : Col_Range;
      Columns : in out Col_Type; Highest : in out Col_Range)
      with
         Pre => Y < Col_Range'Last - 3 and then Can_Move (Shape, X, Y, Columns)
   is
   begin
      for i in 1 .. Shape.Num_Coords loop
         if Natural (Row_Range'Last - X) >= Natural (Shape.Coords (i).X) then
            Columns (Y + Shape.Coords (i).Y)(X + Shape.Coords (i).X) := True;
            if Y + Shape.Coords (i).Y >= Highest
            then
               if Natural (Col_Range'Last - Y) >= Natural (Shape.Coords (i).Y) then
                  Highest := Y + Shape.Coords (i).Y;
                  if Col_Range'Last > Highest then
                     Highest := Highest + Col_Range (1);
                  end if;
               end if;
            end if;
         end if;
      end loop;
   end Place;

   procedure Compute_Depth (Highest : Col_Range; Columns : Col_Type;
      Depth : out Depth_Type) is
   begin
      Depth := (others => 1);
      for x in Row_Range loop
         Depth (x) := 1;
         for y in reverse 1 .. Highest loop
            if Columns (Col_Range (y))(x) then
               Depth (x) := Highest - y + Col_Range (1);
               exit;
            end if;
         end loop;
      end loop;
   end Compute_Depth;

   procedure Blocking (Y : Col_Range; X : Row_Range; Highest : Col_Range;
      Columns : Col_Type; Success : out Boolean; Min_Y : in out Col_Range)
   is
   begin
      Success := False;

      if not Columns (Y)(X) then
         return;
      end if;

      if X = Row_Range'Last then
         Success := True;
         if Y < Min_Y then
            Min_Y := Y;
         end if;
         return;
      end if;

      if Y < Highest then
         Blocking (Y + Col_Range (1), X + Row_Range (1), Highest, Columns,
            Success, Min_Y);
         if Success then
            if Y < Min_Y then
               Min_Y := Y;
            end if;
            return;
         end if;
      end if;

      Blocking (Y, X + Row_Range (1), Highest, Columns, Success, Min_Y);
      if Success then
         if Y < Min_Y then
            Min_Y := Y;
         end if;
         return;
      end if;

      if Y > 1 then
         Blocking (Y - 1, X + Row_Range (1), Highest, Columns, Success, Min_Y);
         if  Y < Min_Y then
            Min_Y := Y;
         end if;
         return;
      end if;
   end Blocking;

   procedure Reduce (Highest : in out Col_Range; Columns : in out Col_Type;
      Reduce_Amount : out Natural)
   is
      Success : Boolean;
      Min_Y : Col_Range;
   begin
      Reduce_Amount := 0;
      for Y in reverse 1 .. Highest loop
         Min_Y := Y;
         Blocking (Y, 1, Highest, Columns, Success, Min_Y);
         if Success then
            Reduce_Amount := Natural (Min_Y);
            if Min_Y = Highest then
               Columns (1 .. Highest) := (others => (others => False));
               Highest := 1;
            else
               Columns (1 .. Highest - Min_Y) :=
                 Columns (Min_Y + Col_Range (1) .. Highest);
               if Min_Y < Highest then
                  Columns (Highest - Min_Y + Col_Range (1) .. Highest)
                     := (others => (others => False));
                  Highest := Highest - Min_Y;
               end if;
            end if;
            return;
         end if;
      end loop;
   end Reduce;

   Data_File : File_Type;

   Jets : String (1 .. 12000);
   Jets_Len : Natural;
   Jets_Pos : Natural;
   Shapes : Shape_Array;
   Columns : Col_Type;
   Highest : Col_Range;
   Total_Reduced : Natural;
   Amount_Reduced : Natural;
   Next_Shape : Shape_Range;
   Shape_X : Row_Range;
   Shape_Y : Col_Range;
   Placed : Boolean;
   Cycle : Natural;
   Found_Cycle : Boolean;
   Num_Depths : Depth_Range;
   Depths : Depth_Array;
   Heights : Height_Array;
   Height_Diff : Natural;
   Long_Height : Long_Integer;
   Long_Rocks : Long_Integer;
   Long_Num_Cycles : Long_Integer;
   Rocks_To_Repeat : Long_Integer;
   Rock : Natural;
   Got_A : Boolean;
   Got_B : Boolean;
   Total_Height_At_Cycle : Natural;

begin
   Shapes (1) := (4, 3, ((0, 0), (1, 0), (2, 0), (3, 0), (0, 0)));
   Shapes (2) := (5, 2, ((1, 0), (0, 1), (1, 1), (2, 1), (1, 2)));
   Shapes (3) := (5, 2, ((0, 0), (1, 0), (2, 0), (2, 1), (2, 2)));
   Shapes (4) := (4, 0, ((0, 0), (0, 1), (0, 2), (0, 3), (0, 0)));
   Shapes (5) := (4, 1, ((0, 0), (1, 0), (0, 1), (1, 1), (0, 0)));

   Columns := (others => (others => False));
   Depths := (others => (others => 1));
   Heights := (others => 0);

   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day17.txt");
--         Name => "test.txt");

   if not End_Of_File (Data_File) then
      Get_Line (Data_File, Jets, Jets_Len);
   else
      Put_Line ("No jets in file");
      return;
   end if;

   Highest := 1;
   Next_Shape := 1;
   Jets_Pos := 1;

   if Jets_Len = 0 then
      Put_Line ("No jets in file");
      return;
   end if;

   if Natural'Last / Jets_Len > 5 then
      Cycle := Jets_Len * 5;
   else
      Put_Line ("Cycle overflow");
      return;
   end if;

   Total_Reduced := 0;

   if Natural'Last / 2 < Cycle then
      Put_Line ("Cycle is too big");
      return;
   end if;

   Found_Cycle := False;

   Rock := 0;
   Long_Rocks := 0;
   Long_Height := 0;
   Total_Height_At_Cycle := 0;

   Num_Depths := 1;

   Got_A := False;
   Got_B := False;

   loop
      pragma Loop_Invariant (Jets_Pos >= 1 and then Jets_Pos <= Jets_Len);

      if Rock = 0 then
         Compute_Depth (Highest, Columns, Depths (1));
         if Natural'Last - Total_Reduced > Natural (Highest) then
            Heights (1) := Total_Reduced + Natural (Highest);
         end if;
      elsif not Found_Cycle and then Rock mod Cycle = 0 then
         if Num_Depths < Depth_Range'Last then
            Num_Depths := Num_Depths + 1;
         else
            Depths (1 .. Depth_Range'Last - 1) :=
               Depths (2 .. Depth_Range'Last);
            Heights (1 .. Depth_Range'Last - 1) :=
               Heights (2 .. Depth_Range'Last);
         end if;
         Compute_Depth (Highest, Columns, Depths (Num_Depths));
         if Natural'Last - Total_Reduced > Natural (Highest) then
            Heights (Num_Depths) := Total_Reduced + Natural (Highest);
         end if;

         if Num_Depths > 400 then
            for i in 1 .. Num_Depths - 1 loop
               if Depths (Num_Depths) = Depths (i) then
                  Found_Cycle := True;
                  if Cycle > 0 and then
                     Long_Integer'Last / Long_Integer (Cycle) > 
                     Long_Integer (Num_Depths - i)
                  then
                     Rocks_To_Repeat := Long_Integer (Cycle) *
                        Long_Integer (Num_Depths - i);
                  else
                     Rocks_To_Repeat := 1;
                  end if;
                  Long_Num_Cycles := (Long_Num_Rocks - Long_Integer (Rock)) /
                     Rocks_To_Repeat - 1;
                  Long_Rocks := Long_Integer (Rock) + Rocks_To_Repeat *
                     Long_Num_Cycles;
                  Height_Diff := 0;
                  if Heights (Num_Depths) > Heights (i) then
                     Height_Diff := Natural (Heights (Num_Depths)) -
                        Natural (Heights (i));
                  end if;
                  if Long_Num_Cycles > 0 and then
                     Long_Integer'Last / Long_Num_Cycles >
                     Long_Integer (Height_Diff)
                  then
                     Long_Height := Long_Integer (Height_Diff) *
                        Long_Num_Cycles;
                     if Long_Integer'Last - Long_Height >
                        Long_Integer (Heights (Num_Depths))
                     then
                        Long_Height := Long_Height + Long_Integer (Heights (Num_Depths));
                     end if;
                  else
                     Long_Height := 0;
                  end if;
                  Total_Height_At_Cycle := Heights (Num_Depths);

                  if Long_Rocks = Long_Num_Rocks then
                     Got_B := True;
                  end if;
                  exit;
               end if;
            end loop;
         end if;
         if Got_B and Got_A then
            exit;
         end if;
      elsif Found_Cycle and then not Got_B
      then
         if Long_Integer'Last > Long_Rocks then
            Long_Rocks := Long_Rocks + 1;
         end if;
         if Long_Rocks = Long_Num_Rocks then
            Got_B := True;
            if Got_A then
               exit;
            end if;
         end if;
      end if;

      if Natural'Last > Rock then
         Rock := Rock + 1;
      end if;

      if Col_Range'Last - Highest <= 3 then
         Put_Line ("Not enough columns");
         exit;
      end if;

      Shape_Y := Highest + Col_Range (3);
      Shape_X := 3;

      Placed := False;

--      Print_Columns (Shapes (Next_Shape), Shape_X, Shape_Y,
--         Columns, Highest);

      while not Placed loop
         pragma Loop_Invariant (Jets_Pos >= 1 and then Jets_Pos <= Jets_Len);
--         Print_Columns (Shapes (Next_Shape), Shape_X, Shape_Y,
--            Columns, Highest);
         if Jets (Jets_Pos) = '>' then
            if Natural (Row_Range'Last) - Natural (Shape_X) >
               Natural (Shapes (Next_Shape).Max_X) and then
               Col_Range'Last - Shape_Y > 3 and then
               Can_Move (Shapes (Next_Shape), Shape_X + Row_Range (1),
                  Shape_Y, Columns)
            then
               Shape_X := Shape_X + Row_Range (1);
            end if;
         elsif Jets (Jets_Pos) = '<' then
            if Shape_X > 1 and then
               Col_Range'Last - Shape_Y > 3 and then
               Can_Move (Shapes (Next_Shape), Shape_X - Row_Range (1),
                  Shape_Y, Columns)
            then
               Shape_X := Shape_X - 1;
            end if;
         end if;
         if Jets_Pos >= Jets_Len then
            Jets_Pos := 1;
         else
            Jets_Pos := Jets_Pos + 1;
         end if;

--         Print_Columns (Shapes (Next_Shape), Shape_X, Shape_Y,
--            Columns, Highest);

         if Shape_Y > 1 and then
            Col_Range'Last - Shape_Y > 4 and then
            Can_Move (Shapes (Next_Shape), Shape_X, Shape_Y - 1, Columns)
         then
            Shape_Y := Shape_Y - 1;
         else
            if Col_Range'Last - Shape_Y > 3 and then
               Can_Move (Shapes (Next_Shape), Shape_X, Shape_Y, Columns)
            then
               Place (Shapes (Next_Shape), Shape_X, Shape_Y, Columns, Highest);
            end if;
            Placed := True;
         end if;

      end loop;

      if Next_Shape = Shape_Range'Last then
         Next_Shape := 1;
      else
         Next_Shape := Next_Shape + 1;
      end if;

      Reduce (Highest, Columns, Amount_Reduced);
      if Amount_Reduced > 0 then
         if Natural'Last - Amount_Reduced > Total_Reduced then
            Total_Reduced := Total_Reduced + Amount_Reduced;
         end if;
      end if;

      if Rock = 2022 then
         if Natural'Last - Total_Reduced > Natural (Highest - 1) then
            Put_Line ("Part A highest is " & Natural'Image (Total_Reduced +
               Natural (Highest - 1)));
         end if;
         Got_A := True;
         if Got_B then
            exit;
         end if;
      end if;

   end loop;

   if Long_Height >= 0 and then
      Natural'Last - Total_Reduced > Natural (Highest) and then
      Long_Integer'Last - Long_Height >
         Long_Integer (Total_Reduced + Natural (Highest) -
            Total_Height_At_Cycle - 1)
   then
      Put_Line ("Part B total = " & Long_Integer'Image (Long_Height +
         Long_Integer (Total_Reduced + Natural (Highest) -
            Total_Height_At_Cycle - 1)));
   end if;
end Day17;
