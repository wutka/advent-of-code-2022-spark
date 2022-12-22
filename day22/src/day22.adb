pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Day22 is
   type Col_Range is range 1 .. 151;
   type Row_Range is range 1 .. 201;

   type Grid_Type is array (Col_Range, Row_Range) of Boolean;

   type Row_Start_Array is array (Row_Range) of Col_Range;
   type Row_End_Array is array (Row_Range) of Col_Range;

   type Facing_Type is (Right, Down, Left, Up);

   function Turn_Right (Facing : Facing_Type) return Facing_Type
   is
   begin
      if Facing = Facing_Type'Last then
         return Facing_Type'First;
      else
         return Facing_Type'Succ (Facing);
      end if;
   end Turn_Right;

   function Turn_Left (Facing : Facing_Type) return Facing_Type
   is
   begin
      if Facing = Facing_Type'First then
         return Facing_Type'Last;
      else
         return Facing_Type'Pred (Facing);
      end if;
   end Turn_Left;

   function First_Row (Pos_X : Col_Range; Pos_Y : Row_Range;
      Row_Start : Row_Start_Array; Row_End : Row_End_Array)
      return Row_Range
   is
      New_Y : Row_Range := Pos_Y;
   begin
      while New_Y > 1 and then
         Pos_X >= Row_Start (New_Y - 1) and then
         Pos_X <= Row_End (New_Y - 1)
      loop
         New_Y := New_Y - 1;
      end loop;
      return New_Y;
   end First_Row;

   function Last_Row (Pos_X : Col_Range; Pos_Y : Row_Range;
      Row_Start : Row_Start_Array; Row_End : Row_End_Array;
      Num_Rows : Row_Range)
      return Row_Range
   is
      New_Y : Row_Range := Pos_Y;
   begin
      while New_Y < Num_Rows and then
         Pos_X >= Row_Start (New_Y + 1) and then
         Pos_X <= Row_End (New_Y + 1)
      loop
         New_Y := New_Y + 1;
      end loop;
      return New_Y;
   end Last_Row;

   procedure Move (Pos_X : in out Col_Range;
      Pos_Y : in out Row_Range;
      Facing : Facing_Type;
      Amount : Natural;
      Row_Start : Row_Start_Array;
      Row_End : Row_End_Array;
      Num_Rows : Row_Range;
      Grid : Grid_Type)
   is
      Next_X : Col_Range;
      Next_Y : Row_Range;
   begin
      Next_Y := Pos_Y;
      for i in 1 .. Amount loop
         case Facing is
            when Right =>
               if Pos_X < Row_End (Pos_Y) then
                  Next_X := Pos_X + 1;
               else
                  Next_X := Row_Start (Pos_Y);
               end if;
               if not Grid (Next_X, Pos_Y) then
                  Pos_X := Next_X;
               else
                  exit;
               end if;
            when Left =>
               if Pos_X > Row_Start (Pos_Y) then
                  Next_X := Pos_X - 1;
               else
                  Next_X := Row_End (Pos_Y);
               end if;
               if not Grid (Next_X, Pos_Y) then
                  Pos_X := Next_X;
               end if;
            when Down =>
               if Pos_Y = Num_Rows or else
                  (Pos_Y < Num_Rows and then
                   Pos_X < Row_Start (Pos_Y + 1)) or else
                  (Pos_Y < Num_Rows and then
                   Pos_X > Row_End (Pos_Y + 1))
               then
                  Next_Y := First_Row (Pos_X, Pos_Y, Row_Start, Row_End);
               else
                  if Pos_Y < Row_Range'Last then
                     Next_Y := Pos_Y + 1;
                  end if;
               end if;
               if not Grid (Pos_X, Next_Y) then
                  Pos_Y := Next_Y;
               else
                  exit;
               end if;
            when Up =>
               if Pos_Y = 1 or else
                  Pos_X < Row_Start (Pos_Y - 1) or else
                  Pos_X > Row_End (Pos_Y - 1)
               then
                  Next_Y := Last_Row (Pos_X, Pos_Y, Row_Start,
                     Row_End, Num_Rows);
               else
                  Next_Y := Pos_Y - 1;
               end if;
               if not Grid (Pos_X, Next_Y) then
                  Pos_Y := Next_Y;
               end if;
         end case;
      end loop;
   end Move;

   procedure First_Row_B (Pos_X : in out Col_Range;
      Pos_Y : in out Row_Range;
      Facing : in out Facing_Type)
   is
   begin
      case Pos_Y is
         when 50 =>
            if Pos_X >= 101 and then Pos_X <= 150 then
               Pos_Y := Row_Range (Pos_X - 50);
               Pos_X := 100;
               Facing := Left;
            else
               Put_Line ("Unexpected x in first row b");
            end if;
         when 150 =>
            if Pos_X >= 51 and then Pos_X <= 100 then
               Pos_Y := 100 + Row_Range (Pos_X);
               Pos_X := 50;
               Facing := Left;
            else
               Put_Line ("Unexpected x in first row b");
            end if;
         when 200 =>
            if Pos_X <= 50 then
               Pos_Y := 1;
               Pos_X := Pos_X + 100;
            else
               Put_Line ("Unexpected x in first row b");
            end if;
         when others =>
            Put_Line ("Unexpected first row y");
      end case;
   end First_Row_B;

   procedure Last_Row_B (Pos_X : in out Col_Range;
      Pos_Y : in out Row_Range;
      Facing : in out Facing_Type)
   is
   begin
      case Pos_Y is
         when 1 =>
            if Pos_X >= 51 and then Pos_X <= 100 then
               Pos_Y := Row_Range (Pos_X) + 100;
               Pos_X := 1;
               Facing := Right;
            elsif Pos_X >= 101 and then Pos_X <= 150 then
               Pos_X := Pos_X - 100;
               Pos_Y := 200;
            else
               Put_Line ("Unexpected x in last row b");
            end if;
         when 101 =>
            if Pos_X <= 50 then
               Pos_Y := Row_Range (Pos_X) + 50;
               Pos_X := 51;
               Facing := Right;
            else
               Put_Line ("Unexpected x in last row b");
            end if;
         when others =>
            Put_Line ("Unexpected last row y");
      end case;
   end Last_Row_B;

   procedure First_Col_B (Pos_X : in out Col_Range;
      Pos_Y : in out Row_Range;
      Facing : in out Facing_Type)
   is
   begin
      case Pos_X is
         when 50 =>
            if Pos_Y >= 151 and then Pos_Y <= 200 then
               Pos_X := Col_Range (Pos_Y - 100);
               Pos_Y := 150;
               Facing := Up;
            else
               Put_Line ("Unexpected y in first col b");
            end if;
         when 100 =>
            if Pos_Y >= 51 and then Pos_Y <= 100 then
               Pos_X := Col_Range (Pos_Y + 50);
               Pos_Y := 50;
               Facing := Up;
            elsif Pos_Y >= 101 and then Pos_Y <= 150 then
               Pos_X := 150;
               Pos_Y := 151 - Pos_Y;
               Facing := Left;
            else
               Put_Line ("Unexpected y in first col b");
            end if;
         when 150 =>
            if Pos_Y <= 50 then
               Pos_X := 100;
               Pos_Y := 151 - Pos_Y;
               Facing := Left;
            else
               Put_Line ("Unexpected y in first col b");
            end if;
         when others =>
            Put_Line ("Unexpected first col x");
      end case;
   end First_Col_B;

   procedure Last_Col_B (Pos_X : in out Col_Range;
      Pos_Y : in out Row_Range;
      Facing : in out Facing_Type)
   is
   begin
      case Pos_X is
         when 1 =>
            if Pos_Y >= 101 and then Pos_Y <= 150 then
               Pos_X := 51;
               Pos_Y := 151 - Pos_Y;
               Facing := Right;
            elsif Pos_Y >= 151 and then Pos_Y <= 200 then
               Pos_X := Col_Range (Pos_Y - 100);
               Pos_Y := 1;
               Facing := Down;
            else
               Put_Line ("Unexpected y in last col b");
            end if;
         when 51 =>
            if Pos_Y <= 50 then
               Pos_X := 1;
               Pos_Y := 151 - Pos_Y;
               Facing := Right;
            elsif Pos_Y >= 51 and then Pos_Y <= 100 then
               Pos_X := Col_Range (Pos_Y - 50);
               Pos_Y := 101;
               Facing := Down;
            else
               Put_Line ("Unexpected y in last col b");
            end if;
         when others =>
            Put_Line ("Unexpected last col x");
      end case;
   end Last_Col_B;

   procedure Move_B (Pos_X : in out Col_Range;
      Pos_Y : in out Row_Range;
      Facing : in out Facing_Type;
      Amount : Natural;
      Row_Start : Row_Start_Array;
      Row_End : Row_End_Array;
      Num_Rows : Row_Range;
      Grid : Grid_Type)
   is
      Next_X : Col_Range;
      Next_Y : Row_Range;
      Next_Facing : Facing_Type;
   begin
      for i in 1 .. Amount loop
         Next_X := Pos_X;
         Next_Y := Pos_Y;
         Next_Facing := Facing;

         case Facing is
            when Right =>
               if Pos_X < Row_End (Pos_Y) then
                  Next_X := Pos_X + 1;
               else
                  First_Col_B (Next_X, Next_Y, Next_Facing);
               end if;
               if not Grid (Next_X, Next_Y) then
                  Pos_X := Next_X;
                  Pos_Y := Next_Y;
                  Facing := Next_Facing;
               else
                  exit;
               end if;
            when Left =>
               if Pos_X > Row_Start (Pos_Y) then
                  Next_X := Pos_X - 1;
               else
                  Last_Col_B (Next_X, Next_Y, Next_Facing);
               end if;
               if not Grid (Next_X, Next_Y) then
                  Pos_X := Next_X;
                  Pos_Y := Next_Y;
                  Facing := Next_Facing;
               end if;
            when Down =>
               if Pos_Y = Num_Rows or else
                  (Pos_Y < Num_Rows and then
                   Pos_X < Row_Start (Pos_Y + 1)) or else
                  (Pos_Y < Num_Rows and then
                   Pos_X > Row_End (Pos_Y + 1))
               then
                  First_Row_B (Next_X, Next_Y, Next_Facing);
               else
                  if Pos_Y < Row_Range'Last then
                     Next_Y := Pos_Y + 1;
                  end if;
               end if;
               if not Grid (Next_X, Next_Y) then
                  Pos_X := Next_X;
                  Pos_Y := Next_Y;
                  Facing := Next_Facing;
               else
                  exit;
               end if;
            when Up =>
               if Pos_Y = 1 or else
                  Pos_X < Row_Start (Pos_Y - 1) or else
                  Pos_X > Row_End (Pos_Y - 1)
               then
                  Last_Row_B (Next_X, Next_Y, Next_Facing);
               else
                  Next_Y := Pos_Y - 1;
               end if;
               if not Grid (Next_X, Next_Y) then
                  Pos_X := Next_X;
                  Pos_Y := Next_Y;
                  Facing := Next_Facing;
               end if;
         end case;
      end loop;
   end Move_B;

   Data_File : File_Type;
   Ch : Character;
   Num : Integer;
   Amount : Natural;
   Num_Rows : Row_Range;
   Num_Cols : Col_Range;
   Row_Start : Row_Start_Array;
   Row_End : Row_End_Array;
   Grid : Grid_Type;
   Pos_X : Col_Range;
   Pos_Y : Row_Range;
   Facing : Facing_Type;
   B_Pos_X : Col_Range;
   B_Pos_Y : Row_Range;
   B_Facing : Facing_Type;
   Is_Start : Boolean;

begin
   Open (File => Data_File,
         Mode => In_File,
--         Name => "test.txt");
         Name => "data/day22.txt");

   Num_Rows := 1;
   Row_Start := (others => 1);
   Row_End := (others => 1);
   Grid := (others => (others => False));

   Pos_X := Row_Start (1);
   Pos_Y := 1;
   Facing := Right;

   B_Pos_X := Row_Start (1);
   B_Pos_Y := 1;
   B_Facing := Right;
   while not End_Of_File (Data_File) loop
      loop
         Num_Cols := 1;
         Is_Start := True;

         while not End_Of_Line (Data_File) loop
            Get (Data_File, Ch);
            if Ch = ' ' and then Is_Start then
               if Row_Start (Num_Rows) < Col_Range'Last then
                  Row_Start (Num_Rows) := Row_Start (Num_Rows) + 1;
               end if;
            elsif Ch = '#' then
               Grid (Num_Cols, Num_Rows) := True;
               Is_Start := False;
            else
               Is_Start := False;
            end if;
            if Num_Cols < Col_Range'Last then
               Num_Cols := Num_Cols + 1;
            end if;
         end loop;
         Skip_Line (Data_File);

         if Num_Cols = 1 then
            exit;
         end if;

         Row_End (Num_Rows) := Num_Cols - 1;
         if Num_Rows < Row_Range'Last then
            Num_Rows := Num_Rows + 1;
         end if;
      end loop;

      if Num_Rows = 1 then
         Put_Line ("No grid found");
         exit;
      end if;

      Num_Rows := Num_Rows - 1;

      Pos_X := Row_Start (1);

      B_Pos_X := Row_Start (1);

      while not End_Of_Line (Data_File) loop
         Get (Data_File, Num);

         if Num >= 0 then
            Amount := Natural (Num);
         else
            Put_Line ("Invalid movement amount");
            return;
         end if;

         Move (Pos_X, Pos_Y, Facing, Amount, Row_Start, Row_End,
            Num_Rows, Grid);

         Move_B (B_Pos_X, B_Pos_Y, B_Facing, Amount, Row_Start,
            Row_End, Num_Rows, Grid);

         if not End_Of_Line (Data_File) then
            Get (Data_File, Ch);

            if Ch = 'L' then
               Facing := Turn_Left (Facing);
               B_Facing := Turn_Left (B_Facing);
            elsif Ch = 'R' then
               Facing := Turn_Right (Facing);
               B_Facing := Turn_Right (B_Facing);
            else
               Put_Line ("Invalid turn direction");
               return;
            end if;
         end if;
      end loop;
   end loop;

   Put ("Part A result is ");
   Put_Line (Natural'Image (1000 * Natural (Pos_Y) +
      4 * Natural (Pos_X) + Natural (Facing_Type'Pos (Facing))));
   Put ("Part B result is ");
   Put_Line (Natural'Image (1000 * Natural (B_Pos_Y) +
      4 * Natural (B_Pos_X) + Natural (Facing_Type'Pos (B_Facing))));

end Day22;
