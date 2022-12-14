pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Day14 is
   type Grid_X_Range is range 0 .. 1000;
   type Grid_Y_Range is range 0 .. 200;

   type Grid_Type is array (Grid_X_Range, Grid_Y_Range) of Boolean;

   Data_File : File_Type;
   Grid : Grid_Type;
   From_X, To_X : Grid_X_Range;
   From_Y, To_Y : Grid_Y_Range;
   Start_X, End_X : Grid_X_Range;
   Start_Y, End_Y : Grid_Y_Range;
   Num : Integer;
   Comma : Character;
   Arrow : String (1 .. 4);

   Max_Y : Grid_Y_Range;

   Sand_X : Grid_X_Range;
   Sand_Y : Grid_Y_Range;
   Num_Grains : Natural;
   Got_Part_A : Boolean;

begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day14.txt");

   Grid := (others => (others => False));

   Max_Y := Grid_Y_Range'First;

   while not End_Of_File (Data_File) loop
      Get (Data_File, Num);
      if Num >= Integer (Grid_X_Range'First) and then
         Num <= Integer (Grid_X_Range'Last)
      then
         From_X := Grid_X_Range (Num);
      else
         Put_Line ("Invalid X value " & Integer'Image (Num));
         return;
      end if;
      Get (Data_File, Comma);
      Get (Data_File, Num);
      if Num >= Integer (Grid_Y_Range'First) and then
         Num <= Integer (Grid_Y_Range'Last)
      then
         From_Y := Grid_Y_Range (Num);
      else
         Put_Line ("Invalid Y value " & Integer'Image (Num));
         return;
      end if;

      while not End_Of_Line (Data_File) loop
         Get (Data_File, Arrow);
         Get (Data_File, Num);
         if Num >= Integer (Grid_X_Range'First) and then
            Num <= Integer (Grid_X_Range'Last)
         then
            To_X := Grid_X_Range (Num);
         else
            Put_Line ("Invalid X value " & Integer'Image (Num));
            return;
         end if;
         Get (Data_File, Comma);
         Get (Data_File, Num);
         if Num >= Integer (Grid_Y_Range'First) and then
            Num <= Integer (Grid_Y_Range'Last)
         then
            To_Y := Grid_Y_Range (Num);
         else
            Put_Line ("Invalid Y value " & Integer'Image (Num));
            return;
         end if;

         --  Since one of the from-to coords will be the same
         --  we can just loop like this rather than trying to
         --  figure out of the line is vertical or horizontal
         if From_X <= To_X then
            Start_X := From_X;
            End_X := To_X;
         else
            Start_X := To_X;
            End_X := From_X;
         end if;
         if From_Y <= To_Y then
            Start_Y := From_Y;
            End_Y := To_Y;
         else
            Start_Y := To_Y;
            End_Y := From_Y;
         end if;
         for x in Start_X .. End_X loop
            for y in Start_Y .. End_Y loop
               Grid (x, y) := True;
            end loop;
         end loop;

         if From_Y > Max_Y then
            Max_Y := From_Y;
         end if;
         if To_Y > Max_Y then
            Max_Y := To_Y;
         end if;

         From_X := To_X;
         From_Y := To_Y;
      end loop;
   end loop;

   Num_Grains := 0;

   Got_Part_A := False;

   while not Grid (500, 0) loop
      Sand_X := 500;
      Sand_Y := 0;

      if Num_Grains < Natural'Last then
         Num_Grains := Num_Grains + 1;
      end if;

      while Sand_Y < Grid_Y_Range'Last and then
         Sand_Y < Max_Y + 1 loop
         if not Grid (Sand_X, Sand_Y + 1) then
            Sand_Y := Sand_Y + 1;
         elsif Sand_X > Grid_X_Range'First and then
            not Grid (Sand_X - 1, Sand_Y + 1)
         then
            Sand_X := Sand_X - 1;
            Sand_Y := Sand_Y + 1;
         elsif Sand_X < Grid_X_Range'Last and then
            not Grid (Sand_X + 1, Sand_Y + 1)
         then
            Sand_X := Sand_X + 1;
            Sand_Y := Sand_Y + 1;
         else
            Grid (Sand_X, Sand_Y) := True;
            exit;
         end if;
      end loop;

      if Sand_Y = Max_Y + 1 then
         Grid (Sand_X, Sand_Y) := True;
      end if;

      if not Got_Part_A and then Sand_Y = Max_Y + 1 then
         if Num_Grains > 0 then
            Put_Line ("Part A sum: " & Natural'Image (Num_Grains - 1));
            Got_Part_A := True;
         end if;
      end if;
   end loop;

   Put_Line ("Part B sum: " & Natural'Image (Num_Grains));
end Day14;
