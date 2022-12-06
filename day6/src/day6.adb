pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;

procedure Day6 is
   function Unique_Pos (Str : String; Num_Chars : Natural; Limit : Natural)
      return Natural
      with
         Pre => Limit >= 1 and then Limit < Num_Chars
         and then Str'First = 1
         and then Num_Chars <= Str'Last;

   function Unique_Pos (Str : String; Num_Chars : Natural; Limit : Natural)
      return Natural
   is
      Unique_Start : Natural;
   begin
      Unique_Start := 1;

      for i in 2 .. Num_Chars loop
         pragma Loop_Invariant (Unique_Start >= 1
            and then Unique_Start < Num_Chars);

         for j in Unique_Start .. i - 1 loop
            pragma Loop_Invariant (Unique_Start >= 1
               and then Unique_Start < Num_Chars);

            if Str (i) = Str (j) then
               Unique_Start := j + 1;
            end if;
         end loop;
         if i - Unique_Start + 1 >= Limit then
            return i;
         end if;
      end loop;
      return 0;
   end Unique_Pos;

   Data_File : File_Type;
   Line : String (1 .. 5000);
   Num_Chars : Natural;
begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day6.txt");

   Get_Line (Data_File, Line, Num_Chars);

   if Num_Chars > 4 then
      Put_Line ("Part A pos: " &
         Natural'Image (Unique_Pos (Line, Num_Chars, 4)));
   end if;

   if Num_Chars > 14 then
      Put_Line ("Part B pos: " &
         Natural'Image (Unique_Pos (Line, Num_Chars, 14)));
   end if;

end Day6;
