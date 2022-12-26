pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;
procedure Day25 is

   type Build_Range is range 1 .. 100;

   function String_To_Snafu (Snafu : String) return Long_Integer
   is
      Mult : Long_Integer;
      Digit_Val : Long_Integer;
      Digit_Mul : Long_Integer;
      Sum : Long_Integer;
   begin
      Sum := 0;
      Mult := 1;
      for i in reverse Snafu'Range loop
         case Snafu (i) is
            when '2' => Digit_Val := 2;
            when '1' => Digit_Val := 1;
            when '0' => Digit_Val := 0;
            when '-' => Digit_Val := -1;
            when '=' => Digit_Val := -2;
            when others => Digit_Val := 0;
         end case;

         if Sum < 1000000000000000 and then
            Sum > -1000000000000000
         then
            pragma Assert (Digit_Val >= -2 and then Digit_Val <= 2);
            if Mult < 10000000000000 and then
               Mult > -10000000000000
            then
               Digit_Mul := Digit_Val * Mult;
            else
               return 0;
            end if;
            Sum := Sum + Digit_Mul;
            Mult := Mult * 5;
         end if;
      end loop;
      return Sum;
   end String_To_Snafu;

   procedure Snafu_To_String (Snafu : Long_Integer;
      Str : in out String; Str_Len : out Natural)
   with Pre => Str'First = 1 and then Str'Last < 200
   is
      Digit_Vals : array (Build_Range) of Integer;
      Next_Digit : Build_Range;
      Ch : Character;
      Snafu_Val : Long_Integer;
      Str_End : Integer;
   begin
      Digit_Vals := (others => -10);
      Snafu_Val := Snafu;

      if Snafu = 0 then
         if Str'Length > 0 then
            Str (Str'First) := '0';
            Str_Len := 1;
         else
            Str_Len := 0;
         end if;
         return;
      end if;

      Next_Digit := 1;
      while Snafu_Val > 0 loop
         Digit_Vals (Next_Digit) := Integer (Snafu_Val mod 5);
         if Next_Digit < Build_Range'Last then
            Next_Digit := Next_Digit + 1;
         end if;
         Snafu_Val := Snafu_Val / 5;
      end loop;

      if Next_Digit > Build_Range'First then
         Next_Digit := Next_Digit - 1;
      end if;
      for i in 1 .. Next_Digit loop
         while Digit_Vals (i) > 2 loop
            Digit_Vals (i) := Digit_Vals (i) - 5;
            if Integer'Last > Digit_Vals (i + 1) then
               Digit_Vals (i + 1) := Digit_Vals (i + 1) + 1;
            end if;
         end loop;
      end loop;

      while Next_Digit < Build_Range'Last - 1 and then
         Digit_Vals (Next_Digit + 1) /= -10
      loop
         Next_Digit := Next_Digit + 1;
         while Digit_Vals (Next_Digit) > 2 loop
            Digit_Vals (Next_Digit) := Digit_Vals (Next_Digit) - 5;
            if Integer'Last > Digit_Vals (Next_Digit + 1) then
               Digit_Vals (Next_Digit + 1) := Digit_Vals (Next_Digit + 1) + 1;
            end if;
         end loop;
      end loop;

      Str_End := Str'First;
      if Str'First >= 0 and then
         Integer'Last - Str'First > Integer (Next_Digit) - 1
      then
         Str_End := Str'First + Integer (Next_Digit) - 1;
      end if;

      for i in 1 .. Next_Digit loop
         if Str'Last > 0 and then
            Integer'Last - Str'Last > Str'First and then
            Integer (i) > (Str'Last - Str'First + 1)
         then
            Str_Len := Str'Last - Str'First + 1;
            return;
         end if;

         case Digit_Vals (i) is
            when 2 => Ch := '2';
            when 1 => Ch := '1';
            when 0 => Ch := '0';
            when -1 => Ch := '-';
            when -2 => Ch := '=';
            when others => Ch := '?';
         end case;
         if Str_End >= Integer (i) and then
            Str_End + Integer (i) + 1 >= Str'First and then
            Str_End + Integer (i) + 1 <= Str'Last
         then
            Str (Str_End - Integer (i) + 1) := Ch;
         end if;
      end loop;

      Str_Len := Natural (Next_Digit);
   end Snafu_To_String;

   Data_File : File_Type;
   Line : String (1 .. 100);
   Len : Natural;
   Snafu_Num : Long_Integer;
   Snafu_Sum : Long_Integer;

begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day25.txt");

   Snafu_Sum := 0;

   while not End_Of_File (Data_File) loop
      Get_Line (Data_File, Line, Len);

      if Len > 0 then
         Snafu_Num := String_To_Snafu (Line (1 .. Len));
         if Snafu_Num >= 0 and then
            Long_Integer'Last - Snafu_Num > Snafu_Sum
         then
            Snafu_Sum := Snafu_Sum + Snafu_Num;
         end if;
      end if;
   end loop;

   Line := (others => ' ');
   Snafu_To_String (Snafu_Sum, Line, Len);
   if Len >= 1 and then Len <= Line'Last
   then
      Put_Line ("Part A value is " & Line (1 .. Len));
   end if;
end Day25;
