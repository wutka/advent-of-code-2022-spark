pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Day10 is

   Register : Integer;
   Clock : Natural;
   Column : Natural;
   Clock_Incr : Natural;
   Data_File : File_Type;
   Instr : String (1 .. 4);
   Arg : Integer;
   Result : Integer;
   Combo : Integer;
   Pixel_Row : String (1 .. 40);
begin
   Result := 0;
   Register := 1;
   Clock := 0;

   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day10.txt");

   Pixel_Row := (others => '.');

   while not End_Of_File (Data_File) loop

      Get (Data_File, Instr);

      if Instr = "noop" then
         Clock_Incr := 1;
      elsif Instr = "addx" then
         Clock_Incr := 2;
      else
         Put_Line ("Unknown instruction " & Instr);
         return;
      end if;

      for i in 1 .. Clock_Incr loop
         if Clock < Natural'Last then
            Clock := Clock + 1;
         else
            Put_Line ("The clock has run out");
               return;
         end if;

         if Clock > 0 then
            Column := (Clock - 1) mod 40;

            if ((Column = 0 and then Register >= 0) or else
                (Column > 0 and then Register >= Integer (Column - 1)))
               and then Register <= Integer (Column + 1)
            then
               Pixel_Row (Column + 1) := '#';
            end if;

            if Column = 39 then
               Put_Line (Pixel_Row);
               Pixel_Row := (others => '.');
            end if;
         end if;

         if Clock = 20 then
            if Register = 0 or else
               (Register < 0 and then
                Integer'First / Integer (Clock) < Register) or else
               (Register > 0 and then
                Integer'Last / Integer (Clock) > Register)
            then
               Result := Register * Integer (Clock);
            else
               Put_Line ("Register * Clock overflows");
               return;
            end if;
         elsif Clock = 60 or else Clock = 100 or else Clock = 140
            or else Clock = 180 or else Clock = 220
         then
            if Register = 0 or else
               (Register < 0 and then
                Integer'First / Integer (Clock) < Register) or else
               (Register > 0 and then
                Integer'Last / Integer (Clock) > Register)
            then
               Combo := Register * Integer (Clock);
            else
               Put_Line ("Register * Clock overflows");
               return;
            end if;

            if (Combo >= 0 and then Integer'Last - Combo > Result) or else
               (Combo < 0 and then Integer'First - Combo < Result)
            then
               Result := Result + Combo;
            else
               Put_Line ("Result sum overflows");
               return;
            end if;
         end if;
      end loop;

      if Instr (1 .. 4) = "addx" then
         Get (Data_File, Arg);
         if (Arg >= 0 and then Integer'Last - Arg > Register) or else
            (Arg < 0 and then Integer'First - Arg < Register)
         then
            Register := Register + Arg;
         else
            Put_Line ("Addition overflows");
            return;
         end if;
      end if;
   end loop;

   Put_Line ("Part A result: " & Integer'Image (Result));
end Day10;
