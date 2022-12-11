pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Day11 is
   type Monkey_Range is range 0 .. 20;

   type Item_Range is range 1 .. 100;
   type Item_Count is range 0 .. Item_Range'Last;
   type Worry_Level_Range is range 0 .. 1000000000000000;
   type Score_Range is range 0 .. 1000000000000000;
   type Item_Array is array (Item_Range) of Worry_Level_Range;

   type Operation_Type is (Add, Mult, Double, Square);

   type Monkey is record
      Num_Items : Item_Count := 0;
      Items : Item_Array := (others => 1);
      Operation : Operation_Type := Add;
      Operation_Arg : Natural := 0;
      Divisor : Positive := 1;
      True_Monkey : Monkey_Range := 0;
      False_Monkey : Monkey_Range := 0;
      Num_Inspected : Natural := 0;
   end record;

   type Monkey_Array is array (Monkey_Range) of Monkey;

   function Get_Score (Monkeys : Monkey_Array; Last_Monkey : Monkey_Range)
      return Score_Range;

   procedure Skip_To (Data_File : File_Type; Ch : Character)
      with
      Pre => Is_Open (Data_File) and then Mode (Data_File) = In_File
   is
      Temp : Character;
   begin
      while not End_Of_Line (Data_File) loop
         Get (Data_File, Temp);
         if Temp = Ch then
            return;
         end if;
      end loop;
   end Skip_To;

   procedure Skip_To_Op (Data_File : File_Type; Op : out Character)
      with
      Pre => Is_Open (Data_File) and then Mode (Data_File) = In_File
   is
      Temp : Character;
   begin
      while not End_Of_Line (Data_File) loop
         Get (Data_File, Temp);
         if Temp = '+' or else Temp = '*' then
            Op := Temp;
            return;
         end if;
      end loop;
      Op := ' ';
   end Skip_To_Op;

   procedure Process_Monkey (m : Monkey_Range;
      Monkeys : in out Monkey_Array;
      Divide_By_3 : Boolean;
      Mod_Val : Positive) is
      Worry_Level : Worry_Level_Range;
      Item : Worry_Level_Range;
      Monkey_Num : Monkey_Range;
   begin
      while Monkeys (m).Num_Items > 0 loop
         Item := Monkeys (m).Items (Item_Range (Monkeys (m).Num_Items));
         Monkeys (m).Num_Items := Monkeys (m).Num_Items - 1;

         case Monkeys (m).Operation is
            when Add =>
               if Worry_Level_Range'Last - Item >
                  Worry_Level_Range (Monkeys (m).Operation_Arg)
               then
                  Worry_Level := Worry_Level_Range (Monkeys (m).Operation_Arg)
                     + Item;
               else
                  Put_Line ("Worry level overflow +");
                  return;
               end if;
            when Mult =>
               if Item = 0 or else Worry_Level_Range'Last / Item >
                  Worry_Level_Range (Monkeys (m).Operation_Arg)
               then
                  Worry_Level := Worry_Level_Range (Monkeys (m).Operation_Arg)
                     * Item;
               else
                  Put_Line ("Worry level overflow *");
                  return;
               end if;
            when Double =>
               if Worry_Level_Range'Last - Item > Item then
                  Worry_Level := Item + Item;
               else
                  Put_Line ("Worry level overflow 2x");
                  return;
               end if;
            when Square =>
               if Item = 0 or else Worry_Level_Range'Last / Item > Item then
                  Worry_Level := Item * Item;
               else
                  Put_Line ("Worry level overflow ^2");
                  Put_Line (Worry_Level_Range'Image (Item));
                  return;
               end if;
         end case;

         if Divide_By_3 then
            Worry_Level := Worry_Level / 3;
         end if;

         Worry_Level := Worry_Level mod Worry_Level_Range (Mod_Val);

         if Worry_Level mod Worry_Level_Range (Monkeys (m).Divisor) = 0 then
            Monkey_Num := Monkeys (m).True_Monkey;
         else
            Monkey_Num := Monkeys (m).False_Monkey;
         end if;

         if Monkeys (Monkey_Num).Num_Items < Item_Count'Last then
            Monkeys (Monkey_Num).Num_Items :=
               Monkeys (Monkey_Num).Num_Items + 1;
            Monkeys (Monkey_Num).Items (
               Item_Range (Monkeys (Monkey_Num).Num_Items)) := Worry_Level;
         else
            Put_Line ("Monkey has too many items");
         end if;

         if Monkeys (m).Num_Inspected < Natural'Last then
            Monkeys (m).Num_Inspected := Monkeys (m).Num_Inspected + 1;
         else
            Put_Line ("Monkey has done too much");
         end if;
      end loop;
   end Process_Monkey;

   function Get_Score (Monkeys : Monkey_Array; Last_Monkey : Monkey_Range)
      return Score_Range
   is
      Highest : Natural := 0;
      Next_Highest : Natural := 0;
   begin
      for i in 0 .. Last_Monkey loop
         if Monkeys (i).Num_Inspected > Highest then
            Next_Highest := Highest;
            Highest := Monkeys (i).Num_Inspected;
         elsif Monkeys (i).Num_Inspected > Next_Highest then
            Next_Highest := Monkeys (i).Num_Inspected;
         end if;
      end loop;

      if Highest = 0 or else Score_Range'Last / Score_Range (Highest) >
         Score_Range (Next_Highest)
      then
         return Score_Range (Highest) * Score_Range (Next_Highest);
      else
         return 0;
      end if;
   end Get_Score;

   Data_File : File_Type;
   Dummy : Character;
   Val : Integer;
   OpChar : Character;
   EOL : Boolean;

   Monkeys : Monkey_Array;
   B_Monkeys : Monkey_Array;
   Next_Monkey : Monkey_Range;
   Last_Monkey : Monkey_Range;
   Mod_Val : Positive;

begin
   Open (File => Data_File,
         Mode => In_File,
--         Name => "test.txt");
         Name => "data/day11.txt");

   Monkeys := (others => <>);
   Next_Monkey := 0;
   Mod_Val := 1;
   Last_Monkey := 0;

   while not End_Of_File (Data_File) loop
      Skip_Line (Data_File);

      Skip_To (Data_File, ':');
      Get (Data_File, Dummy);

      Monkeys (Next_Monkey).Num_Items := 0;
      while not End_Of_Line (Data_File) loop
         Get (Data_File, Val);

         if Monkeys (Next_Monkey).Num_Items < Item_Count'Last then
            Monkeys (Next_Monkey).Num_Items :=
               Monkeys (Next_Monkey).Num_Items + 1;
         else
            Put_Line ("Too many items");
            return;
         end if;

         if Val >= 0 then
            Monkeys (Next_Monkey).Items (Item_Range (
               Monkeys (Next_Monkey).Num_Items)) :=
               Worry_Level_Range (Val);
         else
            Put_Line ("Invalid item");
            return;
         end if;

         if not End_Of_Line (Data_File) then
            Get (Data_File, Dummy);
            Get (Data_File, Dummy);
         end if;
      end loop;
      Skip_Line (Data_File);

      Skip_To_Op (Data_File, OpChar);
      if OpChar = '+' then
         Monkeys (Next_Monkey).Operation := Add;
      elsif OpChar = '*' then
         Monkeys (Next_Monkey).Operation := Mult;
      else
         Put_Line ("Invalid op char");
         return;
      end if;

      Get (Data_File, Dummy);
      Look_Ahead (Data_File, Dummy, EOL);
      if Dummy in '0' .. '9' then
         Get (Data_File, Val);
         if Val >= 0 then
            Monkeys (Next_Monkey).Operation_Arg := Natural (Val);
         else
            Put_Line ("Unexpected negative in operation");
            return;
         end if;
      else
         if Monkeys (Next_Monkey).Operation = Add then
            Monkeys (Next_Monkey).Operation := Double;
         else
            Monkeys (Next_Monkey).Operation := Square;
         end if;
      end if;
      Skip_Line (Data_File);

      Skip_To (Data_File, 'y');
      Get (Data_File, Dummy);
      Get (Data_File, Val);

      if Val >= 1 then
         Monkeys (Next_Monkey).Divisor := Positive (Val);
         if Positive'Last / Val > Mod_Val then
            Mod_Val := Mod_Val * Val;
         else
            Put_Line ("Mod val overflow");
         end if;
      else
         Put_Line ("Invalid divisor");
         return;
      end if;
      Skip_Line (Data_File);

      Skip_To (Data_File, 'y');
      Get (Data_File, Dummy);
      Get (Data_File, Val);

      if Val >= 0 and then Val <= Integer (Monkey_Range'Last) then
         Monkeys (Next_Monkey).True_Monkey := Monkey_Range (Val);
      else
         Put_Line ("Invalid true monkey");
         return;
      end if;

      Skip_Line (Data_File);

      Skip_To (Data_File, 'y');
      Get (Data_File, Dummy);
      Get (Data_File, Val);

      if Val >= 0 and then Val <= Integer (Monkey_Range'Last) then
         Monkeys (Next_Monkey).False_Monkey := Monkey_Range (Val);
      else
         Put_Line ("Invalid false monkey");
         return;
      end if;

      Skip_Line (Data_File);

      if not End_Of_File (Data_File) then
         Skip_Line (Data_File);
      end if;

      if Next_Monkey < Monkey_Range'Last then
         Last_Monkey := Next_Monkey;
         Next_Monkey := Next_Monkey + 1;
      else
         Put_Line ("Too many monkeys");
      end if;

   end loop;

   B_Monkeys := Monkeys;

   for i in 1 .. 20 loop
      for m in 0 .. Last_Monkey loop
         Process_Monkey (m, Monkeys, True, Mod_Val);
      end loop;

--      for m in 0 .. Last_Monkey loop
--         Put (Monkey_Range'Image (m));
--         Put (": ");
--         if Monkeys (m).Num_Items > 0 then
--            for i in 1 .. Monkeys (m).Num_Items loop
--               Put (Natural'Image (Monkeys (m).Items (Item_Range (i))));
--               Put (" ");
--            end loop;
--         end if;
--         Put_Line ("");
--      end loop;
   end loop;

   Put_Line ("Part A score: " &
      Score_Range'Image (Get_Score (Monkeys, Last_Monkey)));

   for i in 1 .. 10000 loop
      for m in 0 .. Last_Monkey loop
         Process_Monkey (m, B_Monkeys, False, Mod_Val);
      end loop;

--      for m in 0 .. Last_Monkey loop
--         Put (Monkey_Range'Image (m));
--         Put (": ");
--         if Monkeys (m).Num_Items > 0 then
--            for i in 1 .. Monkeys (m).Num_Items loop
--               Put (Natural'Image (Monkeys (m).Items (Item_Range (i))));
--               Put (" ");
--            end loop;
--         end if;
--         Put_Line ("");
--      end loop;
   end loop;

   Put_Line ("Part A score: " &
      Score_Range'Image (Get_Score (B_Monkeys, Last_Monkey)));

end Day11;
