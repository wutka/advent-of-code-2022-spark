pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Day20 is
   type Num_Range is range 1 .. 6000;

   type Num_Info is record
      Num : Long_Integer := 0;
      Initial_Pos : Num_Range := 1;
   end record;

   type Num_Array is array (Num_Range) of Num_Info;

   procedure Move (Initial_Pos : Num_Range; Nums : in out Num_Array;
      Num_Numbers : Num_Range)
      with Pre => Num_Numbers > 1
   is
      Start_Pos, End_Pos : Num_Range;
      Dist : Long_Integer;
      Temp : Num_Info;
      End_Num : Long_Integer;
      Left : Boolean;
   begin
      Start_Pos := 1;
      for i in 1 .. Num_Numbers loop
         if Nums (i).Initial_Pos = Initial_Pos then
            Start_Pos := i;
            exit;
         end if;
      end loop;

      Temp := Nums (Start_Pos);

      Dist := Temp.Num;

      Left := False;
      if Dist < 0 and then Dist > Long_Integer'First then
         Left := True;
         Dist := -Dist;
      end if;

      Dist := Dist mod Long_Integer (Num_Numbers - 1);

      if Dist = 0 then
         return;
      end if;

      if Left then
         End_Num := (Long_Integer (Start_Pos) - Dist) mod
            Long_Integer (Num_Numbers - 1);
      else
         End_Num := (Long_Integer (Start_Pos) + Dist) mod
            Long_Integer (Num_Numbers - 1);
      end if;

      if End_Num = 1 then
         End_Pos := Num_Numbers;
      elsif End_Num = 0 then
         End_Pos := Num_Numbers - 1;
      else
         End_Pos := Num_Range (End_Num);
      end if;

      if Start_Pos < Num_Numbers then
         Nums (Start_Pos .. Num_Numbers - 1) :=
            Nums (Start_Pos + 1 .. Num_Numbers);
      end if;

      Nums (End_Pos + 1 .. Num_Numbers) :=
         Nums (End_Pos .. Num_Numbers - 1);
      Nums (End_Pos) := Temp;
   end Move;

   Data_File : File_Type;
   Val : Integer;
   Num_Numbers : Num_Range;

   Nums : Num_Array;
   Nums_B : Num_Array;
   Part_A_Sum : Long_Integer;
   Part_B_Sum : Long_Integer;
   Pos : Num_Range;
   Next_Pos : Long_Integer;
   Offsets : constant array (1 .. 3) of Long_Integer  := (1000, 2000, 3000);

begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day20.txt");

   Num_Numbers := 1;

   while not End_Of_File (Data_File) loop
      Get (Data_File, Val);

      Nums (Num_Numbers) := (Long_Integer (Val), Num_Numbers);

      if Num_Numbers < Num_Range'Last then
         Num_Numbers := Num_Numbers + 1;
      end if;
   end loop;

   if Num_Numbers > 1 then
      Num_Numbers := Num_Numbers - 1;
   end if;

   if Num_Numbers < 2 then
      return;
   end if;

   Nums_B := Nums;

   for i in 1 .. Num_Numbers loop
      Move (i, Nums, Num_Numbers);
   end loop;

   Pos := 1;
   for i in 1 .. Num_Numbers loop
      if Nums (i).Num = 0 then
         Pos := i;
         exit;
      end if;
   end loop;

   Part_A_Sum := 0;
   for i in Offsets'Range loop
      Next_Pos := (Long_Integer (Pos) + Offsets (i))
         mod Long_Integer (Num_Numbers);
      if Next_Pos = 0 then
         Next_Pos := Long_Integer (Num_Numbers);
      end if;
      if (Part_A_Sum >= 0 and then Long_Integer'Last - Part_A_Sum >
          Nums (Num_Range (Next_Pos)).Num) or else
         (Part_A_Sum < 0 and then Long_Integer'First - Part_A_Sum <
          Nums (Num_Range (Next_Pos)).Num)
      then
         Part_A_Sum := Part_A_Sum + Nums (Num_Range (Next_Pos)).Num;
      end if;
   end loop;

   Put_Line ("Part A Sum: " & Long_Integer'Image (Part_A_Sum));

   for i in 1 .. Num_Numbers loop
      if (Nums_B (i).Num >= 0 and then
            Long_Integer'Last / Long_Integer (811589153) >
            Nums_B (i).Num) or else
         (Nums_B (i).Num < 0 and then
            Long_Integer'First / Long_Integer (811589153) <
            Nums_B (i).Num)
      then
         Nums_B (i).Num := Nums_B (i).Num * Long_Integer (811589153);
      end if;
   end loop;

   for i in 1 .. 10 loop
      for j in 1 .. Num_Numbers loop
         Move (j, Nums_B, Num_Numbers);
      end loop;
   end loop;

   Pos := 1;
   for i in 1 .. Num_Numbers loop
      if Nums_B (i).Num = 0 then
         Pos := i;
         exit;
      end if;
   end loop;

   Part_B_Sum := 0;
   for i in Offsets'Range loop
      Next_Pos := (Long_Integer (Pos) + Offsets (i))
         mod Long_Integer (Num_Numbers);
      if Next_Pos = 0 then
         Next_Pos := Long_Integer (Num_Numbers);
      end if;
      if (Part_B_Sum >= 0 and then Long_Integer'Last - Part_B_Sum >
          Nums_B (Num_Range (Next_Pos)).Num) or else
         (Part_B_Sum < 0 and then Long_Integer'First - Part_B_Sum <
          Nums_B (Num_Range (Next_Pos)).Num)
      then
         Part_B_Sum := Part_B_Sum + Nums_B (Num_Range (Next_Pos)).Num;
      end if;
   end loop;

   Put_Line ("Part B Sum: " & Long_Integer'Image (Part_B_Sum));

end Day20;
