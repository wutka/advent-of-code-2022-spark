pragma Spark_Mode (On);
pragma Overflow_Mode (General =>  Eliminated, Assertions => Eliminated);

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Command_Line;
use Ada.Command_Line;

with Score_A;
with Score_B;

procedure Day2 is

   Data_File : File_Type;
   A_Sum : Integer;
   B_Sum : Integer;
   Opp : Character;
   Me : Character;
   Space : Character;
begin
   if Argument_Count /= 1 then
      Put_Line ("No input file supplied");
      return;
   end if;

   A_Sum := 0;
   B_Sum := 0;

   Open (File => Data_File,
         Mode => In_File,
         Name => Argument (1));

   while not End_Of_File (Data_File) loop
      Get (Data_File, Opp);
      Get (Data_File, Space);
      Get (Data_File, Me);
      Skip_Line (Data_File);

      A_Sum := A_Sum + Score_A (Opp, Me);
      B_Sum := B_Sum + Score_B (Opp, Me);
   end loop;

   Close (Data_File);

   Put ("Part A score is ");
   Put_Line (Integer'Image (A_Sum));

   Put ("Part B score is ");
   Put_Line (Integer'Image (B_Sum));

end Day2;
