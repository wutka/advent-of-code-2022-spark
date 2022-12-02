pragma Spark_Mode (On);
pragma Overflow_Mode (General =>  Eliminated, Assertions => Eliminated);

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
with Ada.Command_Line;
use Ada.Command_Line;

procedure Day1 is

   Data_File : File_Type;
   Group_Sum : Integer;
   Calories : Integer;
   Best_3 : array (1 .. 3) of Integer;
begin
   if Argument_Count /= 1 then
      Put_Line ("No input file supplied");
      return;
   end if;

   Group_Sum := 0;
   Best_3 := (0, 0, 0);

   Open (File => Data_File,
         Mode => In_File,
         Name => Argument (1));

   while not End_Of_File (Data_File) loop
      if End_Of_Line (Data_File) then
         if Group_Sum > Best_3 (1) then
            Best_3 (3) := Best_3 (2);
            Best_3 (2) := Best_3 (1);
            Best_3 (1) := Group_Sum;
         elsif Group_Sum > Best_3 (2) then
            Best_3 (3) := Best_3 (2);
            Best_3 (2) := Group_Sum;
         elsif Group_Sum > Best_3 (3) then
            Best_3 (3) := Group_Sum;
         end if;
         Group_Sum := 0;
      else
         Get (Data_File, Calories);
         Group_Sum := Group_Sum + Calories;
      end if;
      Skip_Line (Data_File);
   end loop;

   Close (Data_File);

   Put ("Part A value is ");
   Put_Line (Integer'Image (Best_3 (1)));

   Put ("Part B value is ");
   Put_Line (Integer'Image (Best_3 (1) + Best_3 (2) + Best_3 (3)));

end Day1;
