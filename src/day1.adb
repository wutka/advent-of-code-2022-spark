pragma Spark_Mode (On);
pragma Overflow_Mode (General =>  Eliminated, Assertions => Eliminated);

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Command_Line;
use Ada.Command_Line;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Day1 is

   Data_File : File_Type;
   Depth : Integer;
   Sum : Integer := 0;
begin
   if Argument_Count /= 1 then
      Put_Line ("No input file supplied");
      return;
   end if;

   Open (File => Data_File,
         Mode => In_File,
         Name => Argument (1));

   while not End_Of_File (Data_File) loop
      Get (Data_File, Item => Depth);
      Skip_Line;
      if Sum >= 0 and Depth > 0 and Depth < 1000000000 and Integer'Last - Depth > Sum then
         Sum := Sum + Depth;
      end if;
   end loop;

   Close (Data_File);

   Put ("The sum is ");
   Put_Line (Natural'Image (Sum));

end Day1;
