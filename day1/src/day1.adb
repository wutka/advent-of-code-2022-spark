pragma Spark_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;
procedure Day1 is

   Data_File : File_Type;
   Group_Sum : Natural;
   Calories : Natural;
   FileCal : Integer;
   Best_3 : array (1 .. 3) of Natural;
begin

   Group_Sum := 0;
   Best_3 := (0, 0, 0);

   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day1.txt");

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
         Get (Data_File, FileCal);
         if FileCal >= 0 then
            Calories := Natural (FileCal);
            if Natural'Last - Calories > Group_Sum then
               Group_Sum := Group_Sum + Calories;
            end if;
         end if;
      end if;
      Skip_Line (Data_File);
   end loop;

   Put ("Part A value is ");
   Put_Line (Integer'Image (Best_3 (1)));

   Put ("Part B value is ");
   if Natural'Last - Best_3 (2) > Best_3 (3)
      and then Natural'Last - Best_3 (1) > Best_3 (2) + Best_3 (3)
   then
      Put_Line (Integer'Image (Best_3 (1) + Best_3 (2) + Best_3 (3)));
   else
      Put_Line (" too big.");
   end if;

end Day1;
