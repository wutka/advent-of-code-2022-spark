pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Day4 is
   function Contains (l1 : Integer; r1 : Integer;
      l2 : Integer; r2 : Integer) return Boolean;
   function Overlaps (l1 : Integer; r1 : Integer;
      l2 : Integer; r2 : Integer) return Boolean;

   function Contains (l1 : Integer; r1 : Integer;
      l2 : Integer; r2 : Integer) return Boolean is
   begin
      return ((l1 <= l2) and then (r1 >= r2)) or else
             ((l2 <= l1) and then (r2 >= r1));
   end Contains;

   function Overlaps (l1 : Integer; r1 : Integer;
      l2 : Integer; r2 : Integer) return Boolean is
   begin
      return ((l1 >= l2) and then (l1 <= r2)) or else
             ((r1 >= l2) and then (r1 <= r2)) or else
             ((l2 >= l1) and then (l2 <= r1)) or else
             ((r2 >= l1) and then (r2 <= r1));
   end Overlaps;

   Data_File : File_Type;
   l1 : Integer;
   r1 : Integer;
   l2 : Integer;
   r2 : Integer;
   dummy : Character;

   A_Count : Natural;
   B_Count : Natural;

begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day4.txt");

   A_Count := 0;
   B_Count := 0;

   while not End_Of_File (Data_File) loop
      Get (Data_File, l1);
      Get (Data_File, dummy);
      Get (Data_File, r1);
      Get (Data_File, dummy);
      Get (Data_File, l2);
      Get (Data_File, dummy);
      Get (Data_File, r2);
      Skip_Line (Data_File);

      if Contains (l1, r1, l2, r2) and then Natural'Last > A_Count then
         A_Count := A_Count + 1;
      end if;

      if Overlaps (l1, r1, l2, r2) and then Natural'Last > B_Count then
         B_Count := B_Count + 1;
      end if;
   end loop;

   Put ("Part A Count: ");
   Put_Line (Natural'Image (A_Count));

   Put ("Part B Count: ");
   Put_Line (Natural'Image (B_Count));
end Day4;
