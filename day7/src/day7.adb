pragma SPARK_Mode (On);

--  The original version kept track of the directory
--  structure, but reznikmm implemented the parser as
--  a recursive function and I realized that I didn't
--  need to keep track of directories, just the sizes
--
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Day7 is

   type Sizes_Array is array (1 .. 500) of Natural;

   procedure Parse_Data (Data_File : File_Type;
                         Sizes : in out Sizes_Array;
                         Num_Sizes : in out Natural;
                         Last_Size : out Natural)
   with
      Pre => Is_Open (Data_File) and then Mode (Data_File) = In_File
   is
      Curr_Size : Natural;
      Ch : Character;
      End_Of_Line : Boolean;
      Size : Integer;
      Line : String (1 .. 30);
      Line_Len : Integer;
      Child_Size : Natural;
   begin
      Curr_Size := 0;

      while not End_Of_File (Data_File) loop
         Look_Ahead (Data_File, Ch, End_Of_Line);
         if not End_Of_Line then
            --  Is this line a file size?
            if Ch in '0' .. '9' then
               Get (Data_File, Size);
               Skip_Line (Data_File);
               if Size >= 0 and then
                  Natural'Last - Curr_Size > Size
               then
                  Curr_Size := Curr_Size + Natural (Size);
               else
                  Put_Line ("File size is too large");
               end if;

            --  Otherwise, is this line a command?
            elsif Ch = '$' then
               Get_Line (Data_File, Line, Line_Len);
               --  Look for traverse back up
               if Line_Len = 7 and then Line (1 .. 7) = "$ cd .." then
                  Last_Size := Curr_Size;
                  if Num_Sizes < Sizes'Last then
                     Num_Sizes := Num_Sizes + 1;
                     Sizes (Num_Sizes) := Last_Size;
                  else
                     Put_Line ("Too many sizes");
                  end if;
                  --  To traverse back up, just return to the
                  --  previous caller
                  return;
               elsif Line_Len > 5 and then Line (1 .. 5) = "$ cd "
                  and then Line (6) /= '/'
               then
                  --  If traversing to a subdirectory, recursively
                  --  call the parser to parse that directory
                  Parse_Data (Data_File, Sizes, Num_Sizes, Child_Size);
                  if Natural'Last - Curr_Size > Child_Size then
                     Curr_Size := Curr_Size + Child_Size;
                  else
                     Put_Line ("Sum is too large");
                  end if;
               end if;
            else
               Skip_Line (Data_File);
            end if;
         end if;
      end loop;

      Last_Size := Curr_Size;
      if Num_Sizes < Sizes'Last then
         Num_Sizes := Num_Sizes + 1;
         Sizes (Num_Sizes) := Last_Size;
      else
         Put_Line ("Too many sizes");
      end if;
      return;
   end Parse_Data;

   Data_File : File_Type;
   Sizes : Sizes_Array;
   Num_Sizes : Natural;
   Root_Size : Natural;
   Total_Size : Natural;
   Best_Dir_Size : Natural;
   Space_Needed : Natural;

begin
   Sizes := (others => 0);
   Num_Sizes := 0;

   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day7.txt");

   Parse_Data (Data_File, Sizes, Num_Sizes, Root_Size);

   Total_Size := 0;

   if Num_Sizes > Sizes'Last then
      Put_Line ("Size overflow");
      return;
   end if;

   for i in 1 .. Num_Sizes loop
      if Sizes (i) <= 100000 then
         if Natural'Last - Sizes (i) > Total_Size then
            Total_Size := Total_Size + Sizes (i);
         else
            Put_Line ("Directory sum is too big");
            return;
         end if;
      end if;
   end loop;

   Put_Line ("Part A sum: " & Natural'Image (Total_Size));

   if Natural'Last - Root_Size > 30000000
      and then Root_Size + 30000000 > 70000000
   then
      Best_Dir_Size := Root_Size;
      Space_Needed := Root_Size - 40000000;

      for i in 1 .. Num_Sizes loop
         if Sizes (i) >= Space_Needed
            and then Sizes (i) < Best_Dir_Size
         then
            Best_Dir_Size := Sizes (i);
         end if;
      end loop;
      Put_Line ("Part B value: " & Natural'Image (Best_Dir_Size));
   else
      Put_Line ("There is already enough space");
   end if;
end Day7;
