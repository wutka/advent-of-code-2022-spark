pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Day7 is
   type Dir_Range is range 0 .. 500;
   type Dir_Name is new String (1 .. 10);

   type Directory is record
      Name : Dir_Name;
      Name_Len : Natural;
      Size : Natural;
      Parent : Dir_Range;
   end record;

   type Dir_Array is array (Dir_Range) of Directory;

   Data_File : File_Type;
   Dirs : Dir_Array;
   Num_Dirs : Dir_Range;
   Dummy : Directory;
   Size : Integer;
   Ch : Character;
   End_Of_Line : Boolean;
   Curr_Dir : Dir_Range;
   Line : String (1 .. 100);
   Line_Len : Natural;
   Temp_Dir_Name : Dir_Name;
   Total_Size : Natural;
   Found_Dir : Boolean;
   Temp_Dir : Dir_Range;
   Best_Dir_Size : Natural;
   Space_Needed : Natural;

begin
   --  Create a dummy for initializing dir array
   Dummy := (Name => "          ",
             Name_Len => 0,
             Size => 0,
             Parent => 0);
   Dirs := (others => Dummy);

   --  Create a root directory
   Dirs (0) := (Name => "/         ",
                Name_Len => 1,
                Size => 0,
                Parent => 0);
   Num_Dirs := 1;
   Curr_Dir := 0;

   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day7.txt");

   while not End_Of_File (Data_File) loop
      Look_Ahead (Data_File, Ch, End_Of_Line);
      if not End_Of_Line then
         --  If the first digit is a number, this is a file size
         if Ch >= '0' and then Ch <= '9' then
            --  Get the file size
            Get (Data_File, Size);
            Skip_Line (Data_File);

            if Size > 0 and then Size < Integer'Last and then
               Natural'Last - Size > Dirs (Curr_Dir).Size
            then
               --  Add this size to the parent size
               Dirs (Curr_Dir).Size := Dirs (Curr_Dir).Size + Size;

               --  Now walk back up to root and propagate this size
               Temp_Dir := Curr_Dir;
               while Temp_Dir /= 0 loop
                  Temp_Dir := Dirs (Temp_Dir).Parent;
                  if Natural'Last - Size > Dirs (Temp_Dir).Size then
                     Dirs (Temp_Dir).Size := Dirs (Temp_Dir).Size + Size;
                  else
                     Put_Line ("Parent directory is too big");
                     return;
                  end if;
               end loop;
            else
               Put_Line ("Directory is too big");
               return;
            end if;
         elsif Ch = '$' then
            --  If the first char is a $, look for "cd"
            Get_Line (Data_File, Line, Line_Len);
            if Line_Len >= 4 then
               --  Is this a "cd" ?
               if Line (3) = 'c' and then Line_Len < 14 then
                  --  See if it is cd ..
                  if Line (6) = '.' then
                     Curr_Dir := Dirs (Curr_Dir).Parent;
                  --  Or if it is cd /
                  elsif Line (6) = '/' then
                     Curr_Dir := 0;
                  else
                     --  Otherwise, extract the dir name
                     Temp_Dir_Name := "          ";
                     for i in 1 .. Line_Len - 5 loop
                        Temp_Dir_Name (i) := Line (i + 5);
                     end loop;

                     --  Look for a directory with that name
                     --  whose parent is the current dir
                     Found_Dir := False;
                     for i in 0 .. Num_Dirs loop
                        if Dirs (i).Name_Len = Line_Len - 5 and then
                           Dirs (i).Name (1 .. Line_Len - 5) =
                           Temp_Dir_Name (1 .. Line_Len - 5) and then
                           Dirs (i).Parent = Curr_Dir
                        then
                           Curr_Dir := i;
                           Found_Dir := True;
                           exit;
                        end if;
                     end loop;
                     if not Found_Dir then
                        Put_Line ("Tried to cd to unknown directory " &
                           String (Temp_Dir_Name (1 .. Line_Len - 5))
                           & ".");
                        return;
                     end if;
                  end if;
               end if;
            end if;
         elsif Ch = 'd' then
            --  If this is a dir line, create a new dir with
            --  the current dir as a parent
            Get_Line (Data_File, Line, Line_Len);
            if Line_Len > 4 then
               if Num_Dirs < Dirs'Last and then
                  Line_Len < 14
               then
                  Temp_Dir_Name := "          ";
                  for i in 1 .. Line_Len - 4 loop
                     Temp_Dir_Name (i) := Line (i + 4);
                  end loop;
                  Dirs (Num_Dirs) :=
                     (Name => Temp_Dir_Name,
                      Name_Len => Line_Len - 4,
                      Size => 0,
                      Parent => Curr_Dir);
                  Num_Dirs := Num_Dirs + 1;
               else
                  Put_Line ("Too many directories");
                  return;
               end if;
            end if;
         end if;
      end if;
   end loop;

   Total_Size := 0;

   for i in 0 .. Num_Dirs loop
      if Dirs (i).Size <= 100000 then
         if Natural'Last - Dirs (i).Size > Total_Size then
            Total_Size := Total_Size + Dirs (i).Size;
         else
            Put_Line ("Directory sum is too big");
            return;
         end if;
      end if;
   end loop;

   Put_Line ("Part A sum: " & Natural'Image (Total_Size));

   if Natural'Last - Dirs (0).Size > 30000000
      and then Dirs (0).Size + 30000000 > 70000000
   then
      Best_Dir_Size := Dirs (0).Size;
      Space_Needed := Dirs (0).Size - 40000000;

      for i in 1 .. Num_Dirs loop
         if Dirs (i).Size >= Space_Needed
            and then Dirs (i).Size < Best_Dir_Size
         then
            Best_Dir_Size := Dirs (i).Size;
         end if;
      end loop;
      Put_Line ("Part B value: " & Natural'Image (Best_Dir_Size));
   else
      Put_Line ("There is already enough space");
   end if;
end Day7;
