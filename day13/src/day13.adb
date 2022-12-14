pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;

with List_Cursors;
use List_Cursors;

procedure Day13 is
   type Compare_Result is (LESS, EQUAL, GREATER);

   --  Compares a number with a list, assuming the number comes
   --  first (so when comparing [1,2,3] with 5 you have to swap
   --  the results)
   procedure Compare_List_With_Number (N : Natural;
      Cursor : in out List_Cursor;
      Comparison : out Compare_Result)
   is
      N2 : Natural;
      Sub_Cursor : List_Cursor;
   begin
      Comparison := GREATER;
      if Has_Next (Cursor) then
         if Next_Is_Number (Cursor) then
            Get_Number (Cursor, N2);
            if N < N2 then
               Comparison := LESS;
            elsif N2 < N then
               Comparison := GREATER;
            elsif Has_Next (Cursor) then
               Comparison := LESS;
            else
               Comparison := EQUAL;
            end if;
            return;
         elsif Next_Is_List (Cursor) then
            Get_List (Cursor, Sub_Cursor);
            Compare_List_With_Number (N, Sub_Cursor, Comparison);
            if Comparison = EQUAL then
               if Has_Next (Cursor) then
                  Comparison := LESS;
               end if;
            end if;
         end if;
      end if;
   end Compare_List_With_Number;

   --  Compares two lists
   procedure Compare_Lists (Cursor1, Cursor2 : in out List_Cursor;
      Comparison : out Compare_Result)
   is
      N1, N2 : Natural;
      Sub_Cursor1 : List_Cursor;
      Sub_Cursor2 : List_Cursor;
   begin
      while Has_Next (Cursor1) loop
         if Next_Is_Number (Cursor1) then
            Get_Number (Cursor1, N1);
            if Has_Next (Cursor2) then
               if Next_Is_Number (Cursor2) then
                  Get_Number (Cursor2, N2);
                  if N1 < N2 then
                     Comparison := LESS;
                     return;
                  elsif N2 < N1 then
                     Comparison := GREATER;
                     return;
                  end if;
               elsif Next_Is_List (Cursor2) then
                  Get_List (Cursor2, Sub_Cursor2);
                  Compare_List_With_Number (N1, Sub_Cursor2, Comparison);
                  if Comparison /= EQUAL then
                     return;
                  end if;
               end if;
            else
               Comparison := GREATER;
               return;
            end if;
         elsif Next_Is_List (Cursor1) then
            Get_List (Cursor1, Sub_Cursor1);
            if Has_Next (Cursor2) then
               if Next_Is_Number (Cursor2) then
                  Get_Number (Cursor2, N2);
                  Compare_List_With_Number (N2, Sub_Cursor1, Comparison);
                  --  Since N2 is the number in List 2 here, we have to
                  --  swap the results of the number comparison
                  if Comparison = LESS then
                     Comparison := GREATER;
                     return;
                  elsif Comparison = GREATER then
                     Comparison := LESS;
                     return;
                  end if;
               elsif Next_Is_List (Cursor2) then
                  Get_List (Cursor2, Sub_Cursor2);
                  Compare_Lists (Sub_Cursor1, Sub_Cursor2, Comparison);
                  if Comparison /= EQUAL then
                     return;
                  end if;
               end if;
            else
               Comparison := GREATER;
               return;
            end if;
         end if;
      end loop;

      if Has_Next (Cursor2) then
         Comparison := LESS;
      else
         Comparison := EQUAL;
      end if;
   end Compare_Lists;

   Data_File : File_Type;
   L1, L2 : Line_Type;
   Packet1, Packet2 : Line_Type;
   L1_Len, L2_Len : Natural;
   Comparison : Compare_Result;

   Len : Natural;
   Num_Sorted : Natural;
   Pair_Num : Natural;
   Cursor1, Cursor2 : List_Cursor;

   Num_Less_2 : Natural;
   Num_Less_6 : Natural;

begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day13.txt");

   Num_Sorted := 0;
   Pair_Num := 0;

   Num_Less_2 := 1;
   Num_Less_6 := 2;

   Packet1 := (others => ' ');
   Packet1 (1 .. 5) := "[[2]]";
   Packet2 := (others => ' ');
   Packet2 (1 .. 5) := "[[6]]";

   while not End_Of_File (Data_File) loop
      Get_Line (Data_File, L1, Len);
      if Len > 1 then
         L1_Len := Len;
      else
         Put_Line ("Invalid line length");
         return;
      end if;

      --  Count the number of times line 1 is less than [[2]] and [[6]]
      Make_List_Cursor (L1, 1, L1_Len, Cursor1);
      Make_List_Cursor (Packet1, 1, 5, Cursor2);
      Compare_Lists (Cursor1, Cursor2, Comparison);
      if Comparison = LESS and then Num_Less_2 < Natural'Last
      then
         Num_Less_2 := Num_Less_2 + 1;
      end if;

      Make_List_Cursor (L1, 1, L1_Len, Cursor1);
      Make_List_Cursor (Packet2, 1, 5, Cursor2);
      Compare_Lists (Cursor1, Cursor2, Comparison);
      if Comparison = LESS and then Num_Less_6 < Natural'Last
      then
         Num_Less_6 := Num_Less_6 + 1;
      end if;

      Get_Line (Data_File, L2, Len);
      if Len > 1 then
         L2_Len := Len;
      else
         Put_Line ("Invalid line length");
         return;
      end if;

      --  Count the number of times line 2 is less than [[2]] and [[6]]
      Make_List_Cursor (L2, 1, L2_Len, Cursor1);
      Make_List_Cursor (Packet1, 1, 5, Cursor2);
      Compare_Lists (Cursor1, Cursor2, Comparison);
      if Comparison = LESS and then Num_Less_2 < Natural'Last
      then
         Num_Less_2 := Num_Less_2 + 1;
      end if;

      Make_List_Cursor (L2, 1, L2_Len, Cursor1);
      Make_List_Cursor (Packet2, 1, 5, Cursor2);
      Compare_Lists (Cursor1, Cursor2, Comparison);
      if Comparison = LESS and then Num_Less_6 < Natural'Last
      then
         Num_Less_6 := Num_Less_6 + 1;
      end if;

      --  Compare the lines
      Make_List_Cursor (L1, 1, L1_Len, Cursor1);
      Make_List_Cursor (L2, 1, L2_Len, Cursor2);
      Compare_Lists (Cursor1, Cursor2, Comparison);

      if Pair_Num < Natural'Last then
         Pair_Num := Pair_Num + 1;
      end if;

      if Comparison = LESS then
         if Num_Sorted < Natural'Last - Pair_Num then
            Num_Sorted := Num_Sorted + Pair_Num;
         end if;
      end if;

      if not End_Of_File (Data_File) then
         Skip_Line (Data_File);
      end if;
   end loop;

   Put_Line ("Part A sum: " & Natural'Image (Num_Sorted));
   if Num_Less_2 > 0 and then
      Natural'Last / Num_Less_2 > Num_Less_6
   then
      Put_Line ("Part B value: " & Natural'Image (
         Num_Less_2 * Num_Less_6));
   end if;
end Day13;
