pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;

procedure Day13 is
   type Compare_Result is (LESS, EQUAL, GREATER);

   subtype Line_Type is String (1 .. 500);

   type List_Cursor is record
      Pos : Natural;
      Last : Natural;
      Line : Line_Type;
   end record;

   procedure Cursor_Char (Cursor : in out List_Cursor;
      Ch : out Character)
      with
      Pre => Cursor.Pos >= Cursor.Line'First and then
             Cursor.Last <= Cursor.Line'Last and then
             Cursor.Pos < Cursor.Last,
      Post => Cursor.Pos >= Cursor.Line'First and then
             Cursor.Last <= Cursor.Line'Last and then
             Cursor.Pos <= Cursor.Last
   is
   begin
      Ch := Cursor.Line (Cursor.Pos);
      Cursor.Pos := Cursor.Pos + 1;
   end Cursor_Char;

   procedure Make_List_Cursor (Line : Line_Type; Pos, Last : Natural;
      Cursor : out List_Cursor)
      with
      Pre => Pos >= Line'First and then
         Pos < Last and then
         Last <= Line'Last,
      Post => Cursor.Pos >= Cursor.Line'First and then
             Cursor.Last <= Cursor.Line'Last and then
             Cursor.Pos <= Cursor.Last
   is
      Ch : Character;
      Depth : Natural;
   begin
      Cursor := (Pos, Pos, Line);

      Depth := 0;

      Ch := Cursor.Line (Cursor.Last);
      while Ch /= ']' or else Depth > 0 loop
         pragma Loop_Invariant (
             Cursor.Pos >= Cursor.Line'First and then
             Cursor.Last <= Cursor.Line'Last and then
             Cursor.Pos <= Cursor.Last);
         if Ch = '[' then
            if Depth < Natural'Last then
               Depth := Depth + 1;
            end if;
         elsif Ch = ']' then
            if Depth > 0 then
               Depth := Depth - 1;
            end if;
            if Depth = 0 then
               if Cursor.Pos < Cursor.Last then
                  Cursor.Pos := Cursor.Pos + 1;
               end if;
               return;
            end if;
         end if;
         if Cursor.Last < Last and then
            Cursor.Last < Cursor.Line'Last
         then
            Cursor.Last := Cursor.Last + 1;
            Ch := Cursor.Line (Cursor.Last);
--            Put_Line ("Making List at char " & Character'Image (Ch));
         end if;
      end loop;
   end Make_List_Cursor;

   procedure Make_Sublist_Cursor (Cursor : in out List_Cursor;
      New_Cursor : out List_Cursor)
      with
      Pre => Cursor.Pos >= Cursor.Line'First and then
             Cursor.Last <= Cursor.Line'Last and then
             Cursor.Pos < Cursor.Last and then
             Cursor.Pos <= Cursor.Line'Last,
      Post => Cursor.Pos >= Cursor.Line'First and then
             Cursor.Last <= Cursor.Line'Last and then
             Cursor.Pos <= Cursor.Last and then
             Cursor.Pos <= Cursor.Line'Last and then
             New_Cursor.Pos >= New_Cursor.Line'First and then
             New_Cursor.Last < New_Cursor.Line'Last and then
             New_Cursor.Pos <= New_Cursor.Last
   is
      Ch : Character;
      Depth : Natural;
   begin
      New_Cursor := (Cursor.Pos, Cursor.Pos, Cursor.Line);

      Depth := 0;

      Cursor_Char (Cursor, Ch);
      while Ch /= ']' or else Depth > 0 loop
         pragma Loop_Invariant (
            Cursor.Pos >= Cursor.Line'First and then
            Cursor.Last <= Cursor.Line'Last and then
            Cursor.Pos <= Cursor.Last and then
            New_Cursor.Pos >= New_Cursor.Line'First and then
            New_Cursor.Last < New_Cursor.Line'Last and then
            New_Cursor.Pos <= New_Cursor.Last);
         if Ch = '[' then
            if Depth < Natural'Last then
               Depth := Depth + 1;
            end if;
         elsif Ch = ']' then
            if Depth > 0 then
               Depth := Depth - 1;
            end if;
            if Depth = 0 then
               if Cursor.Pos < Cursor.Last then
                  Cursor.Pos := Cursor.Pos + 1;
               end if;
               if New_Cursor.Pos < New_Cursor.Last then
                  New_Cursor.Pos := New_Cursor.Pos + 1;
               end if;
               return;
            end if;
         end if;
         if Cursor.Pos < Cursor.Last and then
            Cursor.Pos < Cursor.Line'Last and then
            New_Cursor.Last < Cursor.Last and then
            New_Cursor.Last + 1 < New_Cursor.Line'Last
         then
            New_Cursor.Last := New_Cursor.Last + 1;
            Cursor_Char (Cursor, Ch);
         end if;
      end loop;
   end Make_Sublist_Cursor;

   --  Returns true if the next list item is a number
   function Next_Is_Number (Cursor : List_Cursor) return Boolean
      with
      Pre => Cursor.Pos >= 1 and then
             Cursor.Pos <= Cursor.Last and then
             Cursor.Last <= Cursor.Line'Last
   is
   begin
      return Cursor.Pos < Cursor.Last and then
         Cursor.Line (Cursor.Pos) in '0' .. '9';
   end Next_Is_Number;

   --  Returns true if the next list item is a list
   function Next_Is_List (Cursor : List_Cursor) return Boolean
      with
      Pre => Cursor.Pos >= 1 and then
             Cursor.Pos <= Cursor.Last and then
             Cursor.Last <= Cursor.Line'Last
   is
   begin
      return Cursor.Pos < Cursor.Last and then
         Cursor.Line (Cursor.Pos) = '[';
   end Next_Is_List;

   --  Returns true if there are more items in the list
   function Has_Next (Cursor : List_Cursor) return Boolean
      with
      Pre => Cursor.Pos >= 1 and then
             Cursor.Pos <= Cursor.Last and then
             Cursor.Last <= Cursor.Line'Last
   is
   begin
      return Cursor.Pos < Cursor.Last;
   end Has_Next;

   --  Returns the number at the current position in the list
   procedure Get_Number (Cursor : in out List_Cursor; N : out Natural)
      with Pre => Cursor.Pos >= 1 and then
                  Cursor.Pos <= Cursor.Last and then
                  Cursor.Last <= Cursor.Line'Last and then
                  Next_Is_Number (Cursor),
           Post => Cursor.Pos >= 1 and then
                   Cursor.Pos <= Cursor.Last and then
                   Cursor.Pos <= Cursor.Line'Last and then
                   Cursor.Last <= Cursor.Line'Last
   is
      Digit_Val : Natural;
   begin
      N := 0;
      while Cursor.Pos < Cursor.Last and then
         Cursor.Line (Cursor.Pos) in '0' .. '9' loop
         pragma Loop_Invariant (Cursor.Pos >= 1 and then
            Cursor.Pos < Cursor.Last and then
            Cursor.Last <= Cursor.Line'Last);
         if Natural'Last / 10 > N then
            N := N * 10;
            Digit_Val := Character'Pos (Cursor.Line (Cursor.Pos)) - 48;
            if Natural'Last - N > Digit_Val then
               N := N + Digit_Val;
            end if;
         end if;
         Cursor.Pos := Cursor.Pos + 1;
      end loop;

      if Cursor.Pos < Cursor.Last and then
         Cursor.Line (Cursor.Pos) = ','
      then
         Cursor.Pos := Cursor.Pos + 1;
      end if;
   end Get_Number;

   --  Returns the sublist at the current position in the list
   procedure Get_List (Cursor : in out List_Cursor;
      New_Cursor : out List_Cursor)
      with Pre => Cursor.Pos >= 1 and then
                  Cursor.Pos < Cursor.Last and then
                  Cursor.Last <= Cursor.Line'Last and then
                  Next_Is_List (Cursor),
           Post => Cursor.Pos >= 1 and then
                   Cursor.Pos <= Cursor.Last and then
                   Cursor.Pos <= Cursor.Line'Last and then
                   Cursor.Last <= Cursor.Line'Last and then
                   New_Cursor.Pos >= 1 and then
                   New_Cursor.Pos <= New_Cursor.Last and then
                   New_Cursor.Pos <= New_Cursor.Line'Last and then
                   New_Cursor.Last <= New_Cursor.Line'Last
   is
   begin
      Make_Sublist_Cursor (Cursor, New_Cursor);
   end Get_List;

   --  Compares a number with a list, assuming the number comes
   --  first (so when comparing [1,2,3] with 5 you have to swap
   --  the results)
   procedure Compare_List_With_Number (N : Natural;
      Cursor : in out List_Cursor; Comparison : out Compare_Result)
      with Pre => Cursor.Pos >= 1 and then
                  Cursor.Pos <= Cursor.Last and then
                  Cursor.Last <= Cursor.Line'Last,
           Post => Cursor.Pos >= 1 and then
                   Cursor.Pos <= Cursor.Line'Last and then
                   Cursor.Last <= Cursor.Line'Last
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
            if Cursor.Pos < Cursor.Last then
               Get_List (Cursor, Sub_Cursor);
               Compare_List_With_Number (N, Sub_Cursor, Comparison);
               if Comparison = EQUAL then
                  if Has_Next (Cursor) then
                     Comparison := LESS;
                  end if;
               end if;
            end if;
         end if;
      end if;
   end Compare_List_With_Number;

   --  Compares two lists
   procedure Compare_Lists (Cursor1, Cursor2 : in out List_Cursor;
      Comparison : out Compare_Result)
   with
      Pre => Cursor1.Pos >= 1 and then Cursor1.Pos <= Cursor1.Line'Last
         and then Cursor1.Last <= Cursor1.Line'Last and then
         Cursor1.Pos <= Cursor1.Last and then
         Cursor2.Pos >= 1 and then Cursor2.Pos <= Cursor2.Line'Last
         and then Cursor2.Last <= Cursor2.Line'Last and then
         Cursor2.Pos <= Cursor2.Last
   is
      N1, N2 : Natural;
      Sub_Cursor1 : List_Cursor;
      Sub_Cursor2 : List_Cursor;
   begin
      while Has_Next (Cursor1) loop
         pragma Loop_Invariant (
            Cursor1.Pos >= 1 and then Cursor1.Pos <= Cursor1.Last and then
            Cursor1.Last <= Cursor1.Line'Last and then
            Cursor2.Pos >= 1 and then Cursor2.Pos <= Cursor2.Last and then
            Cursor2.Last <= Cursor2.Line'Last);
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
                  if Cursor2.Pos < Cursor2.Last then
                     Get_List (Cursor2, Sub_Cursor2);
                     Compare_List_With_Number (N1, Sub_Cursor2, Comparison);
                     if Comparison /= EQUAL then
                        return;
                     end if;
                  end if;
               end if;
            else
               Comparison := GREATER;
               return;
            end if;
         elsif Cursor1.Pos < Cursor1.Last and then
            Next_Is_List (Cursor1)
         then
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
               elsif Cursor2.Pos < Cursor2.Last and then
                  Next_Is_List (Cursor2)
               then
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
