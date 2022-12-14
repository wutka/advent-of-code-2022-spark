package body List_Cursors with SPARK_Mode is

   function Cursor_Valid (Cursor : List_Cursor) return Boolean is
      (Cursor.Pos >= Cursor.Line'First and then
       Cursor.Pos <= Cursor.Line'Last and then
       Cursor.Last >= Cursor.Pos and then
       Cursor.Last >= Cursor.Line'First and then
       Cursor.Last <= Cursor.Line'Last);

   procedure Cursor_Char (Cursor : in out List_Cursor;
      Ch : out Character)
      with
         Pre => Cursor_Valid (Cursor),
         Post => Cursor_Valid (Cursor)
   is
   begin
      Ch := Cursor.Line (Cursor.Pos);
      if Cursor.Pos < Cursor.Last then
         Cursor.Pos := Cursor.Pos + 1;
         pragma Assert (Cursor.Pos <= Cursor.Last);
      end if;
   end Cursor_Char;

   procedure Make_List_Cursor (Line : Line_Type; Pos, Last : Natural;
      Cursor : out List_Cursor)
   is
      Ch : Character;
      Depth : Natural;
   begin
      Cursor := (Pos, Pos, Line);

      Depth := 0;

      Ch := Cursor.Line (Cursor.Last);
      while Ch /= ']' or else Depth > 0 loop
         pragma Loop_Invariant (
            Cursor_Valid (Cursor));
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
         end if;
      end loop;
   end Make_List_Cursor;

   procedure Make_Sublist_Cursor (Cursor : in out List_Cursor;
      New_Cursor : out List_Cursor)
      with
         Pre => Cursor_Valid (Cursor),
         Post => Cursor_Valid (Cursor) and then
            Cursor_Valid (New_Cursor)
   is
      Ch : Character;
      Depth : Natural;
   begin
      New_Cursor := (Cursor.Pos, Cursor.Pos, Cursor.Line);

      Depth := 0;

      Cursor_Char (Cursor, Ch);
      while Ch /= ']' or else Depth > 0 loop
         pragma Loop_Invariant (
            Cursor_Valid (Cursor) and then
            Cursor_Valid (New_Cursor));
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
   is
   begin
      return Cursor.Pos < Cursor.Last and then
         Cursor.Line (Cursor.Pos) in '0' .. '9';
   end Next_Is_Number;

   --  Returns true if the next list item is a list
   function Next_Is_List (Cursor : List_Cursor) return Boolean
   is
   begin
      return Cursor.Pos < Cursor.Last and then
         Cursor.Line (Cursor.Pos) = '[';
   end Next_Is_List;

   --  Returns true if there are more items in the list
   function Has_Next (Cursor : List_Cursor) return Boolean
   is
   begin
      return Cursor.Pos < Cursor.Last;
   end Has_Next;

   --  Returns the number at the current position in the list
   procedure Get_Number (Cursor : in out List_Cursor; N : out Natural)
   is
      Digit_Val : Natural;
   begin
      N := 0;
      while Cursor.Pos < Cursor.Last and then
         Cursor.Line (Cursor.Pos) in '0' .. '9' loop
         pragma Loop_Invariant (Cursor_Valid (Cursor));
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
   is
   begin
      Make_Sublist_Cursor (Cursor, New_Cursor);
   end Get_List;
end List_Cursors;
