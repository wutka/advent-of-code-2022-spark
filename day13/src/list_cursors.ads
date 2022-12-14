package List_Cursors with SPARK_Mode is
   subtype Line_Type is String (1 .. 500);
   type List_Cursor is private;

   procedure Make_List_Cursor (Line : Line_Type;
      Pos, Last : Natural; Cursor : out List_Cursor)
      with
      Pre => Pos >= Line'First and then
         Pos < Last and then
         Last <= Line'Last;

   function Next_Is_Number (Cursor : List_Cursor)
      return Boolean;

   function Next_Is_List (Cursor : List_Cursor)
      return Boolean;

   function Has_Next (Cursor : List_Cursor)
      return Boolean;

   procedure Get_Number (Cursor : in out List_Cursor;
      N : out Natural)
      with Pre => Has_Next (Cursor) and then
         Next_Is_Number (Cursor);

   procedure Get_List (Cursor : in out List_Cursor;
      New_Cursor : out List_Cursor)
      with Pre => Has_Next (Cursor) and then
         Next_Is_List (Cursor);

private
   type List_Cursor is record
      Pos : Natural := 1;
      Last : Natural := Line_Type'Last;
      Line : Line_Type := (others => ' ');
   end record with
      Type_Invariant =>
         Cursor_Valid (List_Cursor);

   function Cursor_Valid (Cursor : List_Cursor)
      return Boolean with Ghost;

end List_Cursors;
