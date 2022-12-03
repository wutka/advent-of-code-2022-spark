pragma Spark_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;

procedure Day3 is
   type PrioritySet is array (1 .. 52) of Boolean;
   function Priority (ch : Character) return Integer;
   function Score_Set (s : PrioritySet) return Natural;

   function Priority (ch : Character) return Integer is
   begin
      case ch is
         when 'a' .. 'z' => return Character'Pos (ch) - 96;
         when 'A' .. 'Z' => return Character'Pos (ch) - 64 + 26;
         when others => return 0;
      end case;
   end Priority;

   procedure AddToSet (ch : Character; s : in out PrioritySet) is
      prio : Integer;
   begin
      prio := Priority (ch);
      if prio in s'Range then
         s (prio) := True;
      end if;
   end AddToSet;

   procedure Intersection (a : PrioritySet; b : PrioritySet;
      intersect : out PrioritySet) is
   begin
      for i in a'Range loop
         if a (i) and then b (i) then
            intersect (i) := True;
         else
            intersect (i) := False;
         end if;
      end loop;
   end Intersection;

   function Score_Set (s : PrioritySet) return Natural is
      score : Natural;
   begin
      score := 0;
      for i in s'Range loop
         if s (i) and then Natural'Last - i > score then
            score := score + i;
         end if;
      end loop;
      return score;
   end Score_Set;

   procedure Line_To_Set (l : String; s : in out PrioritySet) is
   begin
      for i in l'Range loop
         AddToSet (l (i), s);
      end loop;
   end Line_To_Set;

   Data_File : File_Type;
   Line : String (1 .. 10000);
   Chars_Read : Natural;
   MidPoint : Natural;
   Left_Set : PrioritySet;
   Right_Set : PrioritySet;
   A_Set : PrioritySet;
   B_Set : PrioritySet;
   C_Set : PrioritySet;
   Intersect : PrioritySet;
   Intersect_Temp : PrioritySet;
   TriadPos : Natural;
   A_Score : Natural;
   B_Score : Natural;
   Score : Natural;

begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day3.txt");

   TriadPos := 0;
   A_Score := 0;
   B_Score := 0;

   A_Set := (others => False);
   B_Set := (others => False);
   C_Set := (others => False);

   while not End_Of_File (Data_File) loop
      Get_Line (File => Data_File, Item => Line, Last => Chars_Read);

      Left_Set := (others => False);
      Right_Set := (others => False);

      pragma Assert (Chars_Read <= Line'Last);

      MidPoint := Chars_Read / 2;
      Line_To_Set (Line (1 .. MidPoint), Left_Set);
      Line_To_Set (Line (MidPoint+1 .. Chars_Read), Right_Set);

      Intersection (Left_Set, Right_Set, Intersect);
      Score := Score_Set (Intersect);

      if Natural'Last - Score > A_Score then
         A_Score := A_Score + Score;
      end if;

      if TriadPos = 0 then
         Line_To_Set (Line (1 .. Chars_Read), A_Set);
         TriadPos := 1;
      elsif TriadPos = 1 then
         Line_To_Set (Line (1 .. Chars_Read), B_Set);
         TriadPos := 2;
      elsif TriadPos = 2 then
         Line_To_Set (Line (1 .. Chars_Read), C_Set);
         Intersection (A_Set, B_Set, Intersect_Temp);
         Intersection (C_Set, Intersect_Temp, Intersect);

         Score := Score_Set (Intersect);
         if Natural'Last - Score > B_Score then
            B_Score := B_Score + Score;
         end if;
         TriadPos := 0;
         A_Set := (others => False);
         B_Set := (others => False);
         C_Set := (others => False);
      end if;
   end loop;

   Put ("Part A Score ");
   Put_Line (Natural'Image (A_Score));
   Put ("Part B Score ");
   Put_Line (Natural'Image (B_Score));
end Day3;
