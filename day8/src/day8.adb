pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;

procedure Day8 is
   type Grid_Range is range 1 .. 100;
   type Grid_Digit is range 0 .. 9;

   type Grid_Type is array (Grid_Range, Grid_Range) of Grid_Digit;

   type Dir is (Up, None, Down);

   procedure Compute_Distance (Grid : Grid_Type;
      Start_X : Grid_Range; Start_Y : Grid_Range;
      Width : Grid_Range; Height : Grid_Range;
      X_Dir : Dir; Y_Dir : Dir;
      Distance : out Natural; Visible : out Boolean)
   with
      Pre => (X_Dir = None and then Y_Dir /= None) or else
             (X_Dir /= None and then Y_Dir = None),
      Post => (Distance <= Natural (Grid_Range'Last) + 1)
   is
      X : Grid_Range;
      Y : Grid_Range;
      Next_X : Grid_Range;
      Next_Y : Grid_Range;
      Loop_Range : Grid_Range;
   begin
      X := Start_X;
      Y := Start_Y;

      Distance := 0;

      --  edge distance = 0
      --  technically, the distance might be non-zero in
      --  the desired direction, but ultimately since an edge always
      --  has some 0 distance, the ultimate computation is 0
      if X = 1 or else X >= Width or else Y = 1 or else Y >= Height then
         Visible := True;
         Distance := 0;
         return;
      end if;

      if X_Dir /= None then
         Loop_Range := Width;
      else
         Loop_Range := Height;
      end if;

      --  put a limit on the loop to help gnatprove
      for d in 1 .. Loop_Range loop
         pragma Loop_Invariant (X > 1 and then X < Width and then
            Y > 1 and then Y < Height);
         pragma Loop_Invariant (d <= Grid_Range'Last);
         pragma Loop_Invariant (Distance <= Natural (d));
         case X_Dir is
            when Up => Next_X := X + 1;
            when None => Next_X := X;
            when Down => Next_X := X - 1;
         end case;

         case Y_Dir is
            when Up => Next_Y := Y + 1;
            when None => Next_Y := Y;
            when Down => Next_Y := Y - 1;
         end case;

         if Grid (Next_X, Next_Y) < Grid (Start_X, Start_Y) then
            Distance := Distance + 1;
         else
            --  The distance includes the blocking tree
            Distance := Distance + 1;
            Visible := False;
            return;
         end if;

         --  If we hit an edge, there is no more distance
         --  and we know the item is visible
         if Next_X = 1 or else Next_X >= Width or else
            Next_Y = 1 or else Next_Y >= Height
         then
            Visible := True;
            return;
         end if;
         X := Next_X;
         Y := Next_Y;
      end loop;

      --  should never get here
      Distance := 0;
      Visible := False;
   end Compute_Distance;

   Data_File : File_Type;
   Line : String (1 .. 100);
   Line_Len : Natural;

   Grid : Grid_Type;
   Num_Lines : Natural;
   Width : Grid_Range;
   Height : Grid_Range;

   Num_Visible : Natural;
   Score : Natural;
   Best_Score : Natural;

   Left_Distance : Natural;
   Right_Distance : Natural;
   Up_Distance : Natural;
   Down_Distance : Natural;
   Is_Visible : Boolean;
   Vis_Temp : Boolean;

begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day8.txt");

   Num_Lines := 0;

   Grid := (others => (others => 0));

   Height := 1;
   Width := 1;

   while not End_Of_File (Data_File) loop
      Get_Line (Data_File, Line, Line_Len);

      if Line_Len > 0 and then
         Line_Len <= Natural (Grid_Range'Last)
      then
         Width := Grid_Range (Line_Len);
      else
         Put_Line ("Line is too long");
         return;
      end if;

      if Natural (Grid_Range'Last) > Num_Lines then
         Num_Lines := Num_Lines + 1;
         Height := Grid_Range (Num_Lines);
      else
         Put_Line ("Too many lines");
         return;
      end if;

      for i in 1 .. Width loop
         if Line (Natural (i)) in '0' .. '9' then
            Grid (i, Height) := Character'Pos (Line (Natural (i))) - 48;
         else
            Put_Line ("Invalid character " &
               Character'Image (Line (Natural (i))));
            return;
         end if;
      end loop;

   end loop;

   Num_Visible := 0;
   Best_Score := 0;

   for y in 1 .. Height loop
      for x in 1 .. Width loop

         --  Find the distances in each direction
         Compute_Distance (Grid, x, y,
            Width, Height, Down, None,
            Left_Distance, Vis_Temp);

         Is_Visible := Vis_Temp;

         Compute_Distance (Grid, x, y,
            Width, Height, Up, None,
            Right_Distance, Vis_Temp);

         Is_Visible := Is_Visible or Vis_Temp;

         Compute_Distance (Grid, x, y,
            Width, Height, None, Up,
            Up_Distance, Vis_Temp);

         Is_Visible := Is_Visible or Vis_Temp;

         Compute_Distance (Grid, x, y,
            Width, Height, None, Down,
            Down_Distance, Vis_Temp);

         Is_Visible := Is_Visible or Vis_Temp;

         --  It bugs me to put the Natural'Last check here
         --  Why can't Gnatprove see that this line can only
         --  be executed Width * Height times?
         if Is_Visible and then
            Natural'Last > Num_Visible
         then
            Num_Visible := Num_Visible + 1;
         end if;

         Score := Left_Distance * Right_Distance *
            Up_Distance * Down_Distance;

         if Score > Best_Score then
            Best_Score := Score;
         end if;
      end loop;
   end loop;

   Put_Line ("Part A total: " & Natural'Image (Num_Visible));
   Put_Line ("Part B total: " & Natural'Image (Best_Score));

end Day8;
