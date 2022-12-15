pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Day15 is

   type Coord_Type is range -10_000_000 .. 10_000_000;

   type Dist_Range is range 0 .. 20_000_000;

   type Segment is record
      Start : Coord_Type := 0;
      Finish : Coord_Type := 0;
   end record;

   type Sensor_Type is record
      X, Y : Coord_Type := 0;
      Max_Dist : Dist_Range := 0;
   end record;

   type Segment_Range is range 1 .. 50;
   type Segment_Array is array (Segment_Range) of Segment;

   type Sensor_Range is range 1 .. 50;
   type Sensor_Array is array (Sensor_Range) of Sensor_Type;

   function Overlap (A, B : Segment) return  Boolean is
   begin
      return (A.Start >= B.Start and then A.Start <= B.Finish) or else
             (A.Finish >= B.Start and then A.Finish <= B.Finish) or else
             (B.Start >= A.Start and then B.Start <= A.Finish) or else
             (B.Finish >= A.Start and then B.Finish <= A.Start);
   end Overlap;

   function Min4 (A, B, C, D : Coord_Type) return Coord_Type
   is
      AB_Min, CD_Min : Coord_Type;
   begin
      AB_Min := Coord_Type'Min (A, B);
      CD_Min := Coord_Type'Min (C, D);
      return Coord_Type'Min (AB_Min, CD_Min);
   end Min4;

   function Max4 (A, B, C, D : Coord_Type) return Coord_Type
   is
      AB_Max, CD_Max : Coord_Type;
   begin
      AB_Max := Coord_Type'Max (A, B);
      CD_Max := Coord_Type'Max (C, D);
      return Coord_Type'Max (AB_Max, CD_Max);
   end Max4;

   function Merge (A, B : Segment) return Segment
      with Pre => Overlap (A, B)
   is
   begin
      return (Start => Min4 (A.Start, B.Start, A.Finish, B.Finish),
              Finish => Max4 (A.Start, B.Start, A.Finish, B.Finish));
   end Merge;

   procedure Add_Segment (Segments : in out Segment_Array;
      Next_Segment : in out Segment_Range;
      New_Segment : Segment)
   is
      Merged : Boolean;
   begin
      Segments (Next_Segment) := New_Segment;
      if Segment_Range'Last > Next_Segment then
         Next_Segment := Next_Segment + 1;
      end if;

      Merged := True;
      while Merged loop
         Merged := False;

         for i in 1 .. Next_Segment - 2 loop
            for j in i + 1 .. Next_Segment - 1 loop
               if Overlap (Segments (i), Segments (j)) then
                  Segments (i) := Merge (Segments (i), Segments (j));
                  Merged := True;
                  Segments (j) := Segments (Next_Segment - 1);
                  Next_Segment := Next_Segment - 1;
                  exit;
               end if;
            end loop;
            if Merged then
               exit;
            end if;
         end loop;
      end loop;
   end Add_Segment;

   procedure Skip_To (Data_File : File_Type; Ch : Character)
      with
      Pre => Is_Open (Data_File) and then Mode (Data_File) = In_File
   is
      Temp : Character;
   begin
      while not End_Of_Line (Data_File) loop
         Get (Data_File, Temp);
         if Temp = Ch then
            return;
         end if;
      end loop;
   end Skip_To;

   procedure Read_Coord (Data_File : File_Type;
      Coord : out Coord_Type; Success : out Boolean)
   is
      Val : Integer;
   begin
      Get (Data_File, Val);
      if Val >= Integer (Coord_Type'First) and then
         Val <= Integer (Coord_Type'Last)
      then
         Coord := Coord_Type (Val);
         Success := True;
      else
         Coord := 0;
         Success := False;
      end if;
   end Read_Coord;

   function Manhattan_Distance (X1, Y1, X2, Y2 : Coord_Type)
      return Integer
   is
      XDist, YDist : Integer;
   begin
      if X1 >= X2 then
         XDist := Integer (X1) - Integer (X2);
      else
         XDist := Integer (X2) - Integer (X1);
      end if;

      if Y1 > Y2 then
         YDist := Integer (Y1) - Integer (Y2);
      else
         YDist := Integer (Y2) - Integer (Y1);
      end if;

      return XDist + YDist;
   end Manhattan_Distance;

   function Coord_In_Sensor_Range (X, Y : Coord_Type;
      Sensor : Sensor_Type) return Boolean
   is
   begin
      return Manhattan_Distance (X, Y, Sensor.X, Sensor.Y) <=
         Integer (Sensor.Max_Dist);
   end Coord_In_Sensor_Range;

   function Coord_Not_In_Any_Sensor_Range (X, Y : Coord_Type;
      Sensors : Sensor_Array; Num_Sensors : Sensor_Range) return Boolean
   is
   begin
      for i in 1 .. Num_Sensors loop
         if Coord_In_Sensor_Range (X, Y, Sensors (i)) then
            return False;
         end if;
      end loop;
      return True;
   end Coord_Not_In_Any_Sensor_Range;

   Data_File : File_Type;

   Segments : Segment_Array;
   Next_Segment : Segment_Range;
   Num_Segments : Segment_Range;

   Sensors : Sensor_Array;
   Next_Sensor : Sensor_Range;
   Num_Sensors : Sensor_Range;

   Success : Boolean;

   Sensor_X, Sensor_Y : Coord_Type;
   Beacon_X, Beacon_Y : Coord_Type;
   Dist : Integer;
   Dist2M : Integer;

   Start_X, End_X : Coord_Type;
   X, Y : Coord_Type;
   Dist_Y : Integer;

   Diff : Integer;
   Part_A_Sum : Integer;

   Target_Row : constant := 2_000_000;
   Beacon_Limit : constant := 4_000_000;

begin
   Open (File => Data_File,
         Mode => In_File,
--         Name => "test.txt");
         Name => "data/day15.txt");

   Segments := (others => <>);
   Sensors := (others => <>);
   Next_Segment := 1;
   Next_Sensor := 1;

   while not End_Of_File (Data_File) loop
      Skip_To (Data_File, '=');
      Read_Coord (Data_File, Sensor_X, Success);
      if not Success then
         Put_Line ("Sensor X out of range");
         return;
      end if;
      Skip_To (Data_File, '=');
      Read_Coord (Data_File, Sensor_Y, Success);
      if not Success then
         Put_Line ("Sensor Y out of range");
         return;
      end if;

      Skip_To (Data_File, '=');
      Read_Coord (Data_File, Beacon_X, Success);
      if not Success then
         Put_Line ("Beacon X out of range");
         return;
      end if;
      Skip_To (Data_File, '=');
      Read_Coord (Data_File, Beacon_Y, Success);
      if not Success then
         Put_Line ("Beacon Y out of range");
         return;
      end if;

      Skip_Line (Data_File);

      Dist := Manhattan_Distance (Sensor_X, Sensor_Y, Beacon_X, Beacon_Y);

      if Dist >= 0 and then Dist <= Integer (Dist_Range'Last) then
         Sensors (Next_Sensor) :=
            (X => Sensor_X, Y => Sensor_Y, Max_Dist => Dist_Range (Dist));
         if Next_Sensor < Sensor_Range'Last then
            Next_Sensor := Next_Sensor + 1;
         end if;
      end if;

      if (Sensor_Y <= Target_Row and then
          Integer (Sensor_Y) + Dist >= Target_Row) or else
         (Sensor_Y > Target_Row and then
          Target_Row + Dist >= Integer (Sensor_Y))
      then
         Dist2M := Dist - abs (Integer (Sensor_Y) - Target_Row);
         if Dist2M >= 0 and then
            Integer (Coord_Type'First) + Dist2M <= Integer (Sensor_X)
            and then Integer (Coord_Type'Last) - Dist2M >=
               Integer (Sensor_X)
         then
            Start_X := Sensor_X - Coord_Type (Dist2M);
            End_X := Sensor_X + Coord_Type (Dist2M);

            Add_Segment (Segments, Next_Segment,
               (Start => Start_X, Finish => End_X));
         else
            Put_Line ("Dist2M overflows coords");
         end if;
      end if;
   end loop;

   if Next_Segment > 1 then
      Num_Segments := Next_Segment - 1;
   else
      Put_Line ("There are no segments");
      return;
   end if;

   if Next_Sensor > 1 then
      Num_Sensors := Next_Sensor - 1;
   else
      Put_Line ("There are no sensors");
      return;
   end if;

   Part_A_Sum := 0;
   for i in 1 .. Num_Segments loop
      Diff :=  (Integer (Segments (i).Finish) -
         Integer (Segments (i).Start) + 1);
      if Diff > 0 and then Integer'Last - Diff >= Part_A_Sum then
         Part_A_Sum := Part_A_Sum + Diff;
      end if;
   end loop;
   if Part_A_Sum > 0 then
      Put_Line ("Part A Sum: " & Integer'Image (Part_A_Sum - 1));
   end if;

   --  Examine each coordinate adjacent to the diamond-shaped area
   --  defined by each sensor and see if any of those coordinates
   --  are outside of sensor range
   for i in 1 .. Num_Sensors loop
      for d in 0 .. Sensors (i).Max_Dist loop
         if Integer (Sensors (i).X) - Integer (d) - 1 >= 0 then
            X := Coord_Type (Integer (Sensors (i).X) - Integer (d) - 1);
            Dist_Y := Integer (Sensors (i).Max_Dist) - Integer (d);
            if Integer (Sensors (i).Y) - Dist_Y >= 0 then
               Y := Coord_Type (Integer (Sensors (i).Y) - Dist_Y);
               if Coord_Not_In_Any_Sensor_Range (
                  X, Y, Sensors, Num_Sensors)
               then
                  Put_Line ("Part B value: " &
                     Long_Integer'Image (Long_Integer (X) * 4_000_000 +
                     Long_Integer (Y)));
                  return;
               end if;
            end if;
            if Integer (Sensors (i).Y) + Dist_Y <= Beacon_Limit then
               Y := Coord_Type (Integer (Sensors (i).Y) + Dist_Y);
               if Coord_Not_In_Any_Sensor_Range (
                  X, Y, Sensors, Num_Sensors)
               then
                  Put_Line ("Part B value: " &
                     Long_Integer'Image (Long_Integer (X) * 4_000_000 +
                     Long_Integer (Y)));
                  return;
               end if;
            end if;
         end if;
         if Integer (Sensors (i).X) + Integer (d) + 1 <= Beacon_Limit then
            X := Coord_Type (Integer (Sensors (i).X) + Integer (d) + 1);
            Dist_Y := Integer (Sensors (i).Max_Dist) - Integer (d);
            if Dist_Y > 0 and then Integer'First + Dist_Y <
               Integer (Sensors (i).Y) and then
               Integer (Sensors (i).Y) - Integer (d) >= 0
            then
               Y := Coord_Type (Integer (Sensors (i).Y) - Integer (d));
               if Coord_Not_In_Any_Sensor_Range (
                  X, Y, Sensors, Num_Sensors)
               then
                  Put_Line ("Part B value: " &
                     Long_Integer'Image (Long_Integer (X) * 4_000_000 +
                     Long_Integer (Y)));
                  return;
               end if;
            end if;
            if Integer (Sensors (i).Y) + Dist_Y <= Beacon_Limit then
               Y := Coord_Type (Integer (Sensors (i).Y) + Dist_Y);
               if Coord_Not_In_Any_Sensor_Range (
                  X, Y, Sensors, Num_Sensors)
               then
                  Put_Line ("Part B value: " &
                     Long_Integer'Image (Long_Integer (X) * 4_000_000 +
                     Long_Integer (Y)));
                  return;
               end if;
            end if;
         end if;
      end loop;
   end loop;
end Day15;
