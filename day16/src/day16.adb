pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Formal_Ordered_Sets;

procedure Day16 is
   type Valve_Range is range 1 .. 702;

   type Connected_Range is range 1 .. 10;
   type Conn_Array is array (Connected_Range) of Valve_Range;

   subtype Valve_Name_Type is String (1 .. 2);

   type Valve is record
      Name : Valve_Name_Type := "??";
      Flow_Rate : Natural := 0;
      Num_Connected : Connected_Range := 1;
      Connected : Conn_Array := (others => 1);
   end record;

   type Valve_Array is array (Valve_Range) of Valve;

   type Next_Valve is record
      Distance : Natural := 0;
      Valve_Num : Valve_Range := 1;
   end record;

   --  Compares two Next_Values sorting by smallest distance
   function Next_Less (A, B : Next_Valve) return Boolean is
      (A.Distance < B.Distance or else
       (A.Distance = B.Distance and then A.Valve_Num < B.Valve_Num));

   package Next_Set is new Ada.Containers.Formal_Ordered_Sets (
      Element_Type => Next_Valve, "<" => Next_Less);

   type Useful_Range is range 1 .. 30;
   type Useful_Bits is array (Useful_Range) of Boolean;

   type Minutes_Range is range 0 .. 30;

   type Useful_Array is array (Useful_Range) of Valve_Range;
   type Distance_Array is array (Valve_Range, Valve_Range) of Natural;

   type Valve_Opening is record
      Minute_Opened : Minutes_Range := 0;
      Valve_Num : Valve_Range := 1;
   end record;

   type Valve_Openings_Type is array (Useful_Range) of Valve_Opening;

   Last_Minute_B : constant Minutes_Range := 26;

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

   procedure Skip_N (Data_File : File_Type; N : Natural)
      with
      Pre => Is_Open (Data_File) and then Mode (Data_File) = In_File
   is
      Temp : Character;
      Num_Read : Natural;
   begin
      Num_Read := 0;
      while not End_Of_Line (Data_File) and then Num_Read < N loop
         Get (Data_File, Temp);
         Num_Read := Num_Read + 1;
      end loop;
   end Skip_N;

   procedure Parse_Valve_Name (Valve_Name : Valve_Name_Type;
      Valve_Number : out Valve_Range; Success : out Boolean) is
      Digit1, Digit2 : Natural;
   begin
      if Valve_Name (1) in 'A' .. 'Z' and then
         Valve_Name (2) in 'A' .. 'Z'
      then
         Digit1 := Character'Pos (Valve_Name (1)) - 64;
         pragma Assert (Digit1 >= 1 and then Digit1 <= 26);
         Digit2 := Character'Pos (Valve_Name (2)) - 64;
         pragma Assert (Digit2 >= 1 and then Digit2 <= 26);
         Valve_Number := Valve_Range (
            (Digit1 - 1) * 26 + Digit2);
         Success := True;
      else
         Valve_Number := 1;
         Success := False;
      end if;
   end Parse_Valve_Name;

   --  Computes the shortest path between Start and Finish
   function Shortest_Path (Start, Finish : Valve_Range;
      Valves : Valve_Array) return Natural is
      Nexts : Next_Set.Set (1000);
      Visited : array (Valve_Range) of Boolean := (others => False);
      Distance : Natural;
      Next, New_Next : Next_Valve;
      Start_Next : Next_Valve;
   begin
      Start_Next := (Valve_Num => Start, Distance => 0);
      if Next_Set.Length (Nexts) < Nexts.Capacity and then
         not Next_Set.Contains (Nexts, Start_Next)
      then
         Next_Set.Insert (Nexts, Start_Next);
      end if;

      while not Next_Set.Is_Empty (Nexts) loop
         Next := Next_Set.First_Element (Nexts);
         Next_Set.Delete (Nexts, Next);
         Visited (Next.Valve_Num) := True;
         Distance := Next.Distance;
         if Next.Valve_Num = Finish then
            return Distance;
         end if;
         for i in 1 .. Valves (Next.Valve_Num).Num_Connected loop
            if not Visited (Valves (Next.Valve_Num).Connected (i)) then
               if Distance < Natural'Last then
                  New_Next :=
                     (Valve_Num => Valves (Next.Valve_Num).Connected (i),
                      Distance => Distance + 1);
                  if Next_Set.Length (Nexts) < Nexts.Capacity and then
                     not Next_Set.Contains (Nexts, New_Next)
                  then
                     Next_Set.Insert (Nexts, New_Next);
                  end if;
               end if;
            end if;
         end loop;
      end loop;
      return Natural'Last;
   end Shortest_Path;

   --  Computes a score by iterating through each minute and
   --  opening valves at the appropriate minute
   function Compute_Score (Valve_Openings : Valve_Openings_Type;
      Num_Openings : Natural; Valves : Valve_Array) return Natural
   is
      Score : Natural;
      Flow_Rate : Natural;
      Curr_Opening : Useful_Range;
      All_Opened : Boolean;
   begin
      if Num_Openings = 0 then
         return 0;
      end if;

      Score := 0;
      All_Opened := False;
      Flow_Rate := 0;

      Curr_Opening := 1;
      for m in 1 .. Minutes_Range'Last loop
         --  Since an opened valve's flow doesn't affect the
         --  rate until the minute after it is opened, compute
         --  the score before seeing if a valve opens in this
         --  minute.
         if Natural'Last - Score > Flow_Rate then
            Score := Score + Flow_Rate;
         end if;
         while not All_Opened and then
            m > Valve_Openings (Curr_Opening).Minute_Opened loop
            if Natural'Last - Flow_Rate >
               Valves (Valve_Openings (Curr_Opening).
                  Valve_Num).Flow_Rate
            then
               Flow_Rate := Flow_Rate +
                  Valves (Valve_Openings (Curr_Opening).Valve_Num).Flow_Rate;
            end if;
            if Natural (Curr_Opening) < Num_Openings and then
               Num_Openings <= Natural (Useful_Range'Last)
            then
               Curr_Opening := Curr_Opening + 1;
            else
               All_Opened := True;
            end if;
         end loop;

      end loop;

      return Score;
   end Compute_Score;

   --  Computes the score for me and the elephant
   function Compute_Dual_Score (Valve_Openings : Valve_Openings_Type;
      Num_Openings : Natural; Valve_Openings2 : Valve_Openings_Type;
      Num_Openings2 : Natural;
      Valves : Valve_Array) return Natural
   is
      Score : Natural;
      Flow_Rate : Natural;
      Curr_Opening : Useful_Range;
      Curr_Opening2 : Useful_Range;
      All_Opened : Boolean;
      All_Opened2 : Boolean;
   begin
      if Num_Openings = 0 then
         return 0;
      end if;

      Score := 0;
      All_Opened := False;
      All_Opened2 := False;
      Flow_Rate := 0;

      Curr_Opening := 1;
      Curr_Opening2 := 1;
      for m in 1 .. Last_Minute_B loop
         --  Since an opened valve's flow doesn't affect the
         --  rate until the minute after it is opened, compute
         --  the score before seeing if a valve opens in this
         --  minute.
         if Natural'Last - Score > Flow_Rate then
            Score := Score + Flow_Rate;
         end if;

         --  See if the elephant has opened a valve
         while not All_Opened and then
            m > Valve_Openings (Curr_Opening).Minute_Opened loop
            if Natural'Last - Flow_Rate >
               Valves (Valve_Openings (Curr_Opening).
                  Valve_Num).Flow_Rate
            then
               Flow_Rate := Flow_Rate +
                  Valves (Valve_Openings (Curr_Opening).Valve_Num).Flow_Rate;
            end if;
            if Natural (Curr_Opening) < Num_Openings and then
               Num_Openings <= Natural (Useful_Range'Last)
            then
               Curr_Opening := Curr_Opening + 1;
            else
               All_Opened := True;
            end if;
         end loop;

         --  See if I have opened a valve
         while not All_Opened2 and then
            m > Valve_Openings2 (Curr_Opening2).Minute_Opened loop
            if Natural'Last - Flow_Rate >
               Valves (Valve_Openings2 (Curr_Opening2).
                  Valve_Num).Flow_Rate
            then
               Flow_Rate := Flow_Rate +
                  Valves (Valve_Openings2 (Curr_Opening2).Valve_Num).Flow_Rate;
            end if;
            if Natural (Curr_Opening2) < Num_Openings2 and then
               Num_Openings2 <= Natural (Useful_Range'Last)
            then
               Curr_Opening2 := Curr_Opening2 + 1;
            else
               All_Opened2 := True;
            end if;
         end loop;

      end loop;

      return Score;
   end Compute_Dual_Score;

   --  Recursively try sequences of valve openings
   procedure Find_Best (From : Valve_Range; Next_Minute : Minutes_Range;
      Valve_Openings : Valve_Openings_Type; Next_Opening : Useful_Range;
      Num_Useful : Useful_Range; Useful_Valves : Useful_Array;
      Visited : Useful_Bits;
      Valves : Valve_Array; Distances : Distance_Array;
      Best_Score : in out Natural)
   is
      Dist : Natural;
      Next_Valve : Valve_Range;
      New_Next_Minute : Minutes_Range;
      Minutes_Left : Minutes_Range;
      New_Visited : Useful_Bits;
      Score : Natural;
      New_Valve_Openings : Valve_Openings_Type;

   begin
      for i in 1 .. Num_Useful loop
         if not Visited (i) then
            Next_Valve := Useful_Valves (i);
            Dist := Distances (From, Next_Valve);

            Minutes_Left := Minutes_Range'Last - Next_Minute;

            if Dist < Natural (Minutes_Left) and then
               Natural (Next_Minute) + Dist <=
               Natural (Minutes_Range'Last)
            then
               New_Next_Minute := Next_Minute + Minutes_Range (Dist);

               New_Valve_Openings := Valve_Openings;

               New_Valve_Openings (Next_Opening) :=
                  (Minute_Opened => New_Next_Minute,
                   Valve_Num => Next_Valve);

               New_Next_Minute := New_Next_Minute + 1;

               Score := Compute_Score (New_Valve_Openings,
                  Natural (Next_Opening), Valves);

               if Score > Best_Score then
                  Best_Score := Score;
               end if;

               New_Visited := Visited;
               New_Visited (i) := True;

               if Next_Opening < Useful_Valves'Last then
                  Find_Best (Next_Valve, New_Next_Minute, New_Valve_Openings,
                     Next_Opening + 1, Num_Useful, Useful_Valves,
                     New_Visited, Valves, Distances, Best_Score);
               end if;
            end if;
         end if;
      end loop;
   end Find_Best;

   --  Recursively try elephant valve openings
   procedure Find_Best_Elephant (From : Valve_Range;
      Next_Minute : Minutes_Range;
      Valve_Openings : Valve_Openings_Type; Next_Opening : Useful_Range;
      Other_Openings : Valve_Openings_Type;
      Other_Next_Opening : Useful_Range;
      Num_Useful : Useful_Range; Useful_Valves : Useful_Array;
      Visited : Useful_Bits;
      Valves : Valve_Array; Distances : Distance_Array;
      Best_Score : in out Natural)
   is
      Dist : Natural;
      Next_Valve : Valve_Range;
      New_Next_Minute : Minutes_Range;
      Minutes_Left : Minutes_Range;
      New_Visited : Useful_Bits;
      Score : Natural;
      New_Valve_Openings : Valve_Openings_Type;

   begin
      for i in 1 .. Num_Useful loop
         if not Visited (i) then
            Next_Valve := Useful_Valves (i);
            Dist := Distances (From, Next_Valve);

            if Last_Minute_B >= Next_Minute then
               Minutes_Left := Last_Minute_B - Next_Minute;
            else
               Minutes_Left := 0;
            end if;

            if Dist < Natural (Minutes_Left) and then
               Natural (Next_Minute) + Dist <=
               Natural (Last_Minute_B)
            then
               New_Next_Minute := Next_Minute + Minutes_Range (Dist);

               New_Valve_Openings := Valve_Openings;

               New_Valve_Openings (Next_Opening) :=
                  (Minute_Opened => New_Next_Minute,
                   Valve_Num => Next_Valve);

               New_Next_Minute := New_Next_Minute + 1;

               Score := Compute_Dual_Score (New_Valve_Openings,
                  Natural (Next_Opening),
                  Other_Openings, Natural (Other_Next_Opening),
                  Valves);

               if Score > Best_Score then
                  Best_Score := Score;
               end if;

               New_Visited := Visited;
               New_Visited (i) := True;

               if Next_Opening < Useful_Valves'Last then
                  Find_Best_Elephant (Next_Valve, New_Next_Minute,
                     New_Valve_Openings, Next_Opening + 1,
                     Other_Openings, Other_Next_Opening,
                     Num_Useful, Useful_Valves,
                     New_Visited, Valves, Distances, Best_Score);
               end if;
            end if;
         end if;
      end loop;
   end Find_Best_Elephant;

   --  Recursively try valve openings for me, and at each point in the
   --  sequence, see what valves the elephant would open
   procedure Find_Best_B (From : Valve_Range; Next_Minute : Minutes_Range;
      Valve_Openings : Valve_Openings_Type; Next_Opening : Useful_Range;
      Num_Useful : Useful_Range; Useful_Valves : Useful_Array;
      Visited : Useful_Bits;
      Valves : Valve_Array; Distances : Distance_Array;
      Best_Score : in out Natural)
   is
      Dist : Natural;
      Next_Valve : Valve_Range;
      New_Next_Minute : Minutes_Range;
      Minutes_Left : Minutes_Range;
      New_Visited : Useful_Bits;
      Score : Natural;
      New_Valve_Openings : Valve_Openings_Type;
      Elephant_Openings : constant Valve_Openings_Type :=
         (others => <>);

   begin
      for i in 1 .. Num_Useful loop
         if not Visited (i) then
            Next_Valve := Useful_Valves (i);
            Dist := Distances (From, Next_Valve);

            if Last_Minute_B >= Next_Minute then
               Minutes_Left := Last_Minute_B - Next_Minute;
            else
               Minutes_Left := 0;
            end if;

            if Dist < Natural (Minutes_Left) and then
               Natural (Next_Minute) + Dist <=
               Natural (Last_Minute_B)
            then
               New_Next_Minute := Next_Minute + Minutes_Range (Dist);

               New_Valve_Openings := Valve_Openings;

               New_Valve_Openings (Next_Opening) :=
                  (Minute_Opened => New_Next_Minute,
                   Valve_Num => Next_Valve);

               New_Next_Minute := New_Next_Minute + 1;

               New_Visited := Visited;
               New_Visited (i) := True;

               Score := 0;

               Find_Best_Elephant (1, 0,
                  Elephant_Openings, 1,
                  New_Valve_Openings, Next_Opening,
                  Num_Useful, Useful_Valves,
                  New_Visited, Valves, Distances, Score);

               if Score > Best_Score then
                  Best_Score := Score;
               end if;

               if Next_Opening < Useful_Valves'Last then
                  Find_Best_B (Next_Valve, New_Next_Minute,
                     New_Valve_Openings, Next_Opening + 1,
                     Num_Useful, Useful_Valves,
                     New_Visited, Valves, Distances, Best_Score);
               end if;
            end if;
         end if;
      end loop;
   end Find_Best_B;

   Data_File : File_Type;
   Valve_Name : Valve_Name_Type;
   Valve_Num, Conn_Valve_Num : Valve_Range;
   Conn_Count : Connected_Range;
   Flow_Rate : Integer;
   Success, EOL : Boolean;
   Ch : Character;
   Best_Score : Natural;

   Valves : Valve_Array := (others => <>);
   Useful_Valves : Useful_Array := (others => 1);
   Distances : Distance_Array := (others => (others => Natural'Last));
   Num_Useful : Useful_Range;
   Visited : constant Useful_Bits := (others => False);
   Openings : constant Valve_Openings_Type := (others => <>);

begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day16.txt");

   Num_Useful := 1;

   while not End_Of_File (Data_File) loop
      Skip_N (Data_File, 6);
      Get (Data_File, Valve_Name);
      Parse_Valve_Name (Valve_Name, Valve_Num, Success);
      if not Success then
         Put_Line ("Invalid valve name " & Valve_Name);
         return;
      end if;
      Valves (Valve_Num).Name := Valve_Name;

      Skip_To (Data_File, '=');
      Get (Data_File, Flow_Rate);
      if Flow_Rate >= 0 then
         Valves (Valve_Num).Flow_Rate := Natural (Flow_Rate);
         if Flow_Rate > 0 then
            Useful_Valves (Num_Useful) := Valve_Num;
            if Num_Useful < Useful_Range'Last then
               Num_Useful := Num_Useful + 1;
            end if;
         end if;
      else
         Put_Line ("Invalid flow rate");
         return;
      end if;

      Skip_N (Data_File, 23);
      Look_Ahead (Data_File, Ch, EOL);
      if not EOL and then Ch = 's' then
         Skip_N (Data_File, 2);
      else
         Skip_N (Data_File, 1);
      end if;

      Conn_Count := 1;
      while not End_Of_Line (Data_File) loop
         Get (Data_File, Valve_Name);
         Parse_Valve_Name (Valve_Name, Conn_Valve_Num, Success);
         if not Success then
            Put_Line ("Invalid connected valve name");
            return;
         end if;
         Valves (Valve_Num).Connected (Conn_Count) :=
            Conn_Valve_Num;
         if Conn_Count < Connected_Range'Last then
            Conn_Count := Conn_Count + 1;
         end if;

         if not End_Of_Line (Data_File) then
            Skip_N (Data_File, 2);
         end if;
      end loop;
      Skip_Line (Data_File);

      if Conn_Count = 1 then
         Put_Line ("Not enough connections");
         return;
      else
         Conn_Count := Conn_Count - 1;
      end if;
      Valves (Valve_Num).Num_Connected := Conn_Count;

   end loop;

   if Num_Useful = 1 then
      Put_Line ("No Useful valves");
      return;
   end if;
   Num_Useful := Num_Useful - 1;

   for i in 1 .. Num_Useful loop
      Distances (1, Useful_Valves (i)) :=
         Shortest_Path (1, Useful_Valves (i), Valves);
      for j in 1 .. Num_Useful loop
         if i /= j then
            Distances (Useful_Valves (i), Useful_Valves (j)) :=
               Shortest_Path (Useful_Valves (i),
                  Useful_Valves (j), Valves);
         end if;
      end loop;
   end loop;

   Best_Score := 0;
   Find_Best (1, 0, Openings, 1, Num_Useful, Useful_Valves, Visited,
      Valves, Distances, Best_Score);
   Put_Line ("Best score for part A = " & Natural'Image (Best_Score));

   Best_Score := 0;

   Find_Best_B (1, 0, Openings, 1, Num_Useful, Useful_Valves, Visited,
      Valves, Distances, Best_Score);
   Put_Line ("Best score for part B = " & Natural'Image (Best_Score));

end Day16;
