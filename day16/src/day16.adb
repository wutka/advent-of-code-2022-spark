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

   function Shortest_Path (Start, Finish : Valve_Range;
      Valves : Valve_Array) return Natural is
      Nexts : Next_Set.Set (1000);
      Visited : array (Valve_Range) of Boolean := (others => False);
      Distance : Natural;
      Next, New_Next : Next_Valve;
   begin
      Next_Set.Insert (Nexts, (Valve_Num => Start, Distance => 0));

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
      Put_Line ("Couldn't find path");
      return Natural'Last;
   end Shortest_Path;

   procedure Find_Best (From : Valve_Range; Minutes_Consumed : Minutes_Range;
      Flow_Sum : Natural;  Num_Useful : Useful_Range;
      Useful_Valves : Useful_Array; Visited : Useful_Bits;
      Valves : Valve_Array; Distances : Distance_Array;
      Score : Natural; Best_Score : in out Natural)
   is
      Dist : Natural;
      Next_Valve : Valve_Range;
      Old_Minutes_Left, New_Minutes_Left : Minutes_Range;
      New_Minutes_Consumed : Minutes_Range;
      New_Score, Old_Score_Part, New_Score_Part : Natural;
      New_Flow_Rate : Natural;
      Minutes_Left : Minutes_Range;
      New_Visited : Useful_Bits;

   begin
      for i in 1 .. Num_Useful loop
         if not Visited (i) then
            Next_Valve := Useful_Valves (i);
            Dist := Distances (From, Next_Valve) + 1;
            Minutes_Left := Minutes_Range'Last - Minutes_Consumed;

            if Dist < Natural (Minutes_Left) and then
               Natural (Minutes_Consumed) + Dist <=
               Natural (Minutes_Range'Last)
            then
               New_Minutes_Consumed := Minutes_Consumed + Minutes_Range (Dist);
               New_Minutes_Left := Minutes_Range'Last - New_Minutes_Consumed;

               if Flow_Sum = 0 or else
                  (Natural'Last / Flow_Sum > Dist)
               then
                  Old_Score_Part := Score + Flow_Sum * Dist;
                  New_Flow_Rate := 0;
                  if Natural'Last - Flow_Sum >
                     Valves (Next_Valve).Flow_Rate
                  then
                     New_Flow_Rate := Valves (Next_Valve).Flow_Rate + Flow_Sum;
                  end if;

                  if New_Flow_Rate > 0 and then
                     Natural'Last / New_Flow_Rate > Natural (New_Minutes_Left)
                  then
                     New_Score_Part := New_Flow_Rate * Natural (New_Minutes_Left);
                     if Natural'Last - Old_Score_Part > New_Score_Part then
                        New_Score := Old_Score_Part + New_Score_Part;
                        if New_Score > Best_Score then
                           Best_Score := New_Score;
                        end if;
                        New_Visited := Visited;
                        New_Visited (i) := True;
                        Find_Best (Next_Valve, New_Minutes_Consumed,
                           New_Flow_Rate, Num_Useful, Useful_Valves,
                           New_Visited,
                           Valves, Distances, Old_Score_Part, Best_Score);
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end loop;
   end Find_Best;

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

begin
   Open (File => Data_File,
         Mode => In_File,
--         Name => "test.txt");
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
      Distances (1, Useful_Valves (i)) := Shortest_Path (1, Useful_Valves (i), Valves);
      for j in 1 .. Num_Useful loop
         if i /= j then
            Distances (Useful_Valves (i), Useful_Valves (j)) := Shortest_Path (Useful_Valves (i), Useful_Valves (j), Valves);
         end if;
      end loop;
   end loop;

   Best_Score := 0;
   Find_Best (1, 0, 0, Num_Useful, Useful_Valves, Visited,
      Valves, Distances, 0, Best_Score);
   Put_Line ("Best score for part A = " & Natural'Image (Best_Score));

end Day16;
