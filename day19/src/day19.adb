pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Day19 is
   subtype Cost_Range is Natural range 1 .. 20;

   type Costs_Type is record
      Ore_Cost : Cost_Range;
      Clay_Cost : Cost_Range;
      Obs_Ore_Cost : Cost_Range;
      Obs_Clay_Cost : Cost_Range;
      Geode_Ore_Cost : Cost_Range;
      Geode_Obs_Cost : Cost_Range;
   end record;

   type Run_State_Type is record
      Num_Ore : Natural := 1;
      Ore_Amount : Natural := 0;
      Num_Clay : Natural := 0;
      Clay_Amount : Natural := 0;
      Num_Obs : Natural := 0;
      Obs_Amount : Natural := 0;
      Num_Geode : Natural := 0;
      Geode_Amount : Natural := 0;
   end record;

   type Robot_Type is (None, Ore, Clay, Obs, Geode);

   type Minute_Range is range 1 .. 32;

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

   procedure Update_State (Run_State : in out Run_State_Type) is
   begin
      if Natural'Last - Run_State.Ore_Amount > Run_State.Num_Ore then
         Run_State.Ore_Amount := Run_State.Ore_Amount +
            Run_State.Num_Ore;
      end if;
      if Natural'Last - Run_State.Clay_Amount > Run_State.Num_Clay then
         Run_State.Clay_Amount := Run_State.Clay_Amount +
            Run_State.Num_Clay;
      end if;
      if Natural'Last - Run_State.Obs_Amount > Run_State.Num_Obs then
         Run_State.Obs_Amount := Run_State.Obs_Amount +
            Run_State.Num_Obs;
      end if;
      if Natural'Last - Run_State.Geode_Amount > Run_State.Num_Geode then
         Run_State.Geode_Amount := Run_State.Geode_Amount +
            Run_State.Num_Geode;
      end if;
   end Update_State;

   --  See if this robot can go into the sequence at the current
   --  position (if there are enough resources to start creating it)
   procedure Place_Robot_In_Sequence (Robot : Robot_Type;
      Costs : Costs_Type; Run_State : in out Run_State_Type;
      Success : out Boolean) is
   begin
      Success := False;

      case Robot is
         when None => return;
         when Ore =>
            if Run_State.Ore_Amount >= Costs.Ore_Cost
            then
               Run_State.Ore_Amount := Run_State.Ore_Amount -
                  Costs.Ore_Cost;
               Update_State (Run_State);
               if Natural'Last > Run_State.Num_Ore then
                  Run_State.Num_Ore := Run_State.Num_Ore + 1;
               end if;
               Success := True;
            end if;
            return;
         when Clay =>
            if Run_State.Ore_Amount >= Costs.Clay_Cost then
               Run_State.Ore_Amount := Run_State.Ore_Amount -
                  Costs.Clay_Cost;
               Update_State (Run_State);
               if Natural'Last > Run_State.Num_Clay then
                  Run_State.Num_Clay := Run_State.Num_Clay + 1;
               end if;
               Success := True;
            end if;
            return;
         when Obs =>
            if Run_State.Ore_Amount  >= Costs.Obs_Ore_Cost and then
               Run_State.Clay_Amount >= Costs.Obs_Clay_Cost
            then
               Run_State.Ore_Amount := Run_State.Ore_Amount -
                  Costs.Obs_Ore_Cost;
               Run_State.Clay_Amount := Run_State.Clay_Amount -
                  Costs.Obs_Clay_Cost;
               Update_State (Run_State);
               if Natural'Last > Run_State.Num_Obs then
                  Run_State.Num_Obs := Run_State.Num_Obs + 1;
               end if;
               Success := True;
            end if;
            return;
         when Geode =>
            if Run_State.Ore_Amount  >= Costs.Geode_Ore_Cost and then
               Run_State.Obs_Amount >= Costs.Geode_Obs_Cost
            then
               Run_State.Ore_Amount := Run_State.Ore_Amount -
                  Costs.Geode_Ore_Cost;
               Run_State.Obs_Amount := Run_State.Obs_Amount -
                  Costs.Geode_Obs_Cost;
               Update_State (Run_State);
               if Natural'Last > Run_State.Num_Geode then
                  Run_State.Num_Geode := Run_State.Num_Geode + 1;
               end if;
               Success := True;
            end if;
            return;
      end case;
   end Place_Robot_In_Sequence;

   --  Try all possible sequences of creating robots. The length of each
   --  sequence is limited by when each robot can be created, some sequences
   --  such as those that start with geode or obsidian will fail immediately
   procedure Try_Permutation (Costs : Costs_Type; Max_Minutes : Minute_Range;
      Run_State : Run_State_Type; Start_Minute : Minute_Range;
      Best_Num_Geodes : in out Natural)
   is
      Success : Boolean;
      New_Run_State : Run_State_Type;
   begin
      --  If this is the final minute, just update the counts
      if Start_Minute = Max_Minutes then
         New_Run_State := Run_State;
         Update_State (New_Run_State);
         Best_Num_Geodes := Natural'Max (Best_Num_Geodes,
            New_Run_State.Geode_Amount);
         return;
      end if;

      --  Try each robot type
      for r in Ore .. Geode loop
         New_Run_State := Run_State;
         for Minute in Start_Minute .. Max_Minutes loop
            --  See if the robot can be created at the current minute
            Place_Robot_In_Sequence (r, Costs, New_Run_State, Success);

            --  If the robot can be constructed here, then try the next
            --  robot in the sequence
            if Success then
               if Minute < Max_Minutes then
                  Try_Permutation (Costs, Max_Minutes, New_Run_State,
                     Minute + 1, Best_Num_Geodes);
               end if;
               exit;
            else
               Update_State (New_Run_State);
            end if;
         end loop;

         Best_Num_Geodes := Natural'Max (Best_Num_Geodes,
            New_Run_State.Geode_Amount);
      end loop;
   end Try_Permutation;

   Data_File : File_Type;
   Val : Integer;
   Ch : Character;
   Blueprint_Num : Natural;
   Costs : Costs_Type;
   Part_A_Sum : Natural;
   Part_B_Product : Natural;
   Run_State : Run_State_Type;
   Num_Geodes : Natural;

begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day19.txt");

   Part_A_Sum := 0;
   Part_B_Product := 1;

   while not End_Of_File (Data_File) loop
      Skip_N (Data_File, 10);
      Get (Data_File, Ch);
      if Ch in '0' .. '9' then
         Blueprint_Num := Character'Pos (Ch) - 48;
      else
         Put_Line ("Invalid blueprint num");
         return;
      end if;

      Get (Data_File, Ch);
      if Ch in '0' .. '9' then
         Blueprint_Num := Blueprint_Num * 10 + Character'Pos (Ch) - 48;

         Get (Data_File, Ch);  --  Get the colon
      elsif Ch /= ':' then
         Put_Line ("Invalid blueprint num");
         return;
      end if;

      Skip_N (Data_File, 22);
      Get (Data_File, Val);

      if Val > 0 and then Val <= Cost_Range'Last then
         Costs.Ore_Cost := Cost_Range (Val);
      else
         Put_Line ("Invalid ore robot cost");
         return;
      end if;

      Skip_N (Data_File, 28);
      Get (Data_File, Val);

      if Val > 0 and then Val <= Cost_Range'Last then
         Costs.Clay_Cost := Cost_Range (Val);
      else
         Put_Line ("Invalid clay robot cost");
         return;
      end if;

      Skip_N (Data_File, 32);
      Get (Data_File, Val);

      if Val > 0 and then Val <= Cost_Range'Last then
         Costs.Obs_Ore_Cost := Cost_Range (Val);
      else
         Put_Line ("Invalid obsidian robot ore cost");
         return;
      end if;

      Skip_N (Data_File, 9);
      Get (Data_File, Val);

      if Val > 0 and then Val <= Cost_Range'Last then
         Costs.Obs_Clay_Cost := Cost_Range (Val);
      else
         Put_Line ("Invalid obsidian robot clay cost");
         return;
      end if;

      Skip_N (Data_File, 30);
      Get (Data_File, Val);

      if Val > 0 and then Val <= Cost_Range'Last then
         Costs.Geode_Ore_Cost := Cost_Range (Val);
      else
         Put_Line ("Invalid geode robot ore cost");
         return;
      end if;

      Skip_N (Data_File, 9);
      Get (Data_File, Val);

      if Val > 0 and then Val <= Cost_Range'Last then
         Costs.Geode_Obs_Cost := Cost_Range (Val);
      else
         Put_Line ("Invalid geode robot obsidian cost");
         return;
      end if;

      Skip_Line (Data_File);

      Run_State := (others => <>);

      Num_Geodes := 0;
      Try_Permutation (Costs, 24, Run_State, 1, Num_Geodes);

      Put_Line ("Blueprint " & Natural'Image (Blueprint_Num) &
         " can create " & Natural'Image (Num_Geodes) & " geodes");

      if Blueprint_Num > 0 and then
         Natural'Last / Blueprint_Num > Num_Geodes and then
         Natural'Last - Part_A_Sum > Blueprint_Num * Num_Geodes
      then
         Part_A_Sum := Part_A_Sum + Blueprint_Num * Num_Geodes;
      end if;

      if Blueprint_Num < 4 then
         Run_State := (others => <>);

         Num_Geodes := 0;
         Try_Permutation (Costs, 32, Run_State, 1, Num_Geodes);

         Put_Line ("Blueprint " & Natural'Image (Blueprint_Num) &
            " can create " & Natural'Image (Num_Geodes) &
            " geodes in 32 minutes");
         if Num_Geodes > 0 and then
            Natural'Last / Num_Geodes > Part_B_Product
         then
            Part_B_Product := Part_B_Product * Num_Geodes;
         end if;
      end if;

   end loop;

   Put_Line ("Part A sum: " & Natural'Image (Part_A_Sum));
   Put_Line ("Part B product: " & Natural'Image (Part_B_Product));
end Day19;
