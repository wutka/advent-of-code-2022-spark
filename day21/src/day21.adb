pragma SPARK_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Formal_Ordered_Maps;

procedure Day21 is
   type Yell_Type is (Value, Formula);

   type Operation_Type is (Add, Subtract, Multiply, Divide);

   subtype Monkey_Name_Type is String (1 .. 4);

   type Monkey_Type (Yell : Yell_Type := Value) is record
      case Yell is
         when Value => Val : Long_Integer := 0;
         when Formula =>
            Op : Operation_Type := Add;
            Left : Monkey_Name_Type := "????";
            Right : Monkey_Name_Type := "????";
      end case;
   end record;

   package Monkey_Map is new Ada.Containers.Formal_Ordered_Maps (
      Key_Type => Monkey_Name_Type, Element_Type => Monkey_Type,
      "<" => "<", "=" => "=");

   function Compute (Name : Monkey_Name_Type;
      Monkeys : Monkey_Map.Map) return Long_Integer
   is
      Monkey : Monkey_Type;
      Left, Right : Long_Integer;
   begin
      if not Monkey_Map.Contains (Monkeys, Name) then
         return 0;
      end if;

      Monkey := Monkey_Map.Element (Monkeys, Name);

      case Monkey.Yell is
         when Value => return Monkey.Val;
         when Formula =>
            Left := Compute (Monkey.Left, Monkeys);
            Right := Compute (Monkey.Right, Monkeys);
            case Monkey.Op is
               when Add =>
                  if (Right >= 0 and then
                      Long_Integer'Last - Right > Left) or else
                     (Right < 0 and then
                      Long_Integer'First - Right < Left)
                  then
                     return Left + Right;
                  else
                     return Long_Integer'Last;
                  end if;
               when Subtract =>
                  if (Right >= 0 and then
                      Long_Integer'First + Right < Left) or else
                     (Right < 0 and then
                      Long_Integer'Last + Right > Left)
                  then
                     return Left - Right;
                  else
                     return Long_Integer'First;
                  end if;
               when Multiply =>
                  if (Left > 1 and then Right > 1 and then
                      Long_Integer'Last / Left > Right) or else
                     (Left > Long_Integer'First and then
                      Left < -1 and then Right > 1 and then
                      Long_Integer'Last / (-Left) > Right) or else
                     (Left > 1 and then Right < -1 and then
                      Right > Long_Integer'First and then
                      Long_Integer'Last / (-Right) > Left) or else
                     (Left < -1 and then Right < -1 and then
                      Right > Long_Integer'First and then
                      Left > Long_Integer'First and then
                      Long_Integer'Last / (-Left) > (-Right))
                  then
                     return Left * Right;
                  else
                     return Long_Integer'Last;
                  end if;
               when Divide =>
                  if Right = 0 then
                     return Long_Integer'Last;
                  else
                     if Left > Long_Integer'First and then
                        Left < Long_Integer'Last and then
                        Right > Long_Integer'First and then
                        Right < Long_Integer'Last
                     then
                        return Left / Right;
                     else
                        return 0;
                     end if;
                  end if;
            end case;
      end case;
   end Compute;

   Data_File : File_Type;
   Ch : Character;
   New_Monkey : Monkey_Type;
   Monkey_Name : Monkey_Name_Type;
   Root_Monkey : Monkey_Type;
   Left, Right : Long_Integer;
   Left2, Right2 : Long_Integer;
   Diff : Long_Integer;
   Amount : Long_Integer;
   Start_Guess : Long_Integer;
   Monkeys : Monkey_Map.Map (10000);
   EOF : Boolean;
   Val : Integer;
   Root_Key : constant Monkey_Name_Type := "root";
   Human_Key : constant Monkey_Name_Type := "humn";

begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day21.txt");

   while not End_Of_File (Data_File) loop

      Get (Data_File, Monkey_Name);
      Get (Data_File, Ch);
      Get (Data_File, Ch);

      Look_Ahead (Data_File, Ch, EOF);
      if Ch in '0' .. '9' then
         New_Monkey := (Yell => Value, others => <>);
         Get (Data_File, Val);
         New_Monkey.Val := Long_Integer (Val);
      else
         New_Monkey := (Yell => Formula, others => <>);
         Get (Data_File, New_Monkey.Left);
         Get (Data_File, Ch);
         Get (Data_File, Ch);
         if Ch = '+' then
            New_Monkey.Op := Add;
         elsif Ch = '-' then
            New_Monkey.Op := Subtract;
         elsif Ch = '*' then
            New_Monkey.Op := Multiply;
         elsif Ch = '/' then
            New_Monkey.Op := Divide;
         else
            Put_Line ("Invalid operation - " & Character'Image (Ch));
            return;
         end if;
         Get (Data_File, Ch);
         Get (Data_File, New_Monkey.Right);
      end if;

      if Monkey_Map.Length (Monkeys) < Monkeys.Capacity and then
         not Monkey_Map.Contains (Monkeys, Monkey_Name)
      then
         Monkey_Map.Insert (Monkeys, Monkey_Name, New_Monkey);
      end if;
   end loop;

   Put_Line ("Part A result is " & Long_Integer'Image (
      Compute ("root", Monkeys)));

   if Monkey_Map.Is_Empty (Monkeys) or else
      not Monkey_Map.Contains (Monkeys, Root_Key)
   then
      Put_Line ("No root");
      return;
   end if;

   Root_Monkey := Monkey_Map.Element (Monkeys, Root_Key);
   if Root_Monkey.Yell /= Formula then
      Put_Line ("Root should be a formula");
      return;
   end if;

   --  Start with an initial guess
   Start_Guess := 1;
   loop
      New_Monkey := (Yell => Value, Val => Start_Guess);
      if Monkey_Map.Contains (Monkeys, Human_Key) then
         Monkey_Map.Replace (Monkeys, Human_Key, New_Monkey);
      end if;

      Left := Compute (Root_Monkey.Left, Monkeys);
      Right := Compute (Root_Monkey.Right, Monkeys);

      if Left = Right then
         Put_Line ("Part B answer is " & Long_Integer'Image (Start_Guess));
         return;
      end if;

      --  Try one higher than the guess
      if Start_Guess < Long_Integer'Last then
         New_Monkey := (Yell => Value, Val => Start_Guess + 1);
      end if;

      if Monkey_Map.Contains (Monkeys, Human_Key) then
         Monkey_Map.Replace (Monkeys, Human_Key, New_Monkey);
      end if;

      Left2 := Compute (Root_Monkey.Left, Monkeys);
      Right2 := Compute (Root_Monkey.Right, Monkeys);

      if Left2 = Right2 then
         if Long_Integer'Last > Start_Guess then
            Put_Line ("Part B answer is " & Long_Integer'Image (Start_Guess + 1));
         end if;
         return;
      end if;

      --  Depending on the difference between left and right,
      --  choose a different increment

      Diff := 0;

      if (Right >= 0 and then
          Long_Integer'First + Right < Left) or else
         (Right < 0 and then
          Long_Integer'Last + Right > Left)
      then
         Diff := abs (Left - Right);
      end if;
      if Diff > 1_000_000_000_000 then
         Amount := 10_000_000_000;
      elsif Diff > 1_000_000_000 then
         Amount := 10_000_000;
      elsif Diff > 10_000_000 then
         Amount := 100_000;
      elsif Diff > 1_000_000 then
         Amount := 10_000;
      elsif Diff > 1_000 then
         Amount := 100;
      else
         Amount := 1;
      end if;

      --  Change the starting guess based on whether we
      --  need to go up or down and which direction the
      --  change in guess goes
      if Left < Right then
         if Left2 > Left then
            if Long_Integer'Last - Amount > Start_Guess then
               Start_Guess := Start_Guess + Amount;
            end if;
         else
            if Long_Integer'First + Amount < Start_Guess then
               Start_Guess := Start_Guess - Amount;
            end if;
         end if;
      else
         if Left2 > Left then
            if Long_Integer'First + Amount < Start_Guess then
               Start_Guess := Start_Guess - Amount;
            end if;
         else
            if Long_Integer'Last - Amount > Start_Guess then
               Start_Guess := Start_Guess + Amount;
            end if;
         end if;
      end if;

   end loop;
end Day21;
