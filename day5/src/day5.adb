pragma Spark_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Day5 is
   type Stack_Range is range 0 .. 500;
   type Stack_Array is array (Stack_Range) of Character;

   type Stack is
      record
         Stack_Data : Stack_Array := (others => ' ');
         Stack_Size : Stack_Range := 0;
      end record;

   type Stacks_Range is range 1 .. 9;
   type Stacks_Array is array (Stacks_Range) of Stack;

   function Stack_Is_Empty (s : Stack) return Boolean;
   function Stack_Is_Full (s : Stack) return Boolean;
   function Stack_Size (s : Stack) return Stack_Range;

   function Stack_Is_Empty (s : Stack) return Boolean is (s.Stack_Size = 0);
   function Stack_Is_Full (s : Stack) return Boolean is
   begin
      return (s.Stack_Size = Stack_Range'Last);
   end Stack_Is_Full;

   function Stack_Size (s : Stack) return Stack_Range is (s.Stack_Size);

   procedure Stack_Push (ch : Character; s : in out Stack)
      with
      Pre => s.Stack_Size < Stack_Range'Last,
      Post => s.Stack_Size = s.Stack_Size'Old + 1
   is
   begin
      s.Stack_Data (s.Stack_Size) := ch;
      s.Stack_Size := s.Stack_Size + 1;
   end Stack_Push;

   procedure Stack_Pop (s : in out Stack; ch : out Character)
      with
      Pre => s.Stack_Size > 0,
      Post => s.Stack_Size = s.Stack_Size'Old - 1
   is
   begin
      s.Stack_Size := s.Stack_Size - 1;
      ch := s.Stack_Data (s.Stack_Size);
   end Stack_Pop;

   procedure Stack_Transfer (Amount : Stack_Range;
      From : Stacks_Range; To : Stacks_Range;
      Stacks : in out Stacks_Array)
      with
      Pre => Amount > 0 and then
             To /= From and then
             Stacks (From).Stack_Size >= Amount and then
             Stacks (To).Stack_Size < Stack_Range'Last - Amount
   is
   begin
      Stacks (To).Stack_Data (
         Stacks (To).Stack_Size .. Stacks (To).Stack_Size +
         Amount - 1) :=
         Stacks (From).Stack_Data (Stacks (From).Stack_Size -
         Amount .. Stacks (From).Stack_Size - 1);
      Stacks (To).Stack_Size := Stacks (To).Stack_Size + Amount;
      Stacks (From).Stack_Size := Stacks (From).Stack_Size - Amount;
   end Stack_Transfer;

   procedure Stack_Reverse (s : in out Stack) is
      Temp_Stack : Stack_Array;
   begin
      if s.Stack_Size > 0 then
         Temp_Stack := s.Stack_Data;
         for i in 1 .. s.Stack_Size loop
            s.Stack_Data (s.Stack_Size - i) := Temp_Stack (i - 1);
         end loop;
      end if;
   end Stack_Reverse;

   procedure Print_Stack (s : Stack) is
   begin
      if s.Stack_Size = 0 then
         Put_Line ("(empty)");
      else
         for i in 1 .. s.Stack_Size loop
            Put (s.Stack_Data (i - 1));
         end loop;
         Put_Line ("");
      end if;
   end Print_Stack;

   procedure Read_Natural (Data_File : File_Type; n : out Natural) is
      val : Integer;
   begin
      Get (Data_File, val);
      if val >= 0 then
         n := Natural (val);
      else
         n := 0;
      end if;
   end Read_Natural;

   Data_File : File_Type;
   Stacks_A : Stacks_Array;
   Stacks_B : Stacks_Array;
   Line : String (1 .. 1000);
   Line_Len : Natural;
   Stack_Columns : array (Stacks_Range) of Natural;
   Amount : Natural;
   From : Stacks_Range;
   To : Stacks_Range;
   NatVal : Natural;
   ch : Character;

begin
   Stack_Columns := (2, 6, 10, 14, 18, 22, 26, 30, 34);

   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day5.txt");

   while not End_Of_File (Data_File) loop
      Get_Line (Data_File, Line, Line_Len);
      if Line_Len > 0 then
         if Line (1) = ' ' then
            exit;
         end if;

         for i in Stacks_Range loop
            if Natural (i) < Line_Len and then
               Line (Stack_Columns (i)) /= ' '
            then
               if not Stack_Is_Full (Stacks_A (i)) then
                  Stack_Push (Line (Stack_Columns (i)), Stacks_A (i));
               else
                  Put_Line ("Stack full during initialization");
                  return;
               end if;
            end if;
         end loop;
      end if;
   end loop;
   Skip_Line (Data_File);

   for i in Stacks_Range loop
      Stack_Reverse (Stacks_A (i));
   end loop;

   Stacks_B := Stacks_A;

   while not End_Of_File (Data_File) loop
      Get (Data_File, Line (1 .. 5));
      Read_Natural (Data_File, Amount);
      Get (Data_File, Line (1 .. 6));
      Read_Natural (Data_File, NatVal);
      if NatVal >= Natural (Stacks_Range'First) and then
         NatVal <= Natural (Stacks_Range'Last)
      then
         From := Stacks_Range (NatVal);
      else
         Put_Line ("Invalid stack number " & Natural'Image (NatVal));
         return;
      end if;
      Get (Data_File, Line (1 .. 4));
      Read_Natural (Data_File, NatVal);
      if NatVal >= Natural (Stacks_Range'First) and then
         NatVal <= Natural (Stacks_Range'Last)
      then
         To := Stacks_Range (NatVal);
      else
         Put_Line ("Invalid stack number " & Natural'Image (NatVal));
         return;
      end if;
      Skip_Line (Data_File);

      if Amount > 0 then
         for i in 1 .. Amount loop
            if Stack_Is_Empty (Stacks_A (From)) then
               Put_Line ("Error - move from empty stack");
               return;
            end if;
            if Stack_Is_Full (Stacks_A (To)) then
               Put_Line ("Error - move to full stack");
               return;
            end if;
            Stack_Pop (Stacks_A (From), ch);
            Stack_Push (ch, Stacks_A (To));
         end loop;

         if Natural (Stacks_B (From).Stack_Size) >= Amount and then
            Natural (Stacks_B (To).Stack_Size) <
            Natural (Stack_Range'Last) - Amount and then
            Amount <= Natural (Stack_Range'Last) and then
            From /= To
         then
            Stack_Transfer (Stack_Range (Amount),
               From, To, Stacks_B);
         else
            Put_Line ("Invalid stack transfer");
            return;
         end if;

      end if;
   end loop;

   Put ("Part A Result: ");
   for i in Stacks_Range loop
      if not Stack_Is_Empty (Stacks_A (i)) then
         pragma Assert (Stacks_A (i).Stack_Size > 0);
         Stack_Pop (Stacks_A (i), ch);
         Put (ch);
      end if;
   end loop;
   Put_Line ("");

   Put ("Part B Result: ");
   for i in Stacks_Range loop
      if not Stack_Is_Empty (Stacks_B (i)) then
         pragma Assert (Stacks_B (i).Stack_Size > 0);
         Stack_Pop (Stacks_B (i), ch);
         Put (ch);
      end if;
   end loop;
   Put_Line ("");

end Day5;
