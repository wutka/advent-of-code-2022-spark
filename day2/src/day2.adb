pragma Spark_Mode (On);
pragma Overflow_Mode (General =>  Eliminated, Assertions => Eliminated);

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Command_Line;
use Ada.Command_Line;

procedure Day2 is

   function Score_A (Opp : Character; Me : Character) return Integer;
   function Score_B (Opp : Character; LDW : Character) return Integer;

   function Score_A (Opp : Character; Me : Character) return Integer is
   begin
      case Opp is
         when 'A' =>
            case Me is
               when 'X' => return 4;
               when 'Y' => return 8;
               when 'Z' => return 3;
               when others => return 0;
            end case;
         when 'B' =>
            case Me is
               when 'X' => return 1;
               when 'Y' => return 5;
               when 'Z' => return 9;
               when others => return 0;
            end case;
         when 'C' =>
            case Me is
               when 'X' => return 7;
               when 'Y' => return 2;
               when 'Z' => return 6;
               when others => return 0;
            end case;
         when others => return 0;
      end case;
   end Score_A;

   function Score_B (Opp : Character; LDW : Character) return Integer is
   begin
      case Opp is
         when 'A' =>
            case LDW is
               when 'X' => return 3;
               when 'Y' => return 4;
               when 'Z' => return 8;
               when others => return 0;
            end case;
         when 'B' =>
            case LDW is
               when 'X' => return 1;
               when 'Y' => return 5;
               when 'Z' => return 9;
               when others => return 0;
            end case;
         when 'C' =>
            case LDW is
               when 'X' => return 2;
               when 'Y' => return 6;
               when 'Z' => return 7;
               when others => return 0;
            end case;
         when others => return 0;
      end case;
   end Score_B;

   Data_File : File_Type;
   A_Sum : Integer;
   B_Sum : Integer;
   Opp : Character;
   Me : Character;
   Space : Character;
begin
   if Argument_Count /= 1 then
      Put_Line ("No input file supplied");
      return;
   end if;

   A_Sum := 0;
   B_Sum := 0;

   Open (File => Data_File,
         Mode => In_File,
         Name => Argument (1));

   while not End_Of_File (Data_File) loop
      Get (Data_File, Opp);
      Get (Data_File, Space);
      Get (Data_File, Me);
      Skip_Line (Data_File);

      A_Sum := A_Sum + Score_A (Opp, Me);
      B_Sum := B_Sum + Score_B (Opp, Me);
   end loop;

   Close (Data_File);

   Put ("Part A score is ");
   Put_Line (Integer'Image (A_Sum));

   Put ("Part B score is ");
   Put_Line (Integer'Image (B_Sum));

end Day2;
