pragma Spark_Mode (On);

with Ada.Text_IO;
use Ada.Text_IO;

procedure Day2 is

   function Score_A (Opp : Character; Me : Character) return Natural;
   function Score_B (Opp : Character; LDW : Character) return Natural;

   function Score_A (Opp : Character; Me : Character) return Natural is
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

   function Score_B (Opp : Character; LDW : Character) return Natural is
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
   A_Sum : Natural;
   A_Score : Natural;
   B_Sum : Natural;
   B_Score : Natural;
   Opp : Character;
   Me : Character;
   Space : Character;
begin
   A_Sum := 0;
   B_Sum := 0;

   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day2.txt");

   while not End_Of_File (Data_File) loop
      Get (Data_File, Opp);
      Get (Data_File, Space);
      Get (Data_File, Me);
      Skip_Line (Data_File);

      A_Score := Score_A (Opp, Me);
      if Natural'Last - A_Score > A_Sum then
         A_Sum := A_Sum + A_Score;
      end if;
      B_Score := Score_B (Opp, Me);
      if Natural'Last - B_Score > B_Sum then
         B_Sum := B_Sum + B_Score;
      end if;
   end loop;

   Put ("Part A score is ");
   Put_Line (Integer'Image (A_Sum));

   Put ("Part B score is ");
   Put_Line (Integer'Image (B_Sum));

end Day2;
