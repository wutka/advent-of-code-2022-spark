pragma Spark_Mode (On);
pragma Overflow_Mode (General =>  Eliminated, Assertions => Eliminated);

function Score_B (Opp : Character; LDW : Character) return Integer
is
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
