pragma Spark_Mode (On);
pragma Overflow_Mode (General =>  Eliminated, Assertions => Eliminated);

function Score_A (Opp : Character; Me : Character) return Integer
is
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
