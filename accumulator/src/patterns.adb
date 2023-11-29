package body patterns is
   function Is_Float (X : String) return Boolean
   is
      type Status_Type is
        (
         Start,
         Sign,
         Integer_Part,
         Dot,
         Fractional_Part,
         Exponent,
         Sign_Exponent,
         Exponent_Value
        );

      Status : Status_Type := Start;

      Is_Final : constant array (Status_Type) of Boolean :=
                   (
                    Start | Sign | Dot | Exponent | Sign_Exponent   => False,
                    Integer_Part | Fractional_Part | Exponent_Value => True
                   );
   begin
      for Current_Char of X loop
         case Status is
            when Start =>
               case Current_Char is
                  when '+' | '-' =>
                     Status := Sign;

                  when '0' .. '9' =>
                     Status := Integer_Part;

                  when others =>
                     return False;
               end case;

            when Sign =>
               case Current_Char is
                  when '0' .. '9' =>
                     Status := Fractional_Part;

                  when others =>
                     return False;
               end case;


            when Integer_Part =>
               case Current_Char is
                  when '.'  =>
                     Status := Dot;

                  when '0' .. '9' =>
                     Status := Integer_Part;

                  when others =>
                     return False;
               end case;

            when Dot =>
               case Current_Char is
                  when '0' .. '9' =>
                     Status := Fractional_Part;

                  when others =>
                     return False;
               end case;


            when Fractional_Part =>
               case Current_Char is
                  when 'e' | 'E'  =>
                     Status := Exponent;

                  when '0' .. '9' =>
                     Status := Fractional_Part;

                  when others =>
                     return False;
               end case;


            when Exponent =>
               case Current_Char is
                  when '+' | '-'  =>
                     Status := Sign_Exponent;

                  when '0' .. '9' =>
                     Status := Exponent_Value;

                  when others =>
                     return False;
               end case;

            when Sign_Exponent =>
               case Current_Char is
                  when '0' .. '9' =>
                     Status := Fractional_Part;

                  when others =>
                     return False;
               end case;

            when Exponent_Value =>
               case Current_Char is
                  when '0' .. '9' =>
                     Status := Exponent_Value;

                  when others =>
                     return False;
               end case;
         end case;
      end loop;

      return Is_Final (Status);
   end Is_Float;
end Patterns;
