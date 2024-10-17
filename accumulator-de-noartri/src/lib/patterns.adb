with Ada.Characters.Handling;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;

use Ada.Strings;
use Ada;
package body patterns is
   function Get_Extension (Filename : String) return String
   is (if Filename'Length > 4 and then Filename (Filename'Last - 3) = '.'
       then
          Characters.Handling.To_Lower (Fixed.Tail (Filename, 4))
       else
          "");

   function Chomp (S : String) return String
   is
      use Ada.Strings.Maps.Constants;

      Last : constant Natural := Fixed.Index (Source => S,
                                              Set    => Control_Set,
                                              Test   => Outside,
                                              Going  => Backward);
   begin
      return S (S'First .. Last);
   end Chomp;

   function Strip_Spaces (S : String) return String
   is (Fixed.Trim (Source => S,
                   Side   => Both));

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
                     Status := Integer_Part;

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

   function Is_Integer (X : String) return Boolean
   is
      use Ada.Strings.Maps;

      Tmp : constant String := Strip_Spaces (X);
      Sign : constant Character_Set := To_Set ("+-");
   begin
      if not Is_In (Tmp (Tmp'First), Sign or Constants.Decimal_Digit_Set) then
         return False;
      end if;

      return (for all C of Tmp (Tmp'First + 1 .. Tmp'Last)
              => Is_In (C, Constants.Decimal_Digit_Set));
   end Is_Integer;
end Patterns;
