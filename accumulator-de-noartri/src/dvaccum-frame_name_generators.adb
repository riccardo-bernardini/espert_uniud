package body Dvaccum.Frame_Name_Generators is
   function Make_Name (Object : Generator_Type;
                       Frame  : Event_Processing.Frame_Index)
                       return String
   is
      function Provider (Directive : Character;
                         Parameter : String;
                         Counter   : Positive)
                         return String
      is
         pragma Unreferenced (Counter);
      begin
         case Directive is
            when 'd' =>
               return String_Formatting.C_Style_Formatting
                 (Datum     => Event_Processing.To_Int (Frame),
                  Parameter => Parameter);

            when 'b' =>
               return To_String (Object.Input_Basename);

            when others =>
               raise Constraint_Error;
         end case;
      end Provider;
   begin
      return String_Formatting.Expand (Object.Format, Provider'Access);
   end Make_Name;

   function New_Generator (Format   : String_Formatting.Parsed_Format;
                           Basename : String)
                           return Generator_Type
   is ((To_Unbounded_String (Basename), Format));

end Dvaccum.Frame_Name_Generators;
