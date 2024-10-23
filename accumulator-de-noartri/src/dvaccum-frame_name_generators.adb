package body Dvaccum.Frame_Name_Generators is
   type Provider_Object is
     new String_Formatting.Provider_Interface
       with
      record
         Input_Basename : Unbounded_String;
         Frame_Number   : Frame_Index;
      end record;


   function Provide
     (Provider  : Provider_Object;
      Directive : Character;
      Parameter : String;
      Counter   : Positive)
      return String;

   function Provide
     (Provider  : Provider_Object;
      Directive : Character;
      Parameter : String;
      Counter   : Positive)
      return String
   is
      pragma Unreferenced (Counter);
   begin
      case Directive is
         when 'd' =>
            return String_Formatting.C_Style_Formatting
              (Datum     => Integer (provider.Frame_Number),
               Parameter => Parameter);

         when 'b' =>
            return To_String (Provider.Input_Basename);

         when others =>
            raise Constraint_Error;
      end case;
   end Provide;


   function Make_Name (Object : Generator_Type;
                       Frame  : Frame_Index)
                       return String
   is
   begin
      return String_Formatting.Expand
        (Object.Format,
         Provider_Object'(Input_Basename => object.Input_Basename,
                          Frame_Number   => frame));
   end Make_Name;

   function New_Generator (Format   : String_Formatting.Parsed_Format;
                           Basename : String)
                           return Generator_Type
   is ((To_Unbounded_String (Basename), Format));

end Dvaccum.Frame_Name_Generators;
