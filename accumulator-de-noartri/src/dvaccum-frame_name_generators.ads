with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with String_Formatting;

with DVAccum.Event_Processing;

package Dvaccum.Frame_Name_Generators is
   type Generator_Type is
     new Event_Processing.Frame_Name_Generator
   with private;

   function Make_Name (Object : Generator_Type;
                       Frame  : Event_Processing.Frame_Index)
                       return String;

   function New_Generator (Format   : String_Formatting.Parsed_Format;
                           Basename : String)
                           return Generator_Type;
private
   type Generator_Type is
     new Event_Processing.Frame_Name_Generator
   with record
      Input_Basename : Unbounded_String;
      Format         : String_Formatting.Parsed_Format;
   end record;
end Dvaccum.Frame_Name_Generators;
