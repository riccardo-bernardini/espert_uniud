with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with String_Formatting;


package Dvaccum.Frame_Name_Generators is
   --
   --  Frame_Name_Generator returns the name of the file used
   --  to save a frame.  If Frame=No_Frame the function can return
   --  any value since it will not be used.
   --
   type Abstract_Generator is interface;

   function Make_Name (Object : Abstract_Generator;
                       Frame  : Frame_Index)
                       return String
                       is abstract;

   type Generator_Type is
     new Abstract_Generator
   with
     private;

   function Make_Name (Object : Generator_Type;
                       Frame  : Frame_Index)
                       return String;

   function New_Generator (Format   : String_Formatting.Parsed_Format;
                           Basename : String)
                           return Generator_Type;
private
   type Generator_Type is
     new Abstract_Generator
   with record
      Input_Basename : Unbounded_String;
      Format         : String_Formatting.Parsed_Format;
   end record;
end Dvaccum.Frame_Name_Generators;
