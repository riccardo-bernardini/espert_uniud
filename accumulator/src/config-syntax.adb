with Ada.Strings.Fixed;        use Ada.Strings;
with Ada.Characters.Handling;
with Ada.Strings.Maps.Constants;

with Gnat.Regpat;


package body Config.Syntax is
   function Strip_Spaces (S : String) return String
   is (Fixed.Trim (Source => S,
                   Side   => Both));

   function Parse_Time_Spec (Spec : String) return Float
   is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps.Constants;
      use Ada.Strings.Maps;

      Stripped : constant String := Strip_Spaces (Spec);

      End_Of_Number : constant Natural :=
                        Index (Source => Stripped,
                               Set    => Decimal_Digit_Set or To_Set ("._-+eE"),
                               Test   => Outside);

      Unit : constant String :=
               (if End_Of_Number = 0
                then
                   ""
                else
                   Strip_Spaces (Stripped (End_Of_Number .. Stripped'Last)));

      Value : constant String :=
                (if End_Of_Number = 0
                 then
                    Stripped
                 else
                    Stripped (Stripped'First .. End_Of_Number - 1));

      --------------
      -- To_Float --
      --------------

      function To_Float (X : String) return Float
      is
         use Gnat.Regpat;

         Integer_Regexp : constant Pattern_Matcher :=
                            Compile ("^[-+]?[0-9_]+$");

         Float_Regexp : constant Pattern_Matcher :=
                          Compile ("^[-+]?[0-9_]+\.[0-9_]+([eE][-+]?[0-9_]+)?$");
      begin
         return (if Match (Integer_Regexp, X) then
                    Float (Integer'Value (X))

                 elsif Match (Float_Regexp, X) then
                    Float'Value (X)

                 else
                    raise Bad_Command_Line
                      with "Bad number '" & X & "'");
      end To_Float;
   begin
      if Unit = "" then
         return To_Float (Value) * Camera_Events.Timestamps_Per_Second;

      elsif Unit = "s" then
         return To_Float (Value);

      elsif Unit = "ms" then
         return 1.0e-3 * To_Float (Value);

      elsif Unit = "us" then
         return 1.0e-6 * To_Float (Value);

      elsif Unit = "ns" then
         return 1.0e-9 * To_Float (Value);

      elsif Unit = "fps" then
         return 1.0 / To_Float (Value);

      else
         raise Bad_Command_Line with "Unknown unit '" & Unit & "'";
      end if;
   end Parse_Time_Spec;


   function Parse_Memory_Spec (Spec : String)
                               return Decay_Spec
   is
      use Ada.Strings.Fixed;
      use Ada.Characters.Handling;

      Colon_Pos : constant Natural := Index (Source  => Spec,
                                             Pattern => ":");

      Method : constant String :=
                 Strip_Spaces (To_Lower ((if Colon_Pos = 0
                               then
                                  Spec
                               else
                                  Spec (Spec'First .. Colon_Pos - 1))));

      Parameter : constant String :=
                    Strip_Spaces ((if Colon_Pos = 0
                                  then
                                     ""
                                  else
                                     Spec (Colon_Pos + 1 .. Spec'Last)));
   begin
      if Method = "step" or Method = "s" or Method = "reset" then
         if Parameter /= "" then
            raise Bad_Command_Line with "'step' with parameter";
         end if;

         return (Class => Reset);

      elsif Method = "none" then
         if Parameter /= "" then
            raise Bad_Command_Line with "'none' with parameter";
         end if;

         return (Class => None);

      elsif Method = "linear" or Method = "lin" or Method = "l" then
         if Parameter = "" then
            raise Bad_Command_Line with "'linear' needs a time constant";
         end if;

         return (Class => Linear, Tau => Parse_Time_Spec (Parameter));

      elsif Method = "exponential" or Method = "exp" or Method = "e" then
         if Parameter = "" then
            raise Bad_Command_Line with "'exponential' needs a time constant";
         end if;

         return (Class => Exponential, Tau => Parse_Time_Spec (Parameter));

      else
         raise Bad_Command_Line with "Unknown dynamic '" & Method & "'";
      end if;
   end Parse_Memory_Spec;

   function Parse_Start_Time (Spec : String) return Camera_Events.Timestamp
   is (if Spec = "" then
          Camera_Events.Minus_Infinity
       else
          Parse_Time_Spec (Spec));

   function Parse_Stop_Time (Spec : String) return Camera_Events.Timestamp
   is (if Spec  = "" then
          Camera_Events.Infinity
       else
          Parse_Time_Spec (Spec));

   function Parse_Output_Filename_Template (Spec : String) return Radix_Spec
   is
      use Ada.Strings.Fixed;


      function To_Unbounded (X : String) return Unbounded_String
                             renames To_Unbounded_String;


      Frame_Number_Position : constant Natural :=
                                Index (Source  => Spec,
                                       Pattern => Frame_Number_Marker);

      function Extract_Format (Filename : String) return Images.Format_Type
      is
         subtype Extension is String (1 .. 3);

         Format_To_Extension   : constant array (Images.Format_Type) of Extension :=
                                   (Images.Raw_Image_8 => "raw",
                                    Images.PGM         => "pgm",
                                    Images.PNG         => "png");
      begin
         if Filename'Length < 5 or else Filename (Filename'Last - 3) /= '.' then
            return Images.Raw_Image_8;
         end if;

         declare
            Ext : constant Extension := Tail (Spec, 3);
         begin
            for Fmt in Format_To_Extension'Range loop
               if Ext = Format_To_Extension (Fmt) then
                  return Fmt;
               end if;
            end loop;

            raise Bad_Command_Line
              with "Unknown extension: '" & Ext & "'";
         end;
      end Extract_Format;

      Frame_Format : Images.Format_Type;

   begin
      if Frame_Number_Position = 0 then
         raise Bad_Command_Line
           with "Missing '" & Frame_Number_Marker & "' in frame filename radix";
      end if;

      Frame_Format := Extract_Format (Spec);

      return Radix_Spec'
        (Head               =>
           To_Unbounded (Spec (Spec'First .. Frame_Number_Position - 1)),
         Tail               =>
           To_Unbounded (Spec (Frame_Number_Position + Frame_Number_Marker'Length .. Spec'Last)),
         Frame_Number_Width => Frame_Number_Default_Width,
         Padding_Char       => Frame_Number_Padding_Char,
         Frame_Format       => Frame_Format);
   end Parse_Output_Filename_Template;

end Config.Syntax;