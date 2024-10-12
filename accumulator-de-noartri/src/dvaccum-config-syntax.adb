with Ada.Strings.Fixed;        use Ada.Strings;
with Ada.Characters.Handling;
--  with Ada.Strings.Maps.Constants;

package body DVAccum.Config.Syntax is
   function Strip_Spaces (S : String) return String
   is (Fixed.Trim (Source => S,
                   Side   => Both));


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
            raise Bad_Syntax with "'step' with parameter";
         end if;

         return (Class => Reset);

      elsif Method = "none" then
         if Parameter /= "" then
            raise Bad_Syntax with "'none' with parameter";
         end if;

         return (Class => None);

      elsif Method = "linear" or Method = "lin" or Method = "l" then
         if Parameter = "" then
            raise Bad_Syntax with "'linear' needs a time constant";
         end if;

         return (Class => Linear, Tau => Times.Value (Parameter));

      elsif Method = "exponential" or Method = "exp" or Method = "e" then
         if Parameter = "" then
            raise Bad_Syntax with "'exponential' needs a time constant";
         end if;

         return (Class => Exponential, Tau => Times.Value (Parameter));

      else
         raise Bad_Syntax with "Unknown dynamic '" & Method & "'";
      end if;
   end Parse_Memory_Spec;

   --  function Parse_Start_Time (Spec : String) return Camera_Events.Timestamp
   --  is (if Spec = "" then
   --         Camera_Events.Minus_Infinity
   --      else
   --         Parse_Time_Spec (Spec));
   --
   --  function Parse_Stop_Time (Spec : String) return Camera_Events.Timestamp
   --  is (if Spec  = "" then
   --         Camera_Events.Infinity
   --      else
   --         Parse_Time_Spec (Spec));

   function Parse_Output_Filename_Template (Template : Unbounded_String) return Radix_Spec
   is
      use Ada.Strings.Fixed;


      function To_Unbounded (X : String) return Unbounded_String
                             renames To_Unbounded_String;


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
            Ext : constant Extension := Tail (Filename, 3);
         begin
            for Fmt in Format_To_Extension'Range loop
               if Ext = Format_To_Extension (Fmt) then
                  return Fmt;
               end if;
            end loop;

            raise Bad_Syntax
              with "Unknown extension: '" & Ext & "'";
         end;
      end Extract_Format;

      Frame_Format : Images.Format_Type;

      Spec : constant String := To_String (Template);

      Frame_Number_Position : constant Natural :=
                                Index (Source  => Spec,
                                       Pattern => Frame_Number_Marker);

   begin
      if Frame_Number_Position = 0 then
         raise Bad_Syntax
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

end DVAccum.Config.Syntax;
