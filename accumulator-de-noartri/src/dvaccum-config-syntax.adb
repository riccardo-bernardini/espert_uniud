--  with Ada.Strings.Fixed;        use Ada.Strings;

package body DVAccum.Config.Syntax is
   --  function Strip_Spaces (S : String) return String
   --  is (Fixed.Trim (Source => S,
   --                  Side   => Both));


   function Parse_Output_Filename_Template (Template : String)
                                            return String_Formatting.Parsed_Format
   is
   begin
      return String_Formatting.Parse_Format (Format              => Template,
                                             Accepted_Directives => "bd");

   end Parse_Output_Filename_Template;


   --  function Parse_Output_Filename_Template (Template : Unbounded_String) return Radix_Spec
   --  is
   --     use Ada.Strings.Fixed;
   --
   --
   --     function To_Unbounded (X : String) return Unbounded_String
   --                            renames To_Unbounded_String;
   --
   --
   --     function Extract_Format (Filename : String) return frames.Format_Type
   --     is
   --        subtype Extension is String (1 .. 3);
   --
   --        Format_To_Extension   : constant array (Frames.Format_Type) of Extension :=
   --                                  (Frames.Raw_Image_8 => "raw",
   --                                   Frames.PGM         => "pgm",
   --                                   Frames.PNG         => "png");
   --     begin
   --        if Filename'Length < 5 or else Filename (Filename'Last - 3) /= '.' then
   --           return Frames.Raw_Image_8;
   --        end if;
   --
   --        declare
   --           Ext : constant Extension := Tail (Filename, 3);
   --        begin
   --           for Fmt in Format_To_Extension'Range loop
   --              if Ext = Format_To_Extension (Fmt) then
   --                 return Fmt;
   --              end if;
   --           end loop;
   --
   --           raise Bad_Syntax
   --             with "Unknown extension: '" & Ext & "'";
   --        end;
   --     end Extract_Format;
   --
   --     Frame_Format : Frames.Format_Type;
   --
   --     Spec : constant String := To_String (Template);
   --
   --     Frame_Number_Position : constant Natural :=
   --                               Index (Source  => Spec,
   --                                      Pattern => Frame_Number_Marker);
   --
   --  begin
   --     if Frame_Number_Position = 0 then
   --        raise Bad_Syntax
   --          with "Missing '" & Frame_Number_Marker & "' in frame filename radix";
   --     end if;
   --
   --     Frame_Format := Extract_Format (Spec);
   --
   --     return Radix_Spec'
   --       (Head               =>
   --          To_Unbounded (Spec (Spec'First .. Frame_Number_Position - 1)),
   --        Tail               =>
   --          To_Unbounded (Spec (Frame_Number_Position + Frame_Number_Marker'Length .. Spec'Last)),
   --        Frame_Number_Width => Frame_Number_Default_Width,
   --        Padding_Char       => Frame_Number_Padding_Char,
   --        Frame_Format       => Frame_Format);
   --  end Parse_Output_Filename_Template;

end DVAccum.Config.Syntax;
