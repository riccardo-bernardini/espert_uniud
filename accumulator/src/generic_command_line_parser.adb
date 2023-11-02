pragma Ada_2012;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Ordered_Maps;

package body Generic_Command_Line_Parser is
   Help_Lines : String_Vectors.Vector;

   package Name_Tables is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                 Element_Type => Options);

   procedure Parse_Option_Names (Source    : String;
                                 Names     : out String_Vectors.Vector;
                                 Help_Line : out Unbounded_String)
   is
      Name_Accumulator : Unbounded_String := Null_Unbounded_String;
      C                : Character;
   begin
      Names.Clear;

      for I in Source'Range loop
         C := Source (I);

         case C is
            when '=' | ' ' | ',' =>
               if Name_Accumulator = Null_Unbounded_String then
                  raise Bad_Option_Name with Source;
               end if;

               Names.Append (To_String (Name_Accumulator));

               Name_Accumulator := Null_Unbounded_String;

               if C = '=' or C = ' ' then
                  Help_Line :=
                    To_Unbounded_String (Names.First_Element)
                    & Source (I .. Source'Last);

                  return;
               end if;

            when others =>
               Name_Accumulator := Name_Accumulator & C;
         end case;
      end loop;

      if Name_Accumulator /= Null_Unbounded_String then
         Names.Append (To_String (Name_Accumulator));
      end if;
   end Parse_Option_Names;

   -----------
   -- Parse --
   -----------

   function Parse
     (Source                  : String;
      Names                   : Option_Names;
      Mandatory               : Option_Flags     := All_No;
      When_Repeated           : When_Repeated_Do := Always_Die;
      Option_Value_Separator  : Character        := ':';
      Include_Prefix          : String := "@";
      Concatenation_Separator : String := ",")
      return Option_Values
   is

      Name_To_Option : Name_Tables.Map;

      Result : Option_Values := (others => (Missing => True));

   begin
      for Option in Options loop
         declare
            Parsed_Names : String_Vectors.Vector;
            Help_Line    : Unbounded_String;
         begin
            Parse_Option_Names (Source    => To_String(Names (Option)),
                                Names     => Parsed_Names,
                                Help_Line => Help_Line);

            if Help_Line /= Null_Unbounded_String then
               Help_Lines.Append (To_String (Help_Line));
            end if;

            for Name of Parsed_Names loop
               if Name_To_Option.Contains (Name) then
                  raise Duplicate_Option_Name with Name;
               end if;

               Name_To_Option.Insert (Key      => Name,
                                      New_Item => Option);
            end loop;
         end;
      end loop;

      Result (Options'First) := (False, To_Unbounded_String ("pippo"));

      return Result;

      pragma Compile_Time_Warning (Standard.True, "Parse unimplemented");
      return raise Program_Error with "Unimplemented function Parse";
   end Parse;

   -----------
   -- Parse --
   -----------

   function Parse
     (Names                   : Option_Names;
      Mandatory               : Option_Flags := All_No;
      When_Repeated           : When_Repeated_Do := Always_Die;
      Option_Value_Separator  : Character        := ':';
      Include_Prefix          : String := "@";
      Concatenation_Separator : String := ",")
      return Option_Values
   is
      function Command_Line_Restored return String
      is
         use Ada, Ada.Command_Line;

         Accumulator : Unbounded_String := Null_Unbounded_String;
      begin
         for I in 1 .. Command_Line.Argument_Count loop
            Accumulator := Accumulator & " " & Argument (I);
         end loop;

         return To_String (Accumulator);
      end Command_Line_Restored;
   begin
      return Parse (Source                  => Command_Line_Restored,
                    Names                   => Names,
                    Mandatory               => Mandatory,
                    When_Repeated           => When_Repeated,
                    Option_Value_Separator  => Option_Value_Separator,
                    Include_Prefix          => Include_Prefix,
                    Concatenation_Separator => Concatenation_Separator);
   end Parse;

   -----------------------
   -- Option_Help_Lines --
   -----------------------

   function Option_Help_Lines return String_Vectors.Vector is
   begin
      return Help_Lines;
   end Option_Help_Lines;

end Generic_Command_Line_Parser;
