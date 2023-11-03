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

   procedure Fill_Name_Table (Names      : Option_Names;
                              Prefix     : String;
                              Table      : out Name_Tables.Map;
                              Help_Lines : out String_Vectors.Vector)
   is
      Parsed_Names : String_Vectors.Vector;
      Help_Line    : Unbounded_String;
   begin
      for Option in Options loop
         Parse_Option_Names (Source    => To_String (Names (Option)),
                             Names     => Parsed_Names,
                             Help_Line => Help_Line);

         if Help_Line /= Null_Unbounded_String then
            Help_Lines.Append (To_String (Help_Line));
         end if;

         for Name of Parsed_Names loop
            declare
               Full_Name : constant String := Prefix & Name;
            begin
               if Table.Contains (Full_Name) then
                  raise Duplicate_Option_Name with Name;

               else
                  Table.Insert (Key      => Full_Name,
                                New_Item => Option);
               end if;

            end;
         end loop;
      end loop;
   end Fill_Name_Table;

   ------------
   -- Update --
   ------------

   procedure Update (What                    : in out Option_Values;
                     Name                    : String;
                     Value                   : String;
                     Name_To_Option          : Name_Tables.Map;
                     When_Repeated           : When_Repeated_Do;
                     Concatenation_Separator : String)
   is
      use Name_Tables;

      Pos : constant Cursor := Name_To_Option.Find (Name);
   begin
      if Pos = No_Element then
         raise Unknown_Option with Name;
      end if;

      declare
         Option : constant Options := Element (Pos);
      begin
         if What (Option).Missing then
            What (Option) := (Missing => False,
                              Value   => To_Unbounded_String (Value));

            return;
         end if;

         pragma Assert (not What (Option).Missing);

         case When_Repeated (Option) is
            when Die =>
               raise Repeated_Option with Name;

            when Concatenate =>
               What (Option).Value := What (Option).Value
                 & Concatenation_Separator
                 & To_Unbounded_String (Value);
            when Ignore =>
               null;

            when Overwrite =>
               What (Option).Value := To_Unbounded_String (Value);
         end case;
      end;
   end Update;


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
      Option_Prefix           : String := "--";
      Concatenation_Separator : String := ",")
      return Option_Values
   is
      pragma Unreferenced (Include_Prefix);
      type Status_Type is
        (
         Skipping_Spaces,
         In_Name,
         Begin_Of_Value,
         In_Value
        );

      Name_To_Option : Name_Tables.Map;

      Result       : Option_Values := (others => (Missing => True));

      Status : Status_Type := Skipping_Spaces;

      Name_Accumulator  : Unbounded_String;
      Value_Accumulator : Unbounded_String;
   begin
      Fill_Name_Table (Names      => Names,
                       Table      => Name_To_Option,
                       Prefix     => Option_Prefix,
                       Help_Lines => Help_Lines);

      declare
         Padded_Source : constant String := Source & " ";
         Closing_Value : Character;
      begin
         for Current_Char of Padded_Source loop
            case Status is
               when Skipping_Spaces =>
                  if Current_Char /= ' ' then
                     Name_Accumulator := Null_Unbounded_String & Current_Char;
                  end if;

               when In_Name =>
                  if Current_Char = ' '  then
                     Update (What => Result,
                             Name  => To_String (Name_Accumulator),
                             Value          => "",
                             Name_To_Option => Name_To_Option,
                             When_Repeated           => When_Repeated,
                             Concatenation_Separator => Concatenation_Separator);

                  elsif Current_Char = Option_Value_Separator then
                     Status := Begin_Of_Value;

                  else
                     Name_Accumulator := Name_Accumulator & Current_Char;
                  end if;

               when Begin_Of_Value =>
                  Value_Accumulator := Null_Unbounded_String;
                  Status := In_Value;

                  if Current_Char = '"' then
                     Closing_Value := '"';

                  else
                     Closing_Value := ' ';
                     Value_Accumulator := Value_Accumulator & Current_Char;

                  end if;

               when In_Value =>
                  if Current_Char = Closing_Value  then
                     Update (What                    => Result,
                             Name                    => To_String (Name_Accumulator),
                             Value                   => To_String (Value_Accumulator),
                             Name_To_Option          => Name_To_Option,
                             When_Repeated           => When_Repeated,
                             Concatenation_Separator => Concatenation_Separator);

                     Name_Accumulator := Null_Unbounded_String;
                     Value_Accumulator := Null_Unbounded_String;

                     Status := Skipping_Spaces;
                  else
                     Value_Accumulator := Value_Accumulator & Current_Char;
                  end if;

            end case;
         end loop;
      end;

      declare
         Missing_Options : Unbounded_String;
      begin
         for Opt in Options loop
            if Result (Opt).Missing and Mandatory (Opt) then
               Missing_Options := Missing_Options
                 & " "
                 & Options'Image (Opt);
            end if;
         end loop;

         if Missing_Options /= Null_Unbounded_String then
            raise Missing_Mandatory_Options with To_String (Missing_Options);
         end if;
      end;

      return Result;

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
      Option_Prefix           : String := "--";
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
                    Option_Prefix           => Option_Prefix,
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
