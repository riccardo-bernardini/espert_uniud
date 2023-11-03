pragma Ada_2012;
with Ada.Command_Line;
--  with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Formal_Ordered_Maps;


package body Generic_Command_Line_Parser is

   package Name_Tables is
     new Ada.Containers.Formal_Ordered_Maps (Key_Type     => Unbounded_String,
                                             Element_Type => Options);

   procedure Parse_Option_Names (Source    : String;
                                 Names     : in out String_Vectors.Vector;
                                 Help_Line : out Unbounded_String)
   is
      use String_Vectors;

      Name_Accumulator : Unbounded_String := Null_Unbounded_String;
      C                : Character;
   begin
      Help_Line := Null_Unbounded_String;

      for I in Source'Range loop
         C := Source (I);

         case C is
            when '=' | ' ' | ',' =>
               if Name_Accumulator = Null_Unbounded_String then
                  raise Bad_Option_Name with Source;
               end if;

               Append (Names, Name_Accumulator);

               Name_Accumulator := Null_Unbounded_String;

               if C = '=' or C = ' ' then
                  Help_Line := First_Element (Names)  & Source (I .. Source'Last);

                  return;
               end if;

            when others =>
               Name_Accumulator := Name_Accumulator & C;
         end case;
      end loop;

      if Name_Accumulator /= Null_Unbounded_String then
         Append (Names, Name_Accumulator);
      end if;
   end Parse_Option_Names;

   procedure Fill_Name_Table (Names      : Option_Names;
                              Prefix     : String;
                              Table      : in out Name_Tables.Map;
                              Help_Lines : in out String_Vectors.Vector)
   is
      use String_Vectors;
      use Name_Tables;

      Parsed_Names : String_Vectors.Vector (4096);
      Help_Line    : Unbounded_String;
   begin

      for Option in Options loop
         Parse_Option_Names (Source    => To_String (Names (Option)),
                             Names     => Parsed_Names,
                             Help_Line => Help_Line);

         if Help_Line /= Null_Unbounded_String then
            Append (Help_Lines, Help_Line);
         end if;

         for I in First_Index (Parsed_Names) .. Last_Index (Parsed_Names) loop
            declare
               Name      : constant String := To_String (Element (Parsed_Names, I));
               Full_Name : constant String := Prefix & Name;
            begin
               if Contains (Table, To_Unbounded_String (Full_Name)) then
                  raise Duplicate_Option_Name with Name;

               else
                  Insert (Table,
                          Key      => To_Unbounded_String (Full_Name),
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

      Pos : constant Cursor := Find (Name_To_Option, To_Unbounded_String (Name));
   begin
      if Pos = No_Element then
         raise Unknown_Option with Name;
      end if;

      declare
         Option : constant Options := Element (Name_To_Option, Pos);
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

   procedure Parse
     (Source                  : String;
      Names                   : Option_Names;
      Result                  : out Option_Values;
      Mandatory               : Option_Flags     := All_No;
      When_Repeated           : When_Repeated_Do := Always_Die;
      Option_Value_Separator  : Character        := Default_Value_Separator;
      Include_Prefix          : Character := No_Include_Prefix;
      Option_Prefix           : String := Default_Option_Prefix;
      Concatenation_Separator : String := Default_Concatenation_Separator)
   is
      use type Ada.Containers.Count_Type;

      procedure Process_Include (Filename : Unbounded_String)
      is
      begin
         raise Program_Error with "Include not implemented";
      end Process_Include;

      type Status_Type is
        (
         Skipping_Spaces,
         Begin_Include_Filename,
         In_Include_Filename,
         In_Name,
         Begin_Of_Value,
         In_Value
        );

      Name_To_Option : Name_Tables.Map (100 * Options'Pos (Options'Last));


      Status : Status_Type := Skipping_Spaces;

      Name_Accumulator  : Unbounded_String;
      Value_Accumulator : Unbounded_String;
      Include_Filename  : Unbounded_String;

      Ignored           : String_Vectors.Vector (4096);
   begin
      Fill_Name_Table (Names      => Names,
                       Table      => Name_To_Option,
                       Prefix     => Option_Prefix,
                       Help_Lines => Ignored);

      Result := (others => (Missing => True));

      declare
         Padded_Source : constant String := Source & " ";
         Closing_Value : Character;
      begin
         --
         -- The following initializations are not really necessary since
         -- these variables are initialized when needed by the state
         -- automata implemented by for loop + case.  However, without
         -- them SPARK complains that they could be not initialized.
         --
         Closing_Value := ASCII.NUL;
         Name_Accumulator := Null_Unbounded_String;
         Value_Accumulator := Null_Unbounded_String;
         Include_Filename := Null_Unbounded_String;

         for Current_Char of Padded_Source loop
            case Status is
               when Skipping_Spaces =>
                  if Current_Char = Include_Prefix and Include_Prefix /= No_Include_Prefix then
                     Status := Begin_Include_Filename;

                  elsif Current_Char /= ' '  then
                     Name_Accumulator := Null_Unbounded_String & Current_Char;
                     Status := In_Name;

                  end if;

               when Begin_Include_Filename =>
                  Include_Filename := Null_Unbounded_String;

                  if Current_Char = ''' then
                     Closing_Value := ''';
                  else
                     Include_Filename := Include_Filename & Current_Char;
                     Closing_Value := ' ';
                  end if;

                  Status := In_Include_Filename;

               when In_Include_Filename =>
                  if Current_Char = Closing_Value then
                     Process_Include (Include_Filename);
                  else
                     Include_Filename := Include_Filename & Current_Char;
                  end if;

               when In_Name =>
                  if Current_Char = ' '  then
                     Update (What                    => Result,
                             Name                    => To_String (Name_Accumulator),
                             Value                   => "",
                             Name_To_Option          => Name_To_Option,
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
         Missing_Options : Unbounded_String := Null_Unbounded_String;
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
   end Parse;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Names                   : Option_Names;
      Result                  : out Option_Values;
      Mandatory               : Option_Flags := All_No;
      When_Repeated           : When_Repeated_Do := Always_Die;
      Option_Value_Separator  : Character        := Default_Value_Separator;
      Include_Prefix          : Character := No_Include_Prefix;
      Option_Prefix           : String := Default_Option_Prefix;
      Concatenation_Separator : String := Default_Concatenation_Separator)
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
      Parse (Source                  => Command_Line_Restored,
             Result                  => Result,
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

   --  function Option_Help_Lines return String_Vectors.Vector is
   --  begin
   --     return String_Vectors.Copy (Help_Lines);
   --  end Option_Help_Lines;

end Generic_Command_Line_Parser;
