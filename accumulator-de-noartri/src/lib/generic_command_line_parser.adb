pragma Ada_2012;
with Ada.Characters.Handling;
with Sparked_Command_Line;
-- with Ada.Text_IO; use Ada.Text_IO;

--  with Ada.Containers.Indefinite_Ordered_Maps;
with Simple_Tables;
with Ada.Strings.Fixed;


package body Generic_Command_Line_Parser is

   function Option (Name : String) return Option_Spec
   is
      procedure Parse_Option_Names (Source        : String;
                                    Names         : out String_Vectors.Vector;
                                    Help_Line     : out Unbounded_String;
                                    Default       : out Unbounded_String;
                                    Default_Found : out Boolean)
        with
          Pre => Source'Length < Natural'Last;

      procedure Parse_Option_Names (Source        : String;
                                    Names         : out String_Vectors.Vector;
                                    Help_Line     : out Unbounded_String;
                                    Default       : out Unbounded_String;
                                    Default_Found : out Boolean)
      is
         use String_Vectors;

         Cursor : Positive := Source'First;

         End_Of_Source :  constant Character := Character'Val (0);

         function At_End_Of_Source return Boolean
         is (Cursor > Source'Last);

         function Current_Char return Character
         is (if At_End_Of_Source then
                End_Of_Source
             else
                Source (Cursor));

         procedure Next_Char is
         begin
            Cursor := Cursor + 1;
         end Next_Char;

         function Current_Tail return String
         is (Source (Cursor .. Source'Last));

         Accumulator : Unbounded_String;

         procedure Reset_Accumulator is
         begin
            Accumulator := Null_Unbounded_String;
         end Reset_Accumulator;

         procedure Reset_Accumulator (C : Character) is
         begin
            Accumulator := To_Unbounded_String ((1 => C));
         end Reset_Accumulator;

         procedure Append (C : Character) is
         begin
            Accumulator := Accumulator & C;
         end Append;


         type Parser_Status is (
                                Reading_Name,
                                Name_Separator,
                                Skipping_Space_After_Name,
                                Skipping_Space_After_Separator,
                                In_Default,
                                Skipping_Space_After_Default,
                                Documentation
                               );

         Status : Parser_Status := Reading_Name;
      begin
         Clear (Names);

         Help_Line := Null_Unbounded_String;
         Status := Reading_Name;
         Default_Found := False;
         Default := Null_Unbounded_String;

         Reset_Accumulator;

         while Status /= Documentation and not At_End_Of_Source loop
            case Status is
               when Reading_Name =>
                  if Current_Char = '|' then
                     Status := Name_Separator;
                     Names.Append (To_String (Accumulator));
                     Reset_Accumulator;

                  elsif Current_Char = ' ' then
                     Status := Skipping_Space_After_Name;
                     Names.Append (To_String (Accumulator));
                     Reset_Accumulator;

                  elsif Current_Char = '[' then
                     Status := In_Default;
                     Names.Append (To_String (Accumulator));
                     Reset_Accumulator;

                  else
                     Append (Current_Char);
                  end if;

               when Name_Separator =>
                  if Current_Char = '|' or Current_Char = '[' then
                     raise Bad_Option_Name with Source;

                  elsif Current_Char = ' ' then
                     Status := Skipping_Space_After_Separator;

                  else
                     Reset_Accumulator (Current_Char);
                     Status := Reading_Name;

                  end if;

               when Skipping_Space_After_Separator =>
                  if Current_Char = '|' or Current_Char = '[' then
                     raise Bad_Option_Name with Source;

                  elsif Current_Char = ' ' then
                     Status := Skipping_Space_After_Separator;

                  else
                     Status := Reading_Name;
                     Reset_Accumulator (Current_Char);

                  end if;

               when Skipping_Space_After_Name =>
                  if Current_Char = '|' then
                     Status := Name_Separator;

                  elsif Current_Char = ' ' then
                     Status := Skipping_Space_After_Name;

                  elsif Current_Char = '[' then
                     Status := In_Default;
                     Reset_Accumulator;

                  else
                     Status := Documentation;

                  end if;

               when In_Default =>
                  if Current_Char = ']' then
                     Status := Skipping_Space_After_Default;
                     Default_Found := True;
                     Default := Accumulator;

                  else
                     Append (Current_Char);

                  end if;

               when Skipping_Space_After_Default =>
                  if Current_Char /= ' ' then
                     Status := Documentation;
                  end if;

               when Documentation =>
                  raise Program_Error;  -- we should never arrive here
            end case;

            Next_Char;
         end loop;

         case Status is
            when Reading_Name =>
               Names.Append (To_String (Accumulator));

            when Skipping_Space_After_Name | Skipping_Space_After_Default =>
               null;

            when In_Default | Name_Separator | Skipping_Space_After_Separator =>
               raise Bad_Option_Name with Source;

            when Documentation =>
               Help_Line := To_Unbounded_String (Current_Tail);
         end case;
      end Parse_Option_Names;

      Names : String_Vectors.Vector;
      Help_Line : Unbounded_String;
      Default : Unbounded_String;
      Default_Found : Boolean;
   begin
      Parse_Option_Names (Source        => Name,
                          Names         => Names,
                          Help_Line     => Help_Line,
                          Default       => Default,
                          Default_Found => Default_Found);

      if Default_Found then
         return Option_Spec'(On_Missing    => Use_Default,
                             On_Repetition => Die,
                             Names         => Names,
                             Doc           => Help_Line,
                             Default       => Default);

      else
         return Option_Spec'(On_Missing    => Ignore,
                             On_Repetition => Die,
                             Names         => Names,
                             Doc           => Help_Line,
                             Default       => Null_Unbounded_String);
      end if;
   end Option;

   ---------------
   -- Mandatory --
   ---------------

   function Mandatory return Option_Modifier
   is (Option_Modifier'(Class => Make_Mandatory));

   -----------------
   -- If_Repeated --
   -----------------

   function If_Repeated (Action : Repeated_Option_Action)
                         return Option_Modifier
   is (Option_Modifier'(Class => On_Repeat,
                        Repeat_Action => Action));

   function "and" (Left : Option_Spec; Right : Option_Modifier)
                   return Option_Spec
   is

   begin
      case Right.Class is
         when Make_Mandatory =>
            if Left.On_Missing = Use_Default then
               raise Constraint_Error;
            end if;

            return Option_Spec'(On_Missing    => Die,
                                On_Repetition => Left.On_Repetition,
                                Names         => Left.Names,
                                Doc           => Left.Doc,
                                Default       => Left.Default);

         when On_Repeat =>
            return Option_Spec'(On_Missing    => Left.On_Missing,
                                On_Repetition => Right.Repeat_Action,
                                Names         => Left.Names,
                                Doc           => Left.Doc,
                                Default       => Left.Default);
      end case;
   end "and";




   package Name_Tables is
     new Simple_Tables (Key_Type     => Unbounded_String,
                        Element_Type => Options);


   procedure Fill_Name_Table (Syntax              : CLI_Syntax;
                              Prefix              : String;
                              Table               : in out Name_Tables.Map;
                              Name_Case_Sensitive : Boolean);



   procedure Fill_Name_Table (Syntax              : CLI_Syntax;
                              Prefix              : String;
                              Table               : in out Name_Tables.Map;
                              Name_Case_Sensitive : Boolean)
   is
      use Name_Tables;
      use Ada.Characters.Handling;
   begin

      for Option in Options loop
         for Name of Syntax (Option).Names loop
            declare
               function Normalize (X : String) return String
               is (if Name_Case_Sensitive then  X else To_Lower (X));

               Full_Name : constant String := Prefix & Normalize (Name);
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

   procedure Update (What                    : in out Option_Value_Array;
                     Name                    : String;
                     Value                   : String;
                     Name_To_Option          : Name_Tables.Map;
                     Syntax                  : CLI_Syntax;
                     Concatenation_Separator : String;
                     Name_Case_Sensitive     : Boolean)
   is
      use Name_Tables;
      use Ada.Characters.Handling;


      function Normalize (X : String) return String
      is (if Name_Case_Sensitive then  X else To_Lower (X));


      Pos : constant Cursor := Find (Name_To_Option,
                                     To_Unbounded_String (Normalize (Name)));
   begin
      if Pos = No_Element then
         raise Unknown_Option with Name;
      end if;

      declare
         Option : constant Options := Element (Name_To_Option, Pos);
      begin
         if What (Option).Status /= User_Defined then
            What (Option) := (Status => User_Defined,
                              Value  => To_Unbounded_String (Value));

            return;
         end if;

         pragma Assert (What (Option).Status = User_Defined);

         case Syntax (Option).On_Repetition is
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

   function Parse_Arguments
     (Source                  : String_Vectors.Vector;
      Syntax                  : CLI_Syntax;
      Option_Value_Separator  : String := Default_Value_Separator;
      Include_Prefix          : String := No_Include_Prefix;
      Option_Prefix           : String := Default_Option_Prefix;
      Concatenation_Separator : String := Default_Concatenation_Separator;
      Name_Case_Sensitive     : Boolean := False)
      return Parsed_CL
   is
      use Ada.Strings;

      procedure Process_Include (Filename : String)
      is
      begin
         raise Program_Error with "Include not implemented";
      end Process_Include;

      procedure Process_Option (Input    : String;
                                Result   : in out Option_Value_Array;
                                Name_Map : Name_Tables.Map)
      is
         Separator_Pos : constant Natural :=
                           Fixed.Index (Source  => Input,
                                        Pattern => Option_Value_Separator);
      begin
         if Separator_Pos = 0 then
            Update (What                    => Result,
                    Name                    => Input,
                    Value                   => "",
                    Name_To_Option          => Name_Map,
                    Syntax                  => Syntax,
                    Concatenation_Separator => Concatenation_Separator,
                    Name_Case_Sensitive     => Name_Case_Sensitive);

         elsif Separator_Pos = Input'First then
            raise Constraint_Error;

         else
            Update (What                    => Result,
                    Name                    => Input (Input'First .. Separator_Pos - 1),
                    Value                   => Input (Separator_Pos + 1 .. Input'Last),
                    Name_To_Option          => Name_Map,
                    Syntax                  => Syntax,
                    Concatenation_Separator => Concatenation_Separator,
                    Name_Case_Sensitive     => Name_Case_Sensitive);

         end if;
      end Process_Option;

      function Begin_With (What : String; Prefix : String) return Boolean
      is (What'Length >= Prefix'Length and then
          Fixed.Head (What, Prefix'Length) = Prefix);


      Name_To_Option : Name_Tables.Map :=
                         Name_Tables.New_Map (100 * Options'Pos (Options'Last));


      No_Value          : constant Option_Value :=
                            (Status => Undefined,
                             Value  => Null_Unbounded_String);

      No_More_Options : Boolean;
   begin
      return Result : Parsed_CL :=
        Parsed_CL'(Arguments => String_Vectors.Empty_Vector,
                   Missing   => Option_Lists.Empty_List,
                   Options   => (others => No_Value))
      do

         Fill_Name_Table (Syntax              => Syntax,
                          Table               => Name_To_Option,
                          Prefix              => Option_Prefix,
                          Name_Case_Sensitive => Name_Case_Sensitive);

         for Opt in Options loop
            if Syntax (Opt).On_Missing = Use_Default then
               Result.Options (Opt) :=  (Status => Default,
                                         Value  => Syntax (Opt).Default);
            end if;
         end loop;


         No_More_Options := False;

         for Word of Source loop
            if No_More_Options then
               Result.Arguments.Append (Word);

            elsif Word = Option_Prefix then
               No_More_Options := True;

            elsif Begin_With (Word, Option_Prefix) then
               Process_Option (Input    => Word,
                               Result   => Result.Options,
                               Name_Map => Name_To_Option);

            elsif Begin_With (Word, Include_Prefix) then
               Process_Include (Word);

            else
               Result.Arguments.Append (Word);
            end if;
         end loop;


         for Opt in Options loop
            if not Result.Is_Defined (Opt) and Syntax (Opt).On_Missing = Die then
               Result.Missing.Append (Opt);
            end if;
         end loop;

      end return;
   end Parse_Arguments;

   -----------
   -- Parse --
   -----------

   function Parse_CL
     (Syntax                  : CLI_Syntax;
      Option_Value_Separator  : String := Default_Value_Separator;
      Include_Prefix          : String := No_Include_Prefix;
      Option_Prefix           : String := Default_Option_Prefix;
      Concatenation_Separator : String := Default_Concatenation_Separator;
      Name_Case_Sensitive     : Boolean := False)
      return Parsed_CL
   is
      function Command_Line_Restored return String_Vectors.Vector
      is
         use Sparked_Command_Line;

         Result : String_Vectors.Vector;
      begin
         for I in 1 .. Argument_Count loop
            Result.Append (Argument (I));
         end loop;


         return Result;
      end Command_Line_Restored;
   begin
      return Parse_Arguments
        (Source                  => Command_Line_Restored,
         Syntax                  => Syntax,
         Option_Value_Separator  => Option_Value_Separator,
         Include_Prefix          => Include_Prefix,
         Option_Prefix           => Option_Prefix,
         Concatenation_Separator => Concatenation_Separator,
         Name_Case_Sensitive     => Name_Case_Sensitive);
   end Parse_CL;



   ----------
   -- Join --
   ----------

   function Join (Item      : String_Vectors.Vector;
                  Separator : String)
                  return Unbounded_String
   is
      use String_Vectors;

      Result  : Unbounded_String := Null_Unbounded_String;
   begin
      for I in First_Index (Item) .. Last_Index (Item) loop
         if  Result /= Null_Unbounded_String then
            Result := Result & Separator;
         end if;

         Result := Result & Element (Item, I);
      end loop;

      return Result;
   end Join;

   ----------------
   -- Help_Lines --
   ----------------

   function Help_Lines (Syntax : CLI_Syntax)
                        return String_Vectors.Vector
   is
   begin
      return Result  : String_Vectors.Vector do
         for Descr of Syntax loop
            if Descr.Doc /= Null_Unbounded_String then
               Result.Append (To_String (Descr.Doc));
            end if;
         end loop;
      end return;
   end Help_Lines;


   end Generic_Command_Line_Parser;
