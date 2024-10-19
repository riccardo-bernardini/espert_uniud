pragma Ada_2012;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;

with Ada.Numerics.Elementary_Functions;


package body String_Formatting is

   procedure Dump (Item : Parsed_Format;
                   To   : Text_Io.File_Type := Text_IO.Standard_Output)
   is
      use Ada.Text_Io;

      procedure Dump (Item : Format_Segment)
      is
      begin
         case Item.Class is
            when Text =>
               Put_Line (File => To,
                         Item => """" & Item.Value & """");

            when Directive =>
               Put_Line (File => To,
                         Item => "(" & Item.Label & "," & Item.Parameter & ")");

         end case;
      end Dump;
   begin
      for Segment of Item.Segments loop
         Dump (Segment);
      end loop;
   end Dump;

   function Space_Needed (Datum            : Natural;
                          Basis            : Base_Type)
                          return Positive
   is
      use Ada.Numerics.Elementary_Functions;

      Result : Positive;
   begin
      if Datum = 0 then
         return 1;
      end if;

      Result := Positive (Float'Ceiling
                          (Log (X    => Float (abs Datum + 1),
                                Base => Float (Basis))));

      return Result;
   end Space_Needed;

   ------------------------
   -- C_Style_Formatting --
   ------------------------

   function C_Style_Formatting (Datum     : Integer;
                                Parameter : String;
                                Basis     : Base_Type := 10)
                                return String
   is
      type C_Flags is
        (Zero_Padding, Left_Adjust, Always_Sign);

      package C_Flag_Parsing is
        new Flag_Parsing (C_Flags);

      Names : constant C_Flag_Parsing.Flag_Names :=
                (Zero_Padding => '0',
                 Left_Adjust  => '-',
                 Always_Sign  => '+');

      function To_String (Datum            : Natural;
                          Base             : Base_Type)
                          return String
      is
         D : constant String (1 .. 16) := "0123456789ABCDEF";
      begin
         if Datum = 0 then
            return "0";
         end if;

         return Result : String (1 .. Space_Needed (Datum, Base)) := (others => '#')
         do
            declare
               Buffer : Natural := abs Datum;
               Cursor : Natural := Result'Last;
            begin
               while Buffer > 0 loop
                  Result (Cursor) := D (Buffer mod Base + 1);
                  Buffer := Buffer / Base;
                  Cursor := Cursor - 1;
               end loop;
            end;
         end return;

      end To_String;

      Present    : C_Flag_Parsing.Flag_Array;
      Field_Size : Natural;
      Ignored    : Natural := 0;
   begin


      if Parameter = "" then
         Present := (others => False);
         Field_Size := 0;

      else
         declare
            Prec : constant String :=
                     C_Flag_Parsing.Parse_Flags
                       (Input          => Parameter,
                        Names          => Names,
                        Present        => Present);
         begin
            Parse_Precision
              (Input => Prec,
               Size  => Field_Size,
               Prec  => Ignored);
         end;
      end if;

      declare
         use Ada.Strings.Fixed;

         Sign : constant String := (if Datum < 0 then
                                       "-"
                                    elsif Present (Always_Sign) then
                                       "+"
                                    else
                                       "");

         Core : constant String := To_String (Datum, Basis);

         Tot_Length : constant Positive := Core'Length + Sign'Length;

         Padding : constant Integer := Field_Size - Tot_Length;
      begin
         if Padding <= 0 then
            return Sign & Core;

         else
            if Present (Left_Adjust) then
               return Sign & Core & (Padding * ' ');

            elsif Present (Zero_Padding) then
               return Sign & (Padding * '0') & Core;

            else
               return (Padding * ' ') & Sign & Core;
            end if;
         end if;
      end;
   end C_Style_Formatting;


   ------------------
   -- Parse_Format --
   ------------------

   function Parse_Format
     (Format              : String;
      Accepted_Directives : String := "";
      Directive_Prefix    : Character := '%') return Parsed_Format
   is
      Result : Parsed_Format :=
                 Parsed_Format'(Segments => Segment_Lists.Empty_List);

      First_Unread : Positive := Format'First;

      End_Of_Input : constant Character := Character'Val (0);

      Accepted_Set : constant Strings.Maps.Character_Set :=
                       (if Accepted_Directives'Length = 0
                        then
                           Strings.Maps.Constants.Letter_Set
                        else
                           Strings.Maps.To_Set (Accepted_Directives));

      function Current_Char return Character
      is
      begin
         if First_Unread > Format'Last then
            return End_Of_Input;
         else
            return Format (First_Unread);
         end if;
      end Current_Char;

      procedure Next_Char is
      begin
         First_Unread := First_Unread + 1;
      end Next_Char;

      -----------------
      -- Append_Text --
      -----------------

      procedure Append_Text (Value : Unbounded_String) is
      begin
         Result.Segments.Append
           (Format_Segment'(Class     => Text,
                            Size      => Length (Value),
                            Value     => To_String (Value)));
      end Append_Text;

      ----------------------
      -- Append_Directive --
      ----------------------

      procedure Append_Directive (Label : Character;
                                  Param : Unbounded_String)
      is
      begin
         if not Strings.Maps.Is_In (Label, Accepted_Set) then
            raise Parsing_Error
              with "Unknown directive '" & Label & "'";
         end if;

         Result.Segments.Append
           (Format_Segment'(Class     => Directive,
                            Size      => Length (Param),
                            Parameter => To_String (Param),
                            Label     => Label));
      end Append_Directive;

      type Parsing_Status is (In_Text, Prefix_Seen, In_Prec);

      Status : Parsing_Status := In_Text;
      Buffer : Unbounded_String := Null_Unbounded_String;
   begin
      while Current_Char /= End_Of_Input loop
         case Status is
            when In_Text =>
               if Current_Char = Directive_Prefix then
                  Status := Prefix_Seen;
               else
                  Buffer := Buffer & Current_Char;
               end if;

            when Prefix_Seen =>
               if Current_Char = Directive_Prefix then
                  Status := In_Text;
                  Buffer := Buffer & Directive_Prefix;

               elsif Characters.Handling.Is_Letter (Current_Char) then
                  Append_Text (Buffer);

                  Append_Directive (Current_Char, Null_Unbounded_String);

                  Status := In_Text;

                  Buffer := Null_Unbounded_String;

               else
                  Append_Text (Buffer);

                  Buffer := Null_Unbounded_String & Current_Char;
                  Status := In_Prec;
               end if;

            when In_Prec =>
               if Characters.Handling.Is_Letter (Current_Char) then
                  Append_Directive (Current_Char, Buffer);

                  Status := In_Text;
                  Buffer := Null_Unbounded_String;
               else
                  Buffer := Buffer & Current_Char;
               end if;

         end case;

         Next_Char;
      end loop;

      case Status is
         when In_Text =>
            Append_Text (Buffer);

         when Prefix_Seen =>
            Append_Text (Buffer & Directive_Prefix);

         when In_Prec =>
            raise Parsing_Error with "Unclosed directive";

      end case;

      return Result;
   end Parse_Format;

   ------------
   -- Expand --
   ------------

   function Expand
     (Format              : String;
      Provider            : Provider_Function;
      Accepted_Directives : String := "";
      Directive_Prefix    : Character := '%')
      return String
   is
   begin
      return Expand (Parse_Format (Format, Accepted_Directives, Directive_Prefix),
                     Callback_Based_Provider'(Callback => Provider));
   end Expand;

   ------------
   -- Expand --
   ------------

   function Expand
     (Format              : String;
      Provider            : Provider_Interface'Class;
      Accepted_Directives : String := "";
      Directive_Prefix    : Character := '%') return String
   is
   begin
      return Expand (Parse_Format (Format, Accepted_Directives, Directive_Prefix),
                     Provider);
   end Expand;

   ------------
   -- Expand --
   ------------

   function Expand
     (Format   : Parsed_Format;
      Provider : Provider_Function)
      return String
   is
   begin
      return Expand (Format,
                     Callback_Based_Provider'(Callback => Provider));
   end Expand;

   ------------
   -- Expand --
   ------------

   function Expand
     (Format : Parsed_Format; Provider : Provider_Interface'Class)
      return String
   is
      Result                : Unbounded_String := Null_Unbounded_String;
      Provider_Call_Counter : Positive := 1;
   begin
      for Segment of Format.Segments loop
         case Segment.Class is
            when Text =>
               Result := Result & Segment.Value;

            when Directive =>
               Result := Result &
                 Provider.Provide (Directive => Segment.Label,
                                   Parameter => Segment.Parameter,
                                   Counter   => Provider_Call_Counter);

               Provider_Call_Counter := Provider_Call_Counter + 1;
         end case;
      end loop;

      return To_String (Result);
   end Expand;

   ---------------------
   -- Parse_Precision --
   ---------------------

   procedure Parse_Precision
     (Input : String;
      Size  : out Natural;
      Prec  : out Natural)
   is
      Dot_Position : Natural := 0;
      Dot_Seen     : Boolean := False;
   begin
      if Input = "" then
         Size := 0;
         Prec := 0;
         return;
      end if;

      for I in Input'Range loop
         case Input (I) is
            when '0' .. '9' =>
               null;

            when '.' =>
               if Dot_Seen then
                  raise Parsing_Error
                    with "Bad precision spec: double dot";

               elsif I = Input'First then
                  raise Parsing_Error
                    with "Bad precision spec: empty size";

               else
                  Dot_Seen := True;
                  Dot_Position := I;
               end if;

            when others => raise Parsing_Error
                 with "Bad characters in precision spec '" & Input & "'";
         end case;
      end loop;

      if not Dot_Seen or else Dot_Position = Input'Last then
         Size := Positive'Value (Input);
         Prec := 0;

      else
         declare
            S : constant Natural :=
                  Positive'Value (Input (Input'First .. Dot_Position - 1));
         begin
            if S = 0 then
               raise Parsing_Error with "Size field = 0";

            else
               Size := S;

            end if;
         end;

         Prec := Natural'Value (Input (Dot_Position + 1 .. Input'Last));
      end if;
   end Parse_Precision;

   package body Flag_Parsing is
      function Parse_Flags (Input          : String;
                            Names          : Flag_Names;
                            Present        : out Flag_Array)
                            return String
      is
         procedure Find_Flag (C     : Character;
                              Found : out Boolean;
                              Flag  : out Flags)
           with
             Post => (not Found) or else (Names (Flag) = C);

         ---------------
         -- Find_Flag --
         ---------------

         procedure Find_Flag (C     : Character;
                              Found : out Boolean;
                              Flag  : out Flags)
         is
         begin
            for F in Names'Range loop
               if Names (F) = C then
                  Found := True;
                  Flag := F;
                  return;
               end if;
            end loop;

            Found := False;
            Flag := Flags'First;
         end Find_Flag;

      begin
         Present := (others => False);

         for I in Input'Range loop
            declare
               Flag  : Flags;
               Found : Boolean;
            begin
               Find_Flag (C     => Input (I),
                          Found => Found,
                          Flag  => Flag);

               if Found then
                  Present (Flag) := True;
               else
                  return Input (I .. Input'Last);
               end if;
            end;
         end loop;

         return "";
      end Parse_Flags;
   end Flag_Parsing;

   -------------
   -- Provide --
   -------------

   function Provide
     (Provider  : Callback_Based_Provider; Directive : Character;
      Parameter : String; Counter : Positive) return String
   is
   begin
      return Provider.Callback (Directive, Parameter, Counter);
   end Provide;

end String_Formatting;
