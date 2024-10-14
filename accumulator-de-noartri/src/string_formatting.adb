pragma Ada_2012;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants;

package body String_Formatting is

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
                       (if Accepted_Directives'Length = 0 then
                                          Strings.Maps.Constants.Special_Set
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

               else
                  Append_Text (Buffer);

                  Buffer := Null_Unbounded_String & Current_Char;
                  Status := In_Prec;
               end if;

            when In_Prec =>
               if Characters.Handling.Is_Letter (Current_Char) then
                  Append_Directive (Current_Char, Buffer);

                  Status := In_Text;

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
      Directive_Prefix    : Character := '%') return String
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
     (Format : Parsed_Format; Provider : Provider_Function) return String
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
     (Input : String; Size : out Positive; Prec : out Natural)
   is
      Dot_Position : Natural := 0;
      Dot_Seen     : Boolean := False;
   begin
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
                 with "Bad charaters in precision spec '" & Input & "'";
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
