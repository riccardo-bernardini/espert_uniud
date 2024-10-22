pragma Ada_2012;

with Ada.Containers;

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;



with Tokenize;

use Ada;
use Ada.Strings;

with Patterns;

package body DVAccum.Event_Io is


   type Line_Type is (Comment, Header, Data);


   function First_Time (Sequence : Event_Sequence)return Timestamps.Timestamp
   is (Sequence.Meta.Min_Timestamp);

   function Last_Time (Sequence : Event_Sequence)return Timestamps.Timestamp
   is (Sequence.Meta.Max_Timestamp);


   function Is_Empty (Sequence : Event_Sequence) return Boolean
   is (Sequence.Events.Is_Empty);

   function Source_File_Name (Item : Event_Sequence) return String
   is (To_String (Item.Meta.Source));


   function Type_Of (Line : String) return Line_Type
   is
      use Ada.Strings.Maps.Constants;
      use type Ada.Strings.Maps.Character_Set;

      Stripped : constant String := Patterns.Strip_Spaces (Line);
   begin
      if Stripped = "" or else Stripped (Stripped'First) = '#' then
         return Comment;
      end if;

      if 0 = Fixed.Index (Source => Line,
                          Set    => Decimal_Digit_Set or Maps.To_Set (",- "),
                          Test   => Outside)
      then
         return Data;
      end if;

      return Header;
   end Type_Of;

   procedure Set_Metadata (Sequence      : in out Event_Sequence;
                           Metadata      : in     Metadata_Maps.Map;
                           Min_Timestamp : in Timestamps.Timestamp;
                           Max_Timestamp : in Timestamps.Timestamp;
                           Filename      : in     String)
   is
      -------------
      -- Get_Int --
      -------------

      function Get_Int (Key : String) return Integer
      is
         K : constant Metadata_Key := Metadata_Key (Key);
      begin
         if not Metadata.Contains (K) then
            raise Bad_Event_Stream
              with "Missing " & Key & " metadata";
         end if;

         declare
            Val : constant Metadata_Value := Metadata (K);
         begin
            if not Patterns.Is_Integer (String (Val)) then
               raise Bad_Event_Stream
                 with "Bad integer as value of " & Key;
            end if;

            return Integer'Value (String (Val));
         end;
      end Get_Int;

      N_Rows : constant Positive := Get_Int ("sizeY");
      N_Cols : constant Positive := Get_Int ("sizeX");

   begin
      Sequence.Meta := Sequence_Metadata'
        (Min_Timestamp => Min_Timestamp,
         Max_Timestamp => Max_Timestamp,
         N_Rows        => N_Rows,
         N_Cols        => N_Cols,
         Map           => Metadata,
         Source        => To_Unbounded_String (Filename));
   end Set_Metadata;
   ------------------------
   -- Read_Event_Stream --
   ------------------------

   procedure Read_CSV_Event_Stream
     (Input                  : in     Ada.Text_IO.File_Type;
      Events                 : in out Event_Sequence;
      On_Positive_Event      : in     Event_Weight;
      On_Negative_Event      : in     Event_Weight;
      Offset                 : in     Timestamps.Duration)
   is
      use Ada.Text_Io;

      procedure Die (Msg : String)
      is
      begin
         raise Bad_Event_Stream
           with
             Msg &
             " at line " & Line (Input)'Image
           & " of file " & Name (Input);
      end Die;

      function Parse_Data_Line (Line : String)
                                return DVaccum.Events.Event_Type
        with
          Pre => Type_Of (Line) = Data;

      function Parse_Data_Line (Line : String)
                                return DVaccum.Events.Event_Type
      is
         use type Ada.Containers.Count_Type;

         use Tokenize;
         use Timestamps;

         Fields : constant Token_List := Split (To_Be_Splitted => Line,
                                                Separator      => ',');

         Weight : Event_Weight;
      begin
         if Fields.Length /= 4 then
            Die ("Wrong # of fields in event line: " & Line);
         end if;

         if Fields (4) = "0" then
            Weight := On_Negative_Event;

         elsif Fields (4) = "1" then
            Weight := On_Positive_Event;

         else
            Die ("Bad event sign on line " & Line);
         end if;

         return New_Event (T      => Value (Fields (1))+Offset,
                           X      => Coord_X'Value (Fields (2)),
                           Y      => Coord_Y'Value (Fields (3)),
                           Weight => Weight);
      end Parse_Data_Line;

      procedure Read_Metadata
        (Metadata : in out Metadata_Maps.Map;
         Line     : String)
        with
          Pre => Type_Of (Line) = Comment;

      procedure Read_Metadata
        (Metadata : in out Metadata_Maps.Map;
         Line     : String)
      is
         type Machine_Status is
           (
            Looking_For_Hash,
            Looking_For_Key,
            In_Key,
            Looking_For_Colon,
            Looking_For_Value,
            In_Value,
            Error
           );

         subtype Valid_Status is
           Machine_Status range Looking_For_Hash .. In_Value;

         Is_Final : constant array (Machine_Status) of Boolean :=
                      (
                       Looking_For_Key  => True,
                       In_Value         => True,
                       others           => False
                      );

         Status       : Machine_Status := Looking_For_Hash;

         Key   : Unbounded_String := Null_Unbounded_String;
         Value : Unbounded_String := Null_Unbounded_String;
      begin
         for Current_Char of Line loop
            pragma Assert (Status in Valid_Status);

            case Valid_Status (Status) is
               when Looking_For_Hash =>
                  case Current_Char is
                     when '#' =>
                        Status := Looking_For_Key;

                     when ' ' =>
                        null;

                     when others =>
                        --
                        --  We should never get here since the procedure
                        --  is called for comment lines
                        --
                        raise Program_Error;
                  end case;



               when Looking_For_Key =>
                  case Current_Char is
                     when ' ' =>
                        null;

                     when ':'  =>
                        Status := Error;

                     when others =>
                        Key := To_Unbounded_String ("" & Current_Char);
                        Status := In_Key;
                  end case;

               when In_Key =>
                  case Current_Char is
                     when ' ' =>
                        Status := Looking_For_Colon;

                     when ':'  =>
                        Status := Looking_For_Value;

                     when others =>
                        Key := Key & Current_Char;
                  end case;

               when Looking_For_Colon =>
                  case Current_Char is
                     when ' ' =>
                        null;

                     when ':'  =>
                        Status := Looking_For_Value;

                     when others =>
                        Status := Error;

                  end case;

               when Looking_For_Value =>
                  case Current_Char is
                     when ' ' =>
                        null;

                     when others =>
                        Value := To_Unbounded_String ("" & Current_Char);
                        Status := In_Value;
                  end case;

               when In_Value =>
                  case Current_Char is
                     when ' ' =>
                        Status := Looking_For_Key;

                        Metadata.Insert
                          (Key      => Metadata_Key (To_String (Key)),
                           New_Item => Metadata_Value (To_String (Value)));

                        Key := Null_Unbounded_String;
                        Value := Null_Unbounded_String;

                     when others =>
                        Value := Value & Current_Char;

                  end case;
            end case;

            exit when Status = Error;
         end loop;

         if not Is_Final (Status) then
            return;
         end if;

         if Value /= Null_Unbounded_String then
            Metadata.Insert
              (Key      => Metadata_Key (To_String (Key)),
               New_Item => Metadata_Value (To_String (Value)));
         end if;
      end Read_Metadata;

      Header_Seen        : Boolean := False;
      Previous_Timestamp : Timestamps.Timestamp := Timestamps.Minus_Infinity;
      Min_Timestamp      : Timestamps.Timestamp := Timestamps.Infinity;
      Metadata           : Metadata_Maps.Map;
   begin
      while not End_Of_File (Input) loop
         declare
            Line : constant String := Patterns.Strip_Spaces (Get_Line (Input));
         begin
            --  Put_Line (Type_Of (Line)'Image);
            case Type_Of (Line) is
               when Comment =>
                  if not Header_Seen then
                     Read_Metadata (Metadata, Line);
                  end if;

               when Header =>
                  if Header_Seen then
                     Die ("Double header");
                  end if;

                  Header_Seen := True;

               when Data =>
                  if not Header_Seen then
                     Die ("Missing header");
                  end if;

                  declare
                     use Timestamps;

                     Event : constant Event_Type := Parse_Data_Line (Line);
                  begin
                     if Events.Is_Empty then
                        Min_Timestamp := T (Event);
                     else
                        if  Previous_Timestamp > T (Event) then
                           Die ("Non monotonic timestamps");
                        end if;
                     end if;

                     Events.Events.Insert (Event);

                     Previous_Timestamp := T (Event);
                  end;
            end case;
         end;
      end loop;

      Set_Metadata (Sequence      => Events,
                    Metadata      => Metadata,
                    Min_Timestamp => Min_Timestamp,
                    Max_Timestamp => Previous_Timestamp,
                    Filename      => Ada.Text_IO.Name (Input));
   end Read_CSV_Event_Stream;

   ------------------------------
   -- Read_Binary_Event_Stream --
   ------------------------------

   --  procedure Read_Binary_Event_Stream
   --    (Filename          : in     String;
   --     Events            :    out Event_Sequences.Set;
   --     Metadata          :    out Sequence_Metadata;
   --     On_Positive_Event : in     Event_Weight;
   --     On_Negative_Event : in     Event_Weight;
   --     Offset            : in     Timestamps.Duration)
   --  is
   --     pragma Unreferenced (On_Positive_Event, On_Negative_Event, Offset);
   --     use Ada.Streams.Stream_IO;
   --     use Event_Sequences;
   --
   --     Input_File : File_Type;
   --  begin
   --     pragma Compile_Time_Warning (True, "Outdated.  Do not use");
   --     raise Program_Error;
   --
   --     Open (File => Input_File,
   --           Mode => In_File,
   --           Name => Filename);
   --
   --
   --     declare
   --        Input_Stream : constant Stream_Access := Stream (Input_File);
   --        N_Events     : constant Counter := Counter'Input (Input_Stream);
   --        N_Metadata   : constant Counter := Counter'Input (Input_Stream);
   --     begin
   --        Events.Clear;
   --        Metadata.Map.Clear;
   --
   --        for I in 1 .. N_Events loop
   --           declare
   --              Ev : constant Event_Type := Event_Type'Input (Input_Stream);
   --           begin
   --              Events.Include (Ev);
   --           end;
   --        end loop;
   --
   --        for I in 1 .. N_Metadata loop
   --           declare
   --              Name : constant Metadata_Key :=
   --                       Metadata_Key'Input (Input_Stream);
   --
   --              Value : constant Metadata_Value :=
   --                        Metadata_Value'Input (Input_Stream);
   --           begin
   --              Metadata.Map.Include (Key      => Name,
   --                                    New_Item => Value);
   --           end;
   --        end loop;
   --     end;
   --
   --     Close (Input_File);
   --  end Read_Binary_Event_Stream;
   --  pragma Unreferenced (Read_Binary_Event_Stream);

   -----------------------
   -- Read_Event_Stream --
   -----------------------

   --  procedure Read_Events
   --    (Filename          : in     String;
   --     Events            :    out Event_Sequences.Set;
   --     Metadata          :    out Sequence_Metadata;
   --     On_Positive_Event : in     Event_Weight;
   --     On_Negative_Event : in     Event_Weight;
   --     Offset            : in     Timestamps.Duration)
   --  is
   --     Extension : constant String := Get_Extension (Filename);
   --
   --  begin
   --     if Filename = "-" then
   --        Read_CSV_Event_Stream (Input             => Ada.Text_IO.Standard_Input,
   --                               Events            => Events,
   --                               On_Negative_Event => On_Negative_Event,
   --                               On_Positive_Event => On_Positive_Event,
   --                               Offset            => Offset);
   --
   --     elsif Extension = ".csv" or Extension = "" then
   --        declare
   --           use Ada.Text_IO;
   --
   --           File : Ada.Text_IO.File_Type;
   --        begin
   --           Open (File     => File,
   --                 Mode     => In_File,
   --                 Name     => Filename);
   --
   --           Read_CSV_Event_Stream (Input             => File,
   --                                  Events            => Events,
   --                                  Metadata          => Metadata,
   --                                  On_Negative_Event => On_Negative_Event,
   --                                  On_Positive_Event => On_Positive_Event,
   --                                  Offset            => Offset);
   --
   --           Close (File);
   --        end;
   --
   --     elsif Extension = ".evt" then
   --        raise Program_Error
   --          with "Binary event files unimplemented";
   --
   --        Read_Binary_Event_Stream (Filename          => Filename,
   --                                  Events            => Events,
   --                                  Metadata          => Metadata,
   --                                  On_Negative_Event => On_Negative_Event,
   --                                  On_Positive_Event => On_Positive_Event,
   --                                  Offset            => Offset);
   --
   --     else
   --        raise Bad_Event_Stream
   --          with "Unrecognized extension '" & Extension & "'";
   --     end if;
   --  end Read_Events;

   ----------
   -- Dump --
   ----------

   procedure Dump (What  : Event_Sequence;
                   Where : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Error)
   is
   begin
      for Ev of What.Events loop
         Ada.Text_IO.Put_Line (File => Where,
                               Item => Events.Image (Ev));
      end loop;
   end Dump;


   function First (Item : Event_Iterator) return Event_Cursor
   is (Item.First);

   ----------
   -- Next --
   ----------

   function Next (Item   : Event_Iterator;
                  Cursor : Event_Cursor)
                  return Event_Cursor
   is
   begin
      return Result : Event_Cursor := Cursor do
         Event_Sequences.Next (Result.Cursor);
      end return;
   end Next;

   function All_Events
     (Item : Event_Sequence)
      return Event_Sequence_Iterators.Forward_Iterator'Class
   is (Event_Iterator'(First => (Cursor => Item.Events.First)));

end DVAccum.Event_Io;
