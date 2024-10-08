pragma Ada_2012;
with Ada.Text_IO;

with Ada.Containers;

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants;

with Ada.Characters.Handling;

with Ada.Streams.Stream_IO;

with Tokenize;
with Camera_Events;

use Ada;
use Ada.Strings;

with Times;

package body Event_Streams is
   type Counter is mod 2 ** 64;

   type Line_Type is (Comment, Header, Data);

   function Get_Extension (Filename : String) return String
   is (if Filename'Length > 4 and then Filename (Filename'Last - 3) = '.'
       then
          Characters.Handling.To_Lower (Fixed.Tail (Filename, 4))
       else
          "");

   function Chomp (S : String) return String
   is
      use Ada.Strings.Maps.Constants;

      Last : constant Natural := Fixed.Index (Source => S,
                                              Set    => Control_Set,
                                              Test   => Outside,
                                              Going  => Backward);
   begin
      return S (S'First .. Last);
   end Chomp;

   function Strip_Spaces (S : String) return String
   is (Fixed.Trim (Source => Chomp (S),
                   Side   => Both));


   function Type_Of (Line : String) return Line_Type
   is
      use Ada.Strings.Maps.Constants;
      use type Ada.Strings.Maps.Character_Set;

      Stripped : constant String := Strip_Spaces (Line);
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

   ------------------------
   -- Read_Event_Stream --
   ------------------------

   procedure Read_CSV_Event_Stream
     (Input                  : in     Ada.Text_IO.File_Type;
      Use_Absolute_Timestamp : in     Boolean;
      Events                 :    out Event_Sequences.Event_Sequence;
      Metadata               :    out Event_Sequences.Metadata_Map;
      Negative_Event_Weight  : in     Sign)
   is
      use Ada.Text_Io;

      type Polarity_Format_Type is (Bool, Number);

      function Polarity_Format (Metadata : Event_Sequences.Metadata_Map)
                                return Polarity_Format_Type
      is
         use type Event_Sequences.Metadata_Value;

         Format : constant Event_Sequences.Metadata_Value :=
                    Metadata.Value_Of ("polarity", "boolean");
      begin
         if Format = "boolean" then
            return Bool;

         elsif Format = "float" then
            return Number;

         else
            raise Bad_Event_Stream with "Unknown polarity format '" & String (Format) & "'";
         end if;
      end Polarity_Format;

      function Read_Data_Line (Line            : String;
                               Polarity_Format : Polarity_Format_Type)
                               return Camera_Events.Event_Type
      is
         use type Ada.Containers.Count_Type;

         use Tokenize;
         use Camera_Events;

         Fields : constant Token_List := Split (To_Be_Splitted => Line,
                                                Separator      => ',');

         Weight : Weight_Type;
      begin
         if Fields.Length /= 4 then
            raise Bad_Event_Stream with Line;
         end if;

         Weight := Weight_Type'Value (Fields (4));

         if Polarity_Format = Bool and Weight = 0 then
            Weight := -1;
         end if;

         if Weight < 0 then
            Weight := Weight * Weight_Type (Negative_Event_Weight);
         end if;

         return New_Event (T      => Times.Value (Fields (1)),
                           X      => X_Coordinate_Type'Value (Fields (2)),
                           Y      => Y_Coordinate_Type'Value (Fields (3)),
                           Weight => Weight);
      end Read_Data_Line;

      procedure Read_Metadata
        (Metadata : in out Event_Sequences.Metadata_Map;
         Line     : String)
        with
          Pre => Type_Of (Line) = Comment;

      procedure Read_Metadata
        (Metadata : in out Event_Sequences.Metadata_Map;
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
         New_Metadata : Event_Sequences.Metadata_Map;

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
                        New_Metadata.Wipe_Out;
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
                        New_Metadata.Wipe_Out;
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

                        New_Metadata.Set
                          (Key   => Event_Sequences.Metadata_Name (To_String (Key)),
                           Value => Event_Sequences.Metadata_Value (To_String (Value)));

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
            New_Metadata.Set
              (Key   => Event_Sequences.Metadata_Name (To_String (Key)),
               Value => Event_Sequences.Metadata_Value (To_String (Value)));
         end if;

         Metadata.Update (New_Metadata);
      end Read_Metadata;

      Header_Seen        : Boolean := False;
      Previous_Timestamp : times.Timestamp := times.Minus_Infinity;
      Timestamp_Offset   : times.Duration;
   begin
      while not End_Of_File (Input) loop
         declare
            Line : constant String := Strip_Spaces (Get_Line (Input));
         begin
            --  Put_Line (Type_Of (Line)'Image);
            case Type_Of (Line) is
               when Comment =>
                  if not Header_Seen then
                     Read_Metadata (Metadata, Line);
                  end if;

               when Header =>
                  if Header_Seen then
                     raise Bad_Event_Stream with "Double header";
                  end if;

                  Header_Seen := True;

               when Data =>
                  if not Header_Seen then
                     raise Bad_Event_Stream with "Missing header";
                  end if;

                  declare
                     use Camera_Events;
                     use Times;

                     Event : constant Event_Type :=
                               Read_Data_Line (Line, Polarity_Format (Metadata));
                  begin
                     if Events.Is_Empty then
                        Timestamp_Offset := (if Use_Absolute_Timestamp then
                                                Times.To_Duration (0.0)
                                             else
                                                Times.To_Duration (T (Event)));

                     else
                        if Previous_Timestamp > T (Event) then
                           raise Bad_Event_Stream
                             with "Non monotonic timestamps";
                        end if;
                     end if;

                     Previous_Timestamp := T (Event);

                     Events.Append (Translate (Event, Timestamp_Offset));
                  end;
            end case;
         end;
      end loop;

      --  for Ev of Events loop
      --     Put_Line (Camera_Events.Image (Ev));
      --  end loop;
      --
      --  raise Program_Error;
      --
      --  Metadata.Dump;
   end Read_CSV_Event_Stream;

   ------------------------------
   -- Read_Binary_Event_Stream --
   ------------------------------

   procedure Read_Binary_Event_Stream
     (Filename : in     String;
      Events   :    out Event_Sequences.Event_Sequence;
      Metadata :    out Event_Sequences.Metadata_Map;
      Negative_Event_Weight : in Sign)
   is
      use Ada.Streams.Stream_IO;
      use Camera_Events;
      use Event_Sequences;

      Input_File : File_Type;
   begin
      Open (File => Input_File,
            Mode => In_File,
            Name => Filename);


      declare
         Input_Stream : constant Stream_Access := Stream (Input_File);
         N_Events     : constant Counter := Counter'Input (Input_Stream);
         N_Metadata   : constant Counter := Counter'Input (Input_Stream);
      begin
         Events.Clear;
         Metadata.Wipe_Out;

         for I in 1 .. N_Events loop
            declare
               Ev : Event_Type := Event_Type'Input (Input_Stream);
            begin
               if Weight (Ev) < 0 then
                  Multiply_Weight (Event => ev,
                                   By    => Negative_Event_Weight);
               end if;

               Events.Append (Ev);
            end;
         end loop;

         for I in 1 .. N_Metadata loop
            declare
               Name : constant Metadata_Name :=
                        Metadata_Name'Input (Input_Stream);

               Value : constant Metadata_Value :=
                         Metadata_Value'Input (Input_Stream);
            begin
               Metadata.Set (Key   => Name,
                             Value => Value);
            end;
         end loop;
      end;

      Close (Input_File);
   end Read_Binary_Event_Stream;

   -----------------------
   -- Read_Event_Stream --
   -----------------------

   procedure Read_Event_Stream
     (Filename               : in     String;
      Use_Absolute_Timestamp : in     Boolean;
      Events                 :    out Event_Sequences.Event_Sequence;
      Metadata               :    out Event_Sequences.Metadata_Map;
      Negative_Event_Weight  : in     Sign)
   is
      Extension : constant String := Get_Extension (Filename);

   begin
      if Filename = "-" then
         Read_CSV_Event_Stream (Input                  => Ada.Text_IO.Standard_Input,
                                Use_Absolute_Timestamp => Use_Absolute_Timestamp,
                                Events                 => Events,
                                Metadata               => Metadata,
                                Negative_Event_Weight  => Negative_Event_Weight);

      elsif Extension = ".csv" or Extension = "" then
         declare
            use Ada.Text_IO;

            File : Ada.Text_IO.File_Type;
         begin
            Open (File     => File,
                  Mode     => In_File,
                  Name     => Filename);

            Read_CSV_Event_Stream (Input                  => File,
                                   Use_Absolute_Timestamp => Use_Absolute_Timestamp,
                                   Events                 => Events,
                                   Metadata               => Metadata,
                                   Negative_Event_Weight  => Negative_Event_Weight);

            Close (File);
         end;

      elsif Extension = ".evt" then
         Read_Binary_Event_Stream (Filename    => Filename,
                                   Events      => Events,
                                   Metadata               => Metadata,
                                   Negative_Event_Weight  => Negative_Event_Weight);

      else
         raise Bad_Event_Stream
           with "Unrecognized extension '" & Extension & "'";
      end if;
   end Read_Event_Stream;

   ------------------------------
   -- Save_Binary_Event_Stream --
   ------------------------------

   procedure Save_Binary_Event_Stream
     (Filename : String;
      Events   : Event_Sequences.Event_Sequence;
      Metadata : Event_Sequences.Metadata_Map)
   is
      use Ada.Streams.Stream_IO;
      use Camera_Events;
      use Event_Sequences;

      Output_File : File_Type;
   begin
      Create (File => Output_File,
              Mode => Out_File,
              Name => Filename);


      declare
         Output_Stream : constant Stream_Access := Stream (Output_File);
      begin
         Counter'Output (Output_Stream, Counter (Events.Length));

         Counter'Output (Output_Stream, Counter (Metadata.N_Entries));

         for Event of Events loop
            Event_Type'Output (Output_Stream, Event);
         end loop;

         declare
            procedure Save_Metadata (Name  : Metadata_Name;
                                     Value : Metadata_Value)
            is
            begin
               Metadata_Name'Output (Output_Stream, Name);
               Metadata_Value'Output (Output_Stream, Value);
            end Save_Metadata;
         begin
            Metadata.Iterate (Save_Metadata'Access);
         end;
      end;

      Close (Output_File);
   end Save_Binary_Event_Stream;

   ---------------------------
   -- Save_CSV_Event_Stream --
   ---------------------------

   procedure Save_CSV_Event_Stream
     (Output   : Ada.Text_Io.File_Type;
      Events   : Event_Sequences.Event_Sequence;
      Metadata : Event_Sequences.Metadata_Map)
   is
      use Ada.Text_IO;
      use Camera_Events;
      use Event_Sequences;

      function Metadata_Line (Metadata : Event_Sequences.Metadata_Map)
                              return String
      is
         Result : Unbounded_String := Null_Unbounded_String;

         procedure Add_Metadata (Name   : Metadata_Name;
                                 Value  : Metadata_Value)
         is
         begin
            Result := Result & String (Name) & ": " & String (Value) & " ";
         end Add_Metadata;
      begin
         Metadata.Iterate (Add_Metadata'Access);

         return "# " & To_String (Result);
      end Metadata_Line;
   begin
      Put_Line (Output, Metadata_Line (Metadata));

      Put_Line (Output, "timestamp,x,y,polarity");

      for Event of Events loop
         Put_Line (Output, Times.Image (T (Event))
                   & ","
                   & X (Event)'Image
                   & ","
                   & Y (Event)'Image
                   & ","
                   & (if Weight (Event) > 0 then "1" else "0"));
      end loop;

   end Save_CSV_Event_Stream;

   ---------------------------
   -- Save_CSV_Event_Stream --
   ---------------------------

   procedure Save_CSV_Event_Stream
     (Filename : String;
      Events   : Event_Sequences.Event_Sequence;
      Metadata : Event_Sequences.Metadata_Map)
   is
   begin
      if Filename = "-" then
         Save_CSV_Event_Stream (Output   => Ada.Text_IO.Standard_Output,
                                Events   => Events,
                                Metadata => Metadata);

      else
         declare
            use Ada.Text_IO;

            Output : File_Type;
         begin
            Create (File => Output,
                    Mode => Out_File,
                    Name => Filename);

            Save_CSV_Event_Stream (Output   => Output,
                                   Events   => Events,
                                   Metadata => Metadata);

            Close (Output);
         end;
      end if;
   end Save_CSV_Event_Stream;

   -----------------------
   -- Save_Event_Stream --
   -----------------------

   procedure Save_Event_Stream
     (Filename : String;
      Events   : Event_Sequences.Event_Sequence;
      Metadata : Event_Sequences.Metadata_Map)
   is
      Extension : constant String := Get_Extension (Filename);
   begin
      if Extension = ".csv" or Filename = "-" then
         Save_CSV_Event_Stream (Filename => Filename,
                                Events   => Events,
                                Metadata => Metadata);

      elsif Extension = ".evt" then
         Save_Binary_Event_Stream (Filename => Filename,
                                   Events   => Events,
                                   Metadata => Metadata);
      else
         raise Constraint_Error
           with "Unrecognized extension '" & Extension & "'";
      end if;
   end Save_Event_Stream;

end Event_Streams;
