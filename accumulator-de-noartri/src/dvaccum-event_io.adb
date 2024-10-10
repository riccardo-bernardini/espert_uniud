pragma Ada_2012;

with Ada.Containers;

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;

with Ada.Characters.Handling;

with Ada.Streams.Stream_IO;

with Tokenize;

use Ada;
use Ada.Strings;

package body DVAccum.Event_Io is
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
      Events                 :    out Event_Sequences.Set;
      Metadata               :    out Sequence_Metadata;
      Rectify                : in     Boolean)
   is
      use Ada.Text_Io;

      function Parse_Data_Line (Line : String)
                                return DVaccum.Events.Event_Type
        with
          Pre => Type_Of (Line) = Data;

      function Parse_Data_Line (Line : String)
                                return DVaccum.Events.Event_Type
      is
         use type Ada.Containers.Count_Type;

         use Tokenize;
         use DVaccum.Events;

         Fields : constant Token_List := Split (To_Be_Splitted => Line,
                                                Separator      => ',');

         Weight : Weight_Type;
      begin
         if Fields.Length /= 4 then
            raise Bad_Event_Stream with Line;
         end if;

         if Fields (4) = "0" then
            Weight := Decrease;

         elsif Fields (4) = "1" then
            Weight := Increase;

         else
            raise Bad_Event_Stream;
         end if;

         return New_Event (T      => DVaccum.Timestamps.Value (Fields (1)),
                           X      => X_Coordinate_Type'Value (Fields (2)),
                           Y      => Y_Coordinate_Type'Value (Fields (3)),
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
                          (Key      => Metadata_Key (Key),
                           New_Item => Metadata_Value (Value));

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
              (Key      => Metadata_Key (Key),
               New_Item => Metadata_Value (Value));
         end if;
      end Read_Metadata;

      Header_Seen        : Boolean := False;
      Previous_Timestamp : Timestamps.Timestamp := Timestamps.Minus_Infinity;
   begin
      while not End_Of_File (Input) loop
         declare
            Line : constant String := Strip_Spaces (Get_Line (Input));
         begin
            --  Put_Line (Type_Of (Line)'Image);
            case Type_Of (Line) is
               when Comment =>
                  if not Header_Seen then
                     Read_Metadata (Metadata.Map, Line);
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
                     use Dvaccum.Events;
                     use Timestamps;

                     Event : Event_Type := Parse_Data_Line (Line);
                  begin
                     if Rectify then
                        DVAccum.Events.Rectify (Event);
                     end if;

                     if not Events.Is_Empty
                       and then Previous_Timestamp > T (Event)
                     then
                        raise Bad_Event_Stream with "Non monotonic timestamps";
                     end if;

                     Previous_Timestamp := T (Event);

                     Events.Insert (Event);
                  end;
            end case;
         end;
      end loop;
   end Read_CSV_Event_Stream;

   ------------------------------
   -- Read_Binary_Event_Stream --
   ------------------------------

   procedure Read_Binary_Event_Stream
     (Filename : in     String;
      Events   :    out Event_Sequences.Set;
      Metadata :    out Sequence_Metadata;
      Rectify  : in     Boolean)
   is
      use Ada.Streams.Stream_IO;
      use Dvaccum.Events;
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
         Metadata.Map.Clear;

         for I in 1 .. N_Events loop
            declare
               Ev : Event_Type := Event_Type'Input (Input_Stream);
            begin
               if Rectify then
                  DVAccum.Events.Rectify (Ev);
               end if;

               Events.Include (Ev);
            end;
         end loop;

         for I in 1 .. N_Metadata loop
            declare
               Name : constant Metadata_Key :=
                        Metadata_Key'Input (Input_Stream);

               Value : constant Metadata_Value :=
                         Metadata_Value'Input (Input_Stream);
            begin
               Metadata.Map.Include (Key      => Name,
                                     New_Item => Value);
            end;
         end loop;
      end;

      Close (Input_File);
   end Read_Binary_Event_Stream;

   -----------------------
   -- Read_Event_Stream --
   -----------------------

   procedure Read_Events
     (Filename : in     String;
      Events   :    out Event_Sequences.Set;
      Metadata :    out Sequence_Metadata;
      Rectify  : in     Boolean)
   is
      Extension : constant String := Get_Extension (Filename);

   begin
      if Filename = "-" then
         Read_CSV_Event_Stream (Input    => Ada.Text_IO.Standard_Input,
                                Events   => Events,
                                Metadata => Metadata,
                                Rectify  => Rectify);

      elsif Extension = ".csv" or Extension = "" then
         declare
            use Ada.Text_IO;

            File : Ada.Text_IO.File_Type;
         begin
            Open (File     => File,
                  Mode     => In_File,
                  Name     => Filename);

            Read_CSV_Event_Stream (Input    => Ada.Text_IO.Standard_Input,
                                   Events   => Events,
                                   Metadata => Metadata,
                                   Rectify  => Rectify);

            Close (File);
         end;

      elsif Extension = ".evt" then
         Read_Binary_Event_Stream (Filename => Filename,
                                   Events   => Events,
                                   Metadata => Metadata,
                                   Rectify  => Rectify);

      else
         raise Bad_Event_Stream
           with "Unrecognized extension '" & Extension & "'";
      end if;
   end Read_Events;

   ----------
   -- Dump --
   ----------

   procedure Dump (What : Event_Sequences.Set;
                   Where : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Error)
   is
   begin
      for Ev of What loop
         Ada.Text_IO.Put_Line (File => Where,
                               Item => Events.Image (Ev));
      end loop;
   end Dump;

end DVAccum.Event_Io;
