pragma Ada_2012;
with Ada.Containers;
with Ada.Strings.Unbounded;

with Camera_Events;

with Ada.Strings.Fixed;  use Ada.Strings;

with Ada.Strings.Maps.Constants;

with Tokenize;

package body Event_Streams is
   type Line_Type is (Comment, Header, Data);


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
   -- Parse_Event_Stream --
   ------------------------

   function Parse_Event_Stream
     (Input : Ada.Text_Io.File_Type) return Event_Sequences.Event_Sequence
   is
      use Ada.Text_Io;

      function Parse_Data_Line (Line : String) return Camera_Events.Event_Type
      is
         use type Ada.Containers.Count_Type;

         use Tokenize;
         use Camera_Events;

         Fields : constant Token_List := Split (To_Be_Splitted => Line,
                                                Separator      => ',');
      begin
         if Fields.Length /= 4 then
            raise Bad_Data_Line with Line;
         end if;

         return New_Event (T      => Value (Fields (1)),
                           X      => X_Coordinate_Type'Value (Fields (2)),
                           Y      => Y_Coordinate_Type'Value (Fields (3)),
                           Weight => Weight_Type'Value (Fields (4)));
      end Parse_Data_Line;

      procedure Parse_Metadata
        (Metadata : in out Event_Sequences.Metadata_Map;
         Line     : String)
        with
          Pre => Type_Of (Line) = Comment;

      procedure Parse_Metadata
        (Metadata : in out Event_Sequences.Metadata_Map;
         Line     : String)
      is
         use Ada.Strings.Unbounded;

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

         Status : Machine_Status := Looking_For_Hash;
         New_Metadata : Event_Sequences.Metadata_Map;

         Key : Unbounded_String := Null_Unbounded_String;
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
                        -- We should never get here
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
      end Parse_Metadata;

      Result      : Event_Sequences.Event_Sequence;
      Header_Seen : Boolean := False;
      Metadata    : Event_Sequences.Metadata_Map;
   begin
      while not End_Of_File (Input) loop
         declare
            Line : constant String := Strip_Spaces (Get_Line (Input));
         begin
            --  Put_Line (Type_Of (Line)'Image);
            case Type_Of (Line) is
               when Comment =>
                  if not Header_Seen then
                     Parse_Metadata (Metadata, Line);
                  end if;

               when Header =>
                  if Header_Seen then
                     raise Bad_Data_Line with "Double header";
                  end if;

                  Header_Seen := True;

               when Data =>
                  Result.Append (Parse_Data_Line (Line));

            end case;
         end;
      end loop;

      Metadata.Dump;

      return Result;
   end Parse_Event_Stream;

end Event_Streams;
