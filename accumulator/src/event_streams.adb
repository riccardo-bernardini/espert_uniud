pragma Ada_2012;
with Ada.Containers;

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
      use type ada.Strings.Maps.Character_Set;

      Stripped : constant String := Strip_Spaces (Line);
   begin
      if Stripped = "" or else Stripped (Stripped'First) = '#' then
         return Comment;
      end if;

      if 0 = fixed.Index (Source => Line,
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
                           Y      => y_Coordinate_Type'Value (Fields (3)),
                           Weight => Weight_Type'Value (Fields (4)));
      end Parse_Data_Line;

      Result : Event_Sequences.Event_Sequence;
   begin
      while not End_Of_File (Input) loop
         declare
            Line : constant String := Strip_Spaces (Get_Line (Input));
         begin
            --  Put_Line (Type_Of (Line)'Image);
            case Type_Of (Line) is
               when Comment =>
                  null;

               when Header =>
                  null;

               when Data =>
                  Result.Append (Parse_Data_Line (Line));

            end case;
         end;
      end loop;

      return Result;
   end Parse_Event_Stream;

end Event_Streams;
