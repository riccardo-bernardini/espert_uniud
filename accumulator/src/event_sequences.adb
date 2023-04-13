pragma Ada_2012;
with Ada.Text_IO;
--  with ada.text_io;   use ada.text_io;

package body Event_Sequences is
   use type Camera_Events.X_Coordinate_Type;
   use type Camera_Events.Y_Coordinate_Type;

   --------------
   -- Wipe_Out --
   --------------

   procedure Wipe_Out (Map : in out Metadata_Map)
   is
   begin
      Map.Clear;
   end Wipe_Out;

   ---------
   -- "<" --
   ---------

   function "<" (A, B : Camera_Events.Point_Type) return Boolean is
      use Camera_Events;
   begin
      return A.X  < B.X or else (A.X = B.X and A.Y < B.Y);
   end "<";

   procedure Collect_By_Point
     (Events         : Event_Sequence;
      Last_Timestamp : Camera_Events.Timestamp;
      Result         : out Point_Event_Map)
   is
      use Camera_Events;
   begin
      Result.Clear;

      for Ev of Events loop
         Result.Append (Ev);
      end loop;

      for Pos in Result.M.Iterate loop
         declare
            P : constant Camera_Events.Point_Type := Point_Maps.Key (Pos);
         begin
            Result (P).Append (New_Event (T      => Last_Timestamp,
                                          X      => P.X,
                                          Y      => P.Y,
                                          Weight => 0));
         end;
      end loop;
   end Collect_By_Point;

   --------------
   -- Value_Of --
   --------------

   function Value_Of (Map : Metadata_Map;
                      Key : Metadata_Name) return Metadata_Value
   is
   begin
      if not Map.Has_Key (Key) then
         raise Constraint_Error with "Unknown key '" & String (Key) & "'";

      else
         return Map.Element (Key);
      end if;
   end Value_Of;


   procedure Dump (Map : Metadata_Map) is
      use Metadata_Maps;
      use Ada.Text_IO;
   begin
      for Pos in Map.Iterate loop
         Put (Standard_Error, "[" & String (Key (Pos)) & "]");
         Put (Standard_Error, "[" & String (Element (Pos)) & "]");
         New_Line (Standard_Error);
      end loop;
   end Dump;

   --------------
   -- Value_Of --
   --------------

   function Value_Of (Map     : Metadata_Map;
                      Key     : Metadata_Name;
                      Default : Metadata_Value)
                      return Metadata_Value
   is
   begin
      if not Map.Has_Key (Key) then
         return Default;
      else
         return Map.Element (Key);
      end if;
   end Value_Of;

   ---------
   -- Set --
   ---------

   procedure Set (Map   : in out Metadata_Map;
                  Key   : Metadata_Name;
                  Value : Metadata_Value)
   is
   begin
      Map.Include (Key, Value);
   end Set;

   ------------
   -- Update --
   ------------

   procedure Update (Map : in out Metadata_Map;
                     Src : in     Metadata_Map)
   is
      use Metadata_Maps;
   begin
      for Pos in Src.Iterate loop
         Map.Set (Key (Pos), Element (Pos));
      end loop;
   end Update;

   -------------------
   -- Integer_Value --
   -------------------

   function Integer_Value (Map : Metadata_Map;
                           Key : Metadata_Name)
                           return Natural
   is
   begin
      if not Map.Has_Key (Key) then
         raise Constraint_Error
           with "Missing key '" & String (Key) & "'";
      end if;

      if not (for all C of Map.Value_Of (Key) => C in '0' .. '9') then
         raise Constraint_Error
           with "'" & String (Map.Value_Of (Key)) & "' is not a valid integer";
      end if;

      return Natural'Value (String (Map.Value_Of (Key)));
   end Integer_Value;

   ------------
   -- Size_X --
   ------------

   function Size_X (Map : Metadata_Map) return Camera_Events.X_Coordinate_Type
   is (Camera_Events.X_Coordinate_Type (Integer_Value (Map, "sizeX")));

   ------------
   -- Size_Y --
   ------------

   function Size_Y (Map : Metadata_Map) return Camera_Events.Y_Coordinate_Type
   is (Camera_Events.Y_Coordinate_Type (Integer_Value (Map, "sizeY")));

   function Events (Map   : Point_Event_Map;
                    Point : Camera_Events.Point_Type)
                    return Event_Sequence
   is (if Map.M.Contains (Point) then
          Map.M (Point)

       else
          raise Constraint_Error);

   procedure Clear (Map : in out Point_Event_Map)
   is
   begin
      Map.M.Clear;
   end Clear;
   procedure Append (Map   : in out Point_Event_Map;
                     Event : Camera_Events.Event_Type)
   is
      P : constant Camera_Events.Point_Type := (X => Camera_Events.X (Event),
                                                Y => Camera_Events.Y (Event));
   begin
      if not Map.M.Contains (P) then
         Map.M.Insert (Key      => P,
                       New_Item => Event_Vectors.Empty_List);
      end if;

      Map.M (P).Append (Event);
   end Append;

   function Has_Element (Pos : Cursor) return Boolean
   is (Point_Maps.Has_Element (Pos.C));

   function Iterate (Container : in Point_Event_Map)
                     return Point_event_map_Interfaces.Forward_Iterator'Class
   is (Point_Map_Iterator'(C => Container.M.First));

   function First (Object : Point_Map_Iterator) return Cursor
   is ((C => Object.C));

   function Next (Object : Point_Map_Iterator; Position : Cursor) return Cursor
   is ((C => Point_Maps.Next (Position.C)));


   function Point (Position : Cursor) return Camera_Events.Point_Type
   is (Point_Maps.Key (Position.C));

end Event_Sequences;
