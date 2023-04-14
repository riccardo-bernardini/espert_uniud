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

      for Pos in Result.Iterate loop
         Result.M (Pos.X, Pos.Y).Append (New_Event (T      => Last_Timestamp,
                                                    X      => Pos.X,
                                                    Y      => Pos.Y,
                                                    Weight => 0));
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

   function Next (C : Cursor) return Cursor
   is (if C.X < C.M.all'Last (1) then
          Cursor'(M => C.M,
                  X => C.X + 1,
                  Y => C.Y)

       elsif C.Y < C.M.all'Last (2) then
          Cursor'(M => C.M,
                  X => C.M.all'First (1),
                  Y => C.Y + 1)

       else
          Cursor'(M => C.M,
                  X => C.X,
                  Y => C.M.all'Last (2)+1));


   ------------
   -- Create --
   ------------

   function Create (X_Size : Camera_Events.X_Coordinate_Type;
                    Y_Size : Camera_Events.Y_Coordinate_Type)
                    return Point_Event_Map
   is
      use Camera_Events;

      Last_X : constant X_Coordinate_Type :=
                 X_Coordinate_Type'First + X_Size - 1;

      Last_Y : constant Y_Coordinate_Type :=
                 Y_Coordinate_Type'First + Y_Size - 1;

      M : constant Event_Matrix_Access :=
            new Event_Matrix (X_Coordinate_Type'First .. Last_X,
                              Y_Coordinate_Type'First .. Last_Y);
   begin
      pragma Assert (M.all'Length (1) = Natural (X_Size));
      pragma Assert (M.all'Length (2) = Natural (Y_Size));

      return Point_Event_Map '(Finalization.Limited_Controlled with
                                 M => M);
   end Create;

   function Events (Map   : Point_Event_Map;
                    Point : Camera_Events.Point_Type)
                    return Event_Sequence
   is (Map.M (Point.X, Point.Y));

   procedure Clear (Map : in out Point_Event_Map)
   is
   begin
      for Pos in Map.Iterate loop
         Map.M (Pos.X, Pos.Y).Clear;
      end loop;
   end Clear;

   procedure Append (Map   : in out Point_Event_Map;
                     Event : Camera_Events.Event_Type)
   is
      use Camera_Events;
   begin
      Map.M (X (Event), Y (Event)).Append (Event);
   end Append;

   function Has_Element (Pos : Cursor) return Boolean
   is (Pos.Y <= Pos.M.all'Last (2));

   function Iterate (Container : in Point_Event_Map)
                     return Point_Event_Map_Interfaces.Forward_Iterator'Class
   is (Point_Map_Iterator'(C => Cursor'(M => Container.M,
                                        X => Container.M.all'First (1),
                                        Y => Container.M.all'First (2))));

   function First (Object : Point_Map_Iterator) return Cursor
   is (Object.C);

   function Next (Object : Point_Map_Iterator; Position : Cursor) return Cursor
   is (Next (Position));


   function Point (Position : Cursor) return Camera_Events.Point_Type
   is (Camera_Events.Point_Type'(X => Position.X,
                                 Y => Position.Y));

   procedure Fill_Frame
     (Events_At : in out Point_Event_Map;
      Time      : Camera_Events.Timestamp;
      Size_X    : Camera_Events.X_Coordinate_Type;
      Size_Y    : Camera_Events.Y_Coordinate_Type)
   is
      use Camera_Events;

   begin
      for Pos in Events_At.Iterate loop
         declare
            Zero_Event : constant Event_Type := New_Event (T      => Time,
                                                           X      => Pos.X,
                                                           Y      => Pos.Y,
                                                           Weight => 0);
         begin
            if Events_At.M (Pos.X, Pos.Y).Is_Empty or else
              T (Events_At.M (Pos.X, Pos.Y).Last_Element) < Time
            then
               Events_At.Append (Zero_Event);
            end if;
         end;
      end loop;
   end Fill_Frame;



end Event_Sequences;
