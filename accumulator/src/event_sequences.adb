pragma Ada_2012;
with ada.Text_IO;
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
      map.Clear;
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
      Result         : out Point_Event_Matrix)
   is
      use Camera_Events;
   begin
      --  Put_Line ("[12]");

      for X in Result'Range (1) loop
         for Y in Result'Range (2) loop
            Result (X, Y).Clear;
         end loop;
      end loop;

      --  Put_Line ("[13]");

      for Ev of Events loop
         Result (X (Ev), Y (Ev)).Append (Ev);
      end loop;

      --  Put_Line ("[44]");

      for X in Result'Range (1) loop
         for Y in Result'Range (2) loop
            Result (X, Y).Append (New_Event (T      => Last_Timestamp,
                                             X      => X,
                                             Y      => Y,
                                             Weight => 0));
         end loop;
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
      use ada.Text_IO;
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

end Event_Sequences;
