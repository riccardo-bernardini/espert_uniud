pragma Ada_2012;
with Images;
with ada.text_io;   use ada.text_io;
package body Event_Sequences is
   use type Camera_Events.X_Coordinate_Type;
   use type Camera_Events.y_Coordinate_Type;
   ---------
   -- "<" --
   ---------

   function "<" (A, B : Camera_Events.Point_Type) return Boolean is
      use Camera_Events;
   begin
      return A.X  < B.X or else (A.X = B.X and A.Y < B.Y);
   end "<";

   ----------------------
   -- Collect_By_Point --
   ----------------------
   Result : Point_Event_Map (0 .. Images.Default_X_Size - 1, 0 .. Images.Default_Y_Size - 1) :=
              (others => (others => Event_Vectors.Empty_List));

   function Collect_By_Point
     (Events         : Event_Sequence;
      Last_Timestamp : Camera_Events.Timestamp)
      return Point_Event_Map
   is
      use Camera_Events;

   begin
      Put_Line ("[12]");
      Result := (others => (others => Event_Vectors.Empty_List));
      Put_Line ("[13]");

      for Ev of Events loop
         Result (X (Ev), Y (Ev)).Append (Ev);
      end loop;

      Put_Line ("[44]");

      for X in Result'Range (1) loop
         for Y in Result'Range (2) loop
            Result (X, Y).Append (New_Event (T      => Last_Timestamp,
                                             X      => X,
                                             Y      => Y,
                                             Weight => 0));
         end loop;
      end loop;

      Put_Line ("[99]");

      return Result;
   end Collect_By_Point;

end Event_Sequences;
