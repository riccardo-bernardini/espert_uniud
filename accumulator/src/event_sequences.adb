pragma Ada_2012;
with Images;

package body Event_Sequences is

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

   function Collect_By_Point
     (Events         : Event_Sequence;
      Last_Timestamp : Camera_Events.Timestamp)
      return Point_Event_Map
   is
      use Camera_Events;

      Result : Point_Event_Map
        (0 .. Images.Default_X_Size - 1, 0 .. Images.Default_Y_Size - 1) :=
                 (others => (others => Event_Vectors.Empty_Vector));
   begin
      for Ev of Events loop
         Result (X (Ev), Y (Ev)).Append (Ev);
      end loop;

      for X in Result'Range (1) loop
         for Y in Result'Range (2) loop
            Result (X, Y).Append (New_Event (T      => Last_Timestamp,
                                             X      => X,
                                             Y      => Y,
                                             Weight => 0));
         end loop;
      end loop;

      return Result;
   end Collect_By_Point;

end Event_Sequences;
