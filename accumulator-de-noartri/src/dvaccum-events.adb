pragma Ada_2012;
package body Dvaccum.Events is
   function Less_Then_By_Pixel (A, B : Event_Type) return Boolean
   is (A.X < B.X or
         (A.X = B.X and A.Y < B.Y) or
         (A.X = B.X and A.Y = B.Y and A.T < B.T));

   --------------
   -- Position --
   --------------

   function Position (Event : Event_Type) return Point_Type
   is (Point_Type'(X => Event.X,
                   Y => Event.Y));


   function Translate (Event   : Event_Type;
                       Delta_T : Timestamps.Duration)
                       return Event_Type
   is (Event_Type'(T      => Event.T - Delta_T,
                   X      => Event.X,
                   Y      => Event.Y,
                   Weight => Event.Weight));


   ---------------
   -- Translate --
   ---------------

   procedure Translate (Event   : in out Event_Type;
                        Delta_T : Timestamps.Duration)
   is
   begin
      Event.T := Event.T - Delta_T;
   end Translate;


   function Image (Event : Event_Type) return String
   is ("["
       & Image (Event.T) & ", "
       & Event.X'Image & ", "
       & Event.Y'Image & ", "
       & Event.Weight'Image
       & "]");

   procedure Rectify (Item : in out Event_Type)
   is
   begin
      Item.Weight := Increase;
   end Rectify;

end Dvaccum.events;
