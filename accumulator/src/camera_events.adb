pragma Ada_2012;
package body Camera_Events is
   function To_Duration (T : Timestamp) return Duration
   is (Duration (T.T));

   --------------
   -- Position --
   --------------

   function Position (Event : Event_Type) return Point_Type
   is (Point_Type'(X => Event.X,
                   Y => Event.Y));


   function Translate (Event   : Event_Type;
                       Delta_T : Duration)
                       return Event_Type
   is (Event_Type'(T      => Event.T - Delta_T,
                   X      => Event.x,
                   Y      => Event.y,
                   Weight => Event.Weight));


   function Image (Event : Event_Type) return String
   is ("["
       & Event.T.T'Image & ", "
       & Event.X'Image & ", "
       & Event.y'Image & ", "
       & Event.Weight'Image
       & "]");


   function Image (T : Timestamp) return String
   is (if T = Minus_Infinity then
          "-Inf"

       elsif T = Infinity then
          "Inf"

       else
          T.T'Image);


end Camera_Events;
