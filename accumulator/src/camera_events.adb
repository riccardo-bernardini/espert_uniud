pragma Ada_2012;
package body Camera_Events is
   --------------
   -- Position --
   --------------

   function Position (Event : Event_Type) return Point_Type
   is (Point_Type'(X => Event.X,
                   Y => Event.Y));


   function Translate (Event   : Event_Type;
                       Delta_T : Times.Duration)
                       return Event_Type
   is (Event_Type'(T      => Event.T - Delta_T,
                   X      => Event.X,
                   Y      => Event.Y,
                   Weight => Event.Weight));


   function Image (Event : Event_Type) return String
   is ("["
       & Image (Event.T) & ", "
       & Event.X'Image & ", "
       & Event.Y'Image & ", "
       & Event.Weight'Image
       & "]");

   procedure Multiply_Weight (Event : in out Event_Type;
                              By    : Integer)
   is
   begin
      Event.Weight := Event.Weight * Weight_Type (By);
   end Multiply_Weight;



end Camera_Events;
