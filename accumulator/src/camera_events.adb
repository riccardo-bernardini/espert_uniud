pragma Ada_2012;
package body Camera_Events is

   --------------
   -- Position --
   --------------

   function Position (Event : Event_Type) return Point_Type
   is (Point_Type'(X => Event.X,
                   Y => Event.Y));


end Camera_Events;
