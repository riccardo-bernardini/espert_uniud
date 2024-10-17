with DVAccum.Timestamps;
use  DVAccum.Timestamps;

package DVAccum.Events is
   type Event_Weight is new Float;

   type Event_Type is tagged private;

   function New_Event (T      : Timestamp;
                       X      : Coord_X;
                       Y      : Coord_Y;
                       Weight : Event_Weight)
                       return Event_Type;

   procedure Translate (Event   : in out Event_Type;
                        Delta_T : Timestamps.Duration);

   function Translate (Event   : Event_Type;
                       Delta_T : Timestamps.Duration)
                       return Event_Type
     with
       Pre => T (Event) >= Zero + Delta_T,
       Post => T (Translate'Result) = T (Event)-Delta_T;


   function T (Event : Event_Type) return Timestamp;
   function X (Event : Event_Type) return Coord_X;
   function Y (Event : Event_Type) return Coord_Y;

   function Position (Event : Event_Type) return Point_Type;

   function Weight (Event : Event_Type) return Event_Weight;

   function Image (Event : Event_Type) return String;

   function Less_Then_By_Pixel (A, B : Event_Type) return Boolean;
private

   type Event_Type is
     tagged
      record
         T      : Timestamp;
         X      : Coord_X;
         Y      : Coord_Y;
         Weight : Event_Weight;
      end record;

   function New_Event (T      : Timestamp;
                       X      : Coord_X;
                       Y      : Coord_Y;
                       Weight : Event_Weight)
                       return Event_Type
   is (Event_Type'(T      => T,
                   X      => X,
                   Y      => Y,
                   Weight => Weight));

   function T (Event : Event_Type) return Timestamp
   is (Event.T);

   function X (Event : Event_Type) return Coord_X
   is (Event.X);

   function Y (Event : Event_Type) return Coord_Y
   is (Event.Y);

   function Weight (Event : Event_Type) return Event_Weight
   is (Event.Weight);


end DVAccum.Events;
