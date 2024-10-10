with DVAccum.Timestamps;
use  DVAccum.Timestamps;

package DVAccum.Events is

   type X_Coordinate_Type is mod 2 ** 16;
   type Y_Coordinate_Type is mod 2 ** 16;

   type Point_Type is
      record
         X : X_Coordinate_Type;
         Y : Y_Coordinate_Type;
      end record;


   type Weight_Type is (Positive, Negative);

   type Event_Type is private;

   function New_Event (T      : Timestamp;
                       X      : X_Coordinate_Type;
                       Y      : Y_Coordinate_Type;
                       Weight : Weight_Type)
                       return Event_Type;

   function Translate (Event   : Event_Type;
                       Delta_T : Timestamps.Duration)
                       return Event_Type
     with
       Pre => T (Event) >= Zero + Delta_T,
       Post => T (Translate'Result) = T (Event)-Delta_T;

   procedure Multiply_Weight (Event : in out Event_Type;
                              By    : Integer);

   function T (Event : Event_Type) return Timestamp;
   function X (Event : Event_Type) return X_Coordinate_Type;
   function Y (Event : Event_Type) return Y_Coordinate_Type;

   function Position (Event : Event_Type) return Point_Type;

   function Weight (Event : Event_Type) return Weight_Type;

   function Image (Event : Event_Type) return String;

   function Less_Then_By_Pixel (X, Y : Event_Type) return Boolean;
private

   type Event_Type is
      record
         T      : Timestamp;
         X      : X_Coordinate_Type;
         Y      : Y_Coordinate_Type;
         Weight : Weight_Type;
      end record;

   function New_Event (T      : Timestamp;
                       X      : X_Coordinate_Type;
                       Y      : Y_Coordinate_Type;
                       Weight : Weight_Type)
                       return Event_Type
   is (Event_Type'(T      => T,
                   X      => X,
                   Y      => Y,
                   Weight => Weight));

   function T (Event : Event_Type) return Timestamp
   is (Event.T);

   function X (Event : Event_Type) return X_Coordinate_Type
   is (Event.X);

   function Y (Event : Event_Type) return Y_Coordinate_Type
   is (Event.Y);

   function Weight (Event : Event_Type) return Weight_Type
   is (Event.Weight);


end DVAccum.Events;
