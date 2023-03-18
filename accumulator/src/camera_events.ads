package Camera_Events is
   type Duration is private;
   type Timestamp is private;

   function Value (S : String) return Duration;

   function To_Duration (Seconds : Float) return Duration;

   function Value (S : String) return Timestamp;

   function "+" (T : Timestamp; D : Duration) return Timestamp;

   function "<" (X, Y : Timestamp) return Boolean;

   type X_Coordinate_Type is mod 2 ** 16;
   type Y_Coordinate_Type is mod 2 ** 16;

   type Weight_Type is range -2 ** 31 .. 2 ** 31 - 1;

   type Event_Type is private;

   function New_Event (T      : Timestamp;
                       X      : X_Coordinate_Type;
                       Y      : Y_Coordinate_Type;
                       Weight : Weight_Type)
                       return Event_Type;

   function T (Event : Event_Type) return Timestamp;
   function X (Event : Event_Type) return X_Coordinate_Type;
   function Y (Event : Event_Type) return Y_Coordinate_Type;
   function Weight (Event : Event_Type) return Weight_Type;
private
   type Duration is mod 2 ** 64;
   type Timestamp is mod 2 ** 64;


   function "+" (T : Timestamp; D : Duration) return Timestamp
   is (T + Timestamp (D));

   function "<" (X, Y : Timestamp) return Boolean
   is (Duration (X) < Duration (Y));

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

end Camera_Events;
