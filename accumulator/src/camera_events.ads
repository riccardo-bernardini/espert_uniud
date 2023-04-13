package Camera_Events is
   type Duration is private;
   type Timestamp is private;

   Minus_Infinity : constant Timestamp;
   Infinity       : constant Timestamp;

   Timestamps_Per_Second : constant Float;

   function Value (S : String) return Duration;

   function Image (X : Duration) return String;

   function To_Duration (Seconds : Float) return Duration;

   function "/" (X, Y : Duration) return Float;

   function Value (S : String) return Timestamp;

   function To_Timestamp (Seconds : Float) return Timestamp;

   function "+" (T : Timestamp; D : Duration) return Timestamp;

   function "-" (A, B : Timestamp) return Duration;

   function ">" (X, Y : Timestamp) return Boolean;

   function ">=" (X, Y : Timestamp) return Boolean;

   function "<" (X, Y : Timestamp) return Boolean;

   type X_Coordinate_Type is mod 2 ** 16;
   type Y_Coordinate_Type is mod 2 ** 16;

   type Point_Type is
      record
         X : X_Coordinate_Type;
         Y : Y_Coordinate_Type;
      end record;


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

   function Position (Event : Event_Type) return Point_Type;

   function Weight (Event : Event_Type) return Weight_Type;
private
   type Duration is mod 2 ** 64;
   type Timestamp_Value is mod 2 ** 64;

   type Timestamp is
      record
         T        : Timestamp_Value;
         Infinite : Boolean;
      end record;

   Minus_Infinity : constant Timestamp := (T => -1, Infinite => True);
   Infinity       : constant Timestamp := (T => 1, Infinite => True);

   function Is_Finite (T : Timestamp) return Boolean
   is (not T.Infinite);

   function "+" (T : Timestamp; D : Duration) return Timestamp
   is (if Is_Finite (T) then
         (T        => T.T + Timestamp_Value (D),
          Infinite => False)
       else
          T);

   function "-" (A, B : Timestamp) return Duration
   is (if Is_Finite (A) and Is_Finite (B) then
          Duration (A.T)-Duration (B.T)
       else
          raise Constraint_Error);

   function ">" (X, Y : Timestamp) return Boolean
   is (if X = Minus_Infinity or Y = Infinity then
          False
       elsif X = Infinity or Y = Minus_Infinity then
          True
       else
          Duration (X.T) > Duration (Y.T));

   function "<" (X, Y : Timestamp) return Boolean
   is (Y > X);

   function ">=" (X, Y : Timestamp) return Boolean
   is (not (X < Y));

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

   function "/" (X, Y : Duration) return Float
   is (Float (X) / Float (Y));

   function Value (S : String) return Duration
   is (Duration'Value (S));

   function Image (X : Duration) return String
   is (Duration'Image (X));

   function Value (S : String) return Timestamp
   is ((T       => Timestamp_Value'Value (S),
        Infinite => False));

   Timestamps_Per_Second : constant Float := 1.0e6;

   function To_Duration (Seconds : Float) return Duration
   is (Duration (Seconds * Timestamps_Per_Second));

   function To_Timestamp (Seconds : Float) return Timestamp
   is ((T       => Timestamp_Value (Seconds * Timestamps_Per_Second),
        Infinite => False));


end Camera_Events;
