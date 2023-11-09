package Camera_Events is
   type Timestamp is private;

   Minus_Infinity : constant Timestamp;
   Infinity       : constant Timestamp;

   T0 : constant Timestamp;

   function Is_Finite (T : Timestamp) return Boolean;

   Timestamps_Per_Second : constant Float;

   function To_Timestamp (Seconds : Float) return Timestamp;

   function Value (S : String) return Timestamp;

   function Image (T         : Timestamp;
                   With_Unit : Boolean := False) return String;

   type Duration is private;

   function Value (S : String) return Duration;

   function Image (X         : Duration;
                   With_Unit : Boolean := False) return String;


   function To_Duration (Seconds : Float) return Duration;

   function To_Duration (T : Timestamp) return Duration
     with
       Pre => Is_Finite (T),
       Post => T = T0 + To_Duration'Result;

   function "/" (X, Y : Duration) return Float;

   function "+" (T : Timestamp; D : Duration) return Timestamp
     with
       Post => (if not Is_Finite (T) then "+"'Result = T);

   function "-" (T : Timestamp; D : Duration) return Timestamp
     with
       Post => (if not Is_Finite (T) then "-"'Result = T);

   function "-" (A, B : Timestamp) return Duration
     with
       Pre => Is_Finite (A) and Is_Finite (B) and A >= B;

   function ">" (X, Y : Timestamp) return Boolean;

   function ">=" (X, Y : Timestamp) return Boolean;

   function "<" (X, Y : Timestamp) return Boolean;

   function "<=" (X, Y : Timestamp) return Boolean;

   function Max (X, Y : Timestamp) return Timestamp;

   function Min (X, Y : Timestamp) return Timestamp;

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

   function Translate (Event   : Event_Type;
                       Delta_T : Duration)
                       return Event_Type
     with
       Pre => T (Event) >= T0 + Delta_T,
       Post => T (Translate'Result) = T (Event)-Delta_T;

   function T (Event : Event_Type) return Timestamp;
   function X (Event : Event_Type) return X_Coordinate_Type;
   function Y (Event : Event_Type) return Y_Coordinate_Type;

   function Position (Event : Event_Type) return Point_Type;

   function Weight (Event : Event_Type) return Weight_Type;

   function Image (Event : Event_Type) return String;
private
   type Duration is mod 2 ** 64;
   type Timestamp_Value is mod 2 ** 64;

   type Timestamp is
      record
         T        : Timestamp_Value := 0;
         Infinite : Boolean := False;
      end record
     with
       Dynamic_Predicate => (if Infinite then T = 1 or T = -1);

   Minus_Infinity : constant Timestamp := (T => -1, Infinite => True);
   Infinity       : constant Timestamp := (T => 1, Infinite => True);

   T0 : constant Timestamp := (T => 0, Infinite => False);

   function Is_Finite (T : Timestamp) return Boolean
   is (not T.Infinite);

   function "+" (T : Timestamp; D : Duration) return Timestamp
   is (if Is_Finite (T) then
         (T        => T.T + Timestamp_Value (D),
          Infinite => False)
       else
          T);

   function "-" (T : Timestamp; D : Duration) return Timestamp
   is (if Is_Finite (T) then
         (T        => T.T - Timestamp_Value (D),
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

   function "<=" (X, Y : Timestamp) return Boolean
   is (not (X > Y));

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

   function Image (X : Duration;
                   With_Unit : Boolean := False)
                   return String
   is (Duration'Image (X) & (if With_Unit then "us" else ""));

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
