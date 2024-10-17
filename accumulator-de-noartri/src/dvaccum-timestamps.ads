package DVAccum.Timestamps is
   type Timestamp is private;

   Minus_Infinity : constant Timestamp;
   Infinity       : constant Timestamp;

   Zero : constant Timestamp;

   function Is_Finite (T : Timestamp) return Boolean;

   Timestamps_Per_Second : constant Float;

   function Is_Relative (T : Timestamp) return Boolean;

   function To_Timestamp (Seconds : Float) return Timestamp
     with
       Post => not Is_Relative (To_Timestamp'Result);

   function To_Relative_Timestamp (Seconds : Float) return Timestamp
     with
       Post => Is_Relative (To_Relative_Timestamp'Result);

   function Fix_T0 (T : Timestamp; T0 : Timestamp) return Timestamp
     with
       Pre => Is_Relative (T) and not Is_Relative (T0) and Is_Finite (T0),
     Post => not Is_Relative (Fix_T0'Result);

   function Is_Valid_Timestamp (Spec : String) return Boolean;

   function Value (S : String) return Timestamp
     with
       Pre => Is_Valid_Timestamp (S);

   function Image (T         : Timestamp;
                   With_Unit : Boolean := False) return String;

   type Duration is private;

   function Is_Valid_Duration (Spec : String) return Boolean;

   function Value (S : String) return Duration
     with
       Pre => Is_Valid_Duration (S);

   function Image (X         : Duration;
                   With_Unit : Boolean := False) return String;


   function To_Duration (Seconds : Float) return Duration;

   function Duration_In_Microsec (Microseconds : Integer) return Duration;

   function To_Duration (T : Timestamp) return Duration
     with
       Pre => Is_Finite (T),
       Post => T = Zero + To_Duration'Result;

   function "/" (X, Y : Duration) return Float;

   function "/" (X : Duration; Y : Float) return Duration;

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
private
   type Duration is range -2 ** 63 .. 2 ** 63 - 1;
   type Timestamp_Value is mod 2 ** 64;

   type Timestamp is
      record
         T        : Timestamp_Value := 0;
         Infinite : Boolean := False;
         Relative : Boolean := False;
      end record
     with
       Dynamic_Predicate => (if Infinite then T = 1 or T = -1);

   Minus_Infinity : constant Timestamp := (T => -1, Infinite => True, Relative => False);
   Infinity       : constant Timestamp := (T => 1, Infinite => True, Relative => False);

   Zero : constant Timestamp := (T => 0, Infinite => False, Relative => False);

   Timestamps_Per_Second : constant Float := 1.0e6;

   function Is_Finite (T : Timestamp) return Boolean
   is (not T.Infinite);

   function "+" (T : Timestamp; D : Duration) return Timestamp
   is (if Is_Finite (T) then
         (T        => T.T + Timestamp_Value (D),
          Infinite => False,
          Relative => False)
       else
          T);

   function "-" (T : Timestamp; D : Duration) return Timestamp
   is (if Is_Finite (T) then
         (T        => T.T - Timestamp_Value (D),
          Infinite => False,
          Relative => False)
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

   function "/" (X, Y : Duration) return Float
   is (Float (X) / Float (Y));

   function "/" (X : Duration; Y : Float) return Duration
   is (Duration (Float (X) / Y));

   function Image (X         : Duration;
                   With_Unit : Boolean := False)
                   return String
   is (Duration'Image (X) & (if With_Unit then "us" else ""));



   function To_Duration (Seconds : Float) return Duration
   is (Duration (Seconds * Timestamps_Per_Second));

   function Duration_In_Microsec (Microseconds : Integer) return Duration
   is (Duration (Microseconds));


   function To_Timestamp (Seconds : Float) return Timestamp
   is ((T       => Timestamp_Value (Seconds * Timestamps_Per_Second),
        Infinite => False,
        Relative => False));

end DVAccum.Timestamps;
