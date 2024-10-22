with Ada.Characters.Handling;

--  with Ada.Text_IO; use Ada.Text_IO;
with Dvaccum.Time_Syntax;
with Ada.Strings.Equal_Case_Insensitive;

package body DVAccum.Timestamps is
   use type Time_Syntax.Time_In_Microseconds;

   function To_Duration (T : Timestamp) return Duration
   is (Duration (T.T));

   function To_Duration (V : Time_Syntax.Time_In_Microseconds) return Duration
   is (Duration (V));

   function To_Timestamp (V : Time_Syntax.Time_In_Microseconds) return Timestamp
   is (T => Timestamp_Value (V), Infinite => False, Relative => False);

   function To_Relative_Timestamp (V : Time_Syntax.Time_In_Microseconds) return Timestamp
   is (T => Timestamp_Value (V), Infinite => False, Relative => True);
   pragma Unreferenced (To_Relative_Timestamp);

   function To_Seconds (X : Duration) return Float
   is (Float (X) / Timestamps_Per_Second);

   ------------------------
   -- Is_Valid_Timestamp --
   ------------------------

   function Is_Valid_Timestamp (Spec : String) return Boolean
   is
      use Ada.Characters.Handling;

      Success  : Boolean;
      Value    : Time_Syntax.Time_In_Microseconds;
   begin
      if To_Lower (Spec) = "inf" or To_Lower (Spec) = "-inf" then
         return True;
      end if;

      Time_Syntax.Parse_Time (Input    => Spec,
                              Success  => Success,
                              Value    => Value);

      return Success;
   end Is_Valid_Timestamp;

   -----------------------
   -- Is_Valid_Duration --
   -----------------------

   function Is_Valid_Duration (Spec : String) return Boolean
   is
      Success  : Boolean;
      Value    : Time_Syntax.Time_In_Microseconds;
   begin
      --  Put_Line ("valid duration """ & Spec & """");

      Time_Syntax.Parse_Time (Input    => Spec,
                              Success  => Success,
                              Value    => Value);

      --  Put_Line ("Success  : " & (if Success then "Yes" else "No"));
      --
      --  Put_Line ("Relative : " & (if Relative then "Yes" else "No"));
      --  Put_Line ("Value    : " & Value'Image);

      return Success;
   end Is_Valid_Duration;

   -----------
   -- Value --
   -----------

   function Value (S : String) return Duration
   is
      use Time_Syntax;

      Success  : Boolean;
      Value    : Time_In_Microseconds;
   begin
      Parse_Time (Input    => S,
                  Success  => Success,
                  Value    => Value);

      if not Success then
         raise Constraint_Error;
      end if;

      return To_Duration (Value);
   end Value;

   ---------------------
   -- Parse_Timestamp --
   ---------------------

   function Value (S : String) return Timestamp
   is
      use Time_Syntax;

      Success  : Boolean;
      Value    : Time_In_Microseconds;
   begin
      if Ada.Strings.Equal_Case_Insensitive (S, "inf") then
         return Infinity;

      elsif Ada.Strings.Equal_Case_Insensitive (s, "-inf") then
         return Minus_Infinity;

      end if;

      Parse_Time (Input    => S,
                  Success  => Success,
                  Value    => Value);

      if not Success then
         raise Constraint_Error;
      end if;

      return To_Timestamp (Value);
   end Value;





   function Max (X, Y : Timestamp) return Timestamp
   is (if X > Y then X else Y);

   function Min (X, Y : Timestamp) return Timestamp
   is (if X < Y then X else Y) ;

   function Image (T         : Timestamp;
                   With_Unit : Boolean := False) return String
   is (if T = Minus_Infinity then
          "-Inf"

       elsif T = Infinity then
          "Inf"

       else
          T.T'Image & (if With_Unit then "us" else ""));

   function Is_Relative (T : Timestamp) return Boolean
   is (T.Relative);

   function To_Relative_Timestamp (Seconds : Float) return Timestamp
   is (T        => Timestamp_Value (Seconds),
       Infinite => False,
       Relative => True);

   function Fix_T0 (T : Timestamp; T0 : Timestamp) return Timestamp
   is (T        => T.T + T0.T,
       Infinite => False,
       Relative => False);


   function ">" (X, Y : Duration) return Boolean
   is (Duration_Value (X) > Duration_Value (Y));

   function ">=" (X, Y : Duration) return Boolean
   is (not (X < Y));

   function "<" (X, Y : Duration) return Boolean
   is (Y > X);

   function "<=" (X, Y : Duration) return Boolean
   is (not (X > Y));

   function Max (X, Y : Duration) return Duration
   is (if X > Y then X else Y);

   function Min (X, Y : Duration) return Duration
   is (if X < Y then X else Y);
end DVAccum.Timestamps;
