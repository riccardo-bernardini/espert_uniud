
package DVAccum.Time_Syntax is
   type Time_In_Microseconds is range -2 ** 63 .. 2 ** 63 - 1;

   Infinity : constant Time_In_Microseconds := Time_In_Microseconds'Last;

   Minus_Infinity : constant Time_In_Microseconds := Time_In_Microseconds'First;

   function Is_Valid_Time (Spec : String) return Boolean;

   procedure Parse_Time  (Input    : String;
                          Success  : out Boolean;
                          Value    : out Time_In_Microseconds);
end DVAccum.Time_Syntax;
