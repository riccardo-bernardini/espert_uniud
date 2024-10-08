
package DVAccum.Time_Syntax is
   type Time_In_Microseconds is range -2 ** 63 .. 2 ** 63 - 1;

   function Is_Valid_Time (Spec : String) return Boolean;

   procedure Parse_Time  (Input    : String;
                          Success  : out Boolean;
                          Relative : out Boolean;
                          Value    : out Time_In_Microseconds);
end DVAccum.Time_Syntax;
