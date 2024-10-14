package sparked_Command_Line with SPARK_Mode is
   function Argument_Count return Natural
     with
       Global => (null);

   function Argument (N : Positive) return String
     with
       Pre => N <= Argument_Count,
       Global => (null);

   function Command_Name return String
     with
       Global => (null);

end sparked_Command_Line;
