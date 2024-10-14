pragma Ada_2012;
with Ada.Command_Line;

package body sparked_Command_Line is

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count return Natural
   is (ada.Command_Line.Argument_Count);

   --------------
   -- Argument --
   --------------

   function Argument (N : Positive) return String
   is (Ada.Command_Line.Argument(N));

   ------------------
   -- Command_Name --
   ------------------

   function Command_Name return String
   is (Ada.Command_Line.Command_Name);

end Sparked_Command_Line; ;
