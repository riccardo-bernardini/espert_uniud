pragma Ada_2012;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;

use Ada.Strings.Maps;
use Ada.Strings.Maps.Constants;

package body Dvaccum.Split_Filename is

   Sign : constant Character_Set := To_Set ("-+");

   No_Split : constant Natural := 0;

   function Split_Point (Filename : String) return Natural
     with
       Post =>
         Split_Point'Result = 0
         or (Split_Point'Result < Filename'Last
             and Split_Point'Result > Filename'First
             and Is_In (Filename (Split_Point'Result), Sign)
             and (for all I in Split_Point'Result + 1 .. Filename'Last =>
                          Is_In (Filename (I), Decimal_Digit_Set)));


   -----------------
   -- Split_Point --
   -----------------

   function Split_Point (Filename : String) return Natural
   is
      use Ada.Strings.Fixed;
      use Ada.Strings;

      Pos : constant Natural := Index (Source => Filename,
                                       Set    => Decimal_Digit_Set,
                                       Test   => Outside,
                                       Going  => Backward);
   begin
      if Pos = 0 -- No character different from digit found
        or Pos = Filename'Last  -- Filename ends with a non-digit
        or Pos = Filename'First -- there is no room for the true filename
      then
         return No_Split;

      elsif not Is_In (Filename (Pos), Sign) then
         -- The non-digit character is not a sign
         return No_Split;

      else
         -- Everything OK, return the index of the sign
         return Pos;
      end if;




   end Split_Point;

   ---------------
   -- Offest_Of --
   ---------------

   function Offest_Of (Filename : String) return Timestamps.Duration
   is
      use Timestamps;

      Split : constant Natural := Split_Point (Filename);
   begin
      if Split = No_Split then
         return Duration_In_microsec (0);
      else
         return Duration_In_Microsec
           (Integer'Value (Filename (Split .. Filename'Last)));
      end if;
   end Offest_Of;

   -------------------
   -- True_Filename --
   -------------------

   function True_Filename (Filename : String) return String is
      Split : constant Natural := Split_Point (Filename);
   begin
      if Split = No_Split then
         return Filename;
      else
         return Filename (Filename'First .. Split - 1);
      end if;
   end True_Filename;

end Dvaccum.Split_Filename;
