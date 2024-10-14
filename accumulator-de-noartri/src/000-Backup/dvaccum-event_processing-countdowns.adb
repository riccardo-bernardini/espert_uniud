pragma Ada_2012;
package body Dvaccum.Event_Processing.Countdowns is

   ---------------
   -- Countdown --
   ---------------

   protected body Countdown is

      ----------
      -- Step --
      ----------

      procedure Step is
      begin
         if Counter > 0 then
            Counter := Counter - 1;
         end if;
      end Step;

      ----------
      -- Done --
      ----------

      entry Wait when Counter = 0 is
      begin
         null;
      end Done;

   end Countdown;

end Dvaccum.Event_Processing.Countdowns;
