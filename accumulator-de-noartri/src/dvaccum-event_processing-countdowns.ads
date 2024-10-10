private
package Dvaccum.Event_Processing.Countdowns is
   protected type Countdown (Start : Natural) is
      procedure Step;
      entry Wait;
   private
      Counter : Natural := Start;
   end Countdown;

   type Countdown_Access is access Countdown;
end Dvaccum.Event_Processing.Countdowns;
