pragma Ada_2012;

with Ada.Numerics.Elementary_Functions;

package body Memory_Dynamic is


   ------------
   -- Evolve --
   ------------

   function Evolve
     (Start   : Images.Pixel_Value;
      Dynamic : Dynamic_Type;
      Delta_T : Camera_Events.Duration)
      return Images.Pixel_Value
   is
      use Images;
      use type Camera_Events.Duration;
      use Ada.Numerics.Elementary_Functions;

      new_value : Images.Pixel_Value;
   begin
      case Dynamic.Class is
         when Step =>
            return Start;

         when Linear =>
            New_Value := Start - Pixel_Value (Delta_T / Dynamic.Time_Constant);
            if New_Value < 0.0 then
               return 0.0;

            else
               return New_Value;
            end if;

         when Exponential =>
            return Start * Pixel_Value (Exp (-Delta_T / Dynamic.Time_Constant));
      end case;
   end Evolve;

end Memory_Dynamic;
