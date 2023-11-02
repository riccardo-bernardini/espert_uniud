pragma Ada_2012;

with Ada.Numerics.Elementary_Functions;
--  with Ada.Text_IO; use Ada.Text_IO;

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

   begin
      --  Put_Line ("delta_t=" & Camera_Events.Image (Delta_T));
      case Dynamic.Class is
         when Step | None =>
            return Start;

         when Linear =>
            declare
               Variation : constant Pixel_Value :=
                             Pixel_Value (Delta_T / Dynamic.Time_Constant);

            begin
               if Start > Dynamic.Neutral_Level then
                  return  Pixel_Value'Max (Start - Variation, Dynamic.Neutral_Level);

               else
                  return  Pixel_Value'Min (Start + Variation, Dynamic.Neutral_Level);

               end if;
            end;

         when Exponential =>

            return  Start * Pixel_Value (Exp (-Delta_T / Dynamic.Time_Constant));
      end case;
   end Evolve;

end Memory_Dynamic;
