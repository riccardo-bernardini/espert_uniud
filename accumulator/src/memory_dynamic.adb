pragma Ada_2012;

with Ada.Numerics.Elementary_Functions;
--  with Ada.Text_IO; use Ada.Text_IO;

package body Memory_Dynamic is


   ------------
   -- Evolve --
   ------------

   ------------
   -- Evolve --
   ------------

   function Evolve
     (Initial_Value   : Images.Pixel_Value;
      Dynamic         : Dynamic_Type;
      Delta_T         : Times.Duration)
      return Images.Pixel_Value
   is
      use Images;
      use type Times.Duration;
      use Ada.Numerics.Elementary_Functions;

   begin
      --  Put_Line ("delta_t=" & Times.Image (Delta_T));
      case Dynamic.Class is
         when Step | None =>
            return Initial_Value;

         when Linear =>
            declare
               Variation : constant Pixel_Value :=
                             Pixel_Value (Delta_T / Dynamic.Inverse_Slope);

            begin
               if Initial_Value > Dynamic.Neutral_Level then
                  return  Pixel_Value'Max (Initial_Value - Variation,
                                           Dynamic.Neutral_Level);

               else
                  return  Pixel_Value'Min (Initial_Value + Variation,
                                           Dynamic.Neutral_Level);

               end if;
            end;

         when Exponential =>
            declare
               Decay : constant Pixel_Value :=
                         Pixel_Value (Exp (-Delta_T / Dynamic.Time_Constant));

               Zero  : constant Pixel_Value := Dynamic.Zero_Level;
            begin
               return  Zero + (Initial_Value - Zero)  * Decay;
            end;
      end case;
   end Evolve;

   -----------
   -- Image --
   -----------

   function Image (X : Dynamic_Type) return String
   is
   begin
      case X.Class is
         when None =>
            return "none";

         when Step =>
            return "Reset at " & X.Reset_Value'Image;

         when Linear =>
            return "Linear, tau=" & Times.Image (X.Inverse_Slope, True);

         when Exponential =>
            return "Exponential, tau=" & Times.Image (X.Time_Constant, True);

      end case;
   end Image;

end Memory_Dynamic;
