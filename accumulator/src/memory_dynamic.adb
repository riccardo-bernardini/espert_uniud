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

      New_Value : Images.Pixel_Value;
   begin
      --  Put_Line ("delta_t=" & Camera_Events.Image (Delta_T));
      case Dynamic.Class is
         when Step =>
            return Start;

         when Linear =>
            --  Put_Line ("delta=" & Float'Image (Delta_T / Dynamic.Time_Constant));
            New_Value := Start - Pixel_Value (Delta_T / Dynamic.Time_Constant);

            --  Put_Line (Start'Image & New_Value'Image);
            if New_Value < 0.0 then
               return 0.0;

            else
               return New_Value;
            end if;

         when Exponential =>

            New_Value := Start * Pixel_Value (Exp (-Delta_T / Dynamic.Time_Constant));

            --  Put_Line ("mult=" & Float'Image (Exp (-Delta_T / Dynamic.Time_Constant)));

            --  Put_Line (Start'Image & New_Value'Image);

            return New_Value;
      end case;
   end Evolve;

end Memory_Dynamic;
