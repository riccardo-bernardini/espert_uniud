with Camera_Events;
with Images;

package Memory_Dynamic is
   type Dynamic_Type is private;

   function No_Decay return Dynamic_Type;

   function Step (Reset_To : Images.Pixel_Value)  return Dynamic_Type;

   function Linear (T       : Camera_Events.Duration;
                    Neutral : Images.Pixel_Value)
                    return Dynamic_Type;

   function Exponential (T          : Camera_Events.Duration;
                         Zero_Level : Images.Pixel_Value)
                         return Dynamic_Type;

   function Evolve (Initial_Value   : Images.Pixel_Value;
      Dynamic         : Dynamic_Type;
      Delta_T         : Camera_Events.Duration)
                    return Images.Pixel_Value;

   function Is_Reset (X : Dynamic_Type) return Boolean;

   function Reset_Value (X : Dynamic_Type) return Images.Pixel_Value
     with
       Pre => Is_Reset (X);

   function Image (X : Dynamic_Type) return String;
private
   type Dynamic_Class is (None, Step, Linear, Exponential);

   type Dynamic_Type (Class : Dynamic_Class := Step) is
      record
         case Class is
            when None =>
               null;

            when Step =>
               Reset_Value : Images.Pixel_Value;


            when Exponential =>
               Time_Constant : Camera_Events.Duration;
               Zero_Level    : Images.Pixel_Value;

            when Linear =>
               Inverse_Slope : Camera_Events.Duration;
               Neutral_Level : Images.Pixel_Value;

         end case;
      end record;


   function Is_Reset (X : Dynamic_Type) return Boolean
   is (X.Class = Step);

   function Reset_Value (X : Dynamic_Type) return Images.Pixel_Value
   is (X.Reset_Value);

   function No_Decay return Dynamic_Type
   is (Dynamic_Type'(Class  => None));

   function Step (Reset_To : Images.Pixel_Value)  return Dynamic_Type
   is (Dynamic_Type'(Class  => Step, Reset_Value => Reset_To));

   function Linear (T       : Camera_Events.Duration;
                    Neutral : Images.Pixel_Value)
                    return Dynamic_Type
   is (Dynamic_Type'(Class => Linear,
                     Inverse_Slope => T,
                     Neutral_Level => Neutral));

   function Exponential (T          : Camera_Events.Duration;
                         Zero_Level : Images.Pixel_Value)
                         return Dynamic_Type
   is (Dynamic_Type'(Class => Exponential,
                     Time_Constant => T,
                     Zero_Level    => Zero_Level));

end Memory_Dynamic;
