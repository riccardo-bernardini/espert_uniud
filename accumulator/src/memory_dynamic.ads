with Camera_Events;
with Images;

package Memory_Dynamic is
   type Dynamic_Type is private;

   function No_Decay return Dynamic_Type;

   function Step return Dynamic_Type;

   function Linear (T       : Camera_Events.Duration;
                    Neutral : Images.Pixel_Value)
                    return Dynamic_Type;

   function Exponential (T : Camera_Events.Duration) return Dynamic_Type;

   function Evolve (Start   : Images.Pixel_Value;
                    Dynamic : Dynamic_Type;
                    Delta_T : Camera_Events.Duration)
                    return Images.Pixel_Value;
private
   type Dynamic_Class is (None, Step, Linear, Exponential);

   type Dynamic_Type (Class : Dynamic_Class := Step) is
      record
         case Class is
            when None =>
               null;

            when Step =>
               null;

            when Exponential =>
               Time_Constant : Camera_Events.Duration;

            when Linear =>
               Inverse_Slope : Camera_Events.Duration;
               Neutral_Level : Images.Pixel_Value;

         end case;
      end record;

   function No_Decay return Dynamic_Type
   is (Dynamic_Type'(Class  => None));

   function Step return Dynamic_Type
   is (Dynamic_Type'(Class  => Step));

   function Linear (T : Camera_Events.Duration) return Dynamic_Type
   is (Dynamic_Type'(Class => Linear, Time_Constant => T));

   function Exponential (T : Camera_Events.Duration) return Dynamic_Type
   is (Dynamic_Type'(Class => Exponential, Time_Constant => T));

end Memory_Dynamic;
