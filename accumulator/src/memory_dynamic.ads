with Camera_Events;
with Images;

package Memory_Dynamic is
   type Dynamic_Type is interface;

   function Evolve (Dynamic : Dynamic_Type;
                    Start   : Images.Pixel_Value;
                    Delta_T : Camera_Events.Duration)
                    return Images.Pixel_Value
                    is abstract;

   function Step return Dynamic_Type'Class;

   function Linear (T : Camera_Events.Duration) return Dynamic_Type'Class;

   function Exponential (T : Camera_Events.Duration) return Dynamic_Type'Class;
private
   type Step_Dynamic is
     new Dynamic_Type
   with
     null record;


   overriding function Evolve (Dynamic : Step_Dynamic;
                               Start   : Images.Pixel_Value;
                               Delta_T : Camera_Events.Duration)
                               return Images.Pixel_Value;
   type Linear_Dynamic is
     new Dynamic_Type
   with
      record
         Decay_Time : Camera_Events.Duration;
      end record;


   overriding function Evolve (Dynamic : Linear_Dynamic;
                               Start   : Images.Pixel_Value;
                               Delta_T : Camera_Events.Duration)
                               return Images.Pixel_Value;

   type Exponential_Dynamic is
     new Dynamic_Type
   with
      record
         Decay_Time : Camera_Events.Duration;
      end record;


   overriding function Evolve (Dynamic : Exponential_Dynamic;
                               Start   : Images.Pixel_Value;
                               Delta_T : Camera_Events.Duration)
                               return Images.Pixel_Value;
   --
   --  type Dynamic_Type (Class : Dynamic_Class := Step) is
   --     record
   --        case Class is
   --           when Step =>
   --              null;
   --
   --           when Linear | Exponential =>
   --              Time_Constant : Camera_Events.Duration;
   --
   --        end case;
   --     end record;

end Memory_Dynamic;
