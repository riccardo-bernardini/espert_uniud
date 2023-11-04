with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

private package Config.Syntax is
   function Parse_Time_Spec (Spec : String) return Float;

   function Parse_Time_Spec (Spec : String) return Camera_Events.Duration
   is (Camera_Events.To_Duration (Parse_Time_Spec (Spec)));

   function Parse_Time_Spec (Spec : String) return Camera_Events.Timestamp
   is (Camera_Events.To_Timestamp (Parse_Time_Spec (Spec)));

   function Parse_Time_Spec (Spec : Unbounded_String) return Camera_Events.Duration
   is (Parse_Time_Spec (To_String (Spec)));

   function Parse_Time_Spec (Spec : Unbounded_String) return Camera_Events.Timestamp
   is (Parse_Time_Spec (To_String (Spec)));

   function Parse_Start_Time (Spec : String) return Camera_Events.Timestamp;

   function Parse_Stop_Time (Spec : String) return Camera_Events.Timestamp;


   type Decay_Type is (None, Reset, Linear, Exponential);

   type Decay_Spec (Class : Decay_Type := None) is
      record
         case Class is
            when None | Reset =>
               null;

            when Linear | Exponential =>
               Tau : Camera_Events.Duration;
         end case;
      end record;

   function Parse_Memory_Spec (Spec : String)
                               return Decay_Spec;


   Frame_Number_Marker : constant String := "%d";

   Frame_Number_Padding_Char : constant Character := '0';

   Frame_Number_Default_Width : constant Positive := 5;


   type Radix_Spec is
      record
         Head               : Unbounded_String;
         Tail               : Unbounded_String;
         Frame_Number_Width : Positive;
         Padding_Char       : Character;
         Frame_Format       : Images.Format_Type;
      end record;

   type Sampling_Spec is
      record
         Start           : Camera_Events.Timestamp;
         Stop            : Camera_Events.Timestamp;
         Sampling_Period : Camera_Events.Duration;
      end record;

   function Parse_Output_Filename_Template (Template : Unbounded_String) return Radix_Spec;



end Config.Syntax;
