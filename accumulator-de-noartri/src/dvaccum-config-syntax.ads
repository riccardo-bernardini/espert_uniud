
private package DVAccum.Config.Syntax is


   --  function Parse_Start_Time (Spec : String) return Times.Timestamp;
   --
   --  function Parse_Stop_Time (Spec : String) return Times.Timestamp;


   --  type Decay_Type is (None, Reset, Linear, Exponential);
   --
   --  type Decay_Spec (Class : Decay_Type := None) is
   --     record
   --        case Class is
   --           when None | Reset =>
   --              null;
   --
   --           when Linear | Exponential =>
   --              Tau : Times.Duration;
   --        end case;
   --     end record;

   --  function Parse_Memory_Spec (Spec : String)
   --                              return Decay_Spec;


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
         Start           : Times.Timestamp;
         Stop            : Times.Timestamp;
         Sampling_Period : Times.Duration;
      end record;

   function Parse_Output_Filename_Template (Template : Unbounded_String) return Radix_Spec;

   Bad_Syntax : exception;

end DVAccum.Config.Syntax;
