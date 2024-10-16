with String_Formatting;

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

   type Sampling_Spec is
      record
         Start           : Timestamps.Timestamp;
         Stop            : Timestamps.Timestamp;
         Sampling_Period : Timestamps.Duration;
      end record;

   function Parse_Output_Filename_Template (Template : String)
                                            return String_Formatting.Parsed_Format;

   Bad_Syntax : exception;

end DVAccum.Config.Syntax;
