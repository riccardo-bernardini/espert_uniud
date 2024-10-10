
with DVAccum.Frames;
with DVAccum.Event_IO;
with DVAccum.Timestamps;
with DVAccum.Events;

package Dvaccum.Event_Processing is
   type Frame_Name_Generator is
     access function (Frame_Number : Natural) return String;

   type Filter_Coefficients is
     array (Natural range <>) of Frames.Pixel_Value;

   type Filter_Spec (Num_Degree, Den_Degree : Natural) is
      record
         Num : Filter_Coefficients (0 .. Num_Degree);
         Den : Filter_Coefficients (0 .. Den_Degree);
      end record;

   procedure Process (Event_Sequence : Event_Io.Event_Sequences.Set;
                      Frame_Name     : Frame_Name_Generator;
                      Event_Weight   : Float;
                      Filter         : Filter_Spec;
                      Origin_Shift   : Timestamps.Duration;
                      From           : Timestamps.Timestamp;
                      To             : Timestamps.Timestamp;
                      Frame_Duration : Timestamps.Duration;
                      Oversampling   : Positive;
                      Initial_Image  : Frames.Image_Type);
private
   subtype Event_Index is Positive;

   type Event_Array is
     array (Event_Index range <>) of Events.Event_Type;

   type Event_Array_Access is
     access constant Event_Array;

end Dvaccum.Event_Processing;
