with DVAccum.Frames;
with DVAccum.Event_IO;

with PNG_IO;

package Dvaccum.Event_Processing is
   type Frame_Name_Generator is
     access function (Frame_Number : Natural) return String;

   procedure Process (Event_Sequence : Event_Io.Event_Sequences.Set;
                      Frame_Name     : Frame_Name_Generator;
                      Event_Weight   : Float;
                      Time_Origin    : Timestamps.Timestamp;
                      From           : Timestamps.Timestamp;
                      To             : Timestamps.Timestamp;
                      Frame_Duration : Timestamps.Duration;
                      Oversampling   : Positive;
                      Initial_Image  : Frames.Image_Buffer);
end Dvaccum.Event_Processing;
