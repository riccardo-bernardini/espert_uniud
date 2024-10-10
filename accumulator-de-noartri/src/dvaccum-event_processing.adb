with DVAccum.Event_Processing.Segment_Queues;

package body Dvaccum.Event_Processing is
   procedure Process (Event_Sequence : Event_Io.Event_Sequences.Set;
                      Frame_Name     : Frame_Name_Generator;
                      Event_Weight   : Float;
                      Time_Origin    : Timestamps.Timestamp;
                      From           : Timestamps.Timestamp;
                      To             : Timestamps.Timestamp;
                      Frame_Duration : Timestamps.Duration;
                      Oversampling   : Positive;
                      Initial_Image  : Frames.Image_Type)
   is
   begin
      null;
   end Process;
end Dvaccum.Event_Processing;
