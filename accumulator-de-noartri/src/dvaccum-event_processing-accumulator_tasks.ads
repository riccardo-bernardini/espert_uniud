with DVAccum.Event_Processing.Countdowns;
with DVAccum.Event_Processing.Segment_Queues;
with DVAccum.Event_Processing.Pixel_Buffers;

private
package Dvaccum.Event_Processing.Accumulator_Tasks is
   task type Accumulator is
      entry Start (Segments       : Segment_Queues.Segment_Queue_Access;
                   Events         : Event_Array_Access;
                   Pixels         : Pixel_Buffers.Pixel_Buffer;
                   Filter         : Filter_Spec;
                   From           : Timestamps.Timestamp;
                   To             : Timestamps.Timestamp;
                   Frame_Duration : Timestamps.Duration;
                   Oversampling   : Positive);
   end Accumulator;
end Dvaccum.Event_Processing.Accumulator_Tasks;
