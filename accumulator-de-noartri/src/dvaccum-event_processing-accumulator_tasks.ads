with DVAccum.Event_Processing.Countdowns;
with DVAccum.Event_Processing.Segment_Queues;
with DVAccum.Event_Processing.Pixel_Buffers;

private
package Dvaccum.Event_Processing.Accumulator_Tasks is
   task type Accumulator is
      entry Start (Countdown : Countdowns.Countdown_Access;
                   Segments  : Segment_Queues.Segment_Queue_Access;
                   Events    : Event_Array_Access;
                   Pixels    : Pixel_Buffers.Pixel_Buffer;
                   Filter    : Filter_Spec);
   end Accumulator;
end Dvaccum.Event_Processing.Accumulator_Tasks;
