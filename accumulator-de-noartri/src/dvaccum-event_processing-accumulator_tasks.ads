with DVAccum.Event_Processing.Segment_Queues;
with DVAccum.Event_Processing.Pixel_Buffers;
with DVAccum.Filters;
private
package Dvaccum.Event_Processing.Accumulator_Tasks is
   type Parameter_Record is
      record
         Segments       : Segment_Queues.Segment_Queue_Access;
         Events         : Event_Array_Access;
         Pixels         : Pixel_Buffers.Pixel_Buffer_Access;
         Filter         : Filters.Filter_Type;
         From           : Timestamps.Timestamp;
         To             : Timestamps.Timestamp;
         Frame_Duration : Timestamps.Duration;
         Oversampling   : Positive;
      end record;

   type Parameter_Access is
     access constant Parameter_Record;

   task type Accumulator (Parameters : Parameter_Access);
end Dvaccum.Event_Processing.Accumulator_Tasks;
