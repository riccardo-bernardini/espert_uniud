
with DVAccum.Frames;
with DVAccum.Event_IO;
with DVAccum.Timestamps;
with DVAccum.Events;
with DVAccum.Filters;
with DVAccum.Frame_Name_Generators;

package Dvaccum.Event_Processing is
   use Timestamps;

   procedure Process (Event_Sequence : Event_Io.Event_Sequence;
                      Frame_Name     : Frame_Name_Generators.Abstract_Generator'Class;
                      Event_Weight   : Sample_Value;
                      Offset         : Sample_Value;
                      Filter         : Filters.Filter_Type;
                      From           : Timestamps.Timestamp;
                      To             : Timestamps.Timestamp;
                      Frame_Duration : Timestamps.Duration;
                      Oversampling   : Positive;
                      Initial_Image  : Frames.Image_Type)
     with
       Pre => Frame_Duration <= To - From;
private
   subtype Event_Index is Positive;

   type Event_Array is
     array (Event_Index range <>) of Events.Event_Type;

   type Event_Array_Access is
     access constant Event_Array;

   type Image_Access is
     access constant Frames.Image_Type;

end Dvaccum.Event_Processing;
