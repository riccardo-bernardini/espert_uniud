
with DVAccum.Frames;
with DVAccum.Event_IO;
with DVAccum.Timestamps;
with DVAccum.Events;
with DVAccum.Filters;

package Dvaccum.Event_Processing is

   type Frame_Index is private;

   No_Frame : constant Frame_Index;

   function To_Int (X : Frame_Index) return Natural
     with
       Pre => X /= No_Frame;

   --
   --  Frame_Name_Generator returns the name of the file used
   --  to save a frame.  If Frame=No_Frame the function can return
   --  any value since it will not be used.
   --
   type Frame_Name_Generator is
     access function (Frame : Frame_Index) return String;

   procedure Process (Event_Sequence : Event_Io.Event_Sequence;
                      Frame_Name     : Frame_Name_Generator;
                      Event_Weight   : Sample_Value;
                      Offset         : Sample_Value;
                      Filter         : Filters.Filter_Type;
                      From           : Timestamps.Timestamp;
                      To             : Timestamps.Timestamp;
                      Frame_Duration : Timestamps.Duration;
                      Oversampling   : Positive;
                      Initial_Image  : Frames.Image_Type);
private
   type Frame_Index is range -1 .. Integer'Last;

   subtype Valid_Frame_Index is
     Frame_Index range Frame_Index'First + 1 .. Frame_Index'Last;

   No_Frame : constant Frame_Index := Frame_Index'First;

   function To_Int (X : Frame_Index) return Natural
   is (Natural (X));

   subtype Event_Index is Positive;

   type Event_Array is
     array (Event_Index range <>) of Events.Event_Type;

   type Event_Array_Access is
     access constant Event_Array;

   type Image_Access is
     access constant Frames.Image_Type;

end Dvaccum.Event_Processing;
