with DVAccum.Event_Processing.Pixel_Buffers;
with DVAccum.Frames;    use DVAccum.Frames;

private
package Dvaccum.Event_Processing.Frame_Makers is
   type Parameter_Record (Last_X : Coord_X;
                          Last_Y : Coord_Y)
   is
      record
         Pixels        : Pixel_Buffers.Pixel_Buffer_Access;
         Frame_Name    : Frame_Name_Generator;
         Offset        : Sample_Value;
         Initial_Image : Image_Type (Coord_X'First .. Last_X,
                                     Coord_Y'First .. Last_Y);
      end record;

   type Parameter_Access is
     access constant Parameter_Record;

   task type Frame_Maker (Parameters : Parameter_Access);
end Dvaccum.Event_Processing.Frame_Makers;
