with DVAccum.Event_Processing.Pixel_Buffers;

private
package Dvaccum.Event_Processing.Frame_Makers is
   type Parameter_Record is
      record
         Pixels        : Pixel_Buffers.Pixel_Buffer;
         Frame_Name    : Frame_Name_Generator;
         Initial_Image : Image_Access;
      end record;

   type Parameter_Access is
     access constant Parameter_Record;

   task type Frame_Maker (Parameters : Parameter_Access);
end Dvaccum.Event_Processing.Frame_Makers;
