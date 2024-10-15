with DVAccum.Event_Processing.Pixel_Buffers;

private
package Dvaccum.Event_Processing.Frame_Makers is
   task type Frame_Maker is
      entry Start (Pixels        : Pixel_Buffers.Pixel_Buffer;
                   Frame_Name    : Frame_Name_Generator;
                   Initial_Image : Image_Access;
                   Image_Offset  : Frames.Pixel_Value);

   end Frame_Maker;
end Dvaccum.Event_Processing.Frame_Makers;
