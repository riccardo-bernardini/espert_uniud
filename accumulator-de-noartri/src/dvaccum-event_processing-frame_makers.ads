with System.Multiprocessors;

with Ada.Containers.Indefinite_Holders;

with DVAccum.Event_Processing.Pixel_Buffers;
with DVAccum.Frames;    use DVAccum.Frames;
with DVAccum.Frame_Name_Generators;  use DVAccum.Frame_Name_Generators;

use System;

private
package Dvaccum.Event_Processing.Frame_Makers is
   package Generator_Holders is
     new Ada.Containers.Indefinite_Holders (Abstract_Generator'Class);

   type Parameter_Record (Last_X : Coord_X;
                          Last_Y : Coord_Y)
   is
      record
         Pixels        : Pixel_Buffers.Pixel_Buffer_Access;
         Frame_Name    : Generator_Holders.Holder;
         Offset        : Sample_Value;
         Initial_Image : Image_Type (Coord_X'First .. Last_X,
                                     Coord_Y'First .. Last_Y);
      end record;

   type Parameter_Access is
     access constant Parameter_Record;

   task type Frame_Maker (ID         : Multiprocessors.CPU;
                          Parameters : Parameter_Access);
end Dvaccum.Event_Processing.Frame_Makers;
