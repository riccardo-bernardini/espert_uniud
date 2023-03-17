with Camera_Events;

package Images is
   type Pixel_Value is mod 2 ** 16;

   type Image_Type is
     array (Camera_Events.X_Coordinate_Type range <>,
            Camera_Events.Y_Coordinate_Type range <>)
     of Pixel_Value;

   type Format_Type is (Raw_Image);

   function Load (Filename : String;
                  Format   : Format_Type := Raw_Image) return Image_Type;

   function Zero (X_Size : Camera_Events.X_Coordinate_Type;
                  Y_Size : Camera_Events.Y_Coordinate_Type)
                  return Image_Type;

   procedure Save (Filename : String;
                   Image    : Image_Type;
                   Format   : Format_Type := Raw_Image);
end Images;
