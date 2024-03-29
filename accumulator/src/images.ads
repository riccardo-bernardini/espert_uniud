with Camera_Events;

package Images is
   type Pixel_Value is new Float;

   type Image_Type is
     array (Camera_Events.X_Coordinate_Type range <>,
            Camera_Events.Y_Coordinate_Type range <>)
     of Pixel_Value;

   use type Camera_Events.X_Coordinate_Type;
   use type Camera_Events.Y_Coordinate_Type;

   function Width (Image : Image_Type) return Camera_Events.X_Coordinate_Type
   is (Image'Last (1)-Image'First (1)+1);

   function Height (Image : Image_Type) return Camera_Events.Y_Coordinate_Type
   is (Image'Last (2)-Image'First (2)+1);

   --  Default_X_Size : constant Camera_Events.X_Coordinate_Type := 640;
   --  Default_y_Size : constant Camera_Events.y_Coordinate_Type := 480;

   type Format_Type is (Raw_Image_8, Pgm, Png);

   function Load (Filename : String;
                  Format   : Format_Type := Raw_Image_8) return Image_Type;

   function Uniform (X_Size : Camera_Events.X_Coordinate_Type;
                     Y_Size : Camera_Events.Y_Coordinate_Type;
                     Value  : Pixel_Value := 0.0)
                     return Image_Type;

   procedure Save (Filename : String;
                   Image    : Image_Type;
                   Format   : Format_Type;
                   Min      : Pixel_Value;
                   Max      : Pixel_Value);
end Images;
