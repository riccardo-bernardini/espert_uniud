
package Images is
   type Pixel_Value is new Float;

   type X_Coordinate_Type is mod 2 ** 16;
   type Y_Coordinate_Type is mod 2 ** 16;

   type Point_Type is
      record
         X : X_Coordinate_Type;
         Y : Y_Coordinate_Type;
      end record;


   type Image_Type is
     array (X_Coordinate_Type range <>,
            Y_Coordinate_Type range <>)
     of Pixel_Value;

   function Width (Image : Image_Type) return X_Coordinate_Type
   is (Image'Last (1)-Image'First (1)+1);

   function Height (Image : Image_Type) return Y_Coordinate_Type
   is (Image'Last (2)-Image'First (2)+1);

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
