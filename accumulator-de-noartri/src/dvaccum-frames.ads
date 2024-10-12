
package DVAccum.Frames is
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

   function "+" (Image : Image_Type; Offset : Pixel_Value) return Image_Type;

   procedure Add (Image  : in out Image_Type;
                  Offset : Pixel_Value)
     with
       Post => (for all X in Image'Range (1) =>
                  (for all Y in Image'Range (2) =>
                     (Image (X, Y) = Image'Old (X, Y) + Offset)));

   function "*" (K : Pixel_Value; Image : Image_Type) return Image_Type;

   procedure Multiply (Image  : in out Image_Type;
                       K      : Pixel_Value)
     with
       Post => (for all X in Image'Range (1) =>
                  (for all Y in Image'Range (2) =>
                     (Image (X, Y) = K * Image'Old (X, Y))));

   procedure Multiply_And_Add (Image  : in out Image_Type;
                               K      : Pixel_Value;
                               Offset : Pixel_Value)
     with
       Post => (for all X in Image'Range (1) =>
                  (for all Y in Image'Range (2) =>
                     (Image (X, Y) = K * Image'Old (X, Y) + Offset)));


   function Rescale (Image    : Image_Type;
                     Old_Min  : Pixel_Value;
                     Old_Max  : Pixel_Value;
                     New_Min  : Pixel_Value;
                     New_Max  : Pixel_Value;
                     Saturate : Boolean := True)
                     return Image_Type;

   procedure Rescale (Image    : in out Image_Type;
                      Old_Min  : Pixel_Value;
                      Old_Max  : Pixel_Value;
                      New_Min  : Pixel_Value;
                      New_Max  : Pixel_Value;
                      Saturate : Boolean := True);

   procedure Limit_Up (Image : in out Image_Type;
                       Max   : Pixel_Value);

   procedure Limit_Down (Image : in out Image_Type;
                         Min   : Pixel_Value);

   procedure Limit (Image : in out Image_Type;
                    Min   : Pixel_Value;
                    Max   : Pixel_Value);

   type Format_Type is (Raw_Image_8, Pgm, Png);

   function Load (Filename : String;
                  Format   : Format_Type := Raw_Image_8) return Image_Type;

   function Uniform (X_Size : Positive;
                     Y_Size : Positive;
                     Value  : Pixel_Value := 0.0)
                     return Image_Type;

   procedure Save (Filename : String;
                   Image    : Image_Type;
                   Format   : Format_Type);

end DVAccum.Frames;
