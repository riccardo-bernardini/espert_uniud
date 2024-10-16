
package DVAccum.Frames is


   type Image_Type is
     array (Coord_X range <>,
            Coord_Y range <>)
     of Sample_Value;

   function Width (Image : Image_Type) return Coord_X
   is (Image'Last (1)-Image'First (1)+1);

   function Height (Image : Image_Type) return Coord_Y
   is (Image'Last (2)-Image'First (2)+1);

   function "+" (Image : Image_Type; Offset : Sample_Value) return Image_Type;

   procedure Add (Image  : in out Image_Type;
                  Offset : Sample_Value)
     with
       Post => (for all X in Image'Range (1) =>
                  (for all Y in Image'Range (2) =>
                     (Image (X, Y) = Image'Old (X, Y) + Offset)));

   function "*" (K : Sample_Value; Image : Image_Type) return Image_Type;

   procedure Multiply (Image  : in out Image_Type;
                       K      : Sample_Value)
     with
       Post => (for all X in Image'Range (1) =>
                  (for all Y in Image'Range (2) =>
                     (Image (X, Y) = K * Image'Old (X, Y))));

   procedure Multiply_And_Add (Image  : in out Image_Type;
                               K      : Sample_Value;
                               Offset : Sample_Value)
     with
       Post => (for all X in Image'Range (1) =>
                  (for all Y in Image'Range (2) =>
                     (Image (X, Y) = K * Image'Old (X, Y) + Offset)));


   function Rescale (Image    : Image_Type;
                     Old_Min  : Sample_Value;
                     Old_Max  : Sample_Value;
                     New_Min  : Sample_Value;
                     New_Max  : Sample_Value;
                     Saturate : Boolean := True)
                     return Image_Type;

   procedure Rescale (Image    : in out Image_Type;
                      Old_Min  : Sample_Value;
                      Old_Max  : Sample_Value;
                      New_Min  : Sample_Value;
                      New_Max  : Sample_Value;
                      Saturate : Boolean := True);

   procedure Limit_Up (Image : in out Image_Type;
                       Max   : Sample_Value);

   procedure Limit_Down (Image : in out Image_Type;
                         Min   : Sample_Value);

   procedure Limit (Image : in out Image_Type;
                    Min   : Sample_Value;
                    Max   : Sample_Value);

   type Format_Type is (Raw_Image_8, Pgm, Png);

   function Load (Filename : String;
                  Format   : Format_Type := Raw_Image_8) return Image_Type;

   function Uniform (X_Size : Positive;
                     Y_Size : Positive;
                     Value  : Sample_Value := 0.0)
                     return Image_Type;

   procedure Save (Filename : String;
                   Image    : Image_Type;
                   Format   : Format_Type);

end DVAccum.Frames;
