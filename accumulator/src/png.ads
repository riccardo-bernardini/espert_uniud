with Ada.Finalization;

with Camera_Events;

use Ada;

package PNG is
   type Pixel_Value is mod 2 ** 8;

   type Bit_Depth is range 1 .. 16
     with Static_Predicate => Bit_Depth in 1 | 2 | 4 | 8 | 16;

   type Color_Type is
     (
      Gray,
      Gray_Alpha,
      Palette,
      Rgb,
      Rgb_Alpha,
      Mask_Palette,
      Mask_Color,
      Mask_Alpha
     );

   type Interlace_Method is (None, Adam_7);



   type Image_Buffer (<>) is
     new Finalization.Limited_Controlled
       with private
     with
       Variable_Indexing => Ref;

   type Ref_Element(Data : access Pixel_Value) is limited private
     with Implicit_Dereference => Data;

   function Ref (Img : aliased in out Image_Buffer;
                 X   : Camera_Events.X_Coordinate_Type;
                 Y   : Camera_Events.Y_Coordinate_Type)
                 return Ref_Element;

   function Create (Width  : Camera_Events.X_Coordinate_Type;
                    Heigth : Camera_Events.Y_Coordinate_Type)
                    return Image_Buffer;

   procedure Save_Png (Filename : String;
                       Image    : Image_Buffer;
                       Color    : Color_Type;
                       Depth    : Bit_Depth);

private
   use Camera_Events;

   type Ref_Element(Data : access Pixel_Value) is limited null record;

   type Pixel_Index is range 1 .. 2 ** 32 - 1;

   type Pixel_Array is
     array (Pixel_Index range <>) of aliased Pixel_Value;

   type Pixel_Array_Access is not null access Pixel_Array;

   function Index (Img : Image_Buffer;
                   X   : x_Coordinate_Type;
                   Y   : Y_Coordinate_Type)
                   return Pixel_Index
     with
       Pre => X <= Img.N_Cols and Y <= Img.N_Rows;


   type Image_Buffer (N_Pixel : Pixel_Index) is
     new Finalization.Limited_Controlled with
      record
         Pixels : Pixel_Array_Access;
         N_Rows : Camera_Events.Y_Coordinate_Type;
         N_Cols : Camera_Events.X_Coordinate_Type;
      end record
     with
       Type_Invariant => N_Pixel = Pixel_Index (N_Cols) * Pixel_Index (N_Rows);

   function Index (Img : Image_Buffer;
                   X   : x_Coordinate_Type;
                   Y   : Y_Coordinate_Type)
                   return Pixel_Index
   is (Pixel_Index (X - X_Coordinate_Type'First)
       + Pixel_Index (Img.N_Cols) * Pixel_Index (Y - Y_Coordinate_Type'First));
end PNG;
