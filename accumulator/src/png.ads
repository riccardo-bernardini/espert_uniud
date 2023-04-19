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
   type Ref_Element (Data : access Pixel_Value) is limited null record;

   type Image_Row is
     array (Camera_Events.X_Coordinate_Type range <>) of aliased Pixel_Value;

   type Row_Access is access Image_Row;

   type Row_Array is
     array (Camera_Events.Y_Coordinate_Type range <>) of aliased Row_Access;

   type Image_Buffer (N_Rows : Camera_Events.Y_Coordinate_Type) is
     new Finalization.Limited_Controlled with
      record
         Rows : Row_Array (1 .. N_Rows);
      end record;
end PNG;
