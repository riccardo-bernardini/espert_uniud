pragma Ada_2012;
with Interfaces.C.Strings;

with Png_H;

use Interfaces;


package body Png_IO is
   Color_To_C : constant array (Color_Type) of C.Int :=
                  (
                   Gray         => Png_H.PNG_COLOR_TYPE_GRAY,
                   Gray_Alpha   => Png_H.PNG_COLOR_TYPE_GRAY_ALPHA,
                   Palette      => Png_H.PNG_COLOR_TYPE_PALETTE,
                   Rgb          => Png_H.PNG_COLOR_TYPE_RGB,
                   Rgb_Alpha    => Png_H.PNG_COLOR_TYPE_RGB_ALPHA,
                   Mask_Palette => Png_H.PNG_COLOR_MASK_PALETTE,
                   Mask_Color   => Png_H.PNG_COLOR_MASK_COLOR,
                   Mask_Alpha   => Png_H.PNG_COLOR_MASK_ALPHA
                  );


   ---------
   -- Ref --
   ---------

   function Ref
     (Img : aliased in out Image_Buffer;
      X   : Natural;
      Y   : Natural)
      return Ref_Element
   is (Ref_Element'(Data => Img.Pixels (Img.Index (X, Y))'Access));

   ------------
   -- Create --
   ------------

   function Create
     (Width  : Positive;
      Heigth : Positive) return Image_Buffer
   is
      use Finalization;

      N_Pixel : constant Pixel_Index :=
                  Pixel_Index (Width) * Pixel_Index (Heigth);

      Buffer  : constant Pixel_Array_Access := new Pixel_Array (1 .. N_Pixel);
   begin
      Buffer.all := (others => 0);

      return Image_Buffer'(Limited_Controlled with
                             N_Pixel => N_Pixel,
                           Pixels  => Buffer,
                           N_Rows  => Heigth,
                           N_Cols  => Width);
   end Create;

   --------------
   -- Save_Png --
   --------------

   procedure Save_Png
     (Filename : String;
      Image    : Image_Buffer;
      Color    : Color_Type;
      Depth    : Bit_Depth)
   is
      function Write_Png (Filename : C.Strings.Chars_Ptr;
                          Image    : Pixel_Array;
                          Width    : Unsigned_32;
                          Heigth   : Unsigned_32;
                          Depth    : C.Int;
                          Color    : C.Int)
                          return C.Int
        with
          Import => True,
          Convention => C,
          External_Name => "write_png_file";

      F_Name   : C.Strings.Chars_Ptr := C.Strings.New_String (Filename);

      ERR_OK     : constant := 0;
      ERR_OPEN   : constant := -1;
      ERR_ALLOC  : constant := -2;
      ERR_SETJMP : constant := -3;
   begin
      declare
         Err_Code : constant C.Int :=
                      Write_Png (Filename => F_Name,
                                 Image    => Image.Pixels.all,
                                 Width    => Unsigned_32 (Image.N_Cols),
                                 Heigth   => Unsigned_32 (Image.N_Rows),
                                 Depth    => C.Int (Depth),
                                 Color    => Color_To_C (Color));
      begin

         C.Strings.Free (F_Name);

         case Err_Code is
            when ERR_OK =>
               null;

            when ERR_OPEN =>
               raise Program_Error
                 with "Could not open file '" & Filename & "'";

            when ERR_ALLOC =>
               raise Program_Error
                 with "Could not allocate PNG structures";

            when ERR_SETJMP =>
               raise Program_Error
                 with "Error in PNG library";

            when others =>
               raise Program_Error
                 with "Unknown error code: " & Err_Code'Image;
         end case;
      end;
   end Save_Png;
end Png_IO;
