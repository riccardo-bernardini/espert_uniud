pragma Ada_2012;
with Interfaces.C.Strings;

use Interfaces;

package body PNG is


   ---------
   -- Ref --
   ---------

   function Ref
     (Img : aliased in out Image_Buffer;
      X   : Camera_Events.X_Coordinate_Type;
      Y   : Camera_Events.Y_Coordinate_Type)
      return Ref_Element
   is (Ref_Element'(Data => Img.Pixels (Img.Index (X, Y))'Access));

   ------------
   -- Create --
   ------------

   function Create
     (Width  : Camera_Events.X_Coordinate_Type;
      Heigth : Camera_Events.Y_Coordinate_Type) return Image_Buffer
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
     (Filename : String; Image : Image_Buffer; Color : Color_Type;
      Depth    : Bit_Depth)
   is
      function Write_Png (Filename : C.Strings.Chars_Ptr;
                          Rows     : Row_Pointers.Pointer;
                          Width    : Unsigned_32;
                          Heigth   : Unsigned_32;
                          Depth    : C.Int;
                          Color    : C.Int)
                          return C.Int
        with
          Import => True,
          Convention => C;

      F_Name : C.Strings.Chars_Ptr := C.Strings.New_String (Filename);
   begin

      C.Strings.Free (F_Name);

      pragma Compile_Time_Warning (Standard.True, "Save_Png unimplemented");
      raise Program_Error with "Unimplemented procedure Save_Png";
   end Save_Png;

end PNG;
