pragma Ada_2012;
with Interfaces.C.Strings;
with Interfaces.C.Pointers;

use Interfaces;

package body PNG is
   package Row_Pointers is
     new Interfaces.C.Pointers (Index              => Camera_Events.Y_Coordinate_Type,
                                Element            => Row_Access,
                                Element_Array      => Row_Array,
                                Default_Terminator => 0);
   ---------
   -- Ref --
   ---------

   function Ref
     (Img : aliased in out Image_Buffer;
      X   : Camera_Events.X_Coordinate_Type;
      Y   : Camera_Events.Y_Coordinate_Type)
      return Ref_Element
   is (Ref_Element'(Data => Img.Rows (Y) (X)'Access));

   ------------
   -- Create --
   ------------

   function Create
     (Width  : Camera_Events.X_Coordinate_Type;
      Heigth : Camera_Events.Y_Coordinate_Type) return Image_Buffer
   is
   begin
      return Result : Image_Buffer (Heigth) do
         for Row in Result.Rows'Range loop
            Result.Rows (Row) := new Image_Row (1 .. Width);
         end loop;
      end return;
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
