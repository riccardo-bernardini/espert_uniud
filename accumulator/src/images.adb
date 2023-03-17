pragma Ada_2012;

with Interfaces;
with Ada.Sequential_IO;

package body Images is
   package Unsigned_16_IO is
     new Ada.Sequential_IO (Interfaces.Unsigned_16);

   ----------
   -- Load --
   ----------

   function Load
     (Filename : String; Format : Format_Type := Raw_Image) return Image_Type
   is
      function Load_Raw_Image (Filename : String) return Image_Type
      is
         use Camera_Events;

         Input : Unsigned_16_IO.File_Type;

         X_Size : X_Coordinate_Type;
         Y_Size : Y_Coordinate_Type;
      begin
         Unsigned_16_IO.Open (File => Input,
                              Mode => Unsigned_16_IO.In_File,
                              Name => Filename);

         Unsigned_16_IO.Read (File => Input,
                              Item => Interfaces.Unsigned_16 (X_Size));

         Unsigned_16_IO.Read (File => Input,
                              Item => Interfaces.Unsigned_16 (Y_Size));

         declare
            use Interfaces;

            Result : Image_Type (0 .. X_Size - 1, 0 .. Y_Size - 1);
         begin
            for X in Result'Range (1) loop
               for Y in Result'Range (2) loop
                  Unsigned_16_IO.Read (File => Input,
                                       Item => Unsigned_16 (Result (X, Y)));
               end loop;
            end loop;

            Unsigned_16_IO.Close (Input);

            return Result;
         end;
      end Load_Raw_Image;
   begin
      case Format is
         when Raw_Image =>
            return Load_Raw_Image (Filename);
      end case;
   end Load;

   ----------
   -- Zero --
   ----------

   function Zero
     (X_Size : Camera_Events.X_Coordinate_Type;
      Y_Size : Camera_Events.Y_Coordinate_Type) return Image_Type
   is
      use type Camera_Events.X_Coordinate_Type;
      use type Camera_Events.Y_Coordinate_Type;

      Result : constant Image_Type (0 .. X_Size - 1, 0 .. Y_Size - 1) :=
                 (others => (others => 0));
   begin
      return Result;
   end Zero;

   ----------
   -- Save --
   ----------

   procedure Save
     (Filename : String; Image : Image_Type; Format : Format_Type := Raw_Image)
   is
      procedure Save_Raw_Image (Filename : String; Image : Image_Type)
      is
         use Interfaces;

         Output : Unsigned_16_IO.File_Type;
      begin
         Unsigned_16_IO.Open (File => Output,
                              Mode => Unsigned_16_IO.Out_File,
                              Name => Filename);

         Unsigned_16_IO.Write (File => Output,
                               Item => Unsigned_16 (Image'Length (1)));

         Unsigned_16_IO.Write (File => Output,
                               Item => Unsigned_16 (Image'Length (2)));

         for X in Image'Range (1) loop
            for Y in Image'Range (2) loop
               Unsigned_16_IO.Write (File => Output,
                                     Item => Unsigned_16 (Image (X, Y)));
            end loop;
         end loop;
      end Save_Raw_Image;
   begin
      case Format is
         when Raw_Image =>
            Save_Raw_Image (Filename, Image);
      end case;
   end Save;

end Images;
