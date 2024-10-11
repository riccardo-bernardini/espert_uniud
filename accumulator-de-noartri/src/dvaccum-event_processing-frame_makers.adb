with PNG_IO;

package body Dvaccum.Event_Processing.Frame_Makers is
   function To_Png_Buffer (X : Frames.Image_Type)
                           return PNG_IO.Image_Buffer
   is
   begin
      return Result : PNG_IO.Image_Buffer :=
        PNG_IO.Create (Integer (Frames.Width (X)),
                       Integer (Frames.Height (X)))
      do
         null;
      end return;
   end To_Png_Buffer;

   task body Frame_Maker is
   begin
      loop
         declare
            use Pixel_Buffers;

            Frame_Number : constant Frame_Index :=
                             Parameters.Pixels.Next_Unprocessed_Frame;

            Frame : constant Frames.Image_Type := Parameters.Initial_Image.all;
         begin
            exit when Frame_Number = No_Frame;

            for Px in Parameters.Pixels.Every_Pixel loop
               null;
            end loop;

            declare
               Filename : constant String :=
                            Parameters.Frame_Name (To_Int (Frame_Number));
            begin
               PNG_IO.Save_Png (Filename => Filename,
                                Image    => To_Png_Buffer (Frame),
                                Color    => Png_Io.Gray,
                                Depth    => 8);
            end;

         end;
      end loop;
   end Frame_Maker;
end Dvaccum.Event_Processing.Frame_Makers;
