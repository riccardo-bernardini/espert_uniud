with PNG_IO;

package body Dvaccum.Event_Processing.Frame_Makers is
   function To_Png_Buffer (Image  : Frames.Image_Type;
                           Offset : Frames.Pixel_Value)
                           return PNG_IO.Image_Buffer
   is
      function To_Png_Pixel (X : Frames.Pixel_Value)
                             return Png_Io.Pixel_Value
      is
         use type Frames.Pixel_Value;
      begin
         if X > Frames.Pixel_Value (Png_Io.Pixel_Value'Last) then
            return Png_Io.Pixel_Value'Last;

         elsif X < Frames.Pixel_Value (Png_Io.Pixel_Value'First) then
            return Png_Io.Pixel_Value'First;

         else
            return Png_Io.Pixel_Value (X);
         end if;
      end To_Png_Pixel;

      use type Frames.Pixel_Value;
   begin
      return Result : PNG_IO.Image_Buffer :=
        PNG_IO.Create (Integer (Frames.Width (Image)),
                       Integer (Frames.Height (Image)))
      do
         for X in Image'Range (1) loop
            for Y in Image'Range (2) loop
               Result (Integer (X), Integer (Y)) :=
                 To_Png_Pixel (Image (X, Y)+Offset);
            end loop;
         end loop;
      end return;
   end To_Png_Buffer;

   task body Frame_Maker is
      P              : Pixel_Buffers.Pixel_Buffer;
      Name_Generator : Frame_Name_Generator;
      First_Image    : Image_Access;
      Offset         : Frames.Pixel_Value;
   begin
      accept Start (Pixels        : Pixel_Buffers.Pixel_Buffer;
                    Frame_Name    : Frame_Name_Generator;
                    Initial_Image : Image_Access;
                    Image_Offset  : Frames.Pixel_Value)
      do
         P := Pixels;
         Name_Generator := Frame_Name;
         First_Image := Initial_Image;
         Offset := Image_Offset;
      end Start;

      loop
         declare
            use Pixel_Buffers;

            Frame_Number : constant Frame_Index := P.Next_Unprocessed_Frame;

            Filename : constant String := Name_Generator (To_Int (Frame_Number));

            Frame : Frames.Image_Type := First_Image.all;
         begin
            exit when Frame_Number = No_Frame;

            for Pos in P.Every_Pixel loop
               declare
                  Px : constant Pixel_Descriptor := Element (Pos);
               begin
                  Frame (Px.Location.X, Px.Location.Y) :=
                    P.Value (Frame_Number, Px.Index);
               end;
            end loop;

            PNG_IO.Save_Png (Filename => Filename,
                             Image    => To_Png_Buffer (Frame, Offset),
                             Color    => Png_Io.Gray,
                             Depth    => 8);
         end;
      end loop;
   end Frame_Maker;
end Dvaccum.Event_Processing.Frame_Makers;
