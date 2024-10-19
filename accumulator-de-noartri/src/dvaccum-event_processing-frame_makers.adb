
package body Dvaccum.Event_Processing.Frame_Makers is
   task body Frame_Maker is
   begin
      loop
         declare
            use Pixel_Buffers;

            Frame_Number : constant Frame_Index :=
                             Parameters.Pixels.Next_Unprocessed_Frame;

            Filename : constant String :=
                         Parameters.Frame_Name.Element.Make_Name (Frame_Number);

            Frame : Frames.Image_Type := Parameters.Initial_Image;
         begin
            exit when Frame_Number = No_Frame;

            for Pos in Parameters.Pixels.Every_Pixel loop
               declare
                  Pixel : constant Pixel_Buffers.Pixel_Descriptor :=
                            Pixel_Buffers.Element (Pos);
               begin
                  Frame (Pixel.Location.X, Pixel.Location.Y) :=
                    Parameters.Pixels (Pixel.Index, Frame_Number);
               end;
            end loop;

            Frames.Save (Filename => Filename,
                         Image    => frame,
                         Format   => Frames.PNG);
         end;
      end loop;
   end Frame_Maker;
end Dvaccum.Event_Processing.Frame_Makers;
