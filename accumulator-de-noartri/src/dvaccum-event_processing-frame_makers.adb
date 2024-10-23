with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

package body Dvaccum.Event_Processing.Frame_Makers is


   task body Frame_Maker is
      procedure Stampa (X : String) is
      begin
         Put_Line ("[Frame Maker N. " & ID'Image & "]  " & X);
      end Stampa;

      Frame_Number : Extended_Frame_Index;

   begin
      Frame_Number := Parameters.Pixels.Next_Unprocessed_Frame;

      while Frame_Number /= No_Frame loop

         pragma Assert (Frame_Number in Frame_Index);

         declare
            use Pixel_Buffers;

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
                         Image    => Frame,
                         Format   => Frames.PNG);
         end;

         Frame_Number := Parameters.Pixels.Next_Unprocessed_Frame;
      end loop;

   exception
      when E : others =>
         Stampa ("Bum!" & Ada.Exceptions.Exception_Information (E));

   end Frame_Maker;
end Dvaccum.Event_Processing.Frame_Makers;
