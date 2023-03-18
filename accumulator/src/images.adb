pragma Ada_2012;

with Interfaces;
with Ada.Sequential_IO;

package body Images is
   package Unsigned_8_IO is
     new Ada.Sequential_IO (Interfaces.Unsigned_8);

   ----------
   -- Load --
   ----------

   function Load
     (Filename : String; Format : Format_Type := Raw_Image_8) return Image_Type
   is
      function Load_Raw_Image_8 (Filename : String) return Image_Type
      is
         use Camera_Events;
         use Interfaces;

         Input : Unsigned_8_IO.File_Type;



         function Get_Unsigned_16 (Input : Unsigned_8_IO.File_Type)
                                   return Unsigned_16
         is
            function Join (Hi, Lo : Unsigned_8) return Unsigned_16
            is (Unsigned_16 (Hi) * 256 + Unsigned_16 (Lo));

            Hi     : Unsigned_8;
            Lo     : Unsigned_8;
         begin
            Unsigned_8_IO.Read (File => Input,
                                Item => Hi);

            Unsigned_8_IO.Read (File => Input,
                                Item => Lo);

            return Join (Hi, Lo);
         end Get_Unsigned_16;
      begin
         Unsigned_8_IO.Open (File => Input,
                             Mode => Unsigned_8_IO.In_File,
                             Name => Filename);

         declare
            use Interfaces;

            X_Size : constant X_Coordinate_Type :=
                       X_Coordinate_Type (Get_Unsigned_16 (Input));

            Y_Size : constant Y_Coordinate_Type :=
                       Y_Coordinate_Type (Get_Unsigned_16 (Input));

            Result : Image_Type (0 .. X_Size - 1, 0 .. Y_Size - 1);
         begin
            for X in Result'Range (1) loop
               for Y in Result'Range (2) loop
                  Unsigned_8_IO.Read (File => Input,
                                      Item => Unsigned_8 (Result (X, Y)));
               end loop;
            end loop;

            Unsigned_8_IO.Close (Input);

            return Result;
         end;
      end Load_Raw_Image_8;
   begin
      case Format is
         when Raw_Image_8 =>
            return Load_Raw_Image_8 (Filename);
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
                 (others => (others => 0.0));
   begin
      return Result;
   end Zero;

   ----------
   -- Save --
   ----------

   procedure Save
     (Filename : String; Image : Image_Type; Format : Format_Type := Raw_Image_8)
   is
      procedure Save_Raw_Image_8 (Filename : String; Image : Image_Type)
      is
         use Interfaces;
         use Camera_Events;

         function Hi (X : Unsigned_16) return Unsigned_8
         is (Unsigned_8 (X / 256));

         function Lo (X : Unsigned_16) return Unsigned_8
         is (Unsigned_8 (X mod 256));

         Output : Unsigned_8_IO.File_Type;
         X_Size : constant X_Coordinate_Type := Image'Length (1);
         Y_Size : constant Y_Coordinate_Type := Image'Length (2);
      begin
         Unsigned_8_IO.Open (File => Output,
                             Mode => Unsigned_8_IO.Out_File,
                             Name => Filename);


         Unsigned_8_IO.Write (File => Output,
                              Item => Hi (Unsigned_16 (X_Size)));

         Unsigned_8_IO.Write (File => Output,
                              Item => Lo (Unsigned_16 (X_Size)));

         Unsigned_8_IO.Write (File => Output,
                              Item => Hi (Unsigned_16 (Y_Size)));

         Unsigned_8_IO.Write (File => Output,
                              Item => Lo (Unsigned_16 (Y_Size)));

         for X in Image'Range (1) loop
            for Y in Image'Range (2) loop
               Unsigned_8_IO.Write (File => Output,
                                    Item => Unsigned_8 (Image (X, Y)));
            end loop;
         end loop;
      end Save_Raw_Image_8;
   begin
      case Format is
         when Raw_Image_8 =>
            Save_Raw_Image_8 (Filename, Image);
      end case;
   end Save;

end Images;
