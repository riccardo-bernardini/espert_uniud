pragma Ada_2012;
with Ada.Strings.Fixed;

with Interfaces;
with Ada.Sequential_IO;
with Ada.Streams;
with Ada.Text_IO.Text_Streams;
with Ada.Characters.Latin_9;

with Png_IO;

package body DVAccum.Frames is
   package Unsigned_8_IO is
     new Ada.Sequential_IO (Interfaces.Unsigned_8);


   function Strip_Spaces (X : String) return String
   is (Ada.Strings.Fixed.Trim (Source => X,
                               Side   => Ada.Strings.Both));

   type Byte is mod 2 ** 8;

   function To_Byte (Val : Pixel_Value)
                     return Byte
   is
   begin
      if Val < 0.0 then
         return 0;

      elsif Val > Pixel_Value (Byte'Last) then
         return Byte'Last;

      else
         return Byte (Val);
      end if;
   end To_Byte;

   ----------
   -- Load --
   ----------

   function Load
     (Filename : String; Format : Format_Type := Raw_Image_8) return Image_Type
   is
      function Load_Raw_Image_8 (Filename : String) return Image_Type
      is
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

         when Pgm | Png =>
            raise Constraint_Error
              with "Loading format " & Format'Image & " not implemented";
      end case;
   end Load;

   ----------
   -- uniform --
   ----------

   function Uniform
     (X_Size : Positive;
      Y_Size : Positive;
      Value  : Pixel_Value := 0.0)
      return Image_Type
   is
      Last_X : constant X_Coordinate_Type := X_Coordinate_Type (X_Size) - 1;
      Last_Y : constant Y_Coordinate_Type := Y_Coordinate_Type (Y_Size) - 1;

      Result  : constant Image_Type (0 .. Last_X, 0 .. Last_Y) :=
                  (others => (others => Value));
   begin
      return Result;
   end Uniform;

   ----------
   -- Save --
   ----------

   procedure Save
     (Filename : String;
      Image    : Image_Type;
      Format   : Format_Type)
   is
      procedure Save_Raw_Image_8 (Filename : String; Image : Image_Type)
      is
         use Interfaces;
         function Hi (X : Unsigned_16) return Unsigned_8
         is (Unsigned_8 (X / 256));

         function Lo (X : Unsigned_16) return Unsigned_8
         is (Unsigned_8 (X mod 256));

         Output : Unsigned_8_IO.File_Type;
      begin
         Unsigned_8_IO.Create (File => Output,
                               Mode => Unsigned_8_IO.Out_File,
                               Name => Filename);


         Unsigned_8_IO.Write (File => Output,
                              Item => Hi (Unsigned_16 (Width (Image))));

         Unsigned_8_IO.Write (File => Output,
                              Item => Lo (Unsigned_16 (Width (Image))));

         Unsigned_8_IO.Write (File => Output,
                              Item => Hi (Unsigned_16 (Height (Image))));

         Unsigned_8_IO.Write (File => Output,
                              Item => Lo (Unsigned_16 (Height (Image))));

         for X in Image'Range (1) loop
            for Y in Image'Range (2) loop
               Unsigned_8_IO.Write (File => Output,
                                    Item => Unsigned_8 (To_Byte (Image (X, Y))));
            end loop;
         end loop;
      end Save_Raw_Image_8;

      procedure Save_Pgm (Filename : String;
                          Image    : Image_Type)
      is
         use Ada.Text_IO;
         use Ada.Streams;
         use Ada.Characters;

         pragma Compile_Time_Error (Stream_Element'Size /= 8,
                                    "Stream_Element must be 8 bit long");



         Output : File_Type;
      begin
         Create (File => Output,
                 Mode => Out_File,
                 Name => Filename);

         --  Put_Line ("W = " & X_Coordinate_Type'Image (Width (Image)));
         --  Put_Line ("H = " & y_Coordinate_Type'Image (Height (Image)));


         Put (Output, "P5"
              & " " & Strip_Spaces (X_Coordinate_Type'Image (Width (Image)))
              & " " & Strip_Spaces (Y_Coordinate_Type'Image (Height (Image)))
              & " 255"
              & Latin_9.LF);

         declare
            S : constant Text_Streams.Stream_Access :=
                  Text_Streams.Stream (Output);
         begin
            for Row in Image'Range (2) loop
               for Col in Image'Range (1) loop
                  Byte'Write (S, To_Byte (Image (Col, Row)));
               end loop;
            end loop;
         end;

         Close (Output);
      end Save_Pgm;

      procedure Save_Png (Filename : String;
                          Image    : Image_Type)
      is
         use PNG_IO;

         Buffer : Png_IO.Image_Buffer :=
                    Png_IO.Create (Integer (Width (Image)),
                                   Integer (Height (Image)));

      begin
         for X in Image'Range (1) loop
            for Y in Image'Range (2) loop
               Buffer (Integer (X), Integer (Y)) :=
                 Png_Io.Pixel_Value (To_Byte (Image (X, Y)));
            end loop;
         end loop;

         Png_Io.Save_Png (Filename => Filename,
                          Image    => Buffer,
                          Color    => PNG_IO.Gray,
                          Depth    => 8);
      end Save_Png;
   begin
      case Format is
         when Raw_Image_8 =>
            Save_Raw_Image_8 (Filename, Image);

         when PGM =>
            Save_Pgm (Filename, Image);

         when PNG =>
            Save_PNG (Filename, Image);
      end case;
   end Save;


   ---------
   -- "+" --
   ---------

   function "+" (Image : Image_Type; Offset : Pixel_Value) return Image_Type
   is
      Result : Image_Type := Image;
   begin
      Add (Result, Offset);
      return Result;
   end "+";


   ---------
   -- Add --
   ---------

   procedure Add (Image  : in out Image_Type;
                  Offset : Pixel_Value)
   is
   begin
      Multiply_And_Add (Image, 1.0, Offset);
   end Add;

   --------------
   -- Multiply --
   --------------

   procedure Multiply (Image  : in out Image_Type;
                       K      : Pixel_Value)
   is
   begin
      Multiply_And_Add (Image, K, 0.0);
   end Multiply;

   ----------------------
   -- Multiply_And_Add --
   ----------------------

   procedure Multiply_And_Add (Image  : in out Image_Type;
                               K      : Pixel_Value;
                               Offset : Pixel_Value)
   is
   begin
      for Px of Image loop
         Px := Px * K + Offset;
      end loop;
   end Multiply_And_Add;

   ---------
   -- "+" --
   ---------

   function "*" (K : Pixel_Value; Image : Image_Type) return Image_Type
   is
      Result : Image_Type := Image;
   begin
      Multiply (Result, K);
      return Result;
   end "*";


   -------------
   -- Rescale --
   -------------

   function Rescale (Image    : Image_Type;
                     Old_Min  : Pixel_Value;
                     Old_Max  : Pixel_Value;
                     New_Min  : Pixel_Value;
                     New_Max  : Pixel_Value;
                     Saturate : Boolean := True)
                     return Image_Type
   is
      Result : Image_Type := Image;
   begin
      Rescale (Image    => Result,
               Old_Min  => Old_Min,
               Old_Max  => Old_Max,
               New_Min  => New_Min,
               New_Max  => New_Max,
               Saturate => Saturate);

      return Result;
   end Rescale;

   -------------
   -- Rescale --
   -------------

   procedure Rescale (Image    : in out Image_Type;
                      Old_Min  : Pixel_Value;
                      Old_Max  : Pixel_Value;
                      New_Min  : Pixel_Value;
                      New_Max  : Pixel_Value;
                      Saturate : Boolean := True)
   is
      Old_Delta : constant Pixel_Value := Old_Max - Old_Min;
      New_Delta : constant Pixel_Value := New_Max - New_Min;

      K         : constant Pixel_Value := New_Delta / Old_Delta;
      Offset    : constant Pixel_Value := New_Min - Old_Min * K;
   begin
      Multiply_And_Add (Image, K, Offset);

      if Saturate then
         Limit (Image => Image,
                Min   => New_Min,
                Max   => New_Max);
      end if;
   end Rescale;


   --------------
   -- Limit_Up --
   --------------

   procedure Limit_Up (Image : in out Image_Type;
                       Max   : Pixel_Value)
   is
   begin
      for Px of Image loop
         if Px > Max then
            Px := Max;
         end if;
      end loop;
   end Limit_Up;

   ----------------
   -- Limit_Down --
   ----------------

   procedure Limit_Down (Image : in out Image_Type;
                         Min   : Pixel_Value)
   is
   begin
      for Px of Image loop
         if Px < Min then
            Px := Min;
         end if;
      end loop;
   end Limit_Down;

   -----------
   -- Limit --
   -----------

   procedure Limit (Image : in out Image_Type;
                    Min   : Pixel_Value;
                    Max   : Pixel_Value)
   is
   begin
      for Px of Image loop
         if Px < Min then
            Px := Min;

         elsif Px > Max then
            Px := Max;
         end if;
      end loop;
   end Limit;

end DVAccum.Frames;
