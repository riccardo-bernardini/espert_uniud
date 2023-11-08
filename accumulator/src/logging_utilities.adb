with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Images;
with Memory_Dynamic;

package body Logging_Utilities is



   package Float_Formatting is new Float_IO (Float);

   --------------------
   -- Put_Line_Maybe --
   --------------------

   procedure Put_Line_Maybe (Verbose : Boolean;
                             Text    : String)
   is
   begin
      if Verbose then
         Put_Line (Standard_Error, Text);
      end if;
   end Put_Line_Maybe;

   ---------------
   -- Put_Maybe --
   ---------------

   procedure Put_Maybe (Verbose : Boolean;
                        Text    : String)
   is
   begin
      if Verbose then
         Put (Standard_Error, Text);
         Flush (Standard_Error);
      end if;
   end Put_Maybe;

   -----------------------
   -- Show_Progress_Bar --
   -----------------------

   procedure Show_Progress_Bar (Start_Time   : Camera_Events.Timestamp;
                                Stop_Time    : Camera_Events.Timestamp;
                                Current_Time : Camera_Events.Timestamp)
   is
      use Camera_Events;
      use Ada.Strings.Fixed;

      Full     : constant Camera_Events.Duration := Stop_Time - Start_Time;
      Done     : constant Camera_Events.Duration := Current_Time - Start_Time;
      Fraction : constant Float := Done / Full;

      N_Columns : constant Integer := Integer (Terminal_Width)-10;

      Done_Section_Length : constant Integer :=
                              Integer (Fraction * Float (N_Columns - 1));

      Remaining_Section_Length : constant Integer :=
                                   N_Columns - Done_Section_Length - 1;
   begin
      Put (Standard_Error, ASCII.CR);
      Put (Standard_Error, Done_Section_Length * "=");
      Put (Standard_Error, ">");

      if Remaining_Section_Length > 0 then
         Put (Standard_Error, Remaining_Section_Length * '-');
      end if;

      Put (Standard_Error, " ");
      Float_Formatting.Put (File => Standard_Error,
                            Item => 100.0 * Fraction,
                            Fore => 3,
                            Aft  => 1,
                            Exp  => 0);

      Put (Standard_Error, "%");
      Flush (Standard_Error);
   end Show_Progress_Bar;

   function Get_Metadata_Text return String_Vectors.Vector
   is
      use Ada.Strings.Fixed;

      Result : String_Vectors.Vector (1000);

      Label_Size : constant := 10;

      procedure Append (Label, Text : String) is
         function Padding return String
         is (Integer'Max (0, Label_Size - Label'Length) * " ");

      begin
         String_Vectors.Append (Result, Label & Padding & " : " & Text);
      end Append;
   begin
      Append ("Input", Config.Input);
      Append ("Sampling", Camera_Events.Image (Config.Sampling_Period, True));

      Append ("Min", Images.Pixel_Value'Image (Config.Pixel_Min));
      Append ("Max", Images.Pixel_Value'Image (Config.Pixel_Max));

      if Config.Reset_Each_Frame then
         Append ("Reset at", Images.Pixel_Value'Image (Config.Reset_Value));
      end if;

      Append ("Memory", Memory_Dynamic.Image (Config.Forgetting_Method));

      Append ("Start", Camera_Events.Image (Config.Start_At (Camera_Events.Minus_Infinity)));
      Append ("Stop", Camera_Events.Image (Config.Stop_At (Camera_Events.Infinity)));
      return Result;
   end Get_Metadata_Text;

   -------------------
   -- Dump_Metadata --
   -------------------

   procedure Dump_Metadata (Destination_Filename : String)
   is
      use String_Vectors;

      procedure Do_Dump (Output   : File_Type;
                         Metadata : String_Vectors.Vector)
      is
      begin
         for I in First_Index (Metadata) .. Last_Index (Metadata) loop
            Put_Line (Output, To_String (Element (Metadata, I)));
         end loop;
      end Do_Dump;
   begin
      if Destination_Filename = "-" then
         Do_Dump (Standard_Error, Get_Metadata_Text);

      else
         declare
            Output : File_Type;
         begin
            Create (File => Output,
                    Mode => Out_File,
                    Name => Destination_Filename);

            Do_Dump (Output, Get_Metadata_Text);

            Close (Output);
         end;
      end if;
   end Dump_Metadata;
end Logging_Utilities;
