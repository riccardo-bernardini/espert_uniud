pragma Ada_2012;

with Ada.Command_Line;
with Ada.Containers;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;        use Ada.Strings;

with Ada.Characters.Handling;
with Ada.Strings.Maps.Constants;

with Ada.Characters.Latin_9;
with Gnat.Regpat;
with Tokenize;
with Ada.Text_IO; use Ada.Text_IO;

package body Config is
   use type Camera_Events.Timestamp;

   type Radix_Spec is
      record
         Head               : Unbounded_String;
         Tail               : Unbounded_String;
         Frame_Number_Width : Positive;
         Padding_Char       : Character;
      end record;

   type Sampling_Spec is
      record
         Start           : Camera_Events.Timestamp;
         Stop            : Camera_Events.Timestamp;
         Sampling_Period : Camera_Events.Duration;
      end record;

   Frame_Number_Marker : constant String := "%d";

   Frame_Number_Padding_Char : constant Character := '0';

   Frame_Number_Default_Width : constant Positive := 5;

   Frame_Format : Images.Format_Type;

   I_Am_Ready   : Boolean := False;

   Memory_Dynamic_Spec : Memory_Dynamic.Dynamic_Type;


   Input_Stream : File_Access := null;

   Sampling_Info : Sampling_Spec;

   Frame_Filename_Spec : Radix_Spec;

   First_Image_Filename : Unbounded_String;

   function Package_Ready return Boolean
   is (I_Am_Ready);

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   function Strip_Spaces (S : String) return String
   is (Fixed.Trim (Source => S,
                   Side   => Both));

   procedure Parse_Command_Line is
      use Ada.Command_Line;

      Argument_Counter : Positive := 1;

      function Current_Argument return String
        with
          Pre => Argument_Counter <= Ada.Command_Line.Argument_Count;

      function Current_Argument return String
      is (Argument (Argument_Counter));

      procedure Next_Argument
        with
          Pre => Argument_Counter <= Ada.Command_Line.Argument_Count;

      procedure Next_Argument is
      begin
         Argument_Counter := Argument_Counter + 1;
      end Next_Argument;

      function Parse_Time_Spec (Spec : String) return Float
      is
         use Ada.Strings.Fixed;
         use Ada.Strings.Maps.Constants;
         use Ada.Strings.Maps;

         Stripped : constant String := Strip_Spaces (Spec);

         End_Of_Number : constant Natural :=
                           Index (Source => Stripped,
                                  Set    => Decimal_Digit_Set or To_Set ("._-+eE"),
                                  Test   => Outside);

         Unit : constant String :=
                  (if End_Of_Number = 0
                   then
                      ""
                   else
                      Strip_Spaces (Stripped (End_Of_Number .. Stripped'Last)));

         Value : constant String :=
                   (if End_Of_Number = 0
                    then
                       Stripped
                    else
                       Stripped (Stripped'First .. End_Of_Number - 1));

         --------------
         -- To_Float --
         --------------

         function To_Float (X : String) return Float
         is
            use Gnat.Regpat;

            Integer_Regexp : constant Pattern_Matcher :=
                               Compile ("^[-+]?[0-9_]+$");

            Float_Regexp : constant Pattern_Matcher :=
                             Compile ("^[-+]?[0-9_]+\.[0-9_]+([eE][-+]?[0-9_]+)?$");
         begin
            return (if Match (Integer_Regexp, X) then
                       Float (Integer'Value (X))

                    elsif Match (Float_Regexp, X) then
                       Float'Value (X)

                    else
                       raise Bad_Command_Line
                         with "Bad number '" & X & "'");
         end To_Float;
      begin
         if Unit = "" then
            return To_Float (Value) * Camera_Events.Timestamps_Per_Second;

         elsif Unit = "s" then
            return To_Float (Value);

         elsif Unit = "ms" then
            return 1.0e-3 * To_Float (Value);

         elsif Unit = "us" then
            return 1.0e-6 * To_Float (Value);

         elsif Unit = "ns" then
            return 1.0e-9 * To_Float (Value);

         elsif Unit = "fps" then
            return 1.0 / To_Float (Value);

         else
            raise Bad_Command_Line with "Unknown unit '" & Unit & "'";
         end if;
      end Parse_Time_Spec;

      function Parse_Time_Spec (Spec : String) return Camera_Events.Duration
      is (Camera_Events.To_Duration (Parse_Time_Spec (Spec)));

      function Parse_Time_Spec (Spec : String) return Camera_Events.Timestamp
      is (Camera_Events.To_Timestamp (Parse_Time_Spec (Spec)));

      function Parse_Memory_Spec (Spec : String)
                                  return Memory_Dynamic.Dynamic_Type
      is
         use Ada.Strings.Fixed;
         use Ada.Characters.Handling;

         Colon_Pos : constant Natural := Index (Source  => Spec,
                                                Pattern => ":");

         Method : constant String :=
                    Strip_Spaces (To_Lower ((if Colon_Pos = 0
                                  then
                                     Spec
                                  else
                                     Spec (Spec'First .. Colon_Pos - 1))));

         Parameter : constant String :=
                       Strip_Spaces ((if Colon_Pos = 0
                                     then
                                        ""
                                     else
                                        Spec (Colon_Pos + 1 .. Spec'Last)));
      begin
         if Method = "step" or Method = "s" then
            if Parameter /= "" then
               raise Bad_Command_Line with "'step' with parameter";
            end if;

            return Memory_Dynamic.Step;

         elsif Method = "linear" or Method = "lin" or Method = "l" then
            if Parameter = "" then
               raise Bad_Command_Line with "'linear' needs a time constant";
            end if;

            return Memory_Dynamic.Linear (Parse_Time_Spec (Parameter));

         elsif Method = "exponential" or Method = "exp" or Method = "e" then
            if Parameter = "" then
               raise Bad_Command_Line with "'exponential' needs a time constant";
            end if;

            return Memory_Dynamic.Exponential (Parse_Time_Spec (Parameter));

         else
            raise Bad_Command_Line with "Unknown dynamic '" & Method & "'";
         end if;
      end Parse_Memory_Spec;

      function Parse_Sampling_Spec (Spec : String) return Sampling_Spec
      is
      begin
         if 0 = Fixed.Index (Source  => Spec, Pattern => ":") then
            --  Put_Line ("99(" & Spec & ")");
            return Sampling_Spec'(Start           => Camera_Events.Minus_Infinity,
                                  Stop            => Camera_Events.Infinity,
                                  Sampling_Period => Parse_Time_Spec (Spec));

         else
            --  Put_Line ("88(" & Spec & ")");
            declare
               use type Ada.Containers.Count_Type;
               use Ada.Strings.Fixed;

               Pieces : constant Tokenize.Token_List :=
                          Tokenize.Split (To_Be_Splitted => Spec,
                                          Separator      => ':');

               function Parse_Start_Time (Spec : String) return Camera_Events.Timestamp
               is (if Spec = "" then
                      Camera_Events.Minus_Infinity
                   else
                      Parse_Time_Spec (Spec));

               function Parse_Stop_Time (Spec : String) return Camera_Events.Timestamp
               is (if Spec  = "" then
                      Camera_Events.Infinity
                   else
                      Parse_Time_Spec (Spec));
            begin
               if Pieces.Length /= 3 then
                  raise Bad_Command_Line with "Bad sampling syntax";
               end if;



               return Result : constant Sampling_Spec :=
                 (Start           => Parse_Start_Time (Trim (Pieces (1), Both)),
                  Stop            => Parse_Stop_Time (Trim (Pieces (3), Both)),
                  Sampling_Period => Parse_Time_Spec (Trim (Pieces (2), Both)))
               do

                  if Result.Stop < Result.Start then
                     raise Bad_Command_Line
                       with "Stopping time < start time";
                  end if;

               end return;
            end;
         end if;
      end Parse_Sampling_Spec;

      function Parse_Radix (Spec : String) return Radix_Spec
      is
         use Ada.Strings.Fixed;

         function To_Unbounded (X : String) return Unbounded_String
                                   renames To_Unbounded_String;


         Frame_Number_Position : constant Natural :=
                                   Index (Source  => Spec,
                                          Pattern => Frame_Number_Marker);

         function Extract_Format (Filename : String) return Images.Format_Type
         is
            subtype Extension is String (1 .. 3);

            Format_To_Extension   : constant array (Images.Format_Type) of Extension :=
                                      (Images.Raw_Image_8 => "raw",
                                       Images.PGM         => "pgm",
                                       Images.PNG         => "png");
         begin
            if Filename'Length < 5 or else Filename (Filename'Last - 3) /= '.' then
               return Images.Raw_Image_8;
            end if;

            declare
               Ext : constant Extension := Tail (Spec, 3);
            begin
               for Fmt in Format_To_Extension'Range loop
                  if Ext = Format_To_Extension (Fmt) then
                     return Fmt;
                  end if;
               end loop;

               raise Bad_Command_Line
                 with "Unknown extension: '" & Ext & "'";
            end;
         end Extract_Format;
      begin
         if Frame_Number_Position = 0 then
            raise Bad_Command_Line
              with "Missing '" & Frame_Number_Marker & "' in frame filename radix";
         end if;

         Frame_Format := Extract_Format (Spec);

         return Radix_Spec'
           (Head               =>
              To_Unbounded (Spec (Spec'First .. Frame_Number_Position - 1)),
            Tail               =>
              To_Unbounded (Spec (Frame_Number_Position + Frame_Number_Marker'Length .. Spec'Last)),
            Frame_Number_Width => Frame_Number_Default_Width,
            Padding_Char       => Frame_Number_Padding_Char);
      end Parse_Radix;

      function Help_Asked return Boolean
      is (Argument_Count = 0 or else
            (Argument_Count = 1 and then
               (Argument (1) = "help"
                or Argument (1) = "h"
                or Argument (1) = "-h"
                or Argument (1) = "--help"
                or Argument (1) = "?")));


      Input_Filename_Given : Boolean := False;
      First_Image_Given    : Boolean := False;
   begin
      if Help_Asked then
         raise Full_Help_Asked;
      end if;

      --
      -- CL syntax
      --
      --    main memory-spec  sampling-step  radix  [input-filename] [first-image]
      --
      if Argument_Count < 3 or Argument_Count > 5 then
         raise Bad_Command_Line with "Wrong number of arguments";

      elsif Argument_Count = 3 then
         Input_Filename_Given := False;
         First_Image_Given := False;

      elsif Argument_Count = 4 then
         Input_Filename_Given := True;
         First_Image_Given := False;

      elsif Argument_Count = 5 then
         Input_Filename_Given := True;
         First_Image_Given := True;

      else
         raise Program_Error; -- We should never arrive here
      end if;


      Memory_Dynamic_Spec := Parse_Memory_Spec (Current_Argument);
      Next_Argument;

      Sampling_Info := Parse_Sampling_Spec (Current_Argument);
      Next_Argument;

      --  Ada.Text_IO.Put_Line (Camera_Events.Image (Sampling_Step));

      Frame_Filename_Spec := Parse_Radix (Current_Argument);
      Next_Argument;

      if Input_Filename_Given and then Current_Argument /= "-"  then
         Input_Stream := new Ada.Text_IO.File_Type;

         Ada.Text_IO.Open (File => Input_Stream.all,
                           Mode => Ada.Text_IO.In_File,
                           Name => Current_Argument);

         Next_Argument;
      else
         Input_Stream := new Ada.Text_IO.File_Type'(Ada.Text_IO.Standard_Input);

      end if;

      if First_Image_Given then
         First_Image_Filename := To_Unbounded_String (Current_Argument);
         Next_Argument;
      else
         First_Image_Filename := Null_Unbounded_String;
      end if;

      pragma Assert (Input_Stream /= null
                     and then
                     Ada.Text_Io.Is_Open (Input_Stream.all));

      I_Am_Ready := True;

   end Parse_Command_Line;

   -----------
   -- Input --
   -----------

   function Input return File_Access
   is (Input_Stream);

   ---------------------
   -- Sampling_Period --
   ---------------------

   function Sampling_Period return Camera_Events.Duration
   is (Sampling_Info.Sampling_Period);

   --------------
   -- Start_At --
   --------------

   function Start_At (T_Min : Camera_Events.Timestamp) return Camera_Events.Timestamp
   is (if Sampling_Info.Start > T_Min then
          Sampling_Info.Start
       else
          T_Min);


   -------------
   -- Stop_At --
   -------------

   function Stop_At (T_Max : Camera_Events.Timestamp) return Camera_Events.Timestamp
   is (if Sampling_Info.Stop = Camera_Events.Infinity then
          T_Max

       elsif Sampling_Info.Stop = Camera_Events.Minus_Infinity then
          raise Constraint_Error

       else
          Sampling_Info.Stop);


   -----------------------
   -- Forgetting_Method --
   -----------------------

   function Forgetting_Method return Memory_Dynamic.Dynamic_Type
   is (Memory_Dynamic_Spec);

   -------------------
   -- Output_Format --
   -------------------

   function Output_Format return Images.Format_Type
   is (Frame_Format);

   -----------
   -- Radix --
   -----------


   function Frame_Filename (N : Frame_Index) return String
   is
      function Format_Frame_Number (N : Frame_Index) return String
      is
         use Ada.Strings.Fixed;

         Raw_Image : constant String := Strip_Spaces (Frame_Index'Image (N));

         Padding_Length : constant Integer :=
                            Frame_Filename_Spec.Frame_Number_Width - Raw_Image'Length;

         Padding   : constant String :=
                       (if Padding_Length <= 0
                        then
                           ""
                        else
                           Padding_Length * Frame_Filename_Spec.Padding_Char);
      begin
         return Padding & Raw_Image;
      end Format_Frame_Number;
   begin
      return To_String (Frame_Filename_Spec.Head &
                          Format_Frame_Number (N) &
                          Frame_Filename_Spec.Tail);
   end Frame_Filename;

   ---------------------
   -- Has_Start_Image --
   ---------------------

   function Has_Start_Image return Boolean
   is (First_Image_Filename /= Null_Unbounded_String);

   --------------------------
   -- Start_Image_Filename --
   --------------------------

   function Start_Image_Filename return String
   is (To_String (First_Image_Filename));


   function Start_Image (Size_X : Camera_Events.X_Coordinate_Type;
                         Size_Y : Camera_Events.Y_Coordinate_Type)
                            return Images.Image_Type
   is
      use Camera_Events;
   begin
      if Has_Start_Image then
         return Result : constant Images.Image_Type :=
           Images.Load (Start_Image_Filename) do

            if Result'Length (1) /= Size_X or Result'Length (2) /= Size_Y then
               raise Constraint_Error
                 with "Non-compatible start image size";
            end if;
         end return;

      else
         return Images.Uniform (Size_X, Size_Y, 128.0);

      end if;
   end Start_Image;

   function Short_Help_Text return String
   is ("Usage: "
       & Ada.Command_Line.Command_Name
       & " memory-spec  sampling  radix  [event-filename] [first-image]");

   function Long_Help_Text return String
   is
      use Ada.Characters.Latin_9;
   begin
      return Short_Help_Text
        & LF
        & LF
        & "memory-spec: " & LF
        & "   step (or s)" & LF
        & "   linear:t (or lin:t or l:t)" & LF
        & "   exponential:t (or exp:t or e:t)" & LF
        & LF
        & "sampling: sampling period (framerate). Examples" & LF
        & "  in timestamp units    : 1234 (no unit specified)" & LF
        & "  in seconds, ms, etc   : 1s   1ms  1000ns" & LF
        & "  in frame per seconds  : 1000fps" & LF
        & LF
        & "Note: NO SPACE between number and unit" & LF
        & LF
        & "radix: used to generate the filenames of the frames." & LF
        & "'" & Frame_Number_Marker & "' is replaced by the frame number" & LF
        & "(" & Strip_Spaces (Frame_Number_Default_Width'Image) & " digits"
        & " left padded with '" & Frame_Number_Padding_Char & "')" & LF
        & LF
        & "event-filename: optional, the file with the camera events." & LF
        & "If missing the standard input is read"
        & LF;
   end Long_Help_Text;

end Config;
