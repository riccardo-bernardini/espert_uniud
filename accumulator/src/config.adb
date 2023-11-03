pragma Ada_2012;

with Ada.Command_Line;
--  with Ada.Containers;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;        use Ada.Strings;

--  with Ada.Characters.Handling;
--  with Ada.Strings.Maps.Constants;

with Ada.Characters.Latin_9;
--  with Gnat.Regpat;
--  with Tokenize;
--  with Ada.Text_IO; use Ada.Text_IO;
with Generic_Command_Line_Parser;

with Interfaces.C;

use Interfaces;

with Config.Syntax;  use Config.Syntax;

package body Config is
   function "+" (X : String) return Unbounded_String
                 renames To_Unbounded_String;

   function Strip_Spaces (S : String) return String
   is (Fixed.Trim (Source => S,
                   Side   => Both));



   function Is_A_Tty (Descriptor : C.Int) return Boolean
   is
      use type C.Int;

      function C_Is_A_Tty (Descriptor : C.Int) return C.Int
        with
          Import => True,
          Convention => C,
          External_Name => "isatty";
   begin
      return (C_Is_A_Tty (Descriptor) = 1);
   end Is_A_Tty;

   use type Camera_Events.Timestamp;




   I_Am_Ready   : Boolean := False;

   Memory_Dynamic_Spec : Memory_Dynamic.Dynamic_Type;


   Requested_Verbosity : Verbosity;

   Input_Filename : Unbounded_String := Null_Unbounded_String;

   Sampling_Info : Sampling_Spec;

   Frame_Filename_Spec : Radix_Spec;

   First_Image_Filename : Unbounded_String;

   Min_Level            : Images.Pixel_Value;
   Max_Level            : Images.Pixel_Value;
   Neutral_Level        : Images.Pixel_Value;

   Event_W              : Images.Pixel_Value;

   Rectify_Requested    : Boolean := False;
   Lazy_Decay_Requested : Boolean := False;

   function Package_Ready return Boolean
   is (I_Am_Ready);

   ------------------------
   -- Parse_Command_Line --
   ------------------------




   procedure Parse_Command_Line is
      use Ada.Command_Line;
      use type Images.Pixel_Value;

      type Options is
        (
         Decay,
         Sampling,
         Frame_Rate,
         Output_Template,
         Input_Spec,
         Start_Time,
         Stop_Time,
         Min,
         Max,
         Neutral,
         Event_Weigth,
         Rectify,
         Lazy_Decay
        );

      package CL_Parser is
        new Generic_Command_Line_Parser (Options);

      Option_Specs : constant CL_Parser.Option_Names :=
                       (
                        Decay           => +"decay",
                        Sampling        => +"sampling",
                        Frame_Rate      => +"framerate|frame-rate|fps",
                        Output_Template => +"output",
                        Input_Spec      => +"input",
                        Start_Time      => +"start",
                        Stop_Time       => + "stop",
                        Min             => +"min",
                        Max             => +"max",
                        Neutral         => +"neutral",
                        Event_Weigth    => +"weigth",
                        Rectify         => +"rectify",
                        Lazy_Decay      => +"lazy"
                       );

      Defaults : constant CL_Parser.Option_Defaults :=
                   (
                    Decay           => CL_Parser.Mandatory_Option,
                    Sampling        => CL_Parser.Ignore_If_Missing,
                    Frame_Rate      => CL_Parser.Ignore_If_Missing,
                    Output_Template => CL_Parser.Mandatory_Option,
                    Input_Spec      => CL_Parser.Ignore_If_Missing,
                    Start_Time      => (CL_Parser.Use_Default, +""),
                    Stop_Time       => (CL_Parser.Use_Default, +""),
                    Min             => (CL_Parser.Use_Default, +"0.0"),
                    Max             => (CL_Parser.Use_Default, +"1.0"),
                    Neutral         => CL_Parser.Ignore_If_Missing,
                    Event_Weigth    => (CL_Parser.Use_Default, +"1.0"),
                    Rectify         => CL_Parser.Ignore_If_Missing,
                    Lazy_Decay      => CL_Parser.Ignore_If_Missing
                   );


      function Help_Asked return Boolean
      is (Argument_Count = 0 or else
            (Argument_Count = 1 and then
               (Argument (1) = "help"
                or Argument (1) = "h"
                or Argument (1) = "-h"
                or Argument (1) = "--help"
                or Argument (1) = "?")));

      function To_Pixel_Value (X : Unbounded_String) return Images.Pixel_Value
      is (Images.Pixel_Value'Value (To_String (X)));



      Parsed_Options       : CL_Parser.Option_Values;

      procedure Set_Levels is
      begin
         Min_Level := To_Pixel_Value (Parsed_Options (Min).Value);
         Max_Level := To_Pixel_Value (Parsed_Options (Max).Value);

         if Parsed_Options (Neutral).Missing then
            Neutral_Level := (Min_Level + Max_Level) / 2.0;

         else
            Neutral_Level := To_Pixel_Value (Parsed_Options (Neutral).Value);

         end if;
      end Set_Levels;

      procedure Set_Decay is

         Chosen_Decay : constant Decay_Spec :=
                          Parse_Memory_Spec (To_String (Parsed_Options (Decay).Value));
      begin
         case Chosen_Decay.Class is
            when None =>
               Memory_Dynamic_Spec := Memory_Dynamic.No_Decay;

            when Linear =>
               Memory_Dynamic_Spec := Memory_Dynamic.Linear (T       => Chosen_Decay.Tau,
                                                             Neutral => Neutral_Level);

            when Exponential =>
               Memory_Dynamic_Spec := Memory_Dynamic.Exponential (Chosen_Decay.Tau);

            when Reset =>
               Memory_Dynamic_Spec := Memory_Dynamic.Step (Reset_To => Neutral_Level);

         end case;
      end Set_Decay;

      procedure Parse_Options_And_Apply_Defaults is
         Missing_Options : Unbounded_String;
      begin
         CL_Parser.Parse (Names  => Option_Specs,
                          Result => Parsed_Options);

         CL_Parser.Apply_Defaults (Values   => Parsed_Options,
                                   Missing  => Missing_Options,
                                   Defaults => Defaults);

         if Missing_Options /= Null_Unbounded_String then
            raise Bad_Command_Line
              with
                "Missing mandatory options: " & To_String (Missing_Options);
         end if;
      end Parse_Options_And_Apply_Defaults;

      procedure Set_Sampling_Spec is
         Sampling_Period : Camera_Events.Duration;
      begin
         if Parsed_Options (Sampling).Missing and not Parsed_Options (Frame_Rate).Missing then
            Sampling_Period := Parse_Time_Spec (Parsed_Options (Frame_Rate).Value & "fps");

         elsif Parsed_Options (Sampling).Missing and not Parsed_Options (Frame_Rate).Missing then
            Sampling_Period := Parse_Time_Spec (Parsed_Options (Sampling).Value);

         else
            raise Bad_Command_Line
              with "sampling or framerate, but not both";
         end if;


         Sampling_Info :=
           Sampling_Spec'(Start           => Parse_Start_Time (To_String (Parsed_Options (Start_Time).Value)),
                          Stop            => Parse_Stop_Time (To_String (Parsed_Options (Stop_Time).Value)),
                          Sampling_Period => Sampling_Period);

      end Set_Sampling_Spec;
   begin
      if Help_Asked then
         raise Full_Help_Asked;
      end if;


      Requested_Verbosity := (if Is_A_Tty (2) then
                                 Interactive
                              else
                                 Logging);

      Parse_Options_And_Apply_Defaults;

      Set_Levels;

      Set_Decay;

      Set_Sampling_Spec;

      --  Ada.Text_IO.Put_Line (Camera_Events.Image (Sampling_Step));

      Frame_Filename_Spec := Parse_Output_Filename_Template (To_String (Parsed_Options (Output_Template).Value));

      if Parsed_Options (Input_Spec).Missing  then
         Input_Filename := To_Unbounded_String ("-");
      else
         Input_Filename := Parsed_Options (Input_Spec).Value;
      end if;

      pragma Assert (Input_Filename /= Null_Unbounded_String);

      Event_W := To_Pixel_Value (Parsed_Options (Event_Weigth).Value);

      Lazy_Decay_Requested := not Parsed_Options (Lazy_Decay).Missing;

      Rectify_Requested := not Parsed_Options (Rectify).Missing;


      I_Am_Ready := True;

   end Parse_Command_Line;

   -----------
   -- Input --
   -----------

   function Input return String
   is (To_String (Input_Filename));

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
   is (Frame_Filename_Spec.Frame_Format);

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


   function Verbosity_Level return Verbosity
   is (Requested_Verbosity);


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

   function Event_Contribution return Images.Pixel_Value
   is (Event_W);

   function Pixel_Min return Images.Pixel_Value
   is (Min_Level);

   function Pixel_Max return Images.Pixel_Value
   is (Max_Level);

   function Synchronous_Update return Boolean
   is (Lazy_Decay_Requested);

   function Reset_Each_Frame return Boolean
   is (Memory_Dynamic.Is_Reset (Memory_Dynamic_Spec));

   function Reset_Value return Images.Pixel_Value
   is (Memory_Dynamic.Reset_Value (Memory_Dynamic_Spec));

   function Rectify_Events return Boolean
   is (Rectify_Requested);
end Config;
