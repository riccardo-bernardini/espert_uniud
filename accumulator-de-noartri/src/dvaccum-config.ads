with DVAccum.Frames;
with DVAccum.Timestamps;

with System.Multiprocessors;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

use DVAccum;

package Config with SPARK_Mode is
   -- Used in contracts
   function Package_Ready return Boolean
     with Ghost;

   function T0_Fixed return Boolean
     with
       Ghost;

   procedure Dump_Cli;

   type Parsing_Status is (Success, Full_Help_Asked, Bad_Command_Line);

   type Parsing_Report is
      record
         Status  : Parsing_Status;
         Message : Unbounded_String;
      end record;

   type Frame_Index is range 0 .. Integer'Last;

   type Negative_Event_Action is
     (
      Keep,
      Rectify,
      Clip
     );

   type Verbosity is
     (
      Quiet,        -- no verbose at all
      Logging,      -- Stuff that can go to a file
      Interactive   -- Stuff that makes sense only on a terminal (e.g., progress bar)
     );



   procedure Parse_Command_Line (Report : out Parsing_Report)
     with
       Pre => not Package_Ready,
       Post => (Report.Status = Success) = Package_Ready;


   function Short_Help_Text return String;

   function Long_Help_Text return String;


   function Frame_Filename (N : Frame_Index) return String
     with
       Pre => Package_Ready;

   function Output_Format return DVAccum.Frames.Format_Type
     with
       Pre => Package_Ready;

   function Input return String
     with
       Pre => Package_Ready;

   function Sampling_Period return Timestamps.Duration
     with
       Pre => Package_Ready;

   function Start_At  return Timestamps.Timestamp
     with
       Pre => Package_Ready and then T0_Fixed;

   function Stop_At  return Timestamps.Timestamp
     with
       Pre => Package_Ready and then T0_Fixed;


   function Start_Image (Size_X : Frames.X_Coordinate_Type;
                         Size_Y : Frames.Y_Coordinate_Type)
                         return Frames.Image_Type
     with
       Pre => Package_Ready;

   function Event_Contribution return Frames.Pixel_Value
     with
       Pre => Package_Ready;

   function Pixel_Min return Frames.Pixel_Value
     with
       Pre => Package_Ready;

   function Pixel_Max return Frames.Pixel_Value
     with
       Pre => Package_Ready;

   function Synchronous_Update return Boolean
     with
       Pre => Package_Ready;

   function Reset_Each_Frame return Boolean
     with
       Pre => Package_Ready;

   function On_Negative_Event return Negative_Event_Action
     with
       Pre => Package_Ready;


   function Verbosity_Level return Verbosity
     with
       Pre => Package_Ready;

   function Verbose return Boolean
   is (Verbosity_Level > Quiet)
     with
       Pre => Package_Ready;

   function Show_Progress_Bar return Boolean
   is (Verbosity_Level >= Interactive)
     with
       Pre => Package_Ready;

   function Metadata_Requested return Boolean
     with
       Pre => Package_Ready;

   function Metadata_Filename return String
     with
       Pre => Package_Ready and then Metadata_Requested;

   procedure Fix_T0 (T0 : Timestamps.Timestamp)
     with
       Pre => Package_Ready and not T0_Fixed,
     Post => T0_Fixed;

   function Log_Progress return Boolean
     with
       Pre => Package_Ready;

   function Log_Progress_Filename return String
     with
       Pre => Package_Ready and then Log_Progress;

   function Number_Of_Parallel_Tasks return System.Multiprocessors.CPU;

end Config;
