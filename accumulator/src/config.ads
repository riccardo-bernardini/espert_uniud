with Camera_Events;
with Times;
with Memory_Dynamic;
with Images;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Config with SPARK_Mode is
   type Frame_Index is range 0 .. Integer'Last;

   type Verbosity is
     (
      Quiet,        -- no verbose at all
      Logging,      -- Stuff that can go to a file
      Interactive   -- Stuff that makes sense only on a terminal (e.g., progress bar)
     );

   -- Used in contracts
   function Package_Ready return Boolean
     with Ghost;

   function T0_Fixed return Boolean
     with
       Ghost;

   type Parsing_Status is (Success, Full_Help_Asked, Bad_Command_Line);

   type Parsing_Report is
      record
         Status  : Parsing_Status;
         Message : Unbounded_String;
      end record;

   procedure Parse_Command_Line (Report : out Parsing_Report)
     with
       Pre => not Package_Ready,
       Post => (Report.Status = Success) = Package_Ready;


   function Short_Help_Text return String;

   function Long_Help_Text return String;


   function Frame_Filename (N : Frame_Index) return String
     with
       Pre => Package_Ready;

   function Output_Format return Images.Format_Type
     with
       Pre => Package_Ready;

   function Input return String
     with
       Pre => Package_Ready;

   function Sampling_Period return Times.Duration
     with
       Pre => Package_Ready;

   function Start_At  return Times.Timestamp
     with
       Pre => Package_Ready and then T0_Fixed;

   function Stop_At  return Times.Timestamp
     with
       Pre => Package_Ready and then T0_Fixed;


   function Forgetting_Method return  Memory_Dynamic.Dynamic_Type
     with
       Pre => Package_Ready;


   function Start_Image (Size_X : Camera_Events.X_Coordinate_Type;
                         Size_Y : Camera_Events.Y_Coordinate_Type)
                         return Images.Image_Type
     with
       Pre => Package_Ready;

   function Event_Contribution return Images.Pixel_Value
     with
       Pre => Package_Ready;

   function Pixel_Min return Images.Pixel_Value
     with
       Pre => Package_Ready;

   function Pixel_Max return Images.Pixel_Value
     with
       Pre => Package_Ready;

   function Synchronous_Update return Boolean
     with
       Pre => Package_Ready;

   function Reset_Each_Frame return Boolean
     with
       Pre => Package_Ready;

   function Neutral_Value return Images.Pixel_Value
     with
       Pre => Package_Ready;


   function Rectify_Events return Boolean
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

   procedure Fix_T0 (T0 : Times.Timestamp)
     with
       Pre => Package_Ready and not T0_Fixed,
     Post => T0_Fixed;

   function Log_Progress return Boolean
     with
       Pre => Package_Ready;

   function Log_Progress_Filename return String
     with
       Pre => Package_Ready and then Log_Progress;
end Config;
