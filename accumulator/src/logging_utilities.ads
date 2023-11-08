with Camera_Events;
with String_Vectors;

with Interfaces.C;  use Interfaces;

with Config;

package Logging_Utilities is
    function Terminal_Width return C.Int
     with
       Import => True,
       Convention => C,
       External_Name => "terminal_width";

   procedure Put_Line_Maybe (Verbose : Boolean; Text    : String);
   procedure Put_Maybe (Verbose : Boolean; Text    : String);

   procedure Show_Progress_Bar (Start_Time   : Camera_Events.Timestamp;
                                Stop_Time    : Camera_Events.Timestamp;
                                Current_Time : Camera_Events.Timestamp);

   function Get_Metadata_Text return String_Vectors.Vector
     with
       Pre => Config.Package_Ready;

   procedure Dump_Metadata (Destination_Filename : String)
     with
       Pre => Destination_Filename /= "" and Config.Package_Ready;
end Logging_Utilities;
