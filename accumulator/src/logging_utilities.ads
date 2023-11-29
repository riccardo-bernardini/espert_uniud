with Times;
with String_Vectors;

with Ada.Direct_IO;
with Interfaces.C;  use Interfaces;

with Config;
with ada.Finalization;

-----------------------
-- Logging_Utilities --
-----------------------

package Logging_Utilities is
   type Log_Progress_File is
     new Ada.Finalization.Limited_Controlled
   with
     private;

   procedure Open (File : in out Log_Progress_File;
                   Name : String)
     with
       Pre => not Is_Open (File),
       Post => Is_Open (File);

   procedure Close (File : in out Log_Progress_File)
     with
       Pre => Is_Open (File),
       Post => not Is_Open (File);

   function Is_Open (File : Log_Progress_File) return Boolean;


   procedure Log_Progress (Target       : in out Log_Progress_File;
                           Start_Time   : Times.Timestamp;
                           Stop_Time    : Times.Timestamp;
                           Current_Time : Times.Timestamp)
     with
       Pre => Is_Open (Target);

   function Terminal_Width return C.Int
     with
       Import => True,
       Convention => C,
       External_Name => "terminal_width";

   procedure Put_Line_Maybe (Verbose : Boolean; Text    : String);
   procedure Put_Maybe (Verbose : Boolean; Text    : String);

   procedure Show_Progress_Bar (Start_Time   : Times.Timestamp;
                                Stop_Time    : Times.Timestamp;
                                Current_Time : Times.Timestamp);

   function Get_Metadata_Text return String_Vectors.Vector
     with
       Pre => Config.Package_Ready;

   procedure Dump_Metadata (Destination_Filename : String)
     with
       Pre => Destination_Filename /= "" and Config.Package_Ready;
private
   package Byte_Io is new Ada.Direct_IO (Unsigned_8);

   type Log_Progress_File is
     new Ada.Finalization.Limited_Controlled
       with
      record
         F : Byte_Io.File_Type;
      end record;

   overriding procedure Finalize (Obj : in out Log_Progress_File);

end Logging_Utilities;
