--
--   The user can specify the event filename in the format matching
--
--      (.+)(-|+)[0-9]+)?$
--
--   That is, any string (.*) optionally followed by an integer
--   with sign ((-|+)[0-9]+)
--
--   If the optional part is present, it is an offset that must
--   be added to the timestamp of all events; the remaining part is
--   the "true" filename.
--
--   This package provides function to extract the two parts of
--   the filename.
--
with DVAccum.Timestamps;

package Dvaccum.Split_Filename is
   function Offset_Of (Filename : String) return Timestamps.Duration
     with
       Pre => Filename /= "";

   function True_Filename (Filename : String) return String
     with
       Pre => Filename /= "";
end Dvaccum.Split_Filename;
