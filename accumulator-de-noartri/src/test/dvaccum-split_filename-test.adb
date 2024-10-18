with Ada.Text_Io;            use Ada.Text_Io;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with DVAccum.Timestamps;

procedure Dvaccum.Split_Filename.Test is
   function "+" (X : String) return Unbounded_String
                 renames To_Unbounded_String;

   type Case_Array is
     array (Positive range <>) of Unbounded_String;

   Casi : constant Case_Array :=
            (+"/tmp/gigi.csv",
             +"/home/gino/acquisition-20241011.csv",
             +"/home/gino/acquisition-20241011.csv-42",
             +"/home/gino/acquisition-20241011.csv+1999",
             +"/home/gino/20241011");
begin
   for X of Casi loop
      declare
         S : constant String := To_String (X);
      begin
         Put_Line ("'" & S & "', "
                   & "'" & True_Filename (S) & "', "
                   & Timestamps.Image (Offest_Of (S)));
      end;
   end loop;
end Dvaccum.Split_Filename.Test;
