pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
package body Profiling is

   --------------
   -- Entering --
   --------------

   procedure Entering (Prof : in out Profiler_Type; Section : Section_Name) is
   begin
      Prof.Timestamps.Append (Entering_Event'(Where   => Section,
                                              At_Time => Ada.Calendar.Clock));
   end Entering;

   -----------
   -- Close --
   -----------

   procedure Close
     (Prof : in out Profiler_Type; Cumulative_Times : out Profile_Array)
   is
      use Ada.Calendar;

      Current_Event : Entering_Event;
   begin
      Prof.Entering (Section_Name'First);

      Cumulative_Times := (others => 0.0);

      if Prof.Timestamps.Is_Empty then
         return;
      end if;

      Current_Event := Prof.Timestamps.First_Element;

      for Event of Prof.Timestamps loop
         Cumulative_Times (Current_Event.Where) :=
           Cumulative_Times (Current_Event.Where)
           + (Event.At_Time - Current_Event.At_Time);

         Current_Event := Event;
      end loop;
   end Close;


   procedure Dump (Prof : in out Profiler_Type)
   is
      Data : Profile_Array;
   begin
      Prof.Close (Data);

      for Name in Section_Name loop
         Put_Line (Name'Image & Data (Name)'Image);
      end loop;
   end Dump;

end Profiling;
