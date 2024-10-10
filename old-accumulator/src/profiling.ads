with Ada.Containers.Doubly_Linked_Lists;
with Ada.Calendar;

generic
   type Section_Name is (<>);
package Profiling is
   type Profiler_Type is tagged private;

   procedure Entering (Prof    : in out Profiler_Type;
                       Section : Section_Name);

   type Profile_Array is array (Section_Name) of Duration;

   procedure Close (Prof             : in out Profiler_Type;
                    Cumulative_Times : out Profile_Array);

   procedure Dump (Prof : in out Profiler_Type);
private
   type Entering_Event is
      record
         Where   : Section_Name;
         At_Time : Ada.Calendar.Time;
      end record;

   package Event_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Entering_Event);

   type Profiler_Type is tagged
      record
         Timestamps : Event_Lists.List;
      end record;

end Profiling;
