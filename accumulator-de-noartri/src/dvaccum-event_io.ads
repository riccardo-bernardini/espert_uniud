with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Text_IO;

with DVAccum.Events;
with DVAccum.Timestamps;

package DVAccum.Event_Io is
   use type DVAccum.Events.Event_Type;

   package Event_Sequences is
     new Ada.Containers.Ordered_Sets
     (Element_Type => Events.Event_Type,
      "<"          => Events.Less_Then_By_Pixel);

   procedure Dump (What : Event_Sequences.Set;
                   Where : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Error);

   type Sequence_Metadata is private;

   function First_Time (Metadata : Sequence_Metadata)
     return Timestamps.Timestamp;

   function Last_Time (Metadata : Sequence_Metadata)
     return Timestamps.Timestamp;

   procedure Read_Events
     (Filename : in     String;
      Events   :    out Event_Sequences.Set;
      Metadata :    out Sequence_Metadata;
      Rectify  : in     Boolean);

   Bad_Event_Stream : exception;

private
   type Metadata_Key is new Unbounded_String;

   type Metadata_Value is new Unbounded_String;

   package Metadata_Maps is
      new Ada.Containers.Ordered_Maps
     (Key_Type     => Metadata_key,
      Element_Type => Metadata_Value);

   type Sequence_Metadata is
      record
         Min_Timestamp : Timestamps.Timestamp;
         Max_Timestamp : Timestamps.Timestamp;
         N_Rows        : Positive;
         N_Cols        : Positive;
         Map           : Metadata_Maps.Map;
      end record;

   function First_Time (Metadata : Sequence_Metadata)
                        return Timestamps.Timestamp
   is (Metadata.Min_Timestamp);

   function Last_Time (Metadata : Sequence_Metadata)
                       return Timestamps.Timestamp
   is (Metadata.Max_Timestamp);

end DVAccum.Event_Io;
