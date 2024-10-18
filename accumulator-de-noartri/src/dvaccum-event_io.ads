with Ada.Iterator_Interfaces;

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Text_IO;

with DVAccum.Events;
with DVAccum.Timestamps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package DVAccum.Event_Io is
   use DVAccum.Events;

   type Event_Sequence is tagged private;

   procedure Dump (What  : Event_Sequence;
                   Where : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Error);

   function First_Time (Sequence : Event_Sequence)
                        return Timestamps.Timestamp;

   function Last_Time (Sequence : Event_Sequence)
                       return Timestamps.Timestamp;

   function Length (Sequence : Event_Sequence)
                    return Ada.Containers.Count_Type;

   function Is_Empty (Sequence : Event_Sequence)
                      return Boolean;

   function N_Rows (Sequence : Event_Sequence)
                    return Positive;

   function N_Cols (Sequence : Event_Sequence)
                    return Positive;
   type Event_Cursor is private;

   function Has_Element (Item : Event_Cursor) return Boolean;

   function Element (Item : Event_Cursor) return Event_Type
     with
       Pre => Has_Element(Item);

   package Event_Sequence_Iterators is
     new Ada.Iterator_Interfaces (Event_Cursor, Has_Element);


   function All_Events
     (Item : Event_Sequence)
      return Event_Sequence_Iterators.Forward_Iterator'Class;

   function Source_File_Name (Item : Event_Sequence) return String;

   --  procedure Read_Events
   --    (Filename          : in     String;
   --     Events            :    out Event_Sequences.Set;
   --     Metadata          :    out Sequence_Metadata;
   --     On_Positive_Event : in     Event_Weight;
   --     On_Negative_Event : in     Event_Weight;
   --     Offset            : in     Timestamps.Duration);

   procedure Read_CSV_Event_Stream
     (Input                  : in     Ada.Text_IO.File_Type;
      Events                 : in out Event_Sequence;
      On_Positive_Event      : in     Event_Weight;
      On_Negative_Event      : in     Event_Weight;
      Offset                 : in     Timestamps.Duration);

   Bad_Event_Stream : exception;

private

   package Event_Sequences is
     new Ada.Containers.Ordered_Sets
       (Element_Type => Events.Event_Type,
        "<"          => Events.Less_Then_By_Pixel);

   type Metadata_Key is new String;

   type Metadata_Value is new String;

   package Metadata_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Metadata_Key,
        Element_Type => Metadata_Value);

   type Sequence_Metadata is
      record
         Min_Timestamp : Timestamps.Timestamp;
         Max_Timestamp : Timestamps.Timestamp;
         N_Rows        : Positive;
         N_Cols        : Positive;
         Map           : Metadata_Maps.Map;
         Source        : Unbounded_String;
      end record;

   function First_Time (Metadata : Sequence_Metadata)
                        return Timestamps.Timestamp
   is (Metadata.Min_Timestamp);

   function Last_Time (Metadata : Sequence_Metadata)
                       return Timestamps.Timestamp
   is (Metadata.Max_Timestamp);


   type Event_Sequence is
     tagged
      record
         Events : Event_Sequences.Set;
         Meta   : Sequence_Metadata;
      end record;

   type Event_Cursor is
      record
         Cursor : Event_Sequences.Cursor;
      end record;

   function Has_Element (Item : Event_Cursor) return Boolean
   is (Event_Sequences.Has_Element (Item.Cursor));

   function Length (Sequence : Event_Sequence)
                    return Ada.Containers.Count_Type
   is (Sequence.Events.Length);

   type Event_Iterator is
     new Event_Sequence_Iterators.Forward_Iterator
   with
      record
         First : Event_Cursor;
      end record;

   overriding function First (Item : Event_Iterator) return Event_Cursor;

   overriding function Next (Item   : Event_Iterator;
                             Cursor : Event_Cursor)
                             return Event_Cursor;

   function Element (Item : Event_Cursor) return Event_Type
   is (Event_Sequences.Element (Item.Cursor));

   function N_Rows (Sequence : Event_Sequence) return Positive
   is (Sequence.Meta.N_Rows);

   function N_Cols (Sequence : Event_Sequence) return Positive
   is (Sequence.Meta.N_Cols);

end DVAccum.Event_Io;
