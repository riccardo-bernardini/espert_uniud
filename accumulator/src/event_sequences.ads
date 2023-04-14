with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Finalization;
with Ada.Iterator_Interfaces;

use Ada;

with Camera_Events;

package Event_Sequences is
   use type Camera_Events.Event_Type;

   type Metadata_Name is new String;
   type Metadata_Value is new String;

   type Metadata_Map is tagged private;

   function Has_Key (Map : Metadata_Map;
                     Key : Metadata_Name)
                     return Boolean;

   function Value_Of (Map : Metadata_Map;
                      Key : Metadata_Name) return Metadata_Value
     with
       Pre => Map.Has_Key (Key);

   function Size_X (Map : Metadata_Map) return Camera_Events.X_Coordinate_Type
     with
       Pre => Map.Has_Key ("sizeX") and then
       (for all C of Map.Value_Of ("sizeX") => C in'0' .. '9');

   function Size_Y (Map : Metadata_Map) return Camera_Events.Y_Coordinate_Type
     with
       Pre => Map.Has_Key ("sizeY") and then
       (for all C of Map.Value_Of ("sizeY") => C in'0' .. '9');


   function Value_Of (Map     : Metadata_Map;
                      Key     : Metadata_Name;
                      Default : Metadata_Value)
                      return Metadata_Value
     with
       Post => (if not Map.Has_Key (Key) then Value_Of'Result = Default);

   procedure Set (Map   : in out Metadata_Map;
                  Key   : Metadata_Name;
                  Value : Metadata_Value)
     with
       Post => Map.Has_Key (Key) and then Map.Value_Of (Key) = Value;

   procedure Wipe_Out (Map : in out Metadata_Map);

   function N_Entries (Map : Metadata_Map) return Natural;

   procedure Dump (Map : Metadata_Map);

   procedure Update (Map : in out Metadata_Map;
                     Src : in     Metadata_Map);

   procedure Iterate (Map : Metadata_Map;
                      Callback : access procedure (Name : Metadata_Name;
                                                   Value : Metadata_Value));

   package Event_Vectors is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Camera_Events.Event_Type);


   subtype Event_Sequence is Event_Vectors.List;

   function T_Min (Events : Event_Sequence) return Camera_Events.Timestamp;

   function T_Max (Events : Event_Sequence) return Camera_Events.Timestamp;

   function "<" (A, B : Camera_Events.Point_Type) return Boolean;

   --  package Point_Event_Maps is
   --    new Ada.Containers.Ordered_Maps (Key_Type     => Camera_Events.Point_Type,
   --                                     Element_Type => Event_Sequence);
   --
   --  subtype Point_Event_Map is Point_Event_Maps.Map;


   type Cursor is private;

   function Has_Element (Pos : Cursor) return Boolean;

   package Point_Event_Map_Interfaces is new
     Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Point_Event_Map (<>) is new Finalization.Limited_Controlled with private
     with
       Constant_Indexing => Events,
       Default_Iterator => Iterate;

   function Create (X_Size : Camera_Events.X_Coordinate_Type;
                    Y_Size : Camera_Events.Y_Coordinate_Type)
                    return Point_Event_Map;

   function Iterate (Container : in Point_Event_Map)
                     return Point_Event_Map_Interfaces.Forward_Iterator'Class;

   function Point (Position : Cursor) return Camera_Events.Point_Type;

   function Events (Map   : Point_Event_Map;
                    Point : Camera_Events.Point_Type)
                    return Event_Sequence;

   procedure Clear (Map : in out Point_Event_Map);

   procedure Collect_By_Point
     (Events         : Event_Sequence;
      Last_Timestamp : Camera_Events.Timestamp;
      Result         : out Point_Event_Map);

   -- Commented out, its role now is played by Collect_By_Point
   --  procedure Fill_Frame
   --    (Events_At : in out Point_Event_Map;
   --     Time      : Camera_Events.Timestamp;
   --     Size_X    : Camera_Events.X_Coordinate_Type;
   --     Size_Y    : Camera_Events.Y_Coordinate_Type);

private
   package Metadata_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Metadata_Name,
        Element_Type => Metadata_Value);

   type Metadata_Map is new Metadata_Maps.Map with null record;


   function N_Entries (Map : Metadata_Map) return Natural
   is (Natural (Map.Length));

   function Has_Key (Map : Metadata_Map;
                     Key : Metadata_Name)
                     return Boolean
   is (Map.Contains (Key));

   function T_Min (Events : Event_Sequence) return Camera_Events.Timestamp
   is (Camera_Events.T (Events.First_Element));

   function T_Max (Events : Event_Sequence) return Camera_Events.Timestamp
   is (Camera_Events.T (Events.Last_Element));

   procedure Append (Map   : in out Point_Event_Map;
                     Event : Camera_Events.Event_Type);



   type Event_Matrix is array (Camera_Events.X_Coordinate_Type range <>,
                               Camera_Events.Y_Coordinate_Type range <>)
     of Event_Vectors.List;

   type Event_Matrix_Access is access Event_Matrix;

   type Point_Event_Map is new Finalization.Limited_Controlled with
      record
         M : Event_Matrix_Access;
      end record;

   type Cursor is
      record
         M : Event_Matrix_Access;
         X : Camera_Events.X_Coordinate_Type;
         Y : Camera_Events.Y_Coordinate_Type;
      end record;

   function Next (C : Cursor) return Cursor;

   type Point_Map_Iterator is
     new Point_Event_Map_Interfaces.Forward_Iterator
   with
      record
         C : Cursor;
      end record;

   function First (Object : Point_Map_Iterator) return Cursor;
   function Next (Object : Point_Map_Iterator; Position : Cursor) return Cursor;

end Event_Sequences;
