with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Iterator_Interfaces;

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

   procedure Dump (Map : Metadata_Map);

   procedure Update (Map : in out Metadata_Map;
                     Src : in     Metadata_Map);

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

   type Point_Event_Map is tagged private
     with
       Constant_Indexing => Events,
       Default_Iterator => Iterate;

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

private
   package Metadata_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Metadata_Name,
        Element_Type => Metadata_Value);

   type Metadata_Map is new Metadata_Maps.Map with null record;

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


   use type Event_Vectors.List;

   package Point_Maps is
     new Ada.Containers.Ordered_Maps (Key_Type     => Camera_Events.Point_Type,
                                      Element_Type => Event_Sequence);

   type Point_Event_Map is tagged
      record
         M : Point_Maps.Map;
      end record;

   type Cursor is
      record
         C : Point_Maps.Cursor;
      end record;

   type Point_Map_Iterator is
     new Point_Event_Map_Interfaces.Forward_Iterator
   with
      record
         C : Point_Maps.Cursor;
      end record;

   function First (Object : Point_Map_Iterator) return Cursor;
   function Next (Object : Point_Map_Iterator; Position : Cursor) return Cursor;

end Event_Sequences;
