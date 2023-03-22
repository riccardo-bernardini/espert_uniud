with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;

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
                     src : in     Metadata_Map);

   package Event_Vectors is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => Camera_Events.Event_Type);


   subtype Event_Sequence is Event_Vectors.List;

   function "<" (A, B : Camera_Events.Point_Type) return Boolean;

   --  package Point_Event_Maps is
   --    new Ada.Containers.Ordered_Maps (Key_Type     => Camera_Events.Point_Type,
   --                                     Element_Type => Event_Sequence);
   --
   --  subtype Point_Event_Map is Point_Event_Maps.Map;

   type Point_Event_Matrix is
     array (Camera_Events.X_Coordinate_Type range <>,
            Camera_Events.Y_Coordinate_Type range <>)
     of Event_Sequence;

   type Point_Event_Map is access Point_Event_Matrix;

   function Collect_By_Point (Events         : Event_Sequence;
                              Last_Timestamp : Camera_Events.Timestamp)
                              return Point_Event_Map;

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
end Event_Sequences;
