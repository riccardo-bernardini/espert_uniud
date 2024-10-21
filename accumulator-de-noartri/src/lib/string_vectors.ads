with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
package String_Vectors is
   type Vector (Capacity : Positive) is private;

   function Size (V : Vector) return Natural;

   function Capacity (V : Vector) return Positive;

   procedure Clear (Item : out Vector)
     with
       Post => Size (Item) = 0;

   procedure Append (To : in out Vector; What : Unbounded_String)
     with
       Pre => Size (To) < Capacity (To),
       Post => Size (To) = Size (To)'Old + 1;

   procedure Append (To : in out Vector; What : String)
     with
       Pre => Size (To) < Capacity (To),
       Post => Size (To) = Size (To)'Old + 1;

   function First_Element (V : Vector) return Unbounded_String
     with
       Pre => Size (V) > 0;

   function First_Index (V : Vector) return Positive;

   function Last_Index (V : Vector) return Natural;

   function Element (V : Vector; Index : Positive) return Unbounded_String
     with
       Pre => Index >= First_Index (V) and Index <= Last_Index (V);

   function Join (V : Vector; Glue : String) return String;

private
   type String_Array is array (Positive range <>) of Unbounded_String;

   type Vector (Capacity : Positive) is
      record
         Elements  : String_Array (1 .. Capacity) := (others => Null_Unbounded_String);
         Next_Free : Positive := 1;
      end record
     with
       Type_Invariant => Next_Free <= Capacity + 1;

end String_Vectors;
