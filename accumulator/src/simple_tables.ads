generic
   type Key_Type is private;
   type Element_Type is private;

   with function"=" (X, Y : Key_Type) return Boolean is <>;

   with function"<" (X, Y : Key_Type) return Boolean is <>;
package Simple_Tables is
   type Map (<>) is private;

   function New_Map (Capacity : Positive) return Map;

   type Cursor is private;

   No_Element : constant Cursor;

   function Contains (Item : Map; Key : Key_Type) return Boolean;

   function Size (Item : Map) return Natural;

   function Capacity (Item : Map) return Positive;

   procedure Insert (Item     : in out Map;
                     Key      : Key_Type;
                     New_Item : Element_Type)
     with
       Pre => Size (Item) < Capacity (Item) and Find (Item, Key) = No_Element,
     Post => Size (Item) = Size (Item)'Old + 1;

   function Find (Where : Map;
                  What  : Key_Type)
                  return Cursor;

   function Element (Item : Map;
                     Pos  : Cursor)
                     return Element_Type
     with
       Pre => Pos /= No_Element;

   procedure Dump (Item : Map; Image : access function (K : Key_Type; E : Element_Type) return String);
private
   type Map_Entry is
      record
         Key     : Key_Type;
         Element : Element_Type;
      end record;

   type Cursor is new Natural;

   No_Element : constant Cursor := 0;

   subtype Valid_Cursor is Cursor range 1 .. Cursor'Last;

   type Entry_Array is array (Valid_Cursor range <>) of Map_Entry;

   type Map (Last_Index : Valid_Cursor) is
      record
         Data : Entry_Array (Valid_Cursor'First .. Last_Index);
         Next_Free : Valid_Cursor;
      end record;




end Simple_Tables;
