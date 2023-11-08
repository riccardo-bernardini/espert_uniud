package body Simple_Tables is
   function Contains (Item : Map; Key : Key_Type) return Boolean
   is (Find (Item, key) /= No_Element);

   function Size (Item : Map) return Natural
   is (Positive(Item.Next_Free) - 1);

   function New_Map (Capacity : Positive) return Map
   is (Map'(Last_Index => Cursor (Capacity), Data => <>, Next_Free => Valid_Cursor'First));


   function Capacity (Item : Map) return Positive
   is (Positive (Item.Last_Index));

   ------------
   -- Insert --
   ------------

   procedure Insert (Item     : in out Map;
                     Key      : Key_Type;
                     New_Item : Element_Type)
   is
   begin
      if Find (Item, Key) /= No_Element then
         raise Constraint_Error;
      end if;

      if Size (Item) = Capacity (Item) then
         raise Constraint_Error;
      end if;

      Item.data (Item.Next_Free) := Map_Entry'(Key, New_Item);
   end Insert;

   ----------
   -- Find --
   ----------

   function Find (Where : Map;
                  What  : Key_Type)
                  return Cursor
   is
   begin
      for I in Where.Data'First .. Where.Next_Free - 1 loop
         if Where.Data (I).Key = What then
            return Cursor (I);
         end if;
      end loop;

      return No_Element;
   end Find;

   -------------
   -- Element --
   -------------

   function Element (Item : Map;
                     Pos  : Cursor)
                     return Element_Type
   is
   begin
      if Pos = No_Element then
         raise Constraint_Error;
      end if;

      return Item.Data (Pos).Element;
   end Element;

end Simple_Tables;
