package body String_Vectors is
   function Size (V : Vector) return Natural
   is (V.Next_Free - 1);

   function Capacity (V : Vector) return Positive
   is (V.Capacity);

   ------------
   -- Append --
   ------------

   procedure Append (To : in out Vector; What : Unbounded_String)
   is
   begin
      if Size (To) = Capacity (To) then
         raise Constraint_Error;
      end if;

      To.Elements (To.Next_Free) := What;

      To.Next_Free := To.Next_Free + 1;
   end Append;


   -----------
   -- Clear --
   -----------

   procedure Clear (Item : out Vector)
   is
   begin
      Item.Next_Free := Item.Elements'First;
   end Clear;
   -------------------
   -- First_Element --
   -------------------

   function First_Element (V : Vector) return Unbounded_String
   is
   begin
      if Size (V) = 0 then
         raise Constraint_Error;
      end if;

      return V.Elements (V.Elements'First);
   end First_Element;

   -----------------
   -- First_Index --
   -----------------

   function First_Index (V : Vector) return Positive
   is (V.Elements'First);

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index (V : Vector) return Natural
   is (V.Next_Free - 1);

   -------------
   -- Element --
   -------------

   function Element (V : Vector; Index : Positive) return Unbounded_String
   is
   begin
      if Index > Size (V) then
         raise Constraint_Error;
      end if;

      return V.Elements (Index);
   end Element;

end String_Vectors;
