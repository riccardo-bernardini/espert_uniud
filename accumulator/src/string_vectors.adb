package body String_Vectors is
   function Size (V : Vector) return Natural
   is (V.Next_Free - 1);

   function Capacity (V : Vector) return Positive
   is (V.Capacity);

   procedure Append (To : in out Vector; What : Unbounded_String)
   is
   begin
      if Size (To) = Capacity (To) then
         raise Constraint_Error;
      end if;

      To.Elements (To.Next_Free) := What;

      To.Next_Free := To.Next_Free + 1;
   end Append;

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
   is
   begin
      if Size (V) = 0 then
         raise Constraint_Error;
      end if;

      return V.Elements'First;
   end First_Index;

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index (V : Vector) return Positive
   is
   begin
      if Size (V) = 0 then
         raise Constraint_Error;
      end if;

      return V.Next_Free-1;
   end Last_Index;

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
