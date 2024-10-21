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


   ------------
   -- Append --
   ------------

   procedure Append (To : in out Vector; What : String)
   is
   begin
      Append (To, To_Unbounded_String (What));
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


   ----------
   -- Join --
   ----------

   function Join (V : Vector; Glue : String) return String
   is
      Result : Unbounded_String;
   begin
      for I in First_Index (V) .. Last_Index (V) loop
         Result := Result & Element (V, I);

         if I < Last_Index (V) then
            Result := Result & Glue;
         end if;
      end loop;

      return To_String (Result);
   end Join;


end String_Vectors;
