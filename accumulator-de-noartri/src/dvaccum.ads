package DVAccum is
type Sample_Value is new Float;

   type Coord_X is mod 2 ** 16;
   type Coord_Y is mod 2 ** 16;

   type Point_Type is
      record
         X : Coord_X;
         Y : Coord_Y;
      end record;

   type Frame_Index is private;

   No_Frame : constant Frame_Index;

   function To_Int (X : Frame_Index) return Natural
     with
       Pre => X /= No_Frame;

private
   type Frame_Index is range -1 .. Integer'Last;

   subtype Valid_Frame_Index is
     Frame_Index range Frame_Index'First + 1 .. Frame_Index'Last;

   No_Frame : constant Frame_Index := Frame_Index'First;

   function To_Int (X : Frame_Index) return Natural
   is (Natural (X));

end DVAccum;
