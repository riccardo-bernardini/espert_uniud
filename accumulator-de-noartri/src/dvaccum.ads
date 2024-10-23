package DVAccum is
type Sample_Value is new Float;

   type Coord_X is mod 2 ** 16;
   type Coord_Y is mod 2 ** 16;

   type Point_Type is
      record
         X : Coord_X;
         Y : Coord_Y;
      end record;

   type Frame_Index is range -1 .. Integer'Last;

   subtype Valid_Frame_Index is
     Frame_Index range Frame_Index'First + 1 .. Frame_Index'Last;

   No_Frame : constant Frame_Index := Frame_Index'First;


--  private
--     type Frame_Index ;
--
--     No_Frame : constant Frame_Index := Frame_Index'First;
--
--     First_Frame_Index : constant Frame_Index := Valid_Frame_Index'First;
--
--     function To_Int (X : Frame_Index) return Natural
--     is (Natural (X));
--
--
--     function "<" (L, R : Frame_Index) return Boolean
--     is ((L /= No_Frame and R /= No_Frame and Integer (L) < Integer (R)));
--
--     function "<=" (L, R : Frame_Index) return Boolean
--     is ((L /= No_Frame and R /= No_Frame and Integer (L) <= Integer (R)));
--
--
--     function ">" (L, R : Frame_Index) return Boolean
--     is ((L /= No_Frame and R /= No_Frame and Integer (L) > Integer (R)));
--
--     function ">=" (L, R : Frame_Index) return Boolean
--     is ((L /= No_Frame and R /= No_Frame and Integer (L) >= Integer (R)));

end DVAccum;
