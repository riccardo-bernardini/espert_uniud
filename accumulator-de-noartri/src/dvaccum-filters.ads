with Ada.Containers.Indefinite_Vectors;

with DVAccum.Frames;

--
--  This package implements filter to be used to accumulate the events.  Every
--  filter is the sum of smaller FIR/IIR filters, in other words, the filters
--  are implemented in the parallel form.
--
--  First order proper filters (that is, of the form C/(1-p z^-1)) can have
--  the pole expressed as time constant in seconds using the special '@' form (see
--  syntax below). The time constant τ  is converted to a "discrete time
--  pole" using p = exp(-T/τ), where T is the sampling time.
--
--  The syntax for a textual description of a filter is as follows
--
--     filter = branch ('+' branch)*
--     branch = (poly ('/' poly)?) | (number? '#' number unit)
--     poly   = (number)+
--     unit   = ('u'|'m')'s'
--     number = <floating point number>
--
--  Note that
--  * Although syntactically units can be used everywhere, it is lecit to use
--    them only when used with proper branchs of degree 1.
--  * We follow the Octave/Matlab convention: the given coefficients are the
--    coefficients of the transfer function, not of the difference equation
--    this means that
--    - The denominator always begin with '1'
--    - The denominator coefficients are the opposite of the difference
--      equation coefficients
--    - The first coefficient is relative to z^0, the second to z^-1, ...
--
--  For example,
--
--             1 0.5 / 1 -0.1 0.2
--
--  is a filter with transfer function
--
--                  1 + 0.5 z^-1
--           ---------------------------
--             1 - 0.1 z^-1 + 0.2 z^-2
package Dvaccum.Filters is
   type Filter_Type is private;

   type Signal is
     array (Integer range <>) of Frames.Pixel_Value;

   function Parse (Descr    : String;
                   Sampling : Float) return Filter_Type;

   procedure Reset (Filter : in out Filter_Type);

   procedure Process (Filter : in out Filter_Type;
                      Output :    out Frames.Pixel_Value;
                      Input  :        Frames.Pixel_Value);


   function Apply (Filter : in out Filter_Type;
                   Input  : Signal)
                   return Signal
     with
       Post =>
         Input'First = Apply'Result'First
         and Input'Last = Apply'Result'Last;
private
   type Filter_Atom (Num_Degree   : Natural;
                     Den_Degree   : Natural;
                     Status_Size  : Positive) is
      record
         Num    : Signal (0 .. Num_Degree);
         Den    : Signal (0 .. Den_Degree);
         Status : Signal (1 .. Status_Size) := (others => 0.0);
      end record;

   package Atom_Vectors is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                            Element_Type => Filter_Atom);
   type Filter_Type is
      record
         Atoms : Atom_Vectors.Vector;
      end record;
   end Dvaccum.Filters;
