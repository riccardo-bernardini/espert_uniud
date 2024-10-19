with ada.text_io;  use ada.text_io;

procedure String_Formatting.Test is
   Q : constant Parsed_Format :=
         Parse_Format ("gigi puro=%d 03=[%03d] +=[%+d] -=[%-5d] +5=[%+5d] 5=[%5d] %01f paperino");

   type Zz is new Provider_Interface  with null record;

   function Provide
     (Provider  : ZZ;
      Directive : Character;
      Parameter : String;
      Counter   : Positive)
      return String;

   function Provide
     (Provider  : ZZ;
      Directive : Character;
      Parameter : String;
      Counter   : Positive)
               return String
   is
   begin
      if Directive = 'd' then
         return C_Style_Formatting (Datum     => 42,
                                    Parameter => parameter);
      else
         return "Z";
      end if;
   end Provide;

   R : zz;
begin
   Dump (Q);
   Put_Line ("--------");
   Put_Line (Expand (Q, R));
end String_Formatting.Test;
