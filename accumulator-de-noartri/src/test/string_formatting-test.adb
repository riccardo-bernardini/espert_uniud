with ada.text_io;  use ada.text_io;

procedure String_Formatting.Test is
   Q : constant Parsed_Format :=
         Parse_Format ("gigi %02d %s + %01f paperino");

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
      return "Z";
   end Provide;

   R : zz;
begin
   Dump (Q);
   Put_Line (Expand (Q, R));
end String_Formatting.Test;
