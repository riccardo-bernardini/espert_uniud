with Ada.Text_IO; use Ada.Text_IO;
procedure Time_Syntax.Test is
   procedure Prova (X : String) is
      Success  : Boolean;
      Relative : Boolean;
      Value    : Time_In_Microseconds;
   begin
      Put_Line ("-----------");
      Put_Line ("Parsing """ & X & """");
      Parse_Time (Input    => X,
                  Success  => Success,
                  Relative => Relative,
                  Value    => Value);

      Put_Line ("Success  : " & (if Success then "Yes" else "No"));

      if not Success then
         return;
      end if;

      Put_Line ("Relative : " & (if Relative then "Yes" else "No"));
      Put_Line ("Value    : " & Value'Image);
   end Prova;
begin
   Prova ("112");
   Prova ("112.0e-3ms");
   Prova ("@+112");
   Prova ("@+1.12s");
   Prova ("1ms");
   Prova ("1.0ms");
   Prova ("112.0");
end Time_Syntax.Test;
