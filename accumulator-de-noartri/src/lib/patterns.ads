package patterns is
   function Strip_Spaces (S : String) return String;

   function Is_Float (X : String) return Boolean;

   function Is_Integer (X : String) return Boolean;

   function Get_Extension (Filename : String) return String;

   function Chomp (S : String) return String;

end Patterns;
