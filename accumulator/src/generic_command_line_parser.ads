with Ada.Strings.Unbounded;               use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;

generic
   type Options is (<>);
package  Generic_Command_Line_Parser is
   type Option_Names is array (Options) of Unbounded_String;

   type Option_Flags is array (Options) of Boolean;



   All_No : constant Option_Flags := (others => False);
   All_Yes : constant Option_Flags := (others => True);

   type Repeated_Option_Action is (Concatenate, Overwrite, Die, Ignore);

   type When_Repeated_Do is array (Options) of Repeated_Option_Action;

   Always_Die : constant When_Repeated_Do := (others => Die);

   type Option_Value (Missing : Boolean := False) is
      record
         case Missing is
            when True =>
               null;

            when False =>
               Value : Unbounded_String;
         end case;
      end record;

   type Option_Values is array (Options) of Option_Value;

   function Parse (Source                  : String;
                   Names                   : Option_Names;
                   Mandatory               : Option_Flags := All_No;
                   When_Repeated           : When_Repeated_Do := Always_Die;
                   Option_Value_Separator  : Character := ':';
                   Include_Prefix          : String := "@";
                   Option_Prefix           : String := "--";
                   Concatenation_Separator : String := ",")
                   return Option_Values;

   function Parse (Names                   : Option_Names;
                   Mandatory               : Option_Flags := All_No;
                   When_Repeated           : When_Repeated_Do := Always_Die;
                   Option_Value_Separator  : Character := ':';
                   Include_Prefix          : String := "@";
                   Option_Prefix           : String := "--";
                   Concatenation_Separator : String := ",")
                   return Option_Values;

   package String_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String);

   function Option_Help_Lines return String_Vectors.Vector;

   Bad_Option_Name           : exception;
   Duplicate_Option_Name     : exception;
   Unknown_Option            : exception;
   Repeated_Option           : exception;
   Missing_Mandatory_Options : exception;
end Generic_Command_Line_Parser;
