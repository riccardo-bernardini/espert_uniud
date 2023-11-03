with Ada.Strings.Unbounded;               use Ada.Strings.Unbounded;
with Ada.Containers.Formal_Vectors;

generic
   type Options is (<>);
package  Generic_Command_Line_Parser is
   type Option_Names is array (Options) of Unbounded_String;

   type Option_Flags is array (Options) of Boolean;



   All_No  : constant Option_Flags := (others => False);
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

   Default_Value_Separator : constant Character := '=';
   Default_Option_Prefix   : constant String := "--";
   Default_Concatenation_Separator : constant String := ",";

   No_Include_Prefix : constant Character := ASCII.NUL;


   package String_Vectors is new Ada.Containers.Formal_Vectors
     (Index_Type   => Positive,
      Element_Type => Unbounded_String);

   procedure Parse (Source                  : String;
                    Names                   : Option_Names;
                    Result                  : out Option_Values;
                    When_Repeated           : When_Repeated_Do := Always_Die;
                    Option_Value_Separator  : Character := Default_Value_Separator;
                    Include_Prefix          : Character := No_Include_Prefix;
                    Option_Prefix           : String := Default_Option_Prefix;
                    Concatenation_Separator : String := Default_Concatenation_Separator);

   procedure Parse (Names                   : Option_Names;
                    Result                  : out Option_Values;
                    When_Repeated           : When_Repeated_Do := Always_Die;
                    Option_Value_Separator  : Character := Default_Value_Separator;
                    Include_Prefix          : Character := No_Include_Prefix;
                    Option_Prefix           : String := Default_Option_Prefix;
                    Concatenation_Separator : String := Default_Concatenation_Separator);

   function Find_Missing_Options (Values    : Option_Values;
                                  Mandatory : Option_Flags)
                                  return String_Vectors.Vector;


   function Find_Missing_Options  (Values    : Option_Values;
                                   Mandatory : Option_Flags;
                                   Join_With : String := " ")
                                   return String;

   function Help_Lines (Specs : Option_Names)
                        return String_Vectors.Vector;

   Bad_Option_Name           : exception;
   Duplicate_Option_Name     : exception;
   Unknown_Option            : exception;
   Repeated_Option           : exception;
   Missing_Mandatory_Options : exception;
end Generic_Command_Line_Parser;
