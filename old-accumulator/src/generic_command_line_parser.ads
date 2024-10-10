with Ada.Strings.Unbounded;               use Ada.Strings.Unbounded;
with String_Vectors;

generic
   type Options is (<>);
package  Generic_Command_Line_Parser is
   type Option_Names is array (Options) of Unbounded_String;

   type Default_Class is (Mandatory, Use_Default, Ignore);

   type Default_Spec (Class : Default_Class := Ignore) is
      record
         case Class is
            when Use_Default =>
               Default : Unbounded_String;

            when Mandatory | Ignore =>
               null;
         end case;
      end record;

   Mandatory_Option  : constant Default_Spec := (Class => Mandatory);
   Ignore_If_Missing : constant Default_Spec := (Class => Ignore);

   type Option_Defaults is array (Options) of Default_Spec;

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


   --  package String_Vectors is new Ada.Containers.Formal_Vectors
   --    (Index_Type   => Positive,
   --     Element_Type => Unbounded_String);

   procedure Parse (Source                  : String;
                    Names                   : Option_Names;
                    Result                  : out Option_Values;
                    When_Repeated           : When_Repeated_Do := Always_Die;
                    Option_Value_Separator  : Character        := Default_Value_Separator;
                    Include_Prefix          : Character := No_Include_Prefix;
                    Option_Prefix           : String := Default_Option_Prefix;
                    Concatenation_Separator : String := Default_Concatenation_Separator;
                    Name_Case_Sensitive     : Boolean := False);

   procedure Parse (Names                   : Option_Names;
                    Result                  : out Option_Values;
                    When_Repeated           : When_Repeated_Do := Always_Die;
                    Option_Value_Separator  : Character := Default_Value_Separator;
                    Include_Prefix          : Character := No_Include_Prefix;
                    Option_Prefix           : String := Default_Option_Prefix;
                    Concatenation_Separator : String := Default_Concatenation_Separator;
                    Name_Case_Sensitive     : Boolean := False);

   function Find_Missing_Options (Values    : Option_Values;
                                  Mandatory : Option_Flags)
                                  return String_Vectors.Vector;


   function Find_Missing_Options  (Values    : Option_Values;
                                   Mandatory : Option_Flags;
                                   Join_With : String := " ")
                                   return String;

   procedure Apply_Defaults (Values    : in out Option_Values;
                             Missing   : out Unbounded_String;
                             Defaults  : Option_Defaults);

   procedure Apply_Defaults (Values    : in out Option_Values;
                             Missing   : in out String_Vectors.Vector;
                             Defaults  : Option_Defaults);

   function Help_Lines (Specs : Option_Names)
                        return String_Vectors.Vector;

   Bad_Option_Name           : exception;
   Duplicate_Option_Name     : exception;
   Unknown_Option            : exception;
   Repeated_Option           : exception;
   Missing_Mandatory_Options : exception;
end Generic_Command_Line_Parser;
