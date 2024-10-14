with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Doubly_Linked_Lists;

with Ada.Strings.Unbounded;               use Ada.Strings.Unbounded;

generic
   type Options is (<>);
package  Generic_Command_Line_Parser is
   type Option_Spec is private;

   type CLI_Syntax is array (Options) of Option_Spec;

   function Option (Name : String) return Option_Spec;


   type Option_Modifier (<>) is private;

   function "and" (Left : Option_Spec; Right : Option_Modifier)
                   return Option_Spec;

   function Mandatory return Option_Modifier;

   type Repeated_Option_Action is (Concatenate, Overwrite, Die, Ignore);

   function If_Repeated (Action : Repeated_Option_Action) return Option_Modifier;


   type Parsed_CL is tagged private
     with
       Constant_Indexing => Value;

   function Is_Defined (Item   : Parsed_CL; Option : Options)
                        return Boolean;

   function Is_Default (Item   : Parsed_CL; Option : Options)
                        return Boolean;

   function Value (Item   : Parsed_CL; Option : Options) return String
     with
       Pre => Item.Is_Defined (Option);

   function Value (Item   : Parsed_CL; Option : Options) return Integer
     with
       Pre => Item.Is_Defined (Option);

   function Value (Item   : Parsed_CL; Option : Options) return Float
     with
       Pre => Item.Is_Defined (Option);

   function Argument_Count (Item : Parsed_CL) return Natural;

   function Argument (Item : Parsed_CL; N : Positive)
                      return String
     with
       Pre => N <= Item.Argument_Count;

   package Option_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Options);

   function Missing_Options (Item : Parsed_CL) return Option_Lists.List;


   Default_Value_Separator         : constant String := "=";
   Default_Option_Prefix           : constant String := "--";
   Default_Concatenation_Separator : constant String := ",";

   No_Include_Prefix : constant String := (1 => ASCII.NUL);


   package String_Vectors is
     new  Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                             Element_Type => String);

   function Parse_Arguments
     (Source                  : String_Vectors.Vector;
      Syntax                  : CLI_Syntax;
      Option_Value_Separator  : String := Default_Value_Separator;
      Include_Prefix          : String := No_Include_Prefix;
      Option_Prefix           : String := Default_Option_Prefix;
      Concatenation_Separator : String := Default_Concatenation_Separator;
      Name_Case_Sensitive     : Boolean := False)
      return Parsed_CL;

   function Parse_CL
     (Syntax                  : CLI_Syntax;
      Option_Value_Separator  : String := Default_Value_Separator;
      Include_Prefix          : String := No_Include_Prefix;
      Option_Prefix           : String := Default_Option_Prefix;
      Concatenation_Separator : String := Default_Concatenation_Separator;
      Name_Case_Sensitive     : Boolean := False)
      return Parsed_CL;


   function Help_Lines (Syntax : CLI_Syntax)
                        return String_Vectors.Vector;

   Bad_Option_Name           : exception;
   Duplicate_Option_Name     : exception;
   Unknown_Option            : exception;
   Repeated_Option           : exception;
   Missing_Mandatory_Options : exception;
private
   type On_Missing_Action is (Die, Use_Default, Ignore);

   type Option_Spec is
      record
         On_Missing    : On_Missing_Action;
         On_Repetition : Repeated_Option_Action;
         Names         : String_Vectors.Vector;
         Doc           : Unbounded_String;

         Default       : Unbounded_String;
      end record;

   type Modifier_Class is (Make_Mandatory, On_Repeat);

   type Option_Modifier (Class : Modifier_Class) is
      record
         case Class is
            when Make_Mandatory =>
               null;

            when On_Repeat =>
               Repeat_Action  : Repeated_Option_Action;

         end case;
      end record;


   type Option_Value_Status is (Undefined, User_Defined, Default);

   type Option_Value is
      record
         Status : Option_Value_Status;
         Value  : Unbounded_String;
      end record;

   type Option_Value_Array is
     array (Options) of Option_Value;


   type Parsed_CL is
     tagged
      record
         Options   : Option_Value_Array;
         Arguments : String_Vectors.Vector;
         Missing   : Option_Lists.List;
      end record;

   function Is_Defined (Item   : Parsed_CL;
                        Option : Options)
                        return Boolean
   is (Item.Options (Option).Status /= Undefined);

   function Is_Default (Item   : Parsed_CL;
                        Option : Options)
                        return Boolean
   is (Item.Options (Option).Status = Default);

   function Value (Item   : Parsed_CL;
                   Option : Options) return String
   is (To_String (Item.Options (Option).Value));

   function Argument_Count (Item : Parsed_CL) return Natural
   is (Natural (Item.Arguments.Length));

   function Argument (Item : Parsed_CL;
                      N    : Positive)
                      return String
   is (Item.Arguments.Element (N));


   function Missing_Options (Item : Parsed_CL) return Option_Lists.List
   is (Item.Missing);

   function Value (Item   : Parsed_CL; Option : Options) return Integer
   is (Integer'Value (Item.Value (Option)));

   function Value (Item   : Parsed_CL; Option : Options) return Float
   is (Float'Value (Item.Value (Option)));

end Generic_Command_Line_Parser;
