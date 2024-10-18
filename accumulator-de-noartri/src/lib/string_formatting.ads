with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Characters.Handling;
with Ada.Text_IO;

use Ada;


package String_Formatting is
   type Parsed_Format is private;

   function Parse_Format
     (Format              : String;
      Accepted_Directives : String := "";
      Directive_Prefix    : Character := '%')
      return Parsed_Format
     with
       Pre => Characters.Handling.Is_Special (Directive_Prefix);

   Parsing_Error : exception;

   procedure Dump (Item : Parsed_Format;
                   To   : Text_Io.File_Type := Text_IO.Standard_Output);

   type Provider_Function is
     access function (Directive : Character;
                      Parameter : String;
                      Counter   : Positive)
                      return String;

   type Provider_Interface is interface;

   function Provide
     (Provider  : Provider_Interface;
      Directive : Character;
      Parameter : String;
      Counter   : Positive)
      return String is abstract;

   function Expand
     (Format              : String;
      Provider            : access function (Directive : Character;
                                             Parameter : String;
                                             Counter   : Positive) return String;
      Accepted_Directives : String := "";
      Directive_Prefix    : Character := '%')
      return String
     with
       Pre => Characters.Handling.Is_Special (Directive_Prefix);

   function Expand (Format              : String;
                    Provider            : Provider_Interface'Class;
                    Accepted_Directives : String := "";
                    Directive_Prefix    : Character := '%')
                    return String
     with
       Pre => Characters.Handling.Is_Special (Directive_Prefix);


   function Expand
     (Format   : Parsed_Format;
      Provider : access function (Directive : Character;
                                  Parameter : String;
                                  Counter   : Positive) return String)
      return String;

   function Expand (Format   : Parsed_Format;
                    Provider : Provider_Interface'Class)
                    return String;

   function C_Style_Formatting (Datum     : Integer;
                                Parameter : String;
                                Basis     : Positive := 10)
                                return String;

   procedure Parse_Precision (Input : String;
                              Size  : out Positive;
                              Prec  : out Natural);

   generic
      type Flags is (<>);
   package Flag_Parsing is
      type Flag_Names is array (Flags) of Character;

      type Flag_Array is array (Flags) of Boolean;

      procedure Extract_Flags (Input          : String;
                               Names          : Flag_Names;
                               Present        : out Flag_Array;
                               First_Non_Flag : out Positive);
   end Flag_Parsing;
private
   type Callback_Based_Provider (Callback : Provider_Function) is
     new Provider_Interface
   with
     null record;

   function Provide
     (Provider  : Callback_Based_Provider;
      Directive : Character;
      Parameter : String;
      Counter   : Positive)
      return String;

   type Format_Segment_Class is (Text, Directive);

   type Format_Segment (Class : Format_Segment_Class;
                        Size  : Natural) is
      record
         case Class is
            when Text =>
               Value : String (1 .. Size);

            when Directive =>
               Label     : Character;
               Parameter : String (1 .. Size);

         end case;
      end record;

   package Segment_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Format_Segment);

   type Parsed_Format is
      record
         Segments : Segment_Lists.List;
      end record;

end String_Formatting;
