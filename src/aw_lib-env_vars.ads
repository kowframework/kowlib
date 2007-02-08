-- Basic dealling with environment variable
-- it's here while Ada.Environment_Variable package don't become part of
-- gnat-gcc on gentoo linux distribution
--
-- author Marcelo C. de Freitas
-- createdAt 2007-01-26



with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

package Aw_Lib.Env_Vars is

   function Value( V: in String ) return String;
   -- return the value of the Environment Variable called V;

   function Value( V: in String ) return Unbounded_String;
   -- return the value of the Environment Variable called V;


end Aw_Lib.Env_Vars;
