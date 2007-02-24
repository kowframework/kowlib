-- Package for handling environment variables in POSIX systems
--
-- author Marcelo Coraça de Freitas <marcelo.batera@gmail.com> 
--
-- Repository information:
-- $Date$
-- $Revision$
-- $Author$


with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;
with Interfaces.C.Strings;	use Interfaces.C.Strings;


package body Aw_Lib.Env_Vars is

   function Value( V: in String ) return String is
      -- return the value of the Environment Variable called V;
      function getenv( Var: in chars_ptr ) return chars_ptr;
      pragma Import( C, getenv );

      function To_String( S: chars_ptr ) return String is
         -- convert from C string to Ada String but
         -- if S is a null string raise CONSTRAINT_ERROR.
         -- just like in Ada.Environment_Variables
      begin
         if S = Null_Ptr then
            raise CONSTRAINT_ERROR;
         else
            return Value( S );
         end if;
      end To_String;
      pragma Inline( To_String );


      -- now our own variables

      C_V: chars_ptr := New_String( V );
      C_R: chars_ptr := getenv( C_V );
      R: String := To_String( C_R );

   begin
      Free( C_V );
      return R;
   end;

   function Value( V: in String ) return Unbounded_String is
      -- return the value of the Environment Variable called V;
   begin
      return To_Unbounded_String( Value( V ) );
   end Value;

end Aw_Lib.Env_Vars;
