-- test of the Aw_Lib.Env_Vars package.
-- it's ment to be running on a unix environment!
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-26
--
-- Repository information:
-- $Date$
-- $Revision$
-- $Author$


with Ada.Text_IO;	use Ada.Text_IO;

with Aw_Lib.Env_Vars;	use Aw_Lib.Env_Vars;


procedure Env_Vars_Test is
begin
   Put_Line( Value( "PWD" ) );
   begin
      Put_Line( Value( "UNIEXISTENT_VARIABLE110298ILNLKN" ) );
   exception
      when CONSTRAINT_ERROR =>
         Put_line( "Ooops. I tried to access a variable tha doesn't exist! :D" );
   end;

end Env_Vars_Test;
