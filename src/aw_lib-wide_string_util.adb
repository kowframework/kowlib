-- Library to perform actions over Wide_Strings
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-26




with Ada.Strings.Wide_Unbounded;	use Ada.Strings.Wide_Unbounded;


with Aw_Lib.UWide_String_Vectors;



package body Aw_Lib.Wide_String_Util is

   function implode( Sep: Wide_Character; Vect: Aw_Lib.UWide_String_Vectors.Vector ) return Wide_String is
      -- join all parts of the Vector into a Wide_String of value element1[SEP]element2[SEP]ele...
   begin
      return To_Wide_String( implode( Sep, Vect ) );
   end implode;


   function implode( Sep: Wide_Character; Vect: Aw_Lib.UWide_String_Vectors.Vector ) return Unbounded_Wide_String is
      -- join all parts of the Vector into a Unbounded_Wide_String of value element1[SEP]element2[SEP]ele...

      use Aw_Lib.UWide_String_Vectors;

      function implode_int ( c: Cursor ) return Unbounded_Wide_String is
         -- recursive function to implode string
      begin
         return Element( c ) & Sep & implode_int( Next( c ) );
      exception
         when CONSTRAINT_ERROR =>
            -- we've reached the end of the vector
            return Element( c );
      end;

   begin
      return implode_int( First( Vect ) );
   exception
      when CONSTRAINT_ERROR =>
         --there is nothing on this vector! :O
         return To_Unbounded_Wide_String( "" );
   end implode;


   function explode( Sep: Wide_Character; Str: Wide_String ) return Aw_Lib.UWide_String_Vectors.Vector is
      -- split the string Str by Sep and return a vector containing it.
   begin
      return explode( Sep, To_Unbounded_Wide_String( Str ) );
   end explode;


   function explode( Sep: Wide_Character; Str: Unbounded_Wide_String ) return Aw_Lib.UWide_String_Vectors.Vector is
      -- split the string Str by Sep and return a vector containing it.
      use Aw_Lib.UWide_String_Vectors;
      Vect: Vector;
      ini, fim: Natural := 1;
   begin


      while fim <= Length( Str )
      loop

         if Element( Str, fim ) = Sep  then
            if ini = fim then
               Append( Vect, To_Unbounded_Wide_String( "" ) );
            else
               Append( Vect, Unbounded_Slice( Str, ini, fim - 1 ) );
            end if;
            ini := fim + 1;
         end if;
         fim := fim + 1;

      end loop;

      -- now we add the last element, it doesn't matter if it's null or not:
      if ini = fim then
         -- it's a null element
         Append( Vect, To_Unbounded_Wide_String( "" ) );
      else
         Append( Vect, Unbounded_Slice( Str, ini, fim - 1 ) );
      end if;

      return Vect;

   end explode;


   procedure Str_Replace( From, To: in Wide_Character; Str: in out Wide_String ) is
   -- replace all the ocurences of the character From by To.
   begin
	   for i in Str'Range loop
		   if Str(i) = From then
			   Str(i) := To;
		   end if;
	   end loop;
   end Str_Replace;

   function Str_Replace( From, To: in Wide_Character; Str: in Wide_String ) return Wide_String is
	   -- replace all the ocurences of the character From by To returning the new Value.
	   R: Wide_String := Str;
   begin
	   Str_Replace( From, To, R );
	   return R;
   end Str_Replace;

end Aw_Lib.Wide_String_Util;

