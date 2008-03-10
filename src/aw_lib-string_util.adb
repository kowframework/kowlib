-- Library to perform actions over Strings
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-26
--
-- Repository information:
-- $Date$
-- $Revision$
-- $Author$





with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

with Ada.Containers;          use Ada.Containers;

with Aw_Lib.UString_Vectors;
with Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;

-- Used for string replacement
-- with GNAT.Spitbol.Patterns;


package body Aw_Lib.String_Util is

   function implode( Sep: Character; Vect: Aw_Lib.UString_Vectors.Vector ) return String is
      -- join all parts of the Vector into a String of value element1[SEP]element2[SEP]ele...
   begin
      return To_String( implode( Sep, Vect ) );
   end implode;


   function implode( Sep: Character; Vect: Aw_Lib.UString_Vectors.Vector ) return Unbounded_String is
      -- join all parts of the Vector into a Unbounded_String of value element1[SEP]element2[SEP]ele...

      use Aw_Lib.UString_Vectors;

      function implode_int ( c: Cursor ) return Unbounded_String is
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
         return To_Unbounded_String( "" );
   end implode;


   function explode( Sep: Character; Str: String ) return Aw_Lib.UString_Vectors.Vector is
      -- split the string Str by Sep and return a vector containing it.
   begin
      return explode( Sep, To_Unbounded_String( Str ) );
   end explode;


   function explode( Sep: Character; Str: Unbounded_String ) return Aw_Lib.UString_Vectors.Vector is
      -- split the string Str by Sep and return a vector containing it.
      use Aw_Lib.UString_Vectors;
      Vect: Vector;
      ini, fim: Natural := 1;
   begin


      while fim <= Length( Str )
      loop

         if Element( Str, fim ) = Sep  then
            if ini = fim then
               Append( Vect, To_Unbounded_String( "" ) );
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
         Append( Vect, To_Unbounded_String( "" ) );
      else
         Append( Vect, Unbounded_Slice( Str, ini, fim - 1 ) );
      end if;

      return Vect;

   end explode;


   procedure Str_Replace( From, To: in Character; Str: in out String ) is
   -- replace all the ocurences of the character From by To.
   begin
	   for i in Str'Range loop
		   if Str(i) = From then
			   Str(i) := To;
		   end if;
	   end loop;
   end Str_Replace;

   function Str_Replace( From, To: in Character; Str: in String ) return String is
	   -- replace all the ocurences of the character From by To returning the new Value.
	   R: String := Str;
   begin
	   Str_Replace( From, To, R );
	   return R;
   end Str_Replace;



   procedure Str_Replace( From, To, Str: in Unbounded_String; Result: in out Unbounded_String; Case_Sensitive: Boolean := True ) is
   begin
      Result := Str_Replace( From, To, Str, Case_Sensitive );
   end Str_Replace;

   function Str_Replace( From, To, Str: in Unbounded_String; Case_Sensitive: Boolean := True ) return Unbounded_String is
   begin
      return Str_Replace( To_String( From ), To_String( To ), To_String( Str ), Case_Sensitive );
   end Str_Replace;



   procedure Str_Replace( From, To, Str: in String; Result: in out Unbounded_String; Case_Sensitive: Boolean := True ) is
   begin
      Result := Str_Replace( From, To, Str, Case_Sensitive );
   end Str_Replace;


   function Str_Replace( From, To: in String; Str: in String; Case_Sensitive: Boolean := True ) return Unbounded_String is

      use Positions_Vectors;

      Occurances  : Positions_Vectors.Vector := Find_Occurances( From, Str, Case_Sensitive );
      Size_Dif    : Integer := To'Length - From'Length;
      Result      : Unbounded_String;
      Index_str   : Positive := Str'First;
      Index_Occu  : Positions_Vectors.Extended_Index := Occurances.First_Index;
      Element_Occu: Positions_Vectors.Cursor;
      Actual      : Natural;
      subtype i is Integer range To'Range;

   begin

      if Occurances.Length = 0 then
         return Result; -- To_Unbounded_String( Str )
      end if;

      while Index_str <= Str'Last loop


         if Index_Occu /= Occurances.Last_Index + 1 then
            Actual := Positions_Vectors.Element( To_Cursor( Occurances, Index_Occu ) );
         else
            Actual := 0;
         end if;

         if Index_str = Actual then
            -- replace 

            for i in To'Range loop
               Append( Result, To( i ) );
            end loop;

            Index_Str := Index_Str + From'Length;
            Index_Occu := Index_Occu + 1;
            -- Index_Occu := Occurances.Next;
         else
            Append( Result, Str( Index_Str ) );
            Index_Str := Index_Str + 1;
         end if;

      end loop;

      return Result;

   end Str_Replace;


   function Find_Occurances( Find, Context : in String; Case_Sensitive: Boolean ) return Positions_Vectors.Vector is

      type List is array ( Natural range <> ) of Integer;

      function Pre_Compute( Str : in String; Case_Sensitive: Boolean ) return List is
      Pragma Inline ( Pre_Compute );

         -- computes the failure_function table of the KMP algorithm
         pos : Integer;
         T : List( 0 .. Str'Length ) := ( 0 .. Str'Length => -1 );
         Str2 : String := Str;

         use Ada.Text_IO;
      begin

         if Case_Sensitive then
            Str2 := To_Lower( Str );
         end if;

         for i in 1 .. Str'Length loop
            pos := T( i - 1 );

            while pos /= -1 and then Str( pos + 1 ) /= Str( i ) loop
               pos := T( pos );
            end loop;

            T( i ) := pos + 1;
         end loop;

         return T;

      end;

      -- declarations
      Matches     : Positions_Vectors.Vector;
      Context_Idx : Positive := 1;
      Table       : List := Pre_Compute( Find, Case_Sensitive );
      Find_Idx    : Integer := 0;

      Find2 : String := Find;
      Context2 : String := Context;

      use Ada.Text_IO;
   begin
      -- Knuth-Morris-Pratt algorithm

      if Case_Sensitive then
         Find2 := To_Lower( Find );
         Context2 := To_Lower( Context );
      end if;

      while Context_Idx <= Context2'Length loop

         while Find_Idx /= -1 and then ( Find_Idx = Find2'Length or else Find2( Find_Idx + 1 ) /= Context2( Context_Idx ) ) loop
            Find_Idx := Table( Find_Idx );
         end loop;

         Find_Idx := Find_Idx + 1;
         Context_Idx := Context_Idx + 1;

         if Find_Idx = Find2'Length then
            Matches.Append( Context_Idx - Find2'Length );
         end if;
      end loop;

      return Matches;

   end Find_Occurances;


end Aw_Lib.String_Util;

