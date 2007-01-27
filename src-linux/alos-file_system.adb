-- this is the linux implementation of system-dependent functions for alos
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-25
-- lastUpdate


with Ada.Strings.Unbounded;

with ALOS.Env_Vars; use ALOS.Env_Vars;
with ALOS.String_Util;
with ALOS.UString_Vectors;

package body ALOS.File_System is

   function Get_Home return String is
   -- return the location of user's home dir/my documents folder
   begin
      return Value( "HOME" );
   end Get_Home;


   function Get_Working_Dir return String is
      -- return the local working directory
   begin
      return Value( "PWD" );
   end Get_Working_Dir;


   function Get_Absolute_Path (Original : in String) return String is
      -- return the absolute path of the URL.
      -- system dependent implementation
      i : Integer := Original'First;

      function Process_Home (O : in String) return String is
         -- replace ~/ by it's home
         i : Integer := O'First;
      begin
         if O'Length < 1 or O (i) /= '~' then
            return O;
         else
            if O'Length = 1 then
               return Get_Home;
            elsif O (i + 1) = '/' then
               return Get_Home & O (i + 1 .. O'Last);
            end if;
         end if;
         return O;
      end Process_Home;

      function Process_Working_Dir( O: in String ) return String is
         -- replace ./ by local working directory
      begin
         if O'Length < 1 or O (i) /= '.' then
            return O;
         else
            if O'Length = 1 then
               return Get_Working_dir;
            elsif O (i + 1) = '/' then
               return Get_Working_Dir & O (i + 1 .. O'Last);
            end if;
         end if;
         return O;
      end Process_Working_Dir;


      function Process_Dots (O : in String) return String is
         -- process '..' signs removing one level from the directory hierarchy

         use ALOS.UString_Vectors;
         use ALOS.String_Util;
         use Ada.Strings.Unbounded;

         Vect: Vector := explode( '/', O );

         i: Natural := 1;

         function "=" (L: Unbounded_String; R: String ) return Boolean is
            -- compare an unbonded string with a bounded string
            -- returns true only if they are identical

            L_Index: Natural := 1;
         begin
            if Length( L ) /= R'Length then
               return false;
            end if;

            for R_Index in R'Range loop
               if Element( L, L_Index ) /= R(R_Index) then
                  return false;
               end if;
               L_Index := L_Index + 1;
            end loop;

            return true;

         end "=";


      begin

         while i < Last_Index( Vect ) loop
            if Element( Vect, i ) = ".." then
               i := i - 1;
               Delete( Vect, i );
               Delete( Vect, i );
            else
               i := i + 1;
            end if;
         end loop;

         return implode( '/', Vect );
      end Process_Dots;

   begin
      return Process_Dots (Process_Working_Dir( Process_Home( Original ) ) );
   end Get_Absolute_Path;

   function To_Vector (SPath : in String) return ALOS.UString_Vectors.Vector is

      -- explode the String SPath using ":"
      use ALOS.UString_Vectors;
      Vect : Vector := ALOS.String_Util.explode (':', SPath);
   begin
      Append (Vect, Value ("PWD"));

      return Vect;
   end To_Vector;
end ALOS.File_System;


