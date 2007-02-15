-- this is the linux implementation of system-dependent functions for Aw_Lib
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-25
-- lastUpdate


with Ada.Strings.Wide_Unbounded;
with Ada.Text_IO;

with Aw_Lib.Env_Vars; use Aw_Lib.Env_Vars;
with Aw_Lib.Wide_String_Util;
with Aw_Lib.UWide_String_Vectors;

package body Aw_Lib.File_System is

   function Get_Home return Wide_String is
   -- return the location of user's home dir/my documents folder
   begin
      return Value( "HOME" );
   end Get_Home;

   function Get_Config_Dir( App: in Wide_String ) return Wide_String is
	   -- return a hidden folder where the user can store settings for the
	   -- application called Application.

	   use Aw_Lib.Wide_String_Util;
   begin
	   return  Get_Home & "/." & Str_Replace( '/', '-', App );
   end Get_Config_Dir;

   function Get_Global_Config_Dir( App: in Wide_String := "" ) return Wide_String is
	   -- return a directory where all the configuration global to all users
	   -- of the application App (when set) should be stored.
	   -- if App is not set, return the global configuration folder of the system
	   use Aw_Lib.Wide_String_Util;
   begin
	   return "/etc/" & Str_Replace( '/', '-', App );
   end Get_Global_Config_Dir;




   function Get_Working_Dir return Wide_String is
      -- return the local working directory
   begin
      return Value( "PWD" );
   end Get_Working_Dir;


   function Get_Absolute_Path (Original : in Wide_String) return Wide_String is
      -- return the absolute path of the URL.
      -- system dependent implementation
      i : Integer := Original'First;

      function Process_Home (O : in Wide_String) return Wide_String is
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

      function Process_Working_Dir( O: in Wide_String ) return Wide_String is
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


      function Process_Dots (O : in Wide_String) return Wide_String is
         -- process '..' signs removing one level from the directory hierarchy

         use Aw_Lib.UWide_String_Vectors;
         use Aw_Lib.Wide_String_Util;
         use Ada.Strings.Wide_Unbounded;

         Vect: Vector := explode( '/', O );

         i: Natural := 1;

         function "=" (L: Unbounded_Wide_String; R: Wide_String ) return Boolean is
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

   function To_Vector (SPath : in Wide_String) return Aw_Lib.UWide_String_Vectors.Vector is

      -- explode the Wide_String SPath using ":"
      use Aw_Lib.UWide_String_Vectors;
      Vect : Vector := Aw_Lib.Wide_String_Util.explode (':', SPath);
   begin
      Append (Vect, Value ("PWD"));

      return Vect;
   end To_Vector;




   function Is_File( File_Name: in Wide_String ) return Boolean is
	   -- return true if file called File_Name exists

	   -- NOTE: this isn't the best implementation this could have
	   -- maybe using a C function for file handling which
	   -- actualy use a syscall or something.
	   use Ada.Text_IO;
	   F: File_Type;	
   begin
	   Open( F, In_File, File_Name );
	   Close( F );
	   return true;
   exception
	   when OTHERS =>
		   return false;
   end Is_File;



end Aw_Lib.File_System;


