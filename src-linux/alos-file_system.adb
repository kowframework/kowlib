-- this is the linux implementation of system-dependent functions for alos
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2007-01-25
-- lastUpdate



with ALOS.Env_Vars;	use ALOS.Env_Vars;


package body ALOS.FS is

	function Get_Home return String is
		-- return the location of user's home dir/my documents folder
	begin
		return Value( "HOME" );
	end Get_Home;

	function Get_Absolute_Path( Original: in String ) return String is
	-- return the absolute path of the URL.
	-- system dependent implementation
		i: Integer := Original'First;

		function Process_Home( O: in String ) return String is
			-- replace ~/ by it's home 
			i: Integer := O'First;
		begin
			if O'Length < 1  OR O(i) /= '~' then
				return O;
			else
				if O'Length = 1 then
					return Get_Home;
				elsif O(i+1) = '/' then
					return Get_Home & O(i+1 .. O'Last);
				end if;
			end if;
			return O;
		end Process_Home;


		function Process_Down_Level( O: in String ) return String is
			-- process '..' signs removing one level from the directory hierarchy
		begin
			return O;
		end Process_Down_Level:

	begin
		return Process_Down_Level( Process_Home( Original ) );
	end Get_Absolute_Path;
	

	function To_Path_Vector( SPath: in String ) return ALOS.UString_Vectors.Vector is 
	-- explode the String SPath using ":" or ";" or the specific system separator
	-- returning an array with the absolute path of each directory.
	begin
		return To_Path_Vector( SPath, ':' );
	end To_Path_Vector;
end ALOS.FS;
