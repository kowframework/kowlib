------------------------------------------------------------------------------
--                                                                          --
--                        KOW Framework :: Library                          --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2007-2009, KOW Framework Project               --
--                                                                          --
--                                                                          --
-- KOW_Lib is free library;  you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOW_Lib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with KOW_Lib; see file COPYING. If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- This is the Locales_Test                                                 --
--                                                                          --
-- Example on how to use KOW_Lib.Locales                                    --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Calendar;			use Ada.Calendar;
with Ada.Text_IO;			use Ada.Text_IO;

-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Locales;			use KOW_Lib.Locales;
with KOW_Lib.Locales.Formatting;

procedure Locales_Test is 
	
	procedure Test_Locale( L : in Locale_Type ) is
		procedure PT( Pattern : in String ) is
		begin
			Put_Line(
				Formatting.Format( 
							L	=> L,
							T	=> Clock,
							Pattern	=> Pattern
						      )
					);
		end PT;

		procedure PT( Pattern : in String_Access ) is
		begin
			PT( To_String( Pattern ) );
		end PT;

		procedure PTL( Pattern : in String ) is
		begin
			PT( "%" & Pattern & " => " & Pattern );
		end PTL;

	begin
		New_Line( 2 );
		Put_Line( "Running tests for Locale :: " & To_String( L.Label ) );
		PT( L.Default_Datetime );
		PT( L.Long_Date );
		PTL( "%_m" );
		PTL( "%-m" );
		PTL( "%0m" );

		New_Line;

		Put_Line( Formatting.To_String( L, 1234567.89, 5 ) );
		Put_Line( Formatting.Money_To_String( L, 1234567.89 ) );

		New_Line( 2 );
	end Test_Locale;
begin
	KOW_Lib.Locales.Iterate( Test_Locale'Access );
end Locales_Test;




