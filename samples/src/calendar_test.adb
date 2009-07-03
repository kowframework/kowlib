------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Library                            --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2007-2009, Ada Works Project                 --
--                                                                          --
--                                                                          --
-- Aw_Lib is free library;  you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. Aw_Lib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with Aw_Lib; see file COPYING. If not, write --
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
-- This is the Calendar_Test                                                --
--                                                                          --
-- Example on how to use Aw_Lib.Calendar                                    --
------------------------------------------------------------------------------

with Aw_Lib.Calendar;		use Aw_Lib.Calendar;
with Aw_Lib.Locales;		use Aw_Lib.Locales;

with Ada.Float_Text_IO;		use Ada.Float_Text_IO;
with Ada.Integer_Text_IO;	use Ada.Integer_Text_IO;

with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;
with Ada.Calendar;		use Ada.Calendar;
with Ada.Text_IO;		use Ada.Text_IO;
with Ada.Environment_Variables;	use Ada.Environment_Variables;

procedure Calendar_Test is 
	
	procedure Test_Locale(L : in Locale) is
		n : Long_Float := 52144.3344;
		F: Formatter;
	begin
		F := Get_Formatter(Get_Long_Date_Pattern(L));
		Put_Line(Format(L , F, Get_Date));
	
		F := Get_Formatter(Get_Short_Date_Pattern(L));
		Put_Line(Format(L, F, Get_Date));
		F := Get_Formatter(Get_Default_Time_Pattern(L));
		Put_Line(Format(L, F, Get_Date));
		F := Get_Formatter(Get_Default_Date_Pattern(L));
		Put_Line(Format(L, F, Get_Date));

		Put_Line(Get_Formatted_Currency(L, n, 0));
		Put_Line(Get_Formatted_Currency(L, n, 3));
		Put_Line(Get_Formatted_Currency(L, n));
		Put_Line(Get_Formatted_Percentage(L, n));
		Put_Line(Get_Formatted_Percentage(L, n, 0));
		Put_Line(Get_Formatted_Percentage(L, n, 3));
	--	F := Get_Formatter("%H %I %k %l %M %p %r %S %T");
	--	Put_Line(Format(L, F, Get_Date));

	--	F := Get_Formatter("%a %A %b %B %c %d %D %h %m %w %x %y %Y");
	--	Put_Line(Format(L, F, Get_Date));
	end Test_Locale;
	
	
	Src : Unbounded_String := 
		To_Unbounded_String("Oi trocar will por ${will}");
	T : Time := Clock;
	Hora : Timestamp := To_Timestamp(T);
	
	
	use Locale_Tables;
	
	L : Locale := Get_Environment_Locale;
	Fim : Unbounded_String;
	position: Cursor := Locale_Tables.First(Supported_Locales);
begin
	 Test_Locale(L);
      	 while Has_Element(position) loop
       		 L :=  Element(position);
      		 Test_Locale(L);
 
		 Put_Line( Get_Formated_Full_name( L, "John", "Doe" ));
 
     		 position := Next(position);
 
		 Ada.Text_IO.New_Line( 2 );
	 end loop;
	
	Ada.Text_IO.Put_Line("");
	Ada.Text_IO.Put_Line("Texto 1 ....");
	Ada.Text_IO.Put_Line(To_String(Src));
	Ada.Text_IO.Put_Line(To_String(Date(Src, Hora)));
end Calendar_Test;




