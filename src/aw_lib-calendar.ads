------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Library                            --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2007-2008, Ydea Desenv. de Softwares Ltda          --
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
-- This is the Aw_Lib.Calendar package                                      --
--                                                                          --
-- Provides functions to handle dates and time, especially to format        --
-- according to Locale and Formatter's Pattern.                             --
------------------------------------------------------------------------------



with Ada.Calendar; 					use Ada.Calendar; 
with Ada.Strings.Unbounded; 		use Ada.Strings.Unbounded;
with Aw_Lib.Replacer;				use Aw_Lib.Replacer;
with Aw_Lib.Locales; 				use Aw_Lib.Locales;
with Ada.Strings.Wide_Unbounded;	use Ada.Strings.Wide_Unbounded;

package Aw_Lib.Calendar is

	-------------------------------
	---------- TIMESTAMP ----------
	-------------------------------
	
	type Timestamp is new Natural;

	function Get(This: in Timestamp; Key: in Character)
		return Unbounded_String;
	-- Funcao que recebe uma chave e retorna o valor;
	-- Essas chaves sao usadas na funcao Date tambem!
	--
	-- Chave    Retorno                 Exemplos

	--                   Hora

	-- a        am ou pm minisculo      am e pm
	-- A        AM ou PM maiusculo      AM e PM

	--                   Dia

	-- d        Dia do mes com 0s       01 ate 31
	-- D        Dia da Semana 3 letras  TODO (Ingles, portugues, etc)
	-- F        Mes escrito             TODO (Ingles, portugues, etc)
	-- M        Mes escrito, 3 letras   TODO (,....)
	-- m        Numero do Mes, com 0s   01 ate 12
	-- TODO continuar documentacao


	function To_Timestamp( Hora: in Ada.Calendar.Time) return Timestamp;
	-- Retorna o timestamp dado um Time;

	function To_Time( Hora: in Timestamp) return Time;
	-- Retorna o Time dado um Timestamp;

        function Date( Formato: in Unbounded_String; Hora: in Timestamp)
		return Unbounded_String;
	-- Funcao que Coloca horarios e Datas num formato dado pela string

	procedure Date( Formato: in out Unbounded_String; Hora: in Timestamp);
	-- Coloca os horarios e datas na propria string;


	------------------------------
	--------- FORMATTER ----------
	------------------------------
	
	Pattern_Error : exception;
  	--  Exception raised for incorrect pattern

	type Formatter is record
		Pattern: Unbounded_String;
	end record; 
	--  This formatter will format a date or time according to Pattern.
	--
	--  This pattern is a string to describe date and time output format. The string is
	--  a set of standard character and special tag that are replaced by the
	--  corresponding values. It follows the GNU Date specification. Here are
	--  the recognized directives :
	--
	--          Time fields:
	--
	--          %H   hour (00..23)
	--          %I   hour (01..12)
	--          %k   hour ( 0..23)
	--          %l   hour ( 1..12)
	--          %M   minute (00..59)
	--          %p   locale's AM or PM
	--          %r   time, 12-hour (hh:mm:ss [AP]M)
	--          %s   seconds  since 1970-01-01  00:00:00 UTC
	--                (a nonstandard extension)
	--          %S   second (00..59)
	--          %T   time, 24-hour (hh:mm:ss)
	--
	--          Date fields:
	--
	--          %a   locale's abbreviated weekday name (Sun..Sat)
	--          %A   locale's    full   weekday   name,    variable   length
	--                  (Sunday..Saturday)
	--          %b   locale's abbreviated month name (Jan..Dec)
	--          %B   locale's    full    month    name,   variable    length
	--                  (January..December)
	--          %c   locale's date and time (Sat Nov 04 12:02:33 EST 1989)
	--          %d   day of month (01..31)
	--          %D   date (mm/dd/yy)
	--          %h   same as %b
	--          %m   month (01..12)
	--          %w   day of week (0..6) with 0 corresponding to Sunday
	--          %x   locale's date representation (mm/dd/yy)
	--          %y   last two digits of year (00..99)
	--          %Y   year (1970...)
		
		
	function Get_Formatter(Pattern : Unbounded_String) return Formatter;
	-- returns a Formatter for a Unbounded_String Pattern.
	
	function Get_Formatter(Pattern : String) return Formatter;
	-- returns a Formatter for a String Pattern.
	

	function Format(L: Locale; F: Formatter; date: Time) 
		return String;
	-- returns the date formatted according to Formatter's Pattern and Locale.

	function Format(F : Formatter; Date : Time) return String;
	--  Format the date using the ISO Locale.  
	
	function Get_Date return Time;
	-- returns the current time.

end Aw_Lib.Calendar;






