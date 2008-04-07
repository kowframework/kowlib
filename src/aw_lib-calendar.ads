-- Biblioteca de Calendario, com timestamp
--
-- author Willian Gigliotti <wgigliotti@gmail.com>
-- created at 2007-02-21
--
-- Repository information
-- $Date$
-- $Revision$
-- $Author$

with Ada.Calendar; use Ada.Calendar; 
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Aw_Lib.Replacer; use Aw_Lib.Replacer;
with Aw_Lib.Locales; use Aw_Lib.Locales;
with Ada.Strings.Wide_Unbounded;   use Ada.Strings.Wide_Unbounded;

package Aw_Lib.Calendar is


	---------- TIMESTAMP ----------

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
	-----------------------------
	
	Pattern_Error : exception;
  	--  Exception raised for incorrect picture


	type Formatter is record
		Pattern: Unbounded_String;
	end record;

   --  This is a string to describe date and time output format. The string is
   --  a set of standard character and special tag that are replaced by the
   --  corresponding values. It follows the GNU Date specification. Here are
   --  the recognized directives :
   --
   --          %    a literal %
   --          n    a newline
   --          t    a horizontal tab
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
   --
   --          By default,  date pads numeric fields with zeroes.  GNU date
   --          recognizes the following nonstandard numeric modifiers:
   --
   --          -    (hyphen) do not pad the field
   --          _    (underscore) pad the field with spaces
   --
   --  Here are some GNAT extensions to the GNU Date specification:
   --
   --          %i   milliseconds (3 digits)
   --          %e   microseconds (6 digits)
   --          %o   nanoseconds  (9 digits)

	function Get_Formatter(Pattern : Unbounded_String) return Formatter;
	function Get_Formatter(Pattern : String) return Formatter;

	function Format(L: Locale; F: Formatter; date: Time) 
		return String;


	-- format using the ISO norm --
	function Format(date : Time) return String;

	function Get_Date return Time;

end Aw_Lib.Calendar;






