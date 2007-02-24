-- Biblioteca de Calendario, com timestamp
--
-- author Willian Gigliotti <wgigliotti@gmail.com>
-- created at 2007-02-21

with Ada.Calendar; use Ada.Calendar; 
with Aw_Lib.Replacer; use Aw_Lib.Replacer;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;


package body Aw_Lib.Calendar is


	MomentoUnix: Time := Time_Of(1970, 1,1);
	-- Momento inicial do Timestamp
	
	function Get(This: in Timestamp; Key: in Character) return Unbounded_String is 
	-- Funcao que recebauma chave e retorna o valor;
		Str : Unbounded_String;
		Hora: Time := To_Time(This);


		function To_Unb(Num: Integer; Tamanho: Integer) return Unbounded_String is
			-- Transforma numero em Unb String com tamanho especifico;
			Str: Unbounded_String;
			T : Integer := Tamanho;
			N : Integer := Num;
		begin
			while T >= 1 loop
				Str := Str & To_Unbounded_String(N/T);

				Put("[  ");Put(N);Put(To_String(Str));Put_Line("]");
				N := N mod T;
				T := T/10;
				
			end loop;
			return Str;
		end To_Unb;

	begin
		Put("Entrou um => ");
		Put(Key);
		New_Line;

		case Key is
				
			when 'd' => 
				Str := To_Unb(Integer(Day(Hora)),10);
				Put(Integer(Day(Hora)));
			when 'm' => 
				Str := To_Unb(Integer(Month(Hora)),10);
			when 'y' =>
				Str := To_Unb(Integer(Year(Hora)) mod 100, 10);
			when 'Y' => 
				Str := To_Unb(Integer(Year(Hora)), 1000);
			
			when others => Str := Str;
		end case;
		Put("Saiu um [");
		Put(To_String(Str));
		Put_Line("]");
							
	       return Str ;
	end Get;


	function To_Timestamp( Hora: in Ada.Calendar.Time) return Timestamp is
		-- Retorna o timestamp dado um Time;
		tempo: Timestamp;
	begin
		tempo := Timestamp(Hora - MomentoUnix);
		return tempo;
	end To_Timestamp;

	function To_Time( Hora: in Timestamp) return Time is
		-- Retorna o Time dado um Timestamp;
		Tempo: Time;
		
	begin
		tempo := MomentoUnix + Duration(Hora);
		return tempo;
	end To_Time;


        function Date( Formato: in Unbounded_String; Hora: in Timestamp) return Unbounded_String is
		Str : Unbounded_String := Formato;
	begin
		Date(Str, Hora);
		return Str;
	end Date;




	procedure Date( Formato: in out Unbounded_String; Hora: in Timestamp) is
		Str_Pos : Integer;
		Str     : Unbounded_String;
		Str_New : Unbounded_String;
		Str_Len : Integer;
		Comeco  : Unbounded_String;
		Fim     : Unbounded_String;
	begin
		loop
			Get_Variavel(Formato, 1, Str_Pos, Str_Len, Str);
			exit when Str_Len = 0;
			--Put(Str_Pos); Put(Str_Len); Put(To_String(Str));
			Str_New := get(Hora, Element(Str,1));
			New_Line;
			Put(To_String(Str_New));
			Replace_Slice(Formato, Positive(Str_Pos), Natural(Str_Len + Str_Pos), To_String(Str_New));
			Str_Len := 0;

			exit when Str_Len = 0;
		end loop;		

	end Date;
		

end Aw_Lib.Calendar;




