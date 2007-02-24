
with Aw_Lib.Calendar; use Aw_Lib.Calendar;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;

procedure Teste is 
	Src : Unbounded_String := To_Unbounded_String("Oi trocar will por ${will}");
	T : Time := Clock;
	Hora : Timestamp := To_Timestamp(T);

	
	Fin : Unbounded_String;
begin

	Put_Line("Texte 1 ....");
	Put_Line(To_String(Src));
	Put_Line(To_String(Date(Src, Hora)));
end Teste;




