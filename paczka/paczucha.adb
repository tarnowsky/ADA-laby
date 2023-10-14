with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics;
use Ada.Numerics;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

procedure Paczucha is
	-- Zmienne stale
	--
	-- Syntax: < Nazwa: constant Typ := Wartosc; >
	--
	Liczba_Wyrobow: constant Integer := 5;
	Liczba_Ciast: constant Integer := 3;
	Liczba_Klientow: constant Integer := 2;
	Czas_Oczekiwania_Klienta: constant Integer := 15;
	Czas_Oczekiwania_Skladnika: constant Integer := 10;

	Pojemnosc_Magazynu: constant Integer := 10;

	-- Podtypy, czyli typy, które dzialaja z typami bazowymi
	--
	-- Syntax: < subtype Nazwa is Typ Wlasciwosci; >
	--
	subtype Zakres_Czasu_Produkcji is Integer range 8 .. 24;
	subtype Zakres_Czasu_Konsumpcji is Integer range 10 .. 15;

	subtype Zakres_Wyrobow is Integer range 1 .. Liczba_Wyrobow;
	subtype Zakres_Ciast is Integer range 1 .. Liczba_Ciast;
	subtype Zakres_Klientow is Integer range 1 .. Liczba_Klientow;

	-- Typy
	--
	type Typ_Magazynu is array(Zakres_Wyrobow) of Integer;

	-- Typ tablicowy
	--
	-- Syntax: < array (range'First .. range'Last) of Typ
	--
	Nazwa_Wyrobu: constant array (Zakres_Wyrobow) of String(1 .. 7)
	:= ("Orzechy", "Maka   ", "Mleko  ", "Jaja   ", "Cukier ");

	Nazwa_Zestawu: constant array (Zakres_Ciast) of String(1 .. 7)
    := ("Sernik ", "Piernik", "Tort   ");

	Nazwa_Konsumenta: constant array(Zakres_Klientow) of String(1 .. 5)
	:= ("Kasia", "Kinga");

	Magazyn: Typ_Magazynu := (0, 0, 0, 0, 0);
	Sklad_Zestawu: array(Zakres_Ciast, Zakres_Wyrobow) of Integer := ((2, 1, 2, 1, 2),
																		(2, 2, 0, 1, 0),
																		(1, 1, 2, 0, 1));
	Numer_Zestawu: array(Zakres_Ciast) of Integer := (1, 1, 1);

	-- Paczki
	--
	package Losowa_Konsumpcja is new Discrete_Random(Zakres_Czasu_Konsumpcji);
	package Losowy_Zestaw is new Discrete_Random(Zakres_Ciast);

	-- Taski
	--
	task type TProducent is
		entry Zacznij(Wyrob: in Zakres_Wyrobow; Czas_Produkcji: in Integer);
	end TProducent;

	task type TKonsument is
		entry Zacznij(Numer_Konsumenta: in Zakres_Klientow; Czas_Konsumpcji: in Integer);
	end TKonsument;

	task type TBufor is
		entry Przyjmij(Wyrob: in Zakres_Wyrobow; Numer: in Integer; Czy_Przyjeto: out Boolean);
		entry Wydaj(Zestaw: in Zakres_Ciast; Numer: out Integer);
	end TBufor;

	-- Deklaracje zmiennych tasków
	--
	Producenci: array(1 .. Liczba_Wyrobow) of TProducent;
	Klienci: array(1 .. Liczba_Klientow) of TKonsument;
	Bufor: TBufor;

	-- Body tasków
	--
	task body TProducent is
		package Losowa_Produkcja is new Discrete_Random(Zakres_Czasu_Produkcji);

		Generator: Losowa_Produkcja.Generator;
		Numer_Wyrobu: Integer := 1;

		Nr_Typu_Wyrobu: Integer;
		Produkcja: Integer;
		Przyjeto: Boolean := False;

	begin
		accept Zacznij(Wyrob: in Zakres_Wyrobow; Czas_Produkcji: in Integer) do
			Losowa_Produkcja.Reset(Generator);

			Nr_Typu_Wyrobu := Wyrob;
			Produkcja := Czas_Produkcji;
		end Zacznij;

		Put_Line("P [info]: ZACZETO PRODUKCJE: " & Nazwa_Wyrobu(Nr_Typu_Wyrobu));

		loop
			delay Duration(Losowa_Produkcja.Random(Generator));
			Put_Line("P [info]: WYPRODUKOWANO: " & Nazwa_Wyrobu(Nr_Typu_Wyrobu) & " numer " & Integer'Image(Numer_Wyrobu));

			loop
				Bufor.Przyjmij(Nr_Typu_Wyrobu, Numer_Wyrobu, Przyjeto);
				if Przyjeto = False then
					Put_Line("P [warn]: Brak miejsca na polce dla skladnika " & Nazwa_Wyrobu(Nr_Typu_Wyrobu) & ". Czekam 5 sekund");

					delay Duration(5.0);
				else
					Numer_Wyrobu := Numer_Wyrobu + 1;
				end if;
				exit;
			end loop;
		end loop;
	end TProducent;

	task body TKonsument is
		Generator: Losowa_Konsumpcja.Generator;
		Generator2: Losowy_Zestaw.Generator;

		Nr_Konsumenta: Zakres_Klientow;
		Numer_Zestawu: Integer;
		Konsumpcja: Integer;
		Rodzaj_Zestawu: Integer;

	begin
		accept Zacznij(Numer_Konsumenta: in Zakres_Klientow; Czas_Konsumpcji: in Integer) do
			Losowa_Konsumpcja.Reset(Generator);
			Losowy_Zestaw.Reset(Generator2);

			Nr_Konsumenta := Numer_Konsumenta;
			Konsumpcja := Czas_Konsumpcji;
		end Zacznij;

		loop
			Put_Line("K [info]: PRZYSZEDL: klient " & Nazwa_Konsumenta(Nr_Konsumenta));
			delay Duration(Losowa_Konsumpcja.Random(Generator));

			Rodzaj_Zestawu := Losowy_Zestaw.Random(Generator2);

			--Put_Line("K [info]: Klient " & Nazwa_Konsumenta(Nr_Konsumenta) & " chce kupic " & Nazwa_Zestawu(Rodzaj_Zestawu));

			select
				delay Duration(Czas_Oczekiwania_Klienta);
				Put_Line("K [info]: WYCHODZI: " & Nazwa_Konsumenta(Nr_Konsumenta) & ". Nie chce czekac.");
				--Put_Line("K [error]: " & Nazwa_Konsumenta(Nr_Konsumenta) & " jednak nie kupi " & Nazwa_Zestawu(Rodzaj_Zestawu));
			then abort
				Put_Line("K [info]: Klient " & Nazwa_Konsumenta(Nr_Konsumenta) & " chce kupic " & Nazwa_Zestawu(Rodzaj_Zestawu));
				loop
					Bufor.Wydaj(Rodzaj_Zestawu, Numer_Zestawu);
					if Numer_Zestawu = 0 then
						Put_Line("K [warn]: Brak skladnikow do ciasta " & Nazwa_Zestawu(Rodzaj_Zestawu) & " dla klienta " & Nazwa_Konsumenta(Nr_Konsumenta) & ". Czekam 5 sekund");
						delay Duration(5.0);
					else
						Put_Line("K [info]: " & Nazwa_Konsumenta(Nr_Konsumenta) & " kupil " & Nazwa_Zestawu(Rodzaj_Zestawu) & " #" & Integer'Image(Numer_Zestawu));
						Put_Line("K [info]: WYCHODZI: " & Nazwa_Konsumenta(Nr_Konsumenta) & ".");
						exit;
					end if;
				end loop;
			end select;

			delay Duration(Losowa_Konsumpcja.Random(Generator));
		end loop;
	end TKonsument;

	task body TBufor is
		W_Magazynie: Integer := 0;

		Licznik_Nie_Przyjeto: Integer := 0;
		Max_Wyrob: Integer := 0;
		Max_Wyrob_Rodzaj: Zakres_Wyrobow := 1;

		-- Mozna_Przyjac
		function Mozna_Przyjac(Wyrob: Zakres_Wyrobow) return Boolean is
			Wolne: Integer;
			Tmp_Mag: Typ_Magazynu;
			Brak: array(Zakres_Ciast, Zakres_Wyrobow) of Integer;
			Max_Brak: array(Zakres_Wyrobow) of Integer;
			Braki: Integer;
		begin
			if W_Magazynie >= Pojemnosc_Magazynu then
				return False;
			else
				Wolne := Pojemnosc_Magazynu - W_Magazynie;
				Tmp_Mag := Magazyn;
				Tmp_Mag(Wyrob) := Tmp_Mag(Wyrob) + 1;

				for W in Zakres_Wyrobow
				loop
					Max_Brak(W) := 0;
				end loop;

				for Z in Zakres_Ciast
				loop
					for W in Zakres_Wyrobow
					loop
						Brak(Z, W) := Integer'Max(0, Sklad_Zestawu(Z, W) - Tmp_Mag(W));

						if Brak(Z, W) > Max_Brak(W) then
							Max_Brak(w) := Brak(Z, W);
						end if;
					end loop;
				end loop;
			end if;

			Braki := 0;

			for W in Zakres_Wyrobow
			loop
				Braki := Braki + Max_Brak(W);
			end loop;

			return Wolne >= Braki;
		end Mozna_Przyjac;

		-- Mozna wydac
		function Mozna_Wydac(Zestaw: Zakres_Ciast) return Boolean is
		begin
			for W in Zakres_Wyrobow loop
				if Magazyn(W) < Sklad_Zestawu(Zestaw, W) then
					return False;
				end if;
			end loop;

			return True;
		end Mozna_Wydac;

		-- Wypisz sklad magazynu
		procedure Sklad_Magazynu is
			Str: Unbounded_String;
		begin
			Append(Str, "B [info]: Sklad polki ze skladnikami: ");
			for W in Zakres_Wyrobow loop
				Append(Str, Integer'Image(Magazyn(W)) & ", ");

				if W = Liczba_Wyrobow then
					Append(Str, ASCII.LF);
				end if;
			end loop;

			Put_Line(To_String(Str));
		end Sklad_Magazynu;

		procedure Usun_Skladnik(Wyrob: in Zakres_Wyrobow) is
		begin
			Magazyn(Wyrob) := Magazyn(Wyrob) - 1;
			W_Magazynie := W_Magazynie - 1;
		end Usun_Skladnik;

	begin
		Put_Line("B [info]: Zaczeto Bufor" & ASCII.LF);
		loop
			Put_Line("B [info]: Czekam na zamowienie...");
			select
				accept Wydaj(Zestaw: in Zakres_Ciast; Numer: out Integer) do
					if Mozna_Wydac(Zestaw) then
						Put_Line("B [info]: WYDANO: " & Nazwa_Zestawu(Zestaw) & " nr " & Integer'Image(Numer_Zestawu(Zestaw)));

						for W in Zakres_Wyrobow loop
							Magazyn(W) := Magazyn(W) - Sklad_Zestawu(Zestaw, W);
							W_Magazynie := W_Magazynie - Sklad_Zestawu(Zestaw, W);
						end loop;

						Numer := Numer_Zestawu(Zestaw);
						Numer_Zestawu(Zestaw) := Numer_Zestawu(Zestaw) + 1;
					else
						Numer := 0;
					end if;
				end Wydaj;

			or delay Duration(3.0);
				Put_Line("B [info]: Brak zamowien. Zajmuje sie przyjmowaniem na polki");
				accept Przyjmij(Wyrob: in Zakres_Wyrobow; Numer: in Integer; Czy_Przyjeto: out Boolean) do
					--Put_Line("B [debug]: Przyjmiesz? " & Nazwa_Wyrobu(Wyrob) & " nr" & Integer'Image(Numer) & " na polke");
					if Mozna_Przyjac(Wyrob) then
						Magazyn(Wyrob) := Magazyn(Wyrob) + 1;
						W_Magazynie := W_Magazynie + 1;
						Czy_Przyjeto := True;
						Put_Line("B [info]: PRZYJETO: " & Nazwa_Wyrobu(Wyrob) & " na polke");
						Licznik_Nie_Przyjeto := 0;
					else
						Czy_Przyjeto := False;

						Licznik_Nie_Przyjeto := Licznik_Nie_Przyjeto + 1;
						if Licznik_Nie_Przyjeto >= 3 then
							Put_Line("B [warn]: Nie mozna dodac skladnika 3-ci raz. Mozliwe zakleszczenie. Usuwam skladnik, ktorego jest wiecej niz reszta.");

							Max_Wyrob := 0;
							Max_Wyrob_Rodzaj := 1;
							for W in Zakres_Wyrobow
							loop
								if Magazyn(W) > Magazyn(Max_Wyrob_Rodzaj) then
									Max_Wyrob := Magazyn(W);
									Max_Wyrob_Rodzaj := W;
								end if;
							end loop;
							Usun_Skladnik(Max_Wyrob_Rodzaj);
							Licznik_Nie_Przyjeto := 0;
							Sklad_Magazynu;
						end if;
					end if;
				end Przyjmij;

			end select;
			Sklad_Magazynu;
		end loop;
	end TBufor;

begin
	--Put_Line("Hello, World!");

	for i in 1 .. Liczba_Wyrobow
	loop
		Producenci(i).Zacznij(i, 10);
	end loop;

	for j in 1 .. Liczba_Klientow
	loop
		Klienci(j).Zacznij(j, 12);
	end loop;
end Paczucha;
