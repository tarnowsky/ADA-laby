with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;


procedure Symulacja is
   Liczba_Wyrobow: constant Integer := 5;
   Liczba_Zestawow: constant Integer := 3;
   Liczba_Konsumentow: constant Integer := 2;
   subtype Zakres_Czasu_Produkcji is Integer range 3 .. 6;
   subtype Zakres_Czasu_Konsumpcji is Integer range 4 .. 8;
   subtype Typ_Wyrobow is Integer range 1 .. Liczba_Wyrobow;
   subtype Typ_Zestawow is Integer range 1 .. Liczba_Zestawow;
   subtype Typ_Konsumenta is Integer range 1 .. Liczba_Konsumentow;
   Nazwa_Wyrobu: constant array (Typ_Wyrobow) of String(1 .. 6)
     := ("Salata", "Ser", "Szynka", "Bulka ", "Chleb ");
   Nazwa_Zestawu: constant array (Typ_Zestawow) of String(1 .. 14)
     := ("Kanapka Drwala", "Kanapka Jarska", "Bulka Serowa");
   package Losowa_Konsumpcja is new
     Ada.Numerics.Discrete_Random(Zakres_Czasu_Konsumpcji);
   package Losowy_Zestaw is new
     Ada.Numerics.Discrete_Random(Typ_Zestawow);

   task type Producent is
      entry Zacznij(Wyrob: in Typ_Wyrobow; Czas_Produkcji: in Integer);
   end Producent;

   task type Konsument is
      entry Zacznij(Numer_Konsumenta: in Typ_Konsumenta;
                    Czas_Konsumpcji: in Integer);
   end Konsument;

   task type Bufor is
      entry Przyjmij(Wyrob: in Typ_Wyrobow; Numer: in Integer);
      entry Wydaj(Zestaw: in Typ_Zestawow; Numer: out Integer; Wydano: out Boolean);
   end Bufor;

   P: array ( 1 .. Liczba_Wyrobow ) of Producent;
   K: array ( 1 .. Liczba_Konsumentow ) of Konsument;
   B: Bufor;

   task body Producent is
      package Losowa_Produkcja is new
        Ada.Numerics.Discrete_Random(Zakres_Czasu_Produkcji);
      G: Losowa_Produkcja.Generator;	--  generator liczb losowych
      Nr_Typu_Wyrobu: Integer;
      Numer_Wyrobu: Integer;
      Produkcja: Integer;
      PROD: Integer;
   begin
      accept Zacznij(Wyrob: in Typ_Wyrobow; Czas_Produkcji: in Integer) do
         Losowa_Produkcja.Reset(G);	--  zacznij generator liczb losowych
         Numer_Wyrobu := 1;
         Nr_Typu_Wyrobu := Wyrob;
         Produkcja := Czas_Produkcji;
         PROD := Losowa_Produkcja.Random(G);
      end Zacznij;
      Put_Line("PRODUCENT: Rozpoczeto wyrob komponentu " & Nazwa_Wyrobu(Nr_Typu_Wyrobu));
      loop
         Put_Line("PRODUCENT: Przewidywany czas produkcji to " & Integer'Image(PROD) & " s");
         delay Duration(PROD);
         Put_Line("PRODUCENT: Wyprodukowano komponent " & Nazwa_Wyrobu(Nr_Typu_Wyrobu)
                  & " numer "  & Integer'Image(Numer_Wyrobu));
         -- Wstaw do magazynu
         -- ZMIANA
         loop
            select
               B.Przyjmij(Nr_Typu_Wyrobu, Numer_Wyrobu);
               Numer_Wyrobu := Numer_Wyrobu + 1;
               exit;
            else
               Put_Line("BUFOR: Bufor jest zajety, poczekaj 3s.");
               delay Duration(3.0);
            end select;
         end loop;
         -- END
      end loop;
   end Producent;

   task body Konsument is
      G: Losowa_Konsumpcja.Generator;	--  generator liczb losowych (czas)
      G2: Losowy_Zestaw.Generator;
      Nr_Konsumenta: Typ_Konsumenta;
      Numer_Zestawu: Integer;
      Konsumpcja: Integer;
      Rodzaj_Zestawu: Integer;
      CZAS: Integer;
      Wydano: Boolean;
      Nazwa_Konsumenta: constant array (1 .. Liczba_Konsumentow)
        of String(1 .. 9)
        := ("Pani Asia", "Pan Janek");
   begin
      accept Zacznij(Numer_Konsumenta: in Typ_Konsumenta;
                     Czas_Konsumpcji: in Integer) do
         Losowa_Konsumpcja.Reset(G);	--  ustaw generator
         Losowy_Zestaw.Reset(G2);
         Nr_Konsumenta := Numer_Konsumenta;
         Konsumpcja := Czas_Konsumpcji;
         CZAS := Losowa_Konsumpcja.Random(G);
         Wydano := false;
      end Zacznij;
      -- ZMIANA
      Put_Line("KONSUMENT: Konsument to " & Nazwa_Konsumenta(Nr_Konsumenta));
      Put_Line("KONSUMENT: Przewidywany czas konsumpcji to " & Integer'Image(CZAS) & " s.");
      select
         delay Duration(20.0);
         Put_Line("BUFOR: Klient zrezygnowal z zamowienia");
      then abort
         loop
            delay Duration(CZAS); --  symuluj konsumpcje
            Rodzaj_Zestawu := Losowy_Zestaw.Random(G2);
            Put_Line("KONSUMENT: " & Nazwa_Konsumenta(Nr_Konsumenta) & " zamowil/a " & Nazwa_Zestawu(Rodzaj_Zestawu));
            -- pobierz zestaw do konsumpcji
            loop
               select
                  B.Wydaj(Rodzaj_Zestawu, Numer_Zestawu, Wydano);
                  if Wydano then
                     Put_Line("KONSUMENT: " & Nazwa_Konsumenta(Nr_Konsumenta) & ": pobrano zestaw " &
                                Nazwa_Zestawu(Rodzaj_Zestawu) & " numer " &
                                Integer'Image(Numer_Zestawu + 1));
                     exit;
                  else
                     Put_Line("BUFOR: Brakuje skladnikow do zlozenia kanapki");
                     delay Duration(10.0);
                  end if;
               else
                  Put_Line("BUFOR: Prosze poczekac w kolejce!");
                  delay Duration(5.0);
               end select;
            end loop;
         end loop;
      end select;
   end Konsument;

   task body Bufor is
      Wyjatek: exception;
      Pojemnosc_Magazynu: constant Integer := 30;
      Magazyn: array (Typ_Wyrobow) of Integer
        := (0, 0, 0, 0, 0);
      Sklad_Zestawu: array(Typ_Zestawow, Typ_Wyrobow) of Integer
        := ((0, 2, 2, 0, 1),
            (1, 2, 0, 0, 1),
            (0, 2, 0, 1, 0));
      Numer_Zestawu: array(Typ_Zestawow) of Integer
        := (1, 1, 1);
      W_Magazynie: Integer := 0;

      function Mozna_Przyjac(Wyrob: Typ_Wyrobow) return Boolean is
      begin
         if W_Magazynie >= Pojemnosc_Magazynu then
            raise Wyjatek;
            return False;
         else
            return True;
         end if;
      end Mozna_Przyjac;

      function Mozna_Wydac(Zestaw: Typ_Zestawow) return Boolean is
      begin
         for W in Typ_Wyrobow loop
            if Magazyn(W) < Sklad_Zestawu(Zestaw, W) then
               return False;
            end if;
         end loop;
         return True;
      end Mozna_Wydac;

      procedure Sklad_Magazynu is
      begin
         for W in Typ_Wyrobow loop
            Put_Line("BUFOR: Sklad magazynu: " & Integer'Image(Magazyn(W)) & " "
                     & Nazwa_Wyrobu(W));
         end loop;
      end Sklad_Magazynu;

   begin
      Put_Line("BUFOR: Bufor rozpoczal prace.");
      loop
         select
            accept Przyjmij(Wyrob: in Typ_Wyrobow; Numer: in Integer) do
               begin
                  if Mozna_Przyjac(Wyrob) then
                     Put_Line("BUFOR: Przyjeto komponent: " & Nazwa_Wyrobu(Wyrob) & " nr " &
                                Integer'Image(Numer));
                     Magazyn(Wyrob) := Magazyn(Wyrob) + 1;
                     W_Magazynie := W_Magazynie + 1;
                  else
                     Put_Line("BUFOR: Odrzucono komponent:  " & Nazwa_Wyrobu(Wyrob) & " nr " &
                                Integer'Image(Numer));
                  end if;
                  Sklad_Magazynu;
               exception
                  when Wyjatek =>
                     Put_Line("MAGAZYN: Magazyn jest przepelniony! Komponent nie zostanie przyjety!");
               end;
            end Przyjmij;
         or
            accept Wydaj(Zestaw: in Typ_Zestawow; Numer: out Integer; Wydano: out Boolean) do
               if Mozna_Wydac(Zestaw) then
                  Put_Line("BUFOR: Wydano zestaw " & Nazwa_Zestawu(Zestaw) & " nr " &
                             Integer'Image(Numer_Zestawu(Zestaw) + 1));
                  for W in Typ_Wyrobow loop
                     Magazyn(W) := Magazyn(W) - Sklad_Zestawu(Zestaw, W);
                     W_Magazynie := W_Magazynie - Sklad_Zestawu(Zestaw, W);
                  end loop;
                  Numer := Numer_Zestawu(Zestaw);
                  Numer_Zestawu(Zestaw) := Numer_Zestawu(Zestaw) + 1;
                  Wydano := true;
               else
                  Put_Line("BUFOR: Brak czesci dla zestawu " & Nazwa_Zestawu(Zestaw));
                  Numer := 0;
                  Wydano := false;
               end if;
               Sklad_Magazynu;
            end Wydaj;
         end select;
      end loop;
   end Bufor;

begin
   for I in 1 .. Liczba_Wyrobow loop
      P(I).Zacznij(I, 10);
   end loop;
   for J in 1 .. Liczba_Konsumentow loop
      K(J).Zacznij(J,12);
   end loop;
end Symulacja;


