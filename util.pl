:-module(util, [
        prvocisl_rozklad/2, nsn/3, nsd/3,
        rozdel_dle_zavorek/2,
        rozkouskuj_str_rekr/2,
        negative/2,
        div_with_default/4,
        string_vymenit_koncovku/4
]).

autor("Jakub Hroník").
/*
Modul s obecnými pomocnými funkcemi, které nesouvisí přímo s chemickou tématikou.
*/



%prvocisl_rozklad(+Cislo, -Prvocisla).        -- vrátí list prvočísel takových, že jejich součin je vstupní Cislo
prvocisl_rozklad(Cislo, Prvocisla) :- prvocisl_rozklad_pom(Cislo, 2, Prvocisla).

%nsn(+A,+B, -NejmensiSpolecnyNasobek).        -- vrátí nejmenší společný násobek obou vstupních čísel
nsn(A, B, NejmensiSpolecnyNasobek) :- prvocisl_rozklad(A, ListA), prvocisl_rozklad(B, ListB), nsn_pom(ListA, ListB, NejmensiSpolecnyNasobek).


%nsd(+A,+B, -NejvetsiSpolecnyDelitel).        -- vrátí největší společný dělitel obou vstupních čísel
nsd(A, B, NejvetsiSpolecnyDelitel) :- prvocisl_rozklad(A, ListA), prvocisl_rozklad(B, ListB), nsd_pom(ListA, ListB, NejvetsiSpolecnyDelitel).




%prochází čísla jedno po druhém a zkouší jimi dělit vstupní 'Cislo', úspěšné dělitele přidává do listu a postupuje s jimi vydělenou hodnotou 'Cislo'.
prvocisl_rozklad_pom(1, _, []) :- !.            % jakmile číslo vydělíme samo sebou, zatrhneme rekurzi

prvocisl_rozklad_pom(Cislo, Delitel, Vysl) :-  
        ZbytekPoDeleni is Cislo mod Delitel, ZbytekPoDeleni =:= 0 
                                    -> PoVydeleni is Cislo div Delitel, Vysl = [Delitel|Zbytek] , prvocisl_rozklad_pom(PoVydeleni, Delitel, Zbytek)     % našli jsme obsažené prvočíslo
                                    ; Inc is Delitel + 1, prvocisl_rozklad_pom(Cislo, Inc, Vysl).               % aktuálníčíslo v prohledávavém není obsaženo

%soucin_listu(+[Cisla], -Soucin).    -- Vrátí součin čísel v listu, nebo 1 pro prázdný list.
soucin_listu([], 1).
soucin_listu([0|_], 0) :- !.   % optimalizace výkonu - na hodnotu výsledku by nemělo mít vliv
soucin_listu([H|Zb], Soucin) :- soucin_listu(Zb, SoucinZbytku), Soucin is H * SoucinZbytku.



%nejmenší společný násobek dvou čísel reprezentovaných jejich prvočíselnými rozklady (listy prvočísel seřazených od nejmenšího po největší)
nsn_pom(L,[], Soucin) :- soucin_listu(L, Soucin).
nsn_pom([], L, Soucin) :- soucin_listu(L, Soucin).
nsn_pom([X|ZbA], [X|ZbB], Vysl) :- nsn_pom(ZbA, ZbB, ZbDel), Vysl is X*ZbDel, !.        % pokud je stejné prvočíslo v rozkladech obou dvou čísel, zahrneme ho společně za obě čísla
nsn_pom([A|ZbA], [B|ZbB], Vysl) :- A < B 
                                    -> nsn_pom(ZbA, [B|ZbB], VyslZb), Vysl is A*VyslZb  % prvočíslo, co je jen v jednom z čísel, zahrneme zvlášť -
                                    ;  nsn_pom([A|ZbA], ZbB, VyslZb), Vysl is B*VyslZb. %  - jelikož jsou prvočísl. rozklady seřazené, máme jistotu, že ve druhém čísle dané prvočíslo obsaženo není



%největší společný dělitel dvou čísel reprezentovaných jejich prvočíselnými rozklady (listy prvočísel seřazených od nejmenšího po největší)
nsd_pom(_,[], 1).
nsd_pom([], _, 1).
nsd_pom([X|ZbA], [X|ZbB], Vysl) :- nsd_pom(ZbA, ZbB, ZbDel), Vysl is X*ZbDel, !.        % pokud je stejné prvočíslo v rozkladech obou dvou čísel, zahrneme ho
nsd_pom([A|ZbA], [B|ZbB], Vysl) :- A < B 
                                    -> nsd_pom(ZbA, [B|ZbB], Vysl)       %prvočísla, která jsou v rozkladu jenom jednoho z obou čísel, přeskočíme 
                                    ;  nsd_pom([A|ZbA], ZbB, Vysl).




%pomocné funkce pro pohodlnější určování typu znaků
is_lower(X) :- char_type(X, lower).
is_upper(X) :- char_type(X, upper).
is_num(X) :- char_type(X, digit).


% number_atomic(-Num, +Atom). -- přeparsuje libovolný atomický objekt na číslo
number_atomic(Num, Num) :- number(Num).
number_atomic(Num, Str) :- string(Str), number_string(Num, Str).
number_atomic(Num, Atom) :- atom(Atom), atom_string(Atom, String), number_atomic(Num, String).






%rozdel_dle_zavorek(+Str, - Vysl).
% -- Rozdělí řetězec podle závorek, co v něm jsou napsané
%  - vrací list, který obsahuje podřetězce (odpovídající neuzávorkovaným pasážím) a listy vytvořené rekurzivním voláním na pasáže uvnitř závorek
%  - např. rozdel_dle_zavorek("Fe((NH4)2O2)2", ["Fe", [["NH4"], "2O2"], '2'])
rozdel_dle_zavorek(Str, Vysl) :- string_chars(Str, Chars), rozdel_dle_zavorek_impl(Chars, _, Vysl).



%rozdel_dle_zavorek_impl(+ListZnaku, -ZbytekPoVyreseniZavorky, -VyslList). -- ZbytekPoVyreseniZavorky... pomocný argument, skrze který predikát rekurzivně zavolaný na vnitřek závorek předá
%                                                                                                        volající instanci řetězec, který z celkového řetězce zbyl po sežrání obsahu závorky
rozdel_dle_zavorek_impl([], [], []).

rozdel_dle_zavorek_impl(['('|Zb], Zbytek, [Vnitrek|KonecListu]) :- rozdel_dle_zavorek_impl(Zb, ZaZavorkou, Vnitrek), rozdel_dle_zavorek_impl(ZaZavorkou, Zbytek, KonecListu).
rozdel_dle_zavorek_impl([')'|Zb], Zb, []).

rozdel_dle_zavorek_impl([H|Zb], Zbytek, Vysl) :- H \= '(', H \= ')', (rozdel_dle_zavorek_impl(Zb, Zbytek, [X|Xz]) ->
                                                                 (atomic(X) -> 
                                                                        string_concat(H, X, VyslStr), Vysl = [VyslStr|Xz]
                                                                        ; Vysl = [H|[X|Xz]]) 
                                                                ; rozdel_dle_zavorek_impl(Zb, Zbytek, []), Vysl = [H]).




%rozkouskuj_str(+StringKRozkouskovani, -ListKusu).
%Rozdělí vstupní řetězec alfanumerických znaků na list jeho kusů tak, že číselné a písmenné pasáže jsou vždy zvlášť; nová písmenná pasáž začíná právě přečtením velkého písmene
%  - např. rozkouskuj_str("Na2OH12", ["Na", 2, "O", "H", 12]).
%          rozkouskuj_str("a2c", ?) je nedefinovaný stav (může vrátit cokoliv), jelikož tu je písmenná pasáž (dokonce 2krát) nezačínající velkým písmenem
rozkouskuj_str(Str, List) :- string_chars(Str, CharList), rozkouskuj_str_impl(CharList, "", char, [""|StrList]), zcislovatet_co_jde(StrList, List).


%rozkouskuj_str_impl(+[C|CharList], +Mezivysledek, +StavovyFlag, -Vysl).
rozkouskuj_str_impl([], Mezivysl, _, [Mezivysl]).
rozkouskuj_str_impl([H|Zb], Mezivysl, _, [Mezivysl|Vysl]) :- is_upper(H), rozkouskuj_str_impl(Zb, H, char, Vysl), !.
rozkouskuj_str_impl([H|Zb], Mezivysl, char, [Mezivysl|Vysl]) :- is_num(H), rozkouskuj_str_impl(Zb, H, num, Vysl), !.
rozkouskuj_str_impl([H|Zb], Mezivysl, Flag, Vysl) :- string_concat(Mezivysl, H, NovyMezivysl), rozkouskuj_str_impl(Zb, NovyMezivysl, Flag, Vysl).


%zcislovatet_co_jde(+NesporadanyList, -ListVPoradku).
%                       převede prvky listu, které lze převést, na číselné hodnoty; pro všechny ostatní zaručí, že jsou typu string
zcislovatet_co_jde([],[]).
zcislovatet_co_jde([H|Zb], [Num|Vysl]) :- (number_atomic(Num, H) -> true; atom_string(H, Num)), zcislovatet_co_jde(Zb, Vysl).


%rozkouskuj_str_rekr(+ListStringuKRozkouskovani, -ListKusu).
%  Převezme list řetězců a podlistů, jako vrací predikát rozdel_dle_zavorek, a rekurzivně všechny jeho prvky roztrahá na fragmenty dle predikátu rozkouskuj_str
%  např. rozkouskuj_str_rekr(["Fe", [["NH4"], "2O2"], '2'] ,  ["Fe", [["N", "H", 4], 2, "O", 2], 2]).
rozkouskuj_str_rekr([],[]).
rozkouskuj_str_rekr([X|Zb], Vysl) :- atomic(X) 
                                        -> rozkouskuj_str(X, RozkouskovanyX), rozkouskuj_str_rekr(Zb, VyslZb), append(RozkouskovanyX, VyslZb, Vysl)
                                        ; rozkouskuj_str_rekr(X, RekrX), rozkouskuj_str_rekr(Zb, VyslZb), Vysl = [RekrX|VyslZb].



%div_with_default(+Delenec, +Delitel, +Default, -Vysl).  
%   Vrátí Delenec div Delitel, ale pokud by mělo nastat dělení nulou, vrátí místo toho Default
div_with_default(Delenec, Delitel, Default, Vysl) :- Delitel = 0 -> Vysl = Default ; Vysl is Delenec div Delitel.


% zaručí, že oba parametry jsou hodnotami vzájemně inverzními vůči sčítání; pokud jsou obě hodnoty proměnné, selže
negative(X, N) :- var(X) -> (var(N) ->fail ; X is -N); N is -X.


%string_vymenit_koncovku(+-Zaklad, +PuvKoncovka, +NovaKoncovka, -+Vysl).
string_vymenit_koncovku(Zaklad, PuvKoncovka, NovaKoncovka, Vysl) :- var(Vysl) -> string_concat(Koren, PuvKoncovka, Zaklad), string_concat(Koren, NovaKoncovka, Vysl)
                                                                         ; string_concat(Koren, NovaKoncovka, Vysl), string_concat(Koren, PuvKoncovka, Zaklad).