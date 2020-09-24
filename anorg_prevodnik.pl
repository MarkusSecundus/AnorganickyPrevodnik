
:-module(anorg_prevodnik, [
    jmeno_vzorec/2    % predikát pro obousměrný převod mezi systematickými jmény sloučenin a jejich vzorečky
  ]).
:- use_module(data).
:- use_module(util).

autor("Jakub Hroník").

/*
Hlavní modul programu, obsahující logiku pro převody mezi systematickými názvy a vzorečky chemických sloučenin.

Pro převody slouží predikát jmeno_vzorec/2.
*/



%ion(+-Iont, -+Nazev).
%  Popisuje všechny ionty, se kterými program může pracovat (uvažovat je jako složky sloučenin apod.)
%  Pro popis interní reprezentace iontů viz modul 'data'!
iont(ion(0, Vzorec), Nazev) :- prvek(Vzorec, Nazev, _).   % pro všechny prvky zadefinujeme ionty z nich sestávající s ox. číslem 0
iont(ion(OxCislo, Vzorec),  Nazev) :- iont_expl(ion(OxCislo, Vzorec), Nazev).   % všechny explicitně zadané ionty bereme jako platné ionty
%automaticky vygenerujeme implicitní ionty přidáváním přípon za kořeny jmen prvků
iont(ion(OxCislo, Vzorec), Nazev) :- autogen_kationty(Vzorec), prvek(Vzorec, _, KorenJmena),  koncovka_kationtu(OxCislo, Koncovka), string_concat(KorenJmena, Koncovka, Nazev).

% všechny dodatečně vygenerované kyselinové anionty jsou také ionty
iont(Ion, Nazev) :- kyselinovy_aniont(Ion, _, Nazev).


%pomocné klauzule zaručující, že budou akceptovány i divněji zapsané varianty základních iontů 
iont(mlt(_, X), Nazev) :- nonvar(X), iont(X, Nazev).
iont(ion(_, [X]), Nazev) :- nonvar(X), iont(X, Nazev).








%je_molekula(+-Kationt, +-Aniont, -+Jmeno).
% Schématicky popisuje složení a způsob pojmenovávání sloučenin, jež mají být programem rozpoznatelné.
%   - (Koncipované tak, že nemusí ani by nemělo brát ohledy na takové věci, jako že aniont musí mít ox. č záporné,
%      kationt kladné, a že oba musí být přítomny v takovém poměru, aby se výsledně sečetly na ox. č. 0 - to už automaticky 
%      vynucují predikáty 'je_molekula_guard' a 'je_molekula_broad', s kterými teprve reálně rozpoznávač komunikuje.
%       Je-li třeba pro specifickou sloučeninu tato pravidla porušit, bude nutné připsat klauzuli přímo k oněm predikátům.)0

% zadefinujeme existenci skupiny kyselin
je_molekula(ion(1,"H"), Aniont, Jmeno) :- kyselinovy_aniont(Aniont, Jmeno).

% zadefinujeme existenci skupiny solí (tvorbu jejich názvů řešíme přímo na místě; povšimněte si, že musí fungovat v obou směrech)
je_molekula(Kationt, Aniont, Jmeno) :- var(Jmeno) ->
                                       iont(Kationt, JmenoKationtu), iont(Aniont, JmenoAniontu), jmeno_kategorie_soli(JmenoAniontu, JmenoKategorieSoli), string_concat(JmenoKategorieSoli, " ", Titul), string_concat(Titul, JmenoKationtu, Jmeno)
                                       ;  string_concat(Titul, JmenoKationtu, Jmeno), string_concat(JmenoKategorieSoli, " ", Titul), jmeno_kategorie_soli(JmenoAniontu, JmenoKategorieSoli), iont(Kationt, JmenoKationtu), iont(Aniont, JmenoAniontu).




% zadefinujeme existenci prvků jako skupiny sloučenin
%   - pozn. zde chceme, aby oxidační číslo kationtu bylo rovno 0 - musíme připsat jako klauzuli 'je_molekula_guard'
je_molekula_guard(ion(0, Prv), ion(0,[]), Jmeno) :- prvek(Prv, Jmeno, _).








% wrapper nad 'je_molekula', který vylučuje varianty, kde má kationt nekladné a aniont nezáporné ox. č.
je_molekula_guard(ion(OxK, Kat), ion(OxA, An), Jmeno) :- var(OxK) -> 
                                                                    je_molekula(ion(OxK, Kat), ion(OxA, An), Jmeno), OxK > 0, OxA < 0 
                                                                    ; OxK > 0, OxA < 0, je_molekula(ion(OxK, Kat), ion(OxA, An), Jmeno).


% wrapper nad 'je_molekula_guard', který rozšiřuje funkčnost schématu i na divněji zapsané varianty
je_molekula_broad(Kationt, Aniont, Jmeno) :- je_molekula_guard(Kationt, Aniont, Jmeno).
je_molekula_broad(Kationt, ion(_,[mlt(_, Aniont)]), Jmeno) :- je_molekula_guard(Kationt, Aniont, Jmeno).
je_molekula_broad(ion(_,[mlt(_, Kationt)]), Aniont, Jmeno) :- je_molekula_guard(Kationt, Aniont, Jmeno).
je_molekula_broad(ion(_,[mlt(_, Kationt)]),ion(_,[mlt(_,  Aniont)]), Jmeno) :- je_molekula_guard(Kationt, Aniont, Jmeno).




%jmeno_kategorie_soli(+-JmenoIontu, -+JmenoKategorieSoli).  -- Podle jména iontu vytvoří jméno typu soli, která mu odpovídá, a obráceně.
jmeno_kategorie_soli(JmenoIontu, JmenoKategorieSoli) :- string_concat(JmenoKategorieSoli,"ovy",JmenoIontu).



%jmeno_kyseliny(+-JmenoIontu, -+JmenoKyseliny).  -- Podle jména kyselinového aniontu vytvoří jméno kyseliny, která by mu odpovídala po přidání příslušného množství vodíků, a obráceně.
jmeno_kyseliny(JmenoIontu, JmenoKyseliny):- nonvar(JmenoIontu) -> string_vymenit_koncovku(JmenoIontu, "y", "a", TitulKyseliny_), string_concat("kyselina ", TitulKyseliny_, JmenoKyseliny)
                                            ; string_concat("kyselina ", TitulKyseliny_, JmenoKyseliny), string_vymenit_koncovku(JmenoIontu, "y", "a", TitulKyseliny_).


%kyselinovy_aniont(+-Aniont, -+JmenoKyseliny).  -- Podle objektu kyselinového aniontu v interní reprezentaci zjistí jméno kyseliny, která by mu odpovídala po přidání příslušného množství vodíků, a obráceně.
  % - Pokud je zadáno jméno, dohodí co nejvíce variant, které mu odpovídají - jak hodně jich bude, záleží na datovém predikátu 'c_ROZUMNY_POCET_KYSLIKU_V_KYSELINE_NAVIC'.
kyselinovy_aniont(ion(Ox, [Prv, mlt(Kysliku, ion(-2, "O"))]), JmenoKys) :-  var(JmenoKys) ->
                                                                             iont(Prv, JmenoIontu), get_ox_cislo(Prv, OxPrv), OxPrv > 0,
                                                                             (nonvar(Ox)->
                                                                                Kysliku is (OxPrv-Ox) div 2
                                                                              ; Ox is -2*Kysliku + OxPrv
                                                                             )
                                                                              ,Ox < 0, Kysliku > 0
                                                                              , jmeno_kyseliny(JmenoIontu, JmenoKys)
                                                                            
                                                                            ; jmeno_kyseliny(JmenoIontu, JmenoKys),
                                                                             iont(Prv, JmenoIontu), get_ox_cislo(Prv, OxPrv), OxPrv > 0,
                                                                             (nonvar(Ox)->
                                                                                Kysliku is (OxPrv-Ox) div 2
                                                                              ;
                                                                              (var(Kysliku) ->
                                                                                ZaklKysliku is OxPrv div 2,
                                                                                c_ROZUMNY_POCET_KYSLIKU_V_KYSELINE_NAVIC(KyslikuNavic),
                                                                                Kysliku is ZaklKysliku + KyslikuNavic
                                                                                ; true),
                                                                                 Ox is -2*Kysliku + OxPrv
                                                                             ),
                                                                              Ox < 0, Kysliku > 0.

% pomocná klauzule - aby byl akceptován i tvar, kdy je O[2-] jen jeden a není wrapován do mlt(1,..).
kyselinovy_aniont(ion(Ox, [Prv, ion(-2, "O")]), JmenoKys) :- kyselinovy_aniont(ion(Ox, [Prv, mlt(1, ion(-2, "O"))]), JmenoKys).


%kyselinovy_aniont(Ion, JmenoKys, JmenoAniontu).   -- alespoň 1 argument musí být instanciován.
  % - Obdoba dvouparametrové varianty, která zároveň odvozuje i jméno kyselinového aniontu (/ ze jména kys. aniontu)  
kyselinovy_aniont(Ion, JmenoKys, JmenoAniontu) :- (nonvar(JmenoAniontu)->
      koncovka_kyslikate_soli(I, Koncovka), string_concat(KorenJmenaAniontu, "ovy", JmenoAniontu), string_concat(Koren, Koncovka, KorenJmenaAniontu), koncovka_kationtu(I, KoncovkaKat),
       string_vymenit_koncovku(KoncovkaKat, "y", "a", KoncovkaKyseliny),
        string_concat("kyselina ", Koren, HJmKys), string_concat(HJmKys, KoncovkaKyseliny, JmenoKys), kyselinovy_aniont(Ion, JmenoKys)
      ;
        kyselinovy_aniont(Ion, JmenoKys), string_concat(HJmKys, KoncovkaKyseliny, JmenoKys),string_concat("kyselina ", Koren, HJmKys),
        koncovka_kationtu(I, KoncovkaKat), string_vymenit_koncovku(KoncovkaKat, "y", "a", KoncovkaKyseliny),
        koncovka_kyslikate_soli(I, Koncovka), string_concat(Koren, Koncovka, KorenJmenaAniontu), string_concat(KorenJmenaAniontu, "ovy", JmenoAniontu)
      ).



%to_s(+Iont, -Vzorec).
  % Pro libovolnou chemickou sloučeninu v interní reprezentaci (ion(..,..)) vygeneruje odpovídající chemický vzorec. Funguje pouze jednosměrně.
to_s(ion(_,[]), "") :- !.

to_s(ion(_, [H|Zb]), Str) :- to_s(H, ZacatekStr), to_s(ion(0, Zb), ZbStr), string_concat(ZacatekStr, ZbStr, Str), !.
to_s(ion(_, Prvek), Prvek).     % pokud Prvek není list (to není - víme díky řezu výše), musí být řetězcem obsahujícím jméno prvku

%jednotkový násobič je zbytečný a úplně ho zredukujeme
to_s(mlt(1, X), Str) :- to_s(X, Str), !.    

% vnořené násobiče zredukujeme do jednoho
to_s(mlt(I, mlt(J, X)), Str) :- Mlt is I * J, to_s(mlt(Mlt, X), Str).

%několikanásobnou složenou skupiny vypíšeme dovnitř závorek a za ní napíšeme počet výskytů
to_s(mlt(I, ion(Ox, [H|Zb])), Str) :- to_s(ion(Ox, [H|Zb]), Telo), string_concat("(", Telo, Zacatek), number_string(I, IAsNum), string_concat(")", IAsNum, Konec), string_concat(Zacatek, Konec, Str), !.

%za několikanásobný prvek jenom napíšeme počet výskytů, ale nebudeme ho dávat do závorek
to_s(mlt(I, ion(_, Prvek)), Str) :- number_string(I, IAsNum), string_concat(Prvek, IAsNum, Str).



%na_ionty(+Str, -Iont).
    % Převede vzoreček sloučeniny na odpovídající interní reprezentaci. Funguje pouze jednosměrně.
na_ionty(Str, Iont) :- rozdel_dle_zavorek(Str, Odzavorkovany), rozkouskuj_str_rekr(Odzavorkovany, List), na_ionty_impl(List, Iont).


% Bere list symbolů (písmenných/ číselných částí / podlistů (pro ozávorkované pasáže)) a vrací odpovídající ion v interní reprezentaci (všechny platné varianty).
na_ionty_impl([],ion(0,[])).

% textovému řetězci nemůže odpovídat nic, než koncový iont - zkusíme všechny, které mohou odpovídat. 
na_ionty_impl(Str, ion(Ox,Str)) :- atomic(Str), iont(ion(Ox, Str), _).

% pokud je za aktuálním prvkem číselná hodnota, je to násobič, který na aktuální prvek musíme uplatnit.
na_ionty_impl([H|[X|Zb]], ion(Ox,[mlt(X, ion(OxC, Ion))|Vysl]) ) :- number(X), na_ionty_impl(H, ion(OxC, Ion)), na_ionty_impl(Zb, ion(OxZb,Vysl)), Ox is X * OxC + OxZb.

na_ionty_impl([H|Zb], ion(Ox,[ion(OxC, Ion)|Vysl])) :- na_ionty_impl(H, ion(OxC, Ion)), na_ionty_impl(Zb, ion(OxZb, Vysl)), Ox is OxC + OxZb.





%normalizuj_impl(+Iont, -NormalizovanyIont).
%Převede zápis iontu v interní reprezentaci na co nejméně obskurní tvar interní reprezentace.
normalizuj_impl(X,X) :- string(X).
normalizuj_impl(ion(Ox, X), ion(Ox, NormX)) :- normalizuj_impl(X, NormX).
normalizuj_impl(mlt(1, X), NormX) :- normalizuj_impl(X, NormX).

normalizuj_impl([ion(_, X)], NormX) :- normalizuj_impl(X, NormX), !.
normalizuj_impl([X], [NormX]) :- normalizuj_impl(X, NormX).
normalizuj_impl([],[]).
normalizuj_impl([H,Hz], [NH,NZb]) :- normalizuj_impl(H, NH), normalizuj_impl(Hz, NZb).
normalizuj_impl([H|[Hz|Zb]], [NH|NZb]) :- Zb \= [], normalizuj_impl(H, NH), normalizuj_impl([Hz|Zb], NZb).

normalizuj_impl(mlt(Ox, X), mlt(NormOx, NormX)) :- Ox =\= 1, normalizuj_impl(X,PonormX), (PonormX = mlt(Ox2, NormX) -> NormOx is Ox * Ox2 ;  NormX = PonormX, NormOx = Ox).


%norm(+Iont, -NormalizovanyIont).
% Iteruje normalizaci iontu, dokud má nějaký efekt.
norm(A,B) :- normalizuj_impl(A, Vysl), (Vysl = A -> B = A; norm(Vysl, B)).


%rozdel_na_kationt_a_aniont(+List, -KationtNorm, -AniontNorm).
        % Přesekne daný list iontů na 2 části a z těch vytvoří ionty v interní reprezentaci v normálním tvaru.
rozdel_na_kationt_a_aniont(List, KationtNorm, AniontNorm) :- append(Kationt, Aniont, List),
                                                 soucet_ox_cisel(Kationt, OxKat), soucet_ox_cisel(Aniont, OxAn),
                                                 norm(ion(OxKat, Kationt), KationtNorm), norm(ion(OxAn, Aniont), AniontNorm).

%vzorec_na_jmeno(+Vzorec, -Jmeno).
        % Jednosměrný převodník chemického vzorce na sytematický název odpovídající sloučeniny
        %  - může vyhodit mnoho variant, i jednu variantu vícekrát
vzorec_na_jmeno(Vzorec, Jmeno) :- na_ionty(Vzorec, ion(0, List)), rozdel_na_kationt_a_aniont(List, KationtNorm, AniontNorm), je_molekula_broad(KationtNorm, AniontNorm, Jmeno).

%jmeno_na_vzorec(+Jmeno, -Vzorec).
        % Jednosměrný převodník systematického názvu sloučeniny na odpovídající vzoreček.
        %  - může vyhodit mnoho variant, i jednu variantu vícekrát
jmeno_na_vzorec(Jmeno, Vzorec) :- je_molekula_guard(Kat, An, Jmeno),
                                  get_ox_cislo(Kat, OxK), get_ox_cislo(An, OxA), MinusOxA is -OxA, nsn(OxK, MinusOxA, CelkoveOxCislo),
                                  div_with_default(CelkoveOxCislo, OxK, 1, PocetKationtu), div_with_default(CelkoveOxCislo, MinusOxA, 1, PocetAniontu),
                                   norm(ion(0, [mlt(PocetKationtu, Kat), mlt(PocetAniontu, An)]), NormSloucenina),
                                   to_s(NormSloucenina, Vzorec).



%jmeno_vzorec(+-Jmeno, -+Vzorec).   -- obousměrný převod mezi systematickým názvem sloučeniny a jejím vzorečkem; vždy alespoň 1 argument musí být plně instanciován
jmeno_vzorec(Jmeno, Vzorec) :- var(Jmeno) ->
                                            setof(Jm, vzorec_na_jmeno(Vzorec, Jm), Jmena),  member(Jmeno, Jmena)
                                           ;setof(Vz, jmeno_na_vzorec(Jmeno, Vz), Vzorce),  member(Vzorec, Vzorce). 




%get_ox_cislo(+Ion, -OxCislo).    -- získá oxidační číslo daného iontu nebo množiny iontů
get_ox_cislo(ion(Ox, _), Ox).
get_ox_cislo(mlt(Mlt, X), Vysl) :- get_ox_cislo(X, Ox), Vysl is Mlt * Ox.

%soucet_ox_cisel(+[Ion], -OxCislo).  -- spočítá součet oxidačních čísel všech iontů v listu
soucet_ox_cisel([], 0).
soucet_ox_cisel([H|Zb], Vysl) :- get_ox_cislo(H, Ox), soucet_ox_cisel(Zb, OxZb), Vysl is Ox + OxZb.



