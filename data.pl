:- module(data, [
        prvek/3,      % musí poskytnout seznam všech chemických prvků (zde předán z modulu seznam_prvku)
        iont_expl/2,  % musí definovat predikát pro specielní, explicitně zadané ionty
        autogen_kationty/1,  %musí definovat predikát určující, pro které prvky se mají implicitně vygenerovat základní kationty
        koncovka_kationtu/2, koncovka_kyslikate_soli/2,  %musí definovat predikát pro koncovky, skrze které se budou v závislosti na oxidačním čísle odvozovat z prvků názvy sloučenin 
        c_ROZUMNY_POCET_KYSLIKU_V_KYSELINE_NAVIC/1             %musí definovat
 ]).
:- use_module(seznam_prvku).
:- use_module(util).

autor("Jakub Hroník").

/*
Modul nesoucí data, ze kterých je názvosloví odvozováno.
*/




/*
Popis interní reprezentace chemických sloučenin:

    - term `ion(OxČíslo, Tělo)`... objekt iontu
            OxČíslo... - celkové ox. číslo sloučeniny
                       - musí být typu number
                       - pokud jde o složený iont, musí být rovno součtu ox. čísel všech složek (jinak není definováno, co se bude dít)
            
            Tělo... - buď a) textový řetězec odpovídající značce nějakého prvku
                          b) list dalších termů ion/mlt
    - term `mlt(Počet, Tělo)`... násobič výskytu iontu
            Počet... - kolikrát se tělo ve sloučenině vyskytuje
                     - musí být typu number
            Tělo... další term ion nebo mlt
    - na nejnižší úrovni vždy musí být term typu ion

    příkl.:
        - jednoprvkový iont vodíku s ox. č -1 zapíšeme `ion(-1, "H")`
        - jednoprvkový iont síry s ox. č. 6 zapíšeme `ion(6, "S")`          
        - dvouprvkovou molekulu kyslíku (`O2`) s ox. č. 0 zapíšeme `ion(0, [mlt(2, ion(0, "O"))])`
                       - `mlt(2, ion(0,"O"))` nestačí, jelikož na kořenové úrovni musí být term typu ion -> mlt do něj musíme zanořit
        - amonný kationt (`NH4[1+]`) zapíšeme `ion(1, [ion(-3, "N"), mlt(4, ion(1, "H"))])`
*/







%koncovky, jejichž přilepováním ke kořeni jména prvku budou odvozovány jména kationtů odpovídajících danému ox. číslu
%koncovka_kationtu(OxidacniCislo, Koncovka).
koncovka_kationtu(1,"ny").
koncovka_kationtu(2,"naty").
koncovka_kationtu(3,"ity").
koncovka_kationtu(4,"icity").
koncovka_kationtu(5,"icny").
koncovka_kationtu(5,"ecny").
koncovka_kationtu(6,"ovy").
koncovka_kationtu(7,"isty").
koncovka_kationtu(8,"icely").


%koncovky, jejichž přilepováním ke kořeni jména budou odvozovány jména solných aniontů odpovídajících danému ox. číslu bazického prvku
%koncovka_kyslikate_soli(OxidacniCislo, Koncovka).
koncovka_kyslikate_soli(6, "an").   %např. sůl kys. sírové je 'síran', ne 'sírovan', platí obecně
koncovka_kyslikate_soli(I, Jm) :-  koncovka_kationtu(I, JmKat), I\=6, string_vymenit_koncovku(JmKat, "y", "an", Jm).
/* %alternativně se stejným výsledkem
koncovka_kyslikate_soli(1, "nan").
koncovka_kyslikate_soli(2, "natan").
koncovka_kyslikate_soli(3, "itan").
koncovka_kyslikate_soli(4, "icitan").
koncovka_kyslikate_soli(5, "icnan").
koncovka_kyslikate_soli(5, "ecnan").
koncovka_kyslikate_soli(7, "istan").
koncovka_kyslikate_soli(8, "icelan").
*/


% počty kyslíků (navíc oproti minimu, s kterým by iont měl ox. číslo 0 nebo +1),
% které budou programem zkoušeny při odvozování kyselinových aniontů podle systematických názvů
c_ROZUMNY_POCET_KYSLIKU_V_KYSELINE_NAVIC(X) :- member(X, [1, 2, 3]).






% pro tyto prvky nebudou automaticky generovány žádné kationty - všechny musí být zadány ručně v tomto modulu
skip_auto_kationty("O").
skip_auto_kationty("H").
skip_auto_kationty("F").

% kationty budou automaticky generovány pro všechny prvky kromě těch zmíněných výše
autogen_kationty(X) :- prvek(X,_, _), \+skip_auto_kationty(X).





/*  Názorná ukázka, jak by bylo možné implementovat skupinu peroxidů.
c_ROZUMNY_POCET_PEROXIDOVYCH_KYSLIKU(X) :- member(X, [2,4,6,8,10,12]).


iont_expl(ion(-1, "O"), "kyslik[1-]").
iont_expl(ion(NOx, [mlt(Ox, ion(-1, "O"))]), "peroxidovy") :- negative(Ox,NOx) -> 0 is Ox mod 2 ; c_ROZUMNY_POCET_PEROXIDOVYCH_KYSLIKU(Ox), negative(Ox, NOx).
*/


%pro H a F jsme přeskočili automatickou generaci iontů - musíme je definovat explicitně
%  pozn.: oba prvky jsou reálně schopny nabývat pouze oxidačního čísla 1
%iont_expl(ExplicitneDefinovanyIontVInterniReprezentaci, Jmeno).
iont_expl(ion(1, "H"), "vodny").
iont_expl(ion(1, "F"), "fluorny").


iont_expl(ion(-3, "N"), "dusik[3-]"). 
  %kompozitní ionty musí sestávat jedině z iontů, jež jsou taktéž definované, jinak je program nebude schopen uhodnout; 
  %toto je tedy nutné, aby fungoval amonný kationt
  %schválně používáme divný název - kdyby tento název končil na "ovy", měl by program tendence používat tento iont jako základ pro soli

iont_expl(ion(1, [ion(-3, "N"), mlt(4, ion(1, "H"))]), "amonny").   % NH4[1+] -- amonný kationt

% zadefinováním těchto aniontů s koncovkou "ovy" jsme automaticky přidali skupiny oxidů, sulfidů a hydroxidů jako specielní případy solí 
iont_expl(ion(-2, "O"), "oxidovy").
iont_expl(ion(-2, "S"), "sulfidovy").
iont_expl(ion(-1, [ion(-2, "O"), ion(1, "H")]), "hydroxidovy").


% stejně tak nyní přidáváme skupinu halogenidů
iont_expl(ion(-1,"F"), "fluoridovy").
iont_expl(ion(-1,"Cl"), "chloridovy").
iont_expl(ion(-1,"Br"), "bromidovy").
iont_expl(ion(-1,"I"), "iodidovy").
iont_expl(ion(-1,"At"), "astatidovy").
iont_expl(ion(-1,"Ts"), "tennessinidovy").





