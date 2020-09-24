Převodník mezi vzorečky a systematickými názvy anorganických chemických sloučenin  
===================================================================================

#### _Napsaný v [Prologu](https://www.swi-prolog.org/)_  

&nbsp;   
&nbsp;   
&nbsp;   
&nbsp;   

**Použití...**
- Stačí načíst hlavní modul [_**anorg_prevodnik.pl**_][anorg prevodnik] a zavolat jeho predikát [**`jmeno_vzorec/2`**][predikat jmeno vzorec],
který obstarává obousměrný převod.  
  - Alespoň jeden z argumentů nutně musí být poskytnut, jinak predikát vždy selže.

 - Podporované sloučeniny: základní kyslíkaté kyseliny, jejich soli, oxidy, hydroxidy, sulfidy, halogenidy (implementované jako specielní případy solí)

 - Vzorečky používají intuitivní formát zápisu - značka prvku příp. následovaná číslem udávajícím počet výskytů, za ní další zn. prvku atd., případné složeniny (kyselinové anionty, amonný kationt,...), pokud se vyskytují násobně, jsou zapsány do závorky následované kvantifikátorem (např. 'NaCl', 'H2SO4', '(NH4)2CO3', 'Fe2(SO4)3', ...)

 - Vytvořené názvy jsou v některých případech lehce kostrbaté, jelikož vystihnout všechny mírné odchylky od striktního názvoslovného pravidla k pěkněji vyslovitelným reálně používaným tvarům by mi dalo více práce, než za kolik to stojí.
    
 - Za kyseliny odpovídající danému jménu jsou považovány všechny, které splňují ze jména plynoucí ox. číslo - tedy vyžádáte-li
      např. kys. sírovou, dostanete H2SO4, ale zároveň i H4SO5 atd. . Stejně tak ale díky tomu pro kys. fosforečnou dostanete vedle
      neexistující HPO3 i správnou H3PO4. Jak hodně kyslíků navíc bude program zkoušet dohazovat, lze ovlivnit skrze predikát
      [**`c_ROZUMNY_POCET_KYSLIKU_V_KYSELINE_NAVIC`**][predikat rozumny p kysl] v modulu modul [_**`data.pl`**_][data pl].  
      Obdobné platí i pro soli daných kyselin.
  
&nbsp;     

**Přidávání dat...**
  - **modul [_**data.pl**_][data pl]**  
    - seznam všech chemických prvků
      - pro přehlednost definovány zvlášť v modulu [_**seznam_prvku.pl**_][seznam prvku]
      - modul [`data.pl`][data pl] ho importuje a predikát `prvek/3` exportuje dál
    - koncovky pro automatickou tvorbu jmen iontů
    - seznam prvků, pro něž se mají automaticky generovat kationty (vygenerují se vždy se všemi oxidačními čísly)
    - seznam explicitně definovaných iontů  

  - **Přidávání nových skupin prvků...**
    - pro implementaci úplně nové skupiny prvků je třeba přidat novou klauzuli k predikátu [`je_molekula/3`][predikat je molekula]
    - drtivou většinu skupin (_kyslíkaté kyseliny_, _oxidy_, _sulfidy_, _halogenidy_, ...) lze implementovat jako specielní případ solí
      - sůl je jakákoliv sloučenina libovolného kationtu s aniontem , jehož jméno má koncovku `"ovy"`
        - jméno soli vznikne takto: `(JmenoAniontu - "ovy") + " " + JmenoKationtu`  
          (např. `("siranovy" - "ovy") + " " + "sodny"  -> "siran sodny"`)
      - pro přidání nové skupiny solí tedy stačí přidat novou klauzuli k predikátu [`iont_expl/2`][predikat iont expl]
        - např.:   
            skupinu _oxidů_ přidáme takto: [`iont_expl(ion(-2, "O"), "oxidovy").`][ukazka impl oxidu]   
            obdobně _hydroxidy_: [`iont_expl(ion(-1, [ion(-2, "O"), ion(1, "H")]),"hydroxidovy").`][ukazka impl hydroxidu]   
            [ukázka složitější implementace peroxidů][ukazka impl peroxidu]  
            apod.  
        - pro podrobnější instrukce k zápisu iontů v interní reprezentaci [viz][popis interni repr iontu].







[data pl]: https://github.com/MarkusSecundus/AnorganickyPrevodnik/blob/master/data.pl
[anorg prevodnik]: https://github.com/MarkusSecundus/AnorganickyPrevodnik/blob/master/anorg_prevodnik.pl
[seznam prvku]: https://github.com/MarkusSecundus/AnorganickyPrevodnik/blob/master/seznam_prvku.pl

[popis interni repr iontu]: https://github.com/MarkusSecundus/AnorganickyPrevodnik/blob/master/data.pl#LC21:~:text=Popis%20intern%C3%AD%20reprezentace%20chemick%C3%BDch%20slou%C4%8Denin%3A

[predikat jmeno vzorec]: https://github.com/MarkusSecundus/AnorganickyPrevodnik/blob/master/anorg_prevodnik.pl#LC222:~:text=instanciov%C3%A1n-,jmeno_vzorec
[predikat je molekula]: https://github.com/MarkusSecundus/AnorganickyPrevodnik/blob/master/anorg_prevodnik.pl#LC41:~:text=%25je_molekula(%2B%2DKationt%2C%20%2B%2DAniont%2C%20%2D%2BJmeno).
[predikat iont expl]: https://github.com/MarkusSecundus/AnorganickyPrevodnik/blob/master/data.pl#LC111:~:text=%25iont_expl(ExplicitneDefinovanyIontVInterniReprezentaci%2C%20Jmeno).
[predikat rozumny p kysl]: https://github.com/MarkusSecundus/AnorganickyPrevodnik/blob/master/data.pl#LC81:~:text=n%C3%A1zv%C5%AF-,c_ROZUMNY_POCET_KYSLIKU_V_KYSELINE_NAVIC

[ukazka impl oxidu]: https://github.com/MarkusSecundus/AnorganickyPrevodnik/blob/master/data.pl#LC124:~:text=iont_expl(ion(%2D2%2C%20%22O%22)%2C%20%22oxidovy%22).
[ukazka impl hydroxidu]: https://github.com/MarkusSecundus/AnorganickyPrevodnik/blob/master/data.pl#LC126:~:text=iont_expl(ion(%2D1%2C%20%5Bion(%2D2%2C%20%22O%22)%2C%20ion(1%2C%20%22H%22)%5D)%2C%20%22hydroxidovy%22).
[ukazka impl peroxidu]: https://github.com/MarkusSecundus/AnorganickyPrevodnik/blob/master/data.pl#LC100:~:text=%2F*%20%20N%C3%A1zorn%C3%A1%20uk%C3%A1zka%2C%20jak%20by%20bylo%20mo%C5%BEn%C3%A9%20implementovat%20skupinu%20peroxid%C5%AF.