Převodník mezi vzorečky a systematickými názvy anorganických chemických sloučenin  
===================================================================================

#### _Napsaný v [Prologu](https://en.wikipedia.org/wiki/Prolog)_  

&nbsp;   
&nbsp;   
&nbsp;   
&nbsp;   

**Použití...**
- Stačí načíst hlavní modul [_**anorg_prevodnik.pl**_](https://github.com/MarkusSecundus/AnorganickyPrevodnik/blob/master/anorg_prevodnik.pl) a zavolat jeho predikát [`jmeno_vzorec/2`](https://github.com/MarkusSecundus/AnorganickyPrevodnik/blob/master/anorg_prevodnik.pl#LC222:~:text=instanciov%C3%A1n-,jmeno_vzorec),
který obstarává obousměrný převod.  
  - Alespoň jeden z argumentů nutně musí být poskytnut, jinak predikát vždy selže.

 - Podporované sloučeniny: základní kyslíkaté kyseliny, jejich soli, oxidy, hydroxidy, sulfidy, halogenidy (implementované jako specielní případy solí)

 - Vzorečky používají intuitivní formát zápisu - značka prvku příp. následovaná číslem udávajícím počet výskytů, za ní další zn. prvku atd., případné složeniny (kyselinové anionty, amonný kationt,...), pokud se vyskytují násobně, jsou zapsány do závorky následované kvantifikátorem (např. 'NaCl', 'H2SO4', '(NH4)2CO3', 'Fe2(SO4)3', ...)

 - Vytvořené názvy jsou v některých případech lehce kostrbaté, jelikož vystihnout všechny mírné odchylky od striktního názvoslovného pravidla k pěkněji vyslovitelným reálně používaným tvarům by mi dalo více práce, než za kolik to stojí.
    
 - Za kyseliny odpovídající danému jménu jsou považovány všechny, které splňují ze jména plynoucí ox. číslo - tedy vyžádáte-li
      např. kys. sírovou, dostanete H2SO4, ale zároveň i H4SO5 atd. . Stejně tak ale díky tomu pro kys. fosforečnou dostanete vedle
      neexistující HPO3 i správnou H3PO4. Jak hodně kyslíků navíc bude program zkoušet dohazovat, lze ovlivnit skrze predikát
      [`c_ROZUMNY_POCET_KYSLIKU_V_KYSELINE_NAVIC`](https://github.com/MarkusSecundus/AnorganickyPrevodnik/blob/master/data.pl#LC81:~:text=n%C3%A1zv%C5%AF-,c_ROZUMNY_POCET_KYSLIKU_V_KYSELINE_NAVIC) v modulu `data.pl`.  
      Obdobné platí i pro soli daných kyselin.
  
&nbsp;   
&nbsp;     

**Přidávání dat...**

  - **modul [_**data.pl**_](https://github.com/MarkusSecundus/AnorganickyPrevodnik/blob/master/data.pl)**  
    - seznam všech chemických prvků
      - pro přehlednost definovány zvlášť v modulu [_**seznam_prvku.pl**_](https://github.com/MarkusSecundus/AnorganickyPrevodnik/blob/master/seznam_prvku.pl)
      - modul `data` ho importuje a predikát `prvek/3` z něj exportuje dál
    - koncovky pro automatickou tvorbu jmen iontů
    - seznam prvků, pro něž se mají automaticky generovat kationty (vygenerují se vždy se všemi oxidačními čísly)
    - seznam explicitně definovaných iontů

  - **Přidávání nových skupin prvků...**
    - pro implementaci úplně nové skupiny prvků je třeba přidat novou klauzuli k predikátu [`je_molekula/3`](https://github.com/MarkusSecundus/AnorganickyPrevodnik/blob/master/anorg_prevodnik.pl#LC41:~:text=%25je_molekula(%2B%2DKationt%2C%20%2B%2DAniont%2C%20%2D%2BJmeno).)
    - drtivou většinu skupin (_kyslíkaté kyseliny_, _oxidy_, _sulfidy_, _halogenidy_, ...) lze implementovat jako specielní případ solí
      - sůl je jakákoliv sloučenina libovolného kationtu s aniontem , jehož jméno má koncovku `"ovy"`
        - jméno soli vznikne takto: `(JmenoAniontu - "ovy") + " " + JmenoKationtu`  
          (např. `("siranovy" - "ovy") + " " + "sodny"  -> "siran sodny"`)