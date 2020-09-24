## AnorganickyPrevodnik

####Převodník mezi vzorečky a systematickými názvy anorganických chemických sloučenin.

#####Napsaný v Prologu.
  
  
  




Pro použití stačí načíst hlavní modul 'anorg_prevodnik.pl' a zavolat jeho predikát jmeno_vzorec/2,
který obstarává obousměrný převod.

    - Alespoň jeden z argumentů nutně musí být poskytnut, jinak predikát vždy selže.

    - Podporované sloučeniny: základní kyslíkaté kyseliny, jejich soli, oxidy, hydroxidy, sulfidy, halogenidy (implementované jako specielní případy solí)

    - Vzorečky používají intuitivní formát zápisu - značka prvku příp. následována číslem udávajícím počet výskytů, za ní další zn. prvku atd.,
       případné složeniny (kyselinové anionty, amonný kationt,...), pokud se vyskytují násobně, jsou zapsány do závorky následované kvantifikátorem
       (např. 'NaCl', 'H2SO4', '(NH4)2CO3', 'Fe2(SO4)3', ...)

    - Vytvořené názvy jsou v některých případech lehce kostrbaté, jelikož vystihnout všechny mírné odchylky od striktního názvoslovného pravidla 
      k pěkněji vyslovitelným reálně používaným tvarům by mi dalo více práce, než za kolik to stojí.
    
    - Za kyseliny odpovídající danému jménu jsou považovány všechny, které splňují ze jména plynoucí ox. číslo - tedy vyžádáte-li
      např. kys. sírovou, dostanete H2SO4, ale zároveň i H4SO5 atd. . Stejně tak ale díky tomu pro kys. fosforečnou dostanete vedle
      neexistující HPO3 i správnou H3PO4. Jak hodně kyslíků navíc bude program zkoušet dohazovat, lze ovlivnit skrze predikát
      c_ROZUMNY_POCET_KYSLIKU_V_KYSELINE_NAVIC v modulu 'data'.
      Obdobné platí i pro soli daných kyselin.


Přidávání dat...

  -