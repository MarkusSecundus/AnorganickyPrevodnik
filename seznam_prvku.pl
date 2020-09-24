:- module(seznam_prvku, [prvek/3]).
/*
    Modul vyhrazený čistě pro přechovávání seznamu chemických prvků, z nichž se sloučeniny mohou skládat.
    Měl by nést pouze dlouhou řadu faktů - jeden pro každý chemický prvek.

    Prvek je určen svou značkou (jak je značen ve vzorečcích) a jménem, 
    avšak pro účely pojmenovávání jeho sloučenin musí mít explicitně zadaný taktéž kořen jména - viz níže.
*/


%prvek(+Znacka, Jmeno, KorenJmena)  -- KorenJmena je část, z níž přilepováním přípon budou odvozována jména automatických iontů daného prvku 
prvek("H", "vodik", "vod").         
prvek("He", "helium", "hel").
prvek("Li", "lithium", "lith").
prvek("Be", "beryllium", "beryll").
prvek("B", "bor", "bor").              
prvek("C", "uhlik", "uhl").  % !
prvek("N", "dusik", "dus").          % -- např. zde tedy iont dusNÝ, dusNATÝ, dusITÝ, dusIČITÝ, atd.
prvek("O", "kyslik", "kys").  % !
prvek("F", "fluor" ,"fluor").
prvek("Ne", "neon", "neon").
prvek("Na", "sodik", "sod").
prvek("Mg", "horcik", "horec").
prvek("Al", "hlinik", "hlin").
prvek("Si", "kremik", "krem").
prvek("P", "fosfor", "fosfor").
prvek("S", "sira", "sir").
prvek("Cl", "chlor", "chlor").
prvek("Ar", "argon", "argon").
prvek("K", "draslik", "drasel").   % !
prvek("Ca", "vapnik", "vapen"). 
prvek("Sc", "skandium", "skand").
prvek("Ti", "titan", "titan").
prvek("V", "vanad", "vanad").
prvek("Cr", "chrom", "chrom").
prvek("Mn", "mangan", "mangan").
prvek("Fe", "zelezo", "zelez").
prvek("Co", "kobalt", "kobalt").
prvek("Ni", "nikl", "nikel").   % !
prvek("Cu", "med", "med").
prvek("Zn", "zinek", "zinek").
prvek("Ga", "gallium", "gall").
prvek("Ge", "germanium", "german").
prvek("As", "arsen", "arsen").
prvek("Se", "selen", "selen").
prvek("Br", "brom", "brom").
prvek("Kr", "krypton", "krypton").
prvek("Rb", "rubidium", "rubid").
prvek("Sr", "stroncium", "stront").   % !
prvek("Y", "yttrium", "yttr").        % !
prvek("Zr", "zirkonium", "zirkon").
prvek("Nb", "niob", "niob").
prvek("Mo", "molybden", "molybden").
prvek("Tc", "technecium", "technec").
prvek("Ru", "ruthenium", "ruthen").
prvek("Rh", "rhodium", "rhod").
prvek("Pd", "palladium", "pallad").
prvek("Ag", "stribro", "stribr").
prvek("Cd", "kadmium", "kadm").
prvek("In", "indium", "indium").
prvek("Sn", "cin", "cin").
prvek("Sb", "antimon", "antimon").
prvek("Te", "tellur", "tellur").
prvek("I", "jod", "jod").
prvek("Xe", "xenon", "xenon").
prvek("Cs", "cesium", "ces").
prvek("Ba", "baryum", "bar").
prvek("La", "lanthan", "lanthan").
prvek("Ce", "cer", "cer").
prvek("Pr", "praseodym", "praseodym").     % !
prvek("Nd", "neodym", "neodym").
prvek("Pm", "promethium", "prometh").
prvek("Sm", "samarium", "samar").
prvek("Eu", "europium", "europ").
prvek("Gd", "gadolinium", "gadol").
prvek("Tb", "terbium", "terb").
prvek("Dy", "dysprosium", "dyspros").
prvek("Ho", "holmium", "holm").            % !e
prvek("Er", "erbium", "erb").
prvek("Tm", "thulium", "thul").
prvek("Yb", "ytterbium", "ytterb").
prvek("Lu", "lutecium", "lutec").   % !
prvek("Hf", "hafnium", "hafn").   % !e?
prvek("Ta", "tantal", "tantal").
prvek("W", "wolfram", "wolfram").
prvek("Re", "rhenium", "rhen").
prvek("Os", "osmium", "osm").    % !e
prvek("Ir", "iridium", "irid").
prvek("Pt", "platina", "platin").    % !
prvek("Au", "zlato", "zlat").
prvek("Hg", "rtut", "rtut").
prvek("Tl", "thallium", "thall").
prvek("Pb", "olovo", "olov").   
prvek("Bi", "bismut", "bismut").
prvek("Po", "polonium", "polon").
prvek("At", "astat", "astat").
prvek("Rn", "radon", "radon").
prvek("Fr", "francium", "franc").
prvek("Ra", "radium", "rad").   % !radiovy
prvek("Ac", "aktinium", "aktin").    % !aktinovy 
prvek("Th", "thorium", "thor").
prvek("Pa", "protaktinium", "protaktin").
prvek("U", "uran", "uran").
prvek("Np", "neptunium", "neptun").
prvek("Pu", "plutonium", "pluton").
prvek("Am", "americium", "americ").
prvek("Cm", "curium", "curij"). 
prvek("Bk", "berkelium", "berkelij").
prvek("Cf", "kalifornium", "kalifornij").
prvek("Es", "einsteinium", "einsteinij").
prvek("Fm", "fermium", "fermij").
prvek("Md", "mendelevium", "mendelevij").
prvek("No", "nobelium", "nobelij").
prvek("Lr", "lawrencium", "lawrencij").
prvek("Rf", "rutherfordium", "rutherfordij").
prvek("Db", "dubnium", "dubnij").
prvek("Sg", "seaborgium", "seaborgij").
prvek("Bh", "bohrium", "bohrij").
prvek("Hs", "hassium", "hassij").
prvek("Mt", "meitnerium", "meitnerij").
prvek("Ds", "darmstadtium", "darmstadtij").
prvek("Rg", "roentgenium", "roentgenij").
prvek("Cn", "kopernicium", "kopernicij").
prvek("Nh", "nihonium", "nihon").
prvek("Fl", "flerovium", "flerovij").
prvek("Mc", "moscovium", "moscovij").
prvek("Lv", "livermorium", "livermor").
prvek("Ts", "tennessin", "tennessin").
prvek("Og", "oganesson", "oganesson").