# Teoretická část
- co je expertní systém - vlastnosti, distinkce, typické problémy
- pracuje se znalostí - vlastnosti znalosti zpracovatelné ES
- symbolická reprezentace znalosti -> symbolické výpočty
  - reprezentace stavu pomocí symbolických struktur, inference jako jejich manipulace
- principy - rule-based systémy
  - prohledávání prostoru stavů (stromy)
  - combinatorial explosion -> heuristiky (strategie)
- dopředné vs. zpětné řetězení
  - zpěnté - means-ends analysis
- interpretace výstupu programu
  - plánování akcí
  - hledání odpovědí
  - dokazování vyplývání
- aplikace
  - strips, gps - zpětné řetězení
    - mycin - složitější pravidla
  - clips - dopředné řetězení







## Předběžná osnova
### teoretická část I (8-12) - expertní systémy - čerpá z Jacksona
* úvod - definice (znalosti, expertíza), vlastnosti, odlišnosti, typické aplikace
* přehled AI - co směřovalo k pojmu expertního systému
  * prohledávání prostoru stavů (state space search), dokazování teorémů, heuristiky
  * odvozovací pravidla
  * oddělení znalostní baze od odvozovacího aparátu
* reprezentace znalostí
  * vlastnosti, kritéria
  * definice - reprezentace, syntax, sémantika, problém
* systémy založené na pravidlech
  * fakta, pravidla (podmínky, důsledky)
  * znalostní báze (výchozí stav, primárně pravidla)
    X working memory (aktuální stav, primárně fakta, i když u učícího se systému
      by mohla přibývat i pravidla)
  * dopředné a zpětné řetězení (a generování vs. přijímání odpovědí)
  * historické aplikace
    * STRIPS
      - stav, operace, cíle
      - jednoduchá reprezentace (preconditions (conj), add list, remove list),
      - zpětné řetězení (means-end analysis)
      - pattern matching, substituce, výběr cíle, pravidla k aplikaci
    * MYCIN
      - neurčitost, vážená pravidla
      - volnější reprezentace (podmínky i disjunkce,
        akce pravidel nejen závěry (fakta), ale i instrukce)
      - meta-pravidla
      -> složitější inference
    * limitace uvedených systémů
    * CLIPS
      - obecný - nezávislý na problémové doméně
      - dopředné řetězení
      - struktura programu - základní direktivy
      - spuštění a řízení inference - agenda, ukončení, strategie (nedeterminismus)
  * další - viz wiki
* symbolické výpočty
  * reprezentace stavu pomocí symbolických struktur, inference jako jejich manipulace
  * stavební kameny - symboly, množiny, n-tice, vektory
  * výhody (a nevýhody) LISPu pro symbolické výpočty
  * základní přehled lispu - pravděpodobně jen odkaz na materiály a vyjmenování
    pojmů potřebných pro pochopení implementace

### úvod
- motivace
  - co je ES - AI, náhražka nebo asistence expertu
  - existující možnosti - CLIPS, jejich omezení
    - najít další aktuální možnosti
    - lisa - zeptat se dostála, co se mu na lise nelíbilo
  - proč lisp
- cíle práce
  - co bylo implementováno v bakalářské práci
  - jaká rozšíření jsem měl implementovat
- organizační poznámka - proč začínám praktickou částí

##### pojmy nutné pro zvládnutí příručky
* **znalostní baze** - množina výchozích znalostí (po resetu, před započetím inference)
  - podle některých definic zahrnuje pravidla
  - Jackson odděluje rule set jako **production memory**
    - co ale v tu chvíli modifikuje defrule? pravděpodobně obojí
* **inference** - odvozování (posloupnost odvozovacích kroků - typicky aplikace pravidel)
* **working memory**
  - pracovní množina znalostí - přesné, ale příliš obšírné
  - pracovní znalost - zavádí k představě jedné znalosti, místo celé množiny
  - pracovní paměť - je přesným překladem, ale pojem je ve skutečnosti nesmyslný - není paměť
* knowledge base a working memory - autoři používají různě - někde slučují
  * pokud systém umožňuje přidání / změnu pravidel uživatelem v průběhu, jsou tato součástí
    knowledge base? (production memory)
* pojmy citovat z The Handbook of applied expert systems - kap. 4 - John Durkin
* => working and production memory is initialized from knowledge base
  * working memory can change by rule activation or user interaction
  * production memory - usually static, but sometimes can change (clips - ověřit)

#### implementace (5-8)
* jednotlivé části zadání (5)
  * zpětné řetězení
    * cíle, kroky, odpovědi, backtracking
    * plánování (hledání posloupností akcí vedoucí k výsledku)
      X hledání odpovědí (konzistentní vazby proměnných)
    * dopředné X zpětné, determinismus, aplikovatelnost, výhody, limitace
    * zdokumentovat to, že aplikace pravidel se tiskne odzadu (v opačném pořadí, než je
      třeba akce vykonat) - viz notes.txt
    * příklady z kap. 4 Paradigms of AI programming, omezení
  * použité nástroje - iterate, xlunit (TDD)
* použité algoritmy - RETE - převzít z bakalářky
* PoAIP - kap 4. - zpětné řetězení, kap. 5 - pattern matching (5.2,5.3)

#### další teorie
* principy - state space search, symbolické výpočty - manipulace reprezentace stavu
* AI - porozumění problému strojem - stroj je schopen řešit, prestože nechápe význam
  - syntax X sémantika - inteligence, pravidla chování, odvozovací pravidla -> symbolické výpočty
* limitace ES
  - problémy v běžném životě vyžadují inherentní znalosti - aby byl systém self-contained,
    musel by být obrovský
  - předání znalostí programu - spoustu vědomostí ani nevíme, že máme, další nedokážeme popsat,
    kvantifikovat - intuice, zkušenost (-> abstrahujeme heuristiky, abychom nemuseli stejný problém
    promýšlet pokaždé znovu), neurčitost, vágnost
  - u velkých systémů - nepředvídané interakce vědomostí (McCarthy - The Robot and The Baby)
  - nárůst state space - combinatorial explosion
  - strategie výběru kroku, salience, metapravidla, (řezy - Prolog)
* sémantika programu
  * typy faktů - atributy objektu - triplety, vektory
  * relace
  * další formy - např. goal u dopředného řetězení
  * zajímá pouze uživatele, ES nerozlišuje (ExiL, GPS) X Prolog - funktory, predikáty
    - trade-off expresivity reprezentace a jednoduchosti odvozovacího aparátu
* interpretace výstupu programu
  - plánování akcí
  - hledání odpovědí
  - dokazování vyplývání
* retract ve zpětném řetězení X nepravda v Prologu - pouze pozitivní znalost




### teoretická část II (odkazy na praktickou část) (4-6)
* LISP
  * výhody
    * syntax - S-výrazy, manipulace symbolické reprezentace, makra
    * dynamický
    * umožňuje funkcionální přístup
      * všechny core objekty jsou immutable - např. (modify fact) vrací nový fakt
        - snadná implementace kopie prostředí - není třeba kopírovat immutable objekty
  * nevýhody
    * LISP a OOP
      * package (misto public/private)
      * metody jako generiky, kongruence lambda-listů
      * standardní funkce nejsou generiky - nejdou přetížit
  * co mi umožnuje / ulehčuje X znemožňuje / ztěžuje vzhledem k implementaci ExiLu
* top-down X bottom-up přístup
* TDD - vyhody, uskali, misuse, typy testu


### výsledky (1-2)
* co bylo implementováno
* co by šlo rozšířit
  * patterny jako v CLIPSu - (color box ?or~red~green)
  * pattern matching pracuje jen na prvni urovni seznamu (simple) nebo primo s hodnotami
    slotu (template), nevnoruje se - nebylo by tezke dodelat - PoAIP kap. 5.2,5.3
  * další strategie (conflict resolution) - Jackson, 86, rule salience (váha)
    * zpětné řetězení nepoužívá strategie výběru matchů vůbec (pouze pořadí)
    * možnost uživatelsky definovat strategie není příliš přínosná, pokud
      uživatel potřebuje používat privátní funkce pro práci s matchi a pravidly
  * zpětné řetězení je velmi omezené
  * vyhodnocení aktivací pravidla v lokálním kontextu - problém s evalem
    * aktivace by musely být uloženy jako funkce s bindingy jako parametry
  * objekty jako fakta, dědičnost
  * nejistota - důvěryhodnost faktů, pravidel, vážené cíle
  * metapravidla?
  * výkon
