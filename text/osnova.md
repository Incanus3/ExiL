## Osnova
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
* symbolické výpočty
  * reprezentace stavu pomocí symbolických struktur, inference jako jejich manipulace
  * stavební kameny - symboly, množiny, n-tice, vektory
  * výhody (a nevýhody) LISPu pro symbolické výpočty
  * základní přehled lispu - pravděpodobně jen odkaz na materiály a vyjmenování
    pojmů potřebných pro pochopení implementace


### praktická část
#### dokumentace ExiLu (5-10) + příklady
* uživatelská příručka
  - začít příkladem, ideálně templated fakta
  - popsat jednotlivé sekce kódu, jejich význam (korespondence s fázemi návrhu ES,
    formulace problému, formát dat, vstupní znalosti, odvozovací krok, řízení odvozování, ladění)
    - definice prostředí
    - definice šablon - formát dat
    - definice znalostní báze - vstupní znalost - deffacts, defrules
    - (nastavení sledování průběhu inference - watchers)
    - (úprava průběhu inference - strategie)
    - spuštění / krokování inference - reset, run, step
    - dotazy nad working memory - facts, agenda
    - úprava working memory - assert, retract, modify
    - dotazy nad znalostní bází - fact-groups, rules
    - cleanup - volatile vs durable sloty prostředí
    - undo/redo
    - zpětné řetězení
    - GUI
* referenční příručka
  * popsat jednotlivé fáze detailně - všechny funkce, možnosti, syntax (CLIPS, lispy), redefinice
  * ordered / structured fakta
  * odlišnosti / nedodělávky od CLIPS
  * chování v kombinaci - např. externí reprezentace - query -> repre -> modifier
  * aktivace pravidel jsou vyhodnocovány -> mohou používat lispové funkce (funkční alternativy)
    - uvést příklady - např. pravidla snadno mohou agregovat nejistotu (Jackson, 81)
  * ukončení inference - buď zajistit, že nebude splněno žádné pravidlo (např. odstranit
    goal, pokud je explicitní), nebo explicitně volat halt

#### implementace (5-8)
* architektura
  - popsat jednotlivé package, zodpovědnosti, závislosti (cykly), interakce
    * rete <-> env (as observer - not possible with packages)
    * parser <-> env (in backward chaining, because env analyzes rule's activations,
        which is probably a bad idea)
* jednotlivé části zadání (5)
  * CLIPS kompatibilita - parser
  * GUI - CAPI, #+features, provázanost - gui třeba vázat k prostředí (ideálně opět observer)
  * undo/redo
    * implementace - ulozeni hodnot slotu v closure a obnoveni
      - dynamicke volani copy-'slot'
      - kopie rete + ověření správnosti (ekvivalence)
      -> pruchod grafem s cykly - velmi obecna funkce vyssiho radu
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
* zajímavosti, problémy
* PoAIP - kap 4. - zpětné řetězení, kap. 5 - pattern matching (5.2,5.3)
* co by obnášela různá vylepšení
  * or, wildcardy a custom testy v podmínkách pravidel
    - rozšíření RETE sítě, složitější parsování
  * problém s evalem důsledků (jak bych řešil v ruby - vyhodnocení bloku v prostředí/objektu)

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
