# Analyzátor FB smajlíků
zápočtový program pro předmět Neprocedurální programování. Program z exportu zpráv na FB vytáhne počty smajlíků používané ve zprávách pro jednotlivé měsíce.
## Uživatelská dokumentace
- Stáhněte si svůj arhcív z facebooku podle návodu [zde](https://www.facebook.com/help/131112897028467)
- vyextrahujte archiv
- přesuňte main do vyextrahované složky html
- spusťte main
- po skončení analýzy program vypíše "Done" a výsledek můžete najít v out.txt
### Pozor! 
- Pokud máte na Facebooku opravdu hodně zpráv, bude to chvíli trvat a bere si to opravdu hodně paměti RAM. Pro můj export (messages.htm odkud to bere), který má okolo 100Mb analýza trvá ~30 sekund a vezme si ~7GB RAM.
- Analyzuje pouze UTF-8 smajlíky, které Facebook zavedl nedávno a jdou psát tak, že v listu smajlíků na nějakého kliknete (ať už z mobilu nebo na PC). Smajlíky typu :D nebo ;* program nepočítá.
## Programátorská dokumentace
Veškerý program je v main.hs . Spustitelný je určitě pro GHC 7.10.3. Data jsou uložená v haskellí Mapě - rok : měsíc : smajlík : počet. Vyparsování zpráv je zpracováváno pomocí algoritmu KMP (resp. toho co z něj zbylo po tom, co jsem jej přepsal do Haskellu). Zbytek by měl snad být jasný z pojemnování a komentářů.