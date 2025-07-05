export const coData = `
### Modulo CO (Controlling)

#### CO :
**TKA01 (Controlling areas)**
* Chiavi Primarie: \`MANDT(MANDT), KOKRS(KOKRS)\`
* Descrizione: Tabella di customizing che definisce le aree di controlling.
* Possibili Join:
  * **TKA02** (Controlling area assignment): su \`KOKRS\`

**TKA02 (Controlling area assignment)**
* Chiavi Primarie: \`MANDT(MANDT), BUKRS(BUKRS)\`
* Descrizione: Assegna le società (company codes) alle aree di controlling.
* Possibili Join:
  * **TKA01** (Controlling areas): su \`KOKRS\`

**KEKO (Product-costing header)**
* Chiavi Primarie: \`MANDT(MANDT), KALNR(KALNR), KALKA(CK_KALKA), KADKY(CK_KADKY)\`
* Descrizione: Testata del calcolo costi di prodotto.
* Possibili Join:
  * **KEPH** (Cost components for cost of goods manuf.): su \`KALNR, KALKA, KADKY\`
  * **KALO** (Costing objects): su \`OBJEK=KALNR\`

**KEPH (Cost components for cost of goods manuf.)**
* Chiavi Primarie: \`MANDT(MANDT), KALNR(KALNR), KALKA(CK_KALKA), KADKY(CK_KADKY), KEART(CK_KEART), ELEMT(CK_ELEMT)\`
* Descrizione: Dettaglio dei componenti di costo per un calcolo costi.
* Possibili Join:
  * **KEKO** (Product-costing header): su \`KALNR, KALKA, KADKY\`

**KALO (Costing objects)**
* Chiavi Primarie: \`MANDT(MANDT), OBJEK(CO_OBJEK)\`
* Descrizione: Anagrafica degli oggetti di costing.
* Possibili Join:
  * **KEKO** (Product-costing header): su \`KALNR=OBJEK\`
  * **KANZ** (Sales order items - costing objects): su \`OBJEK\`

**KANZ (Sales order items - costing objects)**
* Chiavi Primarie: \`MANDT(MANDT), KDAUF(KDAUF), KDPOS(KDPOS), OBJEK(CO_OBJEK)\`
* Descrizione: Collega le posizioni degli ordini di vendita agli oggetti di costing.
* Possibili Join:
  * **KALO** (Costing objects): su \`OBJEK\`

##### Cost center master data
**CSKA (Cost elements (data dependent on chart of accounts))**
* Chiavi Primarie: \`MANDT(MANDT), KTOPL(KTOPL), KSTAR(KSTAR)\`
* Descrizione: Anagrafica Voci di CoGe a livello di piano dei conti.
* Possibili Join:
  * **CSKB** (Cost elements (data dependent on controlling area)): su \`KTOPL, KSTAR\`
  * **CSKU** (Cost Element Texts): su \`KTOPL, KSTAR\`

**CSKS (Cost Center Master Data)**
* Chiavi Primarie: \`MANDT(MANDT), KOKRS(KOKRS), KOSTL(KOSTL), DATBI(DATBI)\`
* Descrizione: Dati anagrafici dei centri di costo.
* Possibili Join:
  * **CSKT** (Cost center texts): su \`KOKRS, KOSTL, DATBI, SPRAS\`
  * **CRCO** (Assignment of Work Center to Cost Center): su \`KOKRS, KOSTL\`

**CSKT (Cost center texts)**
* Chiavi Primarie: \`MANDT(MANDT), SPRAS(SPRAS), KOKRS(KOKRS), KOSTL(KOSTL), DATBI(DATBI)\`
* Descrizione: Testi descrittivi per i centri di costo.
* Possibili Join:
  * **CSKS** (Cost Center Master Data): su \`KOKRS, KOSTL, DATBI\`

**CRCO (Assignment of Work Center to Cost Center)**
* Chiavi Primarie: \`MANDT(MANDT), OBJTY(J_OBJTY), OBJID(J_OBJID), LASET(CR_LASET), ENDDA(ENDDA), LANUM(CR_LANUM)\`
* Descrizione: Collega un posto di lavoro (work center) a un centro di costo.
* Possibili Join:
  * **CSKS** (Cost Center Master Data): su \`KOSTL, KOKRS\`

##### Cost center accounting
**COSP (CO Object: Cost Totals for External Postings)**
* Chiavi Primarie: \`MANDT(MANDT), LEDNR(LEDNR), OBJNR(J_OBJNR), GJAHR(GJAHR), WRTTP(WRTTP), VERSN(VERSN), KSTAR(KSTAR), HRKFT(CO_HRKFT), VRGNG(CO_VRGNG), VBUND(VBUND), PARGB(PARGB), BEKNZ(BEKNZ), TWAER(TWAER), PERBL(PERBL)\`
* Descrizione: È una vista che mostra i totali dei costi per registrazioni esterne per oggetto CO e periodo.

**COEP (CO Object: Line Items (by Period))**
* Chiavi Primarie: \`MANDT(MANDT), KOKRS(KOKRS), BELNR(CO_BELNR), BUZEI(CO_BUZEI)\`
* Descrizione: Voci di dettaglio CO. Nota: In S/4HANA, questa tabella è stata in gran parte sostituita da ACDOCA per le nuove registrazioni.
* Possibili Join:
  * **COBK** (CO Object: Document header): su \`KOKRS, BELNR\`

**COBK (CO Object: Document header)**
* Chiavi Primarie: \`MANDT(MANDT), KOKRS(KOKRS), BELNR(CO_BELNR)\`
* Descrizione: Testata per i documenti specifici del CO.
* Possibili Join:
  * **COEP** (CO Object: Line Items): su \`KOKRS, BELNR\`

**COST (CO Object: Cost Totals for All Postings)**
* Chiavi Primarie: \`MANDT(MANDT), LEDNR(LEDNR), OBJNR(J_OBJNR), GJAHR(GJAHR), WRTTP(WRTTP), VERSN(VERSN), TARKZ(TARKZ), PERBL(PERBL)\`
* Descrizione: Tabella dei totali di costo (non più aggiornata in S/4HANA, usare COSP e COSS).
* Possibili Join:
  * Nessun join all'interno di questa sottocategoria.
`;