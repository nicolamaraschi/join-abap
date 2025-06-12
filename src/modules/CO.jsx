export const coData = `
### Modulo CO (Controlling)

#### CO :
**TKA01 (Controlling areas)**
* Chiavi Primarie: \`KOKRS\`
* Descrizione: Tabella di customizing che definisce le aree di controlling.
* Possibili Join:
  * **TKA02** (Controlling area assignment): su \`KOKRS\`

**TKA02 (Controlling area assignment)**
* Chiavi Primarie: \`BUKRS\`
* Descrizione: Assegna le società (company codes) alle aree di controlling.
* Possibili Join:
  * **TKA01** (Controlling areas): su \`KOKRS\`

**KEKO (Product-costing header)**
* Chiavi Primarie: \`KALNR, KALKA, KADKY\`
* Descrizione: Testata del calcolo costi di prodotto.
* Possibili Join:
  * **KEPH** (Cost components for cost of goods manuf.): su \`KALNR, KALKA, KADKY\`
  * **KALO** (Costing objects): su \`OBJEK=KALNR\`

**KEPH (Cost components for cost of goods manuf.)**
* Chiavi Primarie: \`KALNR, KALKA, KADKY, KSCHL, ...\`
* Descrizione: Dettaglio dei componenti di costo per un calcolo costi.
* Possibili Join:
  * **KEKO** (Product-costing header): su \`KALNR, KALKA, KADKY\`

**KALO (Costing objects)**
* Chiavi Primarie: \`OBJEK\`
* Descrizione: Anagrafica degli oggetti di costing.
* Possibili Join:
  * **KEKO** (Product-costing header): su \`KALNR=OBJEK\`
  * **KANZ** (Sales order items - costing objects): su \`OBJEK\`

**KANZ (Sales order items - costing objects)**
* Chiavi Primarie: \`KDAUF, KDPOS\`
* Descrizione: Collega le posizioni degli ordini di vendita agli oggetti di costing.
* Possibili Join:
  * **KALO** (Costing objects): su \`OBJEK\`

##### Cost center master data
**CSKS (Cost Center Master Data)**
* Chiavi Primarie: \`KOKRS, KOSTL, DATBI\`
* Descrizione: Dati anagrafici dei centri di costo.
* Possibili Join:
  * **CSKT** (Cost center texts): su \`KOKRS, KOSTL, DATBI, SPRAS\`
  * **CRCO** (Assignment of Work Center to Cost Center): su \`KOKRS, KOSTL\`

**CSKT (Cost center texts)**
* Chiavi Primarie: \`SPRAS, KOKRS, KOSTL, DATBI\`
* Descrizione: Testi descrittivi per i centri di costo.
* Possibili Join:
  * **CSKS** (Cost Center Master Data): su \`KOKRS, KOSTL, DATBI\`

**CRCO (Assignment of Work Center to Cost Center)**
* Chiavi Primarie: \`OBJTY, OBJID, LASET, ENDDA, LANUM\`
* Descrizione: Collega un posto di lavoro (work center) a un centro di costo.
* Possibili Join:
  * **CSKS** (Cost Center Master Data): su \`KOSTL, KOKRS\`

##### Cost center accounting
**COSP (CO Object: Cost Totals for External Postings)**
* Descrizione: È una vista che mostra i totali dei costi per registrazioni esterne per oggetto CO e periodo.

**COEP (CO Object: Line Items (by Period))**
* Chiavi Primarie: \`KOKRS, BELNR, BUZEI\`
* Descrizione: Voci di dettaglio CO. Nota: In S/4HANA, questa tabella è stata in gran parte sostituita da ACDOCA per le nuove registrazioni.
* Possibili Join:
  * **COBK** (CO Object: Document header): su \`KOKRS, BELNR\`

**COBK (CO Object: Document header)**
* Chiavi Primarie: \`KOKRS, BELNR\`
* Descrizione: Testata per i documenti specifici del CO.
* Possibili Join:
  * **COEP** (CO Object: Line Items): su \`KOKRS, BELNR\`

**COST (CO Object: Cost Totals for All Postings)**
* Chiavi Primarie: \`LEDNR, OBJNR, GJAHR, WRTTP, VERSN, TARKZ, PERBL\`
* Descrizione: Tabella dei totali di costo (non più aggiornata in S/4HANA, usare COSP e COSS).
* Possibili Join:
  * Nessun join all'interno di questa sottocategoria.
`;