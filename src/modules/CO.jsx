export const coData = `
### Modulo CO (Controlling)

#### CO :
**TKA01 (Controlling areas)**
* Chiavi Primarie: \`MANDT, KOKRS\`
* Descrizione: Tabella di customizing che definisce le aree di controlling.
* Possibili Join:
  * **TKA02** (Controlling area assignment): su \`MANDT, KOKRS\`
  * **CSKS** (Cost Center Master Data): su \`MANDT, KOKRS\`

**TKA02 (Controlling area assignment)**
* Chiavi Primarie: \`MANDT, BUKRS\`
* Descrizione: Assegna le società (company codes) alle aree di controlling.
* Possibili Join:
  * **TKA01** (Controlling areas): su \`MANDT, KOKRS\`
  * **T001** (Company Codes): su \`MANDT, BUKRS\`

**KEKO (Product-costing header)**
* Chiavi Primarie: \`MANDT, KALNR, KALKA, KADKY\`
* Descrizione: Testata del calcolo costi di prodotto.
* Possibili Join:
  * **KEPH** (Cost components for cost of goods manuf.): su \`MANDT, KALNR, KALKA, KADKY\`
  * **KALO** (Costing objects): su \`MANDT, OBJEK=KALNR\`
  * **MARA** (Material Master): su \`MANDT, MATNR\` (se il calcolo costi è per materiale)

**KEPH (Cost components for cost of goods manuf.)**
* Chiavi Primarie: \`MANDT, KALNR, KALKA, KADKY, KSCHL, ...\`
* Descrizione: Dettaglio dei componenti di costo (es. materie prime, manodopera) per un calcolo costi.
* Possibili Join:
  * **KEKO** (Product-costing header): su \`MANDT, KALNR, KALKA, KADKY\`

**KALO (Costing objects)**
* Chiavi Primarie: \`MANDT, OBJEK\`
* Descrizione: Anagrafica degli oggetti di costing.
* Possibili Join:
  * **KEKO** (Product-costing header): su \`MANDT, KALNR=OBJEK\`
  * **KANZ** (Sales order items - costing objects): su \`MANDT, OBJEK\`

**KANZ (Sales order items - costing objects)**
* Chiavi Primarie: \`MANDT, KDAUF, KDPOS\`
* Descrizione: Collega le posizioni degli ordini di vendita agli oggetti di costing.
* Possibili Join:
  * **KALO** (Costing objects): su \`MANDT, OBJEK\`
  * **VBAP** (Sales Document Item): su \`MANDT, KDAUF=VBELN, KDPOS=POSNR\`

##### Cost center master data
**CSKS (Cost Center Master Data)**
* Chiavi Primarie: \`MANDT, KOKRS, KOSTL, DATBI\`
* Descrizione: Dati anagrafici dei centri di costo.
* Possibili Join:
  * **CSKT** (Cost center texts): su \`MANDT, KOKRS, KOSTL, DATBI, SPRAS\`
  * **CRCO** (Assignment of Work Center to Cost Center): su \`MANDT, KOKRS, KOSTL\`
  * **COSP** (CO Object: Cost Totals): su \`MANDT, OBJNR\` (OBJNR di CSKS)
  * **ACDOCA** (Universal Journal): su \`MANDT=RCLNT, KOKRS, RCNTR=KOSTL\`

**CSKT (Cost center texts)**
* Chiavi Primarie: \`MANDT, SPRAS, KOKRS, KOSTL, DATBI\`
* Descrizione: Testi descrittivi per i centri di costo.
* Possibili Join:
  * **CSKS** (Cost Center Master Data): su \`MANDT, KOKRS, KOSTL, DATBI\`

**CRCO (Assignment of Work Center to Cost Center)**
* Chiavi Primarie: \`MANDT, OBJTY, OBJID, LASET, ENDDA\`
* Descrizione: Collega un posto di lavoro (work center) a un centro di costo.
* Possibili Join:
  * **CRHD** (Work Center Header): su \`MANDT, OBJTY, OBJID\`
  * **CSKS** (Cost Center Master Data): su \`MANDT, KOSTL, KOKRS\`

##### Cost center accounting
**COSP (CO Object: Cost Totals for External Postings)**
* Chiavi Primarie: \`MANDT, LEDNR, OBJNR, GJAHR, WRTTP, VERSN, KSTAR, ...\`
* Descrizione: Totali dei costi per registrazioni esterne per oggetto CO e periodo.
* Possibili Join:
  * **CSKS** (Cost Center Master Data): su \`MANDT, OBJNR\` (se è un centro di costo)
  * **AUFK** (Order Master Data): su \`MANDT, OBJNR\` (se è un ordine)

**COEP (CO Object: Line Items (by Period))**
* Chiavi Primarie: \`MANDT, KOKRS, BELNR, BUZEI\`
* Descrizione: Voci di dettaglio CO. Nota: In S/4HANA, questa tabella è stata in gran parte sostituita da ACDOCA per le nuove registrazioni.
* Possibili Join:
  * **COBK** (CO Object: Document header): su \`MANDT, KOKRS, BELNR\`
  * **ACDOCA** (Universal Journal): su \`MANDT=RCLNT, KOKRS, AWREF=BELNR, ...\` (per trovare la voce corrispondente)

**COBK (CO Object: Document header)**
* Chiavi Primarie: \`MANDT, KOKRS, BELNR\`
* Descrizione: Testata per i documenti specifici del CO.
* Possibili Join:
  * **COEP** (CO Object: Line Items): su \`MANDT, KOKRS, BELNR\`
  * **ACDOCA** (Universal Journal): su \`MANDT=RCLNT, KOKRS, AWTYP='COBK', AWREF=BELNR\`

**COST (CO Object: Cost Totals for All Postings)**
* Chiavi Primarie: \`MANDT, LEDNR, OBJNR, GJAHR, WRTTP, VERSN, ...\`
* Descrizione: Tabella dei totali di costo (non più aggiornata in S/4HANA, usare COSP e COSS).
* Possibili Join:
  * **CSKS** (Cost Center Master Data): su \`MANDT, OBJNR\`
`;
