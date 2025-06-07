export const pmData = `
### Modulo PM (Plant Maintenance)

#### Plant maintenance
**IHPA (Plant Maintenance: Partners)**
* Chiavi Primarie: \`MANDT, OBJNR, PARVW, COUNTER\`
* Descrizione: Funzioni partner per oggetti di manutenzione (ordini, notifiche, equipment, ubicazioni).
* Possibili Join:
  * **AUFK** (Order Master Data): su \`MANDT, OBJNR\`
  * **QMEL** (Quality Notification): su \`MANDT, OBJNR\`
  * **EQUI** (Equipment Master): su \`MANDT, OBJNR\`
  * **IFLOT** (Functional Location): su \`MANDT, OBJNR\`

**OBJK (Plant Maintenance Object List- Equip.num)**
* Chiavi Primarie: \`MANDT, OBKNR\`
* Descrizione: Lista di oggetti che collega un ordine/notifica a più oggetti tecnici.
* Possibili Join:
  * **EQUI** (Equipment Master): su \`MANDT, EQUNR\`
  * **AUFK** (Order Master Data): su \`MANDT, OBJNR\` (OBJNR è il numero oggetto dell'ordine/notifica a cui la lista è collegata)

**ILOA (PM Object Location and Account Assignment)**
* Chiavi Primarie: \`MANDT, ILOAN\`
* Descrizione: Dati di localizzazione e imputazione (es. centro di costo) per oggetti tecnici.
* Possibili Join:
  * **EQUI** (Equipment Master): su \`MANDT, ILOAN\`
  * **IFLOT** (Functional Location): su \`MANDT, ILOAN\`
  * **CSKS** (Cost Center Master): su \`MANDT, KOSTL\`

**AFIH (Maintenance order header)**
* Chiavi Primarie: \`MANDT, AUFNR\`
* Descrizione: Testata specifica per gli ordini di manutenzione.
* Possibili Join:
  * **AUFK** (Order Master Data): su \`MANDT, AUFNR\`
  * **EQUI** (Equipment Master): su \`MANDT, EQUNR\`
  * **IFLOT** (Functional Location): su \`MANDT, TPLNR\`
  * **QMEL** (Quality Notification): su \`MANDT, QMNUM\`

**SER05 (Doc. Header for S/N for PP - OrderPrOr+No. Serial No.)**
* Chiavi Primarie: \`MANDT, OBKNR\`
* Descrizione: Testata documento per numeri di serie assegnati a ordini di produzione/manutenzione.
* Possibili Join:
  * **OBJK** (Object List): su \`MANDT, OBKNR\`
  * **SERI** (Serial Numbers): su \`MANDT, OBKNR\`
  * **AUFK** (Order Master Data): su \`MANDT, OBJNR\` (tramite OBJK)
`;