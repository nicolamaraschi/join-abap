export const pmData = `
### Modulo PM (Plant Maintenance)

#### Plant maintenance
**IHPA (Plant Maintenance: Partners)**
* Chiavi Primarie: \`OBJNR, PARVW, COUNTER\`
* Descrizione: Funzioni partner per oggetti di manutenzione (ordini, notifiche, equipment, ubicazioni).
* Possibili Join:
  * **AUFK** (Order Master Data): su \`OBJNR\`
  * **EQUI** (Equipment Master): su \`OBJNR\`
  * **IFLOT** (Functional Location): su \`OBJNR\`

**OBJK (Plant Maintenance Object List- Equip.num)**
* Chiavi Primarie: \`OBKNR\`
* Descrizione: Lista di oggetti che collega un ordine/notifica a più oggetti tecnici.
* Possibili Join:
  * **EQUI** (Equipment Master): su \`EQUNR\`
  * **AUFK** (Order Master Data): su \`OBJNR\`

**ILOA (PM Object Location and Account Assignment)**
* Chiavi Primarie: \`ILOAN\`
* Descrizione: Dati di localizzazione e imputazione (es. centro di costo) per oggetti tecnici.
* Possibili Join:
  * **EQUI** (Equipment Master): su \`ILOAN\`
  * **IFLOT** (Functional Location): su \`ILOAN\`

**AFIH (Maintenance order header)**
* Chiavi Primarie: \`AUFNR\`
* Descrizione: Testata specifica per gli ordini di manutenzione.
* Possibili Join:
  * **AUFK** (Order Master Data): su \`AUFNR\`
  * **EQUI** (Equipment Master): su \`EQUNR\`
  * **IFLOT** (Functional Location): su \`TPLNR\`

**SER05 (Doc. Header for S/N for PP - OrderPrOr+No. Serial No.)**
* Chiavi Primarie: \`OBKNR\`
* Descrizione: Testata documento per numeri di serie assegnati a ordini di produzione/manutenzione.
* Possibili Join:
  * **OBJK** (Object List): su \`OBKNR\`
  * **AUFK** (Order Master Data): su \`OBJNR\` (tramite OBJK)
`;