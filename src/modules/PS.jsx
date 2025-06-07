export const psData = `
### Modulo PS (Project System)

#### Basic data
**PRHI (Work Breakdown Structure, Edges (Hierarchy Pointer))**
* Chiavi Primarie: \`MANDT, POSNR_U, POSNR_O\`
* Descrizione: Definisce la relazione gerarchica (padre-figlio) tra elementi WBS.
* Possibili Join:
  * **PRPS** (WBS Element Master Data): su \`MANDT, POSNR_U=PSPNR\` (elemento subordinato) e \`MANDT, POSNR_O=PSPNR\` (elemento superiore)

**PROJ (Project definition)**
* Chiavi Primarie: \`MANDT, PSPNR\`
* Descrizione: Dati di testata della definizione di un progetto.
* Possibili Join:
  * **PRPS** (WBS Element Master Data): su \`MANDT, PSPNR=PROJNR\`

**PRPS (WBS (Work Breakdown Structure) Element Master Data)**
* Chiavi Primarie: \`MANDT, PSPNR\`
* Descrizione: Dati anagrafici di un singolo elemento WBS.
* Possibili Join:
  * **PROJ** (Project Definition): su \`MANDT, PROJNR=PSPNR\`
  * **PRHI** (WBS Hierarchy): su \`MANDT, PSPNR=POSNR_U\` o \`PSPNR=POSNR_O\`
  * **JEST** (Object Status): su \`MANDT, OBJNR\`
  * **RPSCO** (Project info database): su \`MANDT, OBJNR\`

**RPSCO (Project info database: Costs, revenues, finances)**
* Chiavi Primarie: \`MANDT, OBJNR, GJAHR, WRTTP, VERSN, ...\`
* Descrizione: Tabella dati con costi, ricavi e finanze aggregate per oggetti di progetto (WBS, Network, etc.).
* Possibili Join:
  * **PRPS** (WBS Element Master Data): su \`MANDT, OBJNR\`
  * **AUFK** (Order Master Data - for Networks): su \`MANDT, OBJNR\`

**MSPR (Project stock)**
* Chiavi Primarie: \`MANDT, MATNR, WERKS, PSPNR, ...\`
* Descrizione: Gestisce gli stock di materiale con riferimento specifico a un elemento WBS (Project Stock).
* Possibili Join:
  * **MARA** (Material Master): su \`MANDT, MATNR\`
  * **PRPS** (WBS Element Master Data): su \`MANDT, PSPNR\`
  * **MAKT** (Material Description): su \`MANDT, MATNR, SPRAS\`

#### Equipment
**EQUI (Equipment master data)**
* Chiavi Primarie: \`MANDT, EQUNR\`
* Descrizione: Dati anagrafici dell'equipment (macchinario, asset).
* Possibili Join:
  * **EQKT** (Equipment short text): su \`MANDT, EQUNR, SPRAS\`
  * **EQUZ** (Equipment time segment): su \`MANDT, EQUNR\`
  * **ILOA** (PM Object Location and Account Assignment): su \`MANDT, ILOAN\`

**EQKT (Equipment short text)**
* Chiavi Primarie: \`MANDT, SPRAS, EQUNR\`
* Descrizione: Testi brevi descrittivi dell'equipment.
* Possibili Join:
  * **EQUI** (Equipment master data): su \`MANDT, EQUNR\`

**EQUZ (Equipment time segment)**
* Chiavi Primarie: \`MANDT, EQUNR, DATBI\`
* Descrizione: Storico delle installazioni/smontaggi e assegnazioni dell'equipment.
* Possibili Join:
  * **EQUI** (Equipment master data): su \`MANDT, EQUNR\`
  * **IFLOT** (Functional Location): su \`MANDT, TPLNR=HEQNR\`
`;