export const psData = `
### Modulo PS (Project System)

#### Basic data
**PRHI (Work Breakdown Structure, Edges (Hierarchy Pointer))**
* Chiavi Primarie: \`POSNR_U, POSNR_O\`
* Descrizione: Definisce la relazione gerarchica (padre-figlio) tra elementi WBS.
* Possibili Join:
  * **PRPS** (WBS Element Master Data): su \`POSNR_U=PSPNR\` (elemento subordinato) e \`POSNR_O=PSPNR\` (elemento superiore)

**PROJ (Project definition)**
* Chiavi Primarie: \`PSPNR\`
* Descrizione: Dati di testata della definizione di un progetto.
* Possibili Join:
  * **PRPS** (WBS Element Master Data): su \`PSPNR=PROJNR\`

**PRPS (WBS (Work Breakdown Structure) Element Master Data)**
* Chiavi Primarie: \`PSPNR\`
* Descrizione: Dati anagrafici di un singolo elemento WBS.
* Possibili Join:
  * **PROJ** (Project Definition): su \`PROJNR=PSPNR\`
  * **PRHI** (WBS Hierarchy): su \`PSPNR=POSNR_U\` o \`PSPNR=POSNR_O\`
  * **RPSCO** (Project info database): su \`OBJNR\`

**RPSCO (Project info database: Costs, revenues, finances)**
* Chiavi Primarie: \`OBJNR, GJAHR, WRTTP, VERSN, ...\`
* Descrizione: Tabella dati con costi, ricavi e finanze aggregate per oggetti di progetto (WBS, Network, etc.).
* Possibili Join:
  * **PRPS** (WBS Element Master Data): su \`OBJNR\`

**MSPR (Project stock)**
* Chiavi Primarie: \`MATNR, WERKS, PSPNR, ...\`
* Descrizione: Gestisce gli stock di materiale con riferimento specifico a un elemento WBS (Project Stock).
* Possibili Join:
  * **PRPS** (WBS Element Master Data): su \`PSPNR\`

#### Equipment
**EQUI (Equipment master data)**
* Chiavi Primarie: \`EQUNR\`
* Descrizione: Dati anagrafici dell'equipment (macchinario, asset).
* Possibili Join:
  * **EQKT** (Equipment short text): su \`EQUNR, SPRAS\`
  * **EQUZ** (Equipment time segment): su \`EQUNR\`

**EQKT (Equipment short text)**
* Chiavi Primarie: \`SPRAS, EQUNR\`
* Descrizione: Testi brevi descrittivi dell'equipment.
* Possibili Join:
  * **EQUI** (Equipment master data): su \`EQUNR\`

**EQUZ (Equipment time segment)**
* Chiavi Primarie: \`EQUNR, DATBI\`
* Descrizione: Storico delle installazioni/smontaggi e assegnazioni dell'equipment.
* Possibili Join:
  * **EQUI** (Equipment master data): su \`EQUNR\`
`;