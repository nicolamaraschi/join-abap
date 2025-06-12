export const qmData = `
### Modulo QM (Quality Management)

#### Lotto di Controllo e Decisione d'Impiego
**QALS (Inspection Lot Record)**
* Chiavi Primarie: \`PRUEFLOS\`
* Descrizione: Dati del lotto di controllo qualità.
* Possibili Join:
  * **QAVE** (Inspection Processing: Usage Decision): su \`PRUEFLOS\`

**QAVE (Inspection Processing: Usage Decision)**
* Chiavi Primarie: \`PRUEFLOS, KZART, ZAEHLER\`
* Descrizione: Memorizza la decisione d'impiego presa per un lotto di controllo.
* Possibili Join:
  * **QALS** (Inspection lot record): su \`PRUEFLOS\`

#### Risultati del Controllo
**QAMR (Characteristic Results during Inspection Processing)**
* Chiavi Primarie: \`PRUEFLOS, VORGLFNR, MERKNR\`
* Descrizione: Risultati per le caratteristiche del lotto di controllo.
* Possibili Join:
  * **QASR** (Sample Results for Inspection Characteristics): su \`PRUEFLOS, VORGLFNR, MERKNR\`

**QASR (Sample Results for Inspection Characteristics)**
* Chiavi Primarie: \`PRUEFLOS, VORGLFNR, MERKNR, PROBENR\`
* Descrizione: Risultati per i campioni delle caratteristiche del lotto di controllo.
* Possibili Join:
  * **QAMR** (Characteristic Results): su \`PRUEFLOS, VORGLFNR, MERKNR\`

#### Anagrafiche QM (Piani, Caratteristiche)
**QPMK (Master Inspection Characteristics)**
* Chiavi Primarie: \`ZAEHLER, MKMNR, VERSION\`
* Descrizione: Dati anagrafici delle caratteristiche di controllo master.
* Possibili Join:
  * **QPMT** (Texts for Master Inspection Characteristics): su \`ZAEHLER, MKMNR, VERSION, SPRACHE\`
  * **PLMK** (Inspection Characteristics in Task List): su \`MKMNR\`

**QPMT (Texts for Master Inspection Characteristics)**
* Chiavi Primarie: \`ZAEHLER, MKMNR, VERSION, SPRACHE\`
* Descrizione: Testi per le caratteristiche di controllo master.
* Possibili Join:
  * **QPMK** (Master Inspection Characteristics): su \`ZAEHLER, MKMNR, VERSION\`

**PLKO (Task List - Header - usata per Piani di Controllo)**
* Chiavi Primarie: \`PLNTY, PLNNR, PLNAL, ZAEHL\`
* Descrizione: Testata dei Piani di Controllo (PLNTY='Q').
* Possibili Join:
  * **PLMK** (Inspection Characteristics in Task List): su \`PLNTY, PLNNR, PLNKN\` (PLNKN da PLPO)

**PLMK (Inspection Characteristics in Task List)**
* Chiavi Primarie: \`PLNTY, PLNNR, PLNKN, KZEINSTELL, MERKNR, ZAEHL\`
* Descrizione: Caratteristiche di controllo assegnate a un'operazione del piano.
* Possibili Join:
  * **QPMK** (Master Inspection Characteristics): su \`MKMNR\`
`;