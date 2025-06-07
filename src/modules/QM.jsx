export const qmData = `
### Modulo QM (Quality Management)

#### Lotto di Controllo e Decisione d'Impiego
**QALS (Inspection Lot Record)**
* Chiavi Primarie: \`MANDT, PRUEFLOS\`
* Descrizione: Dati del lotto di controllo.
* Possibili Join:
  * **QAVE** (Inspection Processing: Usage Decision): su \`MANDT, PRUEFLOS\`
  * **QAMR** (Characteristic Results during Inspection Processing): su \`MANDT, PRUEFLOS\`
  * **QASR** (Sample Results for Inspection Characteristics): su \`MANDT, PRUEFLOS\`
  * **MARA** (Material Master): su \`MANDT, MATNR\`
  * **LFA1** (Vendor Master): su \`MANDT, LIFNR\` (se origine lotto è fornitore)
  * **KNA1** (Customer Master): su \`MANDT, KUNDE\` (se origine lotto è cliente/consegna)
  * **EKPO** (Purchasing Document Item): su \`MANDT, EBELN, EBELP\` (se origine lotto è EM da OdA)
  * **LIPS** (Delivery Item): su \`MANDT, VBELN, POSNR\` (se origine lotto è consegna)
  * **AFPO** (Production Order Item): su \`MANDT, AUFNR\` (se origine lotto è produzione)

**QAVE (Inspection Processing: Usage Decision)**
* Chiavi Primarie: \`MANDT, PRUEFLOS\`
* Descrizione: Decisione d'impiego per il lotto di controllo.
* Possibili Join:
  * **QALS** (Inspection Lot Record): su \`MANDT, PRUEFLOS\`

#### Risultati del Controllo
**QAMR (Characteristic Results during Inspection Processing)**
* Chiavi Primarie: \`MANDT, PRUEFLOS, MERKNR, ZAEHLER\` (ZAEHLER è contatore per risultati multipli)
* Descrizione: Risultati per le caratteristiche del lotto di controllo.
* Possibili Join:
  * **QALS** (Inspection Lot Record): su \`MANDT, PRUEFLOS\`
  * **QPMK** (Master Inspection Characteristics): su \`MANDT, MKMNR=MERKNR\` (se MERKNR è da anagrafica)

**QASR (Sample Results for Inspection Characteristics)**
* Chiavi Primarie: \`MANDT, PRUEFLOS, PROBENR, MERKNR, ZAEHLER\`
* Descrizione: Risultati per i campioni delle caratteristiche del lotto di controllo.
* Possibili Join:
  * **QALS** (Inspection Lot Record): su \`MANDT, PRUEFLOS\`
  * **QAMR** (Characteristic Results): su \`MANDT, PRUEFLOS, MERKNR\`
  * **QPMK** (Master Inspection Characteristics): su \`MANDT, MKMNR=MERKNR\`

#### Notifiche QM
**(Vedere QMEL, QMFE, QMMA, QMSM, QMUR nel modulo PM, dato che sono tabelle condivise)**

#### Anagrafiche QM (Piani, Caratteristiche)
**QPMK (Master Inspection Characteristics)**
* Chiavi Primarie: \`MANDT, WERKS, MKMNR, VERSION\`
* Descrizione: Dati anagrafici delle caratteristiche di controllo master.
* Possibili Join:
  * **QPMT** (Texts for Master Inspection Characteristics): su \`MANDT, WERKS, MKMNR, VERSION, SPRAS\`
  * **QAMR** (Characteristic Results): su \`MANDT, MERKNR=MKMNR\` (e \`WERKS\` se rilevante)

**QPMT (Texts for Master Inspection Characteristics)**
* Chiavi Primarie: \`MANDT, SPRAS, WERKS, MKMNR, VERSION\`
* Possibili Join:
  * **QPMK** (Master Inspection Characteristics): su \`MANDT, WERKS, MKMNR, VERSION\`

**PLKO (Task List - Header - usata per Piani di Controllo)**
* Chiavi Primarie: \`MANDT, PLNTY, PLNNR, PLNAL, ZAEHL\`
* (Vedi Modulo PP per join generali; PLNTY='Q' per piani QM)
* Possibili Join:
  * **PLMK** (Inspection Characteristics in Task List): su \`MANDT, PLNTY, PLNNR, PLNKN\` (PLNKN da PLPO)
  * **MAPL** (Material-Task List Assignments): su \`MANDT, PLNTY, PLNNR, PLNAL\`

**PLMK (Inspection Characteristics in Task List)**
* Chiavi Primarie: \`MANDT, PLNTY, PLNNR, PLNKN, ZAEHL_MK\`
* Descrizione: Caratteristiche di controllo assegnate a un'operazione del piano.
* Possibili Join:
  * **PLPO** (Task List Operation): su \`MANDT, PLNTY, PLNNR, PLNKN\`
  * **QPMK** (Master Inspection Characteristics): su \`MANDT, WERKS, MKMNR\` (MKMNR da PLMK)
`;