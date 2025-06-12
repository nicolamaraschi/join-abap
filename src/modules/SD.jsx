export const sdData = `
### Modulo SD (Sales and Distribution)

**VBFA (Document flow (alg.))**
* Chiavi Primarie: \`RUUID\`
* Descrizione: Tabella del flusso documenti. Collega i documenti di vendita in sequenza.
* Possibili Join:
  * Nessun join all'interno di questa sottocategoria.

**VTFA (Flow shipping documents)**
* Chiavi Primarie: \`TKNUM, VBELV, POSNV, VBTYP_V, VBELN, POSNN, VBTYP_N\`
* Descrizione: Tabella di customizing che controlla il flusso tra tipi di documenti di fatturazione e di vendita.
* Possibili Join:
  * Nessun join all'interno di questa sottocategoria.

#### Sales order :
**VBAK (Header data)**
* Chiavi Primarie: \`VBELN\`
* Descrizione: Testata dei documenti di vendita (ordini, offerte, contratti).
* Possibili Join:
  * **VBAP** (Item data): su \`VBELN\`
  * **VBPA** (Partners in sales order): su \`VBELN\` (con POSNR = '000000')
  * **VBKD** (Sales district data): su \`VBELN\` (con POSNR = '000000')

**VBAP (Item data)**
* Chiavi Primarie: \`VBELN, POSNR\`
* Descrizione: Posizioni dei documenti di vendita.
* Possibili Join:
  * **VBAK** (Header data): su \`VBELN\`
  * **VBEP** (Data related to line items): su \`VBELN, POSNR\`
  * **VBPA** (Partners in sales order): su \`VBELN, POSNR\`
  * **VBKD** (Sales district data): su \`VBELN, POSNR\`

**VBPA (Partners in sales order)**
* Chiavi Primarie: \`VBELN, POSNR, PARVW\`
* Descrizione: Funzioni partner per testata e posizioni dei documenti di vendita.
* Possibili Join:
  * **VBAK** (Header data): su \`VBELN\`
  * **VBAP** (Item data): su \`VBELN, POSNR\`

**VBKD (Sales district data)**
* Chiavi Primarie: \`VBELN, POSNR\`
* Descrizione: Dati commerciali aggiuntivi per i documenti di vendita.
* Possibili Join:
  * **VBAK** (Header data): su \`VBELN\`
  * **VBAP** (Item data): su \`VBELN, POSNR\`

**VBEP (Data related to line items, delivery lines)**
* Chiavi Primarie: \`VBELN, POSNR, ETENR\`
* Descrizione: Schedulazioni di consegna per una posizione di un documento di vendita.
* Possibili Join:
  * **VBAP** (Item data): su \`VBELN, POSNR\`

#### Billing document :
**VBRK (header data)**
* Chiavi Primarie: \`VBELN\`
* Descrizione: Testata dei documenti di fatturazione.
* Possibili Join:
  * **VBRP** (Item data): su \`VBELN\`

**VBRP (Item data)**
* Chiavi Primarie: \`VBELN, POSNR\`
* Descrizione: Posizioni dei documenti di fatturazione.
* Possibili Join:
  * **VBRK** (header data): su \`VBELN\`

#### Shipping :
**VTTK (Shipment header)**
* Chiavi Primarie: \`TKNUM\`
* Descrizione: Testata del documento di trasporto/spedizione.
* Possibili Join:
  * **VTTP** (Shipment item): su \`TKNUM\`
  * **VTTS** (Stage in transport): su \`TKNUM\`
  * **VTPA** (Shipment partners): su \`TKNUM\`
  * **VEKP** (Handling Unit - Header Table): su \`VPOBJKEY=TKNUM\` (con VPOBJ = '03')

**VTTP (Shipment item)**
* Chiavi Primarie: \`TKNUM, TPNUM\`
* Descrizione: Posizioni del documento di trasporto (tipicamente le consegne incluse).
* Possibili Join:
  * **VTTK** (Shipment header): su \`TKNUM\`
  * **VTSP** (Stage in transport per shipment item): su \`TKNUM, TPNUM\`

**VTTS (Stage in transport)**
* Chiavi Primarie: \`TKNUM, TSNUM\`
* Descrizione: Tappe (stages) definite per un trasporto.
* Possibili Join:
  * **VTTK** (Shipment header): su \`TKNUM\`
  * **VTSP** (Stage in transport per shipment item): su \`TKNUM, TSNUM\`

**VTSP (Stage in transport per shipment item)**
* Chiavi Primarie: \`TKNUM, TSNUM, TPNUM\`
* Descrizione: Associazione tra posizione di trasporto (consegna) e tappa.
* Possibili Join:
  * **VTTP** (Shipment item): su \`TKNUM, TPNUM\`
  * **VTTS** (Stage in transport): su \`TKNUM, TSNUM\`

**VTPA (Shipment partners)**
* Chiavi Primarie: \`VBELN, POSNR, PARVW\`
* Descrizione: Funzioni partner per il documento di trasporto (es. spedizioniere).
* Possibili Join:
  * **VTTK** (Shipment header): su \`TKNUM\`

**VEKP (Handling Unit - Header Table)**
* Chiavi Primarie: \`VENUM\`
* Descrizione: Testata dell'Handling Unit (unità di imballaggio/movimentazione).
* Possibili Join:
  * **VEPO** (Packing: Handling Unit Item (Contents)): su \`VENUM\`
  * **VTTK** (Shipment header): su \`VPOBJKEY=TKNUM\` (se HU assegnata al trasporto)

**VEPO (Packing: Handling Unit Item (Contents))**
* Chiavi Primarie: \`VENUM, VEPOS\`
* Descrizione: Contenuto (posizioni) di un'Handling Unit.
* Possibili Join:
  * **VEKP** (Handling Unit - Header Table): su \`VENUM\`

#### Delivery :
**LIKP (Delivery header)**
* Chiavi Primarie: \`VBELN\`
* Descrizione: Testata del documento di consegna.
* Possibili Join:
  * **LIPS** (Delivery item): su \`VBELN\`

**LIPS (Delivery item)**
* Chiavi Primarie: \`VBELN, POSNR\`
* Descrizione: Posizioni del documento di consegna.
* Possibili Join:
  * **LIKP** (Delivery header): su \`VBELN\`

#### Pricing :
**KONH (Conditions header)**
* Chiavi Primarie: \`KNUMH\`
* Descrizione: Testata dei record di condizione di prezzo (anagrafica).
* Possibili Join:
  * **KONP** (Conditions items): su \`KNUMH\`

**KONP (Conditions items)**
* Chiavi Primarie: \`KNUMH, KOPOS\`
* Descrizione: Posizioni dei record di condizione di prezzo.
* Possibili Join:
  * **KONH** (Conditions header): su \`KNUMH\`

**KONV (Procedure (billing doc or sales order))**
* Chiavi Primarie: \`KNUMV, KPOSN, STUNR, ZAEHK\`
* Descrizione: Condizioni di prezzo in un documento transazionale. Nota: Sostituita da PRCD_ELEMENTS in S/4HANA.
* Possibili Join:
  * Nessun join all'interno di questa sottocategoria.

**KOND (Conditions: Data part)**
* Chiavi Primarie: \`KNUMD, ZUSKO\`
* Descrizione: Tabella cluster che contiene i dati dei record di condizione (obsoleta, dati ora in KONH/KONP).
* Possibili Join:
  * Nessun join all'interno di questa sottocategoria.

#### contracts :
**VEDA (Contract data)**
* Chiavi Primarie: \`VBELN, VPOSN\`
* Descrizione: Dati specifici dei contratti.
* Possibili Join:
  * Nessun join all'interno di questa sottocategoria.
`;