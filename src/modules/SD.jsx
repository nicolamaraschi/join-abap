export const sdData = `
### Modulo SD (Sales and Distribution)

**VBFA (Document flow (alg.))**
* Chiavi Primarie: \`MANDT, VBELV, POSNV, VBELN, POSNN, VBTYP_N\`
* Descrizione: Tabella del flusso documenti. Collega i documenti di vendita in sequenza (es. ordine -> consegna -> fattura).
* Possibili Join:
  * **VBAK** (Sales Document Header): su \`MANDT, VBELV=VBELN\` (come precedente) o \`VBELN=VBELN\` (come successivo)
  * **LIKP** (Delivery Header): su \`MANDT, VBELV=VBELN\` (come precedente) o \`VBELN=VBELN\` (come successivo)
  * **VBRK** (Billing Document Header): su \`MANDT, VBELV=VBELN\` (come precedente) o \`VBELN=VBELN\` (come successivo)

**VTFA (Flow shipping documents)**
* Chiavi Primarie: \`MANDT, VFART, RFART\`
* Descrizione: Tabella di customizing che controlla il flusso tra tipi di documenti di fatturazione e di vendita.
* Possibili Join:
  * **TVFK** (Billing Document Types): su \`MANDT, VFART=FKART\`
  * **TVAK** (Sales Document Types): su \`MANDT, RFART=AUART\`

#### Sales order :
**VBAK (Header data)**
* Chiavi Primarie: \`MANDT, VBELN\`
* Descrizione: Testata dei documenti di vendita (ordini, offerte, contratti).
* Possibili Join:
  * **VBAP** (Item data): su \`MANDT, VBELN\`
  * **VBPA** (Partners in sales order): su \`MANDT, VBELN\` (con POSNR = '000000')
  * **VBKD** (Sales district data): su \`MANDT, VBELN\` (con POSNR = '000000')
  * **VBFA** (Document Flow): su \`MANDT, VBELV=VBELN\`
  * **KNA1** (Customer Master): su \`MANDT, KUNNR\`

**VBAP (Item data)**
* Chiavi Primarie: \`MANDT, VBELN, POSNR\`
* Descrizione: Posizioni dei documenti di vendita.
* Possibili Join:
  * **VBAK** (Header data): su \`MANDT, VBELN\`
  * **VBEP** (Data related to line items): su \`MANDT, VBELN, POSNR\`
  * **MARA** (Material Master): su \`MANDT, MATNR\`
  * **VBPA** (Partners in sales order): su \`MANDT, VBELN, POSNR\`

**VBPA (Partners in sales order)**
* Chiavi Primarie: \`MANDT, VBELN, POSNR, PARVW\`
* Descrizione: Funzioni partner per testata e posizioni dei documenti di vendita.
* Possibili Join:
  * **VBAK** (Header data): su \`MANDT, VBELN\`
  * **KNA1** (Customer Master): su \`MANDT, KUNNR\` (KUNNR è il numero del partner in VBPA)
  * **LFA1** (Vendor Master): su \`MANDT, LIFNR\` (se il partner è un fornitore)

**VBKD (Sales district data)**
* Chiavi Primarie: \`MANDT, VBELN, POSNR\`
* Descrizione: Dati commerciali aggiuntivi per i documenti di vendita (es. termini di pagamento, Incoterms).
* Possibili Join:
  * **VBAK** (Header data): su \`MANDT, VBELN\`
  * **VBAP** (Item data): su \`MANDT, VBELN, POSNR\`

**VBEP (Data related to line items, delivery lines)**
* Chiavi Primarie: \`MANDT, VBELN, POSNR, ETENR\`
* Descrizione: Schedulazioni di consegna per una posizione di un documento di vendita.
* Possibili Join:
  * **VBAP** (Item data): su \`MANDT, VBELN, POSNR\`

#### Billing document :
**VBRK (header data)**
* Chiavi Primarie: \`MANDT, VBELN\`
* Descrizione: Testata dei documenti di fatturazione.
* Possibili Join:
  * **VBRP** (Item data): su \`MANDT, VBELN\`
  * **BKPF** (Accounting documents): su \`MANDT, AWKEY\` (contenente VBRK-VBELN), \`AWSYS\`
  * **VBFA** (Document Flow): su \`MANDT, VBELN\` (come doc. successivo)
  * **KNA1** (Customer Master): su \`MANDT, KUNAG\` (Pagatore)

**VBRP (Item data)**
* Chiavi Primarie: \`MANDT, VBELN, POSNR\`
* Descrizione: Posizioni dei documenti di fatturazione.
* Possibili Join:
  * **VBRK** (Header data): su \`MANDT, VBELN\`
  * **MARA** (Material Master): su \`MANDT, MATNR\`
  * **VBAP** (Sales Order Item): su \`MANDT, AUBEL=VBELN, AUPOS=POSNR\`
  * **LIPS** (Delivery Item): su \`MANDT, VGBEL=VBELN, VGPOS=POSNR\`

#### Shipping :
**VTTK (Shipment header)**
* Chiavi Primarie: \`MANDT, TKNUM\`
* Descrizione: Testata del documento di trasporto/spedizione.
* Possibili Join:
  * **VTTP** (Shipment item): su \`MANDT, TKNUM\`
  * **VTPA** (Shipment partners): su \`MANDT, TKNUM\`
  * **VEKP** (Handling Unit Header): su \`MANDT, VPOBJKEY=TKNUM\` (con VPOBJ = '03')

**VTTP (Shipment item)**
* Chiavi Primarie: \`MANDT, TKNUM, TPNUM\`
* Descrizione: Posizioni del documento di trasporto (tipicamente le consegne incluse).
* Possibili Join:
  * **VTTK** (Shipment header): su \`MANDT, TKNUM\`
  * **LIKP** (Delivery header): su \`MANDT, VBELN\`

**VTTS (Stage in transport)**
* Chiavi Primarie: \`MANDT, TKNUM, TSNUM\`
* Descrizione: Tappe (stages) definite per un trasporto.
* Possibili Join:
  * **VTTK** (Shipment header): su \`MANDT, TKNUM\`

**VTSP (Stage in transport per shipment item)**
* Chiavi Primarie: \`MANDT, TKNUM, TPNUM, TSNUM\`
* Descrizione: Associazione tra posizione di trasporto (consegna) e tappa.
* Possibili Join:
  * **VTTP** (Shipment item): su \`MANDT, TKNUM, TPNUM\`
  * **VTTS** (Stage in transport): su \`MANDT, TKNUM, TSNUM\`

**VTPA (Shipment partners)**
* Chiavi Primarie: \`MANDT, TKNUM, PARVW\`
* Descrizione: Funzioni partner per il documento di trasporto (es. spedizioniere).
* Possibili Join:
  * **VTTK** (Shipment header): su \`MANDT, TKNUM\`
  * **LFA1** (Vendor Master): su \`MANDT, LIFNR\` (LIFNR è il numero del partner)

**VEKP (Handling Unit - Header Table)**
* Chiavi Primarie: \`MANDT, VENUM\`
* Descrizione: Testata dell'Handling Unit (unità di imballaggio/movimentazione).
* Possibili Join:
  * **VEPO** (Handling Unit Item): su \`MANDT, VENUM\`
  * **VTTK** (Shipment Header): su \`MANDT, VPOBJKEY=TKNUM\` (se HU assegnata al trasporto)
  * **LIKP** (Delivery Header): su \`MANDT, VPOBJKEY=VBELN\` (se HU assegnata alla consegna)

**VEPO (Packing: Handling Unit Item (Contents))**
* Chiavi Primarie: \`MANDT, VENUM, VEPOST\`
* Descrizione: Contenuto (posizioni) di un'Handling Unit.
* Possibili Join:
  * **VEKP** (Handling Unit Header): su \`MANDT, VENUM\`
  * **MARA** (Material Master): su \`MANDT, MATNR\`
  * **LIPS** (Delivery Item): su \`MANDT, VBELN, POSNR\` (se il contenuto è una posizione di consegna)

#### Delivery :
**LIKP (Delivery header)**
* Chiavi Primarie: \`MANDT, VBELN\`
* Descrizione: Testata del documento di consegna.
* Possibili Join:
  * **LIPS** (Delivery item): su \`MANDT, VBELN\`
  * **VTTP** (Shipment item): su \`MANDT, VBELN\`
  * **VBFA** (Document Flow): su \`MANDT, VBELN\`

**LIPS (Delivery item)**
* Chiavi Primarie: \`MANDT, VBELN, POSNR\`
* Descrizione: Posizioni del documento di consegna.
* Possibili Join:
  * **LIKP** (Delivery header): su \`MANDT, VBELN\`
  * **VBAP** (Sales Order Item): su \`MANDT, VGBEL=VBELN, VGPOS=POSNR\`
  * **MARA** (Material Master): su \`MANDT, MATNR\`

#### Pricing :
**KONH (Conditions header)**
* Chiavi Primarie: \`MANDT, KNUMH\`
* Descrizione: Testata dei record di condizione di prezzo (anagrafica).
* Possibili Join:
  * **KONP** (Conditions items): su \`MANDT, KNUMH\`
  * **Axxx** (Tabelle di condizione): su \`MANDT, KNUMH\`

**KONP (Conditions items)**
* Chiavi Primarie: \`MANDT, KNUMH, KOPOS\`
* Descrizione: Posizioni dei record di condizione di prezzo (dettagli come scaglioni, importi).
* Possibili Join:
  * **KONH** (Conditions header): su \`MANDT, KNUMH\`

**KONV (Procedure (billing doc or sales order))**
* Chiavi Primarie: \`MANDT, KNUMV, KPOSN, STUNR, ZAEHK\`
* Descrizione: Condizioni di prezzo trovate in un documento transazionale (Nota: In S/4HANA sostituita da PRCD_ELEMENTS per nuovi processi).
* Possibili Join:
  * **VBAK** (Sales Document Header): su \`MANDT, KNUMV\`
  * **VBRK** (Billing Document Header): su \`MANDT, KNUMV\`

**KOND (Conditions: Data part)**
* Descrizione: Tabella cluster che contiene i dati dei record di condizione (obsoleta, dati ora in KONH/KONP).

#### contracts :
**VEDA (Contract data)**
* Chiavi Primarie: \`MANDT, VBELN, POSNR, ETENR\`
* Descrizione: Dati specifici dei contratti (es. date di fatturazione, quantità target).
* Possibili Join:
  * **VBAK** (Sales Document Header): su \`MANDT, VBELN\` (se VEDA è a livello testata)
  * **VBAP** (Sales Document Item): su \`MANDT, VBELN, POSNR\`
`;
