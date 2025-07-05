export const sdData = `
### Modulo SD (Sales and Distribution)

**VBFA (Document flow (alg.))**
* Chiavi Primarie: \`MANDT(MANDT), RUUID(SD_DOC_REL_UUID)\`
* Descrizione: Tabella del flusso documenti. Collega i documenti di vendita in sequenza usando VBELV (doc. precedente) e VBELN (doc. successivo).
* Possibili Join:
  * **VBAK/LIKP/VBRK** (Header precedente): su \`VBELV\`
  * **VBAK/LIKP/VBRK** (Header successivo): su \`VBELN\`

**VTFA (Flow shipping documents)**
* Chiavi Primarie: \`MANDT(MANDT), TKNUM(TKNUM), VBELV(VBELN_VON), POSNV(POSNR_VON), VBTYP_V(VBTYPL), VBELN(VBELN_NACH), POSNN(POSNR_NACH), VBTYP_N(VBTYPL)\`
* Descrizione: Flusso dei documenti di trasporto.
* Possibili Join:
  * **VTTK** (Shipment header): su \`TKNUM\`

#### Sales order :
**VBAK (Header data)**
* Chiavi Primarie: \`MANDT(MANDT), VBELN(VBELN_VA)\`
* Descrizione: Testata dei documenti di vendita (ordini, offerte, contratti).
* Possibili Join:
  * **VBAP** (Item data): su \`VBELN\`
  * **VBPA** (Partners in sales order): su \`VBELN\` (con POSNR = '000000')
  * **VBKD** (Sales district data): su \`VBELN\` (con POSNR = '000000')
  * **VBUK** (Header Status): su \`VBELN\`
  * **VBBE** (Sales Requirements): su \`VBELN\`
  * **VBLB** (Release Order Data): su \`VBELN\`

**VBAP (Item data)**
* Chiavi Primarie: \`MANDT(MANDT), VBELN(VBELN_VA), POSNR(POSNR_VA)\`
* Descrizione: Posizioni dei documenti di vendita.
* Possibili Join:
  * **VBAK** (Header data): su \`VBELN\`
  * **VBEP** (Data related to line items): su \`VBELN, POSNR\`
  * **VBPA** (Partners in sales order): su \`VBELN, POSNR\`
  * **VBKD** (Sales district data): su \`VBELN, POSNR\`
  * **VBUP** (Item Status): su \`VBELN, POSNR\`
  * **LIPS** (Delivery item): su \`VGBEL=VBELN, VGPOS=POSNR\`

**VBPA (Partners in sales order)**
* Chiavi Primarie: \`MANDT(MANDT), VBELN(VBELN), POSNR(POSNR), PARVW(PARVW)\`
* Descrizione: Funzioni partner per testata e posizioni dei documenti di vendita.
* Possibili Join:
  * **VBAK** (Header data): su \`VBELN\`
  * **VBAP** (Item data): su \`VBELN, POSNR\`

**VBKD (Sales district data)**
* Chiavi Primarie: \`MANDT(MANDT), VBELN(VBELN), POSNR(POSNR)\`
* Descrizione: Dati commerciali aggiuntivi per i documenti di vendita.
* Possibili Join:
  * **VBAK** (Header data): su \`VBELN\`
  * **VBAP** (Item data): su \`VBELN, POSNR\`

**VBEP (Data related to line items, delivery lines)**
* Chiavi Primarie: \`MANDT(MANDT), VBELN(VBELN_VA), POSNR(POSNR_VA), ETENR(ETENR)\`
* Descrizione: Schedulazioni di consegna per una posizione di un documento di vendita.
* Possibili Join:
  * **VBAP** (Item data): su \`VBELN, POSNR\`
  * **VBEH** (Schedule Line History): su \`VBELN, POSNR, ETENR\`

**VBEH (Schedule Line History)**
* Chiavi Primarie: \`MANDT(MANDT), VBELN(VBELN_VA), POSNR(POSNR_VA), ETENR(ETENR), ABRLI(ABRLI), ABART(ABART)\`
* Descrizione: Storico delle schedulazioni.
* Possibili Join:
  * **VBEP** (Data related to line items): su \`VBELN, POSNR, ETENR\`

**VBBE (Sales Requirements: Individual Records)**
* Chiavi Primarie: \`MANDT(MANDT), VBELN(VBELN_VA), POSNR(POSNR_VA), ETENR(ETENR)\`
* Descrizione: Fabbisogni di vendita.
* Possibili Join:
  * **VBAK** (Header data): su \`VBELN\`

**VBLB (Release Order Data)**
* Chiavi Primarie: \`MANDT(MANDT), VBELN(VBELN_VA), POSNR(POSNR_VA), ABRLI(ABRLI), ABART(ABART)\`
* Descrizione: Dati Ordine Aperto.
* Possibili Join:
  * **VBAK** (Header data): su \`VBELN\`

#### Billing document :
**VBRK (header data)**
* Chiavi Primarie: \`MANDT(MANDT), VBELN(VBELN_VF)\`
* Descrizione: Testata dei documenti di fatturazione.
* Possibili Join:
  * **VBRP** (Item data): su \`VBELN\`
  * **KONV** (Procedure): su \`KNUMV\`
  * **NAST** (Message Status): su \`OBJKY=VBELN\`

**VBRP (Item data)**
* Chiavi Primarie: \`MANDT(MANDT), VBELN(VBELN_VF), POSNR(POSNR_VF)\`
* Descrizione: Posizioni dei documenti di fatturazione.
* Possibili Join:
  * **VBRK** (header data): su \`VBELN\`
  * **LIPS** (Delivery item): su \`VGBEL=VBELN, VGPOS=POSNR\`

**VBSK (Collective Processing for a Sales Document Header)**
* Chiavi Primarie: \`MANDT(MANDT), SAMMG(SAMMG)\`
* Descrizione: Testata per l'elaborazione collettiva.
* Possibili Join:
  * **VBSS** (Collective Processing: Sales Documents): su \`SAMMG\`

**VBSS (Collective Processing: Sales Documents)**
* Chiavi Primarie: \`MANDT(MANDT), SAMMG(SAMMG), VBELN(VBELN)\`
* Descrizione: Documenti di vendita raggruppati per l'elaborazione collettiva.
* Possibili Join:
  * **VBSK** (Collective Processing Header): su \`SAMMG\`

#### Shipping :
**VTTK (Shipment header)**
* Chiavi Primarie: \`MANDT(MANDT), TKNUM(TKNUM)\`
* Descrizione: Testata del documento di trasporto/spedizione.
* Possibili Join:
  * **VTTP** (Shipment item): su \`TKNUM\`
  * **VTTS** (Stage in transport): su \`TKNUM\`
  * **VTPA** (Shipment partners): su \`TKNUM\`
  * **VEKP** (Handling Unit - Header Table): su \`VPOBJKEY=TKNUM\` (con VPOBJ = '03')
  * **VTFA** (Flow shipping documents): su \`TKNUM\`

**VTTP (Shipment item)**
* Chiavi Primarie: \`MANDT(MANDT), TKNUM(TKNUM), TPNUM(TPNUM)\`
* Descrizione: Posizioni del documento di trasporto (tipicamente le consegne incluse).
* Possibili Join:
  * **VTTK** (Shipment header): su \`TKNUM\`
  * **VTSP** (Stage in transport per shipment item): su \`TKNUM, TPNUM\`
  * **LIKP** (Delivery header): su \`VBELN\`

**VTTS (Stage in transport)**
* Chiavi Primarie: \`MANDT(MANDT), TKNUM(TKNUM), TSNUM(TSNUM)\`
* Descrizione: Tappe (stages) definite per un trasporto.
* Possibili Join:
  * **VTTK** (Shipment header): su \`TKNUM\`
  * **VTSP** (Stage in transport per shipment item): su \`TKNUM, TSNUM\`

**VTSP (Stage in transport per shipment item)**
* Chiavi Primarie: \`MANDT(MANDT), TKNUM(TKNUM), TSNUM(TSNUM), TPNUM(TPNUM)\`
* Descrizione: Associazione tra posizione di trasporto (consegna) e tappa.
* Possibili Join:
  * **VTTP** (Shipment item): su \`TKNUM, TPNUM\`
  * **VTTS** (Stage in transport): su \`TKNUM, TSNUM\`

**VTPA (Shipment partners)**
* Chiavi Primarie: \`MANDT(MANDT), VBELN(VBELN), POSNR(POSNR), PARVW(PARVW)\`
* Descrizione: Funzioni partner per il documento di trasporto (es. spedizioniere).
* Possibili Join:
  * **VTTK** (Shipment header): su \`TKNUM\`

**VEKP (Handling Unit - Header Table)**
* Chiavi Primarie: \`MANDT(MANDT), VENUM(VENUM)\`
* Descrizione: Testata dell'Handling Unit (unità di imballaggio/movimentazione).
* Possibili Join:
  * **VEPO** (Packing: Handling Unit Item (Contents)): su \`VENUM\`
  * **VTTK** (Shipment header): su \`VPOBJKEY=TKNUM\` (se HU assegnata al trasporto)

**VEPO (Packing: Handling Unit Item (Contents))**
* Chiavi Primarie: \`MANDT(MANDT), VENUM(VENUM), VEPOS(VEPOS)\`
* Descrizione: Contenuto (posizioni) di un'Handling Unit.
* Possibili Join:
  * **VEKP** (Handling Unit - Header Table): su \`VENUM\`
  * **LIPS** (Delivery item): su \`VBELN, POSNR\`

#### Delivery :
**LIKP (Delivery header)**
* Chiavi Primarie: \`MANDT(MANDT), VBELN(VBELN_VL)\`
* Descrizione: Testata del documento di consegna.
* Possibili Join:
  * **LIPS** (Delivery item): su \`VBELN\`
  * **VBUK** (Header Status): su \`VBELN\`
  * **VTTK** (Shipment header): tramite la tabella VTTP.
  * **VBLK** (SD:Delivery Note Header): su \`VBELN\`

**LIPS (Delivery item)**
* Chiavi Primarie: \`MANDT(MANDT), VBELN(VBELN_VL), POSNR(POSNR_VL)\`
* Descrizione: Posizioni del documento di consegna.
* Possibili Join:
  * **LIKP** (Delivery header): su \`VBELN\`
  * **VBUP** (Item Status): su \`VBELN, POSNR\`
  * **VBAP** (Item data): su \`VBELN=VGBEL, POSNR=VGPOS\`
  * **VBRP** (Item data): su \`VBELN=VGBEL, POSNR=VGPOS\`

**VBLK (SD:Delivery Note Header)**
* Chiavi Primarie: \`MANDT(MANDT), VBELN(VBELN_VL)\`
* Descrizione: Testata Nota di Consegna (distinta dalla consegna stessa).
* Possibili Join:
  * **LIKP** (Delivery header): su \`VBELN\`

#### Status and Index Tables
**VBUK (Header Status and Administrative Data)**
* Chiavi Primarie: \`MANDT(MANDT), VBELN(VBELN)\`
* Descrizione: Stato di elaborazione della testata per documenti di vendita, consegna e fatturazione.
* Possibili Join:
  * **VBAK/LIKP/VBRK**: su \`VBELN\`

**VBUP (Item Status)**
* Chiavi Primarie: \`MANDT(MANDT), VBELN(VBELN), POSNR(POSNR)\`
* Descrizione: Stato di elaborazione della posizione per documenti di vendita, consegna e fatturazione.
* Possibili Join:
  * **VBAP/LIPS/VBRP**: su \`VBELN, POSNR\`

**VKDFS (SD Index: Billing Initiator)**
* Chiavi Primarie: \`MANDT(MANDT), FKTYP(FKTYP), VKORG(VKORG), FKDAT(FKDAT), KUNNR(KUNAG), FKART(FKART), LLAND(LLAND), VBELN(VBELN)\`
* Descrizione: Indice dei documenti da fatturare.
* Possibili Join:
  * Nessun join diretto, è una tabella di lavoro per la creazione delle fatture.

**VAPMA (SD Index: Order Items by Material)**
* Chiavi Primarie: \`MANDT(MANDT), MATNR(MATNR), VKORG(VKORG), TRVOG(TRVOG), AUDAT(AUDAT), VTWEG(VTWEG), SPART(SPART), AUART(AUART), KUNNR(KUNAG), VKBUR(VKBUR), VKGRP(VKGRP), BSTNK(BSTNK), ERNAM(ERNAM), VBELN(VBELN), POSNR(POSNR)\`
* Descrizione: Indice delle posizioni d'ordine per materiale.
* Possibili Join:
  * **VBAP**: su \`VBELN, POSNR\`

**VAKPA (SD Index: Order by Partner Function)**
* Chiavi Primarie: \`MANDT(MANDT), KUNDE(KUNDE_D), PARVW(PARVW), VKORG(VKORG), TRVOG(TRVOG), AUDAT(AUDAT), VKBUR(VKBUR), VKGRP(VKGRP), VTWEG(VTWEG), SPART(SPART), AUART(AUART), BSTNK(BSTNK), KUNNR(KUNAG), ERNAM(ERNAM), VBELN(VBELN)\`
* Descrizione: Indice degli ordini per funzione partner.
* Possibili Join:
  * **VBAK**: su \`VBELN\`

**VRPMA (SD Index: Billing Items per Material)**
* Chiavi Primarie: \`MANDT(MANDT), MATNR(MATNR), VKORG(VKORG), FKDAT(FKDAT), VTWEG(VTWEG), FKART(FKART), KUNNR(KUNRG), KUNAG(KUNAG), VBTYP(VBTYPL), ERNAM(ERNAM), VBELN(VBELN_VF), POSNR(POSNR_VF)\`
* Descrizione: Indice delle posizioni di fatturazione per materiale.
* Possibili Join:
  * **VBRP**: su \`VBELN, POSNR\`

**NAST (Message Status)**
* Chiavi Primarie: \`MANDT(MANDT), KAPPL(SNA_KAPPL), OBJKY(NA_OBJKEY), KSCHL(SNA_KSCHL), SPRAS(NA_SPRAS), PARNR(NA_PARNR), PARVW(SNA_PARVW), ERDAT(NA_ERDAT), ERUHR(NA_ERUHR)\`
* Descrizione: Stato dei messaggi di output (stampe, EDI, etc.).
* Possibili Join:
  * **VBRK** (Header fattura): su \`OBJKY=VBELN\`
  * **VBAK** (Header ordine): su \`OBJKY=VBELN\`
  * **LIKP** (Header consegna): su \`OBJKY=VBELN\`

#### Pricing :
**KONH (Conditions header)**
* Chiavi Primarie: \`MANDT(MANDT), KNUMH(KNUMH)\`
* Descrizione: Testata dei record di condizione di prezzo (anagrafica).
* Possibili Join:
  * **KONP** (Conditions items): su \`KNUMH\`

**KONP (Conditions items)**
* Chiavi Primarie: \`MANDT(MANDT), KNUMH(KNUMH), KOPOS(KOPOS)\`
* Descrizione: Posizioni dei record di condizione di prezzo.
* Possibili Join:
  * **KONH** (Conditions header): su \`KNUMH\`

**KONV (Conditions (Transaction Data))**
* Chiavi Primarie: \`MANDT(MANDT), KNUMV(KNUMV), KPOSN(KPOSN), STUNR(STUNR), ZAEHK(DZAEHK)\`
* Descrizione: Condizioni di prezzo in un documento transazionale. Nota: Sostituita da PRCD_ELEMENTS in S/4HANA.
* Possibili Join:
  * **VBRK** (Billing Document Header): su \`KNUMV\`
  * **VBAK** (Sales Document Header): su \`KNUMV\`

**KOND (Conditions: Data part)**
* Chiavi Primarie: \`MANDT(MANDT), KNUMD(KNUMD), ZUSKO(DZUSKO)\`
* Descrizione: Tabella cluster che contiene i dati dei record di condizione (obsoleta, dati ora in KONH/KONP).
* Possibili Join:
  * Nessun join all'interno di questa sottocategoria.

#### contracts :
**VEDA (Contract data)**
* Chiavi Primarie: \`MANDT(MANDT), VBELN(VBELN_VA), VPOSN(POSNR_VA)\`
* Descrizione: Dati specifici dei contratti.
* Possibili Join:
  * Nessun join all'interno di questa sottocategoria.
`;