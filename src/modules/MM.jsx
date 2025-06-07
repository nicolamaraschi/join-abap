export const mmData = `
### Modulo MM (Materials Management)

#### Material document
**MKPF (Material Document Header)**
* Chiavi Primarie: \`MANDT, MBLNR, MJAHR\`
* Descrizione: Testata del documento materiale. In S/4HANA, MATDOC è la tabella primaria, ma MKPF è ancora usata per compatibilità.
* Possibili Join:
  * **MSEG** (Material Document Segment): su \`MANDT, MBLNR, MJAHR\`
  * **MATDOC** (Universal Journal for Material Docs): su \`MANDT, MBLNR, MJAHR\`
  * **BKPF** (Accounting Document Header): su \`MANDT, AWKEY\` (contenente MBLNR+MJAHR), \`AWSYS\`
  * **EKBE** (History per Purchasing Document): su \`MANDT, BELNR=MBLNR, GJAHR=MJAHR\`

**MSEG (Material Document Segment)**
* Chiavi Primarie: \`MANDT, MBLNR, MJAHR, ZEILE\`
* Descrizione: Posizione del documento materiale. In S/4HANA, MATDOC è la tabella primaria.
* Possibili Join:
  * **MKPF** (Material Document Header): su \`MANDT, MBLNR, MJAHR\`
  * **MARA** (Material Master): su \`MANDT, MATNR\`
  * **EKPO** (Purchase Document Item): su \`MANDT, EBELN, EBELP\`
  * **AUFM** (Goods movement for prod. order): su \`MANDT, MBLNR, MJAHR, ZEILE\`
  * **MATDOC** (Universal Journal for Material Docs): su \`MANDT, MBLNR, MJAHR, ZEILE\`

#### Purchasing
**EKKO (Purchase Document Header)**
* Chiavi Primarie: \`MANDT, EBELN\`
* Descrizione: Testata dei documenti di acquisto (Ordini, Contratti, Richieste d'offerta).
* Possibili Join:
  * **EKPO** (Purchase Document Item): su \`MANDT, EBELN\`
  * **EKKN** (Account Assignment in Purchasing): su \`MANDT, EBELN\`
  * **LFA1** (Vendor Master): su \`MANDT, LIFNR\`
  * **EKPA** (Partner Functions): su \`MANDT, EBELN\`

**EKPO (Purchase Document Item)**
* Chiavi Primarie: \`MANDT, EBELN, EBELP\`
* Descrizione: Posizioni dei documenti di acquisto.
* Possibili Join:
  * **EKKO** (Purchase Document Header): su \`MANDT, EBELN\`
  * **MARA** (Material Master): su \`MANDT, MATNR\`
  * **EINA** (Purchase Info Record - General): su \`MANDT, INFNR\`
  * **EKET** (Delivery Schedule): su \`MANDT, EBELN, EBELP\`
  * **EKES** (Order Confirmations): su \`MANDT, EBELN, EBELP\`
  * **EKBE** (History per Purchasing Document): su \`MANDT, EBELN, EBELP\`
  * **EKPV** (Shipping-Specific Data): su \`MANDT, EBELN, EBELP\`

**EKPV (Shipping-Specific Data on Stock Tfr. for Purch. Doc. Item)**
* Chiavi Primarie: \`MANDT, EBELN, EBELP\`
* Descrizione: Dati specifici di spedizione per le posizioni di ordini di trasferimento stock.
* Possibili Join:
  * **EKPO** (Purchase Document Item): su \`MANDT, EBELN, EBELP\`
  * **VBAK** (Sales Document Header): su \`MANDT, VBELN\`

**EKET (Delivery schedule)**
* Chiavi Primarie: \`MANDT, EBELN, EBELP, ETENR\`
* Descrizione: Schedulazioni di consegna per una posizione di un documento d'acquisto.
* Possibili Join:
  * **EKPO** (Purchase Document Item): su \`MANDT, EBELN, EBELP\`

**VETVG (Delivery Due Index for Stock Transfer)**
* Chiavi Primarie: \`MANDT, VSTEL, ROUTE, AULWE, VDATU, VSTTR, EBELN, EBELP\`
* Descrizione: Indice per la gestione delle consegne dovute per trasferimenti stock.
* Possibili Join:
  * **EKPO** (Purchase Document Item): su \`MANDT, EBELN, EBELP\`

**EKES (Order Acceptance/Fulfillment Confirmations)**
* Chiavi Primarie: \`MANDT, EBELN, EBELP, ETENS\`
* Descrizione: Conferme d'ordine o di consegna dal fornitore.
* Possibili Join:
  * **EKPO** (Purchase Document Item): su \`MANDT, EBELN, EBELP\`

**EKKN (Account assignment in purchasing)**
* Chiavi Primarie: \`MANDT, EBELN, EBELP, ZEKKN\`
* Descrizione: Dati di imputazione contabile per una posizione di documento d'acquisto.
* Possibili Join:
  * **EKPO** (Purchase Document Item): su \`MANDT, EBELN, EBELP\`
  * **CSKS** (Cost Center Master Data): su \`MANDT, KOSTL\`
  * **AUFK** (Order Master Data): su \`MANDT, AUFNR\`
  * **PRPS** (WBS Element Master Data): su \`MANDT, PS_PSP_PNR=PSPNR\`

**EKAN (Vendor address purchasing)**
* Chiavi Primarie: \`MANDT, EBELN, ADRN2\`
* Descrizione: Indirizzi specifici per i documenti d'acquisto (es. indirizzo di consegna diverso).
* Possibili Join:
  * **EKKO** (Purchase Document Header): su \`MANDT, EBELN\`
  * **ADRC** (Address Management): su \`CLIENT=MANDT, ADDRNUMBER=ADRN2\`

**EKPA (Partner functions)**
* Chiavi Primarie: \`MANDT, EBELN, EBELP, PARVW, PARZA\`
* Descrizione: Funzioni partner a livello di testata e posizione del documento d'acquisto.
* Possibili Join:
  * **EKKO** (Purchase Document Header): su \`MANDT, EBELN\`
  * **EKPO** (Purchase Document Item): su \`MANDT, EBELN, EBELP\`
  * **LFA1** (Vendor Master): su \`MANDT, LIFN2=LIFNR\` (LIFN2 è il numero del partner)

**EIPO (Item export / import data)**
* Chiavi Primarie: \`MANDT, EBELN, EBELP\`
* Descrizione: Dati di import/export a livello di posizione del documento d'acquisto.
* Possibili Join:
  * **EKPO** (Purchase Document Item): su \`MANDT, EBELN, EBELP\`

**EINA (Purchase info record - main data)**
* Chiavi Primarie: \`MANDT, INFNR\`
* Descrizione: Dati generali del record info acquisti (collegamento materiale-fornitore).
* Possibili Join:
  * **EINE** (Purchase info record - organizational data): su \`MANDT, INFNR\`
  * **MARA** (Material Master): su \`MANDT, MATNR\`
  * **LFA1** (Vendor Master): su \`MANDT, LIFNR\`

**EINE (Purchase info record - organizational data)**
* Chiavi Primarie: \`MANDT, INFNR, EKORG, WERKS, ESOKZ\`
* Descrizione: Dati del record info specifici per organizzazione d'acquisto e divisione.
* Possibili Join:
  * **EINA** (Purchase info record - main data): su \`MANDT, INFNR\`
  * **T024E** (Purchasing Organizations): su \`MANDT, EKORG\`
  * **T001W** (Plants/Branches): su \`MANDT, WERKS\`

**EORD (Source list)**
* Chiavi Primarie: \`MANDT, MATNR, WERKS, ZEORD\`
* Descrizione: Lista fonti di approvvigionamento per un materiale in una divisione.
* Possibili Join:
  * **MARA** (Material Master): su \`MANDT, MATNR\`
  * **MARC** (Plant Data for Material): su \`MANDT, MATNR, WERKS\`
  * **LFA1** (Vendor Master): su \`MANDT, LIFNR\`
  * **EKKO** (per contratti): su \`MANDT, EBELN\`

**EBAN (Purchase requisition)**
* Chiavi Primarie: \`MANDT, BANFN, BNFPO\`
* Descrizione: Posizioni delle Richieste di Acquisto (RdA).
* Possibili Join:
  * **EBKN** (Purchase Requisition Account Assignment): su \`MANDT, BANFN, BNFPO\`
  * **EKPO** (Purchase Document Item): su \`MANDT, BANFN, BNFPO\`
  * **MARA** (Material Master): su \`MANDT, MATNR\`

**EBKN (Purchase Requisition Account Assignment)**
* Chiavi Primarie: \`MANDT, BANFN, BNFPO, ZEBKN\`
* Descrizione: Dati di imputazione contabile per una posizione di RdA.
* Possibili Join:
  * **EBAN** (Purchase Requisition): su \`MANDT, BANFN, BNFPO\`

#### Material Master
**MARA (Material Master - General Data)**
* Chiavi Primarie: \`MANDT, MATNR\`
* Descrizione: Dati generali del materiale, validi per tutto il mandante.
* Possibili Join:
  * **MAKT** (Material Descriptions): su \`MANDT, MATNR, SPRAS\`
  * **MARC** (Plant Data for Material): su \`MANDT, MATNR\`
  * **MVKE** (Sales Data for Material): su \`MANDT, MATNR\`
  * **MARM** (Units of Measure for Material): su \`MANDT, MATNR\`
  * **MBEW** (Material Valuation): su \`MANDT, MATNR\`

**MAKT (Material Descriptions)**
* Chiavi Primarie: \`MANDT, MATNR, SPRAS\`
* Descrizione: Testi descrittivi del materiale in diverse lingue.
* Possibili Join:
  * **MARA** (Material Master): su \`MANDT, MATNR\`

**MARC (Plant Data for Material)**
* Chiavi Primarie: \`MANDT, MATNR, WERKS\`
* Descrizione: Dati del materiale specifici per una divisione (impianto).
* Possibili Join:
  * **MARA** (Material Master): su \`MANDT, MATNR\`
  * **MARD** (Storage Location Data for Material): su \`MANDT, MATNR, WERKS\`
  * **T001W** (Plants/Branches): su \`MANDT, WERKS\`

**MARD (Storage Location Data for Material)**
* Chiavi Primarie: \`MANDT, MATNR, WERKS, LGORT\`
* Descrizione: Dati di stock del materiale per magazzino.
* Possibili Join:
  * **MARC** (Plant Data for Material): su \`MANDT, MATNR, WERKS\`
  * **T001L** (Storage Locations): su \`MANDT, WERKS, LGORT\`

**MBEW (Material Valuation)**
* Chiavi Primarie: \`MANDT, MATNR, BWKEY, BWTAR\`
* Descrizione: Dati di valorizzazione del materiale.
* Possibili Join:
  * **MARA** (Material Master): su \`MANDT, MATNR\`
  * **MARC** (Plant Data for Material): su \`MANDT, MATNR, BWKEY=WERKS\` (se area di valorizzazione è la divisione)
  * **T001K** (Valuation Areas): su \`MANDT, BWKEY\`
`;
