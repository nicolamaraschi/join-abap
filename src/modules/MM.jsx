export const mmData = `
### Modulo MM (Materials Management)

#### Material Master Data

**MARA (General Material Data)**

* Chiavi Primarie: \`MANDT(MANDT), MATNR(MATNR)\`
* Descrizione: Dati generali del materiale (tipo materiale, gruppo merce, etc.)
* Possibili Join:

  * **MAKT** (Material Descriptions): su \`MATNR, SPRAS\`
  * **MARM** (Units of Measure for Material): su \`MATNR\`
  * **MBEW** (Material Valuation): su \`MATNR\`
  * **MARD** (Storage Location Data for Material): su \`MATNR\`
  * **MARC** (Material per Plant): su \`MATNR\`
  * **MVKE** (Sales Data): su \`MATNR\`
  * **MLGN** (Warehouse Data): su \`MATNR\`
  * **MLAN** (Tax Data): su \`MATNR\`

---

**MAKT (Material Descriptions)**

* Chiavi Primarie: \`MANDT(MANDT), MATNR(MATNR), SPRAS(SPRAS)\`
* Descrizione: Descrizioni del materiale in diverse lingue.
* Possibili Join:

  * **MARA** (General Material Data): su \`MATNR\`
  * **KNMT** (Customer-Material Info Record): su \`MATNR\` (e \`SPRAS = SY-LANGU\`)

---

**KNMT (Customer-Material Info Record Data Table)**
* Chiavi Primarie: \`MANDT(MANDT), VKORG(VKORG), VTWEG(VTWEG), KUNNR(KUNNR_V), MATNR(MATNR)\`
* Descrizione: Tabella dati Info Record Cliente-Materiale.
* Possibili Join:
  * **MAKT** (Material Descriptions): su \`MATNR\`

---

**MARC (Material Master per Plant)**

* Chiavi Primarie: \`MANDT(MANDT), MATNR(MATNR), WERKS(WERKS_D)\`
* Descrizione: Dati del materiale specifici per impianto (MRP, pianificazione, produzione, etc.)
* Possibili Join:

  * **MARA** (General Material Data): su \`MATNR\`
  * **MAPR** (Link MARC <=> PROP): su \`MATNR, WERKS\`
  * **MDKP** (MRP Document Header): su \`MATNR, WERKS\`

---

**MVKE (Material Sales Data)**

* Chiavi Primarie: \`MANDT(MANDT), MATNR(MATNR), VKORG(VKORG), VTWEG(VTWEG)\`
* Descrizione: Dati di vendita per il materiale (organizzazione di vendita, canale di distribuzione).
* Possibili Join:

  * **MARA** (General Material Data): su \`MATNR\`

---

**MARD (Storage Location Data for Material)**

* Chiavi Primarie: \`MANDT(MANDT), MATNR(MATNR), WERKS(WERKS_D), LGORT(LGORT_D)\`
* Descrizione: Quantità a stock per magazzino.
* Possibili Join:

  * **MARA** (General Material Data): su \`MATNR\`
  * **MARC** (Material per Plant): su \`MATNR, WERKS\`

---

**MLGN (Material / Warehouse Number)**

* Chiavi Primarie: \`MANDT(MANDT), MATNR(MATNR), LGNUM(LGNUM)\`
* Descrizione: Assegnazione materiale a numero magazzino (WM).
* Possibili Join:

  * **MARA** (General Material Data): su \`MATNR\`

---

**MLGT (Material / Storage Type)**

* Chiavi Primarie: \`MANDT(MANDT), MATNR(MATNR), LGNUM(LGNUM), LGTYP(LGTYP)\`
* Descrizione: Dettagli del materiale per tipo di stoccaggio (WM).
* Possibili Join:

  * **MLGN** (Warehouse Number): su \`MATNR, LGNUM\`

---

**MSKA (Sales Order Stock)**

* Chiavi Primarie: \`MANDT(MANDT), MATNR(MATNR), WERKS(WERKS_D), LGORT(LGORT_D), CHARG(CHARG_D), SOBKZ(SOBKZ), VBELN(VBELN), POSNR(POSNR)\`
* Descrizione: Stock speciale relativo ad ordini cliente.
* Possibili Join:

  * **MARA** (General Material Data): su \`MATNR\`

---

**MSPR (Project Stock)**

* Chiavi Primarie: \`MANDT(MANDT), MATNR(MATNR), WERKS(WERKS_D), LGORT(LGORT_D), CHARG(CHARG_D), SOBKZ(SOBKZ), PSPNR(PS_PSP_PNR)\`
* Descrizione: Stock speciale per progetti.
* Possibili Join:

  * **MARA** (General Material Data): su \`MATNR\`

---

**MEAN (International Article Number)**

* Chiavi Primarie: \`MANDT(MANDT), MATNR(MATNR), MEINH(MEINH), LFNUM(LFNUM)\`
* Descrizione: Numeri articolo internazionali (EAN/GTIN).
* Possibili Join:

  * **MARA** (General Material Data): su \`MATNR\`

---

**PGMI (Planning Material)**

* Chiavi Primarie: \`MATNR\`
* Descrizione: Materiale utilizzato nella pianificazione abbinata.
* Possibili Join:

  * **MARA** (General Material Data): su \`MATNR\`

---

**PROP (Forecast Parameters)**

* Chiavi Primarie: \`MATNR, WERKS\`
* Descrizione: Parametri di previsione per il materiale.
* Possibili Join:

  * **MAPR** (Link con MARC): su \`MATNR, WERKS\`

---

**MAPR (Link MARC <=> PROP)**

* Chiavi Primarie: \`MATNR, WERKS\`
* Descrizione: Collegamento tra i dati impianto-materiale e i parametri di previsione.
* Possibili Join:

  * **MARC** (Material per Plant): su \`MATNR, WERKS\`
  * **PROP** (Forecast Parameters): su \`MATNR, WERKS\`

**MBEW (Material Valuation)**

* Chiavi Primarie: \`MANDT(MANDT), MATNR(MATNR), BWKEY(BWKEY), BWTAR(BWTAR_D)\`
* Descrizione: Dati contabili e di valorizzazione del materiale.
* Possibili Join:

  * **MARA** (General Material Data): su \`MATNR\`

---

**MVER (Material Consumption)**

* Chiavi Primarie: \`MANDT(MANDT), MATNR(MATNR), WERKS(WERKS_D), GJAHR(GJAHR), PERKZ(PERKZ), ZAHLR(DZAHLR)\`
* Descrizione: Consumi storici del materiale.
* Possibili Join:

  * **MARA** (General Material Data): su \`MATNR\`

---


**MLAN (Tax Data Material Master)**

* Chiavi Primarie: \`MANDT(MANDT), MATNR(MATNR), ALAND(ALAND)\`
* Descrizione: Dati fiscali del materiale per paese.
* Possibili Join:

  * **MARA** (General Material Data): su \`MATNR\`

---

**MTQSS (Material master view: QM)**

* Chiavi Primarie: \`MATNR, WERKS\`
* Descrizione: Dati qualità per materiale/impianto.
* Possibili Join:

  * **MARC** (Material per Plant): su \`MATNR, WERKS\`

---

**MATERIALID (External Material Number with Version)**

* Chiavi Primarie: \`MATNR, MATNR_EXTERNAL, MATNR_VERSION\`
* Descrizione: Numerazione esterna e versionamento per il materiale.
* Possibili Join:

  * **MARA** (General Material Data): su \`MATNR\`

---

**MDTB (MRP Table)**

* Chiavi Primarie: \`MANDT(MANDT), DTNUM(DTNUM), DTPOS(DTPOS)\`
* Descrizione: Tabelle con i risultati della pianificazione fabbisogni.
* Possibili Join:

  * **MDKP** (Header per MRP): su \`DTNUM\`

---

**MDKP (MRP Document Header)**

* Chiavi Primarie: \`MANDT(MANDT), DTART(DTART), MATNR(MATNR), PLWRK(WERKS_D), PLSCN(PLSCN)\`
* Descrizione: Header delle run MRP.
* Possibili Join:

  * **MDTB** (MRP Table): tramite logica di pianificazione

---



#### Purchasing

**EKKO (Purchasing Document Header)**

* Chiavi Primarie: \`EBELN(EBELN)\`
* Descrizione: Testata del documento d'acquisto (informazioni generali sull'ordine).
* Possibili Join:

  * **EKPO** (Purchasing Document Item): su \`EBELN\`
  * **EKPA** (Partner functions): su \`EBELN\`
  * **EKAN** (Vendor address purchasing): su \`EBELN\`

---

**EKPO (Purchasing Document Item)**

* Chiavi Primarie: \` EBELN(EBELN), EBELP(EBELP)\`
* Descrizione: Dettagli di ogni posizione nell'ordine d'acquisto.
* Possibili Join:

  * **EKKO** (Purchasing Document Header): su \`EBELN\`
  * **EKET** (Scheduling Agreements): su \`EBELN, EBELP\`
  * **EINA** (Purchasing Info Record - General Data): su \`INFNR\`
  * **EINE** (Purchasing Info Record - Purchasing Organization Data): su \`INFNR, EKORG, WERKS, ESOKZ\`
  * **EORD** (Source list): su \`MATNR, WERKS, LIFNR\`
  * **EBAN** (Purchase Requisition): su \`BANFN, BNFPO\`
  * **EKPV** (Shipping-Specific Data on Stock Tfr. for Purch. Doc. Item): su \`EBELN, EBELP\`
  * **EKKN** (Account assignment in purchasing): su \`EBELN, EBELP\`
  * **EKES** (Order Acceptance/Fulfillment Confirmations): su \`EBELN, EBELP\`
  * **EIPO** (Item export / import data): su \`EBELN, EBELP\`
  * **VETVG** (Delivery Due Index for Stock Transfer): su \`EBELN, EBELP\`

---

**EKBE (History of Purchasing Document)**
* Chiavi Primarie: \`MANDT(MANDT), EBELN(EBELN), EBELP(EBELP), ZEKKN(DZEKKN), VGABE(VGABE), GJAHR(MJAHR), BELNR(MBLNR), BUZEI(MBLPO)\`
* Descrizione: Storico delle operazioni su una posizione di un documento d'acquisto (es. entrate merci, entrate fattura).
* Possibili Join:
  * **MSEG** (material document (item level)): su \`MBLNR=BELNR, MJAHR=GJAHR, ZEILE=BUZEI\`
  * **EKPO** (Purchasing Document Item): su \`EBELN, EBELP\`

---

**EKET (Scheduling Agreements)**

* Chiavi Primarie: \`MANDT(MANDT), EBELN(EBELN), EBELP(EBELP), ETENR(EETEN)\`
* Descrizione: Schedulazioni di consegna per un accordo o un ordine.
* Possibili Join:

  * **EKPO** (Purchasing Document Item): su \`EBELN, EBELP\`

---

**EKPV (Shipping-Specific Data on Stock Tfr. for Purch. Doc. Item)**

* Chiavi Primarie: \`EBELN, EBELP\`
* Descrizione: Dati specifici di spedizione per il trasferimento di stock relativi a una posizione del documento d'acquisto.
* Possibili Join:

  * **EKPO** (Purchasing Document Item): su \`EBELN, EBELP\`

---

**VETVG (Delivery Due Index for Stock Transfer)**

* Chiavi Primarie: \`VSTEL, LEDAT, LRRIO, ROUTE, SPDNR, WADAT, KUNWE, VBELN\`
* Descrizione: Indice per le scadenze di consegna relative ai trasferimenti di stock.
* Possibili Join:

  * **EKPO** (Purchasing Document Item): su \`EBELN, EBELP\`

---

**EKES (Order Acceptance/Fulfillment Confirmations)**

* Chiavi Primarie: \`MANDT(MANDT), EBELN(EBELN), EBELP(EBELP), ETENS(ETENS)\`
* Descrizione: Conferme di accettazione o evasione dell'ordine (es. conferme d'ordine, avvisi di spedizione).
* Possibili Join:

  * **EKPO** (Purchasing Document Item): su \`EBELN, EBELP\`

---

**EKKN (Account assignment in purchasing)**

* Chiavi Primarie: \`MANDT(MANDT), EBELN(EBELN), EBELP(EBELP), ZEKKN(DZEKKN)\`
* Descrizione: Assegnazioni contabili nei documenti d'acquisto (es. centro di costo, ordine).
* Possibili Join:

  * **EKPO** (Purchasing Document Item): su \`EBELN, EBELP\`
  * **EBKN** (Account Assignment in Purchase Requisition): su \`BANFN, BNFPO, ZEBEL\`

---

**EKAN (Vendor address purchasing)**

* Chiavi Primarie: \`EBELN\`
* Descrizione: Indirizzi del fornitore associati ai documenti d'acquisto.
* Possibili Join:

  * **EKKO** (Purchasing Document Header): su \`EBELN\`

---

**EKPA (Partner functions)**

* Chiavi Primarie: \`EBELN, EBELP, EKORG, LTSNR, WERKS, PARVW, PARZA\`
* Descrizione: Funzioni dei partner commerciali nel documento d'acquisto (es. fornitore, ricevitore merce).
* Possibili Join:

  * **EKKO** (Purchasing Document Header): su \`EBELN\`

---

**EIPO (Item export / import data)**

* Chiavi Primarie: \`EXNUM, EXPOS\`
* Descrizione: Dati di esportazione/importazione a livello di posizione del documento d'acquisto.
* Possibili Join:

  * **EKPO** (Purchasing Document Item): su \`EBELN, EBELP\`

---

**EINA (Purchasing Info Record - General Data)**

* Chiavi Primarie: \`MANDT(MANDT), INFNR(INFNR)\`
* Descrizione: Informazioni generali sulla relazione materiale-fornitore.
* Possibili Join:

  * **EINE** (Purchasing Info Record - Purchasing Organization Data): su \`INFNR\`
  * **EKPO** (Purchasing Document Item): su \`INFNR\`

---

**EINE (Purchasing Info Record - Purchasing Organization Data)**

* Chiavi Primarie: \`MANDT(MANDT), INFNR(INFNR), EKORG(EKORG), ESOKZ(ESOKZ), WERKS(EWERK)\`
* Descrizione: Dati del record info specifici per l'organizzazione d'acquisto.
* Possibili Join:

  * **EINA** (Purchasing Info Record - General Data): su \`INFNR\`

---

**EORD (Source list)**

 Chiavi Primarie: \`MANDT(MANDT), MATNR(MATNR), WERKS(EWERK), ZEORD(DZEORD)\`
* Descrizione: Elenco fonti, definisce i fornitori validi per un materiale in uno specifico impianto.
* Possibili Join:

  * **EKPO** (Purchasing Document Item): su \`MATNR, WERKS, LIFNR\`
  * **MARA** (General Material Data): su \`MATNR\`
  * **LFA1** (Vendor Master - General Section): su \`LIFNR\`

---

**EBAN (Purchase Requisition)**

* Chiavi Primarie: \`MANDT(MANDT), BANFN(BANFN), BNFPO(BNFPO)\`
* Descrizione: Richiesta interna di approvvigionamento di materiali o servizi.
* Possibili Join:

  * **EKPO** (Purchasing Document Item): su \`BANFN, BNFPO\`
  * **EBKN** (Account Assignment in Purchase Requisition): su \`BANFN, BNFPO\`

---

**EBKN (Account Assignment in Purchase Requisition)**

* Chiavi Primarie: \`MANDT(MANDT), BANFN(BANFN), BNFPO(BNFPO), ZEBKN(DZEBKN)\`
* Descrizione: Assegnazioni contabili a livello di posizione della richiesta d'acquisto.
* Possibili Join:

  * **EBAN** (Purchase Requisition): su \`BANFN, BNFPO\`
  * **EKKN** (Account assignment in purchasing): su \`BANFN, BNFPO, ZEBEL\`



#### Inventory Management (Key Change: MATDOC)
**MATDOC (Material Document)**
* Chiavi Primarie: \`MBLNR, MJAHR, ZEILE\`
* Descrizione: Tabella centrale per la gestione delle scorte in S/4HANA, sostituisce MKPF e MSEG. Contiene tutte le informazioni dei documenti materiale in un'unica tabella.
* Possibili Join:
  * **MKPF** (Header - per compatibilità): su \`MBLNR, MJAHR\`
  * **MSEG** (Item - per compatibilità): su \`MBLNR, MJAHR, ZEILE\`

**MKPF (Material Document Header)**
* Chiavi Primarie: \`MBLNR, MJAHR\`
* Descrizione: Testata del documento materiale. Esiste per retrocompatibilità.
* Possibili Join:
  * **MATDOC** (Material Document): su \`MBLNR, MJAHR\`
  * **MSEG** (Item level): su \`MBLNR, MJAHR\`

**MSEG (material document (item level))**
* Chiavi Primarie: \`MANDT(MANDT), MBLNR(MBLNR), MJAHR(MJAHR), ZEILE(MBLPO)\`
* Descrizione: Posizione del documento materiale. Esiste per retrocompatibilità.
* Possibili Join:
  * **MATDOC** (Material Document): su \`MBLNR, MJAHR, ZEILE\`
  * **MKPF** (Header level): su \`MBLNR, MJAHR\`
  * **EKBE** (History of Purchasing Document): su \`BELNR=MBLNR, GJAHR=MJAHR, BUZEI=ZEILE\`

#### Valuation
**MBEW (Material Valuation)**
* Chiavi Primarie: \`MATNR, BWKEY, BWTAR\`
* Descrizione: Dati contabili e di valorizzazione (prezzo standard, prezzo medio mobile, etc.).
* Possibili Join:
  * **CKMI** (Price History): su \`KALNR\` (collegato tramite logica)

**CKMI (Price History)**
* Chiavi Primarie: \`KALNR, BDATJ, BUPER\`
* Descrizione: Storico delle modifiche di prezzo per un materiale.
* Possibili Join:
  * **MBEW** (Material Valuation): su \`KALNR\` (collegato tramite logica)

#### Material Requirements Planning (MRP)
**MDKP (MRP Header Data)**
* Chiavi Primarie: \`DTNUM\`
* Descrizione: Informazioni generali sulla sessione di esecuzione del MRP.
* Possibili Join:
  * **MDTB** (Planning Table): su \`DTNUM\`

**MDTB (Planning Table)**
* Chiavi Primarie: \`DTNUM, DTPOS\`
* Descrizione: Tabella con i risultati del calcolo dei fabbisogni del MRP.
* Possibili Join:
  * **MDKP** (MRP Header Data): su \`DTNUM\`

#### General and Organizational Data
**T001W (Plants)**
* Chiavi Primarie: \`WERKS\`
* Descrizione: Anagrafica delle Divisioni (Impianti).
* Possibili Join:
  * **T001L** (Storage Locations): su \`WERKS\`

**T024E (Purchasing Organizations)**
* Chiavi Primarie: \`EKORG\`
* Descrizione: Anagrafica delle Organizzazioni d'Acquisto.
* Possibili Join:
  * Nessun join all'interno di questa sottocategoria.

**T001L (Storage Locations)**
* Chiavi Primarie: \`WERKS, LGORT\`
* Descrizione: Anagrafica dei Magazzini.
* Possibili Join:
  * **T001W** (Plants): su \`WERKS\`

#### Pricing Conditions (Important Change: PRCD_ELEMENTS)
**A017 (Material Info Record (Plant-Specific))**
* Chiavi Primarie: \`KAPPL, KSCHL, LIFNR, MATNR, EKORG, WERKS, ESOKZ\`
* Descrizione: Condizioni per record info materiale (specifico per divisione).
* Possibili Join:
  * **KONH** (Conditions Header): su \`KNUMH\` (il numero condizione è nella parte variabile della chiave)

---

**KONH (Conditions (Header))**
* Chiavi Primarie: \`KNUMH\`
* Descrizione: Testata dei record di condizione.
* Possibili Join:
  * **KONP** (Conditions (Item)): su \`KNUMH\`
  * **A017** (e altre tabelle Axxx): su \`KNUMH\`

---

**PRCD_ELEMENTS (Pricing Elements)**
* Chiavi Primarie: \`KNUMV, KPOSN, STUNR, ZAEHK\`
* Descrizione: Tabella centrale per le condizioni di prezzo in S/4HANA. Migliora performance e flessibilità.
* Possibili Join:
  * **KONV** (Procedure): su \`KNUMV, KPOSN, ...\`
  * **KONP** (Conditions items): su \`KNUMH_SRV\` (o campi analoghi)

**KONV (Procedure (billing doc or sales order))**
* Chiavi Primarie: \`KNUMV, KPOSN, STUNR, ZAEHK\`
* Descrizione: Esiste per compatibilità, ma PRCD_ELEMENTS è centrale in S/4HANA.
* Possibili Join:
  * **KONP** (Conditions items): su \`KNUMH\`
  * **PRCD_ELEMENTS** (Pricing Elements): su \`KNUMV, KPOSN, ...\`

**KONP (Conditions items)**
* Chiavi Primarie: \`KNUMH, KOPOS\`
* Descrizione: Posizioni dei record di condizione di prezzo (anagrafica).
* Possibili Join:
  * **KONV** (Procedure): su \`KNUMH\`
  * **PRCD_ELEMENTS** (Pricing Elements): su \`KNUMH_SRV\` (o campi analoghi)
  * **KONH** (Conditions (Header)): su \`KNUMH\`
`;