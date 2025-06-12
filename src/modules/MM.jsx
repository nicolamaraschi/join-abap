export const mmData = `
### Modulo MM (Materials Management)

#### Material Master Data
**MARA (General Material Data)**
* Chiavi Primarie: \`MATNR\`
* Descrizione: Dati generali del materiale (tipo materiale, gruppo merce, etc.)
* Possibili Join:
  * **MAKT** (Material Descriptions): su \`MATNR, SPRAS\`
  * **MARM** (Units of Measure for Material): su \`MATNR\`
  * **MBEW** (Material Valuation): su \`MATNR\`
  * **MARD** (Storage Location Data for Material): su \`MATNR\`

**MAKT (Material Descriptions)**
* Chiavi Primarie: \`MATNR, SPRAS\`
* Descrizione: Descrizioni del materiale in diverse lingue.
* Possibili Join:
  * **MARA** (General Material Data): su \`MATNR\`

**MARM (Units of Measure for Material)**
* Chiavi Primarie: \`MATNR, MEINH\`
* Descrizione: Unità di misura alternative per il materiale (quantità, dimensioni, etc.).
* Possibili Join:
  * **MARA** (General Material Data): su \`MATNR\`

**MBEW (Material Valuation)**
* Chiavi Primarie: \`MATNR, BWKEY, BWTAR\`
* Descrizione: Dati contabili e di valorizzazione del materiale.
* Possibili Join:
  * **MARA** (General Material Data): su \`MATNR\`

**MARD (Storage Location Data for Material)**
* Chiavi Primarie: \`MATNR, WERKS, LGORT\`
* Descrizione: Quantità a stock per magazzino.
* Possibili Join:
  * **MARA** (General Material Data): su \`MATNR\`

#### Purchasing

**EKKO (Purchasing Document Header)**

* Chiavi Primarie: \`EBELN\`
* Descrizione: Testata del documento d'acquisto (informazioni generali sull'ordine).
* Possibili Join:

  * **EKPO** (Purchasing Document Item): su \`EBELN\`

**EKPO (Purchasing Document Item)**

* Chiavi Primarie: \`EBELN, EBELP\`
* Descrizione: Dettagli di ogni posizione nell'ordine d'acquisto.
* Possibili Join:

  * **EKKO** (Purchasing Document Header): su \`EBELN\`
  * **EKET** (Scheduling Agreements): su \`EBELN, EBELP\`
  * **EINA** (Purchasing Info Record - General Data): su \`INFNR\`
  * **EBAN** (Purchase Requisition): su \`BANFN, BNFPO\`
  * **EKPV** (Shipping-Specific Data on Stock Tfr. for Purch. Doc. Item): su \`EBELN, EBELP\`
  * **EKKN** (Account assignment in purchasing): su \`EBELN, EBELP\`
  * **EKES** (Order Acceptance/Fulfillment Confirmations): su \`EBELN, EBELP\`
  * **EIPO** (Item export / import data): su \`EBELN, EBELP\`

**EKET (Scheduling Agreements)**

* Chiavi Primarie: \`EBELN, EBELP, ETENR\`
* Descrizione: Schedulazioni di consegna per un accordo o un ordine.
* Possibili Join:

  * **EKPO** (Purchasing Document Item): su \`EBELN, EBELP\`

**EKPV (Shipping-Specific Data on Stock Tfr. for Purch. Doc. Item)**

* Chiavi Primarie: \`EBELN, EBELP\`
* Descrizione: Dati specifici di spedizione per il trasferimento di stock relativi a una posizione del documento d'acquisto.
* Possibili Join:

  * **EKPO** (Purchasing Document Item): su \`EBELN, EBELP\`

**VETVG (Delivery Due Index for Stock Transfer)**

* Chiavi Primarie: \`EBELN, EBELP\`
* Descrizione: Indice per le scadenze di consegna relative ai trasferimenti di stock.
* Possibili Join:

  * **EKPO** (Purchasing Document Item): su \`EBELN, EBELP\`

**EKES (Order Acceptance/Fulfillment Confirmations)**

* Chiavi Primarie: \`EBELN, EBELP, ETENR, ZAEHL\`
* Descrizione: Conferme di accettazione o evasione dell'ordine (es. conferme d'ordine, avvisi di spedizione).
* Possibili Join:

  * **EKPO** (Purchasing Document Item): su \`EBELN, EBELP\`

**EKKN (Account assignment in purchasing)**

* Chiavi Primarie: \`EBELN, EBELP, ZEBEL\`
* Descrizione: Assegnazioni contabili nei documenti d'acquisto (es. centro di costo, ordine).
* Possibili Join:

  * **EKKO** (Purchasing Document Header): su \`EBELN\`
  * **EKPO** (Purchasing Document Item): su \`EBELN, EBELP\`
  * **EBKN** (Account Assignment in Purchase Requisition): su \`BANFN, BNFPO, ZEBEL\`

**EKAN (Vendor address purchasing)**

* Chiavi Primarie: \`EBELN, ADDR\_NO\`
* Descrizione: Indirizzi del fornitore associati ai documenti d'acquisto.
* Possibili Join:

  * **EKKO** (Purchasing Document Header): su \`EBELN\`

**EKPA (Partner functions)**

* Chiavi Primarie: \`EBELN, PARVW, PARZA\`
* Descrizione: Funzioni dei partner commerciali nel documento d'acquisto (es. fornitore, ricevitore merce).
* Possibili Join:

  * **EKKO** (Purchasing Document Header): su \`EBELN\`

**EIPO (Item export / import data)**

* Chiavi Primarie: \`EBELN, EBELP\`
* Descrizione: Dati di esportazione/importazione a livello di posizione del documento d'acquisto.
* Possibili Join:

  * **EKPO** (Purchasing Document Item): su \`EBELN, EBELP\`

**EINA (Purchasing Info Record - General Data)**

* Chiavi Primarie: \`INFNR\`
* Descrizione: Informazioni generali sulla relazione materiale-fornitore.
* Possibili Join:

  * **EINE** (Purchasing Info Record - Purchasing Organization Data): su \`INFNR\`
  * **EKPO** (Purchasing Document Item): su \`INFNR\`

**EINE (Purchasing Info Record - Purchasing Organization Data)**

* Chiavi Primarie: \`INFNR, EKORG, WERKS, ESOKZ\`
* Descrizione: Dati del record info specifici per l'organizzazione d'acquisto.
* Possibili Join:

  * **EINA** (Purchasing Info Record - General Data): su \`INFNR\`

**EORD (Source list)**

* Chiavi Primarie: \`MATNR, WERKS, LIFNR, GUEBG, GUEEN\`
* Descrizione: Elenco fonti, definisce i fornitori validi per un materiale in uno specifico impianto.
* Possibili Join:

  * **MARA** (General Material Data): su \`MATNR\`
  * **LFA1** (Vendor Master - General Section): su \`LIFNR\`

**EBAN (Purchase Requisition)**

* Chiavi Primarie: \`BANFN, BNFPO\`
* Descrizione: Richiesta interna di approvvigionamento di materiali o servizi.
* Possibili Join:

  * **EKPO** (Purchasing Document Item): su \`BANFN, BNFPO\`
  * **EBKN** (Account Assignment in Purchase Requisition): su \`BANFN, BNFPO\`

**EBKN (Account Assignment in Purchase Requisition)**

* Chiavi Primarie: \`BANFN, BNFPO, ZEBEL\`
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
* Chiavi Primarie: \`MBLNR, MJAHR, ZEILE\`
* Descrizione: Posizione del documento materiale. Esiste per retrocompatibilità.
* Possibili Join:
  * **MATDOC** (Material Document): su \`MBLNR, MJAHR, ZEILE\`
  * **MKPF** (Header level): su \`MBLNR, MJAHR\`

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
`;