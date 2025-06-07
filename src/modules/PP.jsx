// FILE: src/modules/PP.jsx
// Contiene i dati per il modulo Production Planning (PP)

export const ppData = `
### Modulo PP (Production Planning)

#### Work center
**CRHH (Work Center Hierarchy)**
* Chiavi Primarie: \`MANDT, HIERA_ID\`
* Descrizione: Contiene la definizione delle gerarchie dei posti di lavoro.
* Possibili Join:
  * **CRHS** (Hierarchy Structure): su \`MANDT, HIERA_ID\`

**CRHS (Hierarchy Structure)**
* Chiavi Primarie: \`MANDT, OBJID_O, OBJID_U\`
* Descrizione: Definisce la struttura gerarchica, collegando i posti di lavoro (o altre gerarchie) tra loro.
* Possibili Join:
  * **CRHH** (Work Center Hierarchy): su \`MANDT, HIERA_ID\` (per trovare la gerarchia di appartenenza)
  * **CRHD** (Work Center Header): su \`MANDT, OBJID_O=OBJID\` (per il posto di lavoro superiore) o \`MANDT, OBJID_U=OBJID\` (per il posto di lavoro subordinato)

**CRHD (Work Center Header)**
* Chiavi Primarie: \`MANDT, OBJTY, OBJID\`
* Descrizione: Testata del posto di lavoro, contenente i dati principali.
* Possibili Join:
  * **CRTX** (Text for the Work Center): su \`MANDT, OBJTY, OBJID, SPRAS\`
  * **CRCO** (Assignment of Work Center to Cost Center): su \`MANDT, OBJTY, OBJID\`
  * **CRCA** (Work Center Capacity Allocation): su \`MANDT, OBJTY, OBJID\`
  * **AFVC** (Order Operations): su \`MANDT, ARBID=OBJID\`
  * **PLPO** (Task List Operations): su \`MANDT, ARBID=OBJID\`

**CRTX (Text for the Work Center or Production Resource/Tool)**
* Chiavi Primarie: \`MANDT, SPRAS, OBJTY, OBJID\`
* Descrizione: Testi descrittivi per il posto di lavoro.
* Possibili Join:
  * **CRHD** (Work Center Header): su \`MANDT, OBJTY, OBJID\`

**CRCO (Assignment of Work Center to Cost Center)**
* Chiavi Primarie: \`MANDT, OBJTY, OBJID, LASET, ENDDA\`
* Descrizione: Collega il posto di lavoro al centro di costo per le imputazioni contabili.
* Possibili Join:
  * **CRHD** (Work Center Header): su \`MANDT, OBJTY, OBJID\`
  * **CSKS** (Cost Center Master Data): su \`MANDT, KOSTL, KOKRS\`

**KAKO (Capacity Header Segment)**
* Chiavi Primarie: \`MANDT, KAPID\`
* Descrizione: Dati di testata per una specifica capacità (es. capacità macchina, manodopera).
* Possibili Join:
  * **CRCA** (Work Center Capacity Allocation): su \`MANDT, KAPID\`

**CRCA (Work Center Capacity Allocation)**
* Chiavi Primarie: \`MANDT, OBJTY, OBJID, CANUM\`
* Descrizione: Assegna una o più capacità a un posto di lavoro.
* Possibili Join:
  * **CRHD** (Work Center Header): su \`MANDT, OBJTY, OBJID\`
  * **KAKO** (Capacity Header Segment): su \`MANDT, KAPID\`
  * **TC23** (Capacity categories): su \`MANDT, CAPID=CAPID\` (CAPID è il tipo di capacità)

**TC24 (Person responsible for the workcenter)**
* Chiavi Primarie: \`MANDT, VERAN, WERKS\`
* Descrizione: Tabella di customizing per definire le persone responsabili.
* Possibili Join:
  * **CRHD** (Work Center Header): su \`MANDT, VERAN\`

**CRC (Logical DB for Work centers)**
* Descrizione: Non è una tabella di database, ma una Logical Database (LDB). Le LDB sono programmi ABAP speciali che recuperano dati da più tabelle collegate gerarchicamente. 'CRC' è usata per report standard sui posti di lavoro.

#### Routings/operations
**MAPL (Allocation of task lists to materials)**
* Chiavi Primarie: \`MANDT, MATNR, WERKS, PLNTY, PLNNR, ZAEHL\`
* Descrizione: Collega i materiali ai loro cicli di lavoro specifici.
* Possibili Join:
  * **MARA** (Material Master): su \`MANDT, MATNR\`
  * **PLKO** (Task List - Header): su \`MANDT, PLNTY, PLNNR, PLNAL\` (PLNAL da MAPL)
  * **MARC** (Plant Data for Material): su \`MANDT, MATNR, WERKS\`

**PLAS (Task list - selection of operations/activities)**
* Chiavi Primarie: \`MANDT, PLNTY, PLNNR, PLNAL, PLNKN, ZAEHL\`
* Descrizione: Gestisce la selezione e la sequenza delle operazioni all'interno di un ciclo di lavoro.
* Possibili Join:
  * **PLKO** (Task List - Header): su \`MANDT, PLNTY, PLNNR, PLNAL\`
  * **PLPO** (Task List Operation): su \`MANDT, PLNTY, PLNNR, PLNKN\`

**PLFH (Task list - production resources/tools)**
* Chiavi Primarie: \`MANDT, PLNTY, PLNNR, PLNKN, ZAEHL_V, FHMNR\`
* Descrizione: Assegna i production resources/tools (PRT) alle operazioni del ciclo.
* Possibili Join:
  * **PLPO** (Task List Operation): su \`MANDT, PLNTY, PLNNR, PLNKN\`
  * **CRFH** (PRT Master Data): su \`MANDT, PRTNR=FHMNR\`

**PLFL (Task list - sequences)**
* Chiavi Primarie: \`MANDT, PLNTY, PLNNR, FLNKN, ZAEHL\`
* Descrizione: Definisce le sequenze (standard, alternative, parallele) delle operazioni.
* Possibili Join:
  * **PLKO** (Task List - Header): su \`MANDT, PLNTY, PLNNR\`

**PLKO (Task list - header)**
* Chiavi Primarie: \`MANDT, PLNTY, PLNNR, PLNAL\`
* Descrizione: Dati di testata per i cicli di lavoro (Routings).
* Possibili Join:
  * **PLAS** (Task list - selection of operations/activities): su \`MANDT, PLNTY, PLNNR, PLNAL\`
  * **PLPO** (Task List Operation): su \`MANDT, PLNTY, PLNNR, PLNAL\` (tramite PLAS)
  * **MAPL** (Allocation of task lists to materials): su \`MANDT, PLNTY, PLNNR, PLNAL\`
  * **PLKZ** (Task list: main header): su \`MANDT, PLNTY, PLNNR\`

**PLKZ (Task list: main header)**
* Chiavi Primarie: \`MANDT, PLNTY, PLNNR\`
* Descrizione: Dati di testata principali per un gruppo di cicli di lavoro.
* Possibili Join:
  * **PLKO** (Task List - Header): su \`MANDT, PLNTY, PLNNR\`

**PLPH (Phases / suboperations)**
* Chiavi Primarie: \`MANDT, PLNTY, PLNNR, PLNKN, ZAEHL, PHLKN\`
* Descrizione: Definisce le fasi o le sotto-operazioni per un'operazione del ciclo.
* Possibili Join:
  * **PLPO** (Task List Operation): su \`MANDT, PLNTY, PLNNR, PLNKN\`

**PLPO (Task list operation / activity)**
* Chiavi Primarie: \`MANDT, PLNTY, PLNNR, PLNKN\`
* Descrizione: Dettagli di una singola operazione all'interno di un ciclo di lavoro.
* Possibili Join:
  * **PLKO** (Task List - Header): su \`MANDT, PLNTY, PLNNR\` (tramite PLAS)
  * **PLAS** (Task list - selection): su \`MANDT, PLNTY, PLNNR, PLNKN\`
  * **CRHD** (Work Center Header): su \`MANDT, ARBID=OBJID\`
  * **PLMZ** (Allocation of BOM - items to operations): su \`MANDT, PLNTY, PLNNR, PLNKN\`
  * **PLFH** (Task list - PRT): su \`MANDT, PLNTY, PLNNR, PLNKN\`

**PLPR (Log collector for tasklists)**
* Chiavi Primarie: \`MANDT, LOG_GROUP\`
* Descrizione: Raccoglie i log di modifica per i cicli di lavoro.

**PLMZ (Allocation of BOM - items to operations)**
* Chiavi Primarie: \`MANDT, PLNTY, PLNNR, PLNKN, STLTY, STLNR, STLKN\`
* Descrizione: Collega i componenti di una distinta base (BOM) a specifiche operazioni del ciclo.
* Possibili Join:
  * **PLPO** (Task List Operation): su \`MANDT, PLNTY, PLNNR, PLNKN\`
  * **STPO** (BOM Item): su \`MANDT, STLTY, STLNR, STLKN\`

#### Bill of material
**STKO (BOM - header)**
* Chiavi Primarie: \`MANDT, STLNR, STLAL, STLTY\`
* Descrizione: Dati di testata della distinta base (Bill of Material).
* Possibili Join:
  * **STPO** (BOM Item): su \`MANDT, STLNR, STLTY\` (STLAL è implicito dalla testata)
  * **MAST** (Material to BOM Link): su \`MANDT, STLNR, STLAL, STLTY\`
  * **STAS** (BOMs Item Selection): su \`MANDT, STLNR, STLAL, STLTY\`

**STPO (BOM – item)**
* Chiavi Primarie: \`MANDT, STLTY, STLNR, STLKN\`
* Descrizione: Posizioni (componenti) di una distinta base.
* Possibili Join:
  * **STKO** (BOM Header): su \`MANDT, STLTY, STLNR\` (e \`STLAL\` da STKO)
  * **STAS** (BOMs Item Selection): su \`MANDT, STLTY, STLNR, STLKN\`
  * **MARA** (Material Master): su \`MANDT, IDNRK=MATNR\`
  * **PLMZ** (Allocation of BOM to operations): su \`MANDT, STLTY, STLNR, STLKN\`

**STAS (BOMs Item Selection)**
* Chiavi Primarie: \`MANDT, STLTY, STLNR, STLAL, STLKN\`
* Descrizione: Selezione delle posizioni della BOM (es. per validità data o altri criteri).
* Possibili Join:
  * **STPO** (BOM Item): su \`MANDT, STLTY, STLNR, STLKN\`
  * **STKO** (BOM Header): su \`MANDT, STLTY, STLNR, STLAL\`

**STPN (BOMs - follow-up control)**
* Chiavi Primarie: \`MANDT, STLTY, STLNR, STLKN, DATUV\`
* Descrizione: Dati di controllo per le modifiche alle posizioni della BOM.
* Possibili Join:
  * **STPO** (BOM Item): su \`MANDT, STLTY, STLNR, STLKN\`

**STPU (BOM - sub-item)**
* Chiavi Primarie: \`MANDT, STLTY, STLNR, STLKN, SANKA, UPKNZ\`
* Descrizione: Sotto-posizioni di una distinta base (es. per indicare punti di installazione).
* Possibili Join:
  * **STPO** (BOM Item): su \`MANDT, STLTY, STLNR, STLKN\`

**STZU (Permanent BOM data)**
* Chiavi Primarie: \`MANDT, STLTY, STLNR\`
* Descrizione: Dati anagrafici permanenti/addizionali per una BOM.
* Possibili Join:
  * **STKO** (BOM Header): su \`MANDT, STLTY, STLNR\`

**MAST (Material to BOM link)**
* Chiavi Primarie: \`MANDT, MATNR, WERKS, STLAN\`
* Descrizione: Collega un materiale (in un certo impianto e per un certo utilizzo) alla sua distinta base.
* Possibili Join:
  * **MARA** (Material Master): su \`MANDT, MATNR\`
  * **STKO** (BOM Header): su \`MANDT, STLNR, STLAL, STLTY\`
  * **MARC** (Plant Data for Material): su \`MANDT, MATNR, WERKS\`

**KDST (Sales order to BOM link)**
* Chiavi Primarie: \`MANDT, SDKNR\`
* Descrizione: Collega un ordine di vendita a una distinta base specifica per l'ordine (Order BOM).
* Possibili Join:
  * **VBAK** (Sales Document Header): su \`MANDT, VBELN=VBELN\` (da KDST)
  * **STKO** (BOM Header): su \`MANDT, STLNR, STLTY\` (da KDST)

#### Production orders
**AUFK (Production order headers)**
* Chiavi Primarie: \`MANDT, AUFNR\`
* Descrizione: Testata generale per tutti i tipi di ordini, inclusi quelli di produzione.
* Possibili Join:
  * **AFKO** (Order header data PP orders): su \`MANDT, AUFNR\`
  * **AFIH** (Maintenance order header): su \`MANDT, AUFNR\`
  * **JEST** (Object status): su \`MANDT, OBJNR=OBJNR\`
  * **JSTO** (Status profile): su \`MANDT, STSMA=STSMA\` (STSMA da AUFK)
  * **ACDOCA** (Universal Journal): su \`MANDT=RCLNT, AUFNR=AUFNR\`

**AFIH (Maintenance order header)**
* Chiavi Primarie: \`MANDT, AUFNR\`
* Descrizione: Testata specifica per ordini di manutenzione (rilevante in PP per ordini di ricondizionamento).
* Possibili Join:
  * **AUFK** (Order Master Data): su \`MANDT, AUFNR\`
  * **ILOA** (PM Object Location): su \`MANDT, ILOAN\`

**AUFM (Goods movement for prod. order)**
* Chiavi Primarie: \`MANDT, MBLNR, MJAHR, ZEILE\`
* Descrizione: Tabella indice per i movimenti merci relativi a un ordine (Nota: obsoleto in S/4HANA, usare MATDOC).
* Possibili Join:
  * **AUFK** (Order Header): su \`MANDT, AUFNR\`
  * **MSEG** (Material Document Item): su \`MANDT, MBLNR, MJAHR, ZEILE\`
  * **MATDOC** (Universal Journal for Material Docs): su \`MANDT, MBLNR, MJAHR, ZEILE\`

**AFKO (Order header data PP orders)**
* Chiavi Primarie: \`MANDT, AUFNR\`
* Descrizione: Dati di testata specifici per l'ordine di produzione (quantità, date, DBM, ciclo).
* Possibili Join:
  * **AUFK** (Order Master Data): su \`MANDT, AUFNR\`
  * **AFPO** (Order Item): su \`MANDT, AUFNR\`
  * **AFVC** (Order Operations): su \`MANDT, AUFPL\`

**AFPO (Order item)**
* Chiavi Primarie: \`MANDT, AUFNR, POSNR\`
* Descrizione: Dati della posizione dell'ordine di produzione.
* Possibili Join:
  * **AFKO** (Order Header Data PP Orders): su \`MANDT, AUFNR\`
  * **MARA** (Material Master): su \`MANDT, MATNR\`
  * **RESB** (Order Components): su \`MANDT, AUFNR, POSNR=AUFPL_ORD\` (se la prenotazione è per posizione ordine)

**RESB (Order components)**
* Chiavi Primarie: \`MANDT, RSNUM, RSPOS\`
* Descrizione: Componenti materiali richiesti (impegni) per un ordine.
* Possibili Join:
  * **AFKO** (Order Header Data PP Orders): su \`MANDT, AUFNR\`
  * **MARA** (Material Master): su \`MANDT, MATNR\`
  * **AFPO** (Order Item): su \`MANDT, AUFNR, AUFPL_ORD=POSNR\`

**AFVC (Order operations)**
* Chiavi Primarie: \`MANDT, AUFPL, APLZL\`
* Descrizione: Operazioni dell'ordine di produzione, derivate dal ciclo di lavoro.
* Possibili Join:
  * **AFKO** (Order Header Data PP Orders): su \`MANDT, AUFPL\`
  * **AFVV** (Quantities/dates/values in the operation): su \`MANDT, AUFPL, APLZL\`
  * **AFRU** (Order Completion Confirmations): su \`MANDT, AUFPL, APLZL\`
  * **CRHD** (Work Center Header): su \`MANDT, ARBID=OBJID\`

**AFVV (Quantities/dates/values in the operation)**
* Chiavi Primarie: \`MANDT, AUFPL, APLZL\`
* Descrizione: Dettagli quantitativi, date e valori per un'operazione dell'ordine.
* Possibili Join:
  * **AFVC** (Order Operations): su \`MANDT, AUFPL, APLZL\`

**AFVU (User fields of the operation)**
* Chiavi Primarie: \`MANDT, AUFPL, APLZL\`
* Descrizione: Campi utente personalizzati per le operazioni dell'ordine.
* Possibili Join:
  * **AFVC** (Order Operations): su \`MANDT, AUFPL, APLZL\`

**AFFL (Work order sequence)**
* Chiavi Primarie: \`MANDT, AUFPL, FOLGE\`
* Descrizione: Sequenza delle operazioni all'interno dell'ordine.
* Possibili Join:
  * **AFVC** (Order Operations): su \`MANDT, AUFPL\`
  * **AFKO** (Order Header Data PP Orders): su \`MANDT, AUFPL\`

**AFFH (PRT assignment data for the work order)**
* Chiavi Primarie: \`MANDT, AUFPL, APLZL, AFH_ID\`
* Descrizione: Assegnazione dei Production Resources/Tools (PRT) alle operazioni dell'ordine.
* Possibili Join:
  * **AFVC** (Order Operations): su \`MANDT, AUFPL, APLZL\`

**JSTO (Status profile)**
* Chiavi Primarie: \`MANDT, STSMA\`
* Descrizione: Definizione dei profili di stato.
* Possibili Join:
  * **AUFK** (Order Header): su \`MANDT, STSMA\`
  * **JEST** (Object Status): su \`MANDT, STSMA=STSMA\` (tramite OBJNR)

**JEST (Object status)**
* Chiavi Primarie: \`MANDT, OBJNR, STAT, INACT\`
* Descrizione: Tabella centrale che memorizza gli stati (di sistema e utente) attivi per qualsiasi oggetto SAP.
* Possibili Join:
  * **AUFK** (Order Header): su \`MANDT, OBJNR\`
  * **PRPS** (WBS Element): su \`MANDT, OBJNR\`
  * **EQUI** (Equipment): su \`MANDT, OBJNR\`
  * **TJ02T** (System Status Texts): su \`MANDT, ISTAT=STAT, SPRAS\`

**AFRU (Order completion confirmations)**
* Chiavi Primarie: \`MANDT, RUECK, RMZHL\`
* Descrizione: Conferme di completamento (consuntivazioni) per le operazioni dell'ordine.
* Possibili Join:
  * **AFVC** (Order Operations): su \`MANDT, AUFPL, APLZL\`
  * **AFKO** (Order Header Data PP Orders): su \`MANDT, AUFNR\`
`;
