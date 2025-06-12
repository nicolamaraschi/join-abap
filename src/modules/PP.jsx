
export const ppData = `
### Modulo PP (Production Planning)

#### Work center
**CRHH (Work Center Hierarchy)**
* Chiavi Primarie: \`OBJTY, OBJTY\`
* Descrizione: Contiene la definizione delle gerarchie dei posti di lavoro.
* Possibili Join:
  * **CRHS** (Hierarchy Structure): su \`HIERA_ID\`

**CRHS (Hierarchy Structure)**
* Chiavi Primarie: \`OBJTY_HY, OBJID_HY, OBJTY_HO, OBJID_HO, OBJTY_UP, OBJID_UP\`
* Descrizione: Definisce la struttura gerarchica, collegando i posti di lavoro.
* Possibili Join:
  * **CRHH** (Work Center Hierarchy): su \`HIERA_ID\`
  * **CRHD** (Work Center Header): su \`OBJID_O=OBJID\` o \`OBJID_U=OBJID\`

**CRHD (Work Center Header)**
* Chiavi Primarie: \`OBJTY, OBJID\`
* Descrizione: Testata del posto di lavoro, contenente i dati principali.
* Possibili Join:
  * **CRTX** (Text for the Work Center): su \`OBJTY, OBJID, SPRAS\`
  * **CRCO** (Assignment of Work Center to Cost Center): su \`OBJTY, OBJID\`
  * **CRCA** (Work Center Capacity Allocation): su \`OBJTY, OBJID\`

**CRTX (Text for the Work Center or Production Resource/Tool)**
* Chiavi Primarie: \`SPRAS, OBJTY, OBJID\`
* Descrizione: Testi descrittivi per il posto di lavoro.
* Possibili Join:
  * **CRHD** (Work Center Header): su \`OBJTY, OBJID\`

**CRCO (Assignment of Work Center to Cost Center)**
* Chiavi Primarie: \`OBJTY, OBJID, LASET, ENDDA, LANUM\`
* Descrizione: Collega il posto di lavoro al centro di costo per le imputazioni contabili.
* Possibili Join:
  * **CRHD** (Work Center Header): su \`OBJTY, OBJID\`

**KAKO (Capacity Header Segment)**
* Chiavi Primarie: \`KAPID\`
* Descrizione: Dati di testata per una specifica capacità (es. capacità macchina, manodopera).
* Possibili Join:
  * **CRCA** (Work Center Capacity Allocation): su \`KAPID\`

**CRCA (Work Center Capacity Allocation)**
* Chiavi Primarie: \`OBJTY, OBJID, CANUM\`
* Descrizione: Assegna una o più capacità a un posto di lavoro.
* Possibili Join:
  * **CRHD** (Work Center Header): su \`OBJTY, OBJID\`
  * **KAKO** (Capacity Header Segment): su \`KAPID\`

**TC24 (Person responsible for the workcenter)**
* Chiavi Primarie: \`VERAN, WERKS\`
* Descrizione: Tabella di customizing per definire le persone responsabili.
* Possibili Join:
  * **CRHD** (Work Center Header): su \`VERAN\`

**CRC (Logical DB for Work centers)**
* Descrizione: Non è una tabella di database, ma una Logical Database (LDB) usata per i report standard.

#### Routings/operations
**MAPL (Allocation of task lists to materials)**
* Chiavi Primarie: \`MATNR, WERKS, PLNTY, PLNNR, ZAEHL, PLNAL, ZKRIZ\`
* Descrizione: Collega i materiali ai loro cicli di lavoro specifici.
* Possibili Join:
  * **PLKO** (Task List - Header): su \`PLNTY, PLNNR, PLNAL\` (PLNAL da MAPL)

**PLAS (Task list - selection of operations/activities)**
* Chiavi Primarie: \`PLNTY, PLNNR, PLNAL, PLNKN, ZAEHL, PLNFL\`
* Descrizione: Gestisce la selezione e la sequenza delle operazioni all'interno di un ciclo di lavoro.
* Possibili Join:
  * **PLKO** (Task List - Header): su \`PLNTY, PLNNR, PLNAL\`
  * **PLPO** (Task List Operation): su \`PLNTY, PLNNR, PLNKN\`

**PLFH (Task list - production resources/tools)**
* Chiavi Primarie: \`PLNTY, PLNNR, PZLFH, ZAEHL\`
* Descrizione: Assegna i production resources/tools (PRT) alle operazioni del ciclo.
* Possibili Join:
  * **PLPO** (Task List Operation): su \`PLNTY, PLNNR, PLNKN\`

**PLFL (Task list - sequences)**
* Chiavi Primarie: \`PLNTY, PLNNR, PLNAL, PLNFL, ZAEHL\`
* Descrizione: Definisce le sequenze (standard, alternative, parallele) delle operazioni.
* Possibili Join:
  * **PLKO** (Task list - header): su \`PLNTY, PLNNR\`

**PLKO (Task list - header)**
* Chiavi Primarie: \`PLNTY, PLNNR, PLNAL, ZAEHL\`
* Descrizione: Dati di testata per i cicli di lavoro (Routings).
* Possibili Join:
  * **PLAS** (Task list - selection of operations/activities): su \`PLNTY, PLNNR, PLNAL\`
  * **PLPO** (Task List Operation): su \`PLNTY, PLNNR, PLNAL\` (tramite PLAS)
  * **MAPL** (Allocation of task lists to materials): su \`PLNTY, PLNNR, PLNAL\`
  * **PLKZ** (Task list: main header): su \`PLNTY, PLNNR\`
  * **PLFL** (Task list - sequences): su \`PLNTY, PLNNR\`

**PLKZ (Task list: main header)**
* Chiavi Primarie: \`PLNTY, PLNNR, PLNAL\`
* Descrizione: Dati di testata principali per un gruppo di cicli di lavoro.
* Possibili Join:
  * **PLKO** (Task List - Header): su \`PLNTY, PLNNR\`

**PLPH (Phases / suboperations)**
* Chiavi Primarie: \`PLNTY, PLNNR, PLNKN, PLNPH, ZAEHL\`
* Descrizione: Definisce le fasi o le sotto-operazioni per un'operazione del ciclo.
* Possibili Join:
  * **PLPO** (Task List Operation): su \`PLNTY, PLNNR, PLNKN\`

**PLPO (Task list operation / activity)**
* Chiavi Primarie: \`PLNTY, PLNNR, PLNKN, ZAEHL\`
* Descrizione: Dettagli di una singola operazione all'interno di un ciclo di lavoro.
* Possibili Join:
  * **PLAS** (Task list - selection): su \`PLNTY, PLNNR, PLNKN\`
  * **PLMZ** (Allocation of BOM - items to operations): su \`PLNTY, PLNNR, PLNKN\`
  * **PLFH** (Task list - PRT): su \`PLNTY, PLNNR, PLNKN\`
  * **PLPH** (Phases / suboperations): su \`PLNTY, PLNNR, PLNKN\`

**PLPR (Log collector for tasklists)**
* Chiavi Primarie: \`DATUM, UZEIT, PLNNR\`
* Descrizione: Raccoglie i log di modifica per i cicli di lavoro.
* Possibili Join:
  * Nessun join all'interno di questa sottocategoria.

**PLMZ (Allocation of BOM - items to operations)**
* Chiavi Primarie: \`PLNTY, PLNNR, ZUONR, ZAEHL\`
* Descrizione: Collega i componenti di una distinta base (BOM) a specifiche operazioni del ciclo.
* Possibili Join:
  * **PLPO** (Task List Operation): su \`PLNTY, PLNNR, PLNKN\`

#### Bill of material
**STKO (BOM - header)**
* Chiavi Primarie: \`STLTY, STLNR, STLAL, STKOZ\`
* Descrizione: Dati di testata della distinta base (Bill of Material).
* Possibili Join:
  * **STPO** (BOM Item): su \`STLNR, STLTY\`
  * **MAST** (Material to BOM Link): su \`STLNR, STLAL, STLTY\`
  * **STAS** (BOMs Item Selection): su \`STLNR, STLAL, STLTY\`
  * **STZU** (Permanent BOM data): su \`STLTY, STLNR\`
  * **KDST** (Sales order to BOM link): su \`STLNR, STLTY\`

**STPO (BOM – item)**
* Chiavi Primarie: \`STLTY, STLNR, STLKN, STPOZ\`
* Descrizione: Posizioni (componenti) di una distinta base.
* Possibili Join:
  * **STKO** (BOM Header): su \`STLTY, STLNR\`
  * **STAS** (BOMs Item Selection): su \`STLTY, STLNR, STLKN\`
  * **STPN** (BOMs - follow-up control): su \`STLTY, STLNR, STLKN\`
  * **STPU** (BOM - sub-item): su \`STLTY, STLNR, STLKN\`

**STAS (BOMs Item Selection)**
* Chiavi Primarie: \`STLTY, STLNR, STLAL, STLKN, STASZ\`
* Descrizione: Selezione delle posizioni della BOM (es. per validità data o altri criteri).
* Possibili Join:
  * **STPO** (BOM Item): su \`STLTY, STLNR, STLKN\`
  * **STKO** (BOM Header): su \`STLTY, STLNR, STLAL\`

**STPN (BOMs - follow-up control)**
* Chiavi Primarie: \`MATNR, STLTY, STLNR, DPMAT\`
* Descrizione: Dati di controllo per le modifiche alle posizioni della BOM.
* Possibili Join:
  * **STPO** (BOM Item): su \`STLTY, STLNR, STLKN\`

**STPU (BOM - sub-item)**
* Chiavi Primarie: \`STLTY, STLNR, STLKN, STPOZ, UPOSZ\`
* Descrizione: Sotto-posizioni di una distinta base (es. per indicare punti di installazione).
* Possibili Join:
  * **STPO** (BOM Item): su \`STLTY, STLNR, STLKN\`

**STZU (Permanent BOM data)**
* Chiavi Primarie: \`STLTY, STLNR\`
* Descrizione: Dati anagrafici permanenti/addizionali per una BOM.
* Possibili Join:
  * **STKO** (BOM Header): su \`STLTY, STLNR\`

**PLMZ (Allocation of BOM - items to operations)**
* Chiavi Primarie: \`PLNTY, PLNNR, ZUONR, ZAEHL\`
* Descrizione: Collega i componenti di una distinta base (BOM) a specifiche operazioni del ciclo.
* Possibili Join:
  * **STPO** (BOM Item): su \`STLTY, STLNR, STLKN\`

**MAST (Material to BOM link)**
* Chiavi Primarie: \`MATNR, WERKS, STLAN, STLNR, STLAL\`
* Descrizione: Collega un materiale (in un certo impianto e per un certo utilizzo) alla sua distinta base.
* Possibili Join:
  * **STKO** (BOM Header): su \`STLNR, STLAL, STLTY\`

**KDST (Sales order to BOM link)**
* Chiavi Primarie: \`VBELN, VBPOS, MATNR, WERKS, STLAN, STLNR, STLAL\`
* Descrizione: Collega un ordine di vendita a una distinta base specifica per l'ordine (Order BOM).
* Possibili Join:
  * **STKO** (BOM Header): su \`STLNR, STLTY\`

#### Production orders
**AUFK (Production order headers)**
* Chiavi Primarie: \`AUFNR\`
* Descrizione: Testata generale per tutti i tipi di ordini, inclusi quelli di produzione.
* Possibili Join:
  * **AFKO** (Order header data PP orders): su \`AUFNR\`
  * **AFIH** (Maintenance order header): su \`AUFNR\`
  * **JEST** (Object status): su \`OBJNR\`
  * **JSTO** (Status profile): su \`OBJNR\`
  * **AUFM** (Goods movement for prod. order): su \`AUFNR\`

**AFIH (Maintenance order header)**
* Chiavi Primarie: \`AUFNR\`
* Descrizione: Testata specifica per ordini di manutenzione (rilevante in PP per ordini di ricondizionamento).
* Possibili Join:
  * **AUFK** (Order Master Data): su \`AUFNR\`

**AUFM (Goods movement for prod. order)**
* Chiavi Primarie: \`MBLNR, MJAHR, ZEILE\`
* Descrizione: Tabella indice per i movimenti merci relativi a un ordine (Nota: obsoleto in S/4HANA, usare MATDOC).
* Possibili Join:
  * **AUFK** (Order Header): su \`AUFNR\`

**AFKO (Order header data PP orders)**
* Chiavi Primarie: \`AUFNR\`
* Descrizione: Dati di testata specifici per l'ordine di produzione (quantità, date, DBM, ciclo).
* Possibili Join:
  * **AUFK** (Order Master Data): su \`AUFNR\`
  * **AFPO** (Order Item): su \`AUFNR\`
  * **AFVC** (Order Operations): su \`AUFPL\`
  * **AFFL** (Work order sequence): su \`AUFPL\`
  * **AFRU** (Order completion confirmations): su \`AUFNR\`
  * **DRAD_PORDER** (Documents linked to prod. order): su \`AUFNR\`

**AFPO (Order item)**
* Chiavi Primarie: \`AUFNR, POSNR\`
* Descrizione: Dati della posizione dell'ordine di produzione.
* Possibili Join:
  * **AFKO** (Order Header Data PP Orders): su \`AUFNR\`
  * **RESB** (Order Components): su \`AUFNR\`
  * **PRPS** (WBS Element): su \`PROJN = PSPNR\`

**AFRU (Order completion confirmations)**
* Chiavi Primarie: \`RUECK, RMZHL\`
* Descrizione: Conferme di completamento (consuntivazioni) per le operazioni dell'ordine.
* Possibili Join:
  * **AFKO** (Order Header Data PP Orders): su \`AUFNR\`
  * **AFVC** (Order operations): su \`AUFPL, APLZL, VORNR\`

**AFVC (Order operations)**
* Chiavi Primarie: \`AUFPL, APLZL\`
* Descrizione: Operazioni dell'ordine di produzione, derivate dal ciclo di lavoro.
* Possibili Join:
  * **AFKO** (Order Header Data PP Orders): su \`AUFPL\`
  * **AFVV** (Quantities/dates/values in the operation): su \`AUFPL, APLZL\`
  * **AFVU** (User fields of the operation): su \`AUFPL, APLZL\`
  * **AFFH** (PRT assignment data for the work order): su \`AUFPL\`
  * **AFRU** (Order completion confirmations): su \`AUFPL, APLZL,PLNFL, VORNR\`
  * **RESB** (Order components): su \`AUFPL, APLZL, PLNFL, VORNR\`
  * **CRHD** (Work Center Header): su \`ARBID = OBJID\`

**AFVV (Quantities/dates/values in the operation)**
* Chiavi Primarie: \`AUFPL, APLZL\`
* Descrizione: Dettagli quantitativi, date e valori per un'operazione dell'ordine.
* Possibili Join:
  * **AFVC** (Order Operations): su \`AUFPL, APLZL\`

**AFVU (User fields of the operation)**
* Chiavi Primarie: \`AUFPL, APLZL\`
* Descrizione: Campi utente personalizzati per le operazioni dell'ordine.
* Possibili Join:
  * **AFVC** (Order Operations): su \`AUFPL, APLZL\`

**AFFL (Work order sequence)**
* Chiavi Primarie: \`AUFPL, APLZL\`
* Descrizione: Sequenza delle operazioni all'interno dell'ordine.
* Possibili Join:
  * **AFKO** (Order Header Data PP Orders): su \`AUFPL\`

**AFFH (PRT assignment data for the work order)**
* Chiavi Primarie: \`AUFPL, PZLFH\`
* Descrizione: Assegnazione dei Production Resources/Tools (PRT) alle operazioni dell'ordine.
* Possibili Join:
  * **AFVC** (Order Operations): su \`AUFPL, APLZL\`
  * **CRVD_B** (Document link to PRT): su \`OBJID\`

**CRVD_B (Document link to PRT)**
* Chiavi Primarie: \`OBJID\`
* Descrizione: Collega un oggetto (come un PRT) a un documento.
* Possibili Join:
    * **AFFH** (PRT assignment): su \`OBJID\`
    * **DRAW** (Document info record): su \`DOKAR, DOKNR, DOKVR, DOKTL\`

**DRAD_PORDER (Documents linked to production order)**
* Chiavi Primarie: \`DOCAR, DOCNR, DOCVR, DOCTL, AUFNR\`
* Descrizione: Tabella di collegamento tra documenti DMS e ordini di produzione.
* Possibili Join:
    * **AFKO** (Order header data PP orders): su \`AUFNR\`
    * **DRAW** (Document info record): su \`DOCAR, DOCNR, DOCVR, DOCTL\`

**DRAW (Document info record)**
* Chiavi Primarie: \`DOCAR, DOCNR, DOCVR, DOCTL\`
* Descrizione: Anagrafica del documento nel sistema DMS.
* Possibili Join:
    * **DRAD_PORDER** (Documents linked to prod. order): su \`DOCAR, DOCNR, DOCVR, DOCTL\`
    * **CRVD_B** (Document link to PRT): su \`DOKAR, DOKNR, DOKVR, DOKTL\`

**JEST (Object status)**
* Chiavi Primarie: \`OBJNR, STAT\`
* Descrizione: Tabella centrale che memorizza gli stati attivi per qualsiasi oggetto SAP.
* Possibili Join:
  * **AUFK** (Order Header): su \`OBJNR\`
  * **TJ30** (User status codes): su \`STAT = ESTAT\` (e JSTO.STSMA per il profilo corretto)

**JSTO (Status profile)**
* Chiavi Primarie: \`OBJNR\`
* Descrizione: Informazioni sullo schema di stato per un oggetto.
* Possibili Join:
  * **AUFK** (Order Header): su \`OBJNR\`
  * **TJ30** (User status codes): su \`STSMA\`

**PROJ (Project definition)**
* Chiavi Primarie: \`PSPNR\`
* Descrizione: Dati anagrafici della definizione di progetto.
* Possibili Join:
    * **PRPS** (WBS Element): su \`PSPNR = PSPHI\`

**PRPS (WBS Element)**
* Chiavi Primarie: \`PSPNR\`
* Descrizione: Dati anagrafici dell'elemento WBS (Work Breakdown Structure).
* Possibili Join:
    * **AFPO** (Order Item): su \`PSPNR = PS_PSP_PNR\`
    * **PROJ** (Project definition): su \`PSPHI = PSPNR\`

**RESB (Order components)**
* Chiavi Primarie: \`RSNUM, RSPOS, RSART\`
* Descrizione: Componenti materiali richiesti (impegni) per un ordine.
* Possibili Join:
  * **AFPO** (Order Item): su \`AUFNR\`
  * **AFVC** (Order operations): su \`AUFPL, APLZL, VORNR\`

**TJ30 (User status codes)**
* Chiavi Primarie: \`STSMA, ESTAT\`
* Descrizione: Testi e proprietà degli stati utente.
* Possibili Join:
    * **JEST** (Object status): tramite JSTO su \`STSMA\` e JEST.STAT = TJ30.ESTAT
    * **TJ31** (process control user status): su \`STSMA\`

**TJ31 (process control user status)**
* Chiavi Primarie: \`STSMA\`
* Descrizione: Processi di business controllati dallo stato utente.
* Possibili Join:
    * **TJ30** (User status codes): su \`STSMA\`
`;