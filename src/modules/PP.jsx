export const ppData = `
### Modulo PP (Production Planning)

#### Work center
**CRHH (Work Center Hierarchy)**
* Chiavi Primarie: \`MANDT(MANDT), OBJTY(CR_OBJTY), OBJID(CR_OBJID)\`
* Descrizione: Contiene la definizione delle gerarchie dei posti di lavoro.
* Possibili Join:
  * **CRHS** (Hierarchy Structure): su \`HIERA_ID\`

**CRHS (Hierarchy Structure)**
* Chiavi Primarie: \`MANDT(MANDT), OBJTY_HY(CR_OBJTY), OBJID_HY(CR_OBJID), OBJTY_HO(CR_OBJTY), OBJID_HO(CR_OBJID), OBJTY_UP(OBJTY_UP), OBJID_UP(OBJID_UP)\`
* Descrizione: Definisce la struttura gerarchica, collegando i posti di lavoro.
* Possibili Join:
  * **CRHH** (Work Center Hierarchy): su \`HIERA_ID\`
  * **CRHD** (Work Center Header): su \`OBJID_O=OBJID\` o \`OBJID_U=OBJID\`

**CRHD (Work Center Header)**
* Chiavi Primarie: \`MANDT(MANDT), OBJTY(CR_OBJTY), OBJID(CR_OBJID)\`
* Descrizione: Testata del posto di lavoro, contenente i dati principali.
* Possibili Join:
  * **CRTX** (Text for the Work Center): su \`OBJTY, OBJID, SPRAS\`
  * **CRCO** (Assignment of Work Center to Cost Center): su \`OBJTY, OBJID\`
  * **CRCA** (Work Center Capacity Allocation): su \`OBJTY, OBJID\`

**CRTX (Text for the Work Center or Production Resource/Tool)**
* Chiavi Primarie: \`MANDT(MANDT), OBJTY(CR_OBJTY), OBJID(CR_OBJID), SPRAS(SPRAS)\`
* Descrizione: Testi descrittivi per il posto di lavoro.
* Possibili Join:
  * **CRHD** (Work Center Header): su \`OBJTY, OBJID\`

**CRCO (Assignment of Work Center to Cost Center)**
* Chiavi Primarie: \`MANDT(MANDT), OBJTY(CR_OBJTY), OBJID(CR_OBJID), LASET(CR_LASET), ENDDA(ENDDATUM), LANUM(CR_LANUM)\`
* Descrizione: Collega il posto di lavoro al centro di costo per le imputazioni contabili.
* Possibili Join:
  * **CRHD** (Work Center Header): su \`OBJTY, OBJID\`

**KAKO (Capacity Header Segment)**
* Chiavi Primarie: \`MANDT(MANDT), KAPID(KAPID)\`
* Descrizione: Dati di testata per una specifica capacità (es. capacità macchina, manodopera).
* Possibili Join:
  * **CRCA** (Work Center Capacity Allocation): su \`KAPID\`

**CRCA (Work Center Capacity Allocation)**
* Chiavi Primarie: \`MANDT(MANDT), OBJTY(CR_OBJTY), OBJID(CR_OBJID), CANUM(CR_CAPNUM)\`
* Descrizione: Assegna una o più capacità a un posto di lavoro.
* Possibili Join:
  * **CRHD** (Work Center Header): su \`OBJTY, OBJID\`
  * **KAKO** (Capacity Header Segment): su \`KAPID\`

**TC24 (Person responsible for the workcenter)**
* Chiavi Primarie: \`MANDT(MANDT), WERKS(WERKS_D), VERAN(AP_VERAN)\`
* Descrizione: Tabella di customizing per definire le persone responsabili.
* Possibili Join:
  * **CRHD** (Work Center Header): su \`VERAN\`

**CRC (Logical DB for Work centers)**
* Descrizione: Non è una tabella di database, ma una Logical Database (LDB) usata per i report standard.

#### Routings/operations
**MAPL (Allocation of task lists to materials)**
* Chiavi Primarie: \`MANDT(MANDT), MATNR(MATNR), WERKS(WERKS_D), PLNTY(PLNTY), PLNNR(PLNNR), PLNAL(PLNAL), ZKRIZ(DZKRIZ), ZAEHL(CIM_COUNT)\`
* Descrizione: Collega i materiali ai loro cicli di lavoro specifici.
* Possibili Join:
  * **PLKO** (Task List - Header): su \`PLNTY, PLNNR, PLNAL\` (PLNAL da MAPL)

**PLAS (Task list - selection of operations/activities)**
* Chiavi Primarie: \`MANDT(MANDT), PLNTY(PLNTY), PLNNR(PLNNR), PLNAL(PLNAL), PLNFL(PLNFOLGE), PLNKN(PLNKN), ZAEHL(CIM_COUNT)\`
* Descrizione: Gestisce la selezione e la sequenza delle operazioni all'interno di un ciclo di lavoro.
* Possibili Join:
  * **PLKO** (Task List - Header): su \`PLNTY, PLNNR, PLNAL\`
  * **PLPO** (Task List Operation): su \`PLNTY, PLNNR, PLNKN\`

**PLFH (Task list - production resources/tools)**
* Chiavi Primarie: \`MANDT(MANDT), PLNTY(PLNTY), PLNNR(PLNNR), PZLFH(PZLFH), ZAEHL(CIM_COUNT)\`
* Descrizione: Assegna i production resources/tools (PRT) alle operazioni del ciclo.
* Possibili Join:
  * **PLPO** (Task List Operation): su \`PLNTY, PLNNR, PLNKN\`

**PLFL (Task list - sequences)**
* Chiavi Primarie: \`MANDT(MANDT), PLNTY(PLNTY), PLNNR(PLNNR), PLNAL(PLNAL), PLNFL(PLNFOLGE), ZAEHL(CIM_COUNT)\`
* Descrizione: Definisce le sequenze (standard, alternative, parallele) delle operazioni.
* Possibili Join:
  * **PLKO** (Task list - header): su \`PLNTY, PLNNR\`

**PLKO (Task list - header)**
* Chiavi Primarie: \`MANDT(MANDT), PLNTY(PLNTY), PLNNR(PLNNR), PLNAL(PLNAL), ZAEHL(CIM_COUNT)\`
* Descrizione: Dati di testata per i cicli di lavoro (Routings).
* Possibili Join:
  * **PLAS** (Task list - selection of operations/activities): su \`PLNTY, PLNNR, PLNAL\`
  * **PLPO** (Task List Operation): su \`PLNTY, PLNNR, PLNAL\` (tramite PLAS)
  * **MAPL** (Allocation of task lists to materials): su \`PLNTY, PLNNR, PLNAL\`
  * **PLKZ** (Task list: main header): su \`PLNTY, PLNNR\`
  * **PLFL** (Task list - sequences): su \`PLNTY, PLNNR\`

**PLKZ (Task list: main header)**
* Chiavi Primarie: \`MANDT(MANDT), PLNTY(PLNTY), PLNNR(PLNNR), PLNAL(PLNAL)\`
* Descrizione: Dati di testata principali per un gruppo di cicli di lavoro.
* Possibili Join:
  * **PLKO** (Task List - Header): su \`PLNTY, PLNNR\`

**PLPH (Phases / suboperations)**
* Chiavi Primarie: \`MANDT(MANDT), PLNTY(PLNTY), PLNNR(PLNNR), PLNKN(PLNKN), PLNPH(PLNPH), ZAEHL(CIM_COUNT)\`
* Descrizione: Definisce le fasi o le sotto-operazioni per un'operazione del ciclo.
* Possibili Join:
  * **PLPO** (Task List Operation): su \`PLNTY, PLNNR, PLNKN\`

**PLPO (Task list operation / activity)**
* Chiavi Primarie: \`MANDT(MANDT), PLNTY(PLNTY), PLNNR(PLNNR), PLNKN(PLNKN), ZAEHL(CIM_COUNT)\`
* Descrizione: Dettagli di una singola operazione all'interno di un ciclo di lavoro.
* Possibili Join:
  * **PLAS** (Task list - selection): su \`PLNTY, PLNNR, PLNKN\`
  * **PLMZ** (Allocation of BOM - items to operations): su \`PLNTY, PLNNR, PLNKN\`
  * **PLFH** (Task list - PRT): su \`PLNTY, PLNNR, PLNKN\`
  * **PLPH** (Phases / suboperations): su \`PLNTY, PLNNR, PLNKN\`

**PLPR (Log collector for tasklists)**
* Chiavi Primarie: \`DATUM(CP_DATUM), UZEIT(CP_UZEIT), PLNNR(PLNNR)\`
* Descrizione: Raccoglie i log di modifica per i cicli di lavoro.
* Possibili Join:
  * Nessun join all'interno di questa sottocategoria.

**PLMZ (Allocation of BOM - items to operations)**
* Chiavi Primarie: \`MANDT(MANDT), PLNTY(PLNTY), PLNNR(PLNNR), ZUONR(CIM_ZUORD), ZAEHL(CIM_COUNT)\`
* Descrizione: Collega i componenti di una distinta base (BOM) a specifiche operazioni del ciclo.
* Possibili Join:
  * **PLPO** (Task List Operation): su \`PLNTY, PLNNR, PLNKN\`

#### Bill of material
**STKO (BOM - header)**
* Chiavi Primarie: \`MANDT(MANDT), STLTY(STLTY), STLNR(STNUM), STLAL(STALT), STKOZ(CIM_COUNT)\`
* Descrizione: Dati di testata della distinta base (Bill of Material).
* Possibili Join:
  * **STPO** (BOM Item): su \`STLNR, STLTY\`
  * **MAST** (Material to BOM Link): su \`STLNR, STLAL, STLTY\`
  * **STAS** (BOMs Item Selection): su \`STLNR, STLAL, STLTY\`
  * **STZU** (Permanent BOM data): su \`STLTY, STLNR\`
  * **KDST** (Sales order to BOM link): su \`STLNR, STLTY\`

**STPO (BOM – item)**
* Chiavi Primarie: \`MANDT(MANDT), STLTY(STLTY), STLNR(STNUM), STLKN(STLKN), STPOZ(CIM_COUNT)\`
* Descrizione: Posizioni (componenti) di una distinta base.
* Possibili Join:
  * **STKO** (BOM Header): su \`STLTY, STLNR\`
  * **STAS** (BOMs Item Selection): su \`STLTY, STLNR, STLKN\`
  * **STPN** (BOMs - follow-up control): su \`STLTY, STLNR, STLKN\`
  * **STPU** (BOM - sub-item): su \`STLTY, STLNR, STLKN\`

**STAS (BOMs Item Selection)**
* Chiavi Primarie: \`MANDT(MANDT), STLTY(STLTY), STLNR(STNUM), STLAL(STALT), STLKN(STLKN), STASZ(CIM_COUNT)\`
* Descrizione: Selezione delle posizioni della BOM (es. per validità data o altri criteri).
* Possibili Join:
  * **STPO** (BOM Item): su \`STLTY, STLNR, STLKN\`
  * **STKO** (BOM Header): su \`STLTY, STLNR, STLAL\`

**STPN (BOMs - follow-up control)**
* Chiavi Primarie: \`MANDT(MANDT), MATNR(MATNR), STLTY(STLTY), STLNR(STNUM), DPMAT(MATNR)\`
* Descrizione: Dati di controllo per le modifiche alle posizioni della BOM.
* Possibili Join:
  * **STPO** (BOM Item): su \`STLTY, STLNR, STLKN\`

**STPU (BOM - sub-item)**
* Chiavi Primarie: \`MANDT(MANDT), STLTY(STLTY), STLNR(STNUM), STLKN(STLKN), STPOZ(CIM_COUNT), UPOSZ(UPOSZ)\`
* Descrizione: Sotto-posizioni di una distinta base (es. per indicare punti di installazione).
* Possibili Join:
  * **STPO** (BOM Item): su \`STLTY, STLNR, STLKN\`

**STZU (Permanent BOM data)**
* Chiavi Primarie: \`MANDT(MANDT), STLTY(STLTY), STLNR(STNUM)\`
* Descrizione: Dati anagrafici permanenti/addizionali per una BOM.
* Possibili Join:
  * **STKO** (BOM Header): su \`STLTY, STLNR\`

**MAST (Material to BOM link)**
* Chiavi Primarie: \`MANDT(MANDT), MATNR(MATNR), WERKS(WERKS_D), STLAN(STLAN), STLNR(STNUM), STLAL(STALT)\`
* Descrizione: Collega un materiale (in un certo impianto e per un certo utilizzo) alla sua distinta base.
* Possibili Join:
  * **STKO** (BOM Header): su \`STLNR, STLAL, STLTY\`

**KDST (Sales order to BOM link)**
* Chiavi Primarie: \`MANDT(MANDT), VBELN(CS_VBELN), VBPOS(CS_VBPOS), MATNR(MATNR), WERKS(WERKS_D), STLAN(STLAN), STLNR(STNUM), STLAL(STALT)\`
* Descrizione: Collega un ordine di vendita a una distinta base specifica per l'ordine (Order BOM).
* Possibili Join:
  * **STKO** (BOM Header): su \`STLNR, STLTY\`

#### Production orders
**AUFK (Production order headers)**
* Chiavi Primarie: \`MANDT(MANDT), AUFNR(AUFNR)\`
* Descrizione: Testata generale per tutti i tipi di ordini, inclusi quelli di produzione.
* Possibili Join:
  * **AFKO** (Order header data PP orders): su \`AUFNR\`
  * **AFIH** (Maintenance order header): su \`AUFNR\`
  * **JEST** (Object status): su \`OBJNR\`
  * **JSTO** (Status profile): su \`OBJNR\`
  * **AUFM** (Goods movement for prod. order): su \`AUFNR\`

**AFIH (Maintenance order header)**
* Chiavi Primarie: \`MANDT(MANDT), AUFNR(AUFNR)\`
* Descrizione: Testata specifica per ordini di manutenzione (rilevante in PP per ordini di ricondizionamento).
* Possibili Join:
  * **AUFK** (Order Master Data): su \`AUFNR\`

**AUFM (Goods movement for prod. order)**
* Chiavi Primarie: \`MANDT(MANDT), MBLNR(MBLNR), MJAHR(MJAHR), ZEILE(MBLPO)\`
* Descrizione: Tabella indice per i movimenti merci relativi a un ordine (Nota: obsoleto in S/4HANA, usare MATDOC).
* Possibili Join:
  * **AUFK** (Order Header): su \`AUFNR\`

**AFKO (Order header data PP orders)**
* Chiavi Primarie: \`MANDT(MANDT), AUFNR(AUFNR)\`
* Descrizione: Dati di testata specifici per l'ordine di produzione (quantità, date, DBM, ciclo).
* Possibili Join:
  * **AUFK** (Order Master Data): su \`AUFNR\`
  * **AFPO** (Order Item): su \`AUFNR\`
  * **AFVC** (Order Operations): su \`AUFPL\`
  * **AFFL** (Work order sequence): su \`AUFPL\`
  * **AFRU** (Order completion confirmations): su \`AUFNR\`
  * **DRAD_PORDER** (Documents linked to prod. order): su \`AUFNR\`

**AFPO (Order item)**
* Chiavi Primarie: \`MANDT(MANDT), AUFNR(AUFNR), POSNR(CO_POSNR)\`
* Descrizione: Dati della posizione dell'ordine di produzione.
* Possibili Join:
  * **AFKO** (Order Header Data PP Orders): su \`AUFNR\`
  * **RESB** (Order Components): su \`AUFNR\`
  * **PRPS** (WBS Element): su \`PROJN = PSPNR\`

**AFRU (Order completion confirmations)**
* Chiavi Primarie: \`MANDT(MANDT), RUECK(CO_RUECK), RMZHL(CO_RMZHL)\`
* Descrizione: Conferme di completamento (consuntivazioni) per le operazioni dell'ordine.
* Possibili Join:
  * **AFKO** (Order Header Data PP Orders): su \`AUFNR\`
  * **AFVC** (Order operations): su \`AUFPL, APLZL, VORNR\`

**AFVC (Order operations)**
* Chiavi Primarie: \`MANDT(MANDT), AUFPL(CO_AUFPL), APLZL(CO_APLZL)\`
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
* Chiavi Primarie: \`MANDT(MANDT), AUFPL(CO_AUFPL), APLZL(CO_APLZL)\`
* Descrizione: Dettagli quantitativi, date e valori per un'operazione dell'ordine.
* Possibili Join:
  * **AFVC** (Order Operations): su \`AUFPL, APLZL\`

**AFVU (User fields of the operation)**
* Chiavi Primarie: \`MANDT(MANDT), AUFPL(CO_AUFPL), APLZL(CO_APLZL)\`
* Descrizione: Campi utente personalizzati per le operazioni dell'ordine.
* Possibili Join:
  * **AFVC** (Order Operations): su \`AUFPL, APLZL\`

**AFFL (Work order sequence)**
* Chiavi Primarie: \`MANDT(MANDT), AUFPL(CO_AUFPL), APLZL(CIM_COUNT)\`
* Descrizione: Sequenza delle operazioni all'interno dell'ordine.
* Possibili Join:
  * **AFKO** (Order Header Data PP Orders): su \`AUFPL\`

**AFFH (PRT assignment data for the work order)**
* Chiavi Primarie: \`MANDT(MANDT), AUFPL(CO_AUFPL), PZLFH(PZLFH)\`
* Descrizione: Assegnazione dei Production Resources/Tools (PRT) alle operazioni dell'ordine.
* Possibili Join:
  * **AFVC** (Order Operations): su \`AUFPL, APLZL\`
  * **CRVD_B** (Document link to PRT): su \`OBJID\`

**CRVD_B (Document link to PRT)**
* Chiavi Primarie: \`MANDT(MANDT), DOKAR(DOKAR), DOKNR(DOKNR), DOKVR(DOKVR), DOKTL(DOKTL_D), OBJTY(CR_OBJTY), OBJID(CR_OBJID)\`
* Descrizione: Collega un oggetto (come un PRT) a un documento.
* Possibili Join:
    * **AFFH** (PRT assignment): su \`OBJID\`
    * **DRAW** (Document info record): su \`DOKAR, DOKNR, DOKVR, DOKTL\`

**DRAD_PORDER (Documents linked to production order)**
* Chiavi Primarie: \`MANDT(MANDT), DOKAR(DOKAR), DOKNR(DOKNR), DOKVR(DOKVR), DOKTL(DOKTL_D), OBJKY(OBJKY)\`
* Descrizione: Tabella di collegamento tra documenti DMS e ordini di produzione.
* Possibili Join:
    * **AFKO** (Order header data PP orders): su \`AUFNR\`
    * **DRAW** (Document info record): su \`DOCAR, DOCNR, DOCVR, DOCTL\`

**DRAW (Document info record)**
* Chiavi Primarie: \`MANDT(MANDT), DOKAR(DOKAR), DOKNR(DOKNR), DOKVR(DOKVR), DOKTL(DOKTL_D)\`
* Descrizione: Anagrafica del documento nel sistema DMS.
* Possibili Join:
    * **DRAD_PORDER** (Documents linked to prod. order): su \`DOCAR, DOCNR, DOCVR, DOCTL\`
    * **CRVD_B** (Document link to PRT): su \`DOKAR, DOKNR, DOKVR, DOKTL\`

**JEST (Object status)**
* Chiavi Primarie: \`MANDT(MANDT), OBJNR(J_OBJNR), STAT(J_STATUS)\`
* Descrizione: Tabella centrale che memorizza gli stati attivi per qualsiasi oggetto SAP.
* Possibili Join:
  * **AUFK** (Order Header): su \`OBJNR\`
  * **TJ30** (User status codes): su \`STAT = ESTAT\` (e JSTO.STSMA per il profilo corretto)

**JSTO (Status profile)**
* Chiavi Primarie: \`MANDT(MANDT), OBJNR(J_OBJNR)\`
* Descrizione: Informazioni sullo schema di stato per un oggetto.
* Possibili Join:
  * **AUFK** (Order Header): su \`OBJNR\`
  * **TJ30** (User status codes): su \`STSMA\`

**RESB (Order components)**
* Chiavi Primarie: \`MANDT(MANDT), RSNUM(RSNUM), RSPOS(RSPOS), RSART(RSART)\`
* Descrizione: Componenti materiali richiesti (impegni) per un ordine.
* Possibili Join:
  * **AFPO** (Order Item): su \`AUFNR\`
  * **AFVC** (Order operations): su \`AUFPL, APLZL, VORNR\`

**TJ30 (User status codes)**
* Chiavi Primarie: \`MANDT(MANDT), STSMA(J_STSMA), ESTAT(J_ESTAT)\`
* Descrizione: Testi e proprietà degli stati utente.
* Possibili Join:
    * **JEST** (Object status): tramite JSTO su \`STSMA\` e JEST.STAT = TJ30.ESTAT
    * **TJ31** (process control user status): su \`STSMA\`

**TJ31 (process control user status)**
* Chiavi Primarie: \`MANDT(MANDT), STSMA(J_STSMA), VRGNG(J_VORGANG), ESTAT(J_ESTAT)\`
* Descrizione: Processi di business controllati dallo stato utente.
* Possibili Join:
    * **TJ30** (User status codes): su \`STSMA\`

#### Project System
**PROJ (Project definition)**
* Chiavi Primarie: \`MANDT(MANDT), PSPNR(PS_INTNR)\`
* Descrizione: Dati anagrafici della definizione di progetto.
* Possibili Join:
    * **PRPS** (WBS Element): su \`PSPNR = PSPHI\`

**PRPS (WBS (Work Breakdown Structure) Element Master Data)**
* Chiavi Primarie: \`MANDT(MANDT), PSPNR(PS_POSNR)\`
* Descrizione: Dati anagrafici dell'elemento WBS (Work Breakdown Structure).
* Possibili Join:
    * **AFPO** (Order Item): su \`PSPNR = PS_PSP_PNR\`
    * **PROJ** (Project definition): su \`PSPHI = PSPNR\`
    * **PRTE** (Scheduling Data for Project Item): su \`PSPHI, POSNR=PSPNR\`
    * **PRHIS** (Standard WBS, Edges (Hierarchy pointers)): su \`PSPHI, POSNR=PSPNR\`
    * **PRTX** (PS Texts (WBS)): su \`PRPSPNR=PSPNR\`

**PRTE (Scheduling Data for Project Item)**
* Chiavi Primarie: \`MANDT(MANDT), POSNR(PS_POSNR)\`
* Descrizione: Dati di schedulazione per la posizione di progetto.
* Possibili Join:
    * **PRPS** (WBS Element): su \`PSPHI=PSPHI, POSNR=PSPNR\`

**PRHIS (Standard WBS, Edges (Hierarchy pointers))**
* Chiavi Primarie: \`MANDT(MANDT), POSNR(PS_SPSNR)\`
* Descrizione: Puntatori di gerarchia nella WBS standard.
* Possibili Join:
    * **PRPS** (WBS Element): su \`PSPHI=PSPHI, POSNR=PSPNR\`

**PRTX (PS Texts (WBS))**
* Chiavi Primarie: \`PRMANDT(MANDT), PROBTYP(PS_OBTYP), PRPSPNR(PS_POSNR), PRTXTKY(TXTKY)\`
* Descrizione: Identificativo dei testi a livello di WBS.
* Possibili Join:
    * **PRPS** (WBS Element): su \`PRPSPNR=PSPNR\`
    * **PSTX** (PS Texts (Header)): su \`PRTXTKY\`

**PSTX (PS Texts (Header))**
* Chiavi Primarie: \`PSMANDT(MANDT), PSTXTKY(TXTKY)\`
* Descrizione: Testata dei testi di progetto.
* Possibili Join:
    * **PRTX** (PS Texts (WBS)): su \`PRTXTKY\`
    * **PSTT** (PS texts (description)): su \`PRTXTKY\`

**PSTT (PS texts (description))**
* Chiavi Primarie: \`PTMANDT(MANDT), PTLANGU(LANGU), PTTXTKY(TXTKY)\`
* Descrizione: Descrizione effettiva dei testi di progetto in varie lingue.
* Possibili Join:
    * **PSTX** (PS Texts (Header)): su \`PRTXTKY\`
`;