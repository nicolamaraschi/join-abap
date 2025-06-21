export const bapi = {
  "name": "BAPI_PROFITCENTER_CREATE",
  "description": "Creare Centro di Profitto.",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori",
      "structures": [
        { "name": "CONTROLLINGAREA", "type": "Singolo", "fields": [
          { "name": "CONTROLLINGAREA", "desc": "Area di controlling.", "mandatory": true }
        ]},
        { "name": "PROFITCENTER", "type": "Singolo", "fields": [
          { "name": "PROFITCENTER", "desc": "Codice del nuovo centro di profitto.", "mandatory": true }
        ]},
        { "name": "VALIDFROM", "type": "Singolo", "fields": [
          { "name": "VALIDFROM", "desc": "Data inizio validità.", "mandatory": true }
        ]},
        { "name": "BASICDATA", "type": "BAPI0015_1", "fields": [
          { "name": "PRCTR_NAME", "desc": "Nome del centro di profitto.", "mandatory": true },
          { "name": "IN_CHARGE", "desc": "Responsabile.", "mandatory": true },
          { "name": "PRCTR_HIER_GRP", "desc": "Nodo della gerarchia standard.", "mandatory": true }
        ]},
        { "name": "COMPANYCODEASSIGNMENT", "type": "Tabella di BAPI0015_4", "fields": [
          { "name": "COMP_CODE", "desc": "Società da assegnare al centro di profitto.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "Richiede l'assegnazione ad almeno una società. Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": `"&---------------------------------------------------------------------*
"&      Form  f_testa_profitcenter_create
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_PROFITCENTER_CREATE - Creazione Centro di Profitto
"----------------------------------------------------------------------
FORM f_testa_profitcenter_create.
  DATA: lt_companycodes   TYPE STANDARD TABLE OF bapi0015_7,
        ls_companycodes   TYPE bapi0015_7,
        ls_prctrid        TYPE bapi0015id2,
        ls_prctr_data     TYPE bapi0015_4,
        ls_testrun        TYPE bapi0015_9,
        lv_profit_center  TYPE cepc-prctr, " Tipo per il centro di profitto
        lv_societa_input  TYPE bukrs,      " Tipo per il codice società
        lv_kokrs_sim      TYPE kokrs,      " Simulazione area di contabilità gestionale
        lv_validfrom_pc   TYPE recakeydate,
        lv_validto_pc     TYPE recakeydate,
        lv_attivita_test  TYPE char1. " Variabile per simulare l'attività

  CLEAR: gs_return.
  CLEAR: lt_companycodes.

  " Inizializzazione dati di test (da adattare con valori reali e validi)
  lv_profit_center = 'PC_TEST01'. " Sostituire con un codice Centro di Profitto valido
  lv_societa_input = 'TEST'.     " Sostituire con un codice società valido
  lv_kokrs_sim     = 'K100'.     " Sostituire con una Area di Contabilità Gestionale valida per lv_societa_input
  lv_validfrom_pc  = sy-datum.    " Data di validità (es. oggi)
  lv_validto_pc    = '99991231'.  " Data di validità (fine)

  " Imposta 'X' per test run, ' ' per creare realmente
  lv_attivita_test = 'X'. " Modificare in ' ' per creare il Centro di Profitto

  " Controlli preliminari (simulazione ASSERT)
  IF lv_profit_center IS INITIAL.
    MESSAGE 'Il codice centro di profitto non può essere vuoto.' TYPE 'E'.
    EXIT.
  ENDIF.
  IF lv_societa_input IS INITIAL.
    MESSAGE 'Il codice società non può essere vuoto.' TYPE 'E'.
    EXIT.
  ENDIF.

  " LOGICA TESTRUN
  IF lv_attivita_test = ' '. " Simula reca1_activity-create
    ls_testrun-testrun = gc_false.
  ELSE.
    ls_testrun-testrun = gc_true.
  ENDIF.

  " Popola profitcenterid
  ls_prctrid-co_area    = lv_kokrs_sim. " Area di contabilità gestionale simulata
  ls_prctrid-profit_ctr = lv_profit_center.

  " Popola basicdata
  ls_prctr_data-prctr_name       = 'Test Profit Center'.
  ls_prctr_data-long_text        = 'Centro di Profitto per Test BAPI'.
  ls_prctr_data-in_charge        = sy-uname. " Responsabile
  ls_prctr_data-department       = 'TEST_DEP'. " Dipartimento
  ls_prctr_data-prctr_hier_grp   = 'STANDARD'. " Gruppo gerarchia (sostituire con uno valido)
  ls_prctr_data-logsystem        = sy-sysid.   " Sistema logico
  ls_prctr_data-segment          = 'SEGMENT01'. " Segmento (sostituire con uno valido se usato)
  ls_prctr_data-in_charge_user   = sy-uname. " Utente responsabile

  " Popola companycodes
  ls_companycodes-comp_code       = lv_societa_input.
  ls_companycodes-assign_to_prctr = gc_true.
  APPEND ls_companycodes TO lt_companycodes.

  " Chiamata alla BAPI_PROFITCENTER_CREATE
  CALL FUNCTION 'BAPI_PROFITCENTER_CREATE'
    EXPORTING
      profitcenterid = ls_prctrid
      validfrom      = lv_validfrom_pc
      validto        = lv_validto_pc
      basicdata      = ls_prctr_data
      testrun        = ls_testrun
    IMPORTING
      return         = gs_return
    TABLES
      companycodes   = lt_companycodes.

  PERFORM f_scrivi_esito USING 'BAPI_PROFITCENTER_CREATE'.

  " Gestione COMMIT / ROLLBACK post BAPI
  IF ls_testrun-testrun = gc_false. " Esegui commit solo se non è un testrun
    IF gs_return-type CA 'EA'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      MESSAGE 'Rollback effettuato per BAPI_PROFITCENTER_CREATE.' TYPE 'I'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
      MESSAGE 'Commit effettuato per BAPI_PROFITCENTER_CREATE.' TYPE 'I'.
    ENDIF.
  ELSE.
    MESSAGE 'BAPI_PROFITCENTER_CREATE eseguita in modalità testrun.' TYPE 'I'.
  ENDIF.
ENDFORM.`
};
