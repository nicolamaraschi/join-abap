export const bapi = {
  "name": "BAPI_COSTCENTER_CREATEMULTIPLE",
  "description": "Creare Centri di Costo (multipli).",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori",
      "structures": [
        { "name": "CONTROLLINGAREA", "type": "Singolo", "fields": [
          { "name": "CONTROLLINGAREA", "desc": "Area di controlling in cui creare i centri di costo.", "mandatory": true }
        ]},
        { "name": "COSTCENTERLIST", "type": "Tabella di BAPI0012_CCLIST1", "fields": [
          { "name": "COSTCENTER", "desc": "Codice del nuovo centro di costo.", "mandatory": true },
          { "name": "VALID_FROM", "desc": "Data inizio validità.", "mandatory": true },
          { "name": "NAME", "desc": "Nome del centro di costo.", "mandatory": true },
          { "name": "DESCRIPT", "desc": "Descrizione del centro di costo.", "mandatory": true },
          { "name": "PERSON_IN_CHARGE", "desc": "Responsabile.", "mandatory": true },
          { "name": "COSTCTR_HIER_GRP", "desc": "Nodo della gerarchia standard.", "mandatory": true },
          { "name": "COMP_CODE", "desc": "Società.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "Permette la creazione massiva di centri di costo. Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": `
"----------------------------------------------------------------------
" 8. BAPI: BAPI_COSTCENTER_CREATEMULTIPLE
"----------------------------------------------------------------------
  WRITE: / 'Esecuzione Test 8: BAPI_COSTCENTER_CREATEMULTIPLE'.
  ULINE.

  " Scenario: Creazione di nuovi centri di costo.
  DATA: lv_area_controlling_cc  TYPE kokrs VALUE '1000',
        lv_testrun_cc           TYPE abap_bool VALUE lc_falso, " Impostare a VERO per testare senza salvare
        lt_lista_centri_costo   TYPE STANDARD TABLE OF bapi0012_ccinputlist,
        lt_messaggi_cc          TYPE STANDARD TABLE OF bapiret2.

  " Popolamento della lista dei centri di costo da creare
  " --- Primo Centro di Costo
  APPEND VALUE #(
      costcenter      = 'CC_TEST_BAPI_01'
      valid_from      = '20250101'
      valid_to        = '99991231'
      name            = 'Centro Costo Test BAPI 01'
      "description     = 'Descrizione test per CC BAPI 01'
      "cocog_area      = lv_area_controlling_cc
      "person_resp     = 'UTENTE01'
      "hierarchy_area  = 'STD_HIER'
      "company_code    = '1000'
      profit_ctr      = 'PCTR001'
      "cc_type         = 'A'
  ) TO lt_lista_centri_costo.

  " --- Secondo Centro di Costo
  APPEND VALUE #(
      costcenter      = 'CC_TEST_BAPI_02'
      valid_from      = '20250101'
      valid_to        = '99991231'
      name            = 'Centro Costo Test BAPI 02'
      "description     = 'Descrizione test per CC BAPI 02'
      "cocog_area      = lv_area_controlling_cc
      "person_resp     = 'UTENTE02'
      "hierarchy_area  = 'STD_HIER'
      "company_code    = '1000'
      profit_ctr      = 'PCTR002'
      "cc_type         = 'B'
  ) TO lt_lista_centri_costo.

  " Chiamata alla BAPI
  CALL FUNCTION 'BAPI_COSTCENTER_CREATEMULTIPLE'
    EXPORTING
      controllingarea = lv_area_controlling_cc
      testrun         = lv_testrun_cc
    TABLES
      costcenterlist  = lt_lista_centri_costo
      return          = lt_messaggi_cc.

  " Gestione esito BAPI
  DATA lv_successo_cc TYPE abap_bool VALUE lc_vero.
  LOOP AT lt_messaggi_cc INTO DATA(ls_msg_cc) WHERE type CA 'EA'.
    lv_successo_cc = lc_falso.
    EXIT.
  ENDLOOP.

  " Gestione COMMIT / ROLLBACK (annulla anche se era solo un testrun)
  IF lv_successo_cc = lc_falso OR lv_testrun_cc = lc_vero.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    IF lv_testrun_cc = lc_vero AND lv_successo_cc = lc_vero.
      MESSAGE 'Esecuzione in modalità TEST completata con successo. Eseguito ROLLBACK.' TYPE 'S'.
    ELSE.
      MESSAGE 'Creazione centri di costo fallita. Eseguito ROLLBACK.' TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.
  ELSE.
    " Eseguo il COMMIT.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    MESSAGE 'Centri di costo creati con successo.' TYPE 'S'.
  ENDIF.

  " Visualizzazione messaggi
  IF lt_messaggi_cc IS NOT INITIAL.
    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING
        it_message = lt_messaggi_cc.
  ENDIF.
`
};
