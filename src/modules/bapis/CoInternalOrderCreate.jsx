export const bapi = {
  "name": "BAPI_INTERNALORDER_CREATE",
  "description": "Creare Ordine Interno.",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori",
      "structures": [
        { "name": "ORDER_TYPE", "type": "Singolo", "fields": [
          { "name": "ORDER_TYPE", "desc": "Tipo di ordine interno.", "mandatory": true }
        ]},
        { "name": "MASTER_DATA", "type": "BAPI2075_2", "fields": [
          { "name": "CO_AREA", "desc": "Area di controlling.", "mandatory": true },
          { "name": "OBJECT_CLASS", "desc": "Classe oggetto (es. 'OC' per Spese Generali).", "mandatory": true },
          { "name": "SHORT_TEXT", "desc": "Descrizione breve.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "Per la modifica, usare BAPI_INTERNALORDER_CHANGE. Per ottenere i dettagli, BAPI_INTERNALORDER_GETDETAIL. Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": `"----------------------------------------------------------------------
" 9. BAPI: BAPI_INTERNALORDER_CREATE
"----------------------------------------------------------------------
  WRITE: / 'Esecuzione Test 9: BAPI_INTERNALORDER_CREATE'.
  ULINE.

  " Scenario: Creazione di un nuovo ordine interno.
  DATA: ls_dati_anag_io       TYPE bapi2075_7,
        lv_testrun_io         TYPE abap_bool VALUE lc_falso,
        lv_id_ordine_creato   TYPE aufnr,
        lt_messaggi_io        TYPE STANDARD TABLE OF bapiret2.

  " Popolamento dei dati anagrafici
  ls_dati_anag_io-order_type       = 'IA01'.
  ls_dati_anag_io-comp_code        = '1000'.
  ls_dati_anag_io-order_name       = 'Ordine Interno Test BAPI'.
  ls_dati_anag_io-co_area          = '1000'.
  ls_dati_anag_io-profit_ctr       = 'PCTR001'.
  ls_dati_anag_io-respcctr         = 'CC_RESP'.
  ls_dati_anag_io-currency         = 'EUR'.
  ls_dati_anag_io-person_resp      = 'UTENTE_PROVA'.
  ls_dati_anag_io-in_charge_user   = sy-uname.
  ls_dati_anag_io-date_work_begins = sy-datum.
  ls_dati_anag_io-date_work_ends   = sy-datum + 30.

  " Chiamata alla BAPI
  CALL FUNCTION 'BAPI_INTERNALORDER_CREATE'
    EXPORTING
      i_master_data = ls_dati_anag_io
      testrun       = lv_testrun_io
    IMPORTING
      orderid       = lv_id_ordine_creato
    TABLES
      return        = lt_messaggi_io.

  " Gestione esito BAPI
  DATA lv_successo_io TYPE abap_bool VALUE lc_vero.
  LOOP AT lt_messaggi_io INTO DATA(ls_msg_io) WHERE type CA 'EA'.
    lv_successo_io = lc_falso.
    EXIT.
  ENDLOOP.

  " Gestione COMMIT / ROLLBACK (annulla anche se era solo un testrun)
  IF lv_successo_io = lc_falso OR lv_testrun_io = lc_vero.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    IF lv_testrun_io = lc_vero AND lv_successo_io = lc_vero.
      MESSAGE 'Esecuzione in modalità TEST completata con successo. Eseguito ROLLBACK.' TYPE 'S'.
    ELSE.
      MESSAGE 'Creazione ordine interno fallita. Eseguito ROLLBACK.' TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    DATA(lv_msg_succ_io) = |Ordine interno creato con successo: { lv_id_ordine_creato }|.
    MESSAGE lv_msg_succ_io TYPE 'S'.
  ENDIF.

  " Visualizzazione messaggi
  IF lt_messaggi_io IS NOT INITIAL.
    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING
        it_message = lt_messaggi_io.
  ENDIF.

  NEW-LINE.
  NEW-LINE.`
};
