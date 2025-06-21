export const bapi = {
  "name": "BAPI_PRODORD_COMPLETE_TECH",
  "description": "Imposta lo stato di un ordine di produzione su 'Tecnicamente Completo' (TECO).",
  "details": [
    {
      "title": "Parametri di Importazione Chiave",
      "structures": [
        { "name": "Import", "type": "Singolo o Tabella", "fields": [
          { "name": "ORDER_NUMBER", "desc": "Numero Ordine di Produzione per chiusura singola.", "mandatory": false },
          { "name": "ORDERS", "desc": "Tabella di Chiavi Ordine (BAPI_ORDER_KEY) per chiusura multipla.", "mandatory": false }
        ]}
      ]
    },
    { "title": "Note", "content": "Indica che l'ordine è terminato dal punto di vista logistico. Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": `"----------------------------------------------------------------------
" 3. BAPI: BAPI_PRODORD_COMPLETE_TECH
"----------------------------------------------------------------------
  WRITE: / 'Esecuzione Test 3: BAPI_PRODORD_COMPLETE_TECH'.
  ULINE.

  " Scenario: Chiusura tecnica di un ordine di produzione.
  DATA: lt_ordini_input_prodord   TYPE STANDARD TABLE OF bapi_order_key,
        lt_ritorno_dett_prodord TYPE STANDARD TABLE OF bapi_order_return,
        lt_messaggi_prodord     TYPE STANDARD TABLE OF bapiret2.

  " Popolamento dati di input
  APPEND VALUE #( order_number = '000000000042' ) TO lt_ordini_input_prodord.

  " Chiamata alla BAPI
  CALL FUNCTION 'BAPI_PRODORD_COMPLETE_TECH'
    TABLES
      orders        = lt_ordini_input_prodord
      detail_return = lt_ritorno_dett_prodord.

  " Gestione esito BAPI
  DATA: lv_successo_prodord  TYPE abap_bool,
        lv_testo_msg_prodord TYPE string.

  " Converti i messaggi di ritorno specifici in BAPIRET2 standard
  lv_successo_prodord = lc_vero.
  LOOP AT lt_ritorno_dett_prodord INTO DATA(ls_ritorno_dett).
    " *** INIZIO BLOCCO CORRETTO ***
    DATA ls_messaggio TYPE bapiret2.
    CLEAR ls_messaggio.

    " Assegnazione manuale per massima compatibilità, come da codice originale.
    ls_messaggio-type     = ls_ritorno_dett-type.
    "ls_messaggio-id       = ls_ritorno_dett-message_id.
    "ls_messaggio-number   = ls_ritorno_dett-message_no.
    ls_messaggio-message  = ls_ritorno_dett-message_v1 && ls_ritorno_dett-message_v2 &&
                            ls_ritorno_dett-message_v3 && ls_ritorno_dett-message_v4.
    APPEND ls_messaggio TO lt_messaggi_prodord.
    " *** FINE BLOCCO CORRETTO ***

    IF ls_messaggio-type CA 'EA'.
      lv_successo_prodord = lc_falso.
      IF lv_testo_msg_prodord IS INITIAL.
        lv_testo_msg_prodord = ls_messaggio-message.
      ENDIF.
    ENDIF.
  ENDLOOP.

  " Gestione COMMIT / ROLLBACK
  IF lv_successo_prodord = lc_falso.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE 'Chiusura tecnica ordine produzione fallita. Eseguito ROLLBACK.' TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    " Eseguo il COMMIT. Eventuali errori di aggiornamento non vengono
    " riportati in sy-subrc ma vanno controllati in SM13.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    MESSAGE 'Chiusura tecnica ordine di produzione eseguita con successo.' TYPE 'S'.
  ENDIF.

  " Visualizzazione messaggi
  IF lt_messaggi_prodord IS NOT INITIAL.
    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING
        it_message = lt_messaggi_prodord.
  ENDIF.

  NEW-LINE.
  NEW-LINE.
`
};
