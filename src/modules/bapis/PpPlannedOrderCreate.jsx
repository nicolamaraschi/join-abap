export const bapi = {
  "name": "BAPI_PLANNEDORDER_CREATE",
  "description": "Crea ordini pianificati, che sono precursori degli ordini di produzione o delle richieste di acquisto.",
  "details": [
    {
      "title": "Parametri di Importazione Esemplari",
      "structures": [
        { "name": "HEADERDATA", "type": "BAPI_PLNDORD_CREATE_HEADER", "fields": [
          { "name": "MATERIAL", "desc": "Materiale da pianificare.", "mandatory": true },
          { "name": "PLANT", "desc": "Divisione di pianificazione.", "mandatory": true },
          { "name": "PLANNING_PLANT", "desc": "Divisione di produzione.", "mandatory": true },
          { "name": "ORDER_TYPE", "desc": "Tipo ordine pianificato.", "mandatory": true },
          { "name": "TOTAL_QUANTITY", "desc": "Quantità da pianificare.", "mandatory": true },
          { "name": "START_DATE", "desc": "Data di inizio schedulata.", "mandatory": true },
          { "name": "END_DATE", "desc": "Data di fine schedulata.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "Esistono anche BAPI per modificare (BAPI_PLANNEDORDER_CHANGE) e cancellare (BAPI_PLANNEDORDER_DELETE) gli ordini pianificati. Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": `"----------------------------------------------------------------------
" 1. BAPI: BAPI_PLANNEDORDER_CREATE
"----------------------------------------------------------------------
  WRITE: / 'Esecuzione Test 1: BAPI_PLANNEDORDER_CREATE'.
  ULINE.

  " Scenario di Esempio: Creazione di un ordine pianificato.
  " I valori qui sono esempi hardcoded per dimostrare i parametri.

  " Dichiarazione variabili LOCALI per BAPI_PLANNEDORDER_CREATE
  DATA: ls_dati_testa_plord  TYPE bapiplaf_i1,
        ls_ritorno_bapi_plord TYPE bapireturn1,
        lv_num_ord_pianif    TYPE plaf-plnum.

  " Popolamento dei parametri di input
  ls_dati_testa_plord-pldord_profile  = 'LA'.
  ls_dati_testa_plord-material        = 'R10001'.
  ls_dati_testa_plord-plan_plant      = '1000'.
  ls_dati_testa_plord-total_plord_qty = 100.
  ls_dati_testa_plord-mrp_area        = '1000'.
  ls_dati_testa_plord-order_fin_date  = '20250630'.
  ls_dati_testa_plord-firming_ind     = lc_vero.
  ls_dati_testa_plord-base_uom        = 'EA'.
  ls_dati_testa_plord-det_schedule    = lc_vero.

  " Chiamata alla BAPI
  CALL FUNCTION 'BAPI_PLANNEDORDER_CREATE'
    EXPORTING
      headerdata   = ls_dati_testa_plord
    IMPORTING
      return       = ls_ritorno_bapi_plord
      plannedorder = lv_num_ord_pianif.

  " Gestione dell'esito della BAPI
  DATA: lv_successo_plord     TYPE abap_bool,
lt_messaggi_plord     TYPE STANDARD TABLE OF bapiret2,
        ls_riga_messaggio_plord TYPE bapiret2,
        lv_testo_msg_plord    TYPE string.

  " La BAPI restituisce una struttura singola, la convertiamo in una
  " tabella per un'analisi e una visualizzazione standard.
  MOVE-CORRESPONDING ls_ritorno_bapi_plord TO ls_riga_messaggio_plord.
  APPEND ls_riga_messaggio_plord TO lt_messaggi_plord.

  " Verifica se ci sono stati errori o messaggi di abort (E, A)
  lv_successo_plord = lc_vero.
  LOOP AT lt_messaggi_plord INTO ls_riga_messaggio_plord WHERE type CA 'EA'.
    lv_successo_plord = lc_falso.
    lv_testo_msg_plord = ls_riga_messaggio_plord-message.
    EXIT. " Basta il primo errore grave
  ENDLOOP.

  " Gestione COMMIT / ROLLBACK post BAPI
  IF lv_successo_plord = lc_falso.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    IF lv_testo_msg_plord IS INITIAL.
      lv_testo_msg_plord = 'Creazione ordine pianificato fallita. Eseguito ROLLBACK.'.
    ENDIF.
    MESSAGE lv_testo_msg_plord TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    " Eseguo il COMMIT. Eventuali errori di aggiornamento non vengono
    " riportati in sy-subrc ma vanno controllati in SM13.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    DATA(lv_msg_succ_plord) = |Ordine pianificato creato: { lv_num_ord_pianif }|.
    MESSAGE lv_msg_succ_plord TYPE 'S'.
  ENDIF.

  " Visualizzazione dei messaggi dettagliati della BAPI
  IF lt_messaggi_plord IS NOT INITIAL.
    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
  EXPORTING
        it_message = lt_messaggi_plord.
  ENDIF.

  NEW-LINE.
  NEW-LINE.
`
};
