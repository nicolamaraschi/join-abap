export const bapi = {
  "name": "BAPI_PRODORD_RELEASE",
  "description": "Rilascia uno o più ordini di produzione, rendendoli pronti per l'esecuzione (es. movimenti merci, conferme).",
  "details": [
    {
      "title": "Parametri di Importazione Chiave",
      "structures": [
        { "name": "Import", "type": "Singolo o Tabella", "fields": [
          { "name": "ORDER_NUMBER", "desc": "Numero Ordine di Produzione per rilascio singolo.", "mandatory": false },
          { "name": "ORDERS", "desc": "Tabella di Chiavi Ordine (BAPI_ORDER_KEY) per rilascio multiplo.", "mandatory": false }
        ]}
      ]
    },
    { "title": "Note", "content": "È necessario fornire o un singolo numero d'ordine o una tabella di ordini. Il rilascio è un passo cruciale per l'esecuzione dell'ordine. Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": `"--------------------------------------------------------------------
  " 2. LOGICA BAPI_PRODORD_RELEASE (Rilascio Ordine di Produzione)
  "--------------------------------------------------------------------
  " La logica di rilascio viene eseguita solo se la creazione ha avuto successo.

  " Dati locali per il rilascio ordine
  DATA: lt_orders_release  TYPE STANDARD TABLE OF bapi_order_key.
  DATA: ls_order_key_rel   TYPE bapi_order_key.
  DATA: lt_detail_return   TYPE STANDARD TABLE OF bapi_order_return.
  DATA: lt_app_log         TYPE STANDARD TABLE OF bapi_order_application_log.
  DATA: ls_return_release  TYPE bapiret2.

  FIELD-SYMBOLS: <fs_order_key>   TYPE bapi_order_key,
                 <fs_detail_ret>  TYPE bapi_order_return.

  " Reset delle variabili di stato per la nuova BAPI
  CLEAR: lv_bapi_msg_txt.
  lv_bapi_success = gc_true.

  " Preparazione dati di input per il rilascio
  ls_order_key_rel-order_number = lv_ord_num_creato.
  APPEND ls_order_key_rel TO lt_orders_release.

  " Log di input per la documentazione
  ULINE.
  WRITE: / '*** 2. Dati di Input per BAPI_PRODORD_RELEASE ***'.
  LOOP AT lt_orders_release ASSIGNING <fs_order_key>.
    WRITE: / 'Ordine da Rilasciare:', <fs_order_key>-order_number.
  ENDLOOP.
  ULINE.

  " Chiamata alla BAPI di rilascio
  CALL FUNCTION 'BAPI_PRODORD_RELEASE'
    IMPORTING
      return             = ls_return_release
    TABLES
      orders             = lt_orders_release
      detail_return      = lt_detail_return
      application_log    = lt_app_log.

  " Analisi del risultato
  WRITE: / '*** Messaggi da BAPI_PRODORD_RELEASE ***'.
  IF ls_return_release IS NOT INITIAL.
    WRITE: / 'Messaggio Principale:'.
    WRITE: / 'Tipo:', ls_return_release-type, 'ID:', ls_return_release-id, 'Nr:', ls_return_release-number, 'Messaggio:', ls_return_release-message.
    IF ls_return_release-type CA 'EA'.
      lv_bapi_success = gc_false.
      IF lv_bapi_msg_txt IS INITIAL.
        lv_bapi_msg_txt = ls_return_release-message.
      ENDIF.
    ENDIF.
  ENDIF.

  WRITE: / 'Messaggi di Dettaglio:'.
  IF lt_detail_return IS INITIAL.
    WRITE: / 'Nessun messaggio di dettaglio restituito.'.
  ELSE.
    LOOP AT lt_detail_return ASSIGNING <fs_detail_ret>.
      WRITE: / 'Ordine:', <fs_detail_ret>-order_number, 'Tipo:', <fs_detail_ret>-type, 'ID:', <fs_detail_ret>-id, 'Nr:', <fs_detail_ret>-number.
      WRITE: '  Messaggio:', <fs_detail_ret>-message.
      IF <fs_detail_ret>-type CA 'EA' AND lv_bapi_success = gc_true.
        lv_bapi_success = gc_false.
        lv_bapi_msg_txt = <fs_detail_ret>-message.
      ENDIF.
    ENDLOOP.
  ENDIF.
  ULINE.

  " Gestione transazione finale (COMMIT/ROLLBACK)
  IF lv_bapi_success = gc_false.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    IF lv_bapi_msg_txt IS INITIAL.
       lv_bapi_msg_txt = 'Errore generico durante il rilascio, controllare il log dettagliato.'.
    ENDIF.
    MESSAGE |Errore durante il rilascio dell''ordine { lv_ord_num_creato }: { lv_bapi_msg_txt }| TYPE 'E'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    MESSAGE |Ordine di produzione { lv_ord_num_creato } rilasciato con successo.| TYPE 'S'.
    WRITE: / 'Commit eseguito con successo per le operazioni.'.
  ENDIF.

`
};
