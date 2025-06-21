export const bapi = {
  "name": "BAPI_PRODORD_CREATE",
  "description": "Utilizzata per la creazione di ordini di produzione.",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori",
      "structures": [
        { "name": "ORDERDATA", "type": "BAPI_PP_ORDER_CREATE", "fields": [
          { "name": "MATERIAL", "desc": "Materiale di testata per l'ordine.", "mandatory": true },
          { "name": "PLANT", "desc": "Divisione di produzione.", "mandatory": true },
          { "name": "ORDER_TYPE", "desc": "Tipo ordine.", "mandatory": true },
          { "name": "QUANTITY", "desc": "Quantità totale dell'ordine.", "mandatory": true },
          { "name": "BASIC_START_DATE", "desc": "Data di inizio base.", "mandatory": true },
          { "name": "BASIC_END_DATE", "desc": "Data di fine base.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "L'obbligatorietà delle date dipende dalla configurazione del tipo di schedulazione. Richiede BAPI_TRANSACTION_COMMIT per il salvataggio." }
  ],
  "content": `"--------------------------------------------------------------------
  " 1. LOGICA BAPI_PRODORD_CREATE (Creazione Ordine di Produzione)
  "--------------------------------------------------------------------
  " Dati locali per la creazione ordine
  DATA: ls_ord_create_data TYPE bapi_pp_order_create.
  DATA: lv_ord_num_creato  TYPE bapi_order_key-order_number.
  DATA: lv_ord_type_creato TYPE bapi_order_copy-order_type.
  DATA: ls_return_create   TYPE bapiret2.

  " Variabili per la gestione del successo e dei messaggi
  DATA: lv_bapi_success TYPE abap_bool VALUE gc_true.
  DATA: lv_bapi_msg_txt TYPE string.

  " Dati di Test Fissi (DA ADEGUARE AL TUO SISTEMA)
  ls_ord_create_data-material         = 'R-B101'. " Esempio: Sostituisci con un Materiale esistente
  ls_ord_create_data-plant            = '1000'.   " Esempio: Sostituisci con una Divisione esistente
  ls_ord_create_data-order_type       = 'PP01'.   " Esempio: Sostituisci con un Tipo Ordine esistente
  ls_ord_create_data-quantity         = '100'.    " Quantita' di test
  ls_ord_create_data-basic_start_date = sy-datum.
  ls_ord_create_data-basic_end_date   = sy-datum + 1. " Giorno successivo

  " Log di input per la documentazione
  ULINE.
  WRITE: / '*** 1. Dati di Input per BAPI_PRODORD_CREATE ***'.
  WRITE: / 'Materiale:',     ls_ord_create_data-material.
  WRITE: / 'Divisione:',     ls_ord_create_data-plant.
  WRITE: / 'Tipo Ordine:',   ls_ord_create_data-order_type.
  WRITE: / 'Quantita'':',    ls_ord_create_data-quantity.
  WRITE: / 'Data Inizio:',   ls_ord_create_data-basic_start_date.
  WRITE: / 'Data Fine:',     ls_ord_create_data-basic_end_date.
  ULINE.

  " Chiamata alla BAPI di creazione
  CALL FUNCTION 'BAPI_PRODORD_CREATE'
    EXPORTING
      orderdata    = ls_ord_create_data
    IMPORTING
      order_number = lv_ord_num_creato
      order_type   = lv_ord_type_creato
      return       = ls_return_create.

  " Analisi del risultato
  WRITE: / '*** Messaggio da BAPI_PRODORD_CREATE ***'.
  IF ls_return_create IS NOT INITIAL.
    WRITE: / 'Tipo:', ls_return_create-type, 'ID:', ls_return_create-id, 'Nr:', ls_return_create-number.
    WRITE: '  Messaggio:', ls_return_create-message.
    IF ls_return_create-type CA 'EA'. " Errori (E) o Abbort (A)
      lv_bapi_success = gc_false.
      lv_bapi_msg_txt = ls_return_create-message.
    ENDIF.
  ELSE.
    WRITE: / 'Nessun messaggio di ritorno ricevuto dalla BAPI.'.
  ENDIF.
  ULINE.

  " Gestione transazione per la creazione
  IF lv_bapi_success = gc_false.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    IF lv_bapi_msg_txt IS INITIAL.
      lv_bapi_msg_txt = 'Errore generico durante la creazione.'.
    ENDIF.
    MESSAGE |Errore durante la creazione dell''ordine: { lv_bapi_msg_txt }| TYPE 'E'.
    EXIT. " Esce dal programma se la creazione fallisce
  ENDIF.

  " Messaggio di successo provvisorio per la creazione
  MESSAGE |Ordine di produzione { lv_ord_num_creato } creato con successo.| TYPE 'S'.
  WRITE: / 'BAPI Creazione eseguita. Numero ordine (provvisorio):', lv_ord_num_creato.
`
};
