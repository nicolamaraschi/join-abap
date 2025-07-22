export const bapi = {
  "name": "BAPI_BILLINGDOC_CREATEFROMDATA",
  "description": "Crea un documento di fatturazione da dati di riferimento.",
  "details": [],
  "content": `
FORM f_test_bapi_crea_fattura.
  " Scopo: Creare un documento di fatturazione da dati di riferimento.
  DATA: lt_dati_fattura TYPE STANDARD TABLE OF bapivbrk,
        ls_dati_fattura TYPE bapivbrk,
        lt_ritorno      TYPE STANDARD TABLE OF bapireturn1,
        ls_ritorno      TYPE bapireturn1,
        lv_messaggio    TYPE string,
        lv_errore       TYPE abap_bool,
        lt_dati_condizione   TYPE STANDARD TABLE OF bapikomv,
        lt_dati_carta_cred TYPE STANDARD TABLE OF bapiccard_vf.


  WRITE: / 'Test BAPI: BAPI_BILLINGDOC_CREATEFROMDATA'.

  " Dati di input: SOSTITUIRE con dati validi
  CLEAR ls_dati_fattura.
  "ls_dati_fattura-p_suc_doc  = '1'.
  ls_dati_fattura-ref_doc    = '0080000000'.
  ls_dati_fattura-ref_item   = '000010'.
  ls_dati_fattura-req_qty = '10.000'.
  ls_dati_fattura-sales_unit = 'PZ'.
  ls_dati_fattura-material   = 'MATERIALE_01'.
  ls_dati_fattura-price_date = sy-datum.
  ls_dati_fattura-bill_date  = sy-datum.
  APPEND ls_dati_fattura TO lt_dati_fattura.

CALL FUNCTION 'BAPI_BILLINGDOC_CREATEFROMDATA'
  TABLES
    billing_data_in   = lt_dati_fattura
    condition_data_in = lt_dati_condizione   " <-- Aggiunto
    ccard_data_in     = lt_dati_carta_cred   " <-- Aggiunto
    returnlog_out     = lt_ritorno.

  LOOP AT lt_ritorno INTO ls_ritorno WHERE type CA 'EA'.
    lv_errore = abap_true.
    lv_messaggio = ls_ritorno-message.
    EXIT.
  ENDLOOP.

  IF lv_errore = abap_true.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    WRITE: / 'Errore creazione fattura:', lv_messaggio.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
    READ TABLE lt_ritorno INTO ls_ritorno WITH KEY id = 'VF' number = '311'.
    IF sy-subrc = 0.
      WRITE: / 'Creazione eseguita. Documento:', ls_ritorno-message_v1.
    ELSE.
      WRITE: / 'Creazione eseguita (COMMIT), ma numero doc non trovato.'.
    ENDIF.
  ENDIF.
ENDFORM.
`
};
