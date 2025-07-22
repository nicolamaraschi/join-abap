export const bapi = {
  "name": "BAPI_BILLINGDOC_CANCEL1",
  "description": "Annulla un documento di fatturazione esistente.",
  "details": [],
  "content": `
FORM f_test_bapi_annulla_fattura.
  " Scopo: Annullare un documento di fatturazione esistente.

  DATA: lv_fattura   TYPE bapivbrksuccess-bill_doc,
        lt_ritorno   TYPE STANDARD TABLE OF bapireturn1,
        ls_ritorno   TYPE bapireturn1,
        lt_successo  TYPE STANDARD TABLE OF bapivbrksuccess,
        ls_successo  TYPE bapivbrksuccess,
        lv_messaggio TYPE string,
        lv_errore    TYPE abap_bool.

  " Dati di input: SOSTITUIRE con un numero di fattura valido
  lv_fattura = '0090000000'.

  WRITE: / 'Test BAPI: BAPI_BILLINGDOC_CANCEL1'.
  WRITE: / 'Azione: Annullamento fattura', lv_fattura.

  CALL FUNCTION 'BAPI_BILLINGDOC_CANCEL1'
    EXPORTING
      billingdocument = lv_fattura
    TABLES
      return          = lt_ritorno
      success         = lt_successo.

  LOOP AT lt_ritorno INTO ls_ritorno WHERE type CA 'EA'.
    lv_errore = abap_true.
    lv_messaggio = ls_ritorno-message.
    EXIT.
  ENDLOOP.

  IF lv_errore = abap_true.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    WRITE: / 'Errore durante lannullamento:', lv_messaggio.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
    WRITE: / 'Annullamento eseguito con successo.'.
    LOOP AT lt_successo INTO ls_successo.
      WRITE: / '-> Documento di annullamento creato:', ls_successo-bill_doc.
    ENDLOOP.
  ENDIF.
ENDFORM.
`
};
