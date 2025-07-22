export const bapi = {
  "name": "BAPI_AR_ACC_GETSTATEMENT",
  "description": "Ottiene le partite di un conto cliente.",
  "details": [],
  "content": `
FORM f_test_bapi_leggi_est.
  " Scopo: Ottenere le partite di un conto cliente.
  DATA: lt_partite   TYPE STANDARD TABLE OF bapi3007_2,
        ls_partita   TYPE bapi3007_2,
        ls_ritorno   TYPE bapireturn.

  WRITE: / 'Test BAPI: BAPI_AR_ACC_GETSTATEMENT'.

  CALL FUNCTION 'BAPI_AR_ACC_GETSTATEMENT'
    EXPORTING
      customer    = '0000000001' " SOSTITUIRE
      companycode = '1000'       " SOSTITUIRE
      date_from   = sy-datum - 365
      date_to     = sy-datum
    IMPORTING
      return      = ls_ritorno
    TABLES
      lineitems   = lt_partite.

  IF lt_partite IS NOT INITIAL.
    WRITE: / 'Estratto Conto Cliente:'.
    LOOP AT lt_partite INTO ls_partita.
      WRITE: / ls_partita-doc_no, ls_partita-doc_date, ls_partita-amount_long.
    ENDLOOP.
  ENDIF.
ENDFORM.
`
};
