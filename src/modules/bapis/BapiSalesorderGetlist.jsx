export const bapi = {
  "name": "BAPI_SALESORDER_GETLIST",
  "description": "Ottiene una lista di ordini di vendita per un cliente.",
  "details": [],
  "content": `
FORM f_test_bapi_leggi_lista.
  " Scopo: Ottenere una lista di ordini di vendita per un cliente.
  DATA: lt_ordini_vendita TYPE STANDARD TABLE OF bapiorders,
        ls_ordine_vendita TYPE bapiorders,
        ls_ritorno        TYPE bapireturn.

  WRITE: / 'Test BAPI: BAPI_SALESORDER_GETLIST'.

  CALL FUNCTION 'BAPI_SALESORDER_GETLIST'
    EXPORTING
      customer_number    = '0000000001' " SOSTITUIRE
      sales_organization = '1000'       " SOSTITUIRE
    IMPORTING
      return             = ls_ritorno
    TABLES
      sales_orders       = lt_ordini_vendita.

  IF lt_ordini_vendita IS NOT INITIAL.
    WRITE: / 'Ordini di vendita trovati:'.
    LOOP AT lt_ordini_vendita INTO ls_ordine_vendita.
      WRITE: / ls_ordine_vendita-sd_doc, ls_ordine_vendita-doc_date, ls_ordine_vendita-name.
    ENDLOOP.
  ENDIF.
ENDFORM.
`
};
