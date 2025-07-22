export const bapi = {
  "name": "BAPI_PO_GETITEMSREL",
  "description": "Legge i rilasci (per contratti quadro) di un ODA.",
  "details": [],
  "content": `
FORM f_test_bapi_leggi_rilasci_oda.
  " Scopo: Leggere i rilasci (per contratti quadro) di un ODA.
  DATA: lt_rilasci TYPE STANDARD TABLE OF bapiekpoc,
        ls_rilascio TYPE bapiekpoc,
        lt_ritorno   TYPE STANDARD TABLE OF bapireturn,
         lt_testate_oda TYPE STANDARD TABLE OF bapiekkol.

  WRITE: / 'Test BAPI: BAPI_PO_GETITEMSREL'.

  CALL FUNCTION 'BAPI_PO_GETITEMSREL'
  EXPORTING
    rel_group = 'XX'
    rel_code  = '01'
  TABLES
    po_headers = lt_testate_oda " <-- Aggiunto
    po_items   = lt_rilasci
    return     = lt_ritorno.

  IF lt_rilasci IS NOT INITIAL.
    WRITE: / 'Rilasci trovati:'.
    LOOP AT lt_rilasci INTO ls_rilascio.
     " WRITE: / ls_rilascio-po_number, ls_rilascio-doc_date, ls_rilascio-short_text.
    ENDLOOP.
  ENDIF.
ENDFORM.
`
};
