export const bapi = {
  "name": "BAPI_PO_GET_LIST",
  "description": "Elenca gli ODA per gruppo e codice di rilascio.",
  "details": [],
  "content": `
FORM f_test_bapi_leggi_lista_oda.
  " Scopo: Elencare gli ODA per gruppo e codice di rilascio.
  DATA: lt_testate_oda TYPE STANDARD TABLE OF bapiekko,
        ls_testata_oda TYPE bapiekko,
        lt_ritorno     TYPE STANDARD TABLE OF bapireturn,
         lt_posizioni_oda TYPE STANDARD TABLE OF bapiekpo.

  WRITE: / 'Test BAPI: BAPI_PO_GET_LIST'.

 CALL FUNCTION 'BAPI_PO_GET_LIST'
  EXPORTING
    rel_group = 'XX'
    rel_code  = '01'
  TABLES
    po_headers = lt_testate_oda
    po_items   = lt_posizioni_oda " <-- Aggiunto
    return     = lt_ritorno.

  IF lt_testate_oda IS NOT INITIAL.
    WRITE: / 'ODA trovati per rilascio:'.
    LOOP AT lt_testate_oda INTO ls_testata_oda.
      "WRITE: / ls_testata_oda-po_number, ls_testata_oda-creat_date, ls_testata_oda-doc_type.
    ENDLOOP.
  ENDIF.
ENDFORM.
`
};
