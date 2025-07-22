export const bapi = {
  "name": "BAPI_REQUISITION_GETITEMS",
  "description": "Legge le posizioni di una Richiesta d'Acquisto.",
  "details": [],
  "content": `
FORM f_test_bapi_leggi_pos_rda.
  " Scopo: Leggere le posizioni di una Richiesta d'Acquisto.
  DATA: lt_posizioni_rda TYPE STANDARD TABLE OF bapiebanc,
        ls_posizione_rda TYPE bapiebanc,
        lt_ritorno       TYPE STANDARD TABLE OF bapireturn.

  WRITE: / 'Test BAPI: BAPI_REQUISITION_GETITEMS'.

  CALL FUNCTION 'BAPI_REQUISITION_GETITEMS'
    EXPORTING
      preq_no = '0010000000' " SOSTITUIRE
    TABLES
      requisition_items = lt_posizioni_rda
      return            = lt_ritorno.

  IF lt_posizioni_rda IS NOT INITIAL.
    WRITE: / 'Posizioni RdA trovate:'.
    LOOP AT lt_posizioni_rda INTO ls_posizione_rda.
      WRITE: / ls_posizione_rda-preq_item, ls_posizione_rda-material, ls_posizione_rda-quantity.
    ENDLOOP.
  ENDIF.
ENDFORM.
`
};
