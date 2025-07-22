export const bapi = {
  "name": "BAPI_PO_GETITEMS",
  "description": "Legge solo le posizioni di un Ordine d'Acquisto.",
  "details": [],
  "content": `
FORM f_test_bapi_leggi_pos_oda.
  " Scopo: Leggere solo le posizioni di un Ordine d'Acquisto.
  DATA: lt_posizioni TYPE STANDARD TABLE OF bapiekpoc,
        ls_posizione TYPE bapiekpoc,
        lt_ritorno   TYPE STANDARD TABLE OF bapireturn.

  WRITE: / 'Test BAPI: BAPI_PO_GETITEMS'.

  CALL FUNCTION 'BAPI_PO_GETITEMS'
    EXPORTING
      purchaseorder = '4500000000' " SOSTITUIRE
    TABLES
      po_items      = lt_posizioni
      return        = lt_ritorno.

  IF lt_posizioni IS NOT INITIAL.
    WRITE: / 'Posizioni ODA trovate:'.
    LOOP AT lt_posizioni INTO ls_posizione.
      "WRITE: / ls_posizione-po_item, ls_posizione-material, ls_posizione-quantity.
    ENDLOOP.
  ENDIF.
ENDFORM.
`
};
