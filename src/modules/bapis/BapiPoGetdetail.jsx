export const bapi = {
  "name": "BAPI_PO_GETDETAIL",
  "description": "Legge i dati di testata e posizione di un Ordine d'Acquisto.",
  "details": [],
  "content": `
FORM f_test_bapi_leggi_det.
  " Scopo: Leggere i dati di testata e posizione di un Ordine d'Acquisto.
  DATA: ls_testata_oda TYPE bapiekkol,
        lt_posizioni   TYPE STANDARD TABLE OF bapiekpo,
        ls_posizione   TYPE bapiekpo,
        lt_ritorno     TYPE STANDARD TABLE OF bapireturn.

  WRITE: / 'Test BAPI: BAPI_PO_GETDETAIL'.

  CALL FUNCTION 'BAPI_PO_GETDETAIL'
    EXPORTING
      purchaseorder = '4500000000' " SOSTITUIRE
      items         = 'X'
    IMPORTING
      po_header     = ls_testata_oda
    TABLES
      po_items      = lt_posizioni
      return        = lt_ritorno.

  IF ls_testata_oda IS NOT INITIAL.
    WRITE: / 'Dettaglio ODA:', ls_testata_oda-po_number.
    LOOP AT lt_posizioni INTO ls_posizione.
      WRITE: / ls_posizione-po_item, ls_posizione-material, ls_posizione-quantity.
    ENDLOOP.
  ENDIF.
ENDFORM.
`
};
