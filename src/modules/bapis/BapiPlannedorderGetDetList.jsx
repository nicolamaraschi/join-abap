export const bapi = {
  "name": "BAPI_PLANNEDORDER_GET_DET_LIST",
  "description": "Elenca gli Ordini Pianificati.",
  "details": [],
  "content": `
FORM f_test_bapi_leggi_li.
  " Scopo: Elencare gli Ordini Pianificati.
  DATA: ls_criteri_sel TYPE bapiplaf_i3,
        lt_lista_dett  TYPE STANDARD TABLE OF bapiplaf_e1,
        ls_dettaglio   TYPE bapiplaf_e1,
        ls_ritorno     TYPE bapireturn1.

  WRITE: / 'Test BAPI: BAPI_PLANNEDORDER_GET_DET_LIST'.

  ls_criteri_sel-material_long = 'MATERIALE_PP_01'. " SOSTITUIRE

  CALL FUNCTION 'BAPI_PLANNEDORDER_GET_DET_LIST'
    EXPORTING
      selectioncriteria = ls_criteri_sel
    IMPORTING
      return            = ls_ritorno
    TABLES
      detailedlist      = lt_lista_dett.

  IF lt_lista_dett IS NOT INITIAL.
    WRITE: / 'Ordini pianificati trovati:'.
    LOOP AT lt_lista_dett INTO ls_dettaglio.
     " WRITE: / ls_dettaglio-planned_order_num, ls_dettaglio-material.
    ENDLOOP.
  ENDIF.
ENDFORM.
`
};
