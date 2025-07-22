export const bapi = {
  "name": "BAPI_PLANNEDORDER_GET_DETAIL",
  "description": "Legge il dettaglio di un Ordine Pianificato.",
  "details": [],
  "content": `
FORM f_test_bapi_leggi_detta.
  " Scopo: Leggere il dettaglio di un Ordine Pianificato.
  DATA: ls_testata    TYPE bapiplaf_e1,
        lt_componenti TYPE STANDARD TABLE OF bapi_pldordcomp_e1,
        ls_componente TYPE bapi_pldordcomp_e1,
        ls_ritorno    TYPE bapireturn1.

  WRITE: / 'Test BAPI: BAPI_PLANNEDORDER_GET_DETAIL'.

  CALL FUNCTION 'BAPI_PLANNEDORDER_GET_DETAIL'
    EXPORTING
      plannedorder = '0000100000' " SOSTITUIRE
    IMPORTING
      return       = ls_ritorno
      headerdata   = ls_testata
    TABLES
      componentsdata = lt_componenti.

  IF ls_testata IS NOT INITIAL.
    "WRITE: / 'Dettaglio Ordine Pianificato:', ls_testata-planned_order_num.
    LOOP AT lt_componenti INTO ls_componente.
      "WRITE: / ls_componente-reserv_item, ls_componente-component, ls_componente-requ_quan.
    ENDLOOP.
  ENDIF.
ENDFORM.
`
};
