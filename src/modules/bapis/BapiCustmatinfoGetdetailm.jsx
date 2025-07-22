export const bapi = {
  "name": "BAPI_CUSTMATINFO_GETDETAILM",
  "description": "Legge i dettagli di un Info Record Cliente-Materiale.",
  "details": [],
  "content": `
FORM f_test_bapi_leggi_info_cli_mat.
  " Scopo: Leggere i dettagli di un Info Record Cliente-Materiale.
  DATA: lt_dettagli_info TYPE STANDARD TABLE OF bapi_bus3033_knmt_disp,
        ls_dettaglio_info TYPE bapi_bus3033_knmt_disp,
        lt_ritorno       TYPE STANDARD TABLE OF bapiret2.

  WRITE: / 'Test BAPI: BAPI_CUSTMATINFO_GETDETAILM'.

  CALL FUNCTION 'BAPI_CUSTMATINFO_GETDETAILM'
    TABLES
      customermaterialinfodetail = lt_dettagli_info
      return                     = lt_ritorno.
  " NOTA: La firma di questa BAPI è basata su tabelle, non su parametri
  " EXPORTING. L'alimentazione dei criteri di ricerca andrebbe fatta
  " popolando la tabella 'customermaterialinfo' di tipo bapi_bus3033_boid.

  IF lt_dettagli_info IS NOT INITIAL.
    WRITE: / 'Info Record trovati:'.
    LOOP AT lt_dettagli_info INTO ls_dettaglio_info.
      "WRITE: / ls_dettaglio_info-customer, ls_dettaglio_info-cust_matl, ls_dettaglio_info-material.
    ENDLOOP.
  ENDIF.
ENDFORM.
`
};
