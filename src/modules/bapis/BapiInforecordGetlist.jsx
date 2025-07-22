export const bapi = {
  "name": "BAPI_INFORECORD_GETLIST",
  "description": "Elenca i Record Info Acquisto.",
  "details": [],
  "content": `
FORM f_test_bapi_leggi_lis.
  " Scopo: Elencare i Record Info Acquisto.
  DATA: lt_inforec_gen  TYPE STANDARD TABLE OF bapieina,
        ls_inforec_gen  TYPE bapieina,
        lt_inforec_acq  TYPE STANDARD TABLE OF bapieine,
        ls_inforec_acq  TYPE bapieine,
        lt_ritorno      TYPE STANDARD TABLE OF bapireturn.

  WRITE: / 'Test BAPI: BAPI_INFORECORD_GETLIST'.

  CALL FUNCTION 'BAPI_INFORECORD_GETLIST'
    EXPORTING
      vendor   = '0000100000' " SOSTITUIRE
      material = 'MATERIALE_01' " SOSTITUIRE
    TABLES
      inforecord_general  = lt_inforec_gen
      inforecord_purchorg = lt_inforec_acq
      return              = lt_ritorno.

  IF lt_inforec_gen IS NOT INITIAL.
    WRITE: / 'Record Info trovati:'.
    LOOP AT lt_inforec_gen INTO ls_inforec_gen.
      WRITE: / ls_inforec_gen-info_rec, ls_inforec_gen-vendor, ls_inforec_gen-material.
    ENDLOOP.
  ENDIF.
ENDFORM.
`
};
