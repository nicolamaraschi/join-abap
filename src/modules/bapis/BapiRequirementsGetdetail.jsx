export const bapi = {
  "name": "BAPI_REQUIREMENTS_GETDETAIL",
  "description": "Legge il dettaglio di un Fabbisogno Indipendente (PIR).",
  "details": [],
  "content": `
FORM f_test_bapi_leggi_de.
  " Scopo: Leggere il dettaglio di un Fabbisogno Indipendente (PIR).
  DATA: lt_fabbisogni TYPE STANDARD TABLE OF bapisitmeo,
        ls_fabbogno   TYPE bapisitmeo,
        lt_ritorno    TYPE STANDARD TABLE OF bapireturn1.

  WRITE: / 'Test BAPI: BAPI_REQUIREMENTS_GETDETAIL'.

  CALL FUNCTION 'BAPI_REQUIREMENTS_GETDETAIL'
    EXPORTING
      plant             = '1000'          " SOSTITUIRE
      requirementstype  = 'LSF'           " SOSTITUIRE
      version           = '00'            " SOSTITUIRE
      reqmtsplannumber  = 'PIANO_01'      " SOSTITUIRE
      material_long     = 'MATERIALE_PP_01' " SOSTITUIRE
    TABLES
      requirements_out = lt_fabbisogni
      return           = lt_ritorno.

  IF lt_fabbisogni IS NOT INITIAL.
    WRITE: / 'Dettaglio PIR per materiale trovato:'.
    LOOP AT lt_fabbisogni INTO ls_fabbogno.
      "WRITE: / ls_fabbogno-requ_date, ls_fabbogno-requ_qty.
    ENDLOOP.
  ENDIF.
ENDFORM.
`
};
