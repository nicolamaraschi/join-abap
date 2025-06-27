export const content = `REPORT zdemo_popup_advanced.

TABLES: sval.
DATA: gt_campi_popup    TYPE STANDARD TABLE OF sval,
      gv_codice_ritorno TYPE c.

START-OF-SELECTION.
  PERFORM f_prepara_campi_avanzati_popup.
  PERFORM f_chiama_function_module_popup.
  PERFORM f_processa_risultato_popup.

"&---------------------------------------------------------------------*
"&      Form  f_prepara_campi_avanzati_popup
"&---------------------------------------------------------------------*
FORM f_prepara_campi_avanzati_popup.
  DATA: ls_campo TYPE sval.
  REFRESH gt_campi_popup.

  " --- Campo 1: Campo Obbligatorio (MARA-MATNR) ---
  CLEAR ls_campo.
  ls_campo-tabname    = 'MARA'.
  ls_campo-fieldname  = 'MATNR'.
  ls_campo-fieldtext  = 'Codice Materiale'.
  ls_campo-field_obl  = 'X'.
  APPEND ls_campo TO gt_campi_popup.

  " --- Campo 2: Sola Visualizzazione (usando un campo DDIC come base) ---
  CLEAR ls_campo.
  ls_campo-tabname    = 'KNA1'.
  ls_campo-fieldname  = 'NAME1'. " Usiamo un campo nome come base
  ls_campo-fieldtext  = 'Utente Corrente (Display)'.
  ls_campo-value      = sy-uname.
  ls_campo-field_attr = '01'. " '01' = Output only
  APPEND ls_campo TO gt_campi_popup.

  " --- Campo 3: Campo con F4 disattivato ---
  CLEAR ls_campo.
  ls_campo-tabname    = 'T001'.
  ls_campo-fieldname  = 'BUKRS'.
  ls_campo-fieldtext  = 'Società (no F4)'.
  ls_campo-novaluehlp = 'X'.
  APPEND ls_campo TO gt_campi_popup.

  " --- Campo 4: Campo Data ---
  CLEAR ls_campo.
  ls_campo-tabname   = 'VBAK'.
  ls_campo-fieldname = 'ERDAT'.
  ls_campo-fieldtext = 'Data Documento'.
  ls_campo-value     = sy-datum.
  APPEND ls_campo TO gt_campi_popup.
ENDFORM.

"&---------------------------------------------------------------------*
"&      Form  f_chiama_function_module_popup
"&---------------------------------------------------------------------*
FORM f_chiama_function_module_popup.
  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = 'Test Attributi Avanzati'
    IMPORTING
      returncode      = gv_codice_ritorno
    TABLES
      fields          = gt_campi_popup
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.
ENDFORM.

"&---------------------------------------------------------------------*
"&      Form  f_processa_risultato_popup
"&---------------------------------------------------------------------*
FORM f_processa_risultato_popup.
  DATA: ls_campo_letto TYPE sval.
  IF gv_codice_ritorno = 'C'.
    WRITE: / 'L''utente ha confermato. Valori inseriti:'.
    ULINE.
    LOOP AT gt_campi_popup INTO ls_campo_letto.
      WRITE: / ls_campo_letto-fieldtext, ':', ls_campo_letto-value.
    ENDLOOP.
  ELSEIF gv_codice_ritorno = 'A'.
    MESSAGE 'Operazione annullata dall''utente.' TYPE 'S' DISPLAY LIKE 'I'.
  ENDIF.
ENDFORM.`;