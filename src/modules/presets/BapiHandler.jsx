export const content = `"------------------------------
" DEFINIZIONE VARIABILI GENERICHE
"------------------------------
DATA: lt_return         TYPE STANDARD TABLE OF bapiret2,
      ls_return         TYPE bapiret2,
      lv_bapi_success   TYPE abap_bool VALUE abap_true,
      lv_bapi_message   TYPE string.

FIELD-SYMBOLS: <fs_return> TYPE bapiret2.

"------------------------------
" CHIAMATA BAPI GENERICA
"------------------------------
CALL FUNCTION 'BAPI_WHATEVER_YOU_NEED'   " <--- sostituire con la BAPI vera
  EXPORTING
    param1          = value1
    param2          = value2
  IMPORTING
    result_struct   = ls_result    " (se la BAPI ha IMPORTING)
  TABLES
    return          = lt_return.

"------------------------------
" ANALISI RETURN
"------------------------------
LOOP AT lt_return ASSIGNING <fs_return> WHERE type CA 'EA'.
  lv_bapi_success = abap_false.
  WRITE: / '️  Messaggio:', <fs_return>-message.
ENDLOOP.

"------------------------------
" GESTIONE COMMIT / ROLLBACK
"------------------------------
IF lv_bapi_success = abap_false.

  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  " Recupera il messaggio principale da RETURN
  READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
  IF sy-subrc <> 0.
    READ TABLE lt_return INTO ls_return WITH KEY type = 'A'.
  ENDIF.
  IF sy-subrc = 0.
    lv_bapi_message = ls_return-message.
  ELSE.
    READ TABLE lt_return INTO ls_return INDEX 1.
    IF sy-subrc = 0.
      lv_bapi_message = ls_return-message.
    ENDIF.
  ENDIF.

  WRITE: / ' Errore nella BAPI:', lv_bapi_message.

ELSE.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

  WRITE: / ' BAPI eseguita con successo.'.

ENDIF.`;