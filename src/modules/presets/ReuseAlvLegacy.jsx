export const content = `REPORT zdemo_alv_fedele.

*---------------------------------------------------------------------*
* ATTENZIONE:
* Se la struttura non è definita in SE11 o già presente nel Dictionary,
* deve essere creata direttamente nel report principale (non in un INCLUDE).
* In caso contrario, si verifica un DUMP in fase di esecuzione.
*
* Inoltre:
* - La struttura deve essere scritta esattamente nel report principale.
* - Le righe di codice non devono superare i 72 caratteri, altrimenti si
*   verifica un DUMP.
*---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Sezione 1: DICHIARAZIONI GLOBALI
*&---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* TABELLE DI DIZIONARIO
*---------------------------------------------------------------------*
TABLES: vbrk,
        kna1.

*---------------------------------------------------------------------*
* COSTANTI GLOBALI
*---------------------------------------------------------------------*
CONSTANTS:
  gc_true  TYPE abap_bool VALUE abap_true,
  gc_false TYPE abap_bool VALUE abap_false.

*---------------------------------------------------------------------*
* TIPI DI DATI E TABELLA DATI GLOBALE
*---------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_s_dati_finali,
    ceck         TYPE c,
    importo_calc TYPE p DECIMALS 2,
    vbeln        TYPE vbrk-vbeln,
    waerk        TYPE vbrk-waerk,
    fkart        TYPE vbrk-fkart,
    fkdat        TYPE vbrk-fkdat,
    kunag        TYPE vbrk-kunag,
    name1        TYPE kna1-name1,
    ort01        TYPE kna1-ort01,
  END OF ty_s_dati_finali.

DATA: gt_dati_finali TYPE STANDARD TABLE OF ty_s_dati_finali.

*---------------------------------------------------------------------*
* SCHERMATA DI SELEZIONE
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b_criteri WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_fkart TYPE vbrk-fkart OBLIGATORY DEFAULT 'F2'.
  SELECT-OPTIONS: so_fkdat FOR vbrk-fkdat.
SELECTION-SCREEN END OF BLOCK b_criteri.

*&---------------------------------------------------------------------*
*& Sezione 2: LOGICA DI ESECUZIONE PRINCIPALE
*&---------------------------------------------------------------------*

INITIALIZATION.
  sscrfields-functxt_01 = 'Criteri di Selezione Fatture'.

START-OF-SELECTION.
  PERFORM f_seleziona_dati.

  IF gt_dati_finali IS INITIAL.
    MESSAGE 'Nessun dato trovato per i criteri di selezione inseriti.' TYPE 'S' DISPLAY LIKE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  PERFORM f_visualizza_alv_vecchio.

*&---------------------------------------------------------------------*
*& Sezione 3: FORM - ROUTINES APPLICATIVE
*&---------------------------------------------------------------------*

FORM f_seleziona_dati.

  SELECT
    vbrk~netwr AS importo_calc,
    vbrk~vbeln,
    vbrk~waerk,
    vbrk~fkart,
    vbrk~fkdat,
    vbrk~kunag,
    kna1~name1,
    kna1~ort01
  FROM vbrk
  INNER JOIN kna1 ON kna1~kunnr = vbrk~kunag
  INTO CORRESPONDING FIELDS OF TABLE @gt_dati_finali
  WHERE vbrk~fkart = @p_fkart
    AND vbrk~fkdat IN @so_fkdat.

ENDFORM.

*&---------------------------------------------------------------------*
*& Sezione 4: FORM - ROUTINE ALV (LOGICA ORIGINALE)
*&---------------------------------------------------------------------*

FORM f_visualizza_alv_vecchio.
  MESSAGE 'Esecuzione ALV con logica originale...' TYPE 'S' DISPLAY LIKE 'I'.

  DATA: lt_fcat   TYPE slis_t_fieldcat_alv,
        ls_layout TYPE slis_layout_alv.

  ls_layout-zebra      = gc_true.
  ls_layout-grid_title = 'Elenco Fatture (Logica Originale)'.

  DATA:
    BEGIN OF gt_finale OCCURS 0,
      ceck         TYPE c,
      importo_calc TYPE p DECIMALS 2,
      vbeln        LIKE vbrk-vbeln,
      waerk        LIKE vbrk-waerk,
      fkart        LIKE vbrk-fkart,
      fkdat        LIKE vbrk-fkdat,
      kunag        LIKE vbrk-kunag,
      name1        LIKE kna1-name1,
      ort01        LIKE kna1-ort01,
    END OF gt_finale.

  LOOP AT gt_dati_finali ASSIGNING FIELD-SYMBOL(<ls_originale>).
    MOVE-CORRESPONDING <ls_originale> TO gt_finale.
    APPEND gt_finale.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = sy-repid
      i_internal_tabname = 'GT_FINALE'
      i_inclname         = sy-repid
    CHANGING
      ct_fieldcat        = lt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE 'Errore: Parametri di chiamata per ALV MERGE incoerenti.' TYPE 'E'.
      WHEN 2.
        MESSAGE 'Errore: Nome tabella o struttura errato/inattivo per ALV MERGE.' TYPE 'E'.
      WHEN OTHERS.
        MESSAGE 'Errore generico durante creazione Catalogo Campi.' TYPE 'E'.
    ENDCASE.
    LEAVE PROGRAM.
  ENDIF.

  IF lt_fcat IS INITIAL.
    MESSAGE 'Errore: impossibile generare il catalogo campi per l''ALV.' TYPE 'E'.
    LEAVE PROGRAM.
  ENDIF.

  LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    CASE <fs_fcat>-fieldname.
      WHEN 'CECK'.
        <fs_fcat>-checkbox  = gc_true.
        <fs_fcat>-input     = gc_true.
        <fs_fcat>-seltext_m = 'Sel'.
      WHEN 'IMPORTO_CALC'.
        <fs_fcat>-cfieldname = 'WAERK'.
    ENDCASE.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'F_SET_PF_STATUS_VECCHIO'
      i_callback_user_command  = 'F_USER_COMMAND_VECCHIO'
      is_layout                = ls_layout
      it_fieldcat              = lt_fcat
      i_save                   = 'A'
    TABLES
      t_outtab                 = gt_finale[]
    EXCEPTIONS
      OTHERS                   = 1.

  IF sy-subrc <> 0.
    MESSAGE 'Errore visualizzazione griglia.' TYPE 'E'.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
* FORM di Callback - LOGICA ORIGINALE
*---------------------------------------------------------------------*

FORM f_set_pf_status_vecchio USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_ALV'.
ENDFORM.

FORM f_user_command_vecchio USING r_ucomm     TYPE sy-ucomm
                                  rs_selfield TYPE slis_selfield.

  IF rs_selfield-fieldname = 'CECK'.
    READ TABLE gt_dati_finali ASSIGNING FIELD-SYMBOL(<fs_riga_dati>)
                              INDEX rs_selfield-tabindex.
    IF sy-subrc = 0.
      <fs_riga_dati>-ceck = rs_selfield-value.
    ENDIF.
  ENDIF.

  CASE r_ucomm.
    WHEN '&SALVA'.
      DATA lt_sel_righe TYPE STANDARD TABLE OF ty_s_dati_finali.
      LOOP AT gt_dati_finali ASSIGNING FIELD-SYMBOL(<fs_riga>) WHERE ceck = gc_true.
        APPEND <fs_riga> TO lt_sel_righe.
      ENDLOOP.

      IF lt_sel_righe IS INITIAL.
        MESSAGE 'Nessuna riga è stata selezionata.' TYPE 'I'.
      ELSE.
        DATA(lv_num_righe) = lines( lt_sel_righe ).
        MESSAGE |Avvio elaborazione per { lv_num_righe } fatture.| TYPE 'S' DISPLAY LIKE 'I'.
        " Qui andrebbe la logica di elaborazione
      ENDIF.
      rs_selfield-refresh = gc_true.

    WHEN 'BACK' OR '%EX' OR 'RW'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
`;
