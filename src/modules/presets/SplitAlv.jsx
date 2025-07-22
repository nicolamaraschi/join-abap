export const SplitAlv = `
EPORT z_report_alv_splitter.

"**********************************************************************
"* DICHIARAZIONI GLOBALI
"**********************************************************************
TABLES: vbak.

TYPES:
  BEGIN OF ty_s_testata_ordine,
    vbeln TYPE vbak-vbeln,
    audat TYPE vbak-audat,
    kunnr TYPE vbak-kunnr,
    name1 TYPE kna1-name1,
  END OF ty_s_testata_ordine,

  BEGIN OF ty_s_posizione_ordine,
    posnr  TYPE vbap-posnr,
    matnr  TYPE vbap-matnr,
    arktx  TYPE vbap-arktx,
    kwmeng TYPE vbap-kwmeng,
    vrkme  TYPE vbap-vrkme,
  END OF ty_s_posizione_ordine.

DATA:
  gt_testate_ordini   TYPE STANDARD TABLE OF ty_s_testata_ordine,
  gt_posizioni_ordine TYPE STANDARD TABLE OF ty_s_posizione_ordine.

DATA:
  go_splitter_principale TYPE REF TO cl_gui_splitter_container,
  go_contenitore_sopra   TYPE REF TO cl_gui_container,
  go_contenitore_sotto   TYPE REF TO cl_gui_container,
  go_alv_sopra           TYPE REF TO cl_gui_alv_grid,
  go_alv_sotto           TYPE REF TO cl_gui_alv_grid.

"----------------------------------------------------------------------
" CLASSE LOCALE PER LA GESTIONE DEGLI EVENTI
"----------------------------------------------------------------------
CLASS lcl_gestore_eventi DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_double_click
        FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.
ENDCLASS.

CLASS lcl_gestore_eventi IMPLEMENTATION.
  METHOD handle_double_click.
    PERFORM f_popola_alv_sottostante USING e_row-index.
  ENDMETHOD.
ENDCLASS.

DATA: go_gestore_eventi TYPE REF TO lcl_gestore_eventi.

"----------------------------------------------------------------------
" SCHERMATA DI SELEZIONE
"----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b_criteri WITH FRAME TITLE TEXT-001. " Criteri di Selezione
  SELECT-OPTIONS: so_vbeln FOR vbak-vbeln,
                  so_audat FOR vbak-audat.
SELECTION-SCREEN END OF BLOCK b_criteri.

"**********************************************************************
"* BLOCCO DI ELABORAZIONE PRINCIPALE
"**********************************************************************
START-OF-SELECTION.
  PERFORM f_seleziona_dati_testata.

  IF gt_testate_ordini IS INITIAL.
    MESSAGE 'Nessun ordine trovato per i criteri specificati.' TYPE 'S' DISPLAY LIKE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  CALL SCREEN 100.

"**********************************************************************
"* MODULI DI GESTIONE DYNPRO 100
"**********************************************************************
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_ALV'.
  SET TITLEBAR 'TIT_0100'.

  IF go_splitter_principale IS NOT BOUND.
    PERFORM f_visualizza_alv_splitter.
  ENDIF.
ENDMODULE.

MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      IF go_splitter_principale IS BOUND.
        go_splitter_principale->free( ).
        CLEAR: go_splitter_principale, go_contenitore_sopra, go_contenitore_sotto,
               go_alv_sopra, go_alv_sotto, go_gestore_eventi.
      ENDIF.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

"**********************************************************************
"* ROUTINES APPLICATIVE (FORMS)
"**********************************************************************

"&---------------------------------------------------------------------*
"&      Form  f_seleziona_dati_testata
"&---------------------------------------------------------------------*
FORM f_seleziona_dati_testata.
  SELECT vbak~vbeln,
         vbak~audat,
         vbak~kunnr,
         kna1~name1
    FROM vbak
    INNER JOIN kna1 ON vbak~kunnr = kna1~kunnr
    WHERE vbak~vbeln IN @so_vbeln
      AND vbak~audat IN @so_audat
    INTO CORRESPONDING FIELDS OF TABLE @gt_testate_ordini
    UP TO 100 ROWS.
ENDFORM.

"&---------------------------------------------------------------------*
"&      Form  f_visualizza_alv_splitter
"&---------------------------------------------------------------------*
FORM f_visualizza_alv_splitter.
  DATA: ls_layout         TYPE lvc_s_layo,
        lt_fcat_testata   TYPE lvc_t_fcat,
        lt_fcat_posizione TYPE lvc_t_fcat.

  " *** PASSO 1: Costruire manualmente i cataloghi campi ***
  PERFORM f_costruisci_testata CHANGING lt_fcat_testata.
  PERFORM f_costruisci CHANGING lt_fcat_posizione.

  " *** PASSO 2: Creare i contenitori GUI ***
  CREATE OBJECT go_splitter_principale
    EXPORTING parent = cl_gui_container=>default_screen rows = 2 columns = 1.

  go_contenitore_sopra = go_splitter_principale->get_container( row = 1 column = 1 ).
  go_contenitore_sotto = go_splitter_principale->get_container( row = 2 column = 1 ).

  " *** PASSO 3: Creare e visualizzare l'ALV SUPERIORE ***
  CREATE OBJECT go_alv_sopra EXPORTING i_parent = go_contenitore_sopra.
  CREATE OBJECT go_gestore_eventi.
  SET HANDLER go_gestore_eventi->handle_double_click FOR go_alv_sopra.

  ls_layout-zebra = 'X'.
  ls_layout-sel_mode = 'A'.
  ls_layout-grid_title = 'Elenco Ordini di Vendita (Doppio Click per Dettagli)'.

  CALL METHOD go_alv_sopra->set_table_for_first_display
    EXPORTING
      is_layout       = ls_layout
    CHANGING
      it_outtab       = gt_testate_ordini
      it_fieldcatalog = lt_fcat_testata.

  " *** PASSO 4: Creare e visualizzare l'ALV INFERIORE ***
  CREATE OBJECT go_alv_sotto EXPORTING i_parent = go_contenitore_sotto.

  CLEAR ls_layout.
  ls_layout-zebra = 'X'.
  ls_layout-sel_mode = 'A'.
  ls_layout-grid_title = 'Dettaglio Posizioni Ordine'.

  CALL METHOD go_alv_sotto->set_table_for_first_display
    EXPORTING
      is_layout       = ls_layout
    CHANGING
      it_outtab       = gt_posizioni_ordine
      it_fieldcatalog = lt_fcat_posizione.

ENDFORM.

"&---------------------------------------------------------------------*
"&      Form  f_popola_alv_sottostante
"&---------------------------------------------------------------------*
FORM f_popola_alv_sottostante USING iv_indice_riga TYPE lvc_index.
  DATA: ls_testata_selezionata TYPE ty_s_testata_ordine,
        lv_nuovo_titolo        TYPE lvc_title.

  READ TABLE gt_testate_ordini INTO ls_testata_selezionata INDEX iv_indice_riga.
  IF sy-subrc <> 0. RETURN. ENDIF.

  REFRESH gt_posizioni_ordine.
  SELECT posnr, matnr, arktx, kwmeng, vrkme
    FROM vbap
    WHERE vbeln = @ls_testata_selezionata-vbeln
    INTO CORRESPONDING FIELDS OF TABLE @gt_posizioni_ordine.

  CONCATENATE 'Posizioni per l''ordine:' ls_testata_selezionata-vbeln
    INTO lv_nuovo_titolo SEPARATED BY space.
  CALL METHOD go_alv_sotto->set_gridtitle EXPORTING i_gridtitle = lv_nuovo_titolo.
  CALL METHOD go_alv_sotto->refresh_table_display.
ENDFORM
`;
