export const content = `REPORT Z_POPUP_CLEAN.

"**********************************************************************
"* REPORT Z_POPUP_CLEAN
"* DESCRIZIONE:
"* Report di test avanzato che dimostra l'integrazione tra le classi
"* CL_GUI_ALV_TREE e CL_GUI_ALV_GRID.
"*
"* FUNZIONALITÀ IMPLEMENTATE:
"*
"* 1.  **Layout Schermo Diviso (Splitter)**:
"*     - Utilizzo di CL_GUI_SPLITTER_CONTAINER per dividere la dynpro
"*       in due sezioni: albero a sinistra e griglia a destra.
"*
"* 2.  **Albero Gerarchico (CL_GUI_ALV_TREE)**:
"*     - Visualizzazione delle compagnie aeree (CARRID) come nodi
"*       principali in una struttura ad albero.
"*     - Gestione dell'evento NODE_DOUBLE_CLICK per catturare la
"*       selezione di un nodo da parte dell'utente.
"*
"* 3.  **Griglia ALV (CL_GUI_ALV_GRID)**:
"*     - Visualizzazione dei dati dei voli (tabella SFLIGHT).
"*     - La griglia viene aggiornata dinamicamente in base al nodo
"*       selezionato nell'albero.
"*     - Catalogo campi personalizzato (intestazioni, hotspot).
"*     - Ordinamento iniziale con subtotali (per Compagnia e Connessione).
"*     - Griglia editabile per il campo 'Posti Occupati'.
"*
"* 4.  **Gestione Avanzata degli Eventi (Classe Locale)**:
"*     - Gestione centralizzata di tutti gli eventi (albero e griglia)
"*       tramite una classe locale (LCL_GESTORE_EVENTI).
"*     - HOTSPOT_CLICK: Messaggio informativo al clic sulla compagnia aerea.
"*     - TOOLBAR: Aggiunta di pulsanti custom ('Elabora', 'Salva').
"*     - USER_COMMAND: Logica per i pulsanti custom.
"*     - DATA_CHANGED: Validazione in tempo reale dei dati inseriti
"*       nella cella editabile.
"*
"* 5.  **Interattività e Usabilità**:
"*     - Selezione multipla delle righe nella griglia.
"*     - Logica di elaborazione per le righe selezionate.
"*     - Schema di salvataggio sicuro per la griglia editabile.
"**********************************************************************

"----------------------------------------------------------------------
" DICHIARAZIONI GLOBALI
"----------------------------------------------------------------------
" Tipi locali per ALV Tree (sostituisce TYPE-POOLS slist)
TYPES: BEGIN OF lty_treenode,
         node_key   TYPE tv_nodekey,
         relatkey   TYPE tv_nodekey,
         relatship  TYPE i,
         text       TYPE text30,
         isfolder   TYPE c,
         expanded   TYPE c,
       END OF lty_treenode.
TYPES: lty_treenodetab TYPE STANDARD TABLE OF lty_treenode WITH DEFAULT KEY.

DATA: go_contenitore_principale TYPE REF TO cl_gui_custom_container,
      go_splitter             TYPE REF TO cl_gui_splitter_container,
      go_contenitore_albero   TYPE REF TO cl_gui_container,
      go_contenitore_griglia  TYPE REF TO cl_gui_container,
      go_albero_alv           TYPE REF TO cl_gui_alv_tree,
      go_griglia_alv          TYPE REF TO cl_gui_alv_grid,
      gv_nome_control_custom  TYPE scrfname VALUE 'CONTENITORE_ALV'.

DATA: gt_dati_voli      TYPE STANDARD TABLE OF sflight,
      gt_dati_voli_filtrati TYPE STANDARD TABLE OF sflight.

" Strutture per ALV Tree
DATA: gt_gerarchia_nodi TYPE lty_treenodetab.

" Strutture per ALV Grid
DATA: gt_catalogo_campi        TYPE lvc_t_fcat,
      gs_layout                TYPE lvc_s_layo,
      gs_variante              TYPE disvariant,
      gt_criteri_ord_iniziale  TYPE lvc_t_sort,
      gs_criterio_ord_iniziale TYPE lvc_s_sort.

"----------------------------------------------------------------------
" CLASSE PER LA GESTIONE DEGLI EVENTI
"----------------------------------------------------------------------
CLASS lcl_gestore_eventi DEFINITION.
  PUBLIC SECTION.
    METHODS:
      " Eventi per ALV Grid
      handle_hotspot_click
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id,
      handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,
      handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      handle_data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed,
      " Eventi per ALV Tree
      handle_node_double_click
        FOR EVENT node_double_click OF cl_gui_alv_tree
        IMPORTING node_key.
ENDCLASS.

DATA: go_gestore_eventi TYPE REF TO lcl_gestore_eventi.

"----------------------------------------------------------------------
" BLOCCO DI ELABORAZIONE PRINCIPALE
"----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM f_seleziona_dati.
  IF gt_dati_voli IS NOT INITIAL.
    CALL SCREEN 100.
  ELSE.
    MESSAGE TEXT-001 TYPE 'S' DISPLAY LIKE 'I'. " Nessun dato di test trovato.
  ENDIF.

"----------------------------------------------------------------------
" MODULI DI GESTIONE DYNPRO 100
"----------------------------------------------------------------------
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'TIT_0100' WITH TEXT-002. " Report ALV Tree/Grid Integrato
  PERFORM f_visualizza_controlli.
ENDMODULE.

MODULE user_command_0100 INPUT.
  CALL METHOD cl_gui_cfw=>dispatch.
  CASE sy-ucomm.
    WHEN '&F03' OR 'BACK' OR '&F15' OR 'EXIT' OR '&F12' OR 'CANCEL'.
      " Free di tutte le risorse GUI
      IF go_griglia_alv IS BOUND. go_griglia_alv->free( ). CLEAR go_griglia_alv. ENDIF.
      IF go_albero_alv IS BOUND. go_albero_alv->free( ). CLEAR go_albero_alv. ENDIF.
      IF go_splitter IS BOUND. go_splitter->free( ). CLEAR go_splitter. ENDIF.
      IF go_contenitore_principale IS BOUND. go_contenitore_principale->free( ). CLEAR go_contenitore_principale. ENDIF.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

"----------------------------------------------------------------------
" SUBROUTINES (FORMS)
"----------------------------------------------------------------------
FORM f_seleziona_dati.
  SELECT * FROM sflight INTO TABLE @gt_dati_voli UP TO 100 ROWS.
  IF sy-subrc <> 0. CLEAR gt_dati_voli. ENDIF.
ENDFORM.

FORM f_costruisci_gerarchia_albero.
  DATA: ls_nodo TYPE lty_treenode,
        lt_compagnie_univoche TYPE STANDARD TABLE OF s_carr_id,
        lv_chiave_nodo TYPE tv_nodekey.

  CLEAR gt_gerarchia_nodi.
  lt_compagnie_univoche = VALUE #( FOR wa IN gt_dati_voli ( wa-carrid ) ).
  SORT lt_compagnie_univoche.
  DELETE ADJACENT DUPLICATES FROM lt_compagnie_univoche.

  LOOP AT lt_compagnie_univoche INTO DATA(lv_carrid).
    CLEAR ls_nodo.
    lv_chiave_nodo = lv_carrid.
    ls_nodo-node_key = lv_chiave_nodo.
    ls_nodo-text = |{ lv_carrid }|.
    ls_nodo-isfolder = abap_true.
    ls_nodo-expanded = abap_false.
    APPEND ls_nodo TO gt_gerarchia_nodi.
  ENDLOOP.
ENDFORM.

FORM f_visualizza_controlli.
  IF go_contenitore_principale IS NOT BOUND.
    " 1. Crea il contenitore principale e lo splitter
    CREATE OBJECT go_contenitore_principale
      EXPORTING container_name = gv_nome_control_custom EXCEPTIONS OTHERS = 1.
    IF sy-subrc <> 0. MESSAGE TEXT-006 TYPE 'E'. LEAVE PROGRAM. ENDIF.

    CREATE OBJECT go_splitter
      EXPORTING parent = go_contenitore_principale rows = 1 columns = 2.
    CALL METHOD go_splitter->set_column_width EXPORTING id = 1 width = 30. " 30% per l'albero
    CALL METHOD go_splitter->set_column_width EXPORTING id = 2 width = 70. " 70% per la griglia

    go_contenitore_albero = go_splitter->get_container( row = 1 column = 1 ).
    go_contenitore_griglia = go_splitter->get_container( row = 1 column = 2 ).

    " 2. Crea l'istanza del gestore eventi
    CREATE OBJECT go_gestore_eventi.

    " 3. Prepara e visualizza l'ALBERO
    PERFORM f_costruisci_gerarchia_albero.
    CREATE OBJECT go_albero_alv
      EXPORTING
        parent              = go_contenitore_albero
        node_selection_mode = 1.
    SET HANDLER go_gestore_eventi->handle_node_double_click FOR go_albero_alv.
    CALL METHOD go_albero_alv->set_table_for_first_display
      EXPORTING i_structure_name = 'LTY_TREENODE'
      CHANGING it_outtab = gt_gerarchia_nodi.

    " 4. Prepara e visualizza la GRIGLIA
    CREATE OBJECT go_griglia_alv
      EXPORTING i_parent = go_contenitore_griglia.
    SET HANDLER go_gestore_eventi->handle_hotspot_click FOR go_griglia_alv.
    SET HANDLER go_gestore_eventi->handle_toolbar FOR go_griglia_alv.
    SET HANDLER go_gestore_eventi->handle_user_command FOR go_griglia_alv.
    SET HANDLER go_gestore_eventi->handle_data_changed FOR go_griglia_alv.
    CALL METHOD go_griglia_alv->register_edit_event
      EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    PERFORM f_prep_cat_campi_grid.
    PERFORM f_imposta_layout_griglia.
    PERFORM f_imposta_ordinamento_iniziale.

    " Visualizzazione iniziale con tutti i dati
    gt_dati_voli_filtrati = gt_dati_voli.
    CALL METHOD go_griglia_alv->set_toolbar_interactive.
    CALL METHOD go_griglia_alv->set_table_for_first_display
      EXPORTING is_layout = gs_layout is_variant = gs_variante i_save = 'A'
      CHANGING  it_outtab = gt_dati_voli_filtrati it_fieldcatalog = gt_catalogo_campi
                it_sort = gt_criteri_ord_iniziale.
  ENDIF.
ENDFORM.

FORM f_prep_cat_campi_grid.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING i_structure_name = 'SFLIGHT'
    CHANGING ct_fieldcat = gt_catalogo_campi
    EXCEPTIONS OTHERS = 1.
  IF sy-subrc <> 0. MESSAGE TEXT-008 TYPE 'E'. LEAVE PROGRAM. ENDIF.

  FIELD-SYMBOLS: <fs_campo_cat> TYPE lvc_s_fcat.
  LOOP AT gt_catalogo_campi ASSIGNING <fs_campo_cat>.
    CASE <fs_campo_cat>-fieldname.
      WHEN 'CARRID'.   <fs_campo_cat>-coltext = TEXT-009. <fs_campo_cat>-hotspot = abap_true.
      WHEN 'CONNID'.   <fs_campo_cat>-coltext = TEXT-010.
      WHEN 'FLDATE'.   <fs_campo_cat>-coltext = TEXT-011.
      WHEN 'PRICE'.    <fs_campo_cat>-coltext = TEXT-012. <fs_campo_cat>-do_sum = abap_true.
      WHEN 'CURRENCY'. <fs_campo_cat>-coltext = TEXT-013.
      WHEN 'PLANETYPE'.<fs_campo_cat>-coltext = TEXT-014.
      WHEN 'SEATSOCC'.
        <fs_campo_cat>-coltext  = TEXT-015.
        <fs_campo_cat>-edit     = abap_true.
    ENDCASE.
  ENDLOOP.
ENDFORM.

FORM f_imposta_layout_griglia.
  gs_layout-sel_mode   = 'A'.
  gs_layout-zebra      = abap_true.
  gs_layout-cwidth_opt = abap_true.
  gs_layout-grid_title = TEXT-016. " Elenco Voli Disponibili
  gs_variante-report = sy-repid.
ENDFORM.

FORM f_imposta_ordinamento_iniziale.
  CLEAR gt_criteri_ord_iniziale.
  gs_criterio_ord_iniziale-spos = 1. gs_criterio_ord_iniziale-fieldname = 'CARRID'.
  gs_criterio_ord_iniziale-up = abap_true. gs_criterio_ord_iniziale-subtot = abap_true.
  APPEND gs_criterio_ord_iniziale TO gt_criteri_ord_iniziale.
  CLEAR gs_criterio_ord_iniziale.
  gs_criterio_ord_iniziale-spos = 2. gs_criterio_ord_iniziale-fieldname = 'CONNID'.
  gs_criterio_ord_iniziale-up = abap_true.
  APPEND gs_criterio_ord_iniziale TO gt_criteri_ord_iniziale.
ENDFORM.

FORM f_elabora_righe_selezionate.
  DATA: lt_righe_selezionate TYPE lvc_t_row.
  FIELD-SYMBOLS: <fs_dati_riga> TYPE sflight.
  CALL METHOD go_griglia_alv->get_selected_rows IMPORTING et_index_rows = lt_righe_selezionate.
  IF lt_righe_selezionate IS INITIAL.
    MESSAGE TEXT-003 TYPE 'I'.
  ELSE.
    DATA(lv_num_righe) = lines( lt_righe_selezionate ).
    MESSAGE S004(ZMSG) WITH lv_num_righe.
    LOOP AT lt_righe_selezionate INTO DATA(ls_riga_selezionata).
      READ TABLE gt_dati_voli_filtrati ASSIGNING <fs_dati_riga> INDEX ls_riga_selezionata-index.
      IF sy-subrc = 0.
        WRITE: / TEXT-005, <fs_dati_riga>-carrid, <fs_dati_riga>-connid, <fs_dati_riga>-fldate.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.

"----------------------------------------------------------------------
" IMPLEMENTAZIONE DELLA CLASSE GESTORE EVENTI
"----------------------------------------------------------------------
CLASS lcl_gestore_eventi IMPLEMENTATION.
  METHOD handle_node_double_click.
    gt_dati_voli_filtrati = VALUE #( FOR wa IN gt_dati_voli WHERE ( carrid = node_key ) ( wa ) ).
    CALL METHOD go_griglia_alv->refresh_table_display.
  ENDMETHOD.

  METHOD handle_hotspot_click.
    FIELD-SYMBOLS: <fs_riga_cliccata> TYPE sflight.
    IF e_column_id-fieldname = 'CARRID'.
      READ TABLE gt_dati_voli_filtrati ASSIGNING <fs_riga_cliccata> INDEX e_row_id-index.
      IF sy-subrc = 0.
        MESSAGE S017(ZMSG) WITH <fs_riga_cliccata>-carrid <fs_riga_cliccata>-connid <fs_riga_cliccata>-fldate.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD handle_toolbar.
    DATA: ls_pulsante TYPE stb_button.
    CLEAR ls_pulsante.
    ls_pulsante-function  = 'PROC_SEL'. ls_pulsante-icon = '@8D@'.
    ls_pulsante-quickinfo = TEXT-018. ls_pulsante-text = TEXT-019.
    ls_pulsante-butn_type = '0'.
    APPEND ls_pulsante TO e_object->mt_toolbar.

    CLEAR ls_pulsante.
    ls_pulsante-butn_type = 3.
    APPEND ls_pulsante TO e_object->mt_toolbar.

    CLEAR ls_pulsante.
    ls_pulsante-function  = 'SAVE_DATA'. ls_pulsante-icon = '@0S@'.
    ls_pulsante-quickinfo = TEXT-020. ls_pulsante-text = TEXT-021.
    APPEND ls_pulsante TO e_object->mt_toolbar.
  ENDMETHOD.

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN 'PROC_SEL'.
        PERFORM f_elabora_righe_selezionate.
      WHEN 'SAVE_DATA'.
        CALL METHOD go_griglia_alv->check_changed_data.
        CALL METHOD go_griglia_alv->refresh_table_display.
        MESSAGE TEXT-022 TYPE 'S'.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_data_changed.
    FIELD-SYMBOLS: <fs_cella_modificata> TYPE lvc_s_modi.
    LOOP AT er_data_changed->mt_good_cells ASSIGNING <fs_cella_modificata>.
      IF <fs_cella_modificata>-fieldname = 'SEATSOCC'.
        IF <fs_cella_modificata>-value < 0.
          CALL METHOD er_data_changed->add_protocol_entry
            EXPORTING i_msgid = '00' i_msgno = '001' i_msgty = 'E'
                      i_msgv1 = TEXT-023
                      i_fieldname = <fs_cella_modificata>-fieldname
                      i_row_id    = <fs_cella_modificata>-row_id.
        ENDIF.
        IF <fs_cella_modificata>-value CN '1234567890'.
          CALL METHOD er_data_changed->add_protocol_entry
            EXPORTING i_msgid = '00' i_msgno = '001' i_msgty = 'E'
                      i_msgv1 = TEXT-024
                      i_fieldname = <fs_cella_modificata>-fieldname
                      i_row_id    = <fs_cella_modificata>-row_id.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.`;