export const content = `
REPORT z_test_cl_gui_alv_grid.

"**********************************************************************
"* REPORT Z_TEST_CL_GUI_ALV_GRID_FINALE
"* DESCRIZIONE:
"* Report di test completo per la classe CL_GUI_ALV_GRID.
"* Questo programma è il risultato di uno sviluppo incrementale e
"* implementa le seguenti funzionalità:
"*
"* 1.  Visualizzazione ALV Object Oriented in un container custom su Dynpro.
"* 2.  Gestione corretta dell'uscita dalla schermata (Pulsante Indietro).
"* 3.  Personalizzazione del catalogo campi (Intestazioni, Somme, Hotspot).
"* 4.  Gestione eventi tramite classe locale (hotspot_click, toolbar, user_command).
"* 5.  Aggiunta di pulsanti custom alla toolbar ("Elabora", "Salva").
"* 6.  Gestione della selezione multipla e logica di elaborazione.
"* 7.  Impostazione di un ordinamento iniziale con calcolo dei subtotali.
"* 8.  Implementazione di una griglia editabile sicura (Schema "Salva"):
"* - Colonna 'Posti Occupati' resa modificabile.
"* - Evento DATA_CHANGED gestito per la validazione dei dati su Invio.
"* - Evento USER_COMMAND gestito per il pulsante "Salva" che applica
"* le modifiche alla tabella interna e aggiorna la griglia,
"* evitando il rischio di loop infiniti.
"**********************************************************************

"----------------------------------------------------------------------
" DICHIARAZIONI GLOBALI
"----------------------------------------------------------------------
DATA: go_contenitore           TYPE REF TO cl_gui_custom_container,
      go_griglia_alv           TYPE REF TO cl_gui_alv_grid,
      gv_nome_control_custom   TYPE scrfname VALUE 'CONTENITORE_ALV'.

DATA: gt_dati_voli TYPE STANDARD TABLE OF sflight.

DATA: gt_catalogo_campi        TYPE lvc_t_fcat,
      gs_layout                TYPE lvc_s_layo,
      gs_variante              TYPE disvariant,
      gt_criteri_ord_iniziale  TYPE lvc_t_sort,
      gs_criterio_ord_iniziale TYPE lvc_s_sort.

"----------------------------------------------------------------------
" CLASSE PER LA GESTIONE DEGLI EVENTI ALV
"----------------------------------------------------------------------
CLASS lcl_gestore_eventi DEFINITION.
  PUBLIC SECTION.
    METHODS:
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
        IMPORTING er_data_changed.
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
    MESSAGE 'Nessun dato di test trovato.' TYPE 'S' DISPLAY LIKE 'I'.
  ENDIF.

"----------------------------------------------------------------------
" MODULI DI GESTIONE DYNPRO 100
"----------------------------------------------------------------------
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'TIT_0100' WITH 'Report ALV Completo'.
  PERFORM f_visualizza_griglia_alv.
ENDMODULE.

MODULE user_command_0100 INPUT.
  CALL METHOD cl_gui_cfw=>dispatch.
  CASE sy-ucomm.
    WHEN '&F03' OR 'BACK' OR '&F15' OR 'EXIT' OR '&F12' OR 'CANCEL'.
      IF go_griglia_alv IS BOUND. go_griglia_alv->free( ). CLEAR go_griglia_alv. ENDIF.
      IF go_contenitore IS BOUND. go_contenitore->free( ). CLEAR go_contenitore. ENDIF.
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

FORM f_imposta_ordinamento_iniziale.
  CLEAR gs_criterio_ord_iniziale.
  gs_criterio_ord_iniziale-spos = 1. gs_criterio_ord_iniziale-fieldname = 'CARRID'.
  gs_criterio_ord_iniziale-up = abap_true. gs_criterio_ord_iniziale-subtot = abap_true.
  APPEND gs_criterio_ord_iniziale TO gt_criteri_ord_iniziale.
  CLEAR gs_criterio_ord_iniziale.
  gs_criterio_ord_iniziale-spos = 2. gs_criterio_ord_iniziale-fieldname = 'CONNID'.
  gs_criterio_ord_iniziale-up = abap_true.
  APPEND gs_criterio_ord_iniziale TO gt_criteri_ord_iniziale.
ENDFORM.

FORM f_elabora_righe_selezionate.
  DATA: lt_righe_selezionate TYPE lvc_t_row, ls_riga_selezionata  TYPE lvc_s_row, lv_messaggio TYPE string.
  FIELD-SYMBOLS: <fs_dati_riga> TYPE sflight.
  CALL METHOD go_griglia_alv->get_selected_rows IMPORTING et_index_rows = lt_righe_selezionate.
  IF lt_righe_selezionate IS INITIAL.
    MESSAGE 'Nessuna riga selezionata.' TYPE 'I'.
  ELSE.
    " Nota: la sintassi ABAP per i template string e' |...{ }...|
    lv_messaggio = |Inizio elaborazione di { lines( lt_righe_selezionate ) } righe...|.
    MESSAGE lv_messaggio TYPE 'S'.
    LOOP AT lt_righe_selezionate INTO ls_riga_selezionata.
      READ TABLE gt_dati_voli ASSIGNING <fs_dati_riga> INDEX ls_riga_selezionata-index.
      IF sy-subrc = 0.
        WRITE: / 'Elaborata riga:', <fs_dati_riga>-carrid, <fs_dati_riga>-connid, <fs_dati_riga>-fldate.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM f_visualizza_griglia_alv.
  IF go_contenitore IS NOT BOUND.
    CREATE OBJECT go_contenitore
      EXPORTING container_name = gv_nome_control_custom EXCEPTIONS OTHERS = 1.
    IF sy-subrc <> 0. MESSAGE 'Errore contenitore ALV.' TYPE 'E'. LEAVE PROGRAM. ENDIF.
    CREATE OBJECT go_griglia_alv
      EXPORTING i_parent = go_contenitore EXCEPTIONS OTHERS = 1.
    IF sy-subrc <> 0. MESSAGE 'Errore griglia ALV.' TYPE 'E'. LEAVE PROGRAM. ENDIF.
    CREATE OBJECT go_gestore_eventi.
    SET HANDLER go_gestore_eventi->handle_hotspot_click FOR go_griglia_alv.
    SET HANDLER go_gestore_eventi->handle_toolbar FOR go_griglia_alv.
    SET HANDLER go_gestore_eventi->handle_user_command FOR go_griglia_alv.
    SET HANDLER go_gestore_eventi->handle_data_changed FOR go_griglia_alv.

    CALL METHOD go_griglia_alv->register_edit_event
      EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING i_structure_name = 'SFLIGHT'
      CHANGING ct_fieldcat = gt_catalogo_campi
      EXCEPTIONS OTHERS = 1.
    IF sy-subrc <> 0. MESSAGE 'Errore catalogo campi.' TYPE 'E'. LEAVE PROGRAM. ENDIF.

    FIELD-SYMBOLS: <fs_campo_cat> TYPE lvc_s_fcat.
    LOOP AT gt_catalogo_campi ASSIGNING <fs_campo_cat>.
      CASE <fs_campo_cat>-fieldname.
        WHEN 'CARRID'.   <fs_campo_cat>-coltext = 'Compagnia Aerea'. <fs_campo_cat>-hotspot = abap_true.
        WHEN 'CONNID'.   <fs_campo_cat>-coltext = 'ID Connessione'.
        WHEN 'FLDATE'.   <fs_campo_cat>-coltext = 'Data Volo'.
        WHEN 'PRICE'.    <fs_campo_cat>-coltext = 'Prezzo del Volo'. <fs_campo_cat>-do_sum = abap_true.
        WHEN 'CURRENCY'. <fs_campo_cat>-coltext = 'Valuta'.
        WHEN 'PLANETYPE'.<fs_campo_cat>-coltext = 'Tipo Aeromobile'.
        WHEN 'SEATSOCC'.
          <fs_campo_cat>-coltext  = 'Posti Occupati'.
          <fs_campo_cat>-edit     = abap_true.
      ENDCASE.
    ENDLOOP.

    gs_layout-sel_mode   = 'A'.
    gs_layout-zebra      = abap_true.
    gs_layout-cwidth_opt = abap_true.
    gs_layout-grid_title = 'Elenco Voli Disponibili'.
    gs_variante-report = sy-repid.

    PERFORM f_imposta_ordinamento_iniziale.

    CALL METHOD go_griglia_alv->set_toolbar_interactive.
    CALL METHOD go_griglia_alv->set_table_for_first_display
      EXPORTING is_layout = gs_layout is_variant = gs_variante i_save = 'A'
      CHANGING  it_outtab = gt_dati_voli it_fieldcatalog = gt_catalogo_campi
                it_sort = gt_criteri_ord_iniziale.
  ENDIF.
ENDFORM.

"----------------------------------------------------------------------
" IMPLEMENTAZIONE DELLA CLASSE GESTORE EVENTI
"----------------------------------------------------------------------
CLASS lcl_gestore_eventi IMPLEMENTATION.
  METHOD handle_hotspot_click.
    DATA: lv_messaggio TYPE string.
    FIELD-SYMBOLS: <fs_riga_cliccata> TYPE sflight.
    IF e_column_id-fieldname = 'CARRID'.
      READ TABLE gt_dati_voli ASSIGNING <fs_riga_cliccata> INDEX e_row_id-index.
      IF sy-subrc = 0.
        lv_messaggio = |Hai cliccato sulla compagnia: { <fs_riga_cliccata>-carrid }, | && |volo n. { <fs_riga_cliccata>-connid } del { <fs_riga_cliccata>-fldate }.|.
        MESSAGE lv_messaggio TYPE 'I'.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD handle_toolbar.
    DATA: ls_pulsante TYPE stb_button.
    " Pulsante 'Elabora'
    CLEAR ls_pulsante.
    ls_pulsante-function  = 'PROC_SEL'. ls_pulsante-icon = '@8D@'.
    ls_pulsante-quickinfo = 'Elabora Righe Selezionate'. ls_pulsante-text = 'Elabora'.
    ls_pulsante-butn_type = '0'.
    APPEND ls_pulsante TO e_object->mt_toolbar.

    " Separatore e pulsante 'Salva'
    CLEAR ls_pulsante.
    ls_pulsante-butn_type = 3. " 3 = Separator
    APPEND ls_pulsante TO e_object->mt_toolbar.

    CLEAR ls_pulsante.
    ls_pulsante-function  = 'SAVE_DATA'.
    ls_pulsante-icon      = '@0S@'. " Icona Salva
    ls_pulsante-quickinfo = 'Salva le modifiche nella griglia'.
    ls_pulsante-text      = 'Salva'.
    APPEND ls_pulsante TO e_object->mt_toolbar.
  ENDMETHOD.

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN 'PROC_SEL'.
        PERFORM f_elabora_righe_selezionate.
      WHEN 'SAVE_DATA'.
        " Applica le modifiche alla tabella interna
        CALL METHOD go_griglia_alv->check_changed_data.
        " Aggiorna la visualizzazione per mostrare i dati salvati
        " NOTA: Non è sempre necessario, ma garantisce la coerenza visiva
        CALL METHOD go_griglia_alv->refresh_table_display.
        MESSAGE 'Modifiche applicate con successo!' TYPE 'S'.
        " QUI andrebbe la logica per salvare GT_DATI_VOLI nel database
    ENDCASE.
  ENDMETHOD.

  METHOD handle_data_changed.
    " Questo metodo si occupa SOLO della VALIDAZIONE dei dati inseriti
    FIELD-SYMBOLS: <fs_cella_modificata> TYPE lvc_s_modi.
    LOOP AT er_data_changed->mt_good_cells ASSIGNING <fs_cella_modificata>.
      IF <fs_cella_modificata>-fieldname = 'SEATSOCC'.
        " Validazione 1: il valore non può essere negativo
        IF <fs_cella_modificata>-value < 0.
          CALL METHOD er_data_changed->add_protocol_entry
            EXPORTING i_msgid = '00' i_msgno = '001' i_msgty = 'E'
                      i_msgv1 = 'Il numero di posti non può essere negativo.'
                      i_fieldname = <fs_cella_modificata>-fieldname
                      i_row_id    = <fs_cella_modificata>-row_id.
        ENDIF.
        " Validazione 2: il valore deve essere numerico
        IF <fs_cella_modificata>-value CN '1234567890'.
          CALL METHOD er_data_changed->add_protocol_entry
            EXPORTING i_msgid = '00' i_msgno = '001' i_msgty = 'E'
                      i_msgv1 = 'Inserire solo valori numerici.'
                      i_fieldname = <fs_cella_modificata>-fieldname
                      i_row_id    = <fs_cella_modificata>-row_id.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

`;
