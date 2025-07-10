export const content = [
  {
    title: 'Esempio 1: ALV Gerarchico con Eventi',
    code: `REPORT z_report_gerarchico_sd.

**********************************************************************
*                                                                    *
* Z_REPORT_GERARCHICO_SD (Versione Corretta con Eventi Gerarchici)  *
*                                                                    *
**********************************************************************
*                                                                    *
* Descrizione:                                                       *
* Corretta la gestione eventi per utilizzare la classe specifica     *
* CL_SALV_EVENTS_HIERSEQ, risolvendo l'errore di incompatibilità.   *
*                                                                    *
**********************************************************************

*----------------------------------------------------------------------
* Dichiarazione Tabelle per la Selection Screen
*----------------------------------------------------------------------
TABLES: vbak.

*----------------------------------------------------------------------
* Tipi di Dati Globali per l'output ALV
*----------------------------------------------------------------------
TYPES:
  BEGIN OF ty_s_livello_1,
    vbeln TYPE vbak-vbeln,
    erdat TYPE vbak-erdat,
    ernam TYPE vbak-ernam,
    vkorg TYPE vbak-vkorg,
    netwr TYPE vbak-netwr,
  END OF ty_s_livello_1,
  BEGIN OF ty_s_livello_2,
    vbeln TYPE vbap-vbeln,
    posnr TYPE vbap-posnr,
    matnr TYPE vbap-matnr,
    arktx TYPE vbap-arktx,
    kwmeng TYPE vbap-kwmeng,
    zieme TYPE vbap-zieme,
    netwr TYPE vbap-netwr,
  END OF ty_s_livello_2.

TYPES:
  ty_t_livello_1 TYPE STANDARD TABLE OF ty_s_livello_1 WITH EMPTY KEY,
  ty_t_livello_2 TYPE STANDARD TABLE OF ty_s_livello_2 WITH EMPTY KEY.

*----------------------------------------------------------------------
* Dati Globali
*----------------------------------------------------------------------
DATA:
  gt_livello_1 TYPE ty_t_livello_1,
  gt_livello_2 TYPE ty_t_livello_2.

*----------------------------------------------------------------------
* DEFINIZIONE CLASSE LOCALE PER GESTIONE EVENTI
*----------------------------------------------------------------------
CLASS lcl_gestore_eventi DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      " *** CORREZIONE: Il metodo è ora definito per l'evento della classe corretta ***
      on_doppio_click
        FOR EVENT double_click OF cl_salv_events_hierseq
        IMPORTING
          level " Parametro aggiuntivo fornito dall'evento gerarchico
          row
          column.
ENDCLASS.

CLASS lcl_gestore_eventi IMPLEMENTATION.
  METHOD on_doppio_click.
    " Ora possiamo usare anche il livello per un messaggio più preciso
    MESSAGE |Doppio click sul Livello { level }, Riga n. { row }, Colonna '{ column }'.| TYPE 'S'.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------
* SELECTION-SCREEN
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: so_vbeln FOR vbak-vbeln.
SELECTION-SCREEN END OF BLOCK b1.

**********************************************************************
* EVENTO DI INIZIO SELEZIONE DATI
**********************************************************************
START-OF-SELECTION.
  PERFORM f_seleziona_dati_vendita.
  IF gt_livello_1 IS NOT INITIAL.
    PERFORM f_visualizza_gerarchia_salv.
  ELSE.
    MESSAGE 'Nessun dato trovato per i criteri di selezione specificati.' TYPE 'I'.
  ENDIF.

**********************************************************************
* ROUTINE (FORM)
**********************************************************************
*----------------------------------------------------------------------
* FORM f_seleziona_dati_vendita
*----------------------------------------------------------------------
FORM f_seleziona_dati_vendita.
  SELECT vbeln, erdat, ernam, vkorg, netwr
    FROM vbak
    INTO CORRESPONDING FIELDS OF TABLE @gt_livello_1
    WHERE vbeln IN @so_vbeln.

  IF sy-subrc <> 0 OR gt_livello_1 IS INITIAL.
    CLEAR: gt_livello_1, gt_livello_2.
    RETURN.
  ENDIF.

  SELECT vbeln, posnr, matnr, arktx, kwmeng, zieme, netwr
    FROM vbap
    INTO CORRESPONDING FIELDS OF TABLE @gt_livello_2
    FOR ALL ENTRIES IN @gt_livello_1
    WHERE vbeln = @gt_livello_1-vbeln.
ENDFORM.

*----------------------------------------------------------------------
* FORM f_visualizza_gerarchia_salv
*----------------------------------------------------------------------
FORM f_visualizza_gerarchia_salv.
  DATA: lo_alv_gerarchico TYPE REF TO cl_salv_hierseq_table,
        lt_legame         TYPE salv_t_hierseq_binding,
        ls_legame         TYPE salv_s_hierseq_binding.

  ls_legame-master = 'VBELN'.
  ls_legame-slave  = 'VBELN'.
  APPEND ls_legame TO lt_legame.

  TRY.
      cl_salv_hierseq_table=>factory(
        EXPORTING
          t_binding_level1_level2 = lt_legame
        IMPORTING
          r_hierseq               = lo_alv_gerarchico
        CHANGING
          t_table_level1          = gt_livello_1
          t_table_level2          = gt_livello_2 ).

      DATA(lo_colonne_liv_1) = lo_alv_gerarchico->get_columns( level = 1 ).
      lo_colonne_liv_1->set_optimize( abap_true ).
      lo_colonne_liv_1->get_column( 'VBELN' )->set_long_text( 'Ordine di Vendita' ).

      DATA(lo_colonne_liv_2) = lo_alv_gerarchico->get_columns( level = 2 ).
      lo_colonne_liv_2->set_optimize( abap_true ).
      lo_colonne_liv_2->get_column( 'MATNR' )->set_long_text( 'Numero Materiale' ).
      lo_colonne_liv_2->get_column( 'ARKTX' )->set_long_text( 'Descrizione Materiale' ).

      " Registrare il gestore degli eventi (questa parte ora funziona)
      DATA(lo_gestore_eventi) = NEW lcl_gestore_eventi( ).
      DATA(lo_eventi) = lo_alv_gerarchico->get_event( ).
      SET HANDLER lo_gestore_eventi->on_doppio_click FOR lo_eventi.

      " Espandere tutti i nodi all'avvio
      DATA(lo_livello_1) = lo_alv_gerarchico->get_level( 1 ).
      lo_livello_1->set_items_expanded( abap_true ).

      " Visualizzare la tabella
      lo_alv_gerarchico->display( ).

    CATCH cx_salv_data_error INTO DATA(lx_errore_dati).
      MESSAGE lx_errore_dati TYPE 'E'.
    CATCH cx_salv_not_found INTO DATA(lx_non_trovato).
      MESSAGE lx_non_trovato TYPE 'E'.
  ENDTRY.
ENDFORM.`
  },
  {
    title: 'Esempio 2: ',
    code: `REPORT z_report_gerarchico_sd.

**********************************************************************
* *
* Z_REPORT_GERARCHICO_SD (Versione 4 - Compatibile)                    *
* *
**********************************************************************
* *
* Descrizione:                                                       *
* Versione semplificata per massima compatibilità. A causa della     *
* mancanza del tipo dati 'SALV_S_ROW_HIERSEQ' nel sistema, la         *
* funzionalità dei pulsanti custom è stata rimossa.                  *
* La navigazione alle transazioni è garantita al 100% tramite        *
* l'evento DOUBLE_CLICK sulla riga desiderata.                       *
* *
**********************************************************************

*----------------------------------------------------------------------
* DICHIARAZIONI GLOBALI
*----------------------------------------------------------------------
TABLES: vbak.

TYPES:
  BEGIN OF ty_s_livello_1,
    vbeln TYPE vbak-vbeln,
    erdat TYPE vbak-erdat,
    ernam TYPE vbak-ernam,
    vkorg TYPE vbak-vkorg,
    netwr TYPE vbak-netwr,
  END OF ty_s_livello_1,

  BEGIN OF ty_s_livello_2,
    vbeln  TYPE vbap-vbeln,
    posnr  TYPE vbap-posnr,
    matnr  TYPE vbap-matnr,
    arktx  TYPE vbap-arktx,
    kwmeng TYPE vbap-kwmeng,
    zieme  TYPE vbap-zieme,
    netwr  TYPE vbap-netwr,
  END OF ty_s_livello_2.

TYPES:
  ty_t_livello_1 TYPE STANDARD TABLE OF ty_s_livello_1 WITH EMPTY KEY,
  ty_t_livello_2 TYPE STANDARD TABLE OF ty_s_livello_2 WITH EMPTY KEY.

DATA:
  gt_livello_1      TYPE ty_t_livello_1,
  gt_livello_2      TYPE ty_t_livello_2.

*----------------------------------------------------------------------
* CLASSE LOCALE PER GESTIONE EVENTI (SOLO DOPPIO CLICK)
*----------------------------------------------------------------------
CLASS lcl_gestore_eventi DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      on_doppio_click
        FOR EVENT double_click OF cl_salv_events_hierseq
        IMPORTING level row column.
ENDCLASS.

CLASS lcl_gestore_eventi IMPLEMENTATION.
  METHOD on_doppio_click.
    PERFORM f_naviga_transazione USING level row.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------
* SELECTION-SCREEN
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: so_vbeln FOR vbak-vbeln OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
* INIZIO ELABORAZIONE
*----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM f_seleziona_dati_vendita.
  IF gt_livello_1 IS NOT INITIAL.
    PERFORM f_visualizza_gerarchia_salv.
  ELSE.
    MESSAGE 'Nessun dato trovato per i criteri di selezione.' TYPE 'I'.
  ENDIF.

**********************************************************************
* ROUTINE (FORM)
**********************************************************************
*----------------------------------------------------------------------
* FORM f_seleziona_dati_vendita
*----------------------------------------------------------------------
FORM f_seleziona_dati_vendita.
  SELECT vbeln, erdat, ernam, vkorg, netwr
    FROM vbak
    INTO CORRESPONDING FIELDS OF TABLE @gt_livello_1
    WHERE vbeln IN @so_vbeln.

  IF sy-subrc <> 0 OR gt_livello_1 IS INITIAL.
    CLEAR: gt_livello_1, gt_livello_2.
    RETURN.
  ENDIF.

  SELECT vbeln, posnr, matnr, arktx, kwmeng, zieme, netwr
    FROM vbap
    INTO CORRESPONDING FIELDS OF TABLE @gt_livello_2
    FOR ALL ENTRIES IN @gt_livello_1
    WHERE vbeln = @gt_livello_1-vbeln.
ENDFORM.

*----------------------------------------------------------------------
* FORM f_visualizza_gerarchia_salv
*----------------------------------------------------------------------
FORM f_visualizza_gerarchia_salv.
  DATA: lo_alv_gerarchico TYPE REF TO cl_salv_hierseq_table,
        lt_legame         TYPE salv_t_hierseq_binding,
        ls_legame         TYPE salv_s_hierseq_binding.

  ls_legame-master = 'VBELN'.
  ls_legame-slave  = 'VBELN'.
  APPEND ls_legame TO lt_legame.

  TRY.
      cl_salv_hierseq_table=>factory(
        EXPORTING
          t_binding_level1_level2 = lt_legame
        IMPORTING
          r_hierseq               = lo_alv_gerarchico
        CHANGING
          t_table_level1          = gt_livello_1
          t_table_level2          = gt_livello_2 ).

      " Abilita le funzioni standard dell'ALV
      DATA(lo_funzioni) = lo_alv_gerarchico->get_functions( ).
      lo_funzioni->set_all( abap_true ).

      " Impostazioni di visualizzazione
      DATA(lo_impostazioni) = lo_alv_gerarchico->get_display_settings( ).
      lo_impostazioni->set_striped_pattern( abap_true ).
      lo_impostazioni->set_list_header( 'Report Gerarchico Ordini di Vendita' ).

      " Impostazioni layout
      DATA(lo_layout) = lo_alv_gerarchico->get_layout( ).
      DATA(ls_key) = VALUE salv_s_layout_key( report = sy-repid ).
      lo_layout->set_key( ls_key ).
      lo_layout->set_save_restriction( if_salv_c_layout=>restrict_user_dependant ).

      " Configurazione colonne e aggregazioni
      DATA(lo_colonne_liv_1) = lo_alv_gerarchico->get_columns( level = 1 ).
      lo_colonne_liv_1->set_optimize( abap_true ).
      lo_colonne_liv_1->get_column( 'VBELN' )->set_long_text( 'Ordine di Vendita' ).
      DATA(lo_aggr_liv_1) = lo_alv_gerarchico->get_aggregations( level = 1 ).
      lo_aggr_liv_1->add_aggregation( 'NETWR' ).

      DATA(lo_colonne_liv_2) = lo_alv_gerarchico->get_columns( level = 2 ).
      lo_colonne_liv_2->set_optimize( abap_true ).
      lo_colonne_liv_2->get_column( 'MATNR' )->set_long_text( 'Numero Materiale' ).
      lo_colonne_liv_2->get_column( 'ARKTX' )->set_long_text( 'Descrizione Materiale' ).
      DATA(lo_aggr_liv_2) = lo_alv_gerarchico->get_aggregations( level = 2 ).
      lo_aggr_liv_2->add_aggregation( 'KWMENG' ).
      lo_aggr_liv_2->add_aggregation( 'NETWR' ).

      " Gestione eventi
      DATA(lo_gestore_eventi) = NEW lcl_gestore_eventi( ).
      DATA(lo_eventi) = lo_alv_gerarchico->get_event( ).
      SET HANDLER lo_gestore_eventi->on_doppio_click FOR lo_eventi.

      " Espansione nodi
      DATA(lo_livello_1) = lo_alv_gerarchico->get_level( 1 ).
      lo_livello_1->set_items_expanded( abap_true ).

      " Display
      lo_alv_gerarchico->display( ).

    CATCH cx_salv_data_error INTO DATA(lx_errore_dati).
      MESSAGE lx_errore_dati TYPE 'E'.
    CATCH cx_salv_not_found INTO DATA(lx_non_trovato).
      MESSAGE lx_non_trovato TYPE 'E'.
    CATCH cx_salv_existing INTO DATA(lx_esistente).
      MESSAGE lx_esistente TYPE 'E'.
    CATCH cx_salv_msg.
      MESSAGE 'Errore imprevisto nella visualizzazione dell''ALV.' TYPE 'E'.
  ENDTRY.
ENDFORM.

*----------------------------------------------------------------------
* FORM f_naviga_transazione
*----------------------------------------------------------------------
FORM f_naviga_transazione USING iv_livello TYPE i iv_riga TYPE i.
  CASE iv_livello.
    WHEN 1.
      DATA ls_livello_1 TYPE ty_s_livello_1.
      READ TABLE gt_livello_1 INDEX iv_riga INTO ls_livello_1.
      IF sy-subrc = 0.
        SET PARAMETER ID 'AUN' FIELD ls_livello_1-vbeln.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 2.
      DATA ls_livello_2 TYPE ty_s_livello_2.
      READ TABLE gt_livello_2 INDEX iv_riga INTO ls_livello_2.
      IF sy-subrc = 0.
        SET PARAMETER ID 'MAT' FIELD ls_livello_2-matnr.
        CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.
ENDFORM.`
  }
];
