export const content = `REPORT z_alv_tree_finale_v6.

"**********************************************************************
"* MONOREPORT V6 - Corretto secondo pattern SAP Standard (BCALV_TREE_01)
"*--------------------------------------------------------------------*
"* v13: Correzione logica decisiva.
"* Risolto l'errore per cui si tentava di espandere anche i nodi "foglia".
"* Ora il metodo EXPAND_NODE viene chiamato solo per i nodi
"* contrassegnati come cartelle (e_cartella = 'X').
"**********************************************************************

"----------------------------------------------------------------------
"& SECTION 1: DICHIARAZIONI GLOBALI
"----------------------------------------------------------------------
TYPE-POOLS: slis, lvc, cntl.

CONSTANTS:
  gc_vero              TYPE abap_bool VALUE abap_true,
  gc_nome_contenitore  TYPE c LENGTH 30 VALUE 'CONTENITORE_ALBERO_ALV'.

TYPES:
  BEGIN OF ty_riga_dati_albero,
    chiave_nodo_logica  TYPE c LENGTH 32,
    chiave_padre_logica TYPE c LENGTH 32,
    livello             TYPE i,
    testo_nodo          TYPE c LENGTH 128,
    valore_nodo         TYPE c LENGTH 128,
    e_cartella          TYPE abap_bool, " << REINTRODOTTO FLAG PER IDENTIFICARE LE CARTELLE
  END OF ty_riga_dati_albero,

  BEGIN OF ty_mappa_chiavi,
    chiave_logica TYPE c LENGTH 32,
    chiave_alv    TYPE lvc_nkey,
  END OF ty_mappa_chiavi.

"&---------------------------------------------------------------------*
"& Classe per la gestione degli eventi dell'ALV Tree
"&---------------------------------------------------------------------*
CLASS lcl_gestore_eventi DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_nodo_doppio_click
        FOR EVENT node_double_click OF cl_gui_alv_tree
        IMPORTING node_key.
ENDCLASS.

CLASS lcl_gestore_eventi IMPLEMENTATION.
  METHOD handle_nodo_doppio_click.
    MESSAGE |Doppio click sul nodo con chiave ALV: { node_key }| TYPE 'S'.
  ENDMETHOD.
ENDCLASS.

"&---------------------------------------------------------------------*
"& Variabili Globali
"&---------------------------------------------------------------------*
DATA: go_contenitore       TYPE REF TO cl_gui_custom_container,
      go_albero_alv        TYPE REF TO cl_gui_alv_tree,
      go_gestore_eventi    TYPE REF TO lcl_gestore_eventi.

DATA: gt_dati_alv_output   TYPE STANDARD TABLE OF ty_riga_dati_albero.

"----------------------------------------------------------------------
"& SECTION 2: BLOCCHI DI ELABORAZIONE
"----------------------------------------------------------------------
START-OF-SELECTION.
  CALL SCREEN 9000.

"----------------------------------------------------------------------
"& SECTION 3: MODULI DYNPRO
"----------------------------------------------------------------------
MODULE pbo_9000 OUTPUT.
  SET PF-STATUS 'STATO_ALV'.
  SET TITLEBAR 'TITOLO_ALV'.

  IF go_contenitore IS INITIAL.
    PERFORM inizializza_e_crea_albero.
  ENDIF.
ENDMODULE.

MODULE pai_9000 INPUT.
  DATA: lv_ok_code TYPE sy-ucomm.

  lv_ok_code = sy-ucomm.
  CLEAR sy-ucomm.

  CASE lv_ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      IF go_albero_alv IS BOUND.
        go_albero_alv->free( ).
      ENDIF.
      IF go_contenitore IS BOUND.
        go_contenitore->free( ).
      ENDIF.
      FREE: go_albero_alv, go_contenitore.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
      CALL METHOD cl_gui_cfw=>dispatch.
  ENDCASE.
ENDMODULE.


"----------------------------------------------------------------------
"& SECTION 4: LOGICA APPLICATIVA (FORMS)
"----------------------------------------------------------------------

"&---------------------------------------------------------------------*
"& Form  INIZIALIZZA_E_CREA_ALBERO
"&---------------------------------------------------------------------*
FORM inizializza_e_crea_albero.
  CREATE OBJECT go_contenitore
    EXPORTING
      container_name = gc_nome_contenitore
    EXCEPTIONS OTHERS = 1.
  IF sy-subrc <> 0.
    MESSAGE 'Errore: creare l''elemento Custom Control sulla Dynpro 9000.' TYPE 'E'.
  ENDIF.

  CREATE OBJECT go_albero_alv
    EXPORTING
      parent              = go_contenitore
      item_selection      = gc_vero
      node_selection_mode = cl_gui_column_tree=>node_sel_mode_single
    EXCEPTIONS OTHERS = 1.
  IF sy-subrc <> 0.
    MESSAGE 'Errore nella creazione dell''oggetto ALV Tree.' TYPE 'E'.
  ENDIF.

  PERFORM registra_eventi_albero.
  PERFORM definisci_struttura_albero.
  PERFORM costruisci_albero_visivo.

  CALL METHOD go_albero_alv->frontend_update.
ENDFORM.

"&---------------------------------------------------------------------*
"& Form  DEFINISCI_STRUTTURA_ALBERO
"&---------------------------------------------------------------------*
FORM definisci_struttura_albero.
  DATA: lt_cat_campi  TYPE lvc_t_fcat,
        ls_cat_campi  TYPE lvc_s_fcat,
        ls_header     TYPE treev_hhdr.

  CLEAR: ls_cat_campi, lt_cat_campi.
  ls_cat_campi-fieldname = 'VALORE_NODO'.
  ls_cat_campi-coltext   = 'Dettaglio Test'.
  APPEND ls_cat_campi TO lt_cat_campi.

  ls_header-heading = 'Gerarchia Test'.
  ls_header-width   = 40.

  CALL METHOD go_albero_alv->set_table_for_first_display
    EXPORTING
      is_hierarchy_header = ls_header
    CHANGING
      it_outtab           = gt_dati_alv_output
      it_fieldcatalog     = lt_cat_campi.
ENDFORM.

"&---------------------------------------------------------------------*
"& Form  COSTRUISCI_ALBERO_VISIVO
"&---------------------------------------------------------------------*
FORM costruisci_albero_visivo.
  DATA: lt_dati_logici       TYPE STANDARD TABLE OF ty_riga_dati_albero,
        lt_mappa_chiavi      TYPE STANDARD TABLE OF ty_mappa_chiavi,
        ls_mappa_chiavi      TYPE ty_mappa_chiavi,
        lv_chiave_padre_alv  TYPE lvc_nkey,
        lv_chiave_nodo_nuova TYPE lvc_nkey,
        lv_testo_nodo        TYPE lvc_value.

  FIELD-SYMBOLS: <fs_riga_logica> LIKE LINE OF lt_dati_logici.

  PERFORM popola_dati_gerarchia TABLES lt_dati_logici.
  SORT lt_dati_logici BY livello ASCENDING.

  LOOP AT lt_dati_logici ASSIGNING <fs_riga_logica>.
    CLEAR: lv_chiave_padre_alv, lv_chiave_nodo_nuova.

    IF <fs_riga_logica>-chiave_padre_logica IS NOT INITIAL.
      READ TABLE lt_mappa_chiavi INTO ls_mappa_chiavi
        WITH KEY chiave_logica = <fs_riga_logica>-chiave_padre_logica.
      IF sy-subrc = 0.
        lv_chiave_padre_alv = ls_mappa_chiavi-chiave_alv.
      ENDIF.
    ENDIF.

    lv_testo_nodo = <fs_riga_logica>-testo_nodo.

    CALL METHOD go_albero_alv->add_node
      EXPORTING
        i_relat_node_key = lv_chiave_padre_alv
        i_relationship   = cl_gui_column_tree=>relat_last_child
        is_outtab_line   = <fs_riga_logica>
        i_node_text      = lv_testo_nodo
      IMPORTING
        e_new_node_key   = lv_chiave_nodo_nuova.

    ls_mappa_chiavi-chiave_logica = <fs_riga_logica>-chiave_nodo_logica.
    ls_mappa_chiavi-chiave_alv    = lv_chiave_nodo_nuova.
    APPEND ls_mappa_chiavi TO lt_mappa_chiavi.
  ENDLOOP.

  " >> CORREZIONE: Espandiamo solo i nodi che sono stati definiti come cartelle
  LOOP AT lt_dati_logici ASSIGNING <fs_riga_logica> WHERE e_cartella = gc_vero.

    " Per espandere il nodo, dobbiamo recuperare la sua chiave ALV dalla mappa
    READ TABLE lt_mappa_chiavi INTO ls_mappa_chiavi
        WITH KEY chiave_logica = <fs_riga_logica>-chiave_nodo_logica.

    IF sy-subrc = 0.
      CALL METHOD go_albero_alv->expand_node
        EXPORTING
          i_node_key = ls_mappa_chiavi-chiave_alv.
    ENDIF.

  ENDLOOP.
ENDFORM.

"&---------------------------------------------------------------------*
"& Form  POPOLA_DATI_GERARCHIA
"&---------------------------------------------------------------------*
FORM popola_dati_gerarchia TABLES pt_dati_logici TYPE STANDARD TABLE.
  DATA ls_riga_dati TYPE ty_riga_dati_albero.
  CLEAR pt_dati_logici.

  " >> CORREZIONE: Impostiamo il flag E_CARTELLA solo per i nodi genitore
  ls_riga_dati = VALUE #( chiave_nodo_logica = 'RADICE'
                          chiave_padre_logica = ''
                          livello = 1
                          testo_nodo = 'Progetto Principale S/4HANA (3 Righe)'
                          valore_nodo = 'Budget Totale'
                          e_cartella = gc_vero ). " Questo è una cartella
  APPEND ls_riga_dati TO pt_dati_logici.

  ls_riga_dati = VALUE #( chiave_nodo_logica = 'FASE1'
                          chiave_padre_logica = 'RADICE'
                          livello = 2
                          testo_nodo = 'Fase 1: Analisi e Design'
                          valore_nodo = 'Stato: Completata'
                          e_cartella = abap_false ). " Questa è una foglia
  APPEND ls_riga_dati TO pt_dati_logici.

  ls_riga_dati = VALUE #( chiave_nodo_logica = 'FASE2'
                          chiave_padre_logica = 'RADICE'
                          livello = 2
                          testo_nodo = 'Fase 2: Sviluppo'
                          valore_nodo = 'Stato: In Corso'
                          e_cartella = abap_false ). " Questa è una foglia
  APPEND ls_riga_dati TO pt_dati_logici.
ENDFORM.

"&---------------------------------------------------------------------*
"& Form  REGISTRA_EVENTI_ALBERO
"&---------------------------------------------------------------------*
FORM registra_eventi_albero.
  DATA: lt_eventi TYPE cntl_simple_events,
        ls_evento TYPE cntl_simple_event.

  CREATE OBJECT go_gestore_eventi.
  SET HANDLER go_gestore_eventi->handle_nodo_doppio_click FOR go_albero_alv.

  ls_evento-eventid = cl_gui_column_tree=>eventid_node_double_click.
  ls_evento-appl_event = gc_vero.
  APPEND ls_evento TO lt_eventi.

  CALL METHOD go_albero_alv->set_registered_events
    EXPORTING
      events = lt_eventi.
ENDFORM.`;