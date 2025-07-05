export const content = `
REPORT z_test_alv_grid_popup_dynpro.

"**********************************************************************
"* VERSIONE FINALE CON F4 ABILITATO
"*
"* DESCRIZIONE:
"* Risolto il problema dell'aiuto alla ricerca (F4) non funzionante.
"* La soluzione consiste nel costruire esplicitamente il catalogo campi
"* tramite la funzione 'LVC_FIELDCATALOG_MERGE' e passarlo al metodo
"* 'set_table_for_first_display', invece di affidarsi alla generazione
"* interna tramite il parametro 'i_structure_name'. Questo approccio
"* è più robusto e garantisce che tutte le proprietà dei campi,
"* inclusi gli aiuti alla ricerca, vengano lette correttamente.
"**********************************************************************

"----------------------------------------------------------------------
" DICHIARAZIONI GLOBALI
"----------------------------------------------------------------------
DATA: go_contenitore    TYPE REF TO cl_gui_custom_container,
      go_griglia_alv    TYPE REF TO cl_gui_alv_grid,
      gv_nome_controllo TYPE scrfname VALUE 'CONTENITORE_ALV',
      gt_dati_voli      TYPE STANDARD TABLE OF sflight,
      gt_catalogo_campi TYPE lvc_t_fcat. " <-- TABELLA PER IL CATALOGO CAMPI

"----------------------------------------------------------------------
" BLOCCO DI ELABORAZIONE PRINCIPALE
"----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM f_seleziona_dati.
  IF gt_dati_voli IS INITIAL.
    MESSAGE 'Nessun dato di volo trovato.' TYPE 'S' DISPLAY LIKE 'I'.
    RETURN.
  ENDIF.
  CALL SCREEN 200.

"----------------------------------------------------------------------
" MODULI DI GESTIONE DYNPRO 200
"----------------------------------------------------------------------
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'STANDARD_POPUP'.
  SET TITLEBAR 'TIT_POPUP'.

  IF go_contenitore IS NOT BOUND.
    CREATE OBJECT go_contenitore
      EXPORTING
        container_name = gv_nome_controllo
      EXCEPTIONS OTHERS = 1.
    IF sy-subrc <> 0.
      MESSAGE 'Errore contenitore.' TYPE 'E'. LEAVE TO SCREEN 0.
    ENDIF.

    CREATE OBJECT go_griglia_alv
      EXPORTING
        i_parent = go_contenitore.

    " *** MODIFICA CHIAVE 1: COSTRUZIONE ESPLICITA DEL CATALOGO CAMPI ***
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'SFLIGHT'
      CHANGING
        ct_fieldcat      = gt_catalogo_campi
      EXCEPTIONS
        OTHERS           = 1.
    IF sy-subrc <> 0.
      MESSAGE 'Errore nella creazione del catalogo campi.' TYPE 'E'.
      LEAVE TO SCREEN 0.
    ENDIF.

    CALL METHOD go_griglia_alv->set_toolbar_interactive.

    " *** MODIFICA CHIAVE 2: PASSAGGIO DEL CATALOGO CAMPI ESPLICITO ***
    " Rimuoviamo 'i_structure_name' e usiamo 'it_fieldcatalog'.
    CALL METHOD go_griglia_alv->set_table_for_first_display
      EXPORTING
        it_fieldcatalog = gt_catalogo_campi " <-- Passiamo il nostro catalogo
      CHANGING
        it_outtab       = gt_dati_voli.
  ENDIF.
ENDMODULE.

MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR '&F03' OR '&F15' OR '&F12' OR '%EX'.
      IF go_contenitore IS BOUND.
        go_contenitore->free( ).
        CLEAR: go_contenitore, go_griglia_alv.
      ENDIF.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

"----------------------------------------------------------------------
" SUBROUTINES (FORMS)
"----------------------------------------------------------------------
FORM f_seleziona_dati.
  SELECT * FROM sflight INTO TABLE @gt_dati_voli UP TO 50 ROWS.
ENDFORM.

`;
