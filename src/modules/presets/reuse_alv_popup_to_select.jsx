export const content = `
REPORT z_prototipo_solo_popup.

"**********************************************************************
"* PROGRAMMA MINIMALISTA PER ALV POPUP (COSTRUZIONE MANUALE)          *
"*--------------------------------------------------------------------*
"* Scopo: Mostrare la logica essenziale e stabile per chiamare        *
"* REUSE_ALV_POPUP_TO_SELECT costruendo il catalogo campi      *
"* interamente a mano.                                                *
"**********************************************************************

"----------------------------------------------------------------------
" Tipi di Dato Locali (solo per i dati del popup)
"----------------------------------------------------------------------
TYPES:
  BEGIN OF ty_s_dati_popup,
    id          TYPE i,
    descrizione TYPE string,
  END OF ty_s_dati_popup,

  ty_t_dati_popup TYPE STANDARD TABLE OF ty_s_dati_popup WITH EMPTY KEY.


"**********************************************************************
"* FLUSSO DI CONTROLLO PRINCIPALE                                     *
"**********************************************************************
START-OF-SELECTION.
  PERFORM f_mostra_il_popup.


"**********************************************************************
"* SUBROUTINES (FORM)                                                 *
"**********************************************************************

"----------------------------------------------------------------------
" FORM f_mostra_il_popup
" Scopo: Contiene tutta la logica necessaria per il popup.
"----------------------------------------------------------------------
FORM f_mostra_il_popup.

  " 1. Dichiarazione delle variabili necessarie
  DATA:
    lt_dati_per_popup     TYPE ty_t_dati_popup,
    lt_catalogo_campi_pop TYPE slis_t_fieldcat_alv,
    ls_selezione_popup    TYPE slis_selfield.

  " 2. Creazione dei dati di test (fittizi)
  lt_dati_per_popup = VALUE #(
    ( id = 10   descrizione = 'Prima Opzione Selezionabile' )
    ( id = 20   descrizione = 'Seconda Scelta Possibile' )
    ( id = 30   descrizione = 'Terzo Valore di Esempio' )
    ( id = 40   descrizione = 'Ultima Riga del Test' )
  ).

  " 3. Creazione del catalogo campi MANUALE
  " ---> CORREZIONE FINALE: La sintassi della chiamata PERFORM è stata corretta.
  PERFORM f_crea_catalogo_manuale
    CHANGING
      lt_catalogo_campi_pop.

  " 4. Chiamata alla funzione che mostra l'ALV in un popup
  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title       = 'Seleziona un Valore (Catalogo Manuale)'
      i_selection   = abap_true
      i_zebra       = abap_true
      i_tabname     = 'LT_DATI_PER_POPUP'
      it_fieldcat   = lt_catalogo_campi_pop
    IMPORTING
      es_selfield   = ls_selezione_popup
    TABLES
      t_outtab      = lt_dati_per_popup
    EXCEPTIONS
      program_error = 1
      OTHERS        = 2.

  " 5. Gestione del risultato
  IF sy-subrc = 0.
    IF ls_selezione_popup-exit <> abap_true.
      READ TABLE lt_dati_per_popup INTO DATA(ls_riga_scelta) INDEX ls_selezione_popup-tabindex.
      IF sy-subrc = 0.
        MESSAGE |Hai selezionato l'ID '{ ls_riga_scelta-id }' con descrizione: '{ ls_riga_scelta-descrizione }'| TYPE 'S'.
      ENDIF.
    ELSE.
      MESSAGE 'Operazione annullata dall''utente.' TYPE 'I'.
    ENDIF.
  ELSE.
    MESSAGE 'Errore durante la visualizzazione del popup.' TYPE 'E'.
  ENDIF.

ENDFORM.


"----------------------------------------------------------------------
" FORM f_crea_catalogo_manuale
" Scopo: Costruisce il catalogo campi a mano, riga per riga.
"----------------------------------------------------------------------
FORM f_crea_catalogo_manuale
  CHANGING
    ct_catalogo_campi TYPE slis_t_fieldcat_alv.

  DATA: ls_campo TYPE slis_fieldcat_alv.
  CLEAR ct_catalogo_campi.

  " Definiamo la Colonna 1: ID
  CLEAR ls_campo.
  ls_campo-col_pos   = 1.
  ls_campo-fieldname = 'ID'.
  ls_campo-seltext_m = 'ID Valore'.
  ls_campo-outputlen = 10.
  ls_campo-just      = 'R'.
  APPEND ls_campo TO ct_catalogo_campi.

  " Definiamo la Colonna 2: DESCRIZIONE
  CLEAR ls_campo.
  ls_campo-col_pos   = 2.
  ls_campo-fieldname = 'DESCRIZIONE'.
  ls_campo-seltext_m = 'Descrizione Esempio'.
  ls_campo-outputlen = 40.
  ls_campo-just      = 'L'.
  APPEND ls_campo TO ct_catalogo_campi.

ENDFORM.
`;