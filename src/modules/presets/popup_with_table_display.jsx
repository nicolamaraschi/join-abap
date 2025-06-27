export const content = `
REPORT zdemo_popup_table_display.

"**********************************************************************
"* *
"* OBIETTIVO DEL PROGRAMMA:                                           *
"* Questo report è un esempio didattico che mostra come utilizzare    *
"* il function module 'POPUP_WITH_TABLE_DISPLAY' per visualizzare     *
"* una tabella di dati in una finestra di dialogo (popup) e per       *
"* catturare la selezione dell'utente.                                *
"**********************************************************************

"----------------------------------------------------------------------
" DICHIARAZIONE DATI GLOBALI
"----------------------------------------------------------------------

" Tipi di dato per la tabella da visualizzare nel popup
TYPES:
  BEGIN OF ty_dati_popup,
    testo TYPE char80, " Campo singolo per contenere il testo di ogni riga
  END OF ty_dati_popup.

" Dati per il popup
DATA:
  gt_dati_per_popup  TYPE STANDARD TABLE OF ty_dati_popup, " Tabella con i dati da mostrare
  gs_dati_per_popup  TYPE ty_dati_popup,                   " Struttura di lavoro per la tabella
  gv_riga_selezionata TYPE sy-tabix,                       " Per ricevere l'indice della riga scelta
  gs_riga_letta       LIKE LINE OF gt_dati_per_popup.     " Per leggere la riga selezionata dalla tabella

" Costanti per le coordinate di default del popup
CONSTANTS:
  gc_start_col TYPE i VALUE 10, " Colonna iniziale di default
  gc_start_row TYPE i VALUE 5,  " Riga iniziale di default
  gc_end_col   TYPE i VALUE 90, " Colonna finale di default
  gc_end_row   TYPE i VALUE 20. " Riga finale di default

"----------------------------------------------------------------------
" SELECTION-SCREEN
"----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blocco_dati WITH FRAME TITLE TEXT-001. " Titolo: 'Parametri Popup'
  PARAMETERS:
    p_title(80) TYPE c DEFAULT 'Seleziona una riga dall''elenco'.
SELECTION-SCREEN END OF BLOCK blocco_dati.

SELECTION-SCREEN BEGIN OF BLOCK blocco_pos WITH FRAME TITLE TEXT-002. " Titolo: 'Posizione Popup'
  PARAMETERS:
    p_scol TYPE i DEFAULT gc_start_col, " Colonna Inizio
    p_srow TYPE i DEFAULT gc_start_row, " Riga Inizio
    p_ecol TYPE i DEFAULT gc_end_col,   " Colonna Fine
    p_erow TYPE i DEFAULT gc_end_row.   " Riga Fine
SELECTION-SCREEN END OF BLOCK blocco_pos.


"**********************************************************************
"* EVENTI                                                             *
"**********************************************************************

"----------------------------------------------------------------------
" INITIALIZATION: Eseguito prima della visualizzazione della SELECTION-SCREEN
"----------------------------------------------------------------------
INITIALIZATION.
  " Si popolano i dati di esempio da mostrare nel popup.
  " In un'applicazione reale, questi dati proverrebbero da una SELECT sul database.
  CLEAR gt_dati_per_popup.

  gs_dati_per_popup-testo = 'Opzione 1: Visualizza Dettaglio Materiale'.
  APPEND gs_dati_per_popup TO gt_dati_per_popup.

  gs_dati_per_popup-testo = 'Opzione 2: Crea Ordine di Acquisto'.
  APPEND gs_dati_per_popup TO gt_dati_per_popup.

  gs_dati_per_popup-testo = 'Opzione 3: Esegui Stampa Inventario'.
  APPEND gs_dati_per_popup TO gt_dati_per_popup.

  gs_dati_per_popup-testo = 'Opzione 4: Annulla Operazione Corrente'.
  APPEND gs_dati_per_popup TO gt_dati_per_popup.

  gs_dati_per_popup-testo = 'Opzione 5: Esporta dati in Excel'.
  APPEND gs_dati_per_popup TO gt_dati_per_popup.


"----------------------------------------------------------------------
" START-OF-SELECTION: Eseguito dopo l'input dell'utente nella SELECTION-SCREEN
"----------------------------------------------------------------------
START-OF-SELECTION.

  " Controllo preliminare: se non ci sono dati da mostrare, è inutile procedere.
  IF gt_dati_per_popup IS INITIAL.
    MESSAGE 'Nessun dato di esempio da visualizzare nel popup.' TYPE 'I'.
    RETURN.
  ENDIF.

  " Chiamata al function module per la visualizzazione del popup
  CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
    EXPORTING
      endpos_col   = p_ecol       " Posizione colonna finale
      endpos_row   = p_erow       " Posizione riga finale
      startpos_col = p_scol       " Posizione colonna iniziale
      startpos_row = p_srow       " Posizione riga iniziale
      titletext    = p_title      " Titolo della finestra popup
    IMPORTING
      choise       = gv_riga_selezionata " Indice della riga selezionata dall'utente
    TABLES
      valuetab     = gt_dati_per_popup   " Tabella con i valori da mostrare
    EXCEPTIONS
      break_off    = 1            " Eccezione se l'utente preme 'Annulla'
      OTHERS       = 2.

  " Analisi del risultato della chiamata al popup
  IF sy-subrc = 0.
    " L'utente ha selezionato una riga e premuto 'Scegli'

    " Si legge la riga corrispondente all'indice restituito
    READ TABLE gt_dati_per_popup INTO gs_riga_letta INDEX gv_riga_selezionata.
    IF sy-subrc = 0.
      " La lettura ha avuto successo, si mostra un messaggio con il risultato
      DATA(lv_messaggio) = |Hai selezionato la riga numero { gv_riga_selezionata } con il testo: '{ gs_riga_letta-testo }'|.
      MESSAGE lv_messaggio TYPE 'S'.
    ELSE.
      " Errore improbabile se la logica è corretta
      MESSAGE 'Errore interno: impossibile leggere la riga selezionata.' TYPE 'E'.
    ENDIF.

  ELSEIF sy-subrc = 1.
    " L'utente ha chiuso il popup o ha premuto 'Annulla'
    MESSAGE 'Operazione annullata dall''utente.' TYPE 'I'.

  ELSE.
    " Altro errore durante la chiamata alla funzione
    MESSAGE 'Errore imprevisto durante la visualizzazione del popup.' TYPE 'E'.
  ENDIF.`;