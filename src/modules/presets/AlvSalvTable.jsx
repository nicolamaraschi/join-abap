export const content = `" Variabili globali per la gestione dell'ALV con CL_SALV_TABLE
DATA: go_alv            TYPE REF TO cl_salv_table,
      go_evento_handler TYPE REF TO lcl_evento_handler.

" Costanti utili
CONSTANTS: gc_vero  TYPE abap_bool VALUE abap_true,
           gc_falso TYPE abap_bool VALUE abap_false.

"----------------------------------------------------------------------
" FORM per creare, configurare e visualizzare l'ALV
"----------------------------------------------------------------------
FORM f_imposta_alv.
  " Se la tabella di output è vuota, non ha senso mostrare l'ALV
  IF gt_output_alv IS INITIAL.
    MESSAGE 'Nessun dato da visualizzare.' TYPE 'I'.
    EXIT.
  ENDIF.

  TRY.
      " 1. Creazione dell'istanza ALV tramite il metodo factory
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = gt_output_alv ).

      " 2. Configurazione delle Funzioni Standard (Stampa, Esporta, etc.)
      DATA(lo_funzioni) = go_alv->get_functions( ).
      lo_funzioni->set_all( gc_vero ).

      " 3. Configurazione delle Selezioni
      DATA(lo_selezioni) = go_alv->get_selections( ).
      lo_selezioni->set_selection_mode( if_salv_c_selection_mode=>row_column ).

      " 4. Configurazione Colonne (Ottimizzazione e Checkbox)
      DATA(lo_colonne) = go_alv->get_columns( ).
      lo_colonne->set_optimize( gc_vero ).

      " 4a. Rendi la colonna 'BOX' una checkbox editabile
      DATA(lo_colonna_box) = CAST cl_salv_column_table( lo_colonne->get_column( 'BOX' ) ).
      lo_colonna_box->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
      lo_colonna_box->set_short_text( 'Sel.' ). " Testo intestazione colonna

      " 5. Gestione degli Eventi
      CREATE OBJECT go_evento_handler.
      DATA(lo_eventi) = go_alv->get_event( ).
      SET HANDLER go_evento_handler->on_ucomm FOR lo_eventi.
      SET HANDLER go_evento_handler->on_double_click FOR lo_eventi.

      " 6. Impostazione dello Stato GUI personalizzato
      go_alv->set_screen_status(
        pfstatus      = 'ALV_STATUS' " Nome del PF-STATUS creato in SE41
        report        = sy-repid
        set_functions = lo_funzioni->get_functions_to_set( ) ).

      " 7. Visualizzazione dell'ALV
      go_alv->display( ).

    CATCH cx_salv_msg INTO DATA(lx_eccezione_salv).
      MESSAGE lx_eccezione_salv->get_text( ) TYPE 'E'.
  ENDTRY.
ENDFORM.


"----------------------------------------------------------------------
" FORM per gestire i comandi utente (richiamata dall'evento)
"----------------------------------------------------------------------
FORM f_gestisci_ucomm_alv USING iv_ucomm TYPE salv_de_function.

  " Struttura per contenere i dati della riga in elaborazione
  FIELD-SYMBOLS: <fs_riga_output> TYPE ty_s_output_alv.

  CASE iv_ucomm.
    " Gestione dei pulsanti custom (il codice 'PROCESSO' deve
    " corrispondere a quello definito nel PF-STATUS)
    WHEN 'PROCESSO'.
      " Ottiene gli indici di tutte le righe selezionate dall'utente
      DATA(lt_righe_selezionate) = go_alv->get_selections( )->get_selected_rows( ).

      " Controlla se almeno una riga è stata selezionata
      IF lt_righe_selezionate IS INITIAL.
        MESSAGE 'Selezionare almeno una riga tramite checkbox.' TYPE 'S' DISPLAY LIKE 'W'.
        EXIT.
      ENDIF.

      " Cicla su tutte le righe selezionate per processarle
      LOOP AT lt_righe_selezionate INTO DATA(lv_indice_riga).
        " Legge i dati completi della riga dalla tabella principale
        READ TABLE gt_output_alv ASSIGNING <fs_riga_output> INDEX lv_indice_riga.
        IF sy-subrc = 0.
          "
          " *** INSERISCI QUI LA TUA LOGICA APPLICATIVA ***
          " Esempio: chiama una BAPI, aggiorna una tabella, etc.
          " Puoi accedere ai dati della riga tramite il field-symbol <fs_riga_output>
          " Esempio: <fs_riga_output>-campo1, <fs_riga_output>-campo2
          "
          WRITE: / 'Processo la riga con chiave:', <fs_riga_output>-campo1.

          " Al termine dell'elaborazione, deseleziona la checkbox nella tabella dati
          <fs_riga_output>-box = gc_falso.
        ENDIF.
      ENDLOOP.

      " Informa l'utente del completamento
      MESSAGE 'Elaborazione delle righe selezionate completata.' TYPE 'S'.

      " È FONDAMENTALE fare il refresh dell'ALV per mostrare le modifiche
      " (es. le checkbox che sono state deselezionate)
      go_alv->refresh( ).

    " Gestione dei comandi standard di uscita
    WHEN '&SALV_STANDARD_BACK' OR '&SALV_STANDARD_EXIT' OR '&SALV_STANDARD_CANCEL'.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
      " Altri comandi non gestiti
  ENDCASE.

ENDFORM.`;