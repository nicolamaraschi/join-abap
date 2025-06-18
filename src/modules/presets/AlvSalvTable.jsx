export const content = `REPORT z_test_salv_single_report.

"----------------------------------------------------------------------
" DICHIARAZIONI GLOBALI E TIPI
" Sezione dedicata alla dichiarazione di tutte le variabili globali,
" costanti e tipi di dati utilizzati nel report.
"----------------------------------------------------------------------
TABLES: makt, mara. " Dichiarazione di tabelle del dizionario dati (per accesso diretto ai campi)

" Definizione della struttura e tabella interna per l'output dell'ALV.
" Include un campo 'box' per la selezione tramite checkbox.
TYPES: BEGIN OF ty_s_output_alv,
         box      TYPE abap_bool, " Campo per checkbox di selezione riga
         matnr    TYPE makt-matnr, " Codice materiale
         maktx    TYPE makt-maktx, " Testo breve materiale
         meins    TYPE mara-meins, " Unità di misura base
         mtart    TYPE mara-mtart, " Tipo materiale
       END OF ty_s_output_alv.

DATA: gt_output_alv TYPE STANDARD TABLE OF ty_s_output_alv WITH EMPTY KEY. " Tabella interna per i dati ALV

" Variabili globali per l'istanza dell'ALV e la gestione degli eventi.
DATA: go_alv            TYPE REF TO cl_salv_table,
      go_evento_handler TYPE REF TO lcl_evento_handler.

" Costanti booleane per chiarezza (vero/falso).
CONSTANTS: gc_vero  TYPE abap_bool VALUE abap_true,
           gc_falso TYPE abap_bool VALUE abap_false.

"----------------------------------------------------------------------
" DEFINIZIONE E IMPLEMENTAZIONE CLASSE LOCALE PER GESTIONE EVENTI ALV
" Questa classe gestisce gli eventi utente dell'ALV (es. comandi custom,
" doppio click sulle righe). Deve essere definita e implementata prima
" del suo utilizzo nel report.
"----------------------------------------------------------------------
CLASS lcl_evento_handler DEFINITION FINAL.
  PUBLIC SECTION.
    " Metodo per gestire i comandi utente provenienti dalla toolbar dell'ALV.
    METHODS: on_ucomm        FOR EVENT added_function OF cl_salv_events_table
                             IMPORTING e_salv_function.

    " Metodo per gestire il doppio click su una riga dell'ALV.
    " I parametri 'row' e 'column' forniscono l'indice della riga e
    " il nome della colonna cliccata.
    METHODS: on_double_click FOR EVENT double_click OF cl_salv_events_table
                             IMPORTING row column.
ENDCLASS.


CLASS lcl_evento_handler IMPLEMENTATION.
  METHOD on_ucomm.
    " Delega la gestione del comando utente a una FORM locale.
    PERFORM f_gestisci_ucomm_alv USING e_salv_function.
  ENDMETHOD.

  METHOD on_double_click.
    " Gestione del doppio click su una riga dell'ALV.
    DATA: ls_riga_cliccata TYPE ty_s_output_alv.

    " Legge la riga completa della tabella ALV basandosi sull'indice fornito dall'evento.
    READ TABLE gt_output_alv INDEX row INTO ls_riga_cliccata.
    IF sy-subrc = 0.
      " Visualizza un messaggio con il codice materiale della riga cliccata.
      MESSAGE 'Doppio click sulla riga Materiale: ' && ls_riga_cliccata-matnr TYPE 'S'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

"----------------------------------------------------------------------
" EVENTI PROGRAMMA
" Definisce la sequenza di esecuzione del report.
"----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  " Punto di elaborazione per la schermata di selezione.
  " Non ci sono parametri di selezione, ma il blocco viene mantenuto
  " per coerenza strutturale.

START-OF-SELECTION.
  " Esegue l'estrazione dei dati e poi visualizza l'ALV.
  PERFORM f_estrai_dati.
  PERFORM f_imposta_alv.

"----------------------------------------------------------------------
" FORM f_estrai_dati
" Esegue l'estrazione dei dati necessari dal database per popolare l'ALV.
" L'estrazione è limitata a un numero fisso di record per scopi di test.
"----------------------------------------------------------------------
FORM f_estrai_dati.

ENDFORM.

"----------------------------------------------------------------------
" FORM f_imposta_alv
" Configura e visualizza la griglia ALV utilizzando CL_SALV_TABLE.
"----------------------------------------------------------------------
FORM f_imposta_alv.
  " Controlla se ci sono dati da visualizzare.
  IF gt_output_alv IS INITIAL.
    MESSAGE 'Nessun dato da visualizzare.' TYPE 'I'.
    EXIT.
  ENDIF.

  TRY.
      " Crea l'istanza dell'oggetto ALV utilizzando il metodo factory.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = gt_output_alv ).

      " Abilita tutte le funzioni standard dell'ALV (es. stampa, esporta).
      DATA(lo_funzioni) = go_alv->get_functions( ).
      lo_funzioni->set_all( gc_vero ).

      " Imposta la modalità di selezione delle righe e colonne (multipla).
      DATA(lo_selezioni) = go_alv->get_selections( ).
      lo_selezioni->set_selection_mode( if_salv_c_selection_mode=>row_column ).

      " Ottiene l'oggetto per la gestione delle colonne e ottimizza la larghezza.
      DATA(lo_colonne) = go_alv->get_columns( ).
      lo_colonne->set_optimize( gc_vero ).

      " Ridenomina le etichette delle colonne per una migliore leggibilità.
      lo_colonne->get_column( 'MATNR' )->set_long_text( 'Codice Materiale' ).
      lo_colonne->get_column( 'MAKTX' )->set_long_text( 'Descrizione Materiale' ).
      lo_colonne->get_column( 'MEINS' )->set_long_text( 'Unità di Misura' ).
      lo_colonne->get_column( 'MTART' )->set_long_text( 'Tipo Materiale' ).

      " Configura la colonna 'BOX' come checkbox cliccabile e ne imposta il testo.
      DATA(lo_colonna_box) = CAST cl_salv_column_table( lo_colonne->get_column( 'BOX' ) ).
      lo_colonna_box->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
      lo_colonna_box->set_short_text( 'Sel.' ).
      lo_colonna_box->set_long_text( 'Seleziona Riga' ).

      " Esempio di logica per nascondere colonne condizionalmente (simulando una modalità test).
      IF gc_vero = abap_true. " Questa condizione è sempre vera in questo esempio.
        " La colonna 'MTART' viene resa invisibile quando questa logica è attiva.
        lo_colonne->get_column( 'MTART' )->set_visible( gc_falso ).
      ENDIF.

      " Crea l'handler degli eventi e registra i metodi per i comandi utente e il doppio click.
      CREATE OBJECT go_evento_handler.
      DATA(lo_eventi) = go_alv->get_event( ).
      SET HANDLER go_evento_handler->on_ucomm FOR lo_eventi.
      SET HANDLER go_evento_handler->on_double_click FOR lo_eventi.

      " Imposta lo Status GUI personalizzato per l'ALV.
      " NOTA: 'STANDARD_ALV' deve esistere e essere attivo per questo report in SE41.
      go_alv->set_screen_status(
        pfstatus    = 'STANDARD_ALV'
        report      = sy-repid ).

      " Visualizza la griglia ALV.
      go_alv->display( ).

    CATCH cx_salv_msg INTO DATA(lx_eccezione_salv).
      " Gestisce eventuali eccezioni durante la creazione o visualizzazione dell'ALV.
      MESSAGE lx_eccezione_salv->get_text( ) TYPE 'E'.
  ENDTRY.
ENDFORM.

"----------------------------------------------------------------------
" FORM f_gestisci_ucomm_alv
" Gestisce i comandi utente attivati dall'ALV.
" Il parametro 'iv_ucomm' contiene il codice funzione del comando eseguito.
"----------------------------------------------------------------------
FORM f_gestisci_ucomm_alv USING iv_ucomm TYPE salv_de_function.
  FIELD-SYMBOLS: <fs_riga_output> TYPE ty_s_output_alv.

  CASE iv_ucomm.
    WHEN 'PROO'. " Comando custom per elaborare le righe selezionate.
      " Ottiene gli indici di tutte le righe selezionate dall'utente.
      DATA(lt_righe_selezionate) = go_alv->get_selections( )->get_selected_rows( ).

      IF lt_righe_selezionate IS INITIAL.
        " Se nessuna riga è selezionata, mostra un avviso.
        MESSAGE 'Selezionare almeno una riga tramite checkbox.' TYPE 'S' DISPLAY LIKE 'W'.
        EXIT.
      END IF.

      " Cicla su tutte le righe selezionate per elaborarle.
      LOOP AT lt_righe_selezionate INTO DATA(lv_indice_riga).
        " Legge i dati completi della riga selezionata dalla tabella ALV principale.
        READ TABLE gt_output_alv ASSIGNING <fs_riga_output> INDEX lv_indice_riga.
        IF sy-subrc = 0.
          " Esempio di logica applicativa: visualizza il materiale della riga processata.
          MESSAGE |Processo la riga con materiale: { <fs_riga_output>-matnr }| TYPE 'I'.
          " Deseleziona la checkbox dopo l'elaborazione (simulazione).
          <fs_riga_output>-box = gc_falso.
        END IF.
      END LOOP.

      " Messaggio di completamento e refresh dell'ALV per mostrare le modifiche.
      MESSAGE 'Elaborazione delle righe selezionate completata.' TYPE 'S'.
      go_alv->refresh( ).

    WHEN '&SALV_STANDARD_BACK' OR '&SALV_STANDARD_EXIT' OR '&SALV_STANDARD_CANCEL'.
      " Gestisce i comandi standard di uscita dall'ALV.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
      " Gestione per altri comandi non esplicitamente gestiti.
  END CASE.
ENDFORM.

`;
