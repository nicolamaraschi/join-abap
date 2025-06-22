




export const content = [
  {
    title: 'Esempio 1: ALV Report con Gestione Eventi e Checkbox',
    code: ` REPORT z_test_salv_single_report.

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
      "DATA(lo_colonna_box) = CAST cl_salv_column_table( lo_colonne->get_column( 'BOX' ) ).
      "lo_colonna_box->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
      "lo_colonna_box->set_short_text( 'Sel.' ).
      "lo_colonna_box->set_long_text( 'Seleziona Riga' ).

      lo_colonne->get_colum( 'BOX' )->set_visible( gc_falso ).

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
`
  },
  {
    title: 'Esempio 2: ALV Semplice con Colonne Nascoste',
    code: `REPORT z_test_salv_avanzato.


"----------------------------------------------------------------------
"& SECTION 1: DICHIARAZIONI GLOBALI E TIPI
"----------------------------------------------------------------------
TABLES: makt, mara.

" --- TYPE-POOLS per la libreria SALV
TYPE-POOLS: salv.

" --- Definizione della struttura e tabella interna per l'output dell'ALV
TYPES: BEGIN OF ty_s_output_alv,
         box          TYPE abap_bool,
         matnr        TYPE makt-matnr,
         maktx        TYPE makt-maktx,
         meins        TYPE mara-meins,
         mtart        TYPE mara-mtart,
         icona_mtart  TYPE icon_d,
         " Il campo per il colore della riga è stato rimosso dalla logica
         " di visualizzazione per risolvere il dump.
* colore_riga  TYPE c LENGTH 4,
       END OF ty_s_output_alv.

DATA: gt_output_alv TYPE STANDARD TABLE OF ty_s_output_alv WITH EMPTY KEY.

" --- Variabili globali per l'istanza dell'ALV
DATA: go_alv TYPE REF TO cl_salv_table.

" --- Costanti booleane
CONSTANTS: gc_vero  TYPE abap_bool VALUE abap_true,
           gc_falso TYPE abap_bool VALUE abap_false.

"----------------------------------------------------------------------
"& SECTION 2: DEFINIZIONE E IMPLEMENTAZIONE CLASSE LOCALE
"----------------------------------------------------------------------
CLASS lcl_evento_handler DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS: on_ucomm FOR EVENT added_function OF cl_salv_events_table
      IMPORTING e_salv_function.
    METHODS: on_double_click FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column.
ENDCLASS.

" --- Dichiarazione dell'handler DOPO la definizione della classe
DATA: go_evento_handler TYPE REF TO lcl_evento_handler.

CLASS lcl_evento_handler IMPLEMENTATION.
  METHOD on_ucomm.
    PERFORM f_gestisci_ucomm_alv USING e_salv_function.
  ENDMETHOD.
  METHOD on_double_click.
    DATA: ls_riga_cliccata TYPE ty_s_output_alv.
    READ TABLE gt_output_alv INDEX row INTO ls_riga_cliccata.
    IF sy-subrc = 0.
      MESSAGE 'Doppio click sulla riga Materiale: ' && ls_riga_cliccata-matnr TYPE 'S'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

"----------------------------------------------------------------------
"& SECTION 3: BLOCCO DI ELABORAZIONE PRINCIPALE
"----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM f_estrai_dati.
  PERFORM f_imposta_alv.

"----------------------------------------------------------------------
"& SECTION 4: IMPLEMENTAZIONE DELLE FORM
"----------------------------------------------------------------------

"&---------------------------------------------------------------------*
"&      FORM f_estrai_dati
"&---------------------------------------------------------------------*
FORM f_estrai_dati.
  " --- Includi le costanti per le icone (es. icon_green_light)
  INCLUDE <icon>.

  " --- Estrazione dati grezzi
  " CORREZIONE: Aggiunto 'CORRESPONDING FIELDS OF' per garantire
  " il corretto popolamento di tutti i campi, incluso MTART.
  SELECT
      a~matnr, b~maktx, a~meins, a~mtart
    FROM mara AS a
    INNER JOIN makt AS b ON a~matnr = b~matnr
    WHERE b~spras = @sy-langu
    INTO CORRESPONDING FIELDS OF TABLE @gt_output_alv
    UP TO 100 ROWS.

  IF sy-subrc <> 0.
    MESSAGE 'Nessun materiale trovato nel sistema.' TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  " --- Elaborazione post-estrazione per icone
  FIELD-SYMBOLS: <fs_output> LIKE LINE OF gt_output_alv.
  LOOP AT gt_output_alv ASSIGNING <fs_output>.
    " Ora che MTART è popolato correttamente, questa logica funzionerà.
    CASE <fs_output>-mtart.
      WHEN 'ROH'.
        <fs_output>-icona_mtart = icon_yellow_light.
      WHEN 'FERT'.
        <fs_output>-icona_mtart = icon_green_light.
      WHEN 'ERSA'.
        <fs_output>-icona_mtart = icon_red_light.
      WHEN OTHERS.
        CLEAR <fs_output>-icona_mtart.
    ENDCASE.
  ENDLOOP.
ENDFORM.

"&---------------------------------------------------------------------*
"&      FORM f_imposta_alv
"&---------------------------------------------------------------------*
FORM f_imposta_alv.
  IF gt_output_alv IS INITIAL.
    MESSAGE 'Nessun dato da visualizzare.' TYPE 'I'.
    EXIT.
  ENDIF.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = gt_output_alv ).

      " *** 1. FUNZIONI STANDARD ***
      DATA(lo_funzioni) = go_alv->get_functions( ).
      lo_funzioni->set_all( gc_vero ).

      " *** 2. IMPOSTAZIONI DI VISUALIZZAZIONE ***
      DATA(lo_selezioni) = go_alv->get_selections( ).
      lo_selezioni->set_selection_mode( if_salv_c_selection_mode=>row_column ).

          " Imposta la modalità di selezione delle righe e colonne (multipla).
      lo_selezioni = go_alv->get_selections( ).
      lo_selezioni->set_selection_mode( if_salv_c_selection_mode=>row_column ).

      DATA(lo_colonne) = go_alv->get_columns( ).
      lo_colonne->set_optimize( gc_vero ).



      " *** 3. PERSONALIZZAZIONE COLONNE ***
      lo_colonne->get_column( 'MATNR' )->set_long_text( 'Codice Materiale' ).
      lo_colonne->get_column( 'MAKTX' )->set_long_text( 'Descrizione Materiale' ).
      lo_colonne->get_column( 'MEINS' )->set_long_text( 'Unità di Misura' ).
      lo_colonne->get_column( 'MTART' )->set_long_text( 'Tipo Materiale' ).

      " *** 4. CONFIGURAZIONE ICONE E CHECKBOX ***

      " Imposto la colonna delle icone
      TRY.
          DATA(lo_colonna_icona) = CAST cl_salv_column_table( lo_colonne->get_column( 'ICONA_MTART' ) ).
          lo_colonna_icona->set_icon( if_salv_c_bool_sap=>true ).
          lo_colonna_icona->set_long_text( 'Icona Tipo' ).
        CATCH cx_salv_not_found.
      ENDTRY.

*      " Imposto il tipo di cella per la checkbox
*      DATA(lo_colonna_box) = CAST cl_salv_column_table( lo_colonne->get_column( 'BOX' ) ).
*      lo_colonna_box->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
*      lo_colonna_box->set_short_text( 'Sel.' ).

        "DISABILITO LA COLONNA BOX PERCHE TANTO HO LA COLONNA SELEZIONE STADANRD DI SALV QUINDI NON SERVE
         lo_colonne->get_column( 'BOX' )->set_visible( gc_falso ).

      " !!! CORREZIONE DUMP !!!
      " La seguente riga è stata COMMENTATA perché causa un'eccezione
      " CX_SALV_INVALID_INPUT quando viene usata insieme a set_cell_type.
      " Per colorare le righe in questo scenario, bisognerebbe usare una
      " tecnica più complessa (colonna di tipo LVC_T_SCOL).
      " Per stabilità, la colorazione delle righe è stata rimossa.
      " lo_colonne->set_color_column( 'COLORE_RIGA' ).


      " *** 5. AGGIUNTA DEI TOOLTIP PER LE ICONE ***
      DATA(lo_impostazioni_funz) = go_alv->get_functional_settings( ).
      DATA(lo_tooltips) = lo_impostazioni_funz->get_tooltips( ).
      DATA lv_valore_tooltip TYPE lvc_value.
      TRY.
          lv_valore_tooltip = icon_yellow_light.
          lo_tooltips->add_tooltip( type = cl_salv_tooltip=>c_type_icon value = lv_valore_tooltip tooltip = 'Materia Prima (ROH)' ).
          lv_valore_tooltip = icon_green_light.
          lo_tooltips->add_tooltip( type = cl_salv_tooltip=>c_type_icon value = lv_valore_tooltip tooltip = 'Prodotto Finito (FERT)' ).
          lv_valore_tooltip = icon_red_light.
          lo_tooltips->add_tooltip( type = cl_salv_tooltip=>c_type_icon value = lv_valore_tooltip tooltip = 'Parte di Ricambio (ERSA)' ).
        CATCH cx_salv_existing.
      ENDTRY.

      " *** 6. ORDINAMENTI E AGGREGAZIONI ***
      DATA(lo_sorts) = go_alv->get_sorts( ).
      lo_sorts->add_sort( columnname = 'MTART' ).
      " Rimosso per evitare dump su campi non numerici
      " DATA(lo_aggregations) = go_alv->get_aggregations( ).
      " lo_aggregations->add_aggregation( columnname = 'MATNR' aggregation = if_salv_c_aggregation=>total ).

      " *** 7. GESTIONE EVENTI E STATUS GUI ***
      CREATE OBJECT go_evento_handler.
      DATA(lo_eventi) = go_alv->get_event( ).
      SET HANDLER go_evento_handler->on_ucomm FOR lo_eventi.
      SET HANDLER go_evento_handler->on_double_click FOR lo_eventi.
      go_alv->set_screen_status( pfstatus = 'SALV_STANDARD' report = sy-repid ).

      " *** 8. VISUALIZZA L'ALV ***
      go_alv->display( ).

    CATCH cx_salv_msg cx_salv_not_found cx_salv_existing cx_salv_data_error INTO DATA(lx_eccezione_salv).
      MESSAGE lx_eccezione_salv->get_text( ) TYPE 'E'.
  ENDTRY.
ENDFORM.

"&---------------------------------------------------------------------*
"&      FORM f_gestisci_ucomm_alv
"&---------------------------------------------------------------------*
FORM f_gestisci_ucomm_alv USING iv_ucomm TYPE salv_de_function.
  FIELD-SYMBOLS: <fs_riga_output> TYPE ty_s_output_alv.
  CASE iv_ucomm.
    WHEN 'PROO'. " Pulsante 'Processa'
      DATA(lt_righe_selezionate) = go_alv->get_selections( )->get_selected_rows( ).
      IF lt_righe_selezionate IS INITIAL.
        MESSAGE 'Selezionare almeno una riga tramite checkbox.' TYPE 'S' DISPLAY LIKE 'W'.
        EXIT.
      ENDIF.
      LOOP AT lt_righe_selezionate INTO DATA(lv_indice_riga).
        READ TABLE gt_output_alv ASSIGNING <fs_riga_output> INDEX lv_indice_riga.
        IF sy-subrc = 0.
          MESSAGE |Processo la riga con materiale: { <fs_riga_output>-matnr }| TYPE 'I'.
          <fs_riga_output>-box = gc_falso.
        ENDIF.
      ENDLOOP.
      MESSAGE 'Elaborazione delle righe selezionate completata.' TYPE 'S'.
      go_alv->refresh( ).
  ENDCASE.
ENDFORM.
`
  }
];