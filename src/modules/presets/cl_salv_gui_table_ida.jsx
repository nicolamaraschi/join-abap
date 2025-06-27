export const content = `REPORT zres_ida_documentazione.

"**********************************************************************
"
" OGGETTO: Report di Documentazione per CL_SALV_GUI_TABLE_IDA
"
" SCOPO:
" Questo report serve come esempio completo e funzionante per
" l'utilizzo della classe CL_SALV_GUI_TABLE_IDA.
" L'ALV con IDA (In-Memory Data Acceleration) è progettato per
" visualizzare grandi quantità di dati direttamente dal database
" (tipicamente via Core Data Services - CDS) senza caricarli
" interamente nella memoria del server applicativo, sfruttando
" le capacità di paginazione e aggregazione di SAP HANA.
"
" CARATTERISTICHE DIMOSTRATE:
" 1. Creazione di un ALV IDA basato su una Vista CDS standard.
" 2. Gestione dei parametri di selezione (SELECT-OPTIONS) in modo
"    efficiente con la classe CL_SALV_RANGE_TAB_COLLECTOR.
" 3. Implementazione dell'interfaccia IF_SALV_IDA_CALC_FIELD_HANDLER
"    per aggiungere colonne con dati calcolati dinamicamente
"    (in questo caso, testi standard da ordini di vendita).
" 4. Utilizzo di una classe locale per incapsulare tutta la logica.
" 5. Sostituzione di oggetti custom con equivalenti standard (READ_TEXT)
"    per rendere il report auto-consistente e funzionante.
"**********************************************************************

"----------------------------------------------------------------------
" DICHIARAZIONI GLOBALI
"----------------------------------------------------------------------
" La dichiarazione TABLES è necessaria per la sintassi
" 'SELECT-OPTIONS ... FOR ...', anche se considerata obsoleta.
TABLES: vbak.

"----------------------------------------------------------------------
" SCHERMATA DI SELEZIONE
"----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK bl01 WITH FRAME TITLE TEXT-001.
  " Parametro di selezione per filtrare gli ordini di vendita
  SELECT-OPTIONS: so_vbeln FOR vbak-vbeln.
SELECTION-SCREEN END OF BLOCK bl01.


"**********************************************************************
" CLASSE LOCALE PER LA GESTIONE DELL'ALV IDA
"**********************************************************************
CLASS lcl_gestore_alv_ida DEFINITION.
  PUBLIC SECTION.
    " L'interfaccia IF_SALV_IDA_CALC_FIELD_HANDLER è FONDAMENTALE.
    " Permette di "intercettare" il processo di recupero dati dell'ALV
    " per inserire logica custom e campi calcolati.
    INTERFACES if_salv_ida_calc_field_handler.

    " Metodo statico principale che avvia la creazione e visualizzazione
    " dell'ALV. Funge da punto di ingresso per la logica.
    CLASS-METHODS:
      avvia_visualizzazione.

    " Metodo per impostare le intestazioni delle nuove colonne calcolate.
    METHODS:
      imposta_testi_colonne
        IMPORTING
          io_catalogo_campi TYPE REF TO if_salv_gui_field_catalog_ida.

  PRIVATE SECTION.
    " Struttura dati che definisce i nostri nuovi campi calcolati.
    " Questi campi NON esistono nella vista CDS di origine.
    TYPES:
      BEGIN OF ty_campi_calcolati,
        testo_testata   TYPE c LENGTH 255, " Per contenere il testo di testata
        testo_posizione TYPE c LENGTH 255, " Per contenere il testo di posizione
      END OF ty_campi_calcolati.

    " Struttura dati per ricevere i campi dalla riga della vista CDS.
    " I nomi dei campi devono corrispondere a quelli della vista CDS.
    TYPES:
      BEGIN OF ty_dati_riga_cds,
        salesdocument     TYPE vbeln_va,
        salesdocumentitem TYPE posnr_va,
      END OF ty_dati_riga_cds.

    " Tabella membro per memorizzare i nomi dei campi calcolati che
    " sono stati effettivamente richiesti dall'utente (cioè visibili
    " nel layout ALV). Serve per ottimizzare le performance.
    DATA:
      mt_nomi_campi_calcolati TYPE if_salv_ida_types=>yts_field_name.

ENDCLASS.

"**********************************************************************
" IMPLEMENTAZIONE DELLA CLASSE LOCALE
"**********************************************************************
CLASS lcl_gestore_alv_ida IMPLEMENTATION.

  "--------------------------------------------------------------------
  " METODO 1: Definisce la struttura dei nuovi campi calcolati.
  "--------------------------------------------------------------------
  METHOD if_salv_ida_calc_field_handler~get_calc_field_structure.
    " Questo metodo viene chiamato dal framework ALV IDA per sapere
    " "come sono fatti" i campi calcolati che vogliamo aggiungere.
    " Dobbiamo restituire un oggetto che descrive la struttura dati
    " definita nella sezione PRIVATE (ty_campi_calcolati).

    " Creiamo una variabile locale basata sulla nostra struttura.
    DATA(ls_campi_calcolati) = VALUE ty_campi_calcolati( ).

    " Utilizziamo il servizio RTTI (Run Time Type Identification) per
    " ottenere un oggetto descrittore della nostra struttura.
    ro_calc_field_structure ?= cl_abap_structdescr=>describe_by_data( ls_campi_calcolati ).
  ENDMETHOD.

  "--------------------------------------------------------------------
  " METODO 2: Specifica i campi del DB necessari per i calcoli.
  "--------------------------------------------------------------------
  METHOD if_salv_ida_calc_field_handler~get_requested_fields.
    " Questo metodo è un'ottimizzazione cruciale.
    " Viene chiamato dal framework per chiederci: "Per calcolare i tuoi
    " campi custom, di quali campi della tabella di database originale
    " (la vista CDS) hai bisogno?".
    " In questo modo, il framework aggiungerà questi campi alla SELECT
    " SUL DATABASE solo se i nostri campi calcolati sono visibili.

    " Controlliamo se la colonna calcolata 'TESTO_POSIZIONE' è richiesta.
    IF line_exists( its_calc_field_name[ table_line = 'TESTO_POSIZIONE' ] ).
      " Per leggere il testo di posizione, ci servono il numero
      " del documento (SalesDocument) e il numero di posizione (SalesDocumentItem).
      READ TABLE rts_db_field_name WITH KEY table_line = 'SALESDOCUMENT' TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND 'SALESDOCUMENT' TO rts_db_field_name.
      ENDIF.
      READ TABLE rts_db_field_name WITH KEY table_line = 'SALESDOCUMENTITEM' TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND 'SALESDOCUMENTITEM' TO rts_db_field_name.
      ENDIF.
    ENDIF.

    " Controlliamo se la colonna calcolata 'TESTO_TESTATA' è richiesta.
    IF line_exists( its_calc_field_name[ table_line = 'TESTO_TESTATA' ] ).
      " Per il testo di testata, è sufficiente il numero del documento.
      READ TABLE rts_db_field_name WITH KEY table_line = 'SALESDOCUMENT' TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND 'SALESDOCUMENT' TO rts_db_field_name.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  "--------------------------------------------------------------------
  " METODO 3: Inizio elaborazione di una "pagina" di dati.
  "--------------------------------------------------------------------
  METHOD if_salv_ida_calc_field_handler~start_page.
    " Questo metodo viene chiamato prima che il framework inizi a
    " processare le righe di una pagina di dati da visualizzare.
    " Lo usiamo per memorizzare in una variabile membro quali campi
    " calcolati sono richiesti, così non dobbiamo ricalcolarlo per ogni riga.
    mt_nomi_campi_calcolati = its_calc_field_name.
  ENDMETHOD.

  "--------------------------------------------------------------------
  " METODO 4: Logica di calcolo per una singola riga.
  "--------------------------------------------------------------------
  METHOD if_salv_ida_calc_field_handler~calculate_line.
    " Questo è il cuore della logica. Il metodo viene chiamato per
    " OGNI riga che sta per essere visualizzata nell'ALV.
    DATA: ls_campi_calcolati  TYPE ty_campi_calcolati,
          lt_linee_testo_pos  TYPE STANDARD TABLE OF tline,
          lt_linee_testo_tes  TYPE STANDARD TABLE OF tline,
          ls_dati_riga_cds    TYPE ty_dati_riga_cds. " Struttura locale per i dati

    FIELD-SYMBOLS: <ls_linea> LIKE LINE OF lt_linee_testo_pos.

    TRY.
        " CORREZIONE RUNTIME ERROR:
        " Invece di usare ASSIGN, che causa il dump, usiamo CORRESPONDING
        " per mappare i campi dalla reference is_data_base_line alla nostra
        " struttura locale. Questo approccio è più robusto e compatibile.
        ls_dati_riga_cds = CORRESPONDING #( is_data_base_line ).

        " Controlliamo se dobbiamo calcolare il testo di POSIZIONE.
        IF line_exists( mt_nomi_campi_calcolati[ table_line = 'TESTO_POSIZIONE' ] ).
          IF ls_dati_riga_cds-salesdocument IS NOT INITIAL AND ls_dati_riga_cds-salesdocumentitem IS NOT INITIAL.
            DATA(lv_nome_testo_pos) = CONV tdobname( ls_dati_riga_cds-salesdocument && ls_dati_riga_cds-salesdocumentitem ).

            CALL FUNCTION 'READ_TEXT'
              EXPORTING
                id       = 'TX05' " ID del testo per le posizioni (esempio)
                language = sy-langu
                name     = lv_nome_testo_pos
                object   = 'VBBP' " Oggetto testo per posizioni OdV
              TABLES
                lines    = lt_linee_testo_pos
              EXCEPTIONS
                OTHERS   = 6.

            IF sy-subrc = 0 AND lines( lt_linee_testo_pos ) > 0.
              LOOP AT lt_linee_testo_pos ASSIGNING <ls_linea>.
                ls_campi_calcolati-testo_posizione = ls_campi_calcolati-testo_posizione && ' ' && <ls_linea>-tdline.
              ENDLOOP.
              SHIFT ls_campi_calcolati-testo_posizione LEFT DELETING LEADING space.
            ENDIF.
          ENDIF.
        ENDIF.

        " Controlliamo se dobbiamo calcolare il testo di TESTATA.
        IF line_exists( mt_nomi_campi_calcolati[ table_line = 'TESTO_TESTATA' ] ).
          IF ls_dati_riga_cds-salesdocument IS NOT INITIAL.
            DATA(lv_nome_testo_tes) = CONV tdobname( ls_dati_riga_cds-salesdocument ).

            CALL FUNCTION 'READ_TEXT'
              EXPORTING
                id       = '0001' " ID del testo per le testate (esempio)
                language = sy-langu
                name     = lv_nome_testo_tes
                object   = 'VBBK' " Oggetto testo per testate OdV
              TABLES
                lines    = lt_linee_testo_tes
              EXCEPTIONS
                OTHERS   = 6.

            IF sy-subrc = 0 AND lines( lt_linee_testo_tes ) > 0.
              LOOP AT lt_linee_testo_tes ASSIGNING <ls_linea>.
                ls_campi_calcolati-testo_testata = ls_campi_calcolati-testo_testata && ' ' && <ls_linea>-tdline.
              ENDLOOP.
              SHIFT ls_campi_calcolati-testo_testata LEFT DELETING LEADING space.
            ENDIF.
          ENDIF.
        ENDIF.

        " Restituiamo la struttura con i valori calcolati al framework.
        es_calculated_fields = ls_campi_calcolati.

      CATCH cx_root INTO DATA(lo_errore).
        " Gestione semplice degli errori
        DATA(lv_messaggio) = lo_errore->get_text( ).
        MESSAGE lv_messaggio TYPE 'I'.
    ENDTRY.
  ENDMETHOD.

  "--------------------------------------------------------------------
  " METODO 5: Fine elaborazione di una "pagina" di dati.
  "--------------------------------------------------------------------
  METHOD if_salv_ida_calc_field_handler~end_page.
    " Chiamato dopo aver processato tutte le righe di una pagina.
    " Utile per rilasciare risorse o fare calcoli aggregati sulla pagina.
    " In questo esempio, non è necessario fare nulla.
    CLEAR mt_nomi_campi_calcolati.
  ENDMETHOD.

  "--------------------------------------------------------------------
  " METODO : Imposta le intestazioni delle colonne calcolate.
  "--------------------------------------------------------------------
  METHOD imposta_testi_colonne.
    " Poiché i nostri campi calcolati non esistono nel DDIC, non hanno
    " testi di default. Dobbiamo impostarli manually.
    " Passiamo i testi direttamente come letterali per massima compatibilità,
    " come mostrato nello snippet di esempio.

    TRY.
        " Imposta l'intestazione per il campo TESTO_TESTATA.
        io_catalogo_campi->set_field_header_texts(
          iv_field_name   = 'TESTO_TESTATA'
          iv_header_text  = 'Testo di Testata Ordine'
          iv_tooltip_text = 'Testo di Testata Ordine' ).

        " Imposta l'intestazione per il campo TESTO_POSIZIONE.
        io_catalogo_campi->set_field_header_texts(
          iv_field_name   = 'TESTO_POSIZIONE'
          iv_header_text  = 'Testo di Posizione Ordine'
          iv_tooltip_text = 'Testo di Posizione Ordine' ).

      CATCH cx_salv_ida_contract_violation.
        " Errore se il campo non esiste.
    ENDTRY.
  ENDMETHOD.

  "--------------------------------------------------------------------
  " METODO : Punto di ingresso per avviare il report.
  "--------------------------------------------------------------------
  METHOD avvia_visualizzazione.
    " Creiamo un'istanza della nostra classe stessa, che fungerà da
    " gestore (handler) per i campi calcolati.
    DATA(lo_gestore_campi_calcolati) = NEW lcl_gestore_alv_ida( ).

    TRY.
        " 1. CREAZIONE ISTANZA ALV IDA
        " Creiamo l'ALV legandolo a una vista CDS e passando l'istanza
        " del nostro gestore per i campi calcolati.
        " NOTA: Ho sostituito 'ZCDS_SALES_ORDER_LIST' con la CDS standard
        "       'I_SALESDOCUMENTITEM' per rendere il codice funzionante.
        DATA(lo_alv_ida) = cl_salv_gui_table_ida=>create_for_cds_view(
          iv_cds_view_name      = 'I_SALESDOCUMENTITEM'
          io_calc_field_handler = lo_gestore_campi_calcolati
        ).

        " 2. GESTIONE SELECT-OPTIONS
        " La classe CL_SALV_RANGE_TAB_COLLECTOR è il modo corretto per
        " passare i select-options a un ALV IDA.
        DATA(lo_raccoglitore_range) = NEW cl_salv_range_tab_collector( ).

        " Aggiungiamo il nostro select-option, mappandolo al campo
        " corrispondente nella vista CDS ('SALESDOCUMENT').
        lo_raccoglitore_range->add_ranges_for_name(
          iv_name   = 'SALESDOCUMENT'
          it_ranges = so_vbeln[]
        ).

        " Recuperiamo i range raccolti in un formato compatibile con l'ALV.
        lo_raccoglitore_range->get_collected_ranges(
          IMPORTING
            et_named_ranges = DATA(lt_select_options)
        ).

        " Applichiamo i filtri all'istanza dell'ALV.
        lo_alv_ida->set_select_options( it_ranges = lt_select_options ).

        " 3. IMPOSTAZIONE TESTI COLONNE CUSTOM
        " Chiamiamo il nostro metodo per impostare le intestazioni.
        lo_gestore_campi_calcolati->imposta_testi_colonne( lo_alv_ida->field_catalog( ) ).

        " 4. VISUALIZZAZIONE
        " Mostriamo l'ALV a tutto schermo.
        lo_alv_ida->fullscreen( )->display( ).

      CATCH cx_root INTO DATA(lo_eccezione_ida).
        " Gestione specifica per errori dell'ALV IDA.
        MESSAGE lo_eccezione_ida->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.


"**********************************************************************
" EVENTO DI START-OF-SELECTION
"**********************************************************************
START-OF-SELECTION.
  " Punto di ingresso del programma.
  " Chiama il metodo statico della nostra classe per avviare tutto il processo.
  lcl_gestore_alv_ida=>avvia_visualizzazione( ).
`;