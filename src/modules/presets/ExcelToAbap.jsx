
export const content = [
  {
    title: 'Esempio 1: Caricamento da singolo excel',
    code: `
   REPORT Z_POPUP_CLEAN.

"**********************************************************************
"* REPORT Z_POPUP_CLEAN
"* DESCRIZIONE:
"* Report per l'importazione di dati da un file Excel (.xls) in una
"* tabella interna ABAP utilizzando il function module standard
"* 'TEXT_CONVERT_XLS_TO_SAP'.
"*
"* FUNZIONALITÀ IMPLEMENTATE:
"*
"* 1.  **Caricamento File da Locale**:
"*     - Utilizzo della classe CL_GUI_FRONTEND_SERVICES per permettere
"*       all'utente di selezionare un file Excel dal proprio computer.
"*
"* 2.  **Conversione Dati Excel -> ABAP**:
"*     - Utilizzo del F.M. 'TEXT_CONVERT_XLS_TO_SAP' per convertire
"*       il contenuto binario del file in una tabella interna strutturata.
"*     - Gestione automatica dell'intestazione del file Excel.
"*
"* 3.  **Visualizzazione Dati Caricati**:
"*     - I dati importati vengono visualizzati in un report ALV classico
"*       (REUSE_ALV_GRID_DISPLAY) per un feedback immediato all'utente.
"*
"* 4.  **Gestione Errori e Messaggistica**:
"*     - Messaggi chiari per l'utente in caso di successo, annullamento
"*       o errori durante il processo di caricamento e conversione.
"**********************************************************************

"----------------------------------------------------------------------
" SEZIONE 1: DEFINIZIONI TIPI E DATI GLOBALI
"----------------------------------------------------------------------

" Struttura di destinazione per i dati letti da Excel.
" Le colonne del file XLS verranno mappate su questi campi in ordine.
TYPES: BEGIN OF ty_s_dati_excel,
         campo1 TYPE c LENGTH 50,
         campo2 TYPE c LENGTH 50,
         valore TYPE i,
         data   TYPE d,
       END OF ty_s_dati_excel.

DATA: gt_dati_caricati TYPE STANDARD TABLE OF ty_s_dati_excel.

"----------------------------------------------------------------------
" SEZIONE 2: SCHERMATA DI SELEZIONE
"----------------------------------------------------------------------

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001. " Opzioni di Caricamento
  PARAMETERS: p_file  TYPE rlgrap-filename OBLIGATORY, " File Excel da Caricare
              p_head  TYPE c AS CHECKBOX DEFAULT 'X'.  " Il file ha un'intestazione
SELECTION-SCREEN END OF BLOCK b1.

"----------------------------------------------------------------------
" EVENTI PER LA SELEZIONE DEL FILE
"----------------------------------------------------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f_seleziona_file CHANGING p_file.

"----------------------------------------------------------------------
" SEZIONE 3: LOGICA PRINCIPALE
"----------------------------------------------------------------------

START-OF-SELECTION.

  IF p_file IS INITIAL.
    MESSAGE TEXT-002 TYPE 'E'. " Selezionare un file prima di procedere.
    RETURN.
  ENDIF.

  PERFORM f_carica_e_converti_dati
    USING p_file
          p_head
    CHANGING gt_dati_caricati.

  IF gt_dati_caricati IS NOT INITIAL.
    PERFORM f_visualizza_dati_in_alv USING gt_dati_caricati.
  ELSE.
    MESSAGE TEXT-003 TYPE 'I'. " Nessun dato caricato o file vuoto.
  ENDIF.

"----------------------------------------------------------------------
" SEZIONE 4: SUBROUTINES (FORMS)
"----------------------------------------------------------------------

FORM f_seleziona_file CHANGING cv_percorso_file TYPE rlgrap-filename.
  DATA: lt_file_table TYPE filetable,
        lv_rc         TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = CONV string( TEXT-004 ) " Seleziona file Excel
      default_extension       = '*.xls'
      file_filter             = 'File Excel (*.xls)|*.xls|Tutti i file (*.*)|*.*'
    CHANGING
      file_table              = lt_file_table
      rc                      = lv_rc
    EXCEPTIONS
      OTHERS                  = 1.

  IF sy-subrc = 0 AND lv_rc = 1.
    DATA ls_file TYPE LINE OF filetable.
    READ TABLE lt_file_table INTO ls_file INDEX 1.
    IF sy-subrc = 0.
      cv_percorso_file = ls_file-filename.
    ENDIF.
  ENDIF.
ENDFORM.

FORM f_carica_e_converti_dati
  USING iv_percorso_file TYPE rlgrap-filename
        iv_ha_intestazione TYPE c
  CHANGING ct_dati_convertiti TYPE STANDARD TABLE.

  DATA: lt_dati_grezzi TYPE truxs_t_text_data.

  " 1. Carica il contenuto binario del file dal frontend
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = CONV string( iv_percorso_file )
      filetype                = 'BIN'
    CHANGING
      data_tab                = lt_dati_grezzi
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      OTHERS                  = 6.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.
  ENDIF.

  " 2. Chiama il function module per la conversione
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = iv_ha_intestazione
      i_tab_raw_data       = lt_dati_grezzi
      i_filename           = iv_percorso_file
    TABLES
      i_tab_converted_data = ct_dati_convertiti
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

  IF sy-subrc <> 0.
    IF sy-subrc = 1.
      MESSAGE TEXT-005 TYPE 'E'. " Errore durante la conversione del file Excel.
    ELSE.
      MESSAGE TEXT-006 TYPE 'E'. " Errore imprevisto in TEXT_CONVERT_XLS_TO_SAP.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_visualizza_dati_in_alv USING pt_dati TYPE STANDARD TABLE.
  DATA: lt_catalogo_campi TYPE slis_t_fieldcat_alv.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = sy-repid
      i_internal_tabname = 'GT_DATI_CARICATI'
      i_inclname         = sy-repid
    CHANGING
      ct_fieldcat        = lt_catalogo_campi
    EXCEPTIONS
      OTHERS             = 1.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = lt_catalogo_campi
      i_grid_title       = TEXT-007 " Dati Caricati da Excel
    TABLES
      t_outtab           = pt_dati
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.
    `
  },
  {
    title: 'Esempio 2: Caricamento da un excel avente più foglie dentro',
    code: `
    REPORT z_template_read_multisheet_excel.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
" Schermata di selezione per permettere all'utente di specificare
" il percorso del file Excel da elaborare.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_file TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* DEFINIZIONE TIPI E DATI GLOBALI
*----------------------------------------------------------------------*
" --- INIZIO SEZIONE DA PERSONALIZZARE ---
" In questa sezione, definisci le strutture interne che conterranno
" i dati mappati da ciascun foglio Excel.

" Esempio: Struttura per i dati del Foglio1
TYPES: BEGIN OF ty_s_dati_foglio1,
         id          TYPE string,
         description TYPE string,
         doc_status  TYPE string,
         " ... Aggiungere altri campi necessari ...
       END OF ty_s_dati_foglio1.

" Esempio: Struttura per i dati del Foglio2
TYPES: BEGIN OF ty_s_dati_foglio2,
         id          TYPE string,
         item_number TYPE string,
         material    TYPE string,
         quantity    TYPE string,
         " ... Aggiungere altri campi necessari ...
       END OF ty_s_dati_foglio2.

" Dichiarazione delle tabelle interne finali
DATA: gt_dati_foglio1 TYPE STANDARD TABLE OF ty_s_dati_foglio1,
      gt_dati_foglio2 TYPE STANDARD TABLE OF ty_s_dati_foglio2.
" --- FINE SEZIONE DA PERSONALIZZARE ---


*----------------------------------------------------------------------*
* EVENTI
*----------------------------------------------------------------------*
" Aggiunge l'help di ricerca (F4) per selezionare il file dal PC
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = p_file.

" Evento principale che avvia l'elaborazione
START-OF-SELECTION.
  PERFORM f_processa_file_excel.

  " Dopo l'elaborazione, i dati sono pronti nelle tabelle globali
  " per ulteriori utilizzi (es. ALV).
  WRITE: / 'Elaborazione completata.'.


*&---------------------------------------------------------------------*
*&      FORM F_PROCESSA_FILE_EXCEL
*&---------------------------------------------------------------------*
" FORM principale che orchestra la lettura e la mappatura dei dati.
FORM f_processa_file_excel.
  " Definisce le tabelle che conterranno i dati grezzi letti da Excel.
  " Ogni riga di Excel viene letta come una singola stringa.
  DATA: lt_grezzi_foglio1 TYPE truxs_t_text_data,
        lt_grezzi_foglio2 TYPE truxs_t_text_data,
        lv_riga_grezza    TYPE string.

  " --- SEZIONE 1: LETTURA DEI FOGLI EXCEL ---
  " Chiama la FORM generica per leggere ciascun foglio di interesse.
  " Assicurati che i nomi dei fogli ('Foglio1', 'Foglio2') corrispondano
  " esattamente a quelli presenti nel tuo file Excel.

  PERFORM f_leggi_singolo_foglio_ole
    USING    p_file
             'Foglio1' " Nome del primo foglio da leggere
    CHANGING lt_grezzi_foglio1.

  PERFORM f_leggi_singolo_foglio_ole
    USING    p_file
             'Foglio2' " Nome del secondo foglio da leggere
    CHANGING lt_grezzi_foglio2.

  " Controllo di sicurezza: se nessuna tabella è stata popolata,
  " il file potrebbe essere vuoto o i fogli non trovati.
  IF lt_grezzi_foglio1 IS INITIAL AND lt_grezzi_foglio2 IS INITIAL.
    MESSAGE 'Errore: file Excel vuoto o fogli non trovati.' TYPE 'E'.
    LEAVE PROGRAM.
  ENDIF.


  " --- SEZIONE 2: MAPPATURA DATI (DA PERSONALIZZARE) ---
  " Questa è la sezione principale da adattare.
  " Per ogni foglio letto, si cicla sui dati grezzi e si "splittano"
  " le colonne nella struttura ABAP corrispondente.

  " 2.1 Mappatura dati del Foglio1
  FIELD-SYMBOLS: <fs_foglio1> TYPE ty_s_dati_foglio1.
  LOOP AT lt_grezzi_foglio1 INTO lv_riga_grezza.
    APPEND INITIAL LINE TO gt_dati_foglio1 ASSIGNING <fs_foglio1>.
    " SPLIT divide la riga in base al carattere di tabulazione.
    " L'ordine dei campi DEVE corrispondere all'ordine delle colonne in Excel.
    SPLIT lv_riga_grezza AT cl_abap_char_utilities=>horizontal_tab
      INTO <fs_foglio1>-id
           <fs_foglio1>-description
           <fs_foglio1>-doc_status.
           " ... Aggiungere gli altri campi della struttura ...
  ENDLOOP.

  " 2.2 Mappatura dati del Foglio2
  FIELD-SYMBOLS: <fs_foglio2> TYPE ty_s_dati_foglio2.
  LOOP AT lt_grezzi_foglio2 INTO lv_riga_grezza.
    APPEND INITIAL LINE TO gt_dati_foglio2 ASSIGNING <fs_foglio2>.
    SPLIT lv_riga_grezza AT cl_abap_char_utilities=>horizontal_tab
      INTO <fs_foglio2>-id
           <fs_foglio2>-item_number
           <fs_foglio2>-material
           <fs_foglio2>-quantity.
           " ... Aggiungere gli altri campi della struttura ...
  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*&      FORM F_LEGGI_SINGOLO_FOGLIO_OLE (RIUTILIZZABILE)
*&---------------------------------------------------------------------*
" FORM generica e riutilizzabile per leggere un singolo foglio da un
" file Excel usando la tecnologia OLE2 (automazione del frontend).
"
" PARAMETRI:
"   --> iv_percorso_file: Percorso completo del file Excel.
"   --> iv_nome_foglio:   Nome del foglio da leggere (case-sensitive).
"   <-- ct_dati_grezzi:   Tabella restituita con i dati grezzi.
"
FORM f_leggi_singolo_foglio_ole
  USING
    iv_percorso_file TYPE rlgrap-filename
    iv_nome_foglio   TYPE string
  CHANGING
    ct_dati_grezzi   TYPE truxs_t_text_data.

  " Dichiarazione degli oggetti OLE per controllare Excel
  DATA: lo_excel    TYPE ole2_object, " L'applicazione Excel
        lo_workbook TYPE ole2_object, " Il file Excel (cartella di lavoro)
        lo_sheet    TYPE ole2_object, " Il singolo foglio di lavoro
        lo_range    TYPE ole2_object, " L'area di celle utilizzate
        lo_cell     TYPE ole2_object. " La singola cella

  " Variabili di supporto per il ciclo di lettura
  DATA: lv_righe    TYPE i,
        lv_colonne  TYPE i,
        lv_riga     TYPE i,
        lv_colonna  TYPE i,
        lv_valore   TYPE string,
        lv_riga_out TYPE string.

  " Crea un'istanza dell'applicazione Excel sul PC dell'utente
  CREATE OBJECT lo_excel 'excel.application'.
  IF sy-subrc <> 0.
    MESSAGE 'Impossibile avviare Excel. Verificare che sia installato.' TYPE 'E'.
    LEAVE PROGRAM.
  ENDIF.

  " Apre il file Excel specificato
  CALL METHOD OF lo_excel 'Workbooks' = lo_workbook.
  CALL METHOD OF lo_workbook 'Open' EXPORTING #1 = iv_percorso_file.
  IF sy-subrc <> 0.
    MESSAGE |File non trovato o non apribile: { iv_percorso_file }| TYPE 'E'.
    LEAVE PROGRAM.
  ENDIF.

  " Seleziona il foglio di lavoro tramite il suo nome
  GET PROPERTY OF lo_excel 'Worksheets' = lo_sheet EXPORTING #1 = iv_nome_foglio.
  IF sy-subrc <> 0.
    " Se il foglio non viene trovato, non è un errore bloccante.
    " Si esce dalla FORM senza leggere dati, permettendo al programma
    " di continuare a leggere gli altri fogli.
    CALL METHOD OF lo_excel 'Quit'.
    FREE OBJECT: lo_sheet, lo_workbook, lo_excel.
    RETURN.
  ENDIF.

  " Attiva il foglio selezionato
  CALL METHOD OF lo_sheet 'Activate'.

  " Determina l'intervallo di celle utilizzate (UsedRange)
  GET PROPERTY OF lo_sheet 'UsedRange' = lo_range.
  GET PROPERTY OF lo_range 'Rows' = lo_range.
  GET PROPERTY OF lo_range 'Count' = lv_righe.
  GET PROPERTY OF lo_range 'Columns' = lo_range.
  GET PROPERTY OF lo_range 'Count' = lv_colonne.

  " Cicla su tutte le righe utilizzate, saltando la prima (intestazione)
  DO lv_righe - 1 TIMES.
    lv_riga = sy-index + 1. " Indice della riga corrente in Excel
    CLEAR lv_riga_out.

    " Cicla su tutte le colonne della riga corrente
    DO lv_colonne TIMES.
      lv_colonna = sy-index.
      GET PROPERTY OF lo_sheet 'Cells' = lo_cell EXPORTING #1 = lv_riga #2 = lv_colonna.

      " Legge il valore della cella come testo visualizzato.
      " Usare 'Text' invece di 'Value' evita problemi di conversione
      " per date, numeri e formati speciali.
      GET PROPERTY OF lo_cell 'Text' = lv_valore.
      FREE OBJECT lo_cell.

      " Concatena i valori delle celle in una singola stringa,
      " separati da un carattere di tabulazione.
      IF lv_colonna = 1.
        lv_riga_out = lv_valore.
      ELSE.
        CONCATENATE lv_riga_out lv_valore INTO lv_riga_out
          SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
      ENDIF.
    ENDDO.
    " Aggiunge la riga completa alla tabella di output
    APPEND lv_riga_out TO ct_dati_grezzi.
  ENDDO.

  " Chiude l'applicazione Excel e rilascia gli oggetti dalla memoria
  CALL METHOD OF lo_excel 'Quit'.
  FREE OBJECT: lo_range, lo_sheet, lo_workbook, lo_excel.

ENDFORM.

    `
  }
];
