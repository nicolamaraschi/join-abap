
export const content = [
  {
    title: 'Esempio 1: Caricamento da singolo excel',
    code: `
    ================================================================================
DOCUMENTAZIONE: LETTURA FILE EXCEL E MAPPATURA IN TABELLA INTERNA ABAP
================================================================================

SCOPO DEL DOCUMENTO
-------------------
Questa guida descrive i passaggi standard per implementare un report ABAP
in grado di leggere i dati da un file Microsoft Excel (.xls) selezionato
dall'utente e di trasferire tali dati in una tabella interna strutturata
per successive elaborazioni.

PREREQUISITI
------------
- Il file Excel da importare deve avere un formato semplice, preferibilmente .xls.
- I dati nel file Excel devono essere contenuti in un singolo foglio.
- I dati devono essere organizzati in colonne, con una riga di intestazione opzionale.
- Il file deve trovarsi su un percorso accessibile dal PC dell'utente (frontend).

--------------------------------------------------------------------------------
SEZIONE 1: DEFINIZIONE DELLA SELECTION-SCREEN
--------------------------------------------------------------------------------

Il primo passo è creare un'interfaccia di selezione che permetta all'utente
di specificare il percorso del file da caricare.

"----------------------------------------------------------------------
" ESEMPIO DI CODICE PER LA SELECTION-SCREEN
"----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blocco1 WITH FRAME TITLE TEXT-001.
" Il parametro 'p_percorso_file' memorizzerà il percorso completo del file.
" È di tipo 'rlgrap-filename', lo standard per i percorsi file.
" 'OBLIGATORY' rende il campo obbligatorio, l'utente non può lasciarlo vuoto.
  PARAMETERS: p_percorso_file TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blocco1.

" NOTA:
" 'TEXT-001' è un Elemento di Testo (Text Element) che deve essere creato
" nel programma tramite la transazione SE38 (Goto -> Text elements).
" Ad esempio, si potrebbe definire TEXT-001 = 'Selezione File da Caricare'.
" L'uso di Elementi di Testo è una best practice per la gestione multilingua.

--------------------------------------------------------------------------------
SEZIONE 2: ABILITARE L'HELP DI RICERCA (F4) PER IL CAMPO FILE
--------------------------------------------------------------------------------

Per migliorare l'usabilità, è fondamentale fornire all'utente una finestra
di dialogo standard per la ricerca e selezione del file sul proprio computer.
Questo si ottiene gestendo l'evento 'AT SELECTION-SCREEN ON VALUE-REQUEST'.

"----------------------------------------------------------------------
" ESEMPIO DI CODICE PER L'HELP DI RICERCA F4
"----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_percorso_file.
" Questo evento scatta quando l'utente posiziona il cursore sul campo
" 'p_percorso_file' e preme il tasto F4 (o clicca sull'icona di help).

" Si richiama il modulo funzione standard 'F4_FILENAME' che mostra la
" finestra di dialogo per la selezione di un file dal sistema operativo.
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      " Il nome del file selezionato dall'utente viene restituito
      " e popolato automaticamente nel parametro 'p_percorso_file'.
      file_name = p_percorso_file
    EXCEPTIONS
      OTHERS    = 1.

--------------------------------------------------------------------------------
SEZIONE 3: LOGICA DI LETTURA E MAPPATURA (EVENTO START-OF-SELECTION)
--------------------------------------------------------------------------------

Questa è la parte principale dell'elaborazione, che avviene dopo che l'utente
ha compilato la selection-screen e premuto F8 (Esegui).

Il processo si articola in 3 fasi:
1. Definizione della struttura dati di destinazione.
2. Chiamata alla funzione standard per leggere il file Excel.
3. Ciclo sui dati grezzi e mappatura nella tabella strutturata.


-- FASE 3.1: Definizione della Struttura Dati di Destinazione

" Prima di leggere il file, bisogna definire una struttura ABAP che
" rispecchi le colonne del file Excel che si vogliono importare.

" Esempio: Il file Excel ha 3 colonne: ID Prodotto, Descrizione, Prezzo
TYPES: BEGIN OF ty_dati_excel,
         id_prodotto TYPE char20,      " Corrisponde alla colonna 1
         descrizione TYPE char50,      " Corrisponde alla colonna 2
         prezzo      TYPE p DECIMALS 2," Corrisponde alla colonna 3
       END OF ty_dati_excel.

" Si dichiara poi la tabella interna che conterrà i dati finali.
DATA: lt_dati_finali TYPE STANDARD TABLE OF ty_dati_excel.


-- FASE 3.2: Chiamata alla Funzione di Lettura 'TEXT_CONVERT_XLS_TO_SAP'

" Questa funzione standard è molto efficace per leggere file .xls semplici.
" Restituisce i dati in una tabella "grezza", dove ogni riga del file
" Excel è una singola stringa con le colonne separate da un tabulatore.

" Dichiarazione della tabella che conterrà i dati grezzi.
DATA: lt_dati_grezzi TYPE STANDARD TABLE OF alsmex_tabline.

CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
  EXPORTING
    i_line_header        = 'X'  " 'X' se il file Excel ha una riga di intestazione da ignorare
    i_tab_raw_data       = lt_dati_grezzi " Tabella dove vengono scritti i dati grezzi
    i_filename           = p_percorso_file " Percorso del file dalla selection-screen
  TABLES
    i_tab_converted_data = lt_dati_finali " Tabella di destinazione (con struttura corretta)
  EXCEPTIONS
    conversion_failed    = 1
    OTHERS               = 2.

IF sy-subrc <> 0.
  " Se la funzione fallisce, mostrare un messaggio di errore ed uscire.
  MESSAGE 'Errore durante la lettura o conversione del file Excel.' TYPE 'E'.
  LEAVE PROGRAM.
ENDIF.

" NOTA SULLE VERSIONI:
" Il parametro TABLES 'i_tab_converted_data' esegue la mappatura automatica
" se i campi della struttura interna hanno lo stesso ordine delle colonne Excel.
" Tuttavia, questo può essere fonte di errori se l'ordine cambia.
" Un approccio più robusto e controllato è quello di leggere i dati nella
" tabella grezza (EXPORTING i_tab_raw_data) e mapparli manually, come
" descritto nella fase 3.3. Il codice di esempio completo userà questo metodo.


-- FASE 3.3: Mappatura Manuale (Approccio Robusto)

" Questo metodo prevede di ciclare sulla tabella dei dati grezzi e di
" "spezzare" ogni riga nelle sue colonne componenti.

DATA: ls_dati_grezzi TYPE alsmex_tabline,
      ls_dati_finali TYPE ty_dati_excel.

" Loop su ogni riga letta dal file Excel
LOOP AT lt_dati_grezzi INTO ls_dati_grezzi.
  " Pulisce la struttura di lavoro ad ogni ciclo
  CLEAR ls_dati_finali.

  " L'istruzione SPLIT divide la stringa grezza (ls_dati_grezzi-value)
  " in base al delimitatore di tabulazione (cl_abap_char_utilities=>horizontal_tab).
  " Ogni pezzo viene inserito sequenzialmente nei campi della struttura di destinazione.
  SPLIT ls_dati_grezzi-value AT cl_abap_char_utilities=>horizontal_tab
    INTO ls_dati_finali-id_prodotto
         ls_dati_finali-descrizione
         ls_dati_finali-prezzo.

  " Aggiunge la riga mappata correttamente alla tabella finale.
  APPEND ls_dati_finali TO lt_dati_finali.
ENDLOOP.

" A questo punto, la tabella 'lt_dati_finali' contiene tutti i dati
" del file Excel, correttamente tipizzati e pronti per essere usati.
" Esempio: visualizzazione dei dati caricati.
IF lt_dati_finali IS NOT INITIAL.
  WRITE: / 'Caricamento completato. Dati importati:'.
  LOOP AT lt_dati_finali INTO ls_dati_finali.
    WRITE: / ls_dati_finali-id_prodotto,
             ls_dati_finali-descrizione,
             ls_dati_finali-prezzo.
  ENDLOOP.
ELSE.
  MESSAGE 'Nessun dato importato dal file.' TYPE 'I'.
ENDIF.
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
