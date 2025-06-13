export const content = `================================================================================
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
ENDIF.`;