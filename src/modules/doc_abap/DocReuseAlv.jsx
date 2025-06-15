export const content = `
Guida Completa ai Moduli Funzione ALV: REUSE_ALV_GRID_DISPLAY e REUSE_ALV_FIELDCATALOG_MERGE
=============================================================================================

### Introduzione

Questo documento si propone come una guida esaustiva e definitiva per l'utilizzo dei moduli funzione \`REUSE_ALV_FIELDCATALOG_MERGE\` e \`REUSE_ALV_GRID_DISPLAY\`. Sebbene SAP abbia introdotto un framework orientato agli oggetti (CL_SALV_TABLE), l'approccio procedurale classico rimane estremamente diffuso e rilevante per la manutenzione di codice esistente.

Il flusso di lavoro canonico si articola in due fasi distinte:
1.  **Fase 1: Definizione della Struttura (Metadati):** Si descrive ogni colonna della griglia creando un "catalogo campi" (field catalog). Il modulo \`REUSE_ALV_FIELDCATALOG_MERGE\` automatizza questo compito.
2.  **Fase 2: Visualizzazione dei Dati (Rendering):** Si passano i dati e il catalogo campi a \`REUSE_ALV_GRID_DISPLAY\` per renderizzare la griglia a schermo.

---

Sezione 1: La Fondazione – Definire la Struttura con REUSE_ALV_FIELDCATALOG_MERGE
--------------------------------------------------------------------------------------

### 1.1. Scopo e Vantaggi dell'Automazione

Il ruolo primario del modulo è sollevare lo sviluppatore dal compito di creare manualmente il catalogo campi. La best practice consolidata è il modello **"Genera e Modifica"**: si genera un catalogo di base e poi lo si raffina con un \`LOOP AT\` per personalizzare le singole colonne.

### 1.2. Analisi Dettagliata dell'Interfaccia del Modulo Funzione

| Parametro            | Tipo       | Descrizione                                                                      | Esempio di Valore                         |
|----------------------|------------|----------------------------------------------------------------------------------|-------------------------------------------|
| I_PROGRAM_NAME       | EXPORTING  | Nome del programma chiamante. Essenziale per l'analisi del codice.               | \`sy-repid\`                                |
| I_INTERNAL_TABNAME   | EXPORTING  | Nome (stringa) della tabella interna o struttura dati di riferimento.            | \`'GT_MAKT'\`                             |
| I_STRUCTURE_NAME     | EXPORTING  | Nome della struttura o tabella del DDIC. Alternativa a I_INTERNAL_TABNAME.       | \`'MAKT'\`                                |
| I_BYPASSING_BUFFER   | EXPORTING  | Se 'X', forza la rigenerazione del catalogo ignorando la cache.                  | \`'X'\`                                     |
| CT_FIELDCAT          | CHANGING   | Tabella interna che verrà popolata con il catalogo campi generato.               | \`gt_fieldcat\` (tipo \`slis_t_fieldcat_alv\`) |

### 1.3. Esempio Pratico e Risoluzione dei Problemi Comuni

\`\`\`abap
REPORT z_demo_alv_merge.

TYPE-POOLS: slis.

DATA:
  gt_makt     TYPE STANDARD TABLE OF makt,
  gt_fieldcat TYPE slis_t_fieldcat_alv.

START-OF-SELECTION.
  SELECT * FROM makt INTO TABLE gt_makt UP TO 50 ROWS.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = sy-repid
      i_structure_name   = 'MAKT'
    CHANGING
      ct_fieldcat        = gt_fieldcat
    EXCEPTIONS OTHERS   = 1.

  LOOP AT gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    CASE <fs_fcat>-fieldname.
      WHEN 'SPRAS'.
        <fs_fcat>-seltext_m = 'Lingua'.
      WHEN 'MAKTG'.
        <fs_fcat>-no_out = 'X'.
    ENDCASE.
  ENDLOOP.
\`\`\`

---

Sezione 2: Visualizzare la Griglia – Guida a REUSE_ALV_GRID_DISPLAY
----------------------------------------------------------------------

### 2.1. La Chiamata Fondamentale: Parametri Essenziali

* **I_CALLBACK_PROGRAM (EXPORTING):** \`sy-repid\`.
* **IT_FIELDCAT (EXPORTING):** Il catalogo campi preparato.
* **T_OUTTAB (TABLES):** La tabella interna con i dati.

### 2.2. Controllare l'Aspetto: La Struttura IS_LAYOUT (\`slis_layout_alv\`)

| Campo               | Descrizione                                                            |
|---------------------|------------------------------------------------------------------------|
| zebra               | Se 'X', le righe vengono visualizzate con colori alternati.             |
| colwidth_optimize   | Se 'X', la larghezza delle colonne viene adattata al contenuto.        |
| box_fieldname       | Nome del campo per le checkbox di selezione riga.                      |
| info_fieldname      | Nome del campo per la colorazione della riga.                          |
| grid_title          | Titolo descrittivo visualizzato sopra la griglia.                      |

### 2.3. Controllare le Colonne: Proprietà del Catalogo Campi (\`slis_fieldcat_alv\`)

| Campo       | Descrizione                                                      |
|-------------|------------------------------------------------------------------|
| fieldname   | Nome del campo corrispondente nella tabella dati.                |
| seltext_m   | Testo dell'intestazione della colonna.                           |
| do_sum      | Se 'X', calcola il totale per la colonna numerica.               |
| edit        | Se 'X', la colonna è resa modificabile.                          |
| key         | Se 'X', la colonna è trattata come campo chiave (fissa).         |
| no_out      | Se 'X', la colonna è nascosta per default.                       |
| cfieldname  | Nome del campo che contiene il codice valuta di riferimento.     |
| qfieldname  | Nome del campo che contiene l'unità di misura di riferimento.    |

---

Sezione 3: Creare Report ALV Interattivi e Dinamici
-----------------------------------------------------

### 3.1. Gestire le Azioni dell'Utente: I_CALLBACK_USER_COMMAND

Il parametro \`I_CALLBACK_USER_COMMAND\` è il meccanismo principale per rendere un ALV reattivo. Si passa il nome di una subroutine (FORM) che verrà chiamata ogni volta che l'utente esegue un'azione.

La subroutine di callback deve avere una firma ben precisa: \`FORM user_command USING r_ucomm TYPE sy-ucomm rs_selfield TYPE slis_selfield\`.
I due parametri passati sono fondamentali:
* **r_ucomm:** Contiene il codice funzione (\`sy-ucomm\`) dell'azione eseguita (es. \`&IC1\` per il doppio clic).
* **rs_selfield:** È una struttura che fornisce il contesto dell'azione (riga, colonna, valore, etc.).

\`\`\`abap
* Nella chiamata a REUSE_ALV_GRID_DISPLAY:
  i_callback_user_command = 'HANDLE_USER_COMMAND'

*...

* Definizione della subroutine di callback:
FORM handle_user_command USING r_ucomm TYPE sy-ucomm
                              rs_selfield TYPE slis_selfield.
  
  " Controlla se l'azione è un doppio clic
  IF r_ucomm = '&IC1'.
    
    " Legge i dati della riga selezionata dalla tabella dati principale
    READ TABLE gt_makt INDEX rs_selfield-tabindex INTO DATA(ls_makt).
    IF sy-subrc = 0.
      
      " Imposta il Parameter ID per il materiale e chiama la transazione
      SET PARAMETER ID 'MAT' FIELD ls_makt-matnr.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
      
    ENDIF.
  ENDIF.
ENDFORM.
\`\`\`

### 3.2. Personalizzare la Toolbar: I_CALLBACK_PF_STATUS_SET

Si ottiene tramite il parametro \`I_CALLBACK_PF_STATUS_SET\`. Il processo si articola in quattro passaggi:
1.  **Copiare lo Stato GUI Standard:** Copiare lo status 'STANDARD' dal Function Group 'SLIS' nel proprio programma (es. 'Z_MY_STATUS').
2.  **Modificare lo Stato GUI Copiato:** Aggiungere i pulsanti personalizzati con codici funzione (es. \`&EXPORT\`).
3.  **Implementare la FORM di Callback:** Creare una FORM che esegue \`SET PF-STATUS 'Z_MY_STATUS'\`.
4.  **Collegare il Tutto:** Passare il nome della FORM al parametro \`i_callback_pf_status_set\`.

È fondamentale comprendere la sinergia tra \`I_CALLBACK_PF_STATUS_SET\` (che crea il pulsante) e \`I_CALLBACK_USER_COMMAND\` (che gestisce l'azione quando il pulsante viene premuto).

### 3.3. Il Modello a Eventi: Utilizzo della Tabella IT_EVENTS

Permette di agganciare logica a momenti specifici del ciclo di vita dell'ALV (es. \`TOP_OF_PAGE\`).
Il modo corretto e robusto è popolare la tabella \`IT_EVENTS\` programmaticamente:
1.  **Ottenere gli Eventi Disponibili:** Chiamare \`REUSE_ALV_EVENTS_GET\`.
2.  **Registrare un Gestore per un Evento:** Eseguire un \`READ TABLE\` sulla tabella degli eventi, trovare l'evento di interesse (es. \`slis_ev_top_of_page\`), popolare il campo \`form\` con il nome della propria subroutine di gestione e fare \`MODIFY\` sulla tabella.
3.  **Implementare la FORM di gestione:** All'interno della FORM, per \`TOP_OF_PAGE\`, è fondamentale usare \`REUSE_ALV_COMMENTARY_WRITE\` per visualizzare l'intestazione.

\`\`\`abap
DATA: gt_events TYPE slis_t_event.

* Prima della chiamata a REUSE_ALV_GRID_DISPLAY
PERFORM register_alv_events CHANGING gt_events.

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    it_events = gt_events
    ...

FORM register_alv_events CHANGING pt_events TYPE slis_t_event.
  DATA: ls_event TYPE slis_alv_event.
  
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    IMPORTING
      et_events = pt_events.
      
  READ TABLE pt_events INTO ls_event 
       WITH KEY name = slis_ev_top_of_page.
  IF sy-subrc = 0.
    ls_event-form = 'BUILD_TOP_OF_PAGE'.
    MODIFY pt_events FROM ls_event INDEX sy-tabix.
  ENDIF.
ENDFORM.

FORM build_top_of_page.
  DATA: lt_header TYPE slis_t_listheader.
  
  APPEND VALUE #( typ = 'H' info = 'Report Vendite Annuali' ) TO lt_header.
  APPEND VALUE #( typ = 'S' key = 'Data Esecuzione:' info = sy-datum ) TO lt_header.
  
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_header.
ENDFORM.
\`\`\`

---

Sezione 4: Tecniche e Ricette Avanzate per l'ALV
--------------------------------------------------

### 4.1. Calcoli: Totali e Subtotali

* **Totali:** Impostare \`do_sum = 'X'\` nel catalogo campi.
* **Subtotali:** Oltre a \`do_sum = 'X'\`, passare una tabella di ordinamento (tipo \`slis_t_sortinfo_alv\`) al parametro \`IT_SORT\`, impostando \`subtot = 'X'\` per i campi di raggruppamento.

### 4.2. Miglioramenti Visivi: Colorare Righe e Celle

* **Colorare Righe:** Aggiungere un campo colore (es. \`line_color(4) TYPE c\`) alla tabella dati, popolarlo con un codice (es. 'C510' per verde intenso) e specificare il nome del campo in \`is_layout-info_fieldname\`.
* **Colorare Celle:** Richiede di aggiungere una tabella interna di tipo \`SLIS_T_SPECIALCOL_ALV\` alla struttura della tabella dati per ogni riga, specificando il campo (\`FIELDNAME\`) e il colore da applicare.

### 4.3. Manipolazione dei Dati: Creare Griglie ALV Modificabili

1.  **Abilitare Modifica:** Impostare \`edit = 'X'\` nel catalogo campi per le colonne desiderate.
2.  **Catturare Salvataggio:** Gestire il codice funzione \`&DATA_SAVE\` nella \`USER_COMMAND\` FORM.
3.  **Identificare e Salvare:** Creare un backup della tabella dati prima della chiamata ALV. Al salvataggio, confrontare la tabella corrente con il backup per identificare le righe modificate, validarle e aggiornare il database con \`UPDATE\`. Gestire la transazione con \`COMMIT WORK\` o \`ROLLBACK WORK\`.

---

Sezione 5: Contesto, Best Practice e Sviluppo Futuro
------------------------------------------------------

### 5.1. Paradigma Procedurale vs. Orientato agli Oggetti: REUSE_ vs. CL_SALV_TABLE

| Criterio            | REUSE_ALV_... (Procedurale)         | CL_SALV_TABLE (Object-Oriented)   |
|---------------------|-------------------------------------|-----------------------------------|
| Paradigma           | Procedurale, basato su moduli funzione. | Orientato agli oggetti.         |
| Manutenibilità      | Inferiore su progetti complessi.      | Superiore.                        |
| Riusabilità         | Limitata.                             | Elevata.                          |
| Best Practice SAP   | Legacy. Per manutenzione.           | Raccomandato per nuovi sviluppi.  |

### 5.2. Errori Comuni e Risoluzione

* **Dump READ_REPORT_LINE_TOO_LONG:** Righe di codice > 72 caratteri. Soluzione: spezzare le righe.
* **Blocco Oggetti (SET PARAMETER ID):** Altro utente sta bloccando l'oggetto. Soluzione: assicurarsi che non ci siano altre sessioni aperte che bloccano lo stesso oggetto.
* **Catalogo Campi Incompleto:** Il modulo non trova le intestazioni. Soluzione: usare \`LIKE\` invece di \`TYPE\` nelle definizioni dati per mantenere il legame con il DDIC.
* **Intestazione (TOP_OF_PAGE) Vuota:** L'istruzione \`WRITE\` non funziona. Soluzione: usare \`REUSE_ALV_COMMENTARY_WRITE\`.
`;
