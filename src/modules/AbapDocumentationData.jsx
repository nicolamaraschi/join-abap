export const abapDocumentation = {
    title: "Il Compendio dello Sviluppatore ABAP",
    introduction: "Una Guida ai Concetti Fondamentali e agli Strumenti Essenziali.",
    sections: [
      {
        id: "knowledge-base",
        title: "Sezione 1: Navigare nella Knowledge Base Ufficiale di SAP",
        content: `Questa sezione stabilisce le fonti autorevoli per tutta la conoscenza sullo sviluppo ABAP.\n\n` +
                 `1.1 Il SAP Help Portal: Il Repository Centrale\n` +
                 `Il SAP Help Portal è il principale hub centralizzato per tutta la documentazione ufficiale dei prodotti SAP. Copre l'intera suite di prodotti, dai sistemi ERP come SAP S/4HANA alla Business Technology Platform (BTP). Il portale è organizzato per prodotto e fornisce guide all'implementazione, guide utente e note di rilascio. Offre potenti funzionalità di ricerca, consentendo di filtrare per prodotto o in tutto l'ecosistema SAP.\n` +
                 `Il portale è in continua evoluzione. SAP sta investendo attivamente nella modernizzazione della sua infrastruttura per migliorare l'esperienza degli sviluppatori, includendo un programma pilota per commenti diretti e una funzionalità di "Machine Translation On-the-Fly" basata su AI per traduzioni istantanee in quasi 40 lingue.\n\n` +
                 `1.2 La ABAP Keyword Documentation: La Bibbia del Linguaggio\n` +
                 `Questa è la risorsa di riferimento definitiva per la sintassi e la semantica dei linguaggi su un Application Server ABAP (AS ABAP). È la risorsa più cruciale per qualsiasi sviluppatore.\n` +
                 `È possibile accedervi in diversi modi:\n` +
                 `- In modo contestuale nell'editor ABAP premendo F1 su qualsiasi parola chiave.\n` +
                 `- Direttamente tramite i T-Code abaphelp o abapdocu nella SAP GUI.\n` +
                 `- Online attraverso il SAP Help Portal.\n` +
                 `La documentazione è strutturata in sezioni come "ABAP - Reference", "ABAP - Examples" e "ABAP - Release-Specific Changes". La sua interfaccia è stata modernizzata utilizzando XSLT e SAP UI5 per una migliore esperienza utente.\n\n` +
                 `L'ecosistema informativo di SAP può apparire frammentato:\n` +
                 `- SAP Help Portal: Risponde al "cosa" (le regole del linguaggio).\n` +
                 `- SAP Support Portal: Risponde al "come risolvere" (note, incidenti).\n` +
                 `- SAP Community: Risponde al "come fare" (best practice, aiuto tra pari).`,
        code: null
      },
      {
        id: "architecture",
        title: "Sezione 2: Il Progetto Architettonico di ABAP",
        content: `Questa sezione scompone il linguaggio ABAP nei suoi pilastri fondamentali.\n\n` +
                 `2.1 Le Fondamenta: L'ABAP Dictionary (DDIC)\n` +
                 `L'ABAP Dictionary (DDIC) è un repository di metadati centrale che garantisce integrità, coerenza e riutilizzabilità dei dati. Gestisce l'aspetto del Data Definition Language (DDL).\n\n` +
                 `Componenti Fondamentali del DDIC:\n` +
                 `- Tabelle: Definizioni indipendenti dal database che vengono attivate come tabelle fisiche.\n` +
                 `- Viste: Viste logiche su una o più tabelle.\n` +
                 `- Tipi di Dati:\n` +
                 `    - Domini: Definiscono caratteristiche tecniche (tipo, lunghezza, range di valori).\n` +
                 `    - Elementi Dati: Definiscono caratteristiche semantiche (etichette, help F1) e fanno riferimento a un dominio.\n` +
                 `    - Strutture: Collezione di campi riutilizzabili.\n` +
                 `- Tipi Tabella: Progetti globali per tabelle interne.\n` +
                 `- Search Help: Forniscono l'aiuto all'immissione F4.\n` +
                 `- Lock Object: Gestiscono l'accesso concorrente generando moduli di enqueue e dequeue.\n\n` +
                 `2.2 I Paradigmi di Programmazione: Procedurale vs. Orientato agli Oggetti\n` +
                 `- Programmazione Procedurale: Il modello classico basato su \`REPORT\`, subroutine (\`FORM...ENDFORM\`) e moduli funzione.\n` +
                 `- Programmazione Orientata agli Oggetti (ABAP Objects): Introdotta nel 1999, basata su \`CLASS\` e \`INTERFACE\`. È il paradigma raccomandato da SAP per tutti i nuovi sviluppi.\n\n` +
                 `Vantaggi di ABAP Objects:\n` +
                 `- Incapsulamento dei Dati: Migliora stabilità e manutenibilità.\n` +
                 `- Ereditarietà e Interfacce: Abilita riutilizzo e polimorfismo.\n` +
                 `- Eventi: Facilita flussi di programma debolmente accoppiati.\n` +
                 `- Sintassi Pulita e Accesso a Nuove Tecnologie: Unico modo per interagire con tecnologie moderne come Web Dynpro, RTTS e ICF.\n\n` +
                 `2.3 Tecnica di Modularizzazione: Moduli Funzione\n` +
                 `I moduli funzione sono procedure riutilizzabili incapsulate in un Gruppo di Funzioni (\`SAPL<fgrp>\`). Hanno un'interfaccia definita (\`IMPORTING\`, \`EXPORTING\`, \`CHANGING\`, \`TABLES\`) e supportano la gestione delle eccezioni.\n\n` +
                 `2.4 Il Paradigma Moderno: Code-to-Data\n` +
                 `Guidato da SAP HANA, questo paradigma spinge la logica di elaborazione nel database per eseguirla dove risiedono i dati, migliorando drasticamente le prestazioni.\n\n` +
                 `ABAP Managed Database Procedures (AMDP):\n` +
                 `Permettono di scrivere procedure di database (usando SQLScript per HANA) direttamente nei metodi di classi ABAP.\n` +
                 `- Vantaggi: Sfruttano funzioni native di HANA, ideali per calcoli complessi e set di risultati multipli.\n` +
                 `- Svantaggi: Codice specifico del database (non portabile), richiede conoscenza di SQLScript e ha alcune restrizioni (es. no gestione automatica del mandante).\n\n` +
                 `Core Data Services (CDS) Views:\n` +
                 `Un'infrastruttura per definire modelli di dati semanticamente ricchi. Sono una pietra miliare del moderno modello di programmazione ABAP.\n` +
                 `- Caratteristiche Principali: Possono includere calcoli, aggregazioni, \`CASE\` e potenti \`ASSOCIATIONS\`. Supportano annotazioni per arricchire il modello con proprietà specifiche (es. per UI Fiori).\n\n` +
                 `Gerarchia delle Tecniche Code-Pushdown (Preferenza SAP):\n` +
                 `1. Open SQL: Lo strumento più astratto, portabile e integrato.\n` +
                 `2. CDS Views: Un modo dichiarativo più potente per modellare i dati (preferito per il Virtual Data Model).\n` +
                 `3. AMDP: Opzione finale per funzioni specifiche del database o logica procedurale complessa.`,
        code: null
      },
      {
          id: "alv-masterclass",
          title: "Sezione 3: Una Masterclass sull'ABAP List Viewer (ALV)",
          content: `Questa sezione fornisce un'analisi approfondita delle classi globali più critiche per lo sviluppo di interfacce utente in ABAP classico.\n\n` +
                   `3.1 L'Astrazione Moderna: CL_SALV_TABLE\n` +
                   `\`CL_SALV_TABLE\` fa parte del più recente SALV Object Model, un'API semplificata e orientata agli oggetti che agisce come un wrapper, incapsulando la complessità di strumenti più vecchi come \`CL_GUI_ALV_GRID\`.\n\n` +
                   `Metodi Chiave e Utilizzo:\n` +
                   `- \`CL_SALV_TABLE=>FACTORY(...)\`: Punto di ingresso statico per creare un'istanza dell'ALV.\n` +
                   `- \`->DISPLAY()\`: Renderizza l'output ALV.\n` +
                   `- \`->GET_COLUMNS()\`: Restituisce un oggetto per gestire le colonne.\n` +
                   `- \`->GET_FUNCTIONS()\`: Restituisce un oggetto per controllare i pulsanti della toolbar.\n` +
                   `- \`->GET_SELECTIONS()\`: Restituisce un oggetto per gestire le modalità di selezione.\n` +
                   `- \`->GET_EVENT()\`: Restituisce un oggetto per registrare gestori di eventi utente (es. \`DOUBLE_CLICK\`).\n\n` +
                   `Esempio di Codice Ufficiale:`,
          code: `
  " Dichiarazione Dati
  DATA: gt_outtab TYPE TABLE OF sflight.
  DATA: gr_table  TYPE REF TO cl_salv_table.
  
  " Selezione Dati
  SELECT * FROM sflight INTO TABLE gt_outtab.
  
  " Creazione e Visualizzazione ALV
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = gr_table
        CHANGING
          t_table      = gt_outtab ).
    CATCH cx_salv_msg.
      " Gestione errori
  ENDTRY.
  
  gr_table->display( ).
  `
      },
      {
          id: 'alv-grid-details',
          title: '3.2 Il Potente Cavallo di Battaglia: CL_GUI_ALV_GRID',
          content: `\`CL_GUI_ALV_GRID\` è la classe fondamentale del Control Framework per creare controlli a griglia altamente personalizzabili. Richiede una schermata esplicita e un contenitore.\n\n` +
                   `Metodi Chiave e Utilizzo:\n` +
                   `- \`CONSTRUCTOR\`: Istanzia la griglia, richiedendo un contenitore padre (\`i_parent\`).\n` +
                   `- \`SET_TABLE_FOR_FIRST_DISPLAY\`: Metodo principale per visualizzare i dati per la prima volta.\n` +
                   `- \`REFRESH_TABLE_DISPLAY\`: Aggiorna la griglia con nuovi dati.\n` +
                   `- \`REGISTER_EDIT_EVENT\`: Registra eventi come \`mc_evt_enter\` per gestire l'input dell'utente.\n` +
                   `- \`SET_READY_FOR_INPUT\`: Attiva/disattiva la modificabilità della griglia.\n` +
                   `- \`GET_SELECTED_ROWS\`: Recupera le righe selezionate dall'utente.\n\n` +
                   `Esempio di Codice Ufficiale per una Griglia Modificabile:`,
          code: `
  " Dichiarazione Dati
  DATA: lo_grid TYPE REF TO cl_gui_alv_grid,
        lt_fieldcat TYPE lvc_t_fcat,
        lt_data TYPE TABLE OF y_orders_c,
        gs_alv_layout TYPE lvc_s_layo.
  
  " In PBO di una schermata
  MODULE show_orders OUTPUT.
    IF lo_grid IS NOT BOUND.
      " 1. Popolare il catalogo campi (lt_fieldcat)
      " 2. Impostare la proprietà modificabile per una colonna:
      LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).
        IF <fs_fieldcat>-fieldname = 'QUANT2'.
          <fs_fieldcat>-edit = 'X'.
        ENDIF.
      ENDLOOP.
  
      " 3. Popolare i dati (lt_data)
  
      " 4. Creare contenitore e istanza della griglia
      CREATE OBJECT lo_grid
        EXPORTING i_parent = cl_gui_container=>screen0.
  
      " 5. Registrare l'evento 'Enter' per la modifica
      CALL METHOD lo_grid->register_edit_event
        EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_enter.
  
      " 6. Visualizzare la tabella
      CALL METHOD lo_grid->set_table_for_first_display
        EXPORTING
          is_layout        = gs_alv_layout
        CHANGING
          it_outtab        = lt_data
          it_fieldcatalog  = lt_fieldcat.
    ENDIF.
  ENDMODULE.
  `
      },
      {
        id: "alv-comparison",
        title: "3.3 Tabella Comparativa: CL_SALV_TABLE vs. CL_GUI_ALV_GRID",
        content: `Questa tabella funge da guida rapida per scegliere la classe ALV appropriata.\n\n` +
                 `Caratteristica              | CL_GUI_ALV_GRID                                  | CL_SALV_TABLE                                     | Raccomandazione / Caso d'Uso\n`+
                 `---------------------------|--------------------------------------------------|---------------------------------------------------|--------------------------------------------------------------------------\n`+
                 `Celle Modificabili        | Sì. Pieno supporto.                              | No. Principalmente per visualizzazione.           | Usare \`CL_GUI_ALV_GRID\` per qualsiasi requisito di input utente nella griglia.\n`+
                 `Schermata/Contenitore     | Richiesto. Deve essere creato esplicitamente.    | Non Richiesto. Può generare la propria visualizzazione. | Usare \`CL_SALV_TABLE\` per report rapidi e semplici.\n`+
                 `Facilità d'Uso            | Più complesso. Richiede config. manuale.         | Molto semplice. \`FACTORY\` crea griglia in 1 chiamata. | \`CL_SALV_TABLE\` è più efficiente per report standard non modificabili.\n`+
                 `Elaborazione in Background| Problematico. Può generare dump.                 | Sì. Può generare output di lista.                 | \`CL_SALV_TABLE\` è la scelta affidabile per job in background.\n`+
                 `Personalizzazione         | Alta. Controllo granulare su eventi, toolbar.    | Moderata. Controllo di alto livello ma con limiti.  | \`CL_GUI_ALV_GRID\` per UI complesse, \`CL_SALV_TABLE\` per report standard.\n`+
                 `Tecnologia Sottostante    | Classe fondamentale del Control Framework.       | Livello di astrazione sopra \`CL_GUI_ALV_GRID\`.      | \`CL_SALV_TABLE\` è l'approccio moderno per report non modificabili.`,
        code: null
      },
      {
          id: "essential-fm",
          title: "Sezione 4: Il Toolkit Essenziale dello Sviluppatore",
          content: `Questa sezione fornisce un elenco curato dei moduli funzione standard più utilizzati.\n\n` +
                   `4.1 Introduzione ai Moduli Funzione\n` +
                   `I moduli funzione sono procedure riutilizzabili e centralizzate all'interno dei Gruppi di Funzioni. SAP fornisce migliaia di FM standard per compiti comuni.\n\n` +
                   `4.2 Tabella: Moduli Funzione ABAP Essenziali\n\n` +
                   `Modulo Funzione                    | Categoria              | Scopo                                                              | Parametri Chiave\n` +
                   `-----------------------------------|------------------------|--------------------------------------------------------------------|--------------------------------------------------------\n` +
                   `\`GUI_UPLOAD\`                       | I/O File               | Carica un file dal PC a una tabella interna.                         | \`FILENAME\`, \`FILETYPE\`, \`TABLES data_tab\`\n` +
                   `\`GUI_DOWNLOAD\`                     | I/O File               | Scarica una tabella interna in un file sul PC.                     | \`FILENAME\`, \`FILETYPE\`, \`TABLES data_tab\`\n` +
                   `\`READ_TEXT\`                        | Gestione Testi         | Legge oggetti di testo lungo.                                        | \`ID\`, \`LANGUAGE\`, \`NAME\`, \`OBJECT\`, \`TABLES LINES\`\n` +
                   `\`SAVE_TEXT\`                        | Gestione Testi         | Salva o crea oggetti di testo lungo.                                 | \`HEADER\`, \`SAVEMODE_DIRECT\`, \`TABLES LINES\`\n` +
                   `\`BAPI_TRANSACTION_COMMIT\`          | Controllo Transazioni  | Esegue un commit sul database. Essenziale dopo BAPI di modifica.   | \`WAIT\`\n` +
                   `\`BAPI_TRANSACTION_ROLLBACK\`        | Controllo Transazioni  | Annulla le modifiche dall'ultimo commit.                           | (Nessuno)\n` +
                   `\`CONVERT_OTF\`                      | Conversione Dati       | Converte formato OTF (SAPscript/Smart Form) in PDF, etc.             | \`FORMAT\`, \`TABLES OTF, LINES\`\n` +
                   `\`POPUP_TO_CONFIRM\`                 | Dialoghi               | Visualizza una finestra di dialogo di conferma.                      | \`TITLEBAR\`, \`TEXT_QUESTION\`, \`ANSWER\`\n` +
                   `\`F4IF_INT_TABLE_VALUE_REQUEST\`     | Dialoghi               | Mostra una tabella interna come aiuto alla ricerca F4.               | \`RETFIELD\`, \`DYNPPROG\`, \`DYNPNR\`, \`DYNPROFIELD\`\n` +
                   `\`CONVERSION_EXIT_ALPHA_INPUT\`    | Conversione Dati       | Aggiunge zeri iniziali a un numero (es. '123' -> '00000123').  | \`INPUT\`, \`OUTPUT\`\n` +
                   `\`CONVERSION_EXIT_ALPHA_OUTPUT\`   | Conversione Dati       | Rimuove gli zeri iniziali.                                       | \`INPUT\`, \`OUTPUT\``,
        code: null
      },
      {
        id: "fm-analysis",
        title: "4.3 Analisi Dettagliata ed Esempi Ufficiali",
        content: `### BAPI_TRANSACTION_COMMIT\n` +
                 `**Scopo**: Finalizzare una transazione e rendere permanenti le modifiche al database. È obbligatorio chiamare questo FM dopo aver utilizzato una BAPI che crea o modifica dati.\n` +
                 `**Parametri Chiave**: \`WAIT\` (Tipo C): Se impostato su 'X', il programma attende il completamento del task di aggiornamento, cruciale per garantire che i dati appena committati siano disponibili.`,
        code: `
  " Si presume che BAPI_SALESORDER_CREATEFROMDAT2 sia stato chiamato con successo
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT = 'X'.
  IF sy-subrc = 0.
    MESSAGE 'Ordine di vendita creato con successo.' TYPE 'S'.
  ENDIF.
  `
      },
      {
        id: "fm-read-text",
        title: "",
        content: `### READ_TEXT\n` +
                 `**Scopo**: Leggere testi lunghi standard associati a vari oggetti di business SAP (es. testi di un ordine di vendita, note di un materiale).\n` +
                 `**Parametri Chiave**:\n` +
                 `- **OBJECT**: L'oggetto di business (es. 'VBBK' per testata ordine di vendita).\n` +
                 `- **NAME**: La chiave dell'oggetto (es. il numero dell'ordine).\n` +
                 `- **ID**: L'ID specifico del testo (es. '0001').\n` +
                 `- **LANGUAGE**: La lingua.\n` +
                 `- **TABLES LINES**: Tabella interna che riceverà le righe del testo.`,
        code: `
  DATA: lt_text_lines TYPE TABLE OF tline.
  DATA: ls_header LIKE thead.
  
  ls_header-tdobject = 'VBBK'.
  ls_header-tdname   = '0000004969'. " Esempio di numero ordine
  ls_header-tdid     = '0001'.
  ls_header-tdspras  = sy-langu.
  
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id       = ls_header-tdid
      language = ls_header-tdspras
      name     = ls_header-tdname
      object   = ls_header-tdobject
    TABLES
      lines    = lt_text_lines
    EXCEPTIONS
      OTHERS   = 1.
  `
      },
      {
        id: "fm-gui-upload",
        title: "",
        content: `### GUI_UPLOAD\n` +
                 `**Scopo**: Caricare dati da un file presente sul computer dell'utente (server di presentazione) in una tabella interna ABAP.\n` +
                 `**Parametri Chiave**:\n` +
                 `- **FILENAME**: Il percorso completo del file da caricare.\n` +
                 `- **FILETYPE**: Il formato del file ('ASC' per testo, 'BIN' per binario).\n` +
                 `- **HAS_FIELD_SEPARATOR**: Flag ('X') per indicare se i campi sono separati da un delimitatore (es. tab).\n` +
                 `- **TABLES data_tab**: La tabella interna che riceverà i dati.`,
        code: `
  DATA: lt_data_from_file TYPE TABLE OF string.
  DATA: lv_filename TYPE string.
  
  lv_filename = 'C:\\\\temp\\\\mydata.txt'.
  
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_filename
      filetype                = 'ASC'
    TABLES
      data_tab                = lt_data_from_file
    EXCEPTIONS
      file_open_error         = 1
      OTHERS                  = 2.
  `
      },
      {
        id: "conclusions",
        title: "Conclusioni",
        content: `Questo compendio ha analizzato in profondità il linguaggio SAP ABAP. L'analisi ha evidenziato diversi punti chiave per lo sviluppatore moderno:\n\n` +
                 `- **L'Importanza delle Fonti Ufficiali**: La navigazione efficace tra SAP Help Portal, ABAP Keyword Documentation e SAP Community è una competenza fondamentale.\n` +
                 `- **Il Passaggio al Moderno**: L'uso della programmazione orientata agli oggetti (ABAP Objects) è una best practice fortemente raccomandata per stabilità e manutenibilità.\n` +
                 `- **La Strategia Code-to-Data**: Questo paradigma è al centro dello sviluppo su HANA. Gli sviluppatori devono padroneggiare la gerarchia delle tecniche disponibili (Open SQL, CDS Views, AMDP).\n` +
                 `- **La Scelta Consapevole degli Strumenti UI**: La distinzione tra \`CL_SALV_TABLE\` (semplicità per report di visualizzazione) e \`CL_GUI_ALV_GRID\` (potenza per interfacce complesse e modificabili) è cruciale.\n\n` +
                 `In sintesi, lo sviluppatore ABAP di successo oggi non solo deve padroneggiare la sintassi, ma deve anche comprendere le strategie architettoniche sottostanti promosse da SAP per creare applicazioni performanti, manutenibili e a prova di futuro.`,
        code: null
      }
    ]
  };
  