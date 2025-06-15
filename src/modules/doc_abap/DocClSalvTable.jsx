export const content = `
Guida Tecnica Completa alla Classe CL_SALV_TABLE in SAP S/4HANA
==============================================================

Sezione 1: Il Modello a Oggetti SALV: Fondamenti e Istanziazione
---------------------------------------------------------------

### 1.1. Introduzione al Framework SALV

Nel contesto dell'evoluzione di SAP verso S/4HANA, la classe CL_SALV_TABLE rappresenta l'approccio moderno e orientato agli oggetti (Object-Oriented) per la visualizzazione di dati tabellari, posizionandosi come il successore dei moduli funzione tradizionali come REUSE_ALV_GRID_DISPLAY. L'adozione del modello a oggetti SALV (SAP List Viewer) offre vantaggi significativi, tra cui un'incapsulazione migliorata, una maggiore riutilizzabilità e manutenibilità del codice e, soprattutto, l'eliminazione della necessità di creare e gestire manualmente i field catalog. Il framework deduce automaticamente le proprietà delle colonne dai tipi di dati della tabella interna, semplificando notevolmente lo sviluppo.

È importante notare che CL_SALV_TABLE è la classe principale per la gestione di tabelle semplici e bidimensionali. Essa fa parte di una famiglia più ampia di classi SALV, che include CL_SALV_HIERSEQ_TABLE per le liste gerarchico-sequenziali e CL_SALV_TREE per le strutture ad albero, offrendo una soluzione per quasi ogni esigenza di reporting.

### 1.2. Il Pattern di Istanziazione Principale: FACTORY e DISPLAY

Il processo per visualizzare un ALV di base con CL_SALV_TABLE si articola in due passaggi fondamentali, che costituiscono il cuore del framework.

* **Istanziazione tramite CL_SALV_TABLE=>FACTORY:** Questo metodo statico è il punto di ingresso universale per la creazione di un'istanza SALV. La sua firma è peculiare e può inizialmente sorprendere gli sviluppatori abituati a pattern più convenzionali. La tabella dati interna viene passata tramite un parametro CHANGING (t_table), mentre il riferimento all'oggetto ALV creato viene restituito tramite un parametro IMPORTING (r_salv_table). È una pratica di sviluppo robusta e obbligatoria racchiudere questa chiamata in un blocco TRY...CATCH per gestire l'eccezione cx_salv_msg, che viene sollevata in caso di errori durante l'istanziazione.
* **Rendering tramite DISPLAY:** Una volta che l'oggetto ALV è stato creato e tutte le configurazioni desiderate sono state applicate, la chiamata al metodo d'istanza DISPLAY si occupa di renderizzare la griglia a schermo.

Un aspetto fondamentale della filosofia di progettazione di CL_SALV_TABLE è il suo approccio "minimalista per default". Una semplice chiamata a FACTORY seguita da DISPLAY produce una griglia dati pulita, ma priva di molte delle funzionalità standard che gli utenti si aspettano, come i pulsanti per l'ordinamento, il filtro o il salvataggio dei layout. Lo sviluppatore deve esplicitamente "attivare" queste funzionalità tramite chiamate successive, ad esempio get_functions()->set_all(abap_true).

### 1.3. Contesti di Visualizzazione: Schermo Intero, Container e Lista

Il metodo FACTORY permette di specificare dove e come l'ALV verrà visualizzato, offrendo tre contesti principali.

* **Visualizzazione a Schermo Intero (Default):** È l'implementazione più semplice. Omettendo il parametro list_display o impostandolo a abap_false, l'ALV occuperà un'intera schermata generata dinamicamente.
* **Visualizzazione basata su Container:** Per integrare uno o più ALV all'interno di una singola schermata (dynpro), è necessario passare un riferimento a un oggetto container (ad esempio, un'istanza di CL_GUI_CUSTOM_CONTAINER o CL_GUI_SPLITTER_CONTAINER) al parametro r_container del metodo FACTORY.
* **Visualizzazione come Lista Classica:** Impostando il parametro list_display a if_salv_c_bool_sap=>true, l'output assume l'aspetto di un report ABAP classico basato su istruzioni WRITE.

---

Sezione 2: Padroneggiare la Configurazione delle Colonne con CL_SALV_COLUMNS_TABLE
------------------------------------------------------------------------------------

### 2.1. Accesso agli Oggetti Colonna

La manipolazione delle colonne segue un processo a due passaggi:

1.  Ottenere l'oggetto principale che gestisce tutte le colonne: \`lo_columns = lo_salv->get_columns()\`. Questo metodo restituisce un'istanza di \`CL_SALV_COLUMNS_TABLE\`.
2.  Ottenere l'oggetto per una singola colonna specifica: \`lo_column = lo_columns->get_column( 'NOME_COLONNA' )\`. Questo metodo restituisce un'istanza di \`CL_SALV_COLUMN\`.

È essenziale racchiudere la chiamata a \`get_column()\` in un blocco \`TRY...CATCH cx_salv_not_found\` per gestire elegantemente gli errori dovuti a nomi di colonna errati o non esistenti.

### 2.2. Controllare l'Aspetto delle Colonne: Testo, Visibilità e Posizione

* **Testi di Intestazione:** È possibile sovrascrivere le etichette predefinite del Data Dictionary utilizzando i metodi \`set_long_text\`, \`set_medium_text\` e \`set_short_text\`.
* **Visibilità:** Il metodo \`set_visible( abap_false )\` nasconde una colonna dalla visualizzazione predefinita. Per nascondere completamente una colonna, anche dalla configurazione del layout, si utilizza \`set_technical( abap_true )\`.
* **Posizionamento:** Il metodo \`set_column_position\` permette di riordinare programmaticamente le colonne.

### 2.3. Dimensionamento e Ottimizzazione: Un'Analisi Comparativa

* **Ottimizzazione Globale:** La chiamata \`lo_columns->set_optimize( abap_true )\` adatta automaticamente la larghezza di tutte le colonne al loro contenuto.
* **Larghezza Specifica:** Il metodo \`lo_column->set_output_length( '20' )\` imposta una larghezza fissa in caratteri per una singola colonna. Per garantire che \`set_output_length\` abbia effetto, è spesso necessario disabilitare l'ottimizzazione globale.

### 2.4. Proprietà delle Colonne Data-Centric e Interattive

* **Colorazione:** Per una colorazione dinamica a livello di cella o riga, l'approccio corretto consiste nell'aggiungere una colonna speciale alla tabella dati di output (di tipo \`LVC_T_SCOL\`) e popolarla con le informazioni di colore, per poi impostarla come "color column" tramite \`lo_columns->set_color_column( 'NOME_COLONNA_COLORE' )\`.
* **Interattività (Hotspot/Checkbox):** Impostando il tipo di cella con \`set_cell_type( if_salv_c_cell_type=>hotspot )\` o \`set_cell_type( if_salv_c_cell_type=>checkbox_hotspot )\`, la colonna diventa cliccabile, scatenando l'evento \`LINK_CLICK\`.
* **Search Help (F4):** È possibile abilitare un aiuto alla ricerca per una colonna con \`set_ddic_reference()\` e \`set_f4( abap_true )\`.

### 2.5. Tabella di Riferimento: Metodi di CL_SALV_COLUMN

| Nome Metodo         | Classe                   | Descrizione e Caso d'Uso                                                              |
|---------------------|--------------------------|---------------------------------------------------------------------------------------|
| set_long_text       | CL_SALV_COLUMN           | Imposta la variante più lunga del testo di intestazione.                                |
| set_visible         | CL_SALV_COLUMN           | Nasconde o mostra una colonna nella visualizzazione predefinita.                      |
| set_technical       | CL_SALV_COLUMN           | Nasconde una colonna sia dalla visualizzazione che dalle impostazioni del layout.        |
| set_optimize        | CL_SALV_COLUMNS_TABLE    | Abilita/disabilita l'ottimizzazione automatica della larghezza per tutte le colonne. |
| set_output_length   | CL_SALV_COLUMN           | Imposta una larghezza fissa in caratteri per una colonna specifica.                  |
| set_color           | CL_SALV_COLUMN_TABLE     | Imposta un colore statico per un'intera colonna.                                     |
| set_cell_type       | CL_SALV_COLUMN_TABLE     | Cambia la visualizzazione della cella in un hotspot, pulsante o checkbox.             |
| set_ddic_reference  | CL_SALV_COLUMN           | Collega la colonna a un campo del DDIC per ereditarne le proprietà (es. aiuto F4).      |

---

Sezione 3: Implementazione delle Operazioni Standard sui Dati ALV
--------------------------------------------------------------------

Per il 90% delle esigenze di reporting, \`CL_SALV_TABLE\` è sufficiente; per scenari altamente personalizzati, \`CL_GUI_ALV_GRID\` rimane la scelta obbligata.

### 3.1. Ordinamento Programmatico (CL_SALV_SORTS)

\`\`\`abap
DATA(lo_sorts) = lo_salv->get_sorts( ).
lo_sorts->add_sort( columnname = 'VBELN', sequence = if_salv_c_sort=>sort_up ).
\`\`\`

### 3.2. Filtro Programmatico (CL_SALV_FILTERS)

\`\`\`abap
DATA(lo_filters) = lo_salv->get_filters( ).
lo_filters->add_filter( columnname = 'SPART' option = 'EQ' sign = 'I' low = '01' ).
\`\`\`

### 3.3. Aggregazioni e Calcoli (CL_SALV_AGGREGATIONS)

\`\`\`abap
DATA(lo_aggs) = lo_salv->get_aggregations( ).
lo_aggs->add_aggregation( columnname = 'NETWR', aggregation = if_salv_c_aggregation=>total ).
\`\`\`
Nota: non è possibile manipolare direttamente il testo o il comportamento dei subtotali.

---

Sezione 4: Personalizzazione Avanzata dell'UI e User Experience
-------------------------------------------------------------------

### 4.1. Gestione della Barra degli Strumenti (CL_SALV_FUNCTIONS_LIST)

* **Abilitare Funzioni Standard:** \`lo_functions = lo_salv->get_functions( )->set_all( abap_true )\`.
* **Aggiungere Funzioni Personalizzate:** Il metodo \`add_function()\` è supportato solo quando l'ALV è in un container. In modalità schermo intero, si deve usare un **GUI Status** custom.

### 4.2. Persistenza dei Layout (CL_SALV_LAYOUT)

La sequenza corretta per abilitare il salvataggio dei layout:
1.  \`lo_layout = lo_salv->get_layout()\`.
2.  \`lo_layout->set_key( VALUE #( report = sy-repid ) )\`. (Obbligatorio)
3.  \`lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none )\`.
4.  \`lo_layout->set_default( abap_true )\`. (Opzionale, per layout standard)
5.  \`lo_layout->set_initial_layout( 'NOME_VARIANTE' )\`. (Per caricare un layout all'avvio)

### 4.3. Miglioramenti Visivi (CL_SALV_DISPLAY_SETTINGS)

* **Intestazione:** \`set_list_header( 'Titolo Report' )\`.
* **Pattern a Strisce:** \`set_striped_pattern( abap_true )\`.
* **Contenuti Aggiuntivi:** \`set_top_of_list\` e \`set_end_of_list\` per intestazioni/piè di pagina strutturati.

---

Sezione 5: Programmazione Guidata dagli Eventi per Report Interattivi
--------------------------------------------------------------------

### 5.1. Il Modello di Gestione degli Eventi SALV

Il pattern standard prevede la creazione di una classe locale (es. \`LCL_HANDLER\`) che contiene i metodi gestori di eventi. Un'istanza di questa classe viene poi registrata con l'istruzione \`SET HANDLER mio_handler->on_double_click FOR lo_events\`.

### 5.2. Implementazione delle Interazioni Utente Principali

* **DOUBLE_CLICK:** Ideale per funzionalità di drill-down. Il metodo gestore riceve la riga (row) e la colonna (column).
* **LINK_CLICK (Hotspot/Checkbox):** Gestisce il clic su hyperlink o checkbox.
* **ADDED_FUNCTION:** Risponde alla pressione di pulsanti personalizzati, ricevendo il nome della funzione (e_salv_function).

### 5.4. Gestione della Selezione (CL_SALV_SELECTIONS)

* **Controllo Modalità:** \`get_selections()->set_selection_mode( if_salv_c_selection_mode=>row_column )\`.
* **Recupero Selezioni:** \`get_selected_rows()\` restituisce una tabella con gli indici delle righe selezionate.

---

Sezione 6: Il SALV Modificabile: Guida alle Tecniche Moderne e Legacy
-----------------------------------------------------------------------

### 6.1. La Filosofia di Progettazione: Perché il SALV era "Solo Visualizzazione"

\`CL_SALV_TABLE\` è stato originariamente progettato come uno strumento di reporting semplificato. La modificabilità non faceva parte del suo scopo iniziale, il che spiega perché per anni gli sviluppatori hanno fatto ricorso a workaround complessi e non supportati.

### 6.2. La Best Practice di S/4HANA (Release >= 7.56): L'API extended_grid_api

Questo è il metodo raccomandato e supportato per creare griglie modificabili nei moderni sistemi S/4HANA.
\`\`\`abap
" Nel gestore eventi per il pulsante "Modifica"
DATA(ls_api) = o_salv->extended_grid_api( ).
DATA(ls_edit) = ls_api->editable_restricted( ).
" Rende modificabili colonne specifiche o l'intera griglia
ls_edit->set_column_editable( 'NOME_COLONNA' ).
\`\`\`

### 6.4. Strategie di Modifica Alternative

* **Checkbox Modificabili:** Usare una colonna di tipo checkbox e gestire le modifiche tramite l'evento \`LINK_CLICK\`.
* **Dialoghi Modali:** Aprire una finestra di dialogo modale su doppio clic per modificare i dati della riga selezionata.

---

Sezione 7: Sviluppo Robusto: Gestione delle Eccezioni e Best Practice
--------------------------------------------------------------------

### 7.1. Anticipare e Gestire le Eccezioni SALV

È imperativo che tutte le chiamate a metodi che possono fallire (es. \`FACTORY\`, \`get_column\`) siano racchiuse in blocchi \`TRY...CATCH\` appropriati per le seguenti eccezioni:
* \`CX_SALV_MSG\`: Errore generico.
* \`CX_SALV_NOT_FOUND\`: Oggetto (es. colonna) non trovato.
* \`CX_SALV_DATA_ERROR\`: Incongruenza nei dati.
* \`CX_SALV_EXISTING\`: Oggetto già esistente.

### 7.2. Raccomandazioni e Conclusione

**Utilizzare CL_SALV_TABLE per:**
* La stragrande maggioranza delle esigenze di reporting.
* Sviluppo rapido e codice pulito.
* Griglie modificabili in modo supportato (su release >= 7.56).

**Utilizzare CL_GUI_ALV_GRID per:**
* Scenari che richiedono un controllo estremamente granulare (es. testo dei subtotali).
* Eventi complessi non disponibili in SALV.
* Griglie modificabili su sistemi SAP più datati.
`;
