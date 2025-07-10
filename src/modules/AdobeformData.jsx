// src/modules/AdobeformData.jsx

export const adobeformData = {
    "OVERVIEW": [
        {
            name: "Guida Pratica Completa a SAP Interactive Forms by Adobe",
            description: "Una panoramica esaustiva e pratica della tecnologia SAP Interactive Forms by Adobe, dai concetti fondamentali all'implementazione avanzata.",
            module: "Generale",
            transactions: ["SFP", "SEGW", "SCOT", "SE38", "SE78"],
            sections: [
                {
                    title: "Parte 1: Concetti Fondamentali e Architettura",
                    content: `
Questa sezione stabilisce le basi concettuali e architetturali degli Adobe Forms, fornendo il contesto essenziale per comprendere perché questa tecnologia ha sostituito le soluzioni precedenti e quali sono i suoi componenti chiave.

### Dagli SmartForms agli Adobe Forms: Un'Evoluzione Pratica
SAP Interactive Forms by Adobe (spesso abbreviato in Adobe Forms) rappresenta il successore strategico e lo standard attuale per lo sviluppo di moduli in ambiente SAP, sostituendo le tecnologie legacy SAPscript e SmartForms. Sebbene gli SmartForms siano ancora ampiamente presenti e supportati nei sistemi esistenti, tutto il nuovo sviluppo standard di SAP è realizzato esclusivamente con Adobe Forms, rendendone la conoscenza indispensabile per gli sviluppatori moderni.

I vantaggi pratici di questa transizione sono significativi e tangibili:

* **Design e User Experience Superiori:** Adobe Forms introduce un'esperienza di progettazione WYSIWYG (What You See Is What You Get) attraverso l'Adobe LiveCycle Designer (ALCD). Questo strumento è notevolmente più intuitivo e potente del Form Painter di SmartForms, consentendo la creazione di moduli professionali e visivamente accattivanti con un controllo granulare su ogni singolo oggetto.
* **Vera Interattività:** Questa è la differenza più marcata. A differenza degli SmartForms, che sono essenzialmente moduli di stampa statici, gli Adobe Forms possono essere pienamente interattivi. Gli utenti possono compilare campi, cliccare pulsanti, selezionare opzioni da menu a tendina e persino apporre firme digitali. I dati inseriti possono essere poi inviati al sistema SAP per essere elaborati, automatizzando i processi di data entry.
* **Indipendenza dalla Piattaforma e Consistenza:** Basandosi sullo standard universale PDF, gli Adobe Forms garantiscono che il layout del documento rimanga identico su qualsiasi macchina, stampante o sistema operativo. Questo elimina una vasta gamma di problemi di formattazione e stampa che erano comuni con le tecnologie precedenti.
* **Purezza Architetturale:** Adobe Forms impone una rigorosa separazione tra la logica di recupero dei dati (definita nell'Interfaccia) e il design del layout (creato nel Form). Questo contrasta nettamente con gli SmartForms, dove l'interfaccia è incorporata nel modulo stesso e il codice ABAP può essere inserito direttamente nel layout, rendendo la manutenzione complessa e soggetta a errori.

La seguente tabella offre un confronto diretto per evidenziare le differenze chiave per chi proviene dal mondo SmartForms.

| Funzionalità | SAP SmartForms | SAP Interactive Forms by Adobe |
|---|---|---|
| Tecnologia Core | Tecnologia proprietaria SAP | Basato su standard PDF e XML |
| Transazione Principale | SMARTFORMS | SFP |
| Strumento di Design | Form Painter (integrato in SAP GUI) | Adobe LiveCycle Designer (ALCD) (integrato ma installato localmente) |
| Interattività | Limitata (solo Web Forms con input di base) | Completa (campi di input, pulsanti, dropdown, firme digitali, scenari online/offline) |
| Separazione Dati/Layout | Parziale (l'interfaccia è incorporata, il codice ABAP può essere inserito nel layout) | Stretta e obbligatoria (Interfaccia, Contesto e Layout sono oggetti distinti) |
| Riusabilità dell'Interfaccia | No, l'interfaccia è legata al singolo SmartForm | Sì, un'interfaccia può essere usata da più form |
| Scripting | Solo ABAP all'interno dei nodi di codice | JavaScript e FormCalc (eseguiti nel layout) |
| Dipendenze di Sistema | Solo SAP ABAP Stack | SAP ABAP Stack + Java Stack per Adobe Document Services (ADS) (On-premise) |
| Formato di Output | OTF (Output Text Format), convertibile in PDF | PDF nativo, PCL, PostScript, ZPL |
| Accessibilità | Non supportata nativamente | Supportata (conforme a standard come Section 508) |
| Versioning | Manuale (upload/download locale) | Versioning standard del Repository ABAP |

### Architettura Core e Setup: Il Motore Dietro il Form
Per funzionare correttamente, gli Adobe Forms si basano su un'architettura più articolata rispetto agli SmartForms, che introduce nuovi componenti sia lato server che client.

**Adobe Document Services (ADS):** L'ADS è il cuore del sistema, il motore di rendering che risiede su un Application Server (AS) Java. È un insieme di servizi web che, al momento dell'esecuzione, riceve il template del form (in formato XML) e i dati applicativi dal sistema ABAP, li unisce e genera il documento PDF finale. Negli scenari interattivi, l'ADS è anche responsabile dell'estrazione dei dati XML inseriti dall'utente dal PDF inviato.

**Il Requisito del Java Stack (On-Premise):** Una delle differenze architetturali più importanti rispetto agli SmartForms è che, in un ambiente on-premise, l'ADS richiede un AS Java dedicato per poter funzionare. Questa è una considerazione fondamentale per i team Basis e di sviluppo, poiché introduce un ulteriore livello nell'infrastruttura di sistema. È importante notare che le soluzioni più recenti basate su cloud, come SAP Forms service by Adobe su SAP Business Technology Platform (BTP), eliminano la necessità di gestire un AS Java locale, offrendo l'ADS come servizio gestito.

**Il Form Builder (Transazione SFP):** La transazione SFP è il punto di accesso centrale per tutto lo sviluppo di Adobe Forms. A differenza della transazione SMARTFORMS, la SFP gestisce due oggetti di sviluppo distinti ma collegati:
* L'Interfaccia: Un oggetto ABAP autonomo e riutilizzabile.
* Il Form: L'oggetto che contiene il layout e la mappatura dei dati.

**Adobe LiveCycle Designer (ALCD):** È lo strumento grafico per la progettazione del layout. Sebbene sia integrato nel processo di sviluppo tramite la transazione SFP, l'ALCD è un'applicazione Windows che deve essere installata separatamente sulla macchina locale dello sviluppatore. Per garantire la piena compatibilità e licenza, è fondamentale utilizzare la versione di ALCD fornita da SAP tramite il Service Marketplace.

L'architettura di un Adobe Form si fonda su tre pilastri concettuali che ne garantiscono la robustezza e la manutenibilità, imponendo una chiara separazione dei compiti che spesso veniva meno nello sviluppo con SmartForms.

* **L'Interfaccia:** Questo è un oggetto di sviluppo autonomo che definisce il "contratto dati" del form. Contiene i parametri di importazione ed esportazione, le definizioni di dati globali e tipi, e una sezione di inizializzazione dove è possibile scrivere codice ABAP per recuperare i dati dal database (ad esempio, tramite istruzioni SELECT). La sua principale forza è la riusabilità: la stessa interfaccia (es. una che recupera i dati di un ordine di vendita) può essere utilizzata da più form (es. conferma d'ordine, bolla di consegna, fattura), garantendo consistenza e riducendo la duplicazione del codice.
* **Il Contesto (Context):** Il contesto agisce da ponte tra l'interfaccia e il layout. È una struttura ad albero all'interno dell'oggetto Form dove lo sviluppatore mappa esplicitamente quali dati, provenienti dall'interfaccia, devono essere resi disponibili per il design del layout. Questo permette un controllo preciso sul flusso di dati: non tutti i dati dell'interfaccia devono necessariamente essere esposti al layout, ma solo quelli strettamente necessari.
* **Il Layout:** Questo è il design visuale del form, creato nell'ALCD. Contiene elementi statici (testo, immagini, loghi) ed elementi dinamici (campi di testo, tabelle, checkbox) che vengono "legati" (data binding) ai nodi definiti nel Contesto.

Questa architettura a tre livelli introduce una disciplina di sviluppo che, sebbene possa apparire più complessa inizialmente, si traduce in moduli molto più facili da manutenere, scalare e debuggare. La ricerca di un errore logico non richiede più di ispezionare il codice ABAP nascosto nel layout, come poteva accadere con gli SmartForms, poiché la logica di business è confinata nell'interfaccia o nel programma chiamante, mentre il layout si occupa esclusivamente della presentazione e dell'interazione con l'utente. Tuttavia, questa stessa architettura distribuita implica che la risoluzione dei problemi si estende oltre il solo mondo ABAP. Un errore, come la comune Csoap Exception, può originare da una configurazione errata dell'ADS sul Java Stack, da problemi di connessione RFC, da un'installazione non corretta dell'ALCD o persino dalla versione di Adobe Reader installata sul client dell'utente finale.
`
                },
                {
                    title: "Parte 2: Il Ciclo di Sviluppo: Costruire e Potenziare i Form",
                    content: `
Questa parte del report si addentra negli aspetti pratici dello sviluppo, guidando l'utente dalla creazione di un semplice form "Hello, World" fino alla gestione di layout complessi, contenuti dinamici e scripting.

### Il Tuo Primo Adobe Form: Guida Pratica Passo-Passo
Questo tutorial pratico illustra la creazione di un semplice Adobe Form per visualizzare i dati di un cliente, seguendo un approccio strutturato che evidenzia i tre pilastri dell'architettura.

**Passo 1: Creare l'Interfaccia (Transazione SFP)**
L'interfaccia definisce quali dati il nostro form riceverà e come verranno recuperati.
* Eseguire la transazione SFP.
* Selezionare il radio button **Interfaccia**, inserire un nome (es. Z_IF_CUSTOMER_DATA) e cliccare su Crea.
* Nella scheda **Proprietà**, inserire una descrizione e selezionare il tipo di interfaccia **Interfaccia ABAP Dictionary-Based**.
* Navigare nella cartella **Import** dell'interfaccia. Qui definiamo i parametri di input. Creare un nuovo parametro di importazione, ad esempio IM_CUSTOMER_ID di tipo KUNNR.
* Navigare in **Definizioni globali -> Dati globali**. Qui dichiariamo le strutture dati che conterranno i dati del cliente, ad esempio GS_CUSTOMER_DETAILS di tipo KNA1.
* Navigare in **Inizializzazione codice**. Questa sezione permette di scrivere codice ABAP che verrà eseguito prima della chiamata al form. Qui inseriamo la logica per recuperare i dati.
\`\`\`abap
SELECT SINGLE *
  FROM kna1
  INTO gs_customer_details
  WHERE kunnr = im_customer_id.
\`\`\`
Salvare e attivare l'interfaccia. Ora abbiamo un oggetto riutilizzabile che, dato un ID cliente, ne recupera i dettagli.

**Passo 2: Creare l'Oggetto Form (Transazione SFP)**
Ora creiamo il form e lo colleghiamo alla nostra interfaccia.
* Tornare alla schermata iniziale di SFP.
* Selezionare il radio button **Form**, inserire un nome (es. Z_FRM_CUSTOMER_SHEET) e cliccare su Crea.
* Nella finestra di dialogo, inserire una descrizione e associare l'interfaccia creata in precedenza (Z_IF_CUSTOMER_DATA).
* Salvare il form.

**Passo 3: Definire il Contesto**
Il contesto funge da ponte, specificando quali dati dell'interfaccia saranno visibili nel layout.
* Nella parte sinistra della schermata del form, si vedranno i parametri dell'interfaccia (Import, Dati globali, ecc.).
* Trascinare la struttura GS_CUSTOMER_DETAILS dalla sezione **Dati globali** (a sinistra) all'interno del nodo **Contesto** (a destra).
* Espandere il nodo GS_CUSTOMER_DETAILS nel contesto e disattivare (clic destro -> Disattiva) tutti i campi non necessari, lasciando attivi solo quelli che vogliamo visualizzare, come KUNNR, NAME1, e ORT01.
* Salvare.

**Passo 4: Progettare il Layout in ALCD**
Questa è la fase di design visuale.
* Cliccare sulla scheda **Layout**. Verrà avviato l'Adobe LiveCycle Designer (ALCD).
* L'interfaccia di ALCD è composta da diverse finestre chiamate "Palette". Comprendere le principali è fondamentale per lavorare efficacemente.

| Nome Palette | Scopo | Caso d'Uso Pratico |
|---|---|---|
| Data View | Mostra la struttura dati definita nel Contesto SAP. È la fonte dei dati per il form. | Trascinare il campo NAME1 da questa palette al layout per creare un campo di testo già collegato al nome del cliente. |
| Hierarchy | Mostra la struttura gerarchica di tutti gli oggetti presenti nel layout (pagine, subform, campi). | Selezionare un oggetto specifico (es. un subform) per modificarne le proprietà, come il tipo di layout (Flowed/Positioned). |
| Object Library | Contiene tutti gli elementi UI che possono essere aggiunti al form (campi di testo, immagini, tabelle, pulsanti, etc.). | Trascinare un oggetto Text per creare un'intestazione statica come "Scheda Cliente". |
| Font / Paragraph | Permettono di formattare l'aspetto del testo (carattere, dimensione, colore, allineamento). | Selezionare un'intestazione e impostare il font su "Arial", dimensione "14pt", e stile "Grassetto". |
| Layout | Controlla le proprietà di posizionamento e dimensionamento di un oggetto (margini, ancoraggio, dimensioni x/y). | Impostare un margine di 1 cm per un campo di testo per distanziarlo dagli altri elementi. |
| Border | Permette di definire bordi e sfondi per gli oggetti. | Aggiungere un bordo sottile attorno a un gruppo di campi per raggrupparli visivamente. |
| Script Editor | L'editor dove si scrive codice JavaScript o FormCalc per aggiungere logica dinamica al form. | Scrivere uno script per calcolare un totale o nascondere una sezione in base a una condizione. |

* Dalla **Object Library**, trascinare un oggetto Text sul layout e scrivere "Scheda Dati Cliente". Formattarlo usando le palette Font e Paragraph.
* Dalla palette **Data View**, trascinare i campi KUNNR, NAME1, e ORT01 sul layout. ALCD creerà automaticamente un'etichetta (il nome del campo) e un campo di testo associato, già collegato (binding) al dato corrispondente.
* Salvare il layout e tornare a SAP GUI. Attivare il form.

**Passo 5: Scrivere il Programma Driver ABAP**
Infine, creiamo un programma ABAP per chiamare il form e generare il PDF.
Creare un nuovo programma eseguibile in SE38.
Il codice seguente illustra la sequenza di chiamate standard per generare un Adobe Form.
\`\`\`abap
REPORT z_call_customer_form.

PARAMETERS: p_cust TYPE kunnr DEFAULT '0000001234'.

DATA:
  gv_fm_name         TYPE rs38l_fnam,
  gs_fp_outputparams TYPE sfpoutputparams,
  gs_fp_docparams    TYPE sfpdocparams,
  gs_fp_formoutput   TYPE fpformoutput,
  gv_pdf_xstring     TYPE xstring,
  go_cx_root         TYPE REF TO cx_root,
  gv_messaggio_errore TYPE string.

START-OF-SELECTION.

  " 1. Impostare i parametri di output per ottenere il PDF come xstring
  gs_fp_outputparams-getpdf   = abap_true.
  gs_fp_outputparams-nodialog = abap_true.

  TRY.
      " 2. Aprire il job di stampa
      CALL FUNCTION 'FP_JOB_OPEN'
        CHANGING
          ie_outputparams = gs_fp_outputparams.
      IF sy-subrc <> 0.
        MESSAGE 'Errore durante l''apertura del job di stampa (FP_JOB_OPEN).' TYPE 'E'.
        LEAVE PROGRAM.
      ENDIF.

      " 3. Ottenere il nome del function module del form
      CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
        EXPORTING
          i_name     = 'Z_FRM_CUSTOMER_SHEET'
        IMPORTING
          e_funcname = gv_fm_name.
      IF sy-subrc <> 0.
        MESSAGE 'Impossibile trovare il function module per il form Z_FRM_CUSTOMER_SHEET.' TYPE 'E'.
        LEAVE PROGRAM.
      ENDIF.

      " 4. Chiamare il function module del form
      CALL FUNCTION gv_fm_name
        EXPORTING
          im_customer_id   = p_cust
        IMPORTING
          /1bcdwb/formoutput = gs_fp_formoutput
        EXCEPTIONS
          usage_error      = 1
          system_error     = 2
          internal_error   = 3.
      IF sy-subrc <> 0.
        MESSAGE 'Errore durante la generazione del modulo Adobe Form.' TYPE 'E'.
        LEAVE PROGRAM.
      ENDIF.

      " 5. Chiudere il job di stampa
      CALL FUNCTION 'FP_JOB_CLOSE'.
      IF sy-subrc <> 0.
        MESSAGE 'Avviso: errore durante la chiusura del job di stampa.' TYPE 'W'.
      ENDIF.

    CATCH cx_root INTO go_cx_root.
      gv_messaggio_errore = go_cx_root->get_text( ).
      MESSAGE gv_messaggio_errore TYPE 'E'.
      LEAVE PROGRAM.
  ENDTRY.


  " 6. Visualizzare il PDF
  gv_pdf_xstring = gs_fp_formoutput-pdf.

  IF gv_pdf_xstring IS NOT INITIAL.
    DATA: lv_percorso_file TYPE string,
          lv_percorso      TYPE string,
          lv_nome_file     TYPE string,
          lt_pdf_solix     TYPE solix_tab. " Tabella per dati binari

    " Crea un nome file univoco
    CONCATENATE 'scheda_cliente_' p_cust '.pdf' INTO lv_nome_file.

    " Ottiene il percorso completo per il file temporaneo
    " *** CORREZIONE 1: Usato il parametro corretto 'temp_dir' ***
    CALL METHOD cl_gui_frontend_services=>get_temp_directory
      CHANGING
        temp_dir = lv_percorso
      EXCEPTIONS
        OTHERS   = 4.
    IF sy-subrc <> 0.
      MESSAGE 'Impossibile determinare la directory temporanea.' TYPE 'E'.
      LEAVE PROGRAM.
    ENDIF.

    CONCATENATE lv_percorso '\' lv_nome_file INTO lv_percorso_file.

    " *** CORREZIONE 2: Convertire XSTRING in una tabella per GUI_DOWNLOAD ***
    lt_pdf_solix = cl_bcs_convert=>xstring_to_solix( iv_xstring = gv_pdf_xstring ).

    " Salva il PDF in un file locale usando la tabella di dati binari
    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename = lv_percorso_file
        filetype = 'BIN' " Obbligatorio per dati binari
      CHANGING
        data_tab = lt_pdf_solix
      EXCEPTIONS
        OTHERS   = 24.
    IF sy-subrc <> 0.
      MESSAGE 'Errore durante il salvataggio del file PDF temporaneo.' TYPE 'E'.
      LEAVE PROGRAM.
    ENDIF.

    " Esegue il file PDF con il visualizzatore predefinito
    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        document = lv_percorso_file
      EXCEPTIONS
        OTHERS   = 8.
    IF sy-subrc <> 0.
      MESSAGE 'Impossibile aprire il file PDF.' TYPE 'W'.
    ENDIF.

  ELSE.
    MESSAGE 'Il modulo Adobe Form non ha restituito un PDF.' TYPE 'W'.
  ENDIF.
\`\`\`

### Padroneggiare i Contenuti Dinamici: Tabelle e Subform che si Adattano
La vera potenza degli Adobe Forms risiede nella loro capacità di gestire layout che si adattano dinamicamente al volume dei dati. Questo si ottiene attraverso l'uso strategico dei Subform.

**I Subform come Blocchi Costruttivi**
Un Subform è un contenitore che può raggruppare altri oggetti (campi, testo, immagini, e anche altri subform). Sono fondamentali per creare layout strutturati e si dividono in due tipologie cruciali:
* **Positioned (Posizionato):** Gli oggetti all'interno di questo subform hanno coordinate X/Y fisse. Il loro posizionamento è assoluto rispetto al subform contenitore. Questa tipologia è ideale per aree statiche del documento, come intestazioni (header) o piè di pagina (footer), dove gli elementi devono rimanere in una posizione precisa.
* **Flowed (Fluido):** Gli oggetti vengono disposti in sequenza, uno dopo l'altro, in base a una direzione di flusso (dall'alto in basso o da sinistra a destra). Il contenuto "fluisce" e lo spazio occupato dal subform si espande dinamicamente per contenere tutti gli oggetti. Questa è la tipologia essenziale per gestire contenuti di volume variabile, come le righe di una tabella, poiché permette al layout di crescere e generare automaticamente interruzioni di pagina quando necessario.

Il passaggio dal paradigma delle "finestre" statiche di SmartForms a quello dei subform "flowed" è un cambiamento concettuale fondamentale. Lo sviluppatore deve smettere di pensare in termini di riquadri a dimensione fissa e iniziare a concepire il documento come un flusso continuo di contenuti che si adatta ai dati. Padroneggiare questo concetto è la chiave per creare documenti complessi e flessibili, come fatture con un numero variabile di righe d'ordine.

**Creare Tabelle Dinamiche**
Per creare una tabella che si espande per mostrare tutte le righe di una tabella interna passata da SAP, seguire questi passaggi:
* Preparare il Layout: Nel layout ALCD, inserire un Subform dalla Object Library. Nelle proprietà di questo subform (Object -> Subform), impostare il Content su Flowed. Questo subform principale conterrà la nostra tabella.
* Creare la Tabella: Trascinare il nodo della tabella interna (es. IT_ITEMS) dalla Data View all'interno del subform fluido. ALCD creerà automaticamente una struttura di tabella con:
    * Un subform per l'intestazione (HeaderRow).
    * Un subform per la riga dati (Row1 o simile).
    * Campi di testo per le intestazioni delle colonne e campi dati per le righe, già collegati ai campi della struttura della tabella interna.
* Abilitare la Ripetizione: Selezionare il subform che rappresenta la riga dati (es. Row1) nella palette Hierarchy. Nella palette Object, andare alla scheda Binding. Qui, spuntare la casella "Repeat Subform for each Data Item". Questa opzione è il cuore della dinamicità: indica al form di creare un'istanza di questo subform per ogni riga presente nella tabella interna passata da SAP.
* Migliorare la Leggibilità: Per migliorare l'aspetto, selezionare l'oggetto tabella principale. Nella palette Object, andare alla scheda Row Shading e attivare Apply Alternating Row Shading per colorare le righe in modo alternato.

**Gestire la Paginazione e le Interruzioni di Pagina**
Un controllo preciso su dove e quando avvengono le interruzioni di pagina è cruciale per la professionalità di un documento.
* **Mantenere il Contenuto Unito:** Per evitare che un blocco di informazioni (es. un paragrafo di termini e condizioni) venga spezzato tra due pagine, è possibile raggruppare gli oggetti in un subform e, nelle proprietà di quest'ultimo (Object -> Subform), deselezionare la casella "Allow Page Breaks within Content".
* **Evitare Intestazioni Orfane:** Per assicurarsi che un'intestazione di sezione non rimanga da sola alla fine di una pagina, si può usare la proprietà Keep With Next. Selezionando il subform dell'intestazione, nella palette Object -> Pagination, si può impostare Keep With su Next Subform. Questo forza l'intestazione a spostarsi sulla pagina successiva se non c'è abbastanza spazio per essa e per almeno una parte del subform seguente.
* **Interruzioni Condizionali:** È possibile forzare un'interruzione di pagina o inserire un'intestazione intermedia in base ai dati. Ad esempio, in una tabella di voli ordinata per compagnia aerea, si potrebbe voler iniziare una nuova pagina ogni volta che la compagnia aerea cambia. Questo si ottiene selezionando la riga dati della tabella, andando nella palette Object -> Pagination e aggiungendo una condizione (Conditional Breaks). Qui si può scrivere un semplice script (es. in FormCalc) che confronta il valore della compagnia aerea della riga corrente con quello della riga precedente. Se sono diversi, si può impostare un'azione come Go to Next Page.

**Dare Vita ai Form con lo Scripting: FormCalc e JavaScript**
Lo scripting permette di aggiungere logica e interattività direttamente nel layout del form. ALCD supporta due linguaggi: FormCalc e JavaScript.

**Scegliere il Linguaggio Giusto**
La scelta tra FormCalc e JavaScript dipende dalla complessità del compito da svolgere.

| Scenario | Linguaggio Raccomandato | Motivazione | Esempio di Codice (concettuale) |
|---|---|---|---|
| Calcoli Aritmetici Semplici (somme, medie, totali) | FormCalc | Sintassi semplice e simile a Excel, ottimizzato per calcoli e più performante per queste operazioni. | Sum(QUANTITY * PRICE) |
| Funzioni Finanziarie, di Data e Ora | FormCalc | Offre una vasta libreria di funzioni built-in per queste operazioni, rendendo il codice conciso e leggibile. | DateFmt(Date(), "DD.MM.YYYY") |
| Formattazione Condizionale Semplice (nascondere/mostrare un campo) | JavaScript / FormCalc | Entrambi possono farlo facilmente. JavaScript è più comune per la manipolazione degli oggetti del form (DOM). | if ($.rawValue == "X") { this.presence = "hidden"; } |
| Validazione Complessa dei Campi (es. logica custom) | JavaScript | Molto più potente e flessibile per logiche complesse, espressioni regolari e manipolazione di stringhe. | if (event.value.match(/.../)) {... } |
| Iterare su Righe di una Tabella (es. calcolare un totale basato su condizioni) | JavaScript | Permette di creare cicli for per iterare sugli oggetti del form, cosa non possibile in FormCalc. | for (var i=0; i<rows.length; i++) {... } |
| Chiamate a Servizi Web (SOAP) | JavaScript | Offre le capacità necessarie per gestire richieste e risposte complesse, tipiche delle interazioni con servizi esterni. | SOAP.request(...) |

**Esempi Pratici di Scripting**
Gli script vengono inseriti nello Script Editor e associati a un evento specifico di un oggetto (es. calculate per un campo, click per un pulsante, initialize per un subform).

**Calcoli (FormCalc):** Per calcolare il totale di una riga in una tabella, selezionare il campo numerico del totale (es. ROW_TOTAL). Nell'evento calculate, scrivere:
\`\`\`
QUANTITY * UNIT_PRICE
\`\`\`
Per calcolare il gran totale nel piè di pagina della tabella, selezionare il campo del totale generale e, sempre nell'evento calculate, usare la funzione Sum():
\`\`\`
Sum(Row1.ROW_TOTAL[*])
\`\`\`
Questo somma tutte le istanze del campo ROW_TOTAL create dinamicamente.

**Logica Condizionale (JavaScript):** Per nascondere un subform (es. Subform_US_Address) se un campo flag non è impostato, selezionare il subform e, nell'evento initialize o form:ready, scrivere:
\`\`\`javascript
if (data.US_FLAG.rawValue!== "X") {
    this.presence = "hidden";
}
\`\`\`
this.presence può essere impostato su "visible", "hidden" (lo spazio viene collassato), o "invisible" (lo spazio viene mantenuto).

**Validazione dei Campi (JavaScript):** Per validare che un campo numerico (es. HOURS_WORKED) non superi 12, selezionare il campo e, nell'evento validate, scrivere:
\`\`\`javascript
if (this.rawValue > 12) {
    xfa.host.messageBox("Le ore non possono superare 12.", "Errore di Validazione", 1);
    this.rawValue = null; // Opzionale: pulisce il campo
    xfa.host.setFocus(this); // Opzionale: mantiene il focus sul campo
    return false;
}
\`\`\`
xfa.host.messageBox mostra un popup all'utente.

Una best practice fondamentale per le prestazioni è quella di eseguire la logica di business pesante in ABAP. Il programma driver o l'interfaccia dovrebbero pre-calcolare dati complessi, aggregazioni e flag, passando al form solo i risultati. Lo scripting nel layout dovrebbe essere riservato alla logica di presentazione (calcoli semplici, formattazione, visibilità), non a complesse elaborazioni di dati. Questo approccio garantisce form più veloci e facili da manutenere, separando nettamente la logica di business (ABAP) dalla logica di presentazione (scripting).
`
                },
                {
                    title: "Parte 3: Funzionalità Avanzate e Scenari di Business",
                    content: `
Questa sezione esplora funzionalità di alto valore e le integra in esempi di processi di business end-to-end, dimostrando come gli Adobe Forms vadano oltre la semplice generazione di documenti.

### Elementi Avanzati del Form: Codici a Barre, Grafica e Firme Digitali
Oltre ai campi di testo e alle tabelle, ALCD offre una libreria di oggetti standard per esigenze specifiche.

**Codici a Barre:** L'aggiunta di un codice a barre è un processo sorprendentemente semplice e non richiede la creazione di formati di caratteri speciali come in passato.
* Dalla **Object Library**, trascinare l'oggetto Barcode desiderato (es. Code 128, QR Code) sul layout.
* Selezionare l'oggetto barcode. Nella palette Object, scheda Binding, collegare il valore del barcode a un campo del contesto (es. il numero dell'ordine di vendita, VBAK-VBELN).
* Opzionalmente, nella scheda Field dell'oggetto, è possibile nascondere il testo leggibile dall'uomo che appare sotto il codice a barre, impostando la Location del testo su None (non disponibile per tutti i tipi di barcode).

**Grafica e Loghi:** Per inserire un logo aziendale o un'altra immagine statica:
* Caricare l'immagine nel sistema SAP tramite la transazione SE78.
* Nell'interfaccia del form, creare una variabile globale di tipo XSTRING.
* Nel codice di inizializzazione dell'interfaccia o nel programma driver, recuperare il contenuto binario dell'immagine e popolarlo nella variabile XSTRING.
* Nel contesto del form, mappare questa variabile.
* Nel layout ALCD, trascinare un oggetto Image Field dalla Object Library e collegarlo (binding) al nodo dell'immagine nel Data View.

**Firme Digitali:** Gli Adobe Forms supportano nativamente l'inclusione di campi per la firma digitale. Questo permette di creare processi di approvazione completamente digitali e legalmente validi. Un utente può aprire il PDF, apporre la propria firma digitale (che certifica il contenuto del documento in quel momento) e salvarlo. Il sistema SAP può successivamente verificare la validità e l'autenticità delle firme presenti nel documento.

### Scenari di Business Chiave in Pratica
### 7.1 Lo Scenario Offline: Acquisizione Dati Oltre i Confini del Sistema
Questo è uno degli scenari più potenti e unici degli Adobe Forms. Permette di estendere un processo di business a utenti che non hanno accesso diretto al sistema SAP.

**Concetto:** Un utente (es. un fornitore, un tecnico sul campo) riceve un form PDF interattivo precompilato via email. Lo compila offline, senza bisogno di connessione a SAP, e lo invia indietro (es. tramite un pulsante "Invia via Email" nel PDF). Il sistema SAP riceve l'email, estrae automaticamente i dati inseriti dall'utente e aggiorna il database (es. creando una richiesta di servizio, aggiornando i dati anagrafici).

**Passi di Implementazione:**
* Design del Form: Creare un form interattivo con campi di input e un pulsante di invio configurato per l'invio via email.
* Programma Driver (Invio): Un programma ABAP genera il PDF, lo precompila con dati iniziali (es. nome del fornitore) e lo invia come allegato email utilizzando le classi BCS (Business Communication Services).
* Configurazione Inbound (Basis): Configurare SAPconnect (transazione SCOT) per monitorare una casella di posta elettronica dedicata e definire una regola di uscita per processare le email in arrivo che contengono i form compilati.
* Classe Handler: Creare una classe ABAP che implementa l'interfaccia IF_FP_OFFLINE. Il metodo PROCESS_INBOUND di questa classe verrà eseguito automaticamente da SAPconnect quando riceve un'email corrispondente alla regola.
* Estrazione Dati: All'interno del metodo PROCESS_INBOUND, il contenuto del PDF allegato è disponibile. Utilizzando le classi CL_FP e IF_FP_PDF_OBJECT, si chiama l'ADS per estrarre i dati. I metodi l_pdfobj->set_document() e l_pdfobj->execute() con il task SET_TASK_EXTRACTDATA restituiscono i dati inseriti dall'utente come una stringa XML.
* Parsing XML e Aggiornamento: Utilizzare la libreria iXML di SAP (IF_IXML) per analizzare (parsare) la stringa XML, estrarre i valori dei campi e utilizzarli per aggiornare il database SAP, ad esempio chiamando una BAPI o usando istruzioni UPDATE.

Questo scenario trasforma il form da un semplice documento a un vero e proprio strumento di automazione dei processi di business, richiedendo una sinergia di competenze ABAP, design di PDF e configurazione Basis.

### 7.2 Integrazione Email: Distribuire e Inviare Form via Posta Elettronica
Questo scenario si concentra specificamente sul meccanismo di invio email, che è un componente dello scenario offline ma è utile anche per la semplice distribuzione di documenti.

**Passi di Implementazione (ABAP):**
* **Generare il PDF in Memoria:** Nel programma driver, impostare il parametro getpdf = 'X' nella struttura fp_outputparams prima di chiamare FP_JOB_OPEN. Questo istruisce l'ADS a restituire il contenuto binario del PDF in una variabile XSTRING invece di inviarlo allo spooler.
* **Convertire in Binario:** Il PDF viene restituito come XSTRING. Per allegarlo a un'email, deve essere convertito in una tabella di tipo SOLIX_TAB. Utilizzare il function module SCMS_XSTRING_TO_BINARY per questa conversione.
* **Usare le Classi BCS:** Utilizzare le classi standard BCS (CL_BCS,  CL_DOCUMENT_BCS, CL_CAM_ADDRESS_BCS) per costruire e inviare l'email.

**Codice Esempio:**
\`\`\`abap
REPORT z_call_customer_form.

PARAMETERS: p_cust TYPE kunnr DEFAULT '0000001234'.

DATA:
  gv_fm_name         TYPE rs38l_fnam,
  gs_fp_outputparams TYPE sfpoutputparams,
  gs_fp_docparams    TYPE sfpdocparams,
  gs_fp_formoutput   TYPE fpformoutput,
  gv_pdf_xstring     TYPE xstring,
  go_cx_root         TYPE REF TO cx_root,
  gv_messaggio_errore TYPE string.

START-OF-SELECTION.

  " 1. Impostare i parametri di output per ottenere il PDF come xstring
  gs_fp_outputparams-getpdf   = abap_true.
  gs_fp_outputparams-nodialog = abap_true.

  TRY.
      " 2. Aprire il job di stampa
      CALL FUNCTION 'FP_JOB_OPEN'
        CHANGING
          ie_outputparams = gs_fp_outputparams.
      IF sy-subrc <> 0.
        MESSAGE 'Errore durante l''apertura del job di stampa (FP_JOB_OPEN).' TYPE 'E'.
        LEAVE PROGRAM.
      ENDIF.

      " 3. Ottenere il nome del function module del form
      CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
        EXPORTING
          i_name     = 'Z_FRM_CUSTOMER_SHEET'
        IMPORTING
          e_funcname = gv_fm_name.
      IF sy-subrc <> 0.
        MESSAGE 'Impossibile trovare il function module per il form Z_FRM_CUSTOMER_SHEET.' TYPE 'E'.
        LEAVE PROGRAM.
      ENDIF.

      " 4. Chiamare il function module del form
      CALL FUNCTION gv_fm_name
        EXPORTING
          im_customer_id   = p_cust
        IMPORTING
          /1bcdwb/formoutput = gs_fp_formoutput
        EXCEPTIONS
          usage_error      = 1
          system_error     = 2
          internal_error   = 3.
      IF sy-subrc <> 0.
        MESSAGE 'Errore durante la generazione del modulo Adobe Form.' TYPE 'E'.
        LEAVE PROGRAM.
      ENDIF.

      " 5. Chiudere il job di stampa
      CALL FUNCTION 'FP_JOB_CLOSE'.
      IF sy-subrc <> 0.
        MESSAGE 'Avviso: errore durante la chiusura del job di stampa.' TYPE 'W'.
      ENDIF.

    CATCH cx_root INTO go_cx_root.
      gv_messaggio_errore = go_cx_root->get_text( ).
      MESSAGE gv_messaggio_errore TYPE 'E'.
      LEAVE PROGRAM.
  ENDTRY.


  " 6. Visualizzare il PDF
  gv_pdf_xstring = gs_fp_formoutput-pdf.

  IF gv_pdf_xstring IS NOT INITIAL.
    DATA: lv_percorso_file TYPE string,
          lv_percorso      TYPE string,
          lv_nome_file     TYPE string,
          lt_pdf_solix     TYPE solix_tab. " Tabella per dati binari

    " Crea un nome file univoco
    CONCATENATE 'scheda_cliente_' p_cust '.pdf' INTO lv_nome_file.

    " Ottiene il percorso completo per il file temporaneo
    " *** CORREZIONE 1: Usato il parametro corretto 'temp_dir' ***
    CALL METHOD cl_gui_frontend_services=>get_temp_directory
      CHANGING
        temp_dir = lv_percorso
      EXCEPTIONS
        OTHERS   = 4.
    IF sy-subrc <> 0.
      MESSAGE 'Impossibile determinare la directory temporanea.' TYPE 'E'.
      LEAVE PROGRAM.
    ENDIF.

    CONCATENATE lv_percorso '\' lv_nome_file INTO lv_percorso_file.

    " *** CORREZIONE 2: Convertire XSTRING in una tabella per GUI_DOWNLOAD ***
    lt_pdf_solix = cl_bcs_convert=>xstring_to_solix( iv_xstring = gv_pdf_xstring ).

    " Salva il PDF in un file locale usando la tabella di dati binari
    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename = lv_percorso_file
        filetype = 'BIN' " Obbligatorio per dati binari
      CHANGING
        data_tab = lt_pdf_solix
      EXCEPTIONS
        OTHERS   = 24.
    IF sy-subrc <> 0.
      MESSAGE 'Errore durante il salvataggio del file PDF temporaneo.' TYPE 'E'.
      LEAVE PROGRAM.
    ENDIF.

    " Esegue il file PDF con il visualizzatore predefinito
    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        document = lv_percorso_file
      EXCEPTIONS
        OTHERS   = 8.
    IF sy-subrc <> 0.
      MESSAGE 'Impossibile aprire il file PDF.' TYPE 'W'.
    ENDIF.

  ELSE.
    MESSAGE 'Il modulo Adobe Form non ha restituito un PDF.' TYPE 'W'.
  ENDIF.
\`\`\`

### 7.3 Archiviazione e Compliance: Conservazione a Lungo Termine con Archivelink
Per requisiti legali e di audit, molti documenti (fatture, ordini) devono essere archiviati a lungo termine. SAP Archivelink è la soluzione standard per questo scopo.

**Concetto:** Dopo la generazione, il PDF viene inviato ad Archivelink, dove viene memorizzato e collegato a un oggetto di business SAP (es. la fattura numero 4711 è collegata all'oggetto VBRK 4711). Questo permette un facile recupero del documento direttamente dal contesto di business in SAP.

**Passi di Implementazione:**
* **Configurazione (SPRO):** È necessaria una configurazione di base di Archivelink, che include la definizione di un tipo di documento (es. ZINVOICE) e il suo collegamento a un oggetto di business SAP (es. VBRK) e a un content repository.
* **Archiviazione tramite Parametri:** Il metodo più semplice è impostare i parametri di archiviazione nel programma driver prima di chiamare il form.
    * Nella struttura fp_outputparams, impostare arcmode = '3' (stampa e archivia) o '2' (solo archivia).
    * Popolare la tabella daratab nella struttura fp_docparams con i dettagli del collegamento: SAP_OBJECT (es. 'VBRK'), AR_OBJECT (il tipo di documento, es. 'ZINVOICE'), e OBJECT_ID (la chiave dell'oggetto, es. il numero di fattura). Il sistema gestirà automaticamente l'archiviazione.
* **Archiviazione Manuale (con più controllo):** Un approccio alternativo consiste nel generare prima il PDF in una variabile XSTRING (con getpdf = 'X') e poi chiamare esplicitamente il function module ARCHIV_CREATE_TABLE. Questo metodo offre maggiore flessibilità, ad esempio per decidere se archiviare o meno in base a una logica successiva alla generazione del form.

### Applicazione Pratica: Costruire un Modulo d'Ordine d'Acquisto
Questa sezione finale sintetizza molti dei concetti discussi in un esempio reale e moderno: l'estensione di un form standard per l'ordine d'acquisto (PO) in un ambiente S/4HANA, utilizzando l'approccio basato su OData. Questo metodo si allinea con la strategia Fiori di SAP, dove il backend espone i dati tramite servizi e l'interfaccia utente (in questo caso, il form) li consuma. Questo approccio è strategicamente importante per gli sviluppatori che vogliono rimanere aggiornati.

**Passo 1: Copiare il Form Standard:** Iniziare nella Fiori App "Maintain Form Template" e creare una copia del template standard per l'ordine d'acquisto, MM_PUR_PURCHASE_ORDER, dandogli un nome custom (es. ZZ1_MY_PO_FORM).
**Passo 2: Estendere il Servizio OData:** Il form standard utilizza il servizio OData FDP_EF_PURCHASE_ORDER_SRV. Per aggiungere campi custom, è necessario estenderlo.
* Nella transazione SEGW, creare un nuovo progetto ridefinendo il servizio standard FDP_EF_PURCHASE_ORDER_SRV.
* Questo crea nuove classi DPC (Data Provider Class) e MPC (Model Provider Class) che ereditano da quelle standard.
**Passo 3: Aggiungere Campi Custom:** Aggiungere i campi desiderati (es. per "Indirizzo di Fatturazione" e "Importo in Lettere") alla struttura dell'entità OData. Questo si fa creando una struttura "append" alla struttura standard utilizzata dall'entità (es. TDS_ME_PO_HEADER).
**Passo 4: Ridefinire il Metodo GET_ENTITY:** Nella classe DPC estesa (DPC_EXT), ridefinire il metodo che recupera i dati dell'ordine (es. PURCHASEORDER_GET_ENTITY). All'interno di questo metodo:
* Chiamare il metodo della superclasse (super->...) per ottenere tutti i dati standard.
* Scrivere codice ABAP aggiuntivo per popolare i nuovi campi custom. Ad esempio, eseguire una SELECT per recuperare l'indirizzo di fatturazione o chiamare il function module SPELL_AMOUNT per convertire l'importo totale in parole.
**Passo 5: Collegare i Nuovi Dati:**
* Assegnare il nuovo servizio OData custom al form custom nella transazione SFP.
* Scaricare i file di layout (.xdp) e schema dati (.xsd) aggiornati dalla Fiori App "Maintain Form Template".
* Aprire il layout in ALCD, creare una nuova connessione dati utilizzando il file .xsd scaricato, e trascinare i nuovi campi dal Data View sul layout per creare i relativi campi di testo. Eseguire il binding dei dati.
* Caricare il file .xdp modificato di nuovo nel sistema.
**Passo 6: Configurare l'Output Determination (OPD):** Infine, configurare il sistema per utilizzare il nuovo form custom. Nella Fiori App "Output Parameter Determination", creare una nuova regola per l'oggetto di business "Purchase Order" che, in base a determinate condizioni (es. per uno specifico tipo di ordine o società), utilizzi il template del form ZZ1_MY_PO_FORM.
`
                },
                {
                    title: "Parte 4: Conclusioni e Raccomandazioni",
                    content: `
Questa sezione finale fornisce una guida di alto livello per aiutare l'utente a prendere decisioni informate e a scrivere moduli migliori e più manutenibili.

### Best Practice e Come Rendere i Form a Prova di Futuro
La padronanza di SAP Interactive Forms by Adobe richiede non solo la conoscenza tecnica degli strumenti, ma anche l'adozione di best practice che garantiscano prestazioni, manutenibilità e allineamento con la strategia futura di SAP.

**Prestazioni:** La regola d'oro è eseguire la logica pesante in ABAP. Calcoli complessi, aggregazioni di dati e selezioni intensive dal database devono essere eseguiti nel programma driver o nell'inizializzazione dell'interfaccia, non negli script del form. Gli script dovrebbero limitarsi alla logica di presentazione per garantire la massima velocità di rendering. Funzionalità come il caching dei form e il bundling (invio di più form in un'unica chiamata all'ADS) possono ulteriormente ottimizzare le prestazioni in scenari di stampa massiva.

**Manutenibilità:** La chiave per form manutenibili è la modularità. Sfruttare appieno la riusabilità delle interfacce per garantire la coerenza dei dati tra diversi documenti. All'interno del layout, utilizzare i subform per raggruppare logicamente gli elementi, creando un design strutturato e più facile da modificare in futuro.

### Il Futuro: SAP Forms Service on BTP
La direzione strategica di SAP per l'elaborazione dei moduli, specialmente in ambienti cloud e ibridi, è il SAP Forms service by Adobe. Questo servizio, ospitato su SAP Business Technology Platform (BTP), offre le funzionalità dell'ADS tramite una REST API. Il suo vantaggio principale è l'eliminazione della necessità di installare e mantenere un AS Java on-premise, semplificando notevolmente l'infrastruttura di sistema. Comprendere questo modello basato su servizi è fondamentale per gli sviluppatori che vogliono progettare soluzioni a prova di futuro.
`
                }
            ]
        }
    ]
};