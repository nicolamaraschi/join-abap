export const transactionData = {
  // Dati per le App Fiori
  fiori: {
    introduction: {
      title: "Guida di Riferimento alle App Core di SAP S/4HANA (Fiori)",
      content: "Questo report fornisce una guida di riferimento esaustiva alle funzionalità operative centrali all'interno dei moduli SAP S/4HANA, analizzando le principali App Fiori. In S/4HANA, l'interfaccia utente primaria è SAP Fiori, ma le transazioni classiche del SAP GUI sono ancora accessibili."
    },
    modules: [
      {
        id: "FI",
        name: "Financial Accounting (FI)",
        context: "Il modulo FI in S/4HANA è stato trasformato dall'introduzione dell'Universal Journal (tabella ACDOCA), che unifica i dati di FI e CO, eliminando le riconciliazioni e abilitando analisi in tempo reale.",
        submodules: [
          { id: "FI-GL", name: "Contabilità Generale (FI-GL)", transactions: [
              { process: "Gestione Dati Anagrafici C/G", appName: "Manage G/L Account Master Data", appId: "F0731A", tCode: "FS00, FSP0, FSS0", purpose: "Centralizza la creazione, modifica e visualizzazione dei dati anagrafici dei conti Co.Ge." },
              { process: "Registrazione Documento C/G", appName: "Post General Journal Entries", appId: "F0718", tCode: "FB50, F-02", purpose: "Interfaccia moderna per registrazioni contabili generali, alternativa Fiori a FB50." },
              { process: "Visualizzazione Partite C/G", appName: "Display Line Items in General Ledger", appId: "F2217", tCode: "FBL3N, FAGLL03", purpose: "Potente successore di FBL3N per l'analisi dettagliata delle partite Co.Ge. in tempo reale da ACDOCA." },
              { process: "Visualizzazione Saldi C/G", appName: "Display G/L Account Balances", appId: "F0702", tCode: "FS10N, FAGLB03", purpose: "Equivalente Fiori per la visualizzazione dei saldi contabili aggregati per periodo." },
              { process: "Monitoraggio Contabilità Generale", appName: "General Ledger Overview", appId: "F2445", tCode: "N/A", purpose: "Cockpit analitico per monitorare i KPI chiave della contabilità generale con navigazione drill-down." },
          ]},
          { id: "FI-AP", name: "Contabilità Fornitori (FI-AP)", transactions: [
               { process: "Registrazione Fattura Fornitore", appName: "Create Incoming Invoices", appId: "F0859", tCode: "FB60", purpose: "App principale per la registrazione di fatture fornitore non collegate a un OdA." },
               { process: "Gestione Partite Fornitore", appName: "Manage Supplier Line Items", appId: "F0712", tCode: "FBL1N", purpose: "Controparte Fiori di FBL1N per visualizzare, filtrare e gestire le partite dei fornitori." },
               { process: "Esecuzione Pagamenti", appName: "Manage Automatic Payments", appId: "F0770", tCode: "F110", purpose: "Modernizza la gestione del programma di pagamento automatico (F110) in un'unica interfaccia." },
               { process: "Monitoraggio Contabilità Fornitori", appName: "Accounts Payable Overview", appId: "F2917", tCode: "N/A", purpose: "Cockpit analitico per il monitoraggio in tempo reale dei KPI come DPO e fatture bloccate." },
               { process: "Visualizzazione Flusso Documenti", appName: "Display Process Flow - Accounts Payable", appId: "F2691", tCode: "N/A", purpose: "Visualizzazione grafica dell'intera catena di documenti, dall'OdA al pagamento." },
          ]},
          { id: "FI-AR", name: "Contabilità Clienti (FI-AR)", transactions: [
               { process: "Registrazione Fattura Cliente", appName: "Create Outgoing Invoices", appId: "F0798", tCode: "FB70", purpose: "Per la registrazione di fatture clienti che non originano dal ciclo di vendita standard del modulo SD." },
               { process: "Gestione Partite Cliente", appName: "Manage Customer Line Items", appId: "F0711", tCode: "FBL5N", purpose: "Equivalente Fiori di FBL5N per la gestione delle partite clienti, con avvio di Dispute Cases." },
               { process: "Gestione Incassi e Compensazione", appName: "Clear Incoming Payments", appId: "F1345", tCode: "F-28", purpose: "App dedicata alla registrazione degli incassi e alla contestuale compensazione delle partite aperte." },
               { process: "Gestione Solleciti", appName: "Manage Dunning Notices", appId: "F1557", tCode: "F150", purpose: "Gestisce l'intero processo di sollecito, dalla creazione e schedulazione all'invio." },
               { process: "Gestione Riscossioni", appName: "Process Receivables", appId: "F0106A", tCode: "UDM_SPECIALIST", purpose: "Workspace completo per lo specialista delle riscossioni con partite, promesse e storico contatti." },
          ]},
          { id: "FI-AA", name: "Contabilità Cespiti (FI-AA)", transactions: [
              { process: "Gestione Anagrafica Cespiti", appName: "Manage Fixed Assets", appId: "F1684", tCode: "AS01, AS02, AS03", purpose: "App Fiori centrale per la gestione completa del ciclo di vita dell'anagrafica dei cespiti." },
              { process: "Acquisizione Cespiti (AP)", appName: "Post Acquisition (Integrated AP)", appId: "F-90", tCode: "F-90", purpose: "App SAP GUI per registrare un'acquisizione di cespite integrata con la contabilità fornitori (AP)." },
              { process: "Esecuzione Ammortamenti", appName: "Manage Depreciation Runs", appId: "F1297", tCode: "AFAB", purpose: "App Fiori per pianificare, eseguire e monitorare il ciclo di calcolo e registrazione degli ammortamenti." },
              { process: "Analisi Patrimonio Cespiti", appName: "Asset History Sheet", appId: "F1615", tCode: "S_ALR_87011963", purpose: "Report analitico fondamentale che visualizza tutti i movimenti sui valori dei cespiti." },
              { process: "Monitoraggio Cespiti", appName: "Asset Accounting Overview", appId: "F3096", tCode: "N/A", purpose: "Cockpit analitico per il responsabile della contabilità cespiti con KPI e attività imminenti." },
          ]},
        ],
      },
      {
        id: "CO",
        name: "Controlling (CO)",
        context: "Grazie al Universal Journal, i dati di FI e CO risiedono ora in un'unica tabella (ACDOCA), eliminando la necessità di riconciliazioni e consentendo un'analisi integrata e in tempo reale.",
        submodules: [
          { id: "CO-OM-CCA", name: "Contabilità per Centri di Costo (CO-OM-CCA)", transactions: [
              { process: "Gestione Anagrafica Centri di Costo", appName: "Manage Cost Centers", appId: "F1443A", tCode: "KS01, KS02, KS03", purpose: "Punto di accesso unico per la gestione completa dell'anagrafica dei centri di costo." },
              { process: "Pianificazione Costi", appName: "Plan Cost Centers on Periods", appId: "F1473", tCode: "KP06", purpose: "Per l'inserimento manuale dei valori di piano per combinazioni di centri di costo e nature contabili." },
              { process: "Registrazione Costi Effettivi", appName: "Repost Line Items - Cost Accounting", appId: "F2133", tCode: "KB11N", purpose: "Per la ri-registrazione manuale e la correzione di singole partite di costo tra oggetti di controlling." },
              { process: "Gestione Allocazioni", appName: "Manage Allocations", appId: "F3336", tCode: "KSU1, KSV1", purpose: "App moderna per creare, eseguire e monitorare i cicli di allocazione periodica (distribuzione e ripartizione)." },
              { process: "Analisi Costi Effettivi/Pianificati", appName: "Cost Centers - Plan/Actuals", appId: "W0081", tCode: "S_ALR_87013611", purpose: "App analitica chiave per confrontare i costi effettivi con i valori pianificati e analizzare le varianze." },
          ]},
          { id: "CO-OM-OPA", name: "Ordini Interni (CO-OM-OPA)", transactions: [
              { process: "Gestione Anagrafica Ordini Interni", appName: "Manage Internal Orders", appId: "F1604", tCode: "KO01, KO02, KO03", purpose: "App Fiori centrale per la creazione, modifica e visualizzazione dei dati anagrafici degli ordini interni." },
              { process: "Analisi Utilizzo Ordini", appName: "Where-Used List – Internal Orders", appId: "F2533", tCode: "N/A", purpose: "Strumento di analisi per identificare dove un ordine interno è utilizzato all'interno del sistema." },
              { process: "Gestione Regole di Liquidazione", appName: "Manage Settlement Rules – Internal Orders", appId: "F4005", tCode: "KO02", purpose: "Per definire come e verso quali oggetti i costi raccolti sull'ordine interno devono essere scaricati." },
              { process: "Esecuzione Liquidazione", appName: "Run Settlement - Actual", appId: "F3338", tCode: "KO88, KO8G", purpose: "Per eseguire il processo di liquidazione periodica dei costi effettivi dagli ordini interni." },
              { process: "Analisi Costi Ordini", appName: "Internal Orders - Plan/Actuals", appId: "W0076", tCode: "KOB1", purpose: "App analitica di riferimento per il confronto tra costi pianificati ed effettivi sugli ordini interni." },
          ]},
          { id: "CO-PCA", name: "Contabilità per Centri di Profitto (CO-PCA)", transactions: [
              { process: "Gestione Anagrafica Centri di Profitto", appName: "Manage Profit Centers", appId: "F3516", tCode: "KE51, KE52, KE53", purpose: "App Fiori centrale per la gestione dell'anagrafica dei centri di profitto." },
              { process: "Analisi Redditività", appName: "Profit Centers - Plan/Actual", appId: "F0950", tCode: "S_ALR_87013326", purpose: "App analitica per analizzare ricavi e costi per centro di profitto e confrontare dati effettivi e pianificati." },
              { process: "Analisi Partite per Centro di Profitto", appName: "Display Line Items - Margin Analysis", appId: "F2868", tCode: "KE5Z", purpose: "Fornisce un'analisi dettagliata a livello di singola partita dei documenti che compongono il margine." },
          ]},
        ],
      },
      {
        id: "MM",
        name: "Materials Management (MM)",
        context: "S/4HANA potenzia il modulo MM con analisi in tempo reale, processi di acquisto semplificati e la tabella MATDOC come unica fonte di verità per tutti i movimenti di materiale.",
        submodules: [
          { id: "MM-PUR", name: "Acquisti (MM-PUR)", transactions: [
              { process: "Creazione Richiesta d'Acquisto", appName: "Create Purchase Requisition", appId: "F1643", tCode: "ME51N", purpose: "App moderna per la creazione di RdA, ideale per scenari di self-service procurement." },
              { process: "Elaborazione Richieste d'Acquisto", appName: "Process Purchase Requisitions", appId: "F1048", tCode: "ME56, ME57", purpose: "Worklist per i buyer per visualizzare, assegnare fonti e convertire le RdA in OdA." },
              { process: "Gestione Ordini d'Acquisto", appName: "Manage Purchase Orders", appId: "F0842A", tCode: "ME21N, ME22N, ME23N", purpose: "Successore di ME2xN per la gestione completa (creazione, modifica, monitoraggio) degli OdA." },
              { process: "Gestione Contratti d'Acquisto", appName: "Manage Purchase Contracts", appId: "F1600A", tCode: "ME31K, ME32K, ME33K", purpose: "Per la gestione dei contratti quadro, monitorando rilasci e consumi." },
          ]},
          { id: "MM-IM", name: "Gestione Inventario (MM-IM)", transactions: [
              { process: "Entrata Merci da OdA", appName: "Post Goods Receipt for Purchasing Document", appId: "F0843", tCode: "MIGO (Mov. 101)", purpose: "App ottimizzata per la registrazione dell'entrata merci con riferimento a un OdA." },
              { process: "Uscita Merci", appName: "Post Goods Movement", appId: "F2212", tCode: "MIGO (es. Mov. 201)", purpose: "App flessibile per la registrazione di movimenti come uscite per centro di costo o rottamazione." },
              { process: "Trasferimento Stock", appName: "Transfer Stock - In-Plant / Cross-Plant", appId: "F1061 / F1957", tCode: "MIGO (Mov. 311, 301)", purpose: "App specializzate per i trasferimenti di stock tra magazzini o stabilimenti." },
              { process: "Panoramica Stock", appName: "Manage Stock", appId: "F1062", tCode: "MMBE", purpose: "Visione completa e in tempo reale dello stock di un materiale con capacità analitiche avanzate." },
              { process: "Visualizzazione Documenti Materiale", appName: "Material Documents Overview", appId: "F1077", tCode: "MB03", purpose: "Per la ricerca, visualizzazione e analisi dei documenti di materiale registrati." },
          ]},
          { id: "MM-IV", name: "Verifica Fatture (MM-IV)", transactions: [
              { process: "Registrazione Fattura (Logistica)", appName: "Create Supplier Invoice - Advanced", appId: "MIRO", tCode: "MIRO", purpose: "Replica della transazione MIRO per la registrazione di fatture complesse con riferimento a OdA." },
              { process: "Gestione Fatture Fornitore", appName: "Manage Supplier Invoices", appId: "F0859", tCode: "MIR4, MRBR", purpose: "Worklist centrale per monitorare lo stato, modificare, stornare e rilasciare le fatture." },
              { process: "Sblocco Fatture", appName: "Release Blocked Invoices", appId: "MRBR", tCode: "MRBR", purpose: "Per l'analisi e il rilascio delle fatture bloccate a causa di discrepanze di prezzo o quantità." },
              { process: "Gestione Conto Transitorio GR/IR", appName: "Clear GR/IR Clearing Account", appId: "F1947", tCode: "MR11", purpose: "Per la manutenzione e la compensazione del conto transitorio merci/fatture." },
          ]},
        ],
      },
      {
        id: "PP",
        name: "Production Planning (PP)",
        context: "S/4HANA potenzia PP con MRP Live (calcolo MRP ultra-veloce), l'integrazione di PP/DS nel core, e nuovi paradigmi di pianificazione come pMRP (pianificazione predittiva) e DDMRP (pianificazione basata sulla domanda).",
        submodules: [
          { id: "PP-MRP", name: "Pianificazione dei Fabbisogni di Materiali (PP-MRP)", transactions: [
              { process: "Esecuzione Pianificazione MRP", appName: "Schedule MRP Runs", appId: "F1339", tCode: "MD01N", purpose: "Punto di ingresso per schedulare l'esecuzione di MRP Live, l'algoritmo di pianificazione ottimizzato per HANA." },
              { process: "Monitoraggio Copertura Materiali", appName: "Monitor/Manage Material Coverage", appId: "F0251 / F0247", tCode: "MD04, MD07", purpose: "Evoluzione di MD04, fornisce una visione interattiva e grafica della situazione di copertura di un materiale." },
              { process: "Conversione Ordini Pianificati", appName: "Convert Planned Orders", appId: "F2080", tCode: "CO40, CO41", purpose: "Per la conversione massiva o singola degli ordini pianificati in ordini di produzione o RdA." },
              { process: "Gestione Richieste di Modifica MRP", appName: "Manage Change Requests", appId: "F2132", tCode: "N/A", purpose: "App per la gestione controllata delle modifiche ai fabbisogni per evitare instabilità nella pianificazione." },
          ]},
          { id: "PP-SFC", name: "Gestione Ordini di Produzione (PP-SFC)", transactions: [
              { process: "Creazione Ordine di Produzione", appName: "Create Production Order", appId: "CO01", tCode: "CO01", purpose: "Creazione manuale di ordini di produzione, utile per prototipi o ordini di rilavorazione." },
              { process: "Gestione Ordini di Produzione", appName: "Manage Production Orders", appId: "F2336", tCode: "COOIS", purpose: "Strumento principale per i supervisori per monitorare, identificare problemi e rilasciare gli ordini." },
              { process: "Conferma Operazioni", appName: "Confirm Production Operation", appId: "F3069", tCode: "CO11N", purpose: "App user-friendly, anche per mobile, per registrare tempi, quantità prodotte e scarti." },
              { process: "Conferma Ordine", appName: "Confirm Production Order", appId: "CO15", tCode: "CO15", purpose: "Conferma a livello di testata dell'ordine, usata in scenari semplici o per backflush automatico." },
              { process: "Visualizzazione Dettaglio Ordine", appName: "Production Order Object Page", appId: "F2261", tCode: "CO03", purpose: "Scheda informativa con vista a 360 gradi su un singolo ordine di produzione e tutti i documenti collegati." },
          ]},
        ],
      },
      {
        id: "SD",
        name: "Sales and Distribution (SD)",
        context: "L'innovazione chiave in S/4HANA SD è aATP (Advanced Available-to-Promise), un framework sofisticato per un adempimento degli ordini più intelligente e reattivo, che include Backorder Processing (BOP) e Product Allocations (PAL).",
        submodules: [
          { id: "SD-SLS", name: "Vendite (SD-SLS)", transactions: [
              { process: "Creazione Ordine di Vendita", appName: "Create Sales Orders - VA01", appId: "VA01", tCode: "VA01", purpose: "Transazione classica per la creazione di ordini di vendita in scenari complessi." },
              { process: "Gestione Ordini di Vendita", appName: "Manage Sales Orders - Version 2", appId: "F3893", tCode: "VA02, VA03, VA05", purpose: "App Fiori ottimizzata per la creazione, modifica e visualizzazione rapida degli ordini di vendita." },
              { process: "Monitoraggio Evasione Ordini", appName: "Sales Order Fulfillment - Analyze Issues", appId: "F2242", tCode: "SDV03V02", purpose: "App analitica per identificare e risolvere proattivamente i problemi che bloccano l'evasione degli ordini." },
              { process: "Monitoraggio Performance Vendite", appName: "Sales Management Overview", appId: "F2601", tCode: "N/A", purpose: "Cockpit analitico per i manager delle vendite con KPI su ordini, resi, margini e blocchi." },
          ]},
          { id: "SD-SHP", name: "Spedizione (SD-SHP)", transactions: [
              { process: "Creazione Consegna in Uscita", appName: "Create Outbound Deliveries - From Sales Orders", appId: "F0867L1", tCode: "VL01N, VL10A", purpose: "Per creare una o più consegne in uscita a partire da una lista di ordini di vendita in scadenza." },
              { process: "Gestione Consegne", appName: "Manage Outbound Deliveries", appId: "F0867A", tCode: "VL06O", purpose: "Worklist centrale per monitorare lo stato, avviare il picking e registrare l'uscita merci (PGI)." },
              { process: "Picking", appName: "Pick Outbound Delivery", appId: "F1704", tCode: "VL06P", purpose: "Supporta il processo di prelievo dei materiali dal magazzino per una specifica consegna." },
              { process: "Uscita Merci", appName: "Post Goods Issue for Outbound Delivery", appId: "F2435", tCode: "VL02N", purpose: "Registra l'uscita merci (PGI), riducendo lo stock e registrando il costo del venduto." },
          ]},
          { id: "SD-BIL", name: "Fatturazione (SD-BIL)", transactions: [
              { process: "Creazione Fatture", appName: "Create Billing Documents", appId: "F0798", tCode: "VF01", purpose: "Per creare documenti di fatturazione a partire da una lista di documenti pronti per la fatturazione." },
              { process: "Gestione Lista Fatturazione", appName: "Schedule Billing Document Creation", appId: "F1857", tCode: "VF04, VF06", purpose: "Per pianificare e schedulare in background la creazione massiva di fatture." },
              { process: "Gestione Documenti di Fatturazione", appName: "Manage Billing Documents", appId: "F0797", tCode: "VF02, VF03", purpose: "Worklist centrale per l'addetto alla fatturazione per visualizzare, modificare, stornare e analizzare le fatture." },
              { process: "Visualizzazione Dettaglio Fattura", appName: "Billing Document (Object Page)", appId: "F1901", tCode: "VF03", purpose: "Scheda informativa con vista a 360 gradi su un singolo documento di fatturazione." },
          ]},
        ],
      },
    ],
  },
  // Nuovi dati per le transazioni SAP GUI
  "gui": {
    "introduction": {
      "title": "Guida alle Transazioni SAP GUI in S/4HANA",
      "content": "Questa sezione analizza le transazioni classiche della SAP GUI, evidenziando il loro stato (valido, obsoleto), il loro successore strategico e l'impatto dei cambiamenti architetturali di S/4HANA come il Giornale Unico e il Business Partner. La comprensione di questi pilastri è il prerequisito per un utilizzo efficace del sistema."
    },
    "architecturalChanges": {
      "title": "Cambiamenti Architettonici Fondamentali",
      "points": [
        {
          "title": "Il Giornale Unico (Universal Journal - Tabella ACDOCA)",
          "content": "È la nuova, unica fonte di verità per tutte le registrazioni contabili. Unifica i dati di FI, CO, FI-AA e ML, eliminando la necessità di riconciliazioni periodiche e garantendo una coerenza dei dati by design."
        },
        {
          "title": "Il Modello Business Partner (BP)",
          "content": "Sostituisce obbligatoriamente i modelli di dati anagrafici separati per clienti e fornitori. Tutte le informazioni sono centralizzate in un unico oggetto anagrafico gestito tramite la transazione BP."
        },
        {
          "title": "Il Material Ledger (ML)",
          "content": "Non più opzionale, il Material Ledger è la soluzione obbligatoria per la valorizzazione dell'inventario in S/4HANA, fornendo nativamente funzionalità multi-valuta e multi-principio contabile."
        }
      ]
    },
    "modules": [
      {
        "id": "FI",
        "name": "Financial Accounting (FI)",
        "introduction": "Il modulo di Financial Accounting (FI) in S/4HANA ha subito una delle trasformazioni più profonde. Sebbene molte transazioni mantengano un nome familiare, il loro funzionamento interno e le loro dipendenze sono radicalmente cambiati, soprattutto a causa dell'unificazione di conti Co.Ge. ed elementi di costo in FS00 e l'obbligatorietà del Business Partner (BP) per la gestione di clienti e fornitori.",
        "transactions": [
          {
            "tCode": "FS00",
            "description": "Gestione Anagrafica Conti Co.Ge.",
            "status": "Valido (Potenziato)",
            "notes": "Unico punto di accesso per conti Co.Ge. ed elementi di costo."
          },
          {
            "tCode": "FBL1N",
            "description": "Visualizzazione Partite Individuali Fornitori",
            "status": "Valido",
            "notes": "Lettura diretta da ACDOCA per performance e coerenza in tempo reale."
          },
          {
            "tCode": "FBL3N",
            "description": "Elenco partite conti generali",
            "status": "Valido",
            "notes": "Transazione standard per la consultazione partite Co.Ge."
          },
          {
            "tCode": "FBL5N",
            "description": "Visualizzazione Partite Individuali Clienti",
            "status": "Valido",
            "notes": "Lettura diretta da ACDOCA per performance e coerenza in tempo reale."
          },
          {
            "tCode": "F-02",
            "description": "Registrazione Documento Co.Ge.",
            "status": "Valido",
            "notes": "Transazioni standard per registrazioni manuali nel General Ledger."
          },
          {
            "tCode": "FB01",
            "description": "Registrazione documento contabile manuale",
            "status": "Valido",
            "notes": "Transazione principale per registrazioni manuali."
          },
          {
            "tCode": "FB02",
            "description": "Modifica documento contabile",
            "status": "Valido",
            "notes": "Modifica documenti contabili esistenti."
          },
          {
            "tCode": "FB03",
            "description": "Visualizzazione documento contabile",
            "status": "Valido",
            "notes": "Consultazione documenti contabili."
          },
          {
            "tCode": "FB50",
            "description": "Registrazione Documento Co.Ge.",
            "status": "Valido",
            "notes": "Transazioni standard per registrazioni manuali nel General Ledger."
          },
          {
            "tCode": "F110",
            "description": "Programma di Pagamento Automatico",
            "status": "Valido",
            "notes": "Rimane il pilastro per l'elaborazione automatizzata dei pagamenti e incassi."
          },
          {
            "tCode": "AFAB",
            "description": "Esecuzione Ammortamenti",
            "status": "Valido",
            "notes": "Ora calcola e registra direttamente i documenti di ammortamento in ACDOCA."
          },
          {
            "tCode": "FIBF",
            "description": "Gestione user-exit per FI",
            "status": "Valido",
            "notes": "Gestione personalizzazioni e user-exit per modulo FI."
          },
          {
            "tCode": "GGB1",
            "description": "Definizione regole di validazione/sostituzione",
            "status": "Valido",
            "notes": "Configurazione regole di business per validazioni."
          },
          {
            "tCode": "OB28",
            "description": "Attivazione validazioni/sostituzioni FI",
            "status": "Valido",
            "notes": "Attivazione delle regole di validazione e sostituzione."
          }
        ]
      },
      {
        "id": "CO",
        "name": "Controlling (CO)",
        "introduction": "La fusione con FI nel Giornale Unico ha eliminato le riconciliazioni. Ogni transazione CO ora genera una scrittura contabile in tempo reale in ACDOCA, rendendo obsoleto il Reconciliation Ledger (KALC).",
        "transactions": [
          {
            "tCode": "KS01",
            "description": "Creare Centro di Costo",
            "status": "Valido",
            "notes": "Rimangono le transazioni centrali per la gestione dei rispettivi oggetti."
          },
          {
            "tCode": "KO01",
            "description": "Creare Ordine Interno",
            "status": "Valido",
            "notes": "Rimangono le transazioni centrali per la gestione dei rispettivi oggetti."
          },
          {
            "tCode": "KSB1",
            "description": "Centri di Costo: Partite Individuali Attuali",
            "status": "Valido",
            "notes": "Reportistica operativa fondamentale, ora basata su ACDOCA."
          },
          {
            "tCode": "KOB1",
            "description": "Ordini: Partite Individuali Attuali",
            "status": "Valido",
            "notes": "Reportistica operativa fondamentale, ora basata su ACDOCA."
          },
          {
            "tCode": "KSU5",
            "description": "Eseguire Ribaltamento",
            "status": "Valido",
            "notes": "Transazioni standard per le allocazioni periodiche dei costi."
          },
          {
            "tCode": "KSV5",
            "description": "Eseguire Distribuzione",
            "status": "Valido",
            "notes": "Transazioni standard per le allocazioni periodiche dei costi."
          },
          {
            "tCode": "CK11N",
            "description": "Creare Calcolo Costi Materiale",
            "status": "Valido",
            "notes": "Strumenti primari per la pianificazione dei costi di prodotto (costo standard)."
          },
          {
            "tCode": "CK40N",
            "description": "Ciclo di Calcolo Costi",
            "status": "Valido",
            "notes": "Strumenti primari per la pianificazione dei costi di prodotto (costo standard)."
          },
          {
            "tCode": "CKMLCP",
            "description": "Cockpit di Chiusura del Material Ledger",
            "status": "Valido (Obbligatorio)",
            "notes": "Transazione mensile cruciale per il calcolo del costo effettivo."
          },
          {
            "tCode": "KE21N",
            "description": "Creare Partita Individuale CO-PA",
            "status": "Valido (Legacy)",
            "notes": "Rilevante solo per Costing-Based CO-PA; la soluzione strategica è Margin Analysis."
          }
        ]
      },
      {
        "id": "MM",
        "name": "Materials Management (MM)",
        "introduction": "La semplificazione più drastica è nella gestione delle scorte, con la transazione MIGO che diventa l'unico punto di accesso per quasi tutti i movimenti e la tabella MATDOC come unica fonte di verità, eliminando le tabelle di aggregazione e garantendo la coerenza con FI.",
        "transactions": [
          {
            "tCode": "ME21N",
            "description": "Creare Ordine d'Acquisto",
            "status": "Valido",
            "notes": "Transazione centrale e invariata per la creazione di OdA."
          },
          {
            "tCode": "MM01",
            "description": "Creare Materiale",
            "status": "Valido",
            "notes": "Standard indiscusso per la gestione dell'anagrafica materiali."
          },
          {
            "tCode": "MM03",
            "description": "Visualizzazione anagrafica materiale",
            "status": "Valido",
            "notes": "Consultazione dati anagrafici materiali."
          },
          {
            "tCode": "MIGO",
            "description": "Entrata merci / Movimenti magazzino",
            "status": "Valido",
            "notes": "Tutte le funzionalità di movimentazione sono state consolidate in questa transazione."
          },
          {
            "tCode": "MIRO",
            "description": "Registrare Fattura in Entrata",
            "status": "Valido",
            "notes": "Rimane la transazione chiave per il controllo fatture logistico."
          },
          {
            "tCode": "MMBE",
            "description": "Sintesi Stock",
            "status": "Valido",
            "notes": "Ancora utilizzata per una rapida panoramica dello stock."
          },
          {
            "tCode": "ME51N",
            "description": "Creare Richiesta d'Acquisto",
            "status": "Valido",
            "notes": "Transazione standard per la creazione di RdA."
          },
          {
            "tCode": "MI01",
            "description": "Creare Doc. Inventario",
            "status": "Valido",
            "notes": "Il processo di inventario fisico rimane transazionalmente stabile."
          },
          {
            "tCode": "MI04",
            "description": "Registrare Conta",
            "status": "Valido",
            "notes": "Il processo di inventario fisico rimane transazionalmente stabile."
          },
          {
            "tCode": "MI07",
            "description": "Registrare Differenze",
            "status": "Valido",
            "notes": "Il processo di inventario fisico rimane transazionalmente stabile."
          },
          {
            "tCode": "MR8M",
            "description": "Storno documento di fattura (annullamento MIRO)",
            "status": "Valido",
            "notes": "Storno documenti di fattura acquisti."
          },
          {
            "tCode": "MIR4",
            "description": "Visualizzazione documento di fattura (MIRO)",
            "status": "Valido",
            "notes": "Consultazione documenti di fattura acquisti."
          },
          {
            "tCode": "MIR8",
            "description": "Modifica documento di fattura (MIRO)",
            "status": "Valido",
            "notes": "Modifica documenti di fattura acquisti."
          },
          {
            "tCode": "CS11",
            "description": "Visualizzazione distinta base livello singolo",
            "status": "Valido",
            "notes": "Consultazione distinte base."
          },
          {
            "tCode": "CS12",
            "description": "Visualizzazione distinta base livello multiplo",
            "status": "Valido",
            "notes": "Consultazione distinte base multi-livello."
          },
          {
            "tCode": "CS13",
            "description": "Visualizzazione distinta base con struttura applicazione",
            "status": "Valido",
            "notes": "Consultazione avanzata distinte base."
          },
          {
            "tCode": "MSC1N",
            "description": "Creazione dati serial number",
            "status": "Valido",
            "notes": "Gestione numeri seriali."
          },
          {
            "tCode": "MSC2N",
            "description": "Modifica dati serial number",
            "status": "Valido",
            "notes": "Modifica numeri seriali."
          },
          {
            "tCode": "MSC3N",
            "description": "Visualizzazione dati serial number",
            "status": "Valido",
            "notes": "Consultazione numeri seriali."
          }
        ]
      },
      {
        "id": "SD",
        "name": "Sales and Distribution (SD)",
        "introduction": "Il flusso Order-to-Cash (VA01, VL01N, VF01) rimane stabile, ma cambiamenti importanti riguardano i dati anagrafici (BP), la gestione del credito (FIN-FSCM) e i ristorni, completamente sostituiti dalla Gestione Contratti Condizione (WCOCO).",
        "transactions": [
          {
            "tCode": "VA01",
            "description": "Creare Ordine di Vendita",
            "status": "Valido",
            "notes": "La transazione principale per la creazione di ordini cliente rimane invariata."
          },
          {
            "tCode": "VA02",
            "description": "Modifica ordine di vendita",
            "status": "Valido",
            "notes": "Modifica ordini cliente esistenti."
          },
          {
            "tCode": "VA03",
            "description": "Visualizzazione ordine di vendita",
            "status": "Valido",
            "notes": "Consultazione ordini cliente."
          },
          {
            "tCode": "VL01N",
            "description": "Creare Consegna in Uscita",
            "status": "Valido",
            "notes": "Transazione primaria per avviare il processo di spedizione."
          },
          {
            "tCode": "VL02N",
            "description": "Modifica consegna ordine cliente",
            "status": "Valido",
            "notes": "Modifica documenti di consegna."
          },
          {
            "tCode": "VF01",
            "description": "Creare Documento di Fatturazione",
            "status": "Valido",
            "notes": "Strumento standard per la creazione delle fatture clienti."
          },
          {
            "tCode": "VF03",
            "description": "Visualizzazione fattura",
            "status": "Valido",
            "notes": "Consultazione fatture clienti."
          },
          {
            "tCode": "VF11",
            "description": "Storno fattura",
            "status": "Valido",
            "notes": "Storno documenti di fattura clienti."
          },
          {
            "tCode": "VK11",
            "description": "Creare Record di Condizione",
            "status": "Valido",
            "notes": "Ancora centrale per la manutenzione dei prezzi (ma scrive su PRCD_ELEMENTS)."
          },
          {
            "tCode": "VA05",
            "description": "Lista Ordini di Vendita",
            "status": "Valido",
            "notes": "Report operativo standard per la visualizzazione degli ordini."
          },
          {
            "tCode": "VOFM",
            "description": "Gestione routine di determinazione (prezzi, output, ecc.)",
            "status": "Valido",
            "notes": "Configurazione routine di determinazione."
          },
          {
            "tCode": "NACE",
            "description": "Determinazione Output",
            "status": "Valido (Legacy)",
            "notes": "La soluzione strategica è il nuovo Output Management basato su BRF+."
          }
        ]
      },
      {
        "id": "PP",
        "name": "Production Planning (PP)",
        "introduction": "Il cambiamento principale è l'introduzione di MRP Live (MD01N) come strumento strategico per la pianificazione, molto più performante dell'MRP classico. Le transazioni operative (CS01, CR01, CO01) rimangono stabili.",
        "transactions": [
          {
            "tCode": "CS01",
            "description": "Creare Distinta Base (BOM)",
            "status": "Valido",
            "notes": "Fondamentale e invariato per definire la struttura del prodotto."
          },
          {
            "tCode": "CR01",
            "description": "Creare Centro di Lavoro",
            "status": "Valido",
            "notes": "Transazione standard per definire le risorse produttive."
          },
          {
            "tCode": "CA01",
            "description": "Creare Ciclo di Lavoro Standard",
            "status": "Valido",
            "notes": "Utilizzata per definire la sequenza delle operazioni di produzione."
          },
          {
            "tCode": "MD01N",
            "description": "MRP Live",
            "status": "Valido (Strategico)",
            "notes": "Successore ottimizzato per HANA delle transazioni MRP classiche (MD01/MD02)."
          },
          {
            "tCode": "MD04",
            "description": "Lista Stock/Fabbisogni",
            "status": "Valido",
            "notes": "Lo strumento di lavoro più importante per i pianificatori per analizzare i risultati."
          },
          {
            "tCode": "CO01",
            "description": "Creare Ordine di Produzione",
            "status": "Valido",
            "notes": "Le transazioni per la gestione degli ordini di produzione rimangono invariate."
          },
          {
            "tCode": "CO02",
            "description": "Modifica ordine produzione",
            "status": "Valido",
            "notes": "Modifica ordini di produzione esistenti."
          },
          {
            "tCode": "CO03",
            "description": "Visualizzazione ordine produzione",
            "status": "Valido",
            "notes": "Consultazione ordini di produzione."
          },
          {
            "tCode": "CO11N",
            "description": "Conferma Ordine Produzione (Operazione)",
            "status": "Valido",
            "notes": "Transazioni standard per la registrazione dell'avanzamento produttivo."
          },
          {
            "tCode": "CO15",
            "description": "Conferma Ordine Produzione (Testata)",
            "status": "Valido",
            "notes": "Transazioni standard per la registrazione dell'avanzamento produttivo."
          }
        ]
      },
      {
        "id": "QM",
        "name": "Quality Management (QM)",
        "introduction": "Il modulo Quality Management mantiene la sua struttura transazionale classica, con focus sui lotti di ispezione e le notifiche qualità.",
        "transactions": [
          {
            "tCode": "QA01",
            "description": "Creazione lotto di ispezione manuale",
            "status": "Valido",
            "notes": "Creazione manuale lotti di ispezione."
          },
          {
            "tCode": "QA02",
            "description": "Modifica lotto di ispezione",
            "status": "Valido",
            "notes": "Modifica lotti di ispezione esistenti."
          },
          {
            "tCode": "QA03",
            "description": "Visualizzazione lotto di ispezione",
            "status": "Valido",
            "notes": "Consultazione lotti di ispezione."
          },
          {
            "tCode": "QA31",
            "description": "Registrazione risultati ispezione",
            "status": "Valido",
            "notes": "Inserimento risultati delle ispezioni."
          },
          {
            "tCode": "QA32",
            "description": "Modifica risultati ispezione",
            "status": "Valido",
            "notes": "Modifica risultati ispezioni esistenti."
          },
          {
            "tCode": "QA33",
            "description": "Visualizzazione risultati ispezione",
            "status": "Valido",
            "notes": "Consultazione risultati ispezioni."
          },
          {
            "tCode": "QM01",
            "description": "Creazione notifica qualità",
            "status": "Valido",
            "notes": "Creazione notifiche per problemi qualità."
          },
          {
            "tCode": "QM02",
            "description": "Modifica notifica qualità",
            "status": "Valido",
            "notes": "Modifica notifiche qualità esistenti."
          },
          {
            "tCode": "QM03",
            "description": "Visualizzazione notifica qualità",
            "status": "Valido",
            "notes": "Consultazione notifiche qualità."
          }
        ]
      },
      {
        "id": "FE",
        "name": "Fatturazione Elettronica (FE)",
        "introduction": "Modulo dedicato alla gestione dei documenti elettronici e alla conformità normativa per la fatturazione elettronica.",
        "transactions": [
          {
            "tCode": "EDOC_COCKPIT",
            "description": "Monitoraggio e gestione centralizzata dei documenti elettronici",
            "status": "Valido",
            "notes": "Cockpit principale per la gestione della fatturazione elettronica."
          },
          {
            "tCode": "EDOC_RESUBMIT",
            "description": "Ritrasmissione documenti elettronici con errori",
            "status": "Valido",
            "notes": "Reinvio documenti elettronici in errore."
          }
        ]
      },
      {
            "id": "ABAP",
            "name": "Transazioni ABAP",
            "introduction": "Transazioni SAP GUI essenziali per lo sviluppo e la manutenzione di programmi ABAP.",
            "transactions": [
              {
                "tCode": "SE38",
                "description": "Editor ABAP (Programmi, Report)",
                "status": "Valido",
                "notes": "Ambiente principale per scrivere e gestire il codice ABAP."
              },
              {
                "tCode": "SE80",
                "description": "Object Navigator (Programmi, Classi, Funzioni, Dynpro)",
                "status": "Valido",
                "notes": "Strumento integrato per la gestione di tutti gli oggetti di sviluppo ABAP."
              },
              {
                "tCode": "SE24",
                "description": "Class Builder (Classi e Interfacce ABAP)",
                "status": "Valido",
                "notes": "Per la creazione e gestione di classi e interfacce ABAP Objects."
              },
              {
                "tCode": "SE37",
                "description": "Function Builder (Moduli Funzione)",
                "status": "Valido",
                "notes": "Per la creazione e gestione di moduli funzione e gruppi di funzioni."
              },
              {
                "tCode": "SE11",
                "description": "ABAP Dictionary (Tabelle, Viste, Tipi)",
                "status": "Valido",
                "notes": "Per la gestione degli oggetti del dizionario dati ABAP."
              },
              {
                "tCode": "SE09",
                "description": "Transport Organizer (Richieste di Trasporto)",
                "status": "Valido",
                "notes": "Per la gestione delle richieste di trasporto (Workbench e Customizing)."
              },
              {
                "tCode": "SE16N",
                "description": "Data Browser (Visualizzazione Dati Tabella)",
                "status": "Valido",
                "notes": "Per visualizzare il contenuto di qualsiasi tabella SAP."
              },
              {
                "tCode": "SM30",
                "description": "Manutenzione Viste (Customizing)",
                "status": "Valido",
                "notes": "Per la manutenzione dei dati di customizing tramite viste."
              },
              {
                "tCode": "ST22",
                "description": "ABAP Dump Analysis (Analisi Errori Runtime)",
                "status": "Valido",
                "notes": "Per analizzare i dump (errori di runtime) dei programmi ABAP."
              },
              {
                "tCode": "SM13",
                "description": "Update Records (Monitoraggio Aggiornamenti)",
                "status": "Valido",
                "notes": "Per monitorare e gestire i processi di aggiornamento del database."
              },
              {
                "tCode": "SPRO",
                "description": "Customizing (Guida Implementazione)",
                "status": "Valido",
                "notes": "Punto di accesso principale per la configurazione del sistema SAP."
              },
              {
                "tCode": "SMARTFORMS",
                "description": "Smart Forms Builder",
                "status": "Valido",
                "notes": "Per la creazione e gestione di Smart Forms."
              },
              {
                "tCode": "SFP",
                "description": "Adobe Forms Workbench",
                "status": "Valido",
                "notes": "Per la creazione e gestione di Adobe Forms."
              },
              {
                "tCode": "SWO1",
                "description": "Business Object Builder",
                "status": "Valido",
                "notes": "Per la gestione dei Business Object (Workflow)."
              },
              {
                "tCode": "SWDD",
                "description": "Workflow Builder",
                "status": "Valido",
                "notes": "Per la creazione e gestione dei workflow SAP."
              },
              {
                "tCode": "SICF",
                "description": "Maintain Services (HTTP/HTTPS)",
                "status": "Valido",
                "notes": "Per attivare e configurare i servizi web (HTTP/HTTPS) in SAP."
              },
              {
                "tCode": "SRAL",
                "description": "Service Registry",
                "status": "Valido",
                "notes": "Per registrare e gestire i servizi OData e altri servizi."
              },
              {
                "tCode": "LSMW",
                "description": "Legacy System Migration Workbench",
                "status": "Valido",
                "notes": "Strumento per la migrazione dati da sistemi legacy."
              },
              {
                "tCode": "SQVI",
                "description": "QuickViewer (Query Semplici)",
                "status": "Valido",
                "notes": "Per creare query rapide su tabelle e viste."
              },
              {
                "tCode": "ABAPHELP",
                "description": "ABAP Keyword Documentation",
                "status": "Valido",
                "notes": "Documentazione della sintassi ABAP."
              }
            ]
          }
        ]
      },
  abap: {
    introduction: {
      title: "Guida alle Transazioni ABAP",
      content: "Questa sezione elenca le transazioni ABAP più comuni e utili per lo sviluppo e la manutenzione nel sistema SAP."
    },
    modules: [
      {
        id: "DEV",
        name: "Sviluppo ABAP",
        introduction: "Transazioni essenziali per gli sviluppatori ABAP.",
        transactions: [
          { tCode: "SE38", description: "Editor ABAP", status: "Valido", notes: "Per la scrittura e l'esecuzione di programmi ABAP." },
          { tCode: "SE80", description: "Object Navigator", status: "Valido", notes: "Ambiente di sviluppo integrato per oggetti ABAP." },
          { tCode: "SE24", description: "Class Builder", status: "Valido", notes: "Per la creazione e gestione di classi e interfacce ABAP." },
          { tCode: "SE37", description: "Function Builder", status: "Valido", notes: "Per la creazione e gestione di moduli funzione." },
          { tCode: "SE11", description: "ABAP Dictionary", status: "Valido", notes: "Per la gestione di tabelle, viste, tipi di dati." },
          { tCode: "SE09", description: "Transport Organizer", status: "Valido", notes: "Per la gestione delle richieste di trasporto." },
          { tCode: "SE16N", description: "Data Browser", status: "Valido", notes: "Per la visualizzazione del contenuto delle tabelle." },
          { tCode: "SM30", description: "Manutenzione Viste", status: "Valido", notes: "Per la manutenzione dei dati di customizing." },
          { tCode: "ST22", description: "ABAP Dump Analysis", status: "Valido", notes: "Per l'analisi degli errori di runtime ABAP." },
          { tCode: "SM13", description: "Update Records", status: "Valido", notes: "Per il monitoraggio dei processi di aggiornamento." },
          { tCode: "SPRO", description: "Customizing (IMG)", status: "Valido", notes: "Punto di accesso per la configurazione del sistema." },
          { tCode: "SMARTFORMS", description: "Smart Forms Builder", status: "Valido", notes: "Per la creazione e gestione di Smart Forms." },
          { tCode: "SFP", description: "Adobe Forms Workbench", status: "Valido", notes: "Per la creazione e gestione di Adobe Forms." },
          { tCode: "SWO1", description: "Business Object Builder", status: "Valido", notes: "Per la gestione dei Business Object (Workflow)." },
          { tCode: "SWDD", description: "Workflow Builder", status: "Valido", notes: "Per la creazione e gestione dei workflow SAP." },
          { tCode: "SICF", description: "Maintain Services (HTTP/HTTPS)", status: "Valido", notes: "Per attivare e configurare i servizi web in SAP." },
          { tCode: "SRAL", description: "Service Registry", status: "Valido", notes: "Per registrare e gestire i servizi OData." },
          { tCode: "LSMW", description: "Legacy System Migration Workbench", status: "Valido", notes: "Strumento per la migrazione dati da sistemi legacy." },
          { tCode: "SQVI", description: "QuickViewer", status: "Valido", notes: "Per creare query rapide su tabelle e viste." },
          { tCode: "ABAPHELP", description: "ABAP Keyword Documentation", status: "Valido", notes: "Documentazione della sintassi ABAP." }
        ]
      }
    ]
  }
};
