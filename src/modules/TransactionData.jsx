export const transactionData = {
    introduction: {
      title: "Guida di Riferimento alle Transazioni e App Core di SAP S/4HANA",
      content: "Questo report fornisce una guida di riferimento esaustiva alle funzionalità operative centrali all'interno dei moduli SAP S/4HANA Financials (FI), Controlling (CO), Materials Management (MM), Production Planning (PP) e Sales and Distribution (SD). L'analisi si fonda rigorosamente sulla documentazione ufficiale SAP. In S/4HANA, il concetto di 'transazione' si è evoluto: l'interfaccia utente primaria è SAP Fiori, ma le transazioni classiche del SAP GUI sono ancora accessibili, garantendo continuità e una transizione graduale."
    },
    modules: [
      {
        id: "FI",
        name: "Financial Accounting (FI)",
        context: "Il modulo FI in S/4HANA è stato trasformato dall'introduzione dell'Universal Journal (tabella ACDOCA), che unifica i dati di FI e CO, eliminando le riconciliazioni e abilitando analisi in tempo reale.",
        submodules: [
          {
            id: "FI-GL",
            name: "Contabilità Generale (FI-GL)",
            transactions: [
              { process: "Gestione Dati Anagrafici C/G", appName: "Manage G/L Account Master Data", appId: "F0731A", tCode: "FS00, FSP0, FSS0", purpose: "Centralizza la creazione, modifica e visualizzazione dei dati anagrafici dei conti Co.Ge." },
              { process: "Registrazione Documento C/G", appName: "Post General Journal Entries", appId: "F0718", tCode: "FB50, F-02", purpose: "Interfaccia moderna per registrazioni contabili generali, alternativa Fiori a FB50." },
              { process: "Visualizzazione Partite C/G", appName: "Display Line Items in General Ledger", appId: "F2217", tCode: "FBL3N, FAGLL03", purpose: "Potente successore di FBL3N per l'analisi dettagliata delle partite Co.Ge. in tempo reale da ACDOCA." },
              { process: "Visualizzazione Saldi C/G", appName: "Display G/L Account Balances", appId: "F0702", tCode: "FS10N, FAGLB03", purpose: "Equivalente Fiori per la visualizzazione dei saldi contabili aggregati per periodo." },
              { process: "Monitoraggio Contabilità Generale", appName: "General Ledger Overview", appId: "F2445", tCode: "N/A", purpose: "Cockpit analitico per monitorare i KPI chiave della contabilità generale con navigazione drill-down." },
            ],
          },
          {
            id: "FI-AP",
            name: "Contabilità Fornitori (FI-AP)",
            transactions: [
              { process: "Registrazione Fattura Fornitore", appName: "Create Incoming Invoices", appId: "F0859", tCode: "FB60", purpose: "App principale per la registrazione di fatture fornitore non collegate a un OdA." },
              { process: "Gestione Partite Fornitore", appName: "Manage Supplier Line Items", appId: "F0712", tCode: "FBL1N", purpose: "Controparte Fiori di FBL1N per visualizzare, filtrare e gestire le partite dei fornitori." },
              { process: "Esecuzione Pagamenti", appName: "Manage Automatic Payments", appId: "F0770", tCode: "F110", purpose: "Modernizza la gestione del programma di pagamento automatico (F110) in un'unica interfaccia." },
              { process: "Monitoraggio Contabilità Fornitori", appName: "Accounts Payable Overview", appId: "F2917", tCode: "N/A", purpose: "Cockpit analitico per il monitoraggio in tempo reale dei KPI come DPO e fatture bloccate." },
              { process: "Visualizzazione Flusso Documenti", appName: "Display Process Flow - Accounts Payable", appId: "F2691", tCode: "N/A", purpose: "Visualizzazione grafica dell'intera catena di documenti, dall'OdA al pagamento." },
            ],
          },
          {
            id: "FI-AR",
            name: "Contabilità Clienti (FI-AR)",
            transactions: [
              { process: "Registrazione Fattura Cliente", appName: "Create Outgoing Invoices", appId: "F0798", tCode: "FB70", purpose: "Per la registrazione di fatture clienti che non originano dal ciclo di vendita standard del modulo SD." },
              { process: "Gestione Partite Cliente", appName: "Manage Customer Line Items", appId: "F0711", tCode: "FBL5N", purpose: "Equivalente Fiori di FBL5N per la gestione delle partite clienti, con avvio di Dispute Cases." },
              { process: "Gestione Incassi e Compensazione", appName: "Clear Incoming Payments", appId: "F1345", tCode: "F-28", purpose: "App dedicata alla registrazione degli incassi e alla contestuale compensazione delle partite aperte." },
              { process: "Gestione Solleciti", appName: "Manage Dunning Notices", appId: "F1557", tCode: "F150", purpose: "Gestisce l'intero processo di sollecito, dalla creazione e schedulazione all'invio." },
              { process: "Gestione Riscossioni", appName: "Process Receivables", appId: "F0106A", tCode: "UDM_SPECIALIST", purpose: "Workspace completo per lo specialista delle riscossioni con partite, promesse e storico contatti." },
            ],
          },
          {
              id: "FI-AA",
              name: "Contabilità Cespiti (FI-AA)",
              transactions: [
                { process: "Gestione Anagrafica Cespiti", appName: "Manage Fixed Assets", appId: "F1684", tCode: "AS01, AS02, AS03", purpose: "App Fiori centrale per la gestione completa del ciclo di vita dell'anagrafica dei cespiti." },
                { process: "Acquisizione Cespiti (AP)", appName: "Post Acquisition (Integrated AP)", appId: "F-90", tCode: "F-90", purpose: "App SAP GUI per registrare un'acquisizione di cespite integrata con la contabilità fornitori (AP)." },
                { process: "Esecuzione Ammortamenti", appName: "Manage Depreciation Runs", appId: "F1297", tCode: "AFAB", purpose: "App Fiori per pianificare, eseguire e monitorare il ciclo di calcolo e registrazione degli ammortamenti." },
                { process: "Analisi Patrimonio Cespiti", appName: "Asset History Sheet", appId: "F1615", tCode: "S_ALR_87011963", purpose: "Report analitico fondamentale che visualizza tutti i movimenti sui valori dei cespiti." },
                { process: "Monitoraggio Cespiti", appName: "Asset Accounting Overview", appId: "F3096", tCode: "N/A", purpose: "Cockpit analitico per il responsabile della contabilità cespiti con KPI e attività imminenti." },
              ],
            },
        ],
      },
      {
        id: "CO",
        name: "Controlling (CO)",
        context: "Grazie al Universal Journal, i dati di FI e CO risiedono ora in un'unica tabella (ACDOCA), eliminando la necessità di riconciliazioni e consentendo un'analisi integrata e in tempo reale.",
        submodules: [
          {
            id: "CO-OM-CCA",
            name: "Contabilità per Centri di Costo (CO-OM-CCA)",
            transactions: [
              { process: "Gestione Anagrafica Centri di Costo", appName: "Manage Cost Centers", appId: "F1443A", tCode: "KS01, KS02, KS03", purpose: "Punto di accesso unico per la gestione completa dell'anagrafica dei centri di costo." },
              { process: "Pianificazione Costi", appName: "Plan Cost Centers on Periods", appId: "F1473", tCode: "KP06", purpose: "Per l'inserimento manuale dei valori di piano per combinazioni di centri di costo e nature contabili." },
              { process: "Registrazione Costi Effettivi", appName: "Repost Line Items - Cost Accounting", appId: "F2133", tCode: "KB11N", purpose: "Per la ri-registrazione manuale e la correzione di singole partite di costo tra oggetti di controlling." },
              { process: "Gestione Allocazioni", appName: "Manage Allocations", appId: "F3336", tCode: "KSU1, KSV1", purpose: "App moderna per creare, eseguire e monitorare i cicli di allocazione periodica (distribuzione e ripartizione)." },
              { process: "Analisi Costi Effettivi/Pianificati", appName: "Cost Centers - Plan/Actuals", appId: "W0081", tCode: "S_ALR_87013611", purpose: "App analitica chiave per confrontare i costi effettivi con i valori pianificati e analizzare le varianze." },
            ],
          },
          {
            id: "CO-OM-OPA",
            name: "Ordini Interni (CO-OM-OPA)",
            transactions: [
              { process: "Gestione Anagrafica Ordini Interni", appName: "Manage Internal Orders", appId: "F1604", tCode: "KO01, KO02, KO03", purpose: "App Fiori centrale per la creazione, modifica e visualizzazione dei dati anagrafici degli ordini interni." },
              { process: "Analisi Utilizzo Ordini", appName: "Where-Used List – Internal Orders", appId: "F2533", tCode: "N/A", purpose: "Strumento di analisi per identificare dove un ordine interno è utilizzato all'interno del sistema." },
              { process: "Gestione Regole di Liquidazione", appName: "Manage Settlement Rules – Internal Orders", appId: "F4005", tCode: "KO02", purpose: "Per definire come e verso quali oggetti i costi raccolti sull'ordine interno devono essere scaricati." },
              { process: "Esecuzione Liquidazione", appName: "Run Settlement - Actual", appId: "F3338", tCode: "KO88, KO8G", purpose: "Per eseguire il processo di liquidazione periodica dei costi effettivi dagli ordini interni." },
              { process: "Analisi Costi Ordini", appName: "Internal Orders - Plan/Actuals", appId: "W0076", tCode: "KOB1", purpose: "App analitica di riferimento per il confronto tra costi pianificati ed effettivi sugli ordini interni." },
            ],
          },
          {
              id: "CO-PCA",
              name: "Contabilità per Centri di Profitto (CO-PCA)",
              transactions: [
                { process: "Gestione Anagrafica Centri di Profitto", appName: "Manage Profit Centers", appId: "F3516", tCode: "KE51, KE52, KE53", purpose: "App Fiori centrale per la gestione dell'anagrafica dei centri di profitto." },
                { process: "Analisi Redditività", appName: "Profit Centers - Plan/Actual", appId: "F0950", tCode: "S_ALR_87013326", purpose: "App analitica per analizzare ricavi e costi per centro di profitto e confrontare dati effettivi e pianificati." },
                { process: "Analisi Partite per Centro di Profitto", appName: "Display Line Items - Margin Analysis", appId: "F2868", tCode: "KE5Z", purpose: "Fornisce un'analisi dettagliata a livello di singola partita dei documenti che compongono il margine." },
              ],
            },
        ],
      },
      {
        id: "MM",
        name: "Materials Management (MM)",
        context: "S/4HANA potenzia il modulo MM con analisi in tempo reale, processi di acquisto semplificati e la tabella MATDOC come unica fonte di verità per tutti i movimenti di materiale.",
        submodules: [
          {
            id: "MM-PUR",
            name: "Acquisti (MM-PUR)",
            transactions: [
              { process: "Creazione Richiesta d'Acquisto", appName: "Create Purchase Requisition", appId: "F1643", tCode: "ME51N", purpose: "App moderna per la creazione di RdA, ideale per scenari di self-service procurement." },
              { process: "Elaborazione Richieste d'Acquisto", appName: "Process Purchase Requisitions", appId: "F1048", tCode: "ME56, ME57", purpose: "Worklist per i buyer per visualizzare, assegnare fonti e convertire le RdA in OdA." },
              { process: "Gestione Ordini d'Acquisto", appName: "Manage Purchase Orders", appId: "F0842A", tCode: "ME21N, ME22N, ME23N", purpose: "Successore di ME2xN per la gestione completa (creazione, modifica, monitoraggio) degli OdA." },
              { process: "Gestione Contratti d'Acquisto", appName: "Manage Purchase Contracts", appId: "F1600A", tCode: "ME31K, ME32K, ME33K", purpose: "Per la gestione dei contratti quadro, monitorando rilasci e consumi." },
            ],
          },
          {
            id: "MM-IM",
            name: "Gestione Inventario (MM-IM)",
            transactions: [
              { process: "Entrata Merci da OdA", appName: "Post Goods Receipt for Purchasing Document", appId: "F0843", tCode: "MIGO (Mov. 101)", purpose: "App ottimizzata per la registrazione dell'entrata merci con riferimento a un OdA." },
              { process: "Uscita Merci", appName: "Post Goods Movement", appId: "F2212", tCode: "MIGO (es. Mov. 201)", purpose: "App flessibile per la registrazione di movimenti come uscite per centro di costo o rottamazione." },
              { process: "Trasferimento Stock", appName: "Transfer Stock - In-Plant / Cross-Plant", appId: "F1061 / F1957", tCode: "MIGO (Mov. 311, 301)", purpose: "App specializzate per i trasferimenti di stock tra magazzini o stabilimenti." },
              { process: "Panoramica Stock", appName: "Manage Stock", appId: "F1062", tCode: "MMBE", purpose: "Visione completa e in tempo reale dello stock di un materiale con capacità analitiche avanzate." },
              { process: "Visualizzazione Documenti Materiale", appName: "Material Documents Overview", appId: "F1077", tCode: "MB03", purpose: "Per la ricerca, visualizzazione e analisi dei documenti di materiale registrati." },
            ],
          },
          {
            id: "MM-IV",
            name: "Verifica Fatture (MM-IV)",
            transactions: [
              { process: "Registrazione Fattura (Logistica)", appName: "Create Supplier Invoice - Advanced", appId: "MIRO", tCode: "MIRO", purpose: "Replica della transazione MIRO per la registrazione di fatture complesse con riferimento a OdA." },
              { process: "Gestione Fatture Fornitore", appName: "Manage Supplier Invoices", appId: "F0859", tCode: "MIR4, MRBR", purpose: "Worklist centrale per monitorare lo stato, modificare, stornare e rilasciare le fatture." },
              { process: "Sblocco Fatture", appName: "Release Blocked Invoices", appId: "MRBR", tCode: "MRBR", purpose: "Per l'analisi e il rilascio delle fatture bloccate a causa di discrepanze di prezzo o quantità." },
              { process: "Gestione Conto Transitorio GR/IR", appName: "Clear GR/IR Clearing Account", appId: "F1947", tCode: "MR11", purpose: "Per la manutenzione e la compensazione del conto transitorio merci/fatture." },
            ],
          },
        ],
      },
      {
        id: "PP",
        name: "Production Planning (PP)",
        context: "S/4HANA potenzia PP con MRP Live (calcolo MRP ultra-veloce), l'integrazione di PP/DS nel core, e nuovi paradigmi di pianificazione come pMRP (pianificazione predittiva) e DDMRP (pianificazione basata sulla domanda).",
        submodules: [
          {
            id: "PP-MRP",
            name: "Pianificazione dei Fabbisogni di Materiali (PP-MRP)",
            transactions: [
              { process: "Esecuzione Pianificazione MRP", appName: "Schedule MRP Runs", appId: "F1339", tCode: "MD01N", purpose: "Punto di ingresso per schedulare l'esecuzione di MRP Live, l'algoritmo di pianificazione ottimizzato per HANA." },
              { process: "Monitoraggio Copertura Materiali", appName: "Monitor/Manage Material Coverage", appId: "F0251 / F0247", tCode: "MD04, MD07", purpose: "Evoluzione di MD04, fornisce una visione interattiva e grafica della situazione di copertura di un materiale." },
              { process: "Conversione Ordini Pianificati", appName: "Convert Planned Orders", appId: "F2080", tCode: "CO40, CO41", purpose: "Per la conversione massiva o singola degli ordini pianificati in ordini di produzione o RdA." },
              { process: "Gestione Richieste di Modifica MRP", appName: "Manage Change Requests", appId: "F2132", tCode: "N/A", purpose: "App per la gestione controllata delle modifiche ai fabbisogni per evitare instabilità nella pianificazione." },
            ],
          },
          {
            id: "PP-SFC",
            name: "Gestione Ordini di Produzione (PP-SFC)",
            transactions: [
              { process: "Creazione Ordine di Produzione", appName: "Create Production Order", appId: "CO01", tCode: "CO01", purpose: "Creazione manuale di ordini di produzione, utile per prototipi o ordini di rilavorazione." },
              { process: "Gestione Ordini di Produzione", appName: "Manage Production Orders", appId: "F2336", tCode: "COOIS", purpose: "Strumento principale per i supervisori per monitorare, identificare problemi e rilasciare gli ordini." },
              { process: "Conferma Operazioni", appName: "Confirm Production Operation", appId: "F3069", tCode: "CO11N", purpose: "App user-friendly, anche per mobile, per registrare tempi, quantità prodotte e scarti." },
              { process: "Conferma Ordine", appName: "Confirm Production Order", appId: "CO15", tCode: "CO15", purpose: "Conferma a livello di testata dell'ordine, usata in scenari semplici o per backflush automatico." },
              { process: "Visualizzazione Dettaglio Ordine", appName: "Production Order Object Page", appId: "F2261", tCode: "CO03", purpose: "Scheda informativa con vista a 360 gradi su un singolo ordine di produzione e tutti i documenti collegati." },
            ],
          },
        ],
      },
      {
        id: "SD",
        name: "Sales and Distribution (SD)",
        context: "L'innovazione chiave in S/4HANA SD è aATP (Advanced Available-to-Promise), un framework sofisticato per un adempimento degli ordini più intelligente e reattivo, che include Backorder Processing (BOP) e Product Allocations (PAL).",
        submodules: [
          {
            id: "SD-SLS",
            name: "Vendite (SD-SLS)",
            transactions: [
              { process: "Creazione Ordine di Vendita", appName: "Create Sales Orders - VA01", appId: "VA01", tCode: "VA01", purpose: "Transazione classica per la creazione di ordini di vendita in scenari complessi." },
              { process: "Gestione Ordini di Vendita", appName: "Manage Sales Orders - Version 2", appId: "F3893", tCode: "VA02, VA03, VA05", purpose: "App Fiori ottimizzata per la creazione, modifica e visualizzazione rapida degli ordini di vendita." },
              { process: "Monitoraggio Evasione Ordini", appName: "Sales Order Fulfillment - Analyze Issues", appId: "F2242", tCode: "SDV03V02", purpose: "App analitica per identificare e risolvere proattivamente i problemi che bloccano l'evasione degli ordini." },
              { process: "Monitoraggio Performance Vendite", appName: "Sales Management Overview", appId: "F2601", tCode: "N/A", purpose: "Cockpit analitico per i manager delle vendite con KPI su ordini, resi, margini e blocchi." },
            ],
          },
          {
            id: "SD-SHP",
            name: "Spedizione (SD-SHP)",
            transactions: [
              { process: "Creazione Consegna in Uscita", appName: "Create Outbound Deliveries - From Sales Orders", appId: "F0867L1", tCode: "VL01N, VL10A", purpose: "Per creare una o più consegne in uscita a partire da una lista di ordini di vendita in scadenza." },
              { process: "Gestione Consegne", appName: "Manage Outbound Deliveries", appId: "F0867A", tCode: "VL06O", purpose: "Worklist centrale per monitorare lo stato, avviare il picking e registrare l'uscita merci (PGI)." },
              { process: "Picking", appName: "Pick Outbound Delivery", appId: "F1704", tCode: "VL06P", purpose: "Supporta il processo di prelievo dei materiali dal magazzino per una specifica consegna." },
              { process: "Uscita Merci", appName: "Post Goods Issue for Outbound Delivery", appId: "F2435", tCode: "VL02N", purpose: "Registra l'uscita merci (PGI), riducendo lo stock e registrando il costo del venduto." },
            ],
          },
          {
            id: "SD-BIL",
            name: "Fatturazione (SD-BIL)",
            transactions: [
              { process: "Creazione Fatture", appName: "Create Billing Documents", appId: "F0798", tCode: "VF01", purpose: "Per creare documenti di fatturazione a partire da una lista di documenti pronti per la fatturazione." },
              { process: "Gestione Lista Fatturazione", appName: "Schedule Billing Document Creation", appId: "F1857", tCode: "VF04, VF06", purpose: "Per pianificare e schedulare in background la creazione massiva di fatture." },
              { process: "Gestione Documenti di Fatturazione", appName: "Manage Billing Documents", appId: "F0797", tCode: "VF02, VF03", purpose: "Worklist centrale per l'addetto alla fatturazione per visualizzare, modificare, stornare e analizzare le fatture." },
              { process: "Visualizzazione Dettaglio Fattura", appName: "Billing Document (Object Page)", appId: "F1901", tCode: "VF03", purpose: "Scheda informativa con vista a 360 gradi su un singolo documento di fatturazione." },
            ],
          },
        ],
      },
    ],
  };