// src/modules/CdsData.jsx

export const cdsData = [
  {
      // Questo è il documento principale di guida CDS
      name: "Guida Completa alle Core Data Services (CDS)", // Nome usato per la selezione e visualizzazione
      description: "Il nuovo standard per la modellazione dei dati e la definizione di API in SAP S/4HANA.",
      module: "Generale", // Puoi mantenere un'indicazione generica o rimuoverla se non necessaria nel display
      transactions: ["SE11", "SE80", "ADT (Eclipse)", "/IWFND/MAINT_SERVICE"], // Transazioni rilevanti per CDS
      sections: [
          {
              title: "Sezione 1: Introduzione alle Core Data Services",
              content: `
Le CDS sono un'infrastruttura per definire e consumare modelli di dati semanticamente ricchi. Rappresentano un pilastro fondamentale del paradigma **"code-to-data"** di S/4HANA, che sposta l'elaborazione dei dati dal server applicativo al database, sfruttando la potenza di SAP HANA.

### 1.1 Perché le CDS Views?
A differenza delle viste tradizionali del DDIC (create con SE11), le CDS Views offrono vantaggi significativi:
* **Definizione nel Codice Sorgente:** Sono definite in file di testo (DDL Sources), facilitando il versioning e il trasporto.
* **Arricchimento Semantico:** Vengono arricchite con metadati tramite **annotazioni**, che controllano il comportamento, l'esposizione come servizi (OData) e l'aspetto nelle UI Fiori.
* **Funzionalità Avanzate:** Supportano nativamente calcoli, aggregazioni, associazioni (relazioni) e possono essere estese senza modifiche.
* **Ottimizzazione per HANA:** Sono progettate per sfruttare appieno le capacità del database in-memory di HANA.
`
          },
          {
              title: "Sezione 2: L'Evoluzione: DEFINE VIEW vs. DEFINE VIEW ENTITY",
              content: `
All'interno del mondo ABAP CDS, esiste un'ulteriore distinzione evolutiva di importanza critica. Le viste CDS possono essere create utilizzando due sintassi principali: \`DEFINE VIEW\` (la forma più vecchia) e \`DEFINE VIEW ENTITY\` (la forma moderna e raccomandata).

### 2.1 DEFINE VIEW (CDS DDIC-Based View)
Questa sintassi, ormai considerata obsoleta per i nuovi sviluppi, crea due oggetti distinti nel dizionario ABAP (SE11) al momento dell'attivazione: l'entità CDS (l'oggetto logico utilizzato in ABAP) e una vista DDIC-based gestita da CDS (una vista SQL classica visibile in SE11). Questo processo di doppia attivazione è più lento e crea un accoppiamento più stretto con il DDIC tradizionale.

### 2.2 DEFINE VIEW ENTITY
Questa è la sintassi moderna e strategica. Al momento dell'attivazione, l'entità CDS diventa l'oggetto primario e la vista SQL sottostante nel database è considerata un dettaglio di implementazione, non più rappresentata come una vista separata nel DDIC. Questo approccio offre un'attivazione più rapida, una gestione del ciclo di vita più pulita e un migliore allineamento con le future innovazioni di ABAP, come il Restful Application Programming Model (RAP).

### 2.3 Confronto Architetturale
Per guidare immediatamente gli sviluppatori verso la pratica corretta, la seguente tabella riassume le differenze chiave.

| Caratteristica | CDS DDIC-Based View (DEFINE VIEW) | CDS View Entity (DEFINE VIEW ENTITY) |
|---|---|---|
| **Sintassi di Definizione** | \`DEFINE VIEW <cds_entity_name>\` | \`DEFINE VIEW ENTITY <cds_entity_name>\` |
| **Oggetti Generati** | 1. Entità CDS<br/>2. Vista DDIC gestita (visibile in SE11) | 1. Entità CDS<br/>(La vista SQL nel DB è un artefatto di implementazione) |
| **Performance di Attivazione** | Più lenta, a causa della necessità di attivare e sincronizzare due oggetti nel DDIC. | Più veloce, poiché viene attivata solo l'entità CDS a livello ABAP. |
| **Nome Vista SQL** | Definito con \`@AbapCatalog.sqlViewName\`. Deve essere diverso dal nome dell'entità CDS. | Definito con \`@AbapCatalog.sqlViewName\`. Il nome dell'entità e del file DDL devono coincidere. |
| **Gestione del Mandante** | Gestione esplicita spesso richiesta. | Gestione del mandante implicita e più robusta. Le annotazioni per la gestione del mandante non sono più supportate perché gestite automaticamente. |
| **Estensibilità** | Estensibilità più complessa. | Gestione semplificata delle estensioni. |
| **Supporto a Nuove Funzionalità** | Nessun nuovo miglioramento previsto da SAP. Considerata obsoleta. | Pienamente supportata e base per le future innovazioni del linguaggio ABAP e del modello RAP. |

Dato il chiaro indirizzo strategico di SAP, questo report si concentrerà quasi esclusivamente sulla sintassi e sulle capacità di \`DEFINE VIEW ENTITY\`, menzionando la sintassi legacy solo dove necessario per la comprensione di sistemi esistenti.
`
          },
          {
              title: "Sezione 3: La Struttura Fondamentale di una CDS View Entity",
              content: `
Per utilizzare efficacemente le CDS, è essenziale comprendere la struttura di base dei loro file sorgente e la sintassi fondamentale per la definizione di entità e delle loro relazioni.

### 3.1. Anatomia di un Documento DDL
Le definizioni CDS sono scritte in file sorgente di testo con estensione \`.ddls\` (per le Data Definition Language Source) all'interno dell'ambiente di sviluppo ADT in Eclipse. Una regola fondamentale è che il nome dell'artefatto di primo livello definito nel file DDL deve corrispondere esattamente al nome del file stesso. Il processo di attivazione è ciò che trasforma il codice sorgente DDL in oggetti utilizzabili.

### 3.2. Sintassi di Base: DEFINE VIEW ENTITY
La definizione di una vista è il punto di partenza per qualsiasi modello di dati CDS.
\`\`\`abap
@AbapCatalog.sqlViewName: 'ZSQL_INVOICEITEMS'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Invoice Items View Entity'
DEFINE VIEW ENTITY ZDEMO_I_InvoiceItems
AS SELECT FROM sepm_sddl_so_invoice_item
{
key sales_order_invoice_item_key,
    currency_code,
    gross_amount
}
\`\`\`
#### Analisi del Codice
* **Annotazioni Fondamentali:** \`@AbapCatalog.sqlViewName\` è obbligatoria e definisce il nome della vista fisica nel DB. \`@AccessControl.authorizationCheck: #CHECK\` è cruciale per la sicurezza e abilita i controlli di accesso definiti in un ruolo DCL; i valori possibili sono \`#CHECK\` (l'accesso è controllato e vietato se non c'è un ruolo), \`#NOT_REQUIRED\` (l'accesso è consentito senza controlli), e \`#NOT_ALLOWED\` (i controlli non sono permessi).
* **La parola chiave \`key\`:** Non definisce una chiave primaria tecnica nel database, ma una chiave semantica per l'entità CDS. Indica quali campi identificano in modo univoco una riga dal punto di vista del business, informazione fondamentale per i framework come Fiori Elements e RAP.

### 3.3. Associazioni: Modellare le Relazioni
Le associazioni rappresentano le relazioni tra entità e implementano un meccanismo di **"lazy loading"**: il \`LEFT OUTER JOIN\` viene eseguito solo se un campo dell'entità associata è esplicitamente richiesto, ottimizzando le performance.
\`\`\`abap
DEFINE VIEW ENTITY Z_I_InvoiceItems
AS SELECT FROM sepm_sddl_so_invoice_item AS Item
ASSOCIATION [1..1] TO Z_I_InvoiceHeader AS _Header ON $projection.parent_key = _Header.header_key
{
key Item.sales_order_invoice_item_key,

// 1. Accesso a un campo tramite path expression
_Header.buyer.company_name,

Item.gross_amount,

// 2. Esposizione dell'associazione per i consumatori della vista
_Header
}
\`\`\`
`
          },
          {
              title: "Sezione 4: Tecniche Avanzate di Definizione dei Dati",
              content: `
ABAP CDS offre una vasta gamma di funzionalità per manipolare, calcolare e arricchire i dati direttamente a livello di database.

### 4.1. Espressioni CASE
Le espressioni \`CASE\` consentono di implementare una logica condizionale all'interno di una \`SELECT\` list.
\`\`\`abap
case header.payment_status
when 'P' then 'Paid'
when 'D' then 'Due'
else 'Open'
end as PaymentStatusText
\`\`\`

### 4.2. Campi Calcolati ed Espressioni Aritmetiche
È possibile eseguire calcoli (\`+\`, \`-\`, \`*\`) e usare funzioni (\`CONCAT\`, \`CAST\`) direttamente nella \`SELECT\` list.
\`\`\`abap
(gross_amount - net_amount) AS tax_amount
\`\`\`

### 4.3. Funzioni di Aggregazione (GROUP BY e HAVING)
Supporta \`SUM\`, \`COUNT\`, \`MAX\`, \`MIN\`, \`AVG\`. I campi non aggregati devono essere in \`GROUP BY\`. \`HAVING\` filtra dopo l'aggregazione.
\`\`\`abap
DEFINE VIEW ENTITY Z_C_SalesOrderAggregation
AS SELECT FROM Z_I_SalesOrderItem
{
sales_order_id,
SUM(net_amount) AS total_net_amount
}
GROUP BY sales_order_id
HAVING SUM(net_amount) > 1000
\`\`\`

### 4.4. Viste con Parametri di Input
Permettono di creare modelli dinamici usando \`WITH PARAMETERS\` e accedendovi con \`$parameters.\`.
\`\`\`abap
DEFINE VIEW ENTITY Z_I_FlightsByCarrier
WITH PARAMETERS p_carrier_id : /dmo/carrier_id
AS SELECT FROM /dmo/flight
{ key connection_id }
WHERE carrier_id = $parameters.p_carrier_id
\`\`\`

### 4.5. Unire Set di Dati con UNION e UNION ALL
\`UNION\` combina i risultati rimuovendo i duplicati; \`UNION ALL\` li mantiene, risultando più performante. Le \`SELECT\` devono avere lo stesso numero di colonne e tipi compatibili. L'uso di \`CAST\` è spesso necessario.

| Primo Tipo/Tipo Successivo | INT1/2/4 | INT8 | DEC | FLTP | CHAR | SSTRING | NUMC | DATS | TIMS |
|---|---|---|---|---|---|---|---|---|---|
| **INT1/2/4** | x | w | w | w | - | - | - | - | - |
| **INT8** | x | x | w | w | - | - | - | - | - |
| **DEC** | w | w | w | w | - | - | - | - | - |
| **FLTP** | x | w | w | x | - | - | - | - | - |
| **CHAR** | - | - | - | - | w | w | l | w | w |
| **SSTRING** | - | - | - | - | w | w | - | - | - |
| **NUMC** | - | - | - | - | w | - | l | l | l |
| **DATS** | - | - | - | - | w | - | l | x | - |
| **TIMS** | - | - | - | - | w | - | l | - | x |
Legenda: x = tipo identico, w = compatibile con allargamento, l = compatibile con lunghezza, - = non compatibile.
`
          },
          {
              title: "Sezione 5: Esposizione dei Dati tramite Servizi OData",
              content: `
Una delle funzionalità chiave di ABAP CDS è l'esposizione di modelli di dati come servizi OData pronti per il consumo.

### 5.1. Il Metodo Moderno: Annotazione @OData.publish
Il metodo più semplice e raccomandato per esporre una CDS view come servizio OData V2 è usare \`@OData.publish: true\`. Questa annotazione attiva il framework **SADL (Service Adaptation Description Language)** che genera dinamicamente il servizio (metadati e runtime) senza necessità di codice manuale.

### 5.2. Attivazione e Registrazione del Servizio
Una volta annotata la view, il servizio deve essere registrato nel SAP Gateway Hub.
1.  **Attivare la CDS View** in ADT.
2.  **Avviare la transazione \`/IWFND/MAINT_SERVICE\`**.
3.  Cliccare su **"Add Service"**, inserire l'Alias di Sistema e il nome del servizio (es. \`Z_C_SALESORDER_CDS\`).
4.  Selezionare il servizio e aggiungerlo a un pacchetto.
5.  **Testare il Servizio** usando il "SAP Gateway Client".
`
          },
          {
              title: "Sezione 6: Annotazioni UI per SAP Fiori Elements",
              content: `
Fiori Elements costruisce interfacce utente Fiori standard interpretando annotazioni \`@UI\`, che descrivono come presentare i dati.

### 6.1. Metadata Extensions
Una best practice è separare le annotazioni UI in un file **Metadata Extension** (\`.ddlx\`), mantenendo una chiara separazione dei compiti.

### 6.2. Annotazioni Comuni per una List Report
Una List Report visualizza una lista di oggetti. Le annotazioni chiave sono:
* \`@UI.headerInfo\`: Definisce le informazioni nell'intestazione della pagina.
* \`@UI.selectionField\`: Specifica i campi di filtro nella "Filter Bar".
* \`@UI.lineItem\`: Definisce le colonne nella tabella dei risultati.
\`\`\`abap
@Metadata.layer: #CORE
annotate view Z_C_SalesOrder with
{
@UI.lineItem: [ { position: 10 } ]
SalesOrder;
@UI.selectionField: [ { position: 10 } ]
SalesOrderType;
}
\`\`\`

### 6.3. Annotazioni Comuni per una Object Page
La Object Page è la pagina di dettaglio, strutturata in sezioni (facet).
* \`@UI.facet\`: È l'annotazione fondamentale per strutturare la pagina. Definisce le diverse sezioni (tab o gruppi di campi).
* \`@UI.identification\`: Usata dentro un facet, definisce i campi che appaiono nella sezione principale "General Information".
* \`@UI.dataPoint\`: Mette in evidenza un singolo dato chiave (KPI), tipicamente nell'intestazione.
* \`@UI.lineItem\` (con un'associazione): Se un facet punta a un'associazione, le annotazioni \`@UI.lineItem\` sull'entità di destinazione renderizzeranno una tabella di elementi correlati (es. gli item di un ordine).

### 6.4. Tabella di Riferimento Rapido per Annotazioni UI
| Annotazione | Scopo | Contesto d'Uso |
|---|---|---|
| \`@UI.headerInfo\` | Definisce titolo e contatore per la pagina. | List Report, Object Page |
| \`@UI.selectionField\` | Definisce un campo di filtro. | List Report |
| \`@UI.lineItem\` | Definisce una colonna in una tabella. | List Report, Object Page |
| \`@UI.facet\` | Definisce una sezione (tab, gruppo di campi). | Object Page |
| \`@UI.identification\` | Definisce un campo in una sezione "generale". | Object Page (in un facet) |
| \`@UI.dataPoint\` | Evidenzia un singolo valore chiave (KPI). | Object Page Header |
`
          },
          {
              title: "Sezione 7: Conclusione e Best Practices Riepilogative",
              content: `
Per massimizzare l'efficacia, la manutenibilità e le performance, è fondamentale aderire a queste best practice.

### 7.1. Seguire il Virtual Data Model (VDM)
Stratificare le viste per manutenibilità e riusabilità.
* **Basic/Interface (\`I_\`):** Astrazione sulle tabelle fisiche.
* **Composite (\`P_\`):** Combinano dati da viste Basic/Interface.
* **Consumption (\`C_\`):** Per l'UI/API; non accedono mai direttamente alle tabelle fisiche.

### 7.2. Ottimizzazione delle Performance
* **Filtrare Presto (Filter Early):** Applicare \`WHERE\` il più in basso possibile nella gerarchia delle viste.
* **Proiettare solo i Campi Necessari:** Evitare \`SELECT *\`.
* **Usare Associazioni con Saggezza:** Preferirle ai \`JOIN\` per il loro "lazy loading".

### 7.3. Convenzioni e Sicurezza
* **Convenzioni di Nomenclatura:** Adottare prefissi come \`I_\`, \`P_\`, \`C_\`.
* **Sicurezza Prima di Tutto:** Usare sempre \`@AccessControl.authorizationCheck: #CHECK\` e implementare i controlli in file DCL (\`DEFINE ROLE\`) separati.
`
          }
      ]
  }
];