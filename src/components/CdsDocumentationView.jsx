import React from 'react';
import AbapCode from './AbapCode.jsx';

// Questo componente contiene la documentazione specifica per le Core Data Services (CDS).
const CdsDocumentationView = () => {
  return (
    <div className="details-view report-content-wrapper prose">
      
      {/* Introduction */}
      <div className="details-header-card">
        <h2>Guida alle Core Data Services (CDS)</h2>
        <p>Il nuovo standard per la modellazione dei dati e la definizione di API in SAP S/4HANA.</p>
      </div>

      {/* Sezione 1: Cosa sono le CDS Views? */}
      <section id="section-cds-intro" className="module-section">
        <h3>Sezione 1: Introduzione alle Core Data Services</h3>
        <p className="module-context">Le CDS sono un'infrastruttura per definire e consumare modelli di dati semanticamente ricchi. Rappresentano un pilastro fondamentale del paradigma <strong>"code-to-data"</strong> di S/4HANA, che sposta l'elaborazione dei dati dal server applicativo al database, sfruttando la potenza di SAP HANA.</p>
        
        <h4>1.1 Perché le CDS Views?</h4>
        <p>A differenza delle viste tradizionali del DDIC (create con SE11), le CDS Views offrono vantaggi significativi:</p>
        <ul>
            <li><strong>Definizione nel Codice Sorgente:</strong> Sono definite in file di testo (DDL Sources), facilitando il versioning e il trasporto.</li>
            <li><strong>Arricchimento Semantico:</strong> Vengono arricchite con metadati tramite <strong>annotazioni</strong>, che controllano il comportamento, l'esposizione come servizi (OData) e l'aspetto nelle UI Fiori.</li>
            <li><strong>Funzionalità Avanzate:</strong> Supportano nativamente calcoli, aggregazioni, associazioni (relazioni) e possono essere estese senza modifiche.</li>
            <li><strong>Ottimizzazione per HANA:</strong> Sono progettate per sfruttare appieno le capacità del database in-memory di HANA.</li>
        </ul>
      </section>

      {/* Sezione 2: L'Evoluzione - DEFINE VIEW vs. DEFINE VIEW ENTITY */}
      <section id="section-cds-evolution" className="module-section">
        <h3>Sezione 2: L'Evoluzione: DEFINE VIEW vs. DEFINE VIEW ENTITY</h3>
        <p className="module-context">All'interno del mondo ABAP CDS, esiste un'ulteriore distinzione evolutiva di importanza critica. Le viste CDS possono essere create utilizzando due sintassi principali: <code>DEFINE VIEW</code> (la forma più vecchia) e <code>DEFINE VIEW ENTITY</code> (la forma moderna e raccomandata).</p>
        
        <h4>2.1 DEFINE VIEW (CDS DDIC-Based View)</h4>
        <p>Questa sintassi, ormai considerata obsoleta per i nuovi sviluppi, crea due oggetti distinti nel dizionario ABAP (SE11) al momento dell'attivazione: l'entità CDS (l'oggetto logico utilizzato in ABAP) e una vista DDIC-based gestita da CDS (una vista SQL classica visibile in SE11). Questo processo di doppia attivazione è più lento e crea un accoppiamento più stretto con il DDIC tradizionale.</p>
        
        <h4>2.2 DEFINE VIEW ENTITY</h4>
        <p>Questa è la sintassi moderna e strategica. Al momento dell'attivazione, l'entità CDS diventa l'oggetto primario e la vista SQL sottostante nel database è considerata un dettaglio di implementazione, non più rappresentata come una vista separata nel DDIC. Questo approccio offre un'attivazione più rapida, una gestione del ciclo di vita più pulita e un migliore allineamento con le future innovazioni di ABAP, come il Restful Application Programming Model (RAP).</p>
        
        <h4>2.3 Confronto Architetturale</h4>
        <p>Per guidare immediatamente gli sviluppatori verso la pratica corretta, la seguente tabella riassume le differenze chiave.</p>
        <div className="table-container">
            <table>
                <caption>Tabella 1: Confronto Architetturale: CDS DDIC-Based View vs. CDS View Entity</caption>
                <thead>
                    <tr>
                        <th>Caratteristica</th>
                        <th>CDS DDIC-Based View (DEFINE VIEW)</th>
                        <th>CDS View Entity (DEFINE VIEW ENTITY)</th>
                    </tr>
                </thead>
                <tbody>
                    <tr><td><strong>Sintassi di Definizione</strong></td><td><code>DEFINE VIEW &lt;cds_entity_name&gt;</code></td><td><code>DEFINE VIEW ENTITY &lt;cds_entity_name&gt;</code></td></tr>
                    <tr><td><strong>Oggetti Generati</strong></td><td>1. Entità CDS<br/>2. Vista DDIC gestita (visibile in SE11)</td><td>1. Entità CDS<br/>(La vista SQL nel DB è un artefatto di implementazione)</td></tr>
                    <tr><td><strong>Performance di Attivazione</strong></td><td>Più lenta, a causa della necessità di attivare e sincronizzare due oggetti nel DDIC.</td><td>Più veloce, poiché viene attivata solo l'entità CDS a livello ABAP.</td></tr>
                    <tr><td><strong>Nome Vista SQL</strong></td><td>Definito con <code>@AbapCatalog.sqlViewName</code>. Deve essere diverso dal nome dell'entità CDS.</td><td>Definito con <code>@AbapCatalog.sqlViewName</code>. Il nome dell'entità e del file DDL devono coincidere.</td></tr>
                    <tr><td><strong>Gestione del Mandante</strong></td><td>Gestione esplicita spesso richiesta.</td><td>Gestione del mandante implicita e più robusta. Le annotazioni per la gestione del mandante non sono più supportate perché gestite automaticamente.</td></tr>
                    <tr><td><strong>Estensibilità</strong></td><td>Estensibilità più complessa.</td><td>Gestione semplificata delle estensioni.</td></tr>
                    <tr><td><strong>Supporto a Nuove Funzionalità</strong></td><td>Nessun nuovo miglioramento previsto da SAP. Considerata obsoleta.</td><td>Pienamente supportata e base per le future innovazioni del linguaggio ABAP e del modello RAP.</td></tr>
                </tbody>
            </table>
        </div>
        <p style={{marginTop: '1rem'}}>Dato il chiaro indirizzo strategico di SAP, questo report si concentrerà quasi esclusivamente sulla sintassi e sulle capacità di <code>DEFINE VIEW ENTITY</code>, menzionando la sintassi legacy solo dove necessario per la comprensione di sistemi esistenti.</p>
      </section>

      {/* Sezione 3: La Struttura Fondamentale di una CDS View Entity */}
      <section id="section-cds-structure" className="module-section">
        <h3>Sezione 3: La Struttura Fondamentale di una CDS View Entity</h3>
        <p className="module-context">Per utilizzare efficacemente le CDS, è essenziale comprendere la struttura di base dei loro file sorgente e la sintassi fondamentale per la definizione di entità e delle loro relazioni.</p>
        
        <h4>3.1. Anatomia di un Documento DDL</h4>
        <p>Le definizioni CDS sono scritte in file sorgente di testo con estensione <code>.ddls</code> (per le Data Definition Language Source) all'interno dell'ambiente di sviluppo ADT in Eclipse. Una regola fondamentale è che il nome dell'artefatto di primo livello definito nel file DDL deve corrispondere esattamente al nome del file stesso. Il processo di attivazione è ciò che trasforma il codice sorgente DDL in oggetti utilizzabili.</p>

        <h4>3.2. Sintassi di Base: DEFINE VIEW ENTITY</h4>
        <p>La definizione di una vista è il punto di partenza per qualsiasi modello di dati CDS.</p>
        <AbapCode code={`
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
        `} />
        <h5>Analisi del Codice</h5>
        <ul>
          <li><strong>Annotazioni Fondamentali:</strong> <code>@AbapCatalog.sqlViewName</code> è obbligatoria e definisce il nome della vista fisica nel DB. <code>@AccessControl.authorizationCheck: #CHECK</code> è cruciale per la sicurezza e abilita i controlli di accesso definiti in un ruolo DCL; i valori possibili sono <code>#CHECK</code> (l'accesso è controllato e vietato se non c'è un ruolo), <code>#NOT_REQUIRED</code> (l'accesso è consentito senza controlli), e <code>#NOT_ALLOWED</code> (i controlli non sono permessi).</li>
          <li><strong>La parola chiave <code>key</code>:</strong> Non definisce una chiave primaria tecnica nel database, ma una chiave semantica per l'entità CDS. Indica quali campi identificano in modo univoco una riga dal punto di vista del business, informazione fondamentale per i framework come Fiori Elements e RAP.</li>
        </ul>

        <h4>3.3. Associazioni: Modellare le Relazioni</h4>
        <p>Le associazioni rappresentano le relazioni tra entità e implementano un meccanismo di <strong>"lazy loading"</strong>: il <code>LEFT OUTER JOIN</code> viene eseguito solo se un campo dell'entità associata è esplicitamente richiesto, ottimizzando le performance.</p>
        <AbapCode code={`
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
        `} />
      </section>
      
      {/* Sezione 4: Tecniche Avanzate di Definizione dei Dati */}
      <section id="section-cds-advanced" className="module-section">
        <h3>Sezione 4: Tecniche Avanzate di Definizione dei Dati</h3>
        <p className="module-context">ABAP CDS offre una vasta gamma di funzionalità per manipolare, calcolare e arricchire i dati direttamente a livello di database.</p>

        <h4>4.1. Espressioni CASE</h4>
        <p>Le espressioni <code>CASE</code> consentono di implementare una logica condizionale all'interno di una <code>SELECT</code> list.</p>
        <AbapCode code={`
case header.payment_status
  when 'P' then 'Paid'
  when 'D' then 'Due'
  else 'Open'
end as PaymentStatusText
        `} />

        <h4>4.2. Campi Calcolati ed Espressioni Aritmetiche</h4>
        <p>È possibile eseguire calcoli (<code>+</code>, <code>-</code>, <code>*</code>) e usare funzioni (<code>CONCAT</code>, <code>CAST</code>) direttamente nella <code>SELECT</code> list.</p>
        <AbapCode code={`
(gross_amount - net_amount) AS tax_amount
        `} />
        
        <h4>4.3. Funzioni di Aggregazione (GROUP BY e HAVING)</h4>
        <p>Supporta <code>SUM</code>, <code>COUNT</code>, <code>MAX</code>, <code>MIN</code>, <code>AVG</code>. I campi non aggregati devono essere in <code>GROUP BY</code>. <code>HAVING</code> filtra dopo l'aggregazione.</p>
        <AbapCode code={`
DEFINE VIEW ENTITY Z_C_SalesOrderAggregation
  AS SELECT FROM Z_I_SalesOrderItem
{
  sales_order_id,
  SUM(net_amount) AS total_net_amount
}
GROUP BY sales_order_id
HAVING SUM(net_amount) > 1000
        `} />
        
        <h4>4.4. Viste con Parametri di Input</h4>
        <p>Permettono di creare modelli dinamici usando <code>WITH PARAMETERS</code> e accedendovi con <code>$parameters.</code>.</p>
        <AbapCode code={`
DEFINE VIEW ENTITY Z_I_FlightsByCarrier
  WITH PARAMETERS p_carrier_id : /dmo/carrier_id
  AS SELECT FROM /dmo/flight
{ key connection_id }
WHERE carrier_id = $parameters.p_carrier_id
        `} />

        <h4>4.5. Unire Set di Dati con UNION e UNION ALL</h4>
        <p><code>UNION</code> combina i risultati rimuovendo i duplicati; <code>UNION ALL</code> li mantiene, risultando più performante. Le <code>SELECT</code> devono avere lo stesso numero di colonne e tipi compatibili. L'uso di <code>CAST</code> è spesso necessario.</p>
        
        {/* TABELLA CORRETTA */}
        <div className="table-container">
            <table>
                <caption>Tabella 2: Tabella di Compatibilità dei Tipi di Dati per UNION</caption>
                <thead>
                    <tr><th>Primo Tipo/Tipo Successivo</th><th>INT1/2/4</th><th>INT8</th><th>DEC</th><th>FLTP</th><th>CHAR</th><th>SSTRING</th><th>NUMC</th><th>DATS</th><th>TIMS</th></tr>
                </thead>
                <tbody>
                    <tr><td><strong>INT1/2/4</strong></td><td>x</td><td>w</td><td>w</td><td>w</td><td>-</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
                    <tr><td><strong>INT8</strong></td><td>x</td><td>x</td><td>w</td><td>w</td><td>-</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
                    <tr><td><strong>DEC</strong></td><td>w</td><td>w</td><td>w</td><td>w</td><td>-</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
                    <tr><td><strong>FLTP</strong></td><td>x</td><td>w</td><td>w</td><td>x</td><td>-</td><td>-</td><td>-</td><td>-</td><td>-</td></tr>
                    <tr><td><strong>CHAR</strong></td><td>-</td><td>-</td><td>-</td><td>-</td><td>w</td><td>w</td><td>l</td><td>w</td><td>w</td></tr>
                    <tr><td><strong>SSTRING</strong></td><td>-</td><td>-</td><td>-</td><td>-</td><td>w</td><td>w</td><td>-</td><td>-</td><td>-</td></tr>
                    <tr><td><strong>NUMC</strong></td><td>-</td><td>-</td><td>-</td><td>-</td><td>w</td><td>-</td><td>l</td><td>l</td><td>l</td></tr>
                    <tr><td><strong>DATS</strong></td><td>-</td><td>-</td><td>-</td><td>-</td><td>w</td><td>-</td><td>l</td><td>x</td><td>-</td></tr>
                    <tr><td><strong>TIMS</strong></td><td>-</td><td>-</td><td>-</td><td>-</td><td>w</td><td>-</td><td>l</td><td>-</td><td>x</td></tr>
                </tbody>
                <tfoot>
                    <tr><td colSpan="10">Legenda: x = tipo identico, w = compatibile con allargamento, l = compatibile con lunghezza, - = non compatibile.</td></tr>
                </tfoot>
            </table>
        </div>
      </section>

      {/* Sezione 5: Esposizione dei Dati tramite Servizi OData */}
      <section id="section-cds-odata" className="module-section">
        <h3>Sezione 5: Esposizione dei Dati tramite Servizi OData</h3>
        <p className="module-context">Una delle funzionalità chiave di ABAP CDS è l'esposizione di modelli di dati come servizi OData pronti per il consumo.</p>

        <h4>5.1. Il Metodo Moderno: Annotazione @OData.publish</h4>
        <p>Il metodo più semplice e raccomandato per esporre una CDS view come servizio OData V2 è usare <code>@OData.publish: true</code>. Questa annotazione attiva il framework <strong>SADL (Service Adaptation Description Language)</strong> che genera dinamicamente il servizio (metadati e runtime) senza necessità di codice manuale.</p>

        <h4>5.2. Attivazione e Registrazione del Servizio</h4>
        <p>Una volta annotata la view, il servizio deve essere registrato nel SAP Gateway Hub.</p>
        <ol>
          <li><strong>Attivare la CDS View</strong> in ADT.</li>
          <li><strong>Avviare la transazione <code>/IWFND/MAINT_SERVICE</code></strong>.</li>
          <li>Cliccare su <strong>"Add Service"</strong>, inserire l'Alias di Sistema e il nome del servizio (es. <code>Z_C_SALESORDER_CDS</code>).</li>
          <li>Selezionare il servizio e aggiungerlo a un pacchetto.</li>
          <li><strong>Testare il Servizio</strong> usando il "SAP Gateway Client".</li>
        </ol>
      </section>

      {/* Sezione 6: Annotazioni UI per SAP Fiori Elements */}
      <section id="section-cds-ui-annotations" className="module-section">
        <h3>Sezione 6: Annotazioni UI per SAP Fiori Elements</h3>
        <p className="module-context">Fiori Elements costruisce interfacce utente Fiori standard interpretando annotazioni <code>@UI</code>, che descrivono come presentare i dati.</p>

        <h4>6.1. Metadata Extensions</h4>
        <p>Una best practice è separare le annotazioni UI in un file <strong>Metadata Extension</strong> (<code>.ddlx</code>), mantenendo una chiara separazione dei compiti.</p>

        <h4>6.2. Annotazioni Comuni per una List Report</h4>
        <p>Una List Report visualizza una lista di oggetti. Le annotazioni chiave sono:</p>
        <ul>
          <li><code>@UI.headerInfo</code>: Definisce le informazioni nell'intestazione della pagina.</li>
          <li><code>@UI.selectionField</code>: Specifica i campi di filtro nella "Filter Bar".</li>
          <li><code>@UI.lineItem</code>: Definisce le colonne nella tabella dei risultati.</li>
        </ul>
        <AbapCode code={`
@Metadata.layer: #CORE
annotate view Z_C_SalesOrder with
{
  @UI.lineItem: [ { position: 10 } ]
  SalesOrder;
  @UI.selectionField: [ { position: 10 } ]
  SalesOrderType;
}
        `} />

        {/* DESCRIZIONE CORRETTA */}
        <h4>6.3. Annotazioni Comuni per una Object Page</h4>
        <p>La Object Page è la pagina di dettaglio, strutturata in sezioni (facet).</p>
        <ul>
            <li><code>@UI.facet</code>: È l'annotazione fondamentale per strutturare la pagina. Definisce le diverse sezioni (tab o gruppi di campi).</li>
            <li><code>@UI.identification</code>: Usata dentro un facet, definisce i campi che appaiono nella sezione principale "General Information".</li>
            <li><code>@UI.dataPoint</code>: Mette in evidenza un singolo dato chiave (KPI), tipicamente nell'intestazione.</li>
            <li><code>@UI.lineItem</code> (con un'associazione): Se un facet punta a un'associazione, le annotazioni <code>@UI.lineItem</code> sull'entità di destinazione renderizzeranno una tabella di elementi correlati (es. gli item di un ordine).</li>
        </ul>
        
        <h4>6.4. Tabella di Riferimento Rapido per Annotazioni UI</h4>
        <div className="table-container">
            <table>
                <thead><tr><th>Annotazione</th><th>Scopo</th><th>Contesto d'Uso</th></tr></thead>
                <tbody>
                    <tr><td><code>@UI.headerInfo</code></td><td>Definisce titolo e contatore per la pagina.</td><td>List Report, Object Page</td></tr>
                    <tr><td><code>@UI.selectionField</code></td><td>Definisce un campo di filtro.</td><td>List Report</td></tr>
                    <tr><td><code>@UI.lineItem</code></td><td>Definisce una colonna in una tabella.</td><td>List Report, Object Page</td></tr>
                    <tr><td><code>@UI.facet</code></td><td>Definisce una sezione (tab, gruppo di campi).</td><td>Object Page</td></tr>
                    <tr><td><code>@UI.identification</code></td><td>Definisce un campo in una sezione "generale".</td><td>Object Page (in un facet)</td></tr>
                    <tr><td><code>@UI.dataPoint</code></td><td>Evidenzia un singolo valore chiave (KPI).</td><td>Object Page Header</td></tr>
                </tbody>
            </table>
        </div>
      </section>
      
      {/* Sezione 7: Conclusione e Best Practices */}
      <section id="section-cds-best-practices" className="module-section">
        <h3>Sezione 7: Conclusione e Best Practices Riepilogative</h3>
        <p className="module-context">Per massimizzare l'efficacia, la manutenibilità e le performance, è fondamentale aderire a queste best practice.</p>
        
        <h4>7.1. Seguire il Virtual Data Model (VDM)</h4>
        <p>Stratificare le viste per manutenibilità e riusabilità.</p>
        <ul>
          <li><strong>Basic/Interface (<code>I_</code>):</strong> Astrazione sulle tabelle fisiche.</li>
          <li><strong>Composite (<code>P_</code>):</strong> Combinano dati da viste Basic/Interface.</li>
          <li><strong>Consumption (<code>C_</code>):</strong> Per l'UI/API; non accedono mai direttamente alle tabelle fisiche.</li>
        </ul>

        <h4>7.2. Ottimizzazione delle Performance</h4>
        <ul>
          <li><strong>Filtrare Presto (Filter Early):</strong> Applicare <code>WHERE</code> il più in basso possibile nella gerarchia delle viste.</li>
          <li><strong>Proiettare solo i Campi Necessari:</strong> Evitare <code>SELECT *</code>.</li>
          <li><strong>Usare Associazioni con Saggezza:</strong> Preferirle ai <code>JOIN</code> per il loro "lazy loading".</li>
        </ul>
        
        <h4>7.3. Convenzioni e Sicurezza</h4>
        <ul>
            <li><strong>Convenzioni di Nomenclatura:</strong> Adottare prefissi come <code>I_</code>, <code>P_</code>, <code>C_</code>.</li>
            <li><strong>Sicurezza Prima di Tutto:</strong> Usare sempre <code>@AccessControl.authorizationCheck: #CHECK</code> e implementare i controlli in file DCL (<code>DEFINE ROLE</code>) separati.</li>
        </ul>
      </section>

    </div>
  );
};

export default CdsDocumentationView;