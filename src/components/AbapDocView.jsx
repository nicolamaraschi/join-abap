import React from 'react';
import AbapCode from './AbapCode.jsx'; // Assicurati che il percorso sia corretto

// Questo componente contiene tutta la documentazione ABAP
// come una pagina statica, per massima stabilità e leggibilità del codice.
const AbapDocumentationView = () => {
  return (
    <div className="details-view report-content-wrapper prose">
      
      {/* Introduction */}
      <div className="details-header-card">
        <h2>Il Compendio dello Sviluppatore ABAP</h2>
        <p>Una Guida ai Concetti Fondamentali e agli Strumenti Essenziali.</p>
      </div>

      {/* Sezione 1: Knowledge Base */}
      <section id="section-knowledge-base" className="module-section">
        <h3>Sezione 1: Navigare nella Knowledge Base Ufficiale di SAP</h3>
        <p className="module-context">Questa sezione stabilisce le fonti autorevoli per tutta la conoscenza sullo sviluppo ABAP.</p>
        
        <h4>1.1 Il SAP Help Portal: Il Repository Centrale</h4>
        <p>Il SAP Help Portal è il principale hub centralizzato per tutta la documentazione ufficiale dei prodotti SAP. Copre l'intera suite di prodotti, dai sistemi ERP come SAP S/4HANA alla Business Technology Platform (BTP). Il portale è organizzato per prodotto e fornisce guide all'implementazione, guide utente e note di rilascio. Offre potenti funzionalità di ricerca, consentendo di filtrare per prodotto o in tutto l'ecosistema SAP.</p>
        <p>Il portale è in continua evoluzione. SAP sta investendo attivamente nella modernizzazione della sua infrastruttura per migliorare l'esperienza degli sviluppatori, includendo un programma pilota per commenti diretti e una funzionalità di "Machine Translation On-the-Fly" basata su AI per traduzioni istantanee in quasi 40 lingue.</p>

        <h4>1.2 La ABAP Keyword Documentation: La Bibbia del Linguaggio</h4>
        <p>Questa è la risorsa di riferimento definitiva per la sintassi e la semantica dei linguaggi su un Application Server ABAP (AS ABAP). È la risorsa più cruciale per qualsiasi sviluppatore.</p>
        <p>È possibile accedervi in diversi modi:</p>
        <ul>
            <li>In modo contestuale nell'editor ABAP premendo <strong>F1</strong> su qualsiasi parola chiave.</li>
            <li>Direttamente tramite i T-Code <strong>abaphelp</strong> o <strong>abapdocu</strong> nella SAP GUI.</li>
            <li>Online attraverso il SAP Help Portal.</li>
        </ul>
        <p>L'ecosistema informativo di SAP può apparire frammentato:</p>
        <ul>
            <li><strong>SAP Help Portal</strong>: Risponde al "cosa" (le regole del linguaggio).</li>
            <li><strong>SAP Support Portal</strong>: Risponde al "come risolvere" (note, incidenti).</li>
            <li><strong>SAP Community</strong>: Risponde al "come fare" (best practice, aiuto tra pari).</li>
        </ul>
      </section>

      {/* Sezione 2: Architettura */}
      <section id="section-architecture" className="module-section">
        <h3>Sezione 2: Il Progetto Architettonico di ABAP</h3>
        <p className="module-context">Questa sezione scompone il linguaggio ABAP nei suoi pilastri fondamentali.</p>

        <h4>2.1 Le Fondamenta: L'ABAP Dictionary (DDIC)</h4>
        <p>L'ABAP Dictionary (DDIC) è un repository di metadati centrale che garantisce integrità, coerenza e riutilizzabilità dei dati.</p>
        <h5>Componenti Fondamentali del DDIC:</h5>
        <ul>
            <li><strong>Tabelle</strong>: Definizioni indipendenti dal database.</li>
            <li><strong>Viste</strong>: Viste logiche su una o più tabelle.</li>
            <li><strong>Tipi di Dati</strong>: Domini (caratteristiche tecniche), Elementi Dati (caratteristiche semantiche), Strutture.</li>
            <li><strong>Tipi Tabella</strong>: Progetti globali per tabelle interne.</li>
            <li><strong>Search Help</strong>: Forniscono l'aiuto all'immissione F4.</li>
            <li><strong>Lock Object</strong>: Gestiscono l'accesso concorrente.</li>
        </ul>

        <h4>2.2 I Paradigmi di Programmazione: Procedurale vs. Orientato agli Oggetti</h4>
        <p>SAP raccomanda vivamente l'uso di ABAP Objects (programmazione orientata agli oggetti) per tutti i nuovi sviluppi grazie a vantaggi come incapsulamento, ereditarietà e accesso a nuove tecnologie.</p>

        <h4>2.4 Il Paradigma Moderno: Code-to-Data</h4>
        <p>Guidato da SAP HANA, questo paradigma spinge la logica di elaborazione nel database. Le tecniche principali, in ordine di preferenza SAP, sono:</p>
        <ol>
            <li><strong>Open SQL</strong>: Lo strumento più astratto e portabile.</li>
            <li><strong>Core Data Services (CDS) Views</strong>: Un modo dichiarativo potente per modellare i dati.</li>
            <li><strong>ABAP Managed Database Procedures (AMDP)</strong>: Opzione finale per logica complessa e specifica del database.</li>
        </ol>
      </section>

      {/* Sezione 3: ALV */}
      <section id="section-alv-masterclass" className="module-section">
        <h3>Sezione 3: Una Masterclass sull'ABAP List Viewer (ALV)</h3>
        
        <h4>3.1 L'Astrazione Moderna: CL_SALV_TABLE</h4>
        <p><code>CL_SALV_TABLE</code> è un'API semplificata e moderna per la visualizzazione di dati. È ideale per report rapidi e non modificabili.</p>
        <div className="preset-code-container">
            <AbapCode code={`
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
            `} />
        </div>

        <h4>3.2 Il Potente Cavallo di Battaglia: CL_GUI_ALV_GRID</h4>
        <p><code>CL_GUI_ALV_GRID</code> è la classe fondamentale per creare griglie altamente personalizzabili e interattive, specialmente se è richiesta la modifica dei dati da parte dell'utente.</p>
        <div className="preset-code-container">
            <AbapCode code={`
" In PBO di una schermata
MODULE show_orders OUTPUT.
  IF lo_grid IS NOT BOUND.
    " ... (Setup del field catalog e dei dati) ...
    <fs_fieldcat>-edit = 'X'. " Rende una colonna modificabile

    CREATE OBJECT lo_grid
      EXPORTING i_parent = cl_gui_container=>screen0.
    
    lo_grid->set_table_for_first_display(...).
  ENDIF.
ENDMODULE.
            `} />
        </div>
      </section>

      {/* Sezione 4: Toolkit */}
      <section id="section-essential-fm" className="module-section">
          <h3>Sezione 4: Il Toolkit Essenziale dello Sviluppatore</h3>
          <p>Questa sezione elenca alcuni dei moduli funzione standard più comuni e utili.</p>
          <div className="table-container" style={{marginTop: '1rem'}}>
              <table>
                  <thead>
                      <tr>
                          <th>Modulo Funzione</th>
                          <th>Categoria</th>
                          <th>Scopo</th>
                      </tr>
                  </thead>
                  <tbody>
                      <tr><td><code>GUI_UPLOAD</code></td><td>I/O File</td><td>Carica un file dal PC a una tabella interna.</td></tr>
                      <tr><td><code>GUI_DOWNLOAD</code></td><td>I/O File</td><td>Scarica una tabella interna in un file sul PC.</td></tr>
                      <tr><td><code>READ_TEXT</code></td><td>Gestione Testi</td><td>Legge oggetti di testo lungo.</td></tr>
                      <tr><td><code>BAPI_TRANSACTION_COMMIT</code></td><td>Controllo Transazioni</td><td>Esegue un commit sul database.</td></tr>
                      <tr><td><code>POPUP_TO_CONFIRM</code></td><td>Dialoghi</td><td>Visualizza una finestra di dialogo di conferma.</td></tr>
                  </tbody>
              </table>
          </div>
      </section>

    </div>
  );
};

export default AbapDocumentationView;

