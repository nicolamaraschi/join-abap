export const content = `
Guida Tecnica Completa alla Classe SAP CL_DOCUMENT_BCS
=====================================================

Sezione 1: Concetti Fondamentali: CL_DOCUMENT_BCS nel Framework BCS
-------------------------------------------------------------------
Questa sezione stabilisce il contesto, spiegando che CL_DOCUMENT_BCS non è un'utilità a sé stante, ma un componente critico all'interno di un ecosistema più ampio e orientato agli oggetti.

### 1.1 Scopo e Ruolo: l'Oggetto Documento

La classe CL_DOCUMENT_BCS è il pilastro fondamentale per la creazione di qualsiasi contenuto da inviare tramite i Business Communication Services (BCS) di SAP. Il suo scopo primario è incapsulare un documento come un oggetto ABAP. Questo oggetto può rappresentare sia il corpo principale di una comunicazione (ad esempio, il testo di un'email) sia un allegato. La classe implementa l'interfaccia IF_DOCUMENT_BCS, un dettaglio tecnico che la rende un componente polimorfico e standardizzato all'interno del framework.

L'approccio orientato agli oggetti permette di separare nettamente la gestione del contenuto del documento dalla meccanica del suo invio. In pratica, la classe CL_DOCUMENT_BCS agisce come un contenitore che detiene tutte le proprietà intrinseche di un documento: il suo contenuto (in formato testuale o binario), il suo tipo (ad esempio, 'RAW' per testo semplice, 'HTM' per HTML, 'PDF' per documenti PDF), e i suoi metadati (come oggetto, importanza e sensibilità). Questa incapsulazione garantisce che, una volta creato, l'oggetto documento sia un'entità autonoma e completa, pronta per essere passata al servizio di invio.

### 1.2 L'Ecosistema BCS: una Catena di Oggetti

L'utilizzo efficace di CL_DOCUMENT_BCS richiede la comprensione del suo ruolo all'interno di un processo a catena che coinvolge diversi oggetti del framework BCS. Il flusso di lavoro standard per l'invio di una comunicazione è una sequenza logica di creazione e interazione di oggetti:

1.  **Creazione della Richiesta di Invio:** Il processo inizia con la creazione di un'istanza dell'oggetto che gestirà la trasmissione. Questo viene fatto tramite una chiamata al metodo statico della classe principale del framework: \`DATA(lo_send_request) = cl_bcs=>create_persistent( )\`. Questo oggetto lo_send_request funge da orchestratore per l'intero processo di invio.
2.  **Creazione dell'Oggetto Documento:** Successivamente, si crea l'oggetto documento utilizzando uno dei metodi di factory di CL_DOCUMENT_BCS, come: \`DATA(lo_document) = cl_document_bcs=>create_document(...)\`. Questo è il momento in cui il contenuto (corpo dell'email, allegati) viene definito.
3.  **Assegnazione del Documento alla Richiesta:** L'oggetto documento appena creato viene quindi collegato alla richiesta di invio: \`lo_send_request->set_document( lo_document )\`. Da questo punto, la richiesta di invio "conosce" quale contenuto dovrà trasmettere.
4.  **Creazione e Assegnazione di Mittente e Destinatari:** Vengono creati oggetti separati per rappresentare il mittente (es. \`cl_sapuser_bcs\`) e i destinatari (es. \`cl_cam_address_bcs\`), che vengono poi aggiunti alla richiesta di invio.
5.  **Invio della Richiesta:** Infine, viene invocato il metodo di invio sull'oggetto richiesta: \`lo_send_request->send( )\`.

Questa sequenza evidenzia un principio architetturale chiave: la "separazione delle responsabilità". La classe CL_DOCUMENT_BCS gestisce il "cosa" (il contenuto), mentre la classe CL_BCS gestisce il "come" (la trasmissione).

---

Sezione 2: Istanziazione del Documento: Metodi di Factory Statici
-----------------------------------------------------------------
Questa sezione descrive in dettaglio i metodi "costruttori" utilizzati per creare un'istanza di un oggetto documento.

### 2.1 Il Costruttore Principale: CREATE_DOCUMENT

È il metodo di factory più versatile per creare un documento partendo sia da contenuto testuale (\`I_TEXT\`) sia da contenuto binario (\`I_HEX\`).

* **I_TYPE**: Definisce il formato ('RAW', 'HTM', 'PDF'). Essenziale per il rendering corretto.
* **I_SUBJECT**: Titolo interno del documento (max 50 caratteri).
* **I_TEXT**: Tabella di tipo \`BCSY_TEXT\` per contenuto testuale.
* **I_HEX**: Tabella di tipo \`SOLIX_TAB\` per contenuto binario (HTML, PDF, immagini).

**Esempio (Testo Semplice):**
\`\`\`abap
DATA: lt_text TYPE bcsy_text.
APPEND 'Questa è una riga di testo.' TO lt_text.
DATA(lo_document) = cl_document_bcs=>create_document(
  i_type    = 'RAW'
  i_text    = lt_text
  i_subject = 'Testo Semplice'
).
\`\`\`

**Esempio (HTML):**
\`\`\`abap
DATA: lv_html_string TYPE string,
      lv_html_xstring TYPE xstring,
      lt_html_solix  TYPE solix_tab.

lv_html_string = '<html><body><h1>Titolo</h1><p>Testo in HTML.</p></body></html>'.
CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
  EXPORTING text = lv_html_string
  IMPORTING buffer = lv_html_xstring.

lt_html_solix = cl_document_bcs=>xstring_to_solix( lv_html_xstring ).

DATA(lo_html_document) = cl_document_bcs=>create_document(
  i_type    = 'HTM'
  i_hex     = lt_html_solix
  i_subject = 'Email HTML'
).
\`\`\`

---

Sezione 3: Arricchire il Documento: Allegati e Conversioni
-----------------------------------------------------------

### 3.1 Il Metodo ADD_ATTACHMENT

Aggiunge un allegato a un oggetto documento esistente.

* **I_ATTACHMENT_TYPE**: Tipo di file/estensione ('PDF', 'CSV', 'XLS').
* **I_ATTACHMENT_SUBJECT**: Nome del file come apparirà al destinatario.
* **I_ATT_CONTENT_HEX**: Contenuto binario dell'allegato (BEST PRACTICE).

**L'Imperativo Binario:** Per garantire l'integrità dei file, è fondamentale convertire sempre il contenuto degli allegati in formato binario (\`SOLIX_TAB\` o \`XSTRING\`) e passarlo al parametro \`I_ATT_CONTENT_HEX\`. Evitare \`I_ATTACHMENT_TEXT\` per file non di testo.

### 3.2 Utilità Essenziali per la Conversione dei Dati

| Sorgente            | Destinazione              | Snippet di Conversione                                        |
|---------------------|---------------------------|---------------------------------------------------------------|
| STRING (per HTML)   | I_HEX / I_ATT_CONTENT_HEX | \`SCMS_STRING_TO_XSTRING\` poi \`cl_bcs=>xstring_to_solix()\` |
| SOLI_TAB (Testo)    | I_HEX / I_ATT_CONTENT_HEX | \`SCMS_TEXT_TO_BINARY\`                                         |
| XSTRING (da PDF)    | I_HEX / I_ATT_CONTENT_HEX | \`cl_document_bcs=>xstring_to_solix()\`                         |
| SOLIX_TAB (Binario) | XSTRING                   | \`SCMS_BINARY_TO_XSTRING\`                                      |

---

Sezione 4: Gestione delle Eccezioni con CX_DOCUMENT_BCS
----------------------------------------------------------
È obbligatorio avvolgere il codice BCS in un blocco \`TRY...CATCH\` per gestire professionalmente gli errori.

**Best Practice:** Intercettare sempre prima le eccezioni più specifiche.

\`\`\`abap
TRY.
    " ... codice di creazione documento e allegati ...
  CATCH cx_document_bcs INTO DATA(lx_document_bcs).
    " Errore specifico del documento
    WRITE: lx_document_bcs->get_text( ).
  CATCH cx_bcs INTO DATA(lx_bcs).
    " Errore generico BCS
    WRITE: lx_bcs->get_text( ).
ENDTRY.
\`\`\`

---

Sezione 5: Tecniche Avanzate
-----------------------------

### 5.1 Superare il Limite di 50 Caratteri dell'Oggetto Email

L'oggetto finale dell'email si imposta sull'oggetto richiesta, non sul documento.

\`\`\`abap
" Dopo lo_send_request->set_document(...)
lo_send_request->set_message_subject(
  ip_subject = 'Questo è un oggetto molto lungo che supera i 50 caratteri'
).
\`\`\`

---

Sezione 6: Scenario Completo (Email HTML con Allegato)
--------------------------------------------------------

\`\`\`abap
REPORT z_bcs_html_with_attachment.

DATA: lv_recipient_email TYPE ad_smtpadr VALUE 'destinatario@esempio.com'.
DATA: lv_pdf_xstring     TYPE xstring. " Supponiamo sia già popolata

TRY.
    DATA(lo_send_request) = cl_bcs=>create_persistent( ).

    DATA(lv_html_body) = '<html>... corpo HTML ...</html>'.
    DATA(lt_html_solix) = cl_document_bcs=>string_to_soli( lv_html_body ).

    DATA(lo_document) = cl_document_bcs=>create_document(
      i_type    = 'HTM',
      i_text    = lt_html_solix,
      i_subject = 'Dettagli Ordine'
    ).

    lo_document->add_attachment(
      i_attachment_type    = 'PDF',
      i_attachment_subject = 'ConfermaOrdine.pdf',
      i_att_content_hex    = cl_document_bcs=>xstring_to_solix( lv_pdf_xstring )
    ).

    lo_send_request->set_document( lo_document ).
    lo_send_request->set_message_subject( 'Oggetto lungo qui' ).

    DATA(lo_recipient) = cl_cam_address_bcs=>create_internet_address( lv_recipient_email ).
    lo_send_request->add_recipient( lo_recipient ).

    lo_send_request->send( ).
    COMMIT WORK.
    WRITE: / 'Email inviata.'.

  CATCH cx_bcs INTO DATA(lx_bcs).
    WRITE: / 'Errore:', lx_bcs->get_text( ).
ENDTRY.
\`\`\`
`;