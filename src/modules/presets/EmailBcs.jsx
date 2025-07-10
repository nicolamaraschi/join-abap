export const content = [
  {
    title: 'Esempio 1: FORM generica per invio email',
    code: `*----------------------------------------------------------------------*
* FORM GENERICA PER INVIO EMAIL
*----------------------------------------------------------------------*
* Scopo:
* Invia un'email con un singolo allegato di testo (es. TXT, CSV).
* Questa FORM è un template generico pronto all'uso.
*
* Come si usa:
* 1. Dichiarare nel proprio programma le variabili per i parametri.
* 2. Riempire le variabili con i dati (destinatari, oggetto, etc.).
* 3. Chiamare questa FORM con:
* PERFORM f_send_email_generic
* USING ...
* CHANGING ...
*
* Parametri di Ingresso (USING):
* - iv_subject: (TYPE so_obj_des) L'oggetto dell'email.
* - it_body: (TYPE bcsy_text) Tabella con il testo del corpo.
* - it_recipients: (TYPE STANDARD TABLE OF ad_smtpadr) Tabella
* con gli indirizzi email dei destinatari.
* - it_attachment_content: (TYPE STANDARD TABLE OF string) Tabella
* con il contenuto dell'allegato.
* - iv_attachment_name: (TYPE string) Nome del file (es. "report.csv").
* - iv_attachment_type: (TYPE so_obj_tp) Tipo/estensione del file.
*
* Parametri di Uscita (CHANGING):
* - cv_success: (TYPE abap_bool) Vale 'X' se l'invio ha successo.
*----------------------------------------------------------------------*
FORM f_send_email_generic
  USING
    VALUE(iv_subject)            TYPE so_obj_des
    it_body                      TYPE bcsy_text
    it_recipients                TYPE STANDARD TABLE OF ad_smtpadr
    it_attachment_content        TYPE STANDARD TABLE OF string
    VALUE(iv_attachment_name)    TYPE string
    VALUE(iv_attachment_type)    TYPE so_obj_tp
  CHANGING
    cv_success                   TYPE abap_bool.

  " Inizializzazione del parametro di uscita
  CLEAR cv_success.

  " Controllo preliminare: esce se non ci sono destinatari
  IF it_recipients IS INITIAL.
    RETURN.
  ENDIF.

  DATA: lo_bcs       TYPE REF TO cl_bcs,
        lo_document  TYPE REF TO cl_document_bcs,
        lo_recipient TYPE REF TO if_recipient_bcs.

  TRY.
      " 1. Crea la richiesta di invio
      lo_bcs = cl_bcs=>create_persistent( ).

      " 2. Crea il documento (corpo e oggetto)
      lo_document = cl_document_bcs=>create_document(
        i_type    = 'RAW'
        i_text    = it_body
        i_subject = iv_subject ).

      " 3. Aggiunge l'allegato, se fornito
      IF it_attachment_content IS NOT INITIAL.
        DATA(lv_content_string) = concat_lines_of( table = it_attachment_content sep = cl_abap_char_utilities=>cr_lf ).
        DATA(lt_content_bin) = cl_bcs_convert=>string_to_solix( iv_string = lv_content_string ).
        DATA(lv_att_size) = xstrlen( cl_bcs_convert=>solix_to_xstring( lt_content_bin ) ).

        lo_document->add_attachment(
          i_attachment_type    = iv_attachment_type
          i_attachment_subject = iv_attachment_name
          i_attachment_size    = lv_att_size
          i_att_content_hex    = lt_content_bin ).
      ENDIF.

      " 4. Associa il documento alla richiesta di invio
      lo_bcs->set_document( lo_document ).

      " 5. Aggiunge tutti i destinatari
      LOOP AT it_recipients INTO DATA(lv_recipient_addr).
        lo_recipient = cl_cam_address_bcs=>create_internet_address( lv_recipient_addr ).
        lo_bcs->add_recipient( i_recipient = lo_recipient ).
      ENDLOOP.

      " 6. Invia l'email
      IF lo_bcs->send( i_with_error_screen = 'X' ) = abap_true.
        cv_success = abap_true.
        COMMIT WORK.
      ENDIF.

    CATCH cx_bcs.
      " In caso di errore, cv_success resta vuoto ('')
      RETURN.
  ENDTRY.

ENDFORM.`
  },
  {
    title: 'Esempio 2: Inserisci qui il titolo del secondo esempio',
    code: `
    REPORT z_email_finale_completo.

"----------------------------------------------------------------------
" SEZIONE 1: DEFINIZIONI TIPI
" Descrizione: Definiamo delle strutture dati personalizzate per gestire
" in modo flessibile i destinatari e gli allegati.
"----------------------------------------------------------------------
TYPES: BEGIN OF ty_s_recipient,
         email_address  TYPE ad_smtpadr,  " Indirizzo email del destinatario
         recipient_type TYPE c LENGTH 1,  " Tipo: 'A'=To, 'B'=CC, 'C'=BCC
       END OF ty_s_recipient.
TYPES: ty_t_recipient TYPE STANDARD TABLE OF ty_s_recipient WITH EMPTY KEY.

TYPES: BEGIN OF ty_s_attachment,
         content TYPE solix_tab,       " Contenuto binario del file
         name    TYPE so_obj_des,      " Nome file (es. 'report.csv')
         type    TYPE so_obj_tp,       " Estensione (es. 'CSV')
       END OF ty_s_attachment.
TYPES: ty_t_attachment TYPE STANDARD TABLE OF ty_s_attachment WITH EMPTY KEY.


"----------------------------------------------------------------------
" SEZIONE 2: SCHERMATA DI SELEZIONE
" Descrizione: Un solo parametro per specificare, se si desidera,
" un Ordine di Vendita a cui collegare l'email inviata.
"----------------------------------------------------------------------
PARAMETERS: p_vbeln TYPE vbeln_va. " Opzionale: Ordine a cui collegare l'email


"----------------------------------------------------------------------
" SEZIONE 3: LOGICA PRINCIPALE
"----------------------------------------------------------------------
START-OF-SELECTION.

  " --- Dichiarazione delle variabili locali ---
  " Queste variabili conterranno tutte le informazioni per l'email.
  DATA: lv_subject      TYPE so_obj_des,
        lt_body         TYPE bcsy_text,
        lv_body_type    TYPE so_obj_tp,
        lt_recipients   TYPE ty_t_recipient,
        lt_attachments  TYPE ty_t_attachment,
        lv_priority     TYPE c LENGTH 1,
        lv_reply_to     TYPE ad_smtpadr,
        lv_success      TYPE abap_bool.

  " Work Area (strutture singole) per popolare le tabelle
  DATA: ls_recipient  LIKE LINE OF lt_recipients,
        ls_attachment LIKE LINE OF lt_attachments.

  " --- Preparazione Dati di Esempio ---
  " In un programma reale, questi dati proverrebbero da altre logiche.
  lv_subject = |Email di test collegata all'ordine { p_vbeln }|.
  APPEND '<html><body><h2>Report Definitivo</h2><p>Questo è il template finale e calibrato.</p></body></html>' TO lt_body.
  lv_body_type = 'HTM'. " Specifichiamo che il corpo è HTML

  " Popoliamo la tabella destinatari con il metodo classico
  CLEAR ls_recipient.
  ls_recipient-email_address = 'destinatario.a@esempio.com'.
  ls_recipient-recipient_type = 'A'. " 'A' = TO (Destinatario principale)
  APPEND ls_recipient TO lt_recipients.

  CLEAR ls_recipient.
  ls_recipient-email_address = 'manager.in.copia@esempio.com'.
  ls_recipient-recipient_type = 'B'. " 'B' = CC (Copia Carbone)
  APPEND ls_recipient TO lt_recipients.

  lv_priority = '1'. " '1' = Priorità Alta
  lv_reply_to = 'noreply@esempio.com'.


  " --- Inizio Blocco Logica di Invio ---
  CLEAR lv_success.
  IF lt_recipients IS INITIAL.
    WRITE: / 'Nessun destinatario, invio annullato.'.
    RETURN.
  ENDIF.

  " Usiamo TRY...CATCH per gestire qualsiasi errore dalle classi BCS
  TRY.
      " Preparazione Allegato 1
      DATA: lv_str_att1 TYPE string, lt_bin_att1 TYPE solix_tab.
      lv_str_att1 = 'Contenuto del primo allegato.'.
      " Convertiamo la stringa in formato binario (necessario per l'allegato)
      CALL METHOD cl_bcs_convert=>string_to_solix
        EXPORTING iv_string = lv_str_att1
        IMPORTING et_solix  = lt_bin_att1.

      " Popoliamo la tabella allegati con il metodo classico
      CLEAR ls_attachment.
      ls_attachment-name    = 'allegato1.txt'.
      ls_attachment-type    = 'TXT'.
      ls_attachment-content = lt_bin_att1.
      APPEND ls_attachment TO lt_attachments.

      " 1. Crea la richiesta di invio principale (l'email "vuota")
      DATA(lo_bcs) = cl_bcs=>create_persistent( ).

      " 2. Crea l'oggetto documento con corpo e oggetto
      DATA(lo_document) = cl_document_bcs=>create_document(
        i_type    = lv_body_type
        i_text    = lt_body
        i_subject = lv_subject ).

      " 3. Aggiunge gli allegati al documento (logica per allegati multipli)
      LOOP AT lt_attachments INTO ls_attachment.
        DATA: lv_att_size TYPE so_obj_len.
        " Calcoliamo la dimensione dell'allegato in una variabile separata,
        " come richiesto dal compilatore.
        lv_att_size = xstrlen( cl_bcs_convert=>solix_to_xstring( ls_attachment-content ) ).
        lo_document->add_attachment(
          i_attachment_type    = ls_attachment-type
          i_attachment_subject = ls_attachment-name
          i_attachment_size    = lv_att_size
          i_att_content_hex    = ls_attachment-content ).
      ENDLOOP.

      " 4. Associa il documento (con corpo, oggetto, allegati) alla richiesta
      lo_bcs->set_document( lo_document ).

      " 5. Aggiunge i destinatari (logica per A, CC, CCN)
      LOOP AT lt_recipients INTO ls_recipient.
        DATA(lo_recipient) = cl_cam_address_bcs=>create_internet_address( ls_recipient-email_address ).
        DATA: lv_copy TYPE abap_bool, lv_blind_copy TYPE abap_bool.
        CLEAR: lv_copy, lv_blind_copy.
        IF ls_recipient-recipient_type = 'B'.
          lv_copy = abap_true.
        ELSEIF ls_recipient-recipient_type = 'C'.
          lv_blind_copy = abap_true.
        ENDIF.
        " Chiamata al metodo con sintassi classica per la massima compatibilità
        CALL METHOD lo_bcs->add_recipient
          EXPORTING
            i_recipient  = lo_recipient
            i_copy       = lv_copy
            i_blind_copy = lv_blind_copy.
      ENDLOOP.

      " 6. Collega l'email a un oggetto SAP (se specificato)
      IF p_vbeln IS NOT INITIAL.
        " Otteniamo l'oggetto 'send_request'
        DATA(lo_send_request) = lo_bcs->send_request.
        " Prepariamo la chiave dell'oggetto di business
        DATA: ls_appl_object  TYPE borident.
        ls_appl_object-objtype = 'BUS2032'. " Tipo Oggetto standard per Ordine di Vendita
        ls_appl_object-objkey  = p_vbeln.
        " Chiamiamo il metodo sull'oggetto corretto con il parametro corretto
        CALL METHOD lo_send_request->create_link
          EXPORTING i_appl_object = ls_appl_object.
      ENDIF.

      " 7. Imposta opzioni avanzate (se fornite)
      IF lv_reply_to IS NOT INITIAL.
        lo_bcs->set_reply_to( i_reply_to = cl_cam_address_bcs=>create_internet_address( lv_reply_to ) ).
      ENDIF.
      IF lv_priority IS NOT INITIAL.
        lo_bcs->set_priority( i_priority = lv_priority ).
      ENDIF.

      " 8. Esegue l'invio finale
      IF lo_bcs->send( i_with_error_screen = 'X' ) = abap_true.
        lv_success = abap_true.
        COMMIT WORK AND WAIT.
      ENDIF.

    CATCH cx_bcs INTO DATA(lx_bcs).
      MESSAGE lx_bcs->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
      lv_success = abap_false.
  ENDTRY.

  " --- Messaggio di feedback all'utente ---
  IF lv_success = abap_true.
    IF p_vbeln IS NOT INITIAL.
      WRITE: / 'Email inviata e collegata con successo all''ordine', p_vbeln.
    ELSE.
      WRITE: / 'Email inviata con successo.'.
    ENDIF.
  ELSE.
    WRITE: / 'Errore durante l''invio dell''email.'.
  ENDIF.`
  }
];