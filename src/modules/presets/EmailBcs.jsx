export const content = `*----------------------------------------------------------------------*
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

ENDFORM.`;