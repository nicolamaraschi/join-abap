export const bapi = {
  "name": "BAPI_BUPA_CREATE_FROM_DATA",
  "description": "BAPI centrale in S/4HANA per creare un Business Partner.",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori",
      "structures": [
        { "name": "Singoli", "type": "Parametri", "fields": [
          { "name": "PARTNERCATEGORY", "desc": "Categoria BP ('1' Persona, '2' Organizzazione, '3' Gruppo).", "mandatory": true },
          { "name": "PARTNERGROUP", "desc": "Raggruppamento (controlla numerazione e viste).", "mandatory": true }
        ]},
        { "name": "CENTRALDATAORGANIZATION", "type": "BAPIBUSISM000_CENTRAL", "fields": [
          { "name": "NAME_ORG1", "desc": "Nome dell'organizzazione (se categoria '2').", "mandatory": true },
          { "name": "SEARCHTERM1", "desc": "Termine di ricerca.", "mandatory": false }
        ]},
        { "name": "CENTRALDATAPERSON", "type": "BAPIBUSISM000_CENTRAL_PERSON", "fields": [
          { "name": "FIRSTNAME", "desc": "Nome (se categoria '1').", "mandatory": true },
          { "name": "LASTNAME", "desc": "Cognome (se categoria '1').", "mandatory": true },
          { "name": "BIRTHDATE", "desc": "Data di nascita.", "mandatory": false }
        ]},
        { "name": "ADDRESSDATA", "type": "BAPIBUSISM000_ADDRESS", "fields": [
          { "name": "COUNTRY", "desc": "Nazione.", "mandatory": true },
          { "name": "CITY", "desc": "Città.", "mandatory": true },
          { "name": "STREET", "desc": "Via e numero civico.", "mandatory": true },
          { "name": "POSTL_COD1", "desc": "Codice di Avviamento Postale.", "mandatory": true },
          { "name": "LANGU", "desc": "Lingua.", "mandatory": false }
        ]}
      ]
    },
    { "title": "Note", "content": "Questa BAPI crea i dati centrali del BP. Per aggiungere ruoli (es. cliente, fornitore) e dati dipendenti (dati società, dati vendite), sono necessarie chiamate successive ad altre BAPI (es. BAPI_BUPA_ROLE_ADD_2). La configurazione del Raggruppamento (PARTNERGROUP) è cruciale. Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": `"----------------------------------------------------------------------
" 10. BAPI: Blocco BAPI_BUPA_... (Creazione Business Partner)
"----------------------------------------------------------------------
  WRITE: / 'Esecuzione Test 10: Creazione Business Partner e dettagli associati'.
  ULINE.

  " Scenario: Creazione di un BP di tipo Persona, con aggiunta di ruolo
  " e dettagli bancari. L'intera sequenza è una singola LUW.

  " Dichiarazione variabili LOCALI per l'intera sequenza
  DATA: lv_id_bp_creato      TYPE bu_partner,
        lt_messaggi_globali_bp TYPE STANDARD TABLE OF bapiret2,
        lv_successo_globale_bp TYPE abap_bool VALUE lc_vero.

  " --- 10.1: BAPI_BUPA_CREATE_FROM_DATA ---
  WRITE: / 'Passo 10.1: Chiamata a BAPI_BUPA_CREATE_FROM_DATA'.
  DATA: ls_dati_centrali_bp  TYPE bapibus1006_central,
        ls_dati_persona_bp   TYPE bapibus1006_central_person,
        ls_dati_indirizzo_bp TYPE bapibus1006_address,
        lv_partner_category  TYPE bapibus1006_head-partn_cat.

  lv_partner_category = '1'. " 1 = Persona

  ls_dati_persona_bp-firstname = 'Mario'.
  ls_dati_persona_bp-lastname = 'Rossi'.
  ls_dati_persona_bp-correspondlanguage = 'I'.
  ls_dati_indirizzo_bp-city = 'Vercelli'.
  ls_dati_indirizzo_bp-postl_cod1 = '13100'.
  ls_dati_indirizzo_bp-street = 'Via BAPI'.
  ls_dati_indirizzo_bp-house_no = '123'.
  ls_dati_indirizzo_bp-country = 'IT'.

  DATA lt_return_create TYPE STANDARD TABLE OF bapiret2.

  CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
    EXPORTING
      partnercategory   = lv_partner_category " *** CORREZIONE: Aggiunto parametro obbligatorio
      centraldata       = ls_dati_centrali_bp
      centraldataperson = ls_dati_persona_bp
      addressdata       = ls_dati_indirizzo_bp
    IMPORTING
      businesspartner   = lv_id_bp_creato
    TABLES
      return            = lt_return_create.

  APPEND LINES OF lt_return_create TO lt_messaggi_globali_bp.

  " Continua solo se il passo precedente non ha prodotto errori gravi
  LOOP AT lt_messaggi_globali_bp INTO DATA(ls_temp_msg) WHERE type CA 'EA'.
    lv_successo_globale_bp = lc_falso.
    EXIT.
  ENDLOOP.

  IF lv_successo_globale_bp = lc_vero AND lv_id_bp_creato IS NOT INITIAL.
    " --- 10.2: BAPI_BUPA_ROLE_ADD_2 ---
    WRITE: / 'Passo 10.2: Chiamata a BAPI_BUPA_ROLE_ADD_2'.
    DATA lt_return_role TYPE STANDARD TABLE OF bapiret2.
    CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
      EXPORTING
        businesspartner             = lv_id_bp_creato
        businesspartnerrolecategory = 'FLCU01' " Esempio: Ruolo Cliente FI
      TABLES
        return                      = lt_return_role.
    APPEND LINES OF lt_return_role TO lt_messaggi_globali_bp.

    " --- 10.3: BAPI_BUPA_BANKDETAIL_ADD (controlla di nuovo il successo) ---
    LOOP AT lt_return_role INTO ls_temp_msg WHERE type CA 'EA'.
      lv_successo_globale_bp = lc_falso.
      EXIT.
    ENDLOOP.

    IF lv_successo_globale_bp = lc_vero.
      WRITE: / 'Passo 10.3: Chiamata a BAPI_BUPA_BANKDETAIL_ADD'.
      DATA(ls_dati_banca) = VALUE bapibus1006_bankdetail(
          bank_ctry = 'IT'
          bank_key  = '03069'
          bank_acct = '000012345678'
      ).
      DATA lt_return_bank TYPE STANDARD TABLE OF bapiret2.
      CALL FUNCTION 'BAPI_BUPA_BANKDETAIL_ADD'
        EXPORTING
          businesspartner = lv_id_bp_creato
          bankdetaildata  = ls_dati_banca
        TABLES
          return          = lt_return_bank.
      APPEND LINES OF lt_return_bank TO lt_messaggi_globali_bp.
    ENDIF.
  ENDIF.

  " --- GESTIONE FINALE COMMIT / ROLLBACK per l'intera sequenza BP ---
  " Verifica finale di tutti i messaggi raccolti
  LOOP AT lt_messaggi_globali_bp INTO ls_temp_msg WHERE type CA 'EA'.
    lv_successo_globale_bp = lc_falso.
    EXIT.
  ENDLOOP.

  IF lv_successo_globale_bp = lc_falso OR lv_id_bp_creato IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE 'Creazione/aggiornamento Business Partner fallita. Eseguito ROLLBACK.' TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    DATA(lv_msg_succ_bp) = |Business Partner { lv_id_bp_creato } e dettagli associati creati con successo.|.
    MESSAGE lv_msg_succ_bp TYPE 'S'.
  ENDIF.

  " Visualizzazione di tutti i messaggi raccolti
  IF lt_messaggi_globali_bp IS NOT INITIAL.
    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING
        it_message = lt_messaggi_globali_bp.
  ENDIF.

  NEW-LINE.
  NEW-LINE.`
};
