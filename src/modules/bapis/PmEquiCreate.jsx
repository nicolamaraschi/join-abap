export const bapi = {
  "name": "BAPI_EQUI_CREATE",
  "description": "Creazione di nuovi equipment (oggetti tecnici).",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori",
      "structures": [
        { "name": "DATA_GENERAL", "type": "BAPI_ITOB", "fields": [
          { "name": "EQUICATGRY", "desc": "Categoria Equipment (es. 'M' per Macchine). Cruciale.", "mandatory": true },
          { "name": "DESCRIPT", "desc": "Descrizione dell'equipment.", "mandatory": true },
          { "name": "MAINTPLANT", "desc": "Divisione di manutenzione.", "mandatory": true },
          { "name": "OBJECTTYPE", "desc": "Tipo Oggetto Tecnico.", "mandatory": true }
        ]},
        { "name": "DATA_SPECIFIC", "type": "BAPI_ITOB_EQ_ONLY", "fields": [
          { "name": "MANUFACTURER", "desc": "Costruttore.", "mandatory": false },
          { "name": "MODEL_NO", "desc": "Numero Modello.", "mandatory": false },
          { "name": "CONSTYEAR", "desc": "Anno di Costruzione.", "mandatory": false },
          { "name": "START_UP_DATE", "desc": "Data di Messa in Servizio.", "mandatory": false }
        ]}
      ]
    },
    { "title": "Note", "content": "La Categoria Equipment (EQUICATGRY) è fondamentale perché determina la logica e i campi disponibili/obbligatori. Richiede BAPI_TRANSACTION_COMMIT per salvare l'equipment." }
  ],
  "content": `"----------------------------------------------------------------------
" 7. BAPI: BAPI_EQUI_CREATE
"----------------------------------------------------------------------
  WRITE: / 'Esecuzione Test 7: BAPI_EQUI_CREATE'.
  ULINE.

  " Scenario: Creazione di un nuovo equipaggiamento.
  DATA: lv_num_esterno_equi   TYPE sernr VALUE 'SN-EXAMPLE-BAPI-002',
        ls_dati_generali_equi   TYPE bapi_itob,
        ls_dati_specifici_equi  TYPE bapi_itob_eq_only,
        lv_equipment_creato     TYPE equnr,
        ls_ritorno_bapi_equi    TYPE bapiret2.

  " Popolamento dati specifici dell'equipaggiamento
  ls_dati_specifici_equi-equicatgry = 'M'. " M = Macchine
  "ls_dati_specifici_equi-maintplant = '1000'.
  ls_dati_specifici_equi-serialno   = lv_num_esterno_equi.

  " Popolamento dati generali
  "ls_dati_generali_equi-author      = sy-uname.
  ls_dati_generali_equi-objecttype  = '0101'. " Esempio tipo oggetto

  " Chiamata alla BAPI
  CALL FUNCTION 'BAPI_EQUI_CREATE'
    EXPORTING
      data_general  = ls_dati_generali_equi
      data_specific = ls_dati_specifici_equi
    IMPORTING
      equipment     = lv_equipment_creato
      return        = ls_ritorno_bapi_equi.

  " Gestione esito BAPI
  DATA: lv_successo_equi  TYPE abap_bool,
        lt_messaggi_equi  TYPE STANDARD TABLE OF bapiret2,
        lv_testo_msg_equi TYPE string.

  " La BAPI restituisce una struttura singola, la convertiamo in tabella
  APPEND ls_ritorno_bapi_equi TO lt_messaggi_equi.

  lv_successo_equi = lc_vero.
  LOOP AT lt_messaggi_equi INTO DATA(ls_msg_equi) WHERE type CA 'EA'.
    lv_successo_equi = lc_falso.
    lv_testo_msg_equi = ls_msg_equi-message.
    EXIT.
  ENDLOOP.

  " Gestione COMMIT / ROLLBACK
  IF lv_successo_equi = lc_falso.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE 'Creazione equipaggiamento fallita. Eseguito ROLLBACK.' TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    " Eseguo il COMMIT.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    DATA(lv_msg_succ_equi) = |Equipaggiamento creato: { lv_equipment_creato } (Seriale: { lv_num_esterno_equi })|.
    MESSAGE lv_msg_succ_equi TYPE 'S'.
  ENDIF.

  " Visualizzazione messaggi
  IF lt_messaggi_equi IS NOT INITIAL.
    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING
        it_message = lt_messaggi_equi.
  ENDIF.

  NEW-LINE.
  NEW-LINE.
`
};
