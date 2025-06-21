export const bapi = {
  "name": "BAPI_BILLINGDOC_CREATEMULTIPLE",
  "description": "Creare Documenti di Fatturazione Multipli.",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori",
      "structures": [
        { "name": "BILLINGDATAIN", "type": "Tabella di BAPIVFKK", "fields": [
          { "name": "REF_DOC", "desc": "Numero del documento di riferimento da fatturare (es. ordine o consegna).", "mandatory": true },
          { "name": "REF_ITEM", "desc": "Numero posizione del documento di riferimento.", "mandatory": false }
        ]},
        { "name": "TESTRUN", "type": "Singolo", "fields": [
          { "name": "TESTRUN", "desc": "Se 'X', esegue una simulazione senza creare documenti.", "mandatory": false }
        ]}
      ]
    },
    { "title": "Note", "content": "Questa BAPI è usata per fatturare documenti esistenti (es. consegne). Per la creazione di fatture da dati esterni, usare BAPI_BILLINGDOC_CREATEFROMDATA. Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": `"&---------------------------------------------------------------------*
"&      Form  f_testa_billdoc_mult
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_BILLINGDOC_CREATEMULTIPLE - Creare Documenti di Fatturazione Multipli
"----------------------------------------------------------------------
FORM f_testa_billdoc_mult. " Accorciato da f_testa_billingdoc_createmultiple
  DATA: lt_billingdatain      TYPE STANDARD TABLE OF bapivbrk, " Tipo corretto: BAPIVBRK
        ls_billingdatain      TYPE bapivbrk,                   " Tipo corretto: BAPIVBRK
        lt_success_ret        TYPE STANDARD TABLE OF bapivbrksuccess, " Tipo corretto per SUCCESS table (ex billingdatain_ret)
        lt_return_bill        TYPE STANDARD TABLE OF bapiret1. " Tipo corretto per RETURN (BAPIRET1)

  CLEAR: gs_return.
  CLEAR: lt_return_bill.
  CLEAR: lt_billingdatain.
  CLEAR: lt_success_ret.

  " Campi obbligatori (questi sono ora campi della struttura BAPIVBRK)
  " NOTA: I campi REF_DOC e REF_ITEM non sono direttamente in BAPIVBRK come nell'esempio precedente.
  " BAPIVBRK è la struttura di testata per una fattura.
  " Se si desidera fatturare documenti di riferimento (ordini/consegne),
  " questi dati devono essere popolati in modo appropriato per BAPI_BILLINGDOC_CREATEMULTIPLE.
  " Esempio basato sulla struttura BAPIVBRK:
  ls_billingdatain-salesorg = 'TEST'.     " Organizzazione commerciale (sostituire)
  ls_billingdatain-bill_date = sy-datum.   " Data documento fatt.
  ls_billingdatain-doc_type = 'ZF2'.     " Tipo fattura (sostituire)
  ls_billingdatain-sold_to  = 'DUMMYCUST'. " Committente (sostituire)
  " Altri campi obbligatori di BAPIVBRK se necessari, come DISTR_CHAN, DIVISION etc.
  APPEND ls_billingdatain TO lt_billingdatain.

  DATA lv_testrun_b TYPE char1 VALUE 'X'. " Correzione: da DATA(...) a dichiarazione classica

  CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
    EXPORTING
      testrun           = lv_testrun_b
    TABLES
      billingdatain     = lt_billingdatain
      success           = lt_success_ret  " Nome del parametro corretto (ex billingdatain_ret)
      return            = lt_return_bill. " Tipo corretto per RETURN (BAPIRET1)

  " Per visualizzare il risultato di BAPI_BILLINGDOC_CREATEMULTIPLE,
  " trasferiamo il primo messaggio da lt_return_bill (BAPIRET1) a gs_return (BAPIRET2).
  READ TABLE lt_return_bill INTO DATA(ls_ret_bill) INDEX 1.
  IF sy-subrc = 0.
    gs_return-type = ls_ret_bill-type.
    gs_return-id = ls_ret_bill-id.
    gs_return-number = ls_ret_bill-number.
    gs_return-message = ls_ret_bill-message.
  ELSE.
    gs_return-message = 'Nessun messaggio significativo restituito da BAPI_BILLINGDOC_CREATEMULTIPLE.'.
    gs_return-type = 'S'.
  ENDIF.

  PERFORM f_scrivi_esito USING 'BAPI_BILLINGDOC_CREATEMULTIPLE'.

  " Richiede BAPI_TRANSACTION_COMMIT
  IF lv_testrun_b = gc_false. " Esegui commit solo se non è un testrun
    IF gs_return-type CA 'EA'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      MESSAGE 'Rollback effettuato per BAPI_BILLINGDOC_CREATEMULTIPLE.' TYPE 'I'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
      MESSAGE 'Commit effettuato per BAPI_BILLINGDOC_CREATEMULTIPLE.' TYPE 'I'.
    ENDIF.
  ELSE.
    MESSAGE 'BAPI_BILLINGDOC_CREATEMULTIPLE eseguita in modalità testrun.' TYPE 'I'.
  ENDIF.
ENDFORM.
`
};
