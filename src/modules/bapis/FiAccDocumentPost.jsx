export const bapi = {
  "name": "BAPI_ACC_DOCUMENT_POST",
  "description": "Questa BAPI è ampiamente utilizzata per la registrazione di diverse tipologie di documenti contabili, inclusi quelli relativi a conti Co.Ge., fornitori e clienti.",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori/Fondamentali",
      "structures": [
        { "name": "DOCUMENTHEADER", "type": "BAPIACHE09", "fields": [
          { "name": "BUS_ACT", "desc": "Transazione Contabile (es. 'RFBU').", "mandatory": true },
          { "name": "USERNAME", "desc": "Utente che esegue la registrazione.", "mandatory": true },
          { "name": "COMP_CODE", "desc": "Società.", "mandatory": true },
          { "name": "DOC_DATE", "desc": "Data del documento originale.", "mandatory": true },
          { "name": "PSTNG_DATE", "desc": "Data di registrazione.", "mandatory": true },
          { "name": "DOC_TYPE", "desc": "Classifica il documento contabile (es. 'SA').", "mandatory": true },
          { "name": "REF_DOC_NO", "desc": "Numero del documento esterno di riferimento.", "mandatory": false }
        ]},
        { "name": "ACCOUNTGL", "type": "Tabella di BAPIACGL09", "fields": [
          { "name": "ITEMNO_ACC", "desc": "Numero progressivo della riga.", "mandatory": true },
          { "name": "GL_ACCOUNT", "desc": "Conto Co.Ge. da movimentare.", "mandatory": true },
          { "name": "COSTCENTER", "desc": "Centro di Costo (se il conto lo richiede).", "mandatory": false },
          { "name": "PROFIT_CTR", "desc": "Profit Center (se il conto lo richiede).", "mandatory": false },
          { "name": "ITEMNO_TAX", "desc": "Collega la riga di costo/ricavo alla riga IVA.", "mandatory": false }
        ]},
        { "name": "ACCOUNTPAYABLE", "type": "Tabella di BAPIACAP09", "fields": [
          { "name": "ITEMNO_ACC", "desc": "Numero progressivo della riga.", "mandatory": true },
          { "name": "VENDOR_NO", "desc": "Codice del fornitore.", "mandatory": true },
          { "name": "BLINE_DATE", "desc": "Data base per calcolo scadenza.", "mandatory": false }
        ]},
        { "name": "ACCOUNTRECEIVABLE", "type": "Tabella di BAPIACAR09", "fields": [
          { "name": "ITEMNO_ACC", "desc": "Numero progressivo della riga.", "mandatory": true },
          { "name": "CUSTOMER", "desc": "Codice del cliente.", "mandatory": true },
          { "name": "BLINE_DATE", "desc": "Data base per calcolo scadenza.", "mandatory": false }
        ]},
        { "name": "ACCOUNTTAX", "type": "Tabella di BAPIACTX09", "fields": [
          { "name": "ITEMNO_ACC", "desc": "Numero progressivo della riga IVA.", "mandatory": true },
          { "name": "GL_ACCOUNT", "desc": "Conto Co.Ge. per la registrazione IVA.", "mandatory": true },
          { "name": "TAX_CODE", "desc": "Codice IVA.", "mandatory": true },
          { "name": "COND_KEY", "desc": "Chiave condizione per l'IVA.", "mandatory": false },
          { "name": "TAXJURCODE", "desc": "Codice giurisdizione fiscale (se usato).", "mandatory": false }
        ]},
        { "name": "CURRENCYAMOUNT", "type": "Tabella di BAPIACCR09", "fields": [
          { "name": "ITEMNO_ACC", "desc": "Collega l'importo alla riga del documento.", "mandatory": true },
          { "name": "CURR_TYPE", "desc": "Tipo di valuta (es. '00', '10').", "mandatory": true },
          { "name": "CURRENCY", "desc": "La valuta dell'importo.", "mandatory": true },
          { "name": "AMT_DOCCUR", "desc": "L'importo nella valuta del documento (positivo per Dare, negativo per Avere).", "mandatory": true },
          { "name": "AMT_BASE", "desc": "Base imponibile IVA (per le righe IVA).", "mandatory": false }
        ]}
      ]
    },
    { "title": "Note", "content": "È best practice utilizzare BAPI_ACC_DOCUMENT_CHECK prima della registrazione per validare i dati. È indispensabile eseguire BAPI_TRANSACTION_COMMIT per salvare permanentemente il documento. L'obbligatorietà di molti campi dipende fortemente dallo scenario contabile e dalla configurazione del sistema." }
  ],
  "content": `"&---------------------------------------------------------------------*
"&      Form  f_testa_acc_doc_post
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_ACC_DOCUMENT_POST
"----------------------------------------------------------------------
FORM f_testa_acc_doc_post.
  DATA: ls_doc_header       TYPE bapiache09,
        lt_accountgl        TYPE STANDARD TABLE OF bapiacgl09,
        ls_accountgl        TYPE bapiacgl09,
        lt_currencyamount   TYPE STANDARD TABLE OF bapiaccr09,
        ls_currencyamount   TYPE bapiaccr09,
        lt_return_bapiacc   TYPE STANDARD TABLE OF bapiret2. " BAPI_ACC_DOCUMENT_POST usa BAPIRET2 TABLES

  CLEAR: gs_return.            " Reset della struttura di ritorno generica
  CLEAR: lt_return_bapiacc.    " Reset della tabella di ritorno specifica per questa BAPI

  " Dati di test per BAPI_ACC_DOCUMENT_POST
  ls_doc_header-bus_act   = 'RFBU'.
  ls_doc_header-username  = sy-uname.
  ls_doc_header-comp_code = 'TEST'. " Sostituire con codice società valido
  ls_doc_header-doc_date  = sy-datum.
  ls_doc_header-pstng_date = sy-datum.
  ls_doc_header-doc_type  = 'SA'.

  CLEAR ls_accountgl.
  ls_accountgl-itemno_acc = 1.
  ls_accountgl-gl_account = '0000113100'. " Sostituire con conto C/G valido
  APPEND ls_accountgl TO lt_accountgl.

  CLEAR ls_accountgl.
  ls_accountgl-itemno_acc = 2.
  ls_accountgl-gl_account = '0000113100'. " Sostituire con conto C/G valido
  APPEND ls_accountgl TO lt_accountgl.

  CLEAR ls_currencyamount.
  ls_currencyamount-itemno_acc = 1.
  ls_currencyamount-currency   = 'EUR'.
  ls_currencyamount-amt_doccur = '100.00'.
  APPEND ls_currencyamount TO lt_currencyamount.

  CLEAR ls_currencyamount.
  ls_currencyamount-itemno_acc = 2.
  ls_currencyamount-currency   = 'EUR'.
  ls_currencyamount-amt_doccur = '-100.00'.
  APPEND ls_currencyamount TO lt_currencyamount.

  " Chiamata alla BAPI_ACC_DOCUMENT_POST
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader = ls_doc_header
    TABLES
      accountgl      = lt_accountgl
      currencyamount = lt_currencyamount
      return         = lt_return_bapiacc. " Questa BAPI usa TABLES RETURN di tipo BAPIRET2

  " Per visualizzare il risultato di BAPI_ACC_DOCUMENT_POST nella FORM generica,
  " trasferiamo il primo messaggio da lt_return_bapiacc a gs_return.
  READ TABLE lt_return_bapiacc INTO gs_return INDEX 1.
  IF sy-subrc <> 0.
    gs_return-message = 'Nessun messaggio significativo restituito da BAPI_ACC_DOCUMENT_POST.'.
    gs_return-type = 'S'. " Successo predefinito se nessun messaggio
  ENDIF.

  PERFORM f_scrivi_esito USING 'BAPI_ACC_DOCUMENT_POST'.
ENDFORM.`
};
