export const bapi = {
  "name": "BAPI_INCOMINGINVOICE_CREATE",
  "description": "Registrare Fattura Fornitore (Logistica - MIRO).",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori",
      "structures": [
        { "name": "HEADERDATA", "type": "BAPI_INCINV_CREATE_HEADER", "fields": [
          { "name": "INVOICE_IND", "desc": "Flag: 'X' per Fattura, ' ' per Nota di Credito.", "mandatory": true },
          { "name": "DOC_DATE", "desc": "Data del documento.", "mandatory": true },
          { "name": "PSTNG_DATE", "desc": "Data di registrazione.", "mandatory": true },
          { "name": "COMP_CODE", "desc": "Società.", "mandatory": true },
          { "name": "GROSS_AMOUNT", "desc": "Importo totale lordo della fattura.", "mandatory": true },
          { "name": "CURRENCY", "desc": "Valuta.", "mandatory": true }
        ]},
        { "name": "ITEMDATA", "type": "Tabella di BAPI_INCINV_CREATE_ITEM", "fields": [
          { "name": "INVOICE_DOC_ITEM", "desc": "Numero progressivo della riga fattura.", "mandatory": true },
          { "name": "PO_NUMBER", "desc": "Numero dell'ordine d'acquisto di riferimento.", "mandatory": true },
          { "name": "PO_ITEM", "desc": "Numero posizione dell'OdA.", "mandatory": true },
          { "name": "ITEM_AMOUNT", "desc": "Importo della posizione.", "mandatory": true },
          { "name": "QUANTITY", "desc": "Quantità fatturata.", "mandatory": true },
          { "name": "PO_UNIT", "desc": "Unità di misura della quantità.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "Per la fatturazione basata su entrata merci, è necessario fornire i riferimenti al documento di movimento merci (REF_DOC, REF_DOC_YEAR, REF_DOC_IT). Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": `"----------------------------------------------------------------------
" 2. BAPI: BAPI_INCOMINGINVOICE_CREATE
"----------------------------------------------------------------------
  WRITE: / 'Esecuzione Test 2: BAPI_INCOMINGINVOICE_CREATE'.
  ULINE.

  " Scenario di Esempio: Creazione di una fattura in entrata.
  DATA: ls_testa_fattura          TYPE bapi_incinv_create_header,
        lt_posizioni_fattura      TYPE STANDARD TABLE OF bapi_incinv_create_item,
        lt_messaggi_fattura       TYPE STANDARD TABLE OF bapiret2,
        lv_num_doc_fattura        TYPE belnr_d,
        lv_anno_fiscale_fattura   TYPE gjahr.

  " Popolamento dati di testata
  ls_testa_fattura-doc_type     = 'RE'.
  ls_testa_fattura-comp_code    = '1000'.
  ls_testa_fattura-doc_date     = sy-datum.
  ls_testa_fattura-pstng_date   = sy-datum.
  "ls_testa_fattura-vendor_no    = '100000'.
  ls_testa_fattura-ref_doc_no   = 'ESEMPIO_REF_BAPI'.
  ls_testa_fattura-gross_amount = '1210.00'.
  ls_testa_fattura-currency     = 'EUR'.
  "ls_testa_fattura-tax_amount   = '210.00'.
  ls_testa_fattura-calc_tax_ind = lc_vero.

  " Popolamento dati di posizione
  APPEND VALUE #(
    invoice_doc_item = '000001'
    po_number        = '4500000001'
    po_item          = '00010'
    quantity         = '10.00'
    po_unit          = 'EA'
    item_amount      = '1000.00'
    tax_code         = 'V1'
  ) TO lt_posizioni_fattura.

  " Chiamata alla BAPI
  CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE'
    EXPORTING
      headerdata     = ls_testa_fattura
    IMPORTING
      invoicedocnumber = lv_num_doc_fattura
      fiscalyear     = lv_anno_fiscale_fattura
    TABLES
      itemdata       = lt_posizioni_fattura
      return         = lt_messaggi_fattura.

  " Gestione esito BAPI
  DATA: lv_successo_fattura  TYPE abap_bool,
        lv_testo_msg_fattura TYPE string.

  lv_successo_fattura = lc_vero.
  LOOP AT lt_messaggi_fattura INTO DATA(ls_riga_msg_fattura) WHERE type CA 'EA'.
    lv_successo_fattura = lc_falso.
    lv_testo_msg_fattura = ls_riga_msg_fattura-message.
    EXIT.
  ENDLOOP.

  " Gestione COMMIT / ROLLBACK
  IF lv_successo_fattura = lc_falso.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE 'Creazione fattura in entrata fallita. Eseguito ROLLBACK.' TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    " Eseguo il COMMIT. Eventuali errori di aggiornamento non vengono
    " riportati in sy-subrc ma vanno controllati in SM13.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    DATA(lv_msg_succ_fattura) = |Fattura creata: { lv_num_doc_fattura } / { lv_anno_fiscale_fattura }|.
    MESSAGE lv_msg_succ_fattura TYPE 'S'.
  ENDIF.

  " Visualizzazione messaggi
  IF lt_messaggi_fattura IS NOT INITIAL.
    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING
        it_message = lt_messaggi_fattura.
  ENDIF.

  NEW-LINE.
  NEW-LINE.
`
};
