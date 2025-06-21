export const bapi = {
  "name": "BAPI_PO_CREATE1",
  "description": "Permette la creazione di ordini d'acquisto (OdA).",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori",
      "structures": [
        { "name": "POHEADER", "type": "BAPIMEPOHEADER", "fields": [
          { "name": "DOC_TYPE", "desc": "Tipo Documento.", "mandatory": true },
          { "name": "VENDOR", "desc": "Numero Fornitore.", "mandatory": true },
          { "name": "PURCH_ORG", "desc": "Organizzazione Acquisti.", "mandatory": true },
          { "name": "PUR_GROUP", "desc": "Gruppo Acquisti.", "mandatory": true },
          { "name": "COMP_CODE", "desc": "Società.", "mandatory": true },
          { "name": "CURRENCY", "desc": "Valuta.", "mandatory": false }
        ]},
        { "name": "POITEM", "type": "Tabella di BAPIMEPOITEM", "fields": [
          { "name": "PO_ITEM", "desc": "Numero Posizione OdA.", "mandatory": true },
          { "name": "MATERIAL", "desc": "Numero Materiale.", "mandatory": false },
          { "name": "PLANT", "desc": "Divisione.", "mandatory": true },
          { "name": "QUANTITY", "desc": "Quantità.", "mandatory": true },
          { "name": "NET_PRICE", "desc": "Prezzo Netto.", "mandatory": true },
          { "name": "ITEM_CAT", "desc": "Tipo Posizione.", "mandatory": false },
          { "name": "ACCTASSCAT", "desc": "Tipo Imputazione.", "mandatory": false }
        ]},
        { "name": "POACCOUNT", "type": "Tabella di BAPIMEPOACCOUNT", "fields": [
          { "name": "PO_ITEM", "desc": "Numero posizione a cui si riferisce l'imputazione.", "mandatory": true },
          { "name": "G_L_ACCT", "desc": "Conto Co.Ge.", "mandatory": true },
          { "name": "COSTCENTER", "desc": "Centro di Costo (se imputazione 'K').", "mandatory": false }
        ]}
      ]
    },
    { "title": "Note", "content": "L'uso delle strutture 'X' (es. POHEADERX, POITEMX, POACCOUNTX) è critico per indicare quali campi si stanno fornendo. Omettere un flag 'X' farà sì che il valore corrispondente venga ignorato. Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": `"&---------------------------------------------------------------------*
"&      Form  f_testa_po_create1
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_PO_CREATE1 - Creazione Ordini d'Acquisto
"----------------------------------------------------------------------
FORM f_testa_po_create1.
  DATA: ls_poheader       TYPE bapimepoheader,  " POHEADER (BAPIMEPOHEADER)
        ls_poheaderx      TYPE bapimepoheaderx, " POHEADERX
        lt_poitem         TYPE STANDARD TABLE OF bapimepoitem,    " Correzione: Tipo BAPIMEPOITEM
        ls_poitem         TYPE bapimepoitem,                      " Correzione: Tipo BAPIMEPOITEM
        lt_poitemx        TYPE STANDARD TABLE OF bapimepoitemx,    " POITEMX (BAPIMEPOITEMX)
        ls_poitemx        TYPE bapimepoitemx,                      " POITEMX (BAPIMEPOITEMX)
        lt_poaccount      TYPE STANDARD TABLE OF bapimepoaccount, " Correzione: Tipo BAPIMEPOACCOUNT
        ls_poaccount      TYPE bapimepoaccount,
        lt_poaccountx     TYPE STANDARD TABLE OF bapimepoaccountx, " Correzione: Tipo BAPIMEPOACCOUNTX
        ls_poaccountx     TYPE bapimepoaccountx,
        lv_ponumber       TYPE ekko-ebeln, " Correzione: Tipo EKKO-EBELN per Numero Ordine Acquisto
        lt_return_po      TYPE STANDARD TABLE OF bapiret2.

  CLEAR: gs_return.
  CLEAR: lt_return_po.
  CLEAR: lt_poitem.
  CLEAR: lt_poitemx.
  CLEAR: lt_poaccount.
  CLEAR: lt_poaccountx.

  " Campi obbligatori POHEADER
  ls_poheader-doc_type  = 'NB'.     " Tipo Documento (es. Ordine standard)
  ls_poheader-vendor    = 'DUMMY'.  " Numero Fornitore (sostituire)
  ls_poheader-purch_org = 'TEST'.   " Organizzazione Acquisti (sostituire)
  ls_poheader-pur_group = '001'.    " Gruppo Acquisti (sostituire)
  ls_poheader-comp_code = 'TEST'.   " Società (sostituire)
  ls_poheader-currency  = 'EUR'.    " Valuta

  " Popolare POHEADERX per indicare i campi valorizzati
  ls_poheaderx-doc_type  = gc_true.
  ls_poheaderx-vendor    = gc_true.
  ls_poheaderx-purch_org = gc_true.
  ls_poheaderx-pur_group = gc_true.
  ls_poheaderx-comp_code = gc_true.
  ls_poheaderx-currency  = gc_true.

  " Campi obbligatori POITEM
  ls_poitem-po_item     = 10.         " Numero Posizione OdA
  ls_poitem-material    = 'DUMMY_MAT'. " Numero Materiale (sostituire)
  ls_poitem-plant       = 'TEST'.     " Divisione (sostituire)
  ls_poitem-quantity    = 10.         " Quantità
  ls_poitem-net_price   = '100.00'.   " Prezzo Netto
  ls_poitem-item_cat    = '0'.        " Tipo Posizione (es. standard)
  ls_poitem-acctasscat  = 'K'.        " Tipo Imputazione (es. K = Centro di Costo)
  APPEND ls_poitem TO lt_poitem.

  " Popolare POITEMX
  ls_poitemx-po_item    = 10.
  ls_poitemx-material   = gc_true.
  ls_poitemx-plant      = gc_true.
  ls_poitemx-quantity   = gc_true.
  ls_poitemx-net_price  = gc_true.
  ls_poitemx-item_cat   = gc_true.
  ls_poitemx-acctasscat = gc_true.
  APPEND ls_poitemx TO lt_poitemx.

  " Campi obbligatori POACCOUNT (solo se item_cat = 'K' ad esempio)
  ls_poaccount-po_item    = 10.         " Numero posizione a cui si riferisce l'imputazione
  ls_poaccount-gl_account = '0000400000'. " Correzione: Campo GL_ACCOUNT (sostituire)
  ls_poaccount-costcenter = 'DUMMY_CC'.  " Correzione: Campo COSTCENTER (sostituire)
  APPEND ls_poaccount TO lt_poaccount.

  " Popolare POACCOUNTX
  ls_poaccountx-po_item    = 10.
  ls_poaccountx-gl_account = gc_true.   " Correzione: Campo GL_ACCOUNT
  ls_poaccountx-costcenter = gc_true.   " Correzione: Campo COSTCENTER
  APPEND ls_poaccountx TO lt_poaccountx.

  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader        = ls_poheader
      poheaderx       = ls_poheaderx
    IMPORTING
      exppurchaseorder = lv_ponumber
    TABLES
      return          = lt_return_po
      poitem          = lt_poitem
      poitemx         = lt_poitemx
      poaccount       = lt_poaccount
      poaccountx      = lt_poaccountx.

  READ TABLE lt_return_po INTO gs_return INDEX 1.
  IF sy-subrc <> 0.
    gs_return-message = 'Nessun messaggio significativo restituito da BAPI_PO_CREATE1.'.
    gs_return-type = 'S'.
  ENDIF.

  PERFORM f_scrivi_esito USING 'BAPI_PO_CREATE1'.
  IF lv_ponumber IS NOT INITIAL.
    WRITE: / 'Ordine d''Acquisto creato:', lv_ponumber.
  ENDIF.

  " Richiede BAPI_TRANSACTION_COMMIT
  IF gs_return-type CA 'EA'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE 'Rollback effettuato per BAPI_PO_CREATE1.' TYPE 'I'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
    MESSAGE 'Commit effettuato per BAPI_PO_CREATE1.' TYPE 'I'.
  ENDIF.
ENDFORM.
`
};
