export const bapi = {
  "name": "BAPI_OUTB_DELIVERY_CREATE_SLS",
  "description": "Utilizzata per creare consegne in uscita con riferimento a posizioni di ordini di vendita.",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori",
      "structures": [
        { "name": "SHIP_POINT", "type": "Singolo", "fields": [
          { "name": "SHIP_POINT", "desc": "Punto di spedizione. Sebbene il sistema possa determinarlo, specificarlo fornisce un controllo esplicito.", "mandatory": false }
        ]},
        { "name": "DUE_DATE", "type": "Singolo", "fields": [
          { "name": "DUE_DATE", "desc": "Data di creazione della consegna (spesso SY-DATUM, la data corrente).", "mandatory": true }
        ]},
        { "name": "SALES_ORDER_ITEMS", "type": "Tabella", "fields": [
          { "name": "REF_DOC", "desc": "Numero dell'ordine di vendita di riferimento.", "mandatory": true },
          { "name": "REF_ITEM", "desc": "Numero della posizione dell'ordine di vendita di riferimento.", "mandatory": true },
          { "name": "DLV_QTY", "desc": "Quantità da consegnare.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "Non è progettata per la consegna di massa e richiede un COMMIT WORK." }
  ],
  "content": `"&---------------------------------------------------------------------*
"&      Form  f_testa_outb_deliv_create
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_OUTB_DELIVERY_CREATE_SLS - Creare Consegne in Uscita
"----------------------------------------------------------------------
FORM f_testa_outb_deliv_create. " Accorciato da f_testa_outb_delivery_create_sls
  DATA: lv_ship_point       TYPE vstel, " Correzione: Tipo VSTEL per punto di spedizione
        ld_due_date         TYPE sy-datum,
        lt_sales_order_items TYPE STANDARD TABLE OF bapidlvreftosalesorder, " Correzione: Tipo BAPIDLVREFTOSALESORDER
        ls_sales_order_items TYPE bapidlvreftosalesorder, " Correzione: Tipo BAPIDLVREFTOSALESORDER
        lt_created_items    TYPE STANDARD TABLE OF bapidlvitemcreated, " Correzione: Tipo BAPIDLVITEMCREATED
        lt_return_delivery  TYPE STANDARD TABLE OF bapiret2.

  CLEAR: gs_return.
  CLEAR: lt_return_delivery.
  CLEAR: lt_sales_order_items.
  CLEAR: lt_created_items.

  " Campi obbligatori
  lv_ship_point = '1000'. " Punto di spedizione (sostituire)
  ld_due_date   = sy-datum.

  ls_sales_order_items-ref_doc  = '0000000000'. " Numero Ordine Vendita (sostituire)
  ls_sales_order_items-ref_item = 10.         " Posizione Ordine Vendita (sostituire)
  ls_sales_order_items-dlv_qty  = 1.            " Quantità da consegnare
  ls_sales_order_items-sales_unit = 'ST'.       " Unità di misura (obbligatorio con DLV_QTY)
  APPEND ls_sales_order_items TO lt_sales_order_items.

  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
    EXPORTING
      ship_point          = lv_ship_point
      due_date            = ld_due_date
    TABLES
      sales_order_items   = lt_sales_order_items
      created_items       = lt_created_items
      return              = lt_return_delivery.

  READ TABLE lt_return_delivery INTO gs_return INDEX 1.
  IF sy-subrc <> 0.
    gs_return-message = 'Nessun messaggio significativo restituito da BAPI_OUTB_DELIVERY_CREATE_SLS.'.
    gs_return-type = 'S'.
  ENDIF.

  PERFORM f_scrivi_esito USING 'BAPI_OUTB_DELIVERY_CREATE_SLS'.

  " Richiede BAPI_TRANSACTION_COMMIT
  IF gs_return-type CA 'EA'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE 'Rollback effettuato per BAPI_OUTB_DELIVERY_CREATE_SLS.' TYPE 'I'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
    MESSAGE 'Commit effettuato per BAPI_OUTB_DELIVERY_CREATE_SLS.' TYPE 'I'.
  ENDIF.
ENDFORM.`
};
