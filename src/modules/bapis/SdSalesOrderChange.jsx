export const bapi = {
  "name": "BAPI_SALESORDER_CHANGE",
  "description": "Modificare Ordine Cliente.",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori",
      "structures": [
        { "name": "Import Parameters", "type": "Singoli", "fields": [
          { "name": "SALESDOCUMENT", "desc": "Numero del documento di vendita da modificare.", "mandatory": true }
        ]},
        { "name": "ORDER_HEADER_IN", "type": "BAPISDH1", "fields": [
          { "name": "PO_DAT_S", "desc": "Nuova data dell'ordine d'acquisto del cliente.", "mandatory": false }
        ]},
        { "name": "ORDER_HEADER_INX", "type": "BAPISDH1X", "fields": [
          { "name": "UPDATEFLAG", "desc": "Flag di aggiornamento per la testata (impostare a 'U').", "mandatory": true },
          { "name": "PO_DAT_S", "desc": "Flag per indicare la modifica della data OdA cliente (impostare a 'X').", "mandatory": false }
        ]},
        { "name": "ORDER_ITEM_IN", "type": "Tabella di BAPISDITM", "fields": [
          { "name": "ITM_NUMBER", "desc": "Numero posizione da modificare.", "mandatory": true },
          { "name": "TARGET_QTY", "desc": "Nuova quantità di posizione.", "mandatory": false }
        ]},
        { "name": "ORDER_ITEM_INX", "type": "Tabella di BAPISDITMX", "fields": [
          { "name": "ITM_NUMBER", "desc": "Numero posizione da modificare.", "mandatory": true },
          { "name": "UPDATEFLAG", "desc": "Flag di aggiornamento per la posizione (impostare a 'U').", "mandatory": true },
          { "name": "TARGET_QTY", "desc": "Flag per indicare la modifica della quantità (impostare a 'X').", "mandatory": false }
        ]}
      ]
    },
    { "title": "Note", "content": "L'uso delle strutture 'X' è fondamentale per indicare quali campi si intende modificare. Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": `"&---------------------------------------------------------------------*
"&      Form  f_testa_salesorder_change
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_SALESORDER_CHANGE - Modifica Ordine Cliente
"----------------------------------------------------------------------
FORM f_testa_salesorder_change.
  DATA: lv_salesdocument_c    TYPE vbeln_va, " Correzione: Tipo VBELN_VA per numero documento di vendita
        ls_order_header_in    TYPE bapisdh1,
        ls_order_header_inx   TYPE bapisdh1x,
        lt_order_item_in      TYPE STANDARD TABLE OF bapisditm,
        ls_order_item_in      TYPE bapisditm,
        lt_order_item_inx     TYPE STANDARD TABLE OF bapisditmx,
        ls_order_item_inx     TYPE bapisditmx,
        lt_return_change      TYPE STANDARD TABLE OF bapiret2. " BAPI usa BAPIRET2 TABLES

  CLEAR: gs_return.
  CLEAR: lt_return_change.

  " Campi obbligatori per la modifica
  lv_salesdocument_c = '0000000000'. " Sostituire con un numero d'ordine esistente

  " Modifica testata (es. data ordine d'acquisto)
  ls_order_header_in-po_dat_s = sy-datum + 10. " Nuova data es. tra 10 giorni
  ls_order_header_inx-updateflag = 'U'.       " Flag obbligatorio per l'aggiornamento
  ls_order_header_inx-po_dat_s = gc_true.      " Flag per indicare modifica data OdA

  " Modifica posizione (es. quantità)
  ls_order_item_in-itm_number = 10.         " Numero posizione (sostituire)
  ls_order_item_in-target_qty = 5.          " Nuova quantità
  APPEND ls_order_item_in TO lt_order_item_in.

  ls_order_item_inx-itm_number = 10.         " Numero posizione (sostituire)
  ls_order_item_inx-updateflag = 'U'.         " Flag obbligatorio per l'aggiornamento
  ls_order_item_inx-target_qty = gc_true.     " Flag per indicare modifica quantità
  APPEND ls_order_item_inx TO lt_order_item_inx.


  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      salesdocument     = lv_salesdocument_c
      order_header_in   = ls_order_header_in
      order_header_inx  = ls_order_header_inx
    TABLES
      order_item_in     = lt_order_item_in
      order_item_inx    = lt_order_item_inx
      return            = lt_return_change.

  READ TABLE lt_return_change INTO gs_return INDEX 1.
  IF sy-subrc <> 0.
    gs_return-message = 'Nessun messaggio significativo restituito da BAPI_SALESORDER_CHANGE.'.
    gs_return-type = 'S'.
  ENDIF.

  PERFORM f_scrivi_esito USING 'BAPI_SALESORDER_CHANGE'.

  " Richiede BAPI_TRANSACTION_COMMIT
  IF gs_return-type CA 'EA'. " Se errore o abort
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE 'Rollback effettuato per BAPI_SALESORDER_CHANGE.' TYPE 'I'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
    MESSAGE 'Commit effettuato per BAPI_SALESORDER_CHANGE.' TYPE 'I'.
  ENDIF.
ENDFORM.
`
};
