export const bapi = {
  "name": "BAPI_SALESORDER_CREATEFROMDAT2",
  "description": "Utilizzata per la creazione di ordini di vendita, richieste di offerta e quotazioni.",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori",
      "structures": [
        { "name": "LOGIC_SWITCH", "type": "BAPISDLS", "fields": [
          { "name": "PRICING", "desc": "Controlla la determinazione prezzi (es. 'B' per nuova determinazione).", "mandatory": false },
          { "name": "ATP_WRKMOD", "desc": "Controlla la verifica ATP.", "mandatory": false }
        ]},
        { "name": "ORDER_HEADER_IN", "type": "BAPISDHD1", "fields": [
          { "name": "DOC_TYPE", "desc": "Tipo Documento di Vendita (es. 'TA').", "mandatory": true },
          { "name": "SALES_ORG", "desc": "Organizzazione Commerciale.", "mandatory": true },
          { "name": "DISTR_CHAN", "desc": "Canale di Distribuzione.", "mandatory": true },
          { "name": "DIVISION", "desc": "Divisione.", "mandatory": true },
          { "name": "PMNTTRMS", "desc": "Condizioni di Pagamento.", "mandatory": true },
          { "name": "REQ_DATE_H", "desc": "Data richiesta consegna (a livello testata).", "mandatory": false }
        ]},
        { "name": "ORDER_ITEMS_IN", "type": "Tabella di BAPISDITM", "fields": [
          { "name": "ITM_NUMBER", "desc": "Numero Posizione del documento SD.", "mandatory": true },
          { "name": "MATERIAL", "desc": "Numero Materiale.", "mandatory": true },
          { "name": "TARGET_QTY", "desc": "Quantità Obiettivo.", "mandatory": true },
          { "name": "TARGET_QU", "desc": "Unità di Misura Obiettivo.", "mandatory": true },
          { "name": "PLANT", "desc": "Divisione fornitrice.", "mandatory": true }
        ]},
        { "name": "ORDER_PARTNERS", "type": "Tabella di BAPIPARNR", "fields": [
          { "name": "PARTN_ROLE", "desc": "Funzione Partner (es. 'AG' per Committente, 'WE' per Destinatario).", "mandatory": true },
          { "name": "PARTN_NUMB", "desc": "Numero Cliente/Partner.", "mandatory": true }
        ]},
        { "name": "ORDER_SCHEDULES_IN", "type": "Tabella di BAPISCHDL", "fields": [
          { "name": "ITM_NUMBER", "desc": "Numero Posizione a cui si riferisce la schedulazione.", "mandatory": true },
          { "name": "REQ_QTY", "desc": "Quantità Richiesta per la data specificata.", "mandatory": true },
          { "name": "REQ_DATE", "desc": "Data Richiesta Consegna per la quantità specificata.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "I campi organizzativi (SALES_ORG, DISTR_CHAN, DIVISION) sono fondamentali. Almeno un partner con funzione 'AG' (Committente) è tipicamente richiesto. È necessario eseguire BAPI_TRANSACTION_COMMIT per salvare l'ordine." }
  ],
  "content": `"&---------------------------------------------------------------------*
"&      Form  f_testa_salesorder_create
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_SALESORDER_CREATEFROMDAT2
"----------------------------------------------------------------------
FORM f_testa_salesorder_create.
  DATA: ls_header           TYPE bapisdhd1,
        lt_items            TYPE STANDARD TABLE OF bapisditm,
        ls_item             TYPE bapisditm,
        lt_partners         TYPE STANDARD TABLE OF bapiparnr,
        ls_partner          TYPE bapiparnr,
        lt_schedules        TYPE STANDARD TABLE OF bapischdl,
        ls_schedule         TYPE bapischdl,
        lt_return_sales     TYPE STANDARD TABLE OF bapiret2. " Questa BAPI usa BAPIRET2 TABLES

  CLEAR: gs_return.          " Reset della struttura di ritorno generica
  CLEAR: lt_return_sales.    " Reset della tabella di ritorno specifica per questa BAPI

  ls_header-doc_type    = 'TA'.     " Tipo documento (es. ordine standard)
  ls_header-sales_org   = 'TEST'.   " Organizzazione di vendita (sostituire con valore valido)
  ls_header-distr_chan  = '01'.     " Canale di distribuzione (sostituire con valore valido)
  ls_header-division    = '01'.     " Divisione (sostituire con valore valido)
  ls_header-pmnttrms    = '0001'.   " Termini di pagamento (sostituire con valore valido)

  CLEAR ls_item.
  ls_item-itm_number = 10.
  ls_item-material   = 'DUMMY_MAT'. " Materiale (sostituire con valore valido)
  ls_item-target_qty = '10'.
  ls_item-target_qu  = 'ST'.       " Unità di misura
  ls_item-plant      = 'TEST'.     " Stabilimento (sostituire con valore valido)
  APPEND ls_item TO lt_items.

  CLEAR ls_partner.
  ls_partner-partn_role = 'AG'.     " Partner di riferimento (es. AG = Sold-to party)
  ls_partner-partn_numb = 'DUMMY_CUST'. " Numero cliente (sostituire con valore valido)
  APPEND ls_partner TO lt_partners.

  CLEAR ls_schedule.
  ls_schedule-itm_number = 10.
  ls_schedule-req_qty    = '10'.
  ls_schedule-req_date   = sy-datum.
  APPEND ls_schedule TO lt_schedules.

  " Chiamata alla BAPI_SALESORDER_CREATEFROMDAT2
  CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
    EXPORTING
      order_header_in    = ls_header
    TABLES
      return             = lt_return_sales " Questa BAPI usa TABLES RETURN di tipo BAPIRET2
      order_items_in     = lt_items
      order_partners     = lt_partners
      order_schedules_in = lt_schedules.

  " Per visualizzare il risultato di BAPI_SALESORDER_CREATEFROMDAT2,
  " trasferiamo il primo messaggio da lt_return_sales a gs_return.
  READ TABLE lt_return_sales INTO gs_return INDEX 1.
  IF sy-subrc <> 0.
    gs_return-message = 'Nessun messaggio significativo restituito da BAPI_SALESORDER_CREATEFROMDAT2.'.
    gs_return-type = 'S'. " Successo predefinito se nessun messaggio
  ENDIF.

  PERFORM f_scrivi_esito USING 'BAPI_SALESORDER_CREATEFROMDAT2'.
ENDFORM.`
};
