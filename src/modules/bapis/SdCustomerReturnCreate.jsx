export const bapi = {
  "name": "BAPI_CUSTOMERRETURN_CREATE",
  "description": "Utilizzata per creare documenti di vendita di reso cliente.",
  "details": [
    {
      "title": "Strutture di Importazione Chiave e Campi Obbligatori",
      "structures": [
        { "name": "RETURN_HEADER_IN", "type": "Struttura", "fields": [
          { "name": "DOC_TYPE", "desc": "Dati di testata dell'ordine di reso (es. tipo ordine, area di vendita, cliente).", "mandatory": true }
        ]},
        { "name": "RETURN_HEADER_INX", "type": "Struttura", "fields": [
          { "name": "UPDATEFLAG", "desc": "Flag di aggiornamento per i dati di testata ('I' per insert, 'U' per update).", "mandatory": true }
        ]},
        { "name": "RETURN_ITEMS_IN", "type": "Tabella", "fields": [
          { "name": "MATERIAL", "desc": "Dati di posizione (materiale, quantità, etc.).", "mandatory": true }
        ]},
        { "name": "RETURN_ITEMS_INX", "type": "Tabella", "fields": [
          { "name": "UPDATEFLAG", "desc": "Flag di aggiornamento per i dati di posizione.", "mandatory": true }
        ]},
        { "name": "RETURN_PARTNERS", "type": "Tabella", "fields": [
          { "name": "PARTN_ROLE", "desc": "Informazioni sui partner del reso.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "Confermare i campi obbligatori con la documentazione SE37. Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": `"&---------------------------------------------------------------------*
"&      Form  f_testa_cust_ret_create
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_CUSTOMERRETURN_CREATE - Creare Resi Cliente
"----------------------------------------------------------------------
*FORM f_testa_cust_ret_create.
* DATA: ls_return_header_in   TYPE bapisdh1,
* ls_return_header_inx  TYPE bapisdh1x,
* lt_return_items_in    TYPE STANDARD TABLE OF bapisditm,
* ls_return_items_in    TYPE bapisditm,
* lt_return_items_inx   TYPE STANDARD TABLE OF bapisditmx,
* ls_return_items_inx   TYPE bapisditmx,
* lt_return_partners    TYPE STANDARD TABLE OF bapiparnr,
* ls_return_partners    TYPE bapiparnr,
* lv_salesdocument_ret  TYPE vbeln_va,
* lt_return_createret   TYPE STANDARD TABLE OF bapiret2.
*
* CLEAR: gs_return.
* CLEAR: lt_return_createret.
* CLEAR: lt_return_items_in.
* CLEAR: lt_return_items_inx.
* CLEAR: lt_return_partners.
*
* " Campi per la testata del reso (ls_return_header_in)
* " Il campo DOC_TYPE NON esiste nella struttura BAPISDH1 del tuo sistema.
* " La BAPI dovrebbe determinare il tipo documento (es. 'RE') internamente.
* ls_return_header_in-sales_org   = 'TEST'.   " Org. Vendita (sostituire con valore valido)
* ls_return_header_in-distr_chan  = '01'.     " Canale Distr. (sostituire con valore valido)
* ls_return_header_in-division    = '01'.     " Divisione (sostituire con valore valido)
* ls_return_header_in-purch_no_c  = 'TEST_ORD_CLI'. " Esempio: Riferimento cliente (sostituire)
* ls_return_header_in-req_date_h  = sy-datum + 7. " Esempio: Data di consegna richiesta (sostituire)
* ls_return_header_in-pmnttrms    = '0001'.   " Esempio: Condizioni di pagamento (sostituire)
* ls_return_header_in-sales_off   = '1000'.   " Esempio: Ufficio vendite (sostituire)
* ls_return_header_in-sales_grp   = '001'.    " Esempio: Gruppo vendite (sostituire)
*
* " Popolamento dei flag 'X' nella struttura di controllo (ls_return_header_inx)
* ls_return_header_inx-updateflag   = 'I'.      " 'I' per insert (obbligatorio per nuova creazione)
* ls_return_header_inx-sales_org    = gc_true.
* ls_return_header_inx-distr_chan   = gc_true.
* ls_return_header_inx-division     = gc_true.
* ls_return_header_inx-purch_no_c   = gc_true.
* ls_return_header_inx-req_date_h   = gc_true.
* ls_return_header_inx-pmnttrms     = gc_true.
* ls_return_header_inx-sales_off    = gc_true.
* ls_return_header_inx-sales_grp    = gc_true.
*
* " Campi per la posizione del reso (ls_return_items_in)
* ls_return_items_in-material = 'DUMMY_MAT'. " Materiale (sostituire)
* ls_return_items_in-target_qty = 1.         " Quantità
* ls_return_items_in-plant = 'TEST'.         " Stabilimento (sostituire)
* ls_return_items_in-sales_unit = 'ST'.      " Unità di misura (spesso obbligatoria con quantità)
* APPEND ls_return_items_in TO lt_return_items_in.
*
* " Popolamento dei flag 'X' per la posizione del reso (ls_return_items_inx)
* ls_return_items_inx-updateflag = 'I'.      " 'I' per insert (obbligatorio per nuova creazione)
* ls_return_items_inx-material = gc_true.
* ls_return_items_inx-target_qty = gc_true.
* ls_return_items_inx-plant = gc_true.
* ls_return_items_inx-sales_unit = gc_true.
* APPEND ls_return_items_inx TO lt_return_items_inx.
*
* " Campi per il partner (ls_return_partners)
* ls_return_partners-partn_role = 'AG'.      " Ruolo partner (es. AG = Sold-to party)
* ls_return_partners-partn_numb = 'DUMMY_CUST'. " Numero cliente (sostituire)
* APPEND ls_return_partners TO lt_return_partners.
*
* " Chiamata alla BAPI_CUSTOMERRETURN_CREATE
* CALL FUNCTION 'BAPI_CUSTOMERRETURN_CREATE'
* EXPORTING
* return_header_in    = ls_return_header_in
* return_header_inx   = ls_return_header_inx
* IMPORTING
* salesdocument       = lv_salesdocument_ret
* TABLES
* return_items_in     = lt_return_items_in
* return_items_inx    = lt_return_items_inx
* return_partners     = lt_return_partners
* return              = lt_return_createret.
*
* READ TABLE lt_return_createret INTO gs_return INDEX 1.
* IF sy-subrc <> 0.
* gs_return-message = 'Nessun messaggio significativo restituito da BAPI_CUSTOMERRETURN_CREATE.'.
* gs_return-type = 'S'.
* ENDIF.
*
* PERFORM f_scrivi_esito USING 'BAPI_CUSTOMERRETURN_CREATE'.
* IF lv_salesdocument_ret IS NOT INITIAL.
* WRITE: / 'Reso Cliente creato:', lv_salesdocument_ret.
* ENDIF.
*
* " Gestione COMMIT / ROLLBACK post BAPI
* IF gs_return-type CA 'EA'.
* CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
* MESSAGE 'Rollback effettuato per BAPI_CUSTOMERRETURN_CREATE.' TYPE 'I'.
* ELSE.
* CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
* MESSAGE 'Commit effettuato per BAPI_CUSTOMERRETURN_CREATE.' TYPE 'I'.
* ENDIF.
*ENDFORM.
`
};
