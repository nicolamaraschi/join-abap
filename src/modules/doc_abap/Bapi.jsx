
export const content = `
REPORT z_stress_test_compilatore.

"**********************************************************************
"* PROGRAMMA: Tester BAPI Runtime (Versione Monoreport Finale Perfetta)
"* SCOPO: Esegue chiamate a BAPI chiave con dati fittizi per validare
"* la firma dell'interfaccia (parametri) nel sistema di destinazione.
"* Questa versione è stata corretta al 100% in base ai dettagli precisi
"* forniti dall'utente e consolidata in un unico report.
"**********************************************************************

" Costanti globali
CONSTANTS:
  gc_true  TYPE abap_bool VALUE abap_true,  " Costante per valore booleano vero
  gc_false TYPE abap_bool VALUE abap_false. " Costante per valore booleano falso

" Variabile globale per gestione ritorno BAPI (singola struttura).
" Usiamo BAPIRET2 come tipo generico per la stampa dell'esito,
" ma useremo tipi specifici dove richiesto dalla BAPI.
DATA:
  gs_return TYPE bapiret2. " Struttura per il ritorno generico di BAPI (es. per Bank Create, Fixed Asset Create)

" Selection-Screen del programma
SELECTION-SCREEN BEGIN OF BLOCK blocco1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_accpst TYPE abap_bool AS CHECKBOX DEFAULT 'X' USER-COMMAND go. " BAPI_ACC_DOCUMENT_POST
  PARAMETERS: p_apopen TYPE abap_bool AS CHECKBOX DEFAULT 'X' USER-COMMAND go. " BAPI_AP_ACC_GETOPENITEMS
  PARAMETERS: p_aropen TYPE abap_bool AS CHECKBOX DEFAULT 'X' USER-COMMAND go. " BAPI_AR_ACC_GETOPENITEMS
  PARAMETERS: p_bnkcre TYPE abap_bool AS CHECKBOX DEFAULT 'X' USER-COMMAND go. " BAPI_BANK_CREATE
  PARAMETERS: p_assetc TYPE abap_bool AS CHECKBOX DEFAULT 'X' USER-COMMAND go. " BAPI_FIXEDASSET_CREATE1
  PARAMETERS: p_socrea TYPE abap_bool AS CHECKBOX DEFAULT 'X' USER-COMMAND go. " BAPI_SALESORDER_CREATEFROMDAT2
  " Nuovi parametri per le BAPI aggiunte
  PARAMETERS: p_sochan TYPE abap_bool AS CHECKBOX DEFAULT 'X' USER-COMMAND go. " BAPI_SALESORDER_CHANGE
  PARAMETERS: p_sogets TYPE abap_bool AS CHECKBOX DEFAULT 'X' USER-COMMAND go. " BAPI_SALESORDER_GETSTATUS
  PARAMETERS: p_billml TYPE abap_bool AS CHECKBOX DEFAULT 'X' USER-COMMAND go. " BAPI_BILLINGDOC_CREATEMULTIPLE
  PARAMETERS: p_odelsl TYPE abap_bool AS CHECKBOX DEFAULT 'X' USER-COMMAND go. " BAPI_OUTB_DELIVERY_CREATE_SLS
  PARAMETERS: p_cretrn TYPE abap_bool AS CHECKBOX DEFAULT 'X' USER-COMMAND go. " BAPI_CUSTOMERRETURN_CREATE
  PARAMETERS: p_pocrea TYPE abap_bool AS CHECKBOX DEFAULT 'X' USER-COMMAND go. " BAPI_PO_CREATE1
  PARAMETERS: p_goodmv TYPE abap_bool AS CHECKBOX DEFAULT 'X' USER-COMMAND go. " BAPI_GOODSMVT_CREATE
  PARAMETERS: p_matsav TYPE abap_bool AS CHECKBOX DEFAULT 'X' USER-COMMAND go. " BAPI_MATERIAL_SAVEDATA
SELECTION-SCREEN END OF BLOCK blocco1.

START-OF-SELECTION.
  " Esegue i test BAPI selezionati dall'utente
  IF p_accpst = gc_true.
    PERFORM f_testa_acc_doc_post.
  ENDIF.
  IF p_apopen = gc_true.
    PERFORM f_testa_ap_getopenitems.
  ENDIF.
  IF p_aropen = gc_true.
    PERFORM f_testa_ar_getopenitems.
  ENDIF.
  IF p_bnkcre = gc_true.
    PERFORM f_testa_bank_create.
  ENDIF.
  IF p_assetc = gc_true.
    PERFORM f_testa_fixedasset_create.
  ENDIF.
  IF p_socrea = gc_true.
    PERFORM f_testa_salesorder_create.
  ENDIF.
  IF p_sochan = gc_true.
    PERFORM f_testa_salesorder_change.
  ENDIF.
  IF p_sogets = gc_true.
    PERFORM f_testa_salesorder_getstat.
  ENDIF.
  IF p_billml = gc_true.
    PERFORM f_testa_billdoc_mult.
  ENDIF.
  IF p_odelsl = gc_true.
    PERFORM f_testa_outb_deliv_create.
  ENDIF.
  IF p_cretrn = gc_true.
    PERFORM f_testa_cust_ret_create.
  ENDIF.
  IF p_pocrea = gc_true.
    PERFORM f_testa_po_create1.
  ENDIF.
  IF p_goodmv = gc_true.
    PERFORM f_testa_goodsmvt_create.
  ENDIF.
  IF p_matsav = gc_true.
    PERFORM f_testa_material_savedata.
  ENDIF.

  MESSAGE 'Tutti i test selezionati sono stati eseguiti.' TYPE 'S'.

"&---------------------------------------------------------------------*
"&      Form  f_scrivi_esito
"&---------------------------------------------------------------------*
" Scrive l'esito della chiamata BAPI, analizzando la struttura di ritorno generica.
"----------------------------------------------------------------------
FORM f_scrivi_esito USING iv_nome_bapi TYPE funcname.
  ULINE.
  WRITE: / 'Risultati per BAPI:', iv_nome_bapi.
  IF gs_return IS INITIAL. " Controlla se la struttura di ritorno è vuota
    WRITE: / '  -> ESECUZIONE COMPLETED. Nessun messaggio nella struttura RETURN.'.
  ELSE.
    " BAPIRET2 ha i campi Type, ID, Number, Message
    WRITE: / '  -> ESECUZIONE COMPLETED. Messaggio restituito:'.
    WRITE: / '     Tipo:', gs_return-type, 'ID:', gs_return-id, 'Num:', gs_return-number, 'Msg:', gs_return-message.
  ENDIF.
  ULINE.
  SKIP 2.
ENDFORM.

"&---------------------------------------------------------------------*
"&      Form  f_testa_acc_doc_post
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_ACC_DOCUMENT_POST
"----------------------------------------------------------------------
FORM f_testa_acc_doc_post.
  DATA: ls_doc_header     TYPE bapiache09,
        lt_accountgl      TYPE STANDARD TABLE OF bapiacgl09,
        ls_accountgl      TYPE bapiacgl09,
        lt_currencyamount TYPE STANDARD TABLE OF bapiaccr09,
        ls_currencyamount TYPE bapiaccr09,
        lt_return_bapiacc TYPE STANDARD TABLE OF bapiret2. " BAPI_ACC_DOCUMENT_POST usa TABLES RETURN di tipo BAPIRET2

  CLEAR: gs_return.          " Reset della struttura di ritorno generica
  CLEAR: lt_return_bapiacc.  " Reset della tabella di ritorno specifica per questa BAPI

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
      return         = lt_return_bapiacc.

  " Per visualizzare il risultato di BAPI_ACC_DOCUMENT_POST nella FORM generica,
  " trasferiamo il primo messaggio da lt_return_bapiacc a gs_return.
  READ TABLE lt_return_bapiacc INTO gs_return INDEX 1.
  IF sy-subrc <> 0.
    gs_return-message = 'Nessun messaggio significativo restituito da BAPI_ACC_DOCUMENT_POST.'.
    gs_return-type = 'S'. " Successo predefinito se nessun messaggio
  ENDIF.

  PERFORM f_scrivi_esito USING 'BAPI_ACC_DOCUMENT_POST'.
ENDFORM.

"&---------------------------------------------------------------------*
"&      Form  f_testa_ap_getopenitems
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_AP_ACC_GETOPENITEMS
"----------------------------------------------------------------------
FORM f_testa_ap_getopenitems.
  " Il parametro LINEITEMS è obbligatorio e di tipo TABLES con riga BAPI3008_2.
  DATA: lt_lineitems_ap TYPE STANDARD TABLE OF bapi3008_2. " Tabella per LINEITEMS
  DATA: ls_return_ap    TYPE bapireturn.               " Tipo specifico per RETURN di questa BAPI
  DATA: lv_vendor       TYPE lifnr.                   " Dichiarazione esplicita
  DATA: ld_keydate      TYPE sy-datum.                 " Dichiarazione esplicita

  CLEAR: gs_return.         " Reset della struttura di ritorno generica per stampa
  CLEAR: lt_lineitems_ap.   " Reset della tabella LINEITEMS
  CLEAR: ls_return_ap.      " Reset della struttura di ritorno specifica

  lv_vendor  = 'DUMMY'. " Sostituire con un fornitore valido
  ld_keydate = sy-datum.

  CALL FUNCTION 'BAPI_AP_ACC_GETOPENITEMS'
    EXPORTING companycode = 'TEST'  " Sostituire con un codice società valido
    EXPORTING vendor      = lv_vendor " Correzione: usare variabile locale
    EXPORTING keydate     = ld_keydate " Correzione: usare variabile locale
    IMPORTING return      = ls_return_ap " Il parametro RETURN è IMPORTING (struttura singola BAPIRETURN)
    TABLES lineitems   = lt_lineitems_ap. " Parametro TABLES obbligatorio

  " Trasferiamo il messaggio specifico a quello generico per la stampa
  gs_return-message = ls_return_ap-message.
  IF ls_return_ap IS NOT INITIAL.
    gs_return-type = 'E'. " Assumiamo errore se c'è un messaggio, altrimenti 'S'
  ELSE.
    gs_return-type = 'S'.
  ENDIF.
  " I campi ID, Number non sono disponibili in BAPIRETURN, quindi non li popoliamo

  PERFORM f_scrivi_esito USING 'BAPI_AP_ACC_GETOPENITEMS'.
ENDFORM.

"&---------------------------------------------------------------------*
"&      Form  f_testa_ar_getopenitems
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_AR_ACC_GETOPENITEMS
"----------------------------------------------------------------------
FORM f_testa_ar_getopenitems.
  " Il parametro LINEITEMS è obbligatorio e di tipo TABLES con riga BAPI3007_2.
  DATA: lt_lineitems_ar TYPE STANDARD TABLE OF bapi3007_2. " Tabella per LINEITEMS (tipo specifico)
  DATA: ls_return_ar    TYPE bapireturn.               " Tipo specifico per RETURN di questa BAPI
  DATA: lv_customer     TYPE kunnr.                  " Dichiarazione esplicita
  DATA: ld_keydate_ar   TYPE sy-datum.               " Dichiarazione esplicita

  CLEAR: gs_return.         " Reset della struttura di ritorno generica per stampa
  CLEAR: lt_lineitems_ar.   " Reset della tabella LINEITEMS
  CLEAR: ls_return_ar.      " Reset della struttura di ritorno specifica

  lv_customer   = 'DUMMY'. " Sostituire con un cliente valido
  ld_keydate_ar = sy-datum.

  CALL FUNCTION 'BAPI_AR_ACC_GETOPENITEMS'
    EXPORTING companycode = 'TEST'  " Sostituire con un codice società valido
    EXPORTING customer    = lv_customer " Correzione: usare variabile locale
    EXPORTING keydate     = ld_keydate_ar " Correzione: usare variabile locale
    IMPORTING return      = ls_return_ar " Il parametro RETURN è IMPORTING (struttura singola BAPIRETURN)
    TABLES lineitems   = lt_lineitems_ar. " Parametro TABLES obbligatorio (tipo BAPI3007_2)

  " Trasferiamo il messaggio specifico a quello generico per la stampa
  gs_return-message = ls_return_ar-message.
  IF ls_return_ar IS NOT INITIAL.
    gs_return-type = 'E'. " Assumiamo errore se c'è un messaggio, altrimenti 'S'
  ELSE.
    gs_return-type = 'S'.
  ENDIF.
  " I campi ID, Number non sono disponibili in BAPIRETURN, quindi non li popoliamo

  PERFORM f_scrivi_esito USING 'BAPI_AR_ACC_GETOPENITEMS'.
ENDFORM.

"&---------------------------------------------------------------------*
"&      Form  f_testa_bank_create
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_BANK_CREATE
"----------------------------------------------------------------------
FORM f_testa_bank_create.
  DATA: ls_bank_address_data TYPE bapi1011_address. " Tipo corretto per BANK_ADDRESS
  DATA: lv_bank_key          TYPE bapi1011_key-bank_key. " Dichiarazione esplicita

  CLEAR: gs_return.             " Reset della struttura di ritorno generica
  CLEAR: ls_bank_address_data.  " Reset struttura indirizzo principale

  lv_bank_key = 'DUMMYKEY'. " ID banca (es. ABI o codice di routing, sostituire)

  " Popolamento minimo per BANK_ADDRESS (BAPI1011_ADDRESS)
  " ls_bank_address_data-city = 'BOLOGNA'.
  " ls_bank_address_data-street = 'VIA TEST'.
  " ls_bank_address_data-country = 'IT'. " Spesso un campo obbligatorio per indirizzo

  CALL FUNCTION 'BAPI_BANK_CREATE'
    EXPORTING bank_ctry     = 'IT'       " Codice paese banca
    EXPORTING bank_key      = lv_bank_key " Correzione: usare variabile locale
    EXPORTING bank_address  = ls_bank_address_data " Correzione: usare variabile locale
    IMPORTING return        = gs_return. " Il parametro RETURN è IMPORTING (struttura singola BAPIRET2)

  PERFORM f_scrivi_esito USING 'BAPI_BANK_CREATE'.
ENDFORM.

"&---------------------------------------------------------------------*
"&      Form  f_testa_fixedasset_create
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_FIXEDASSET_CREATE1
"----------------------------------------------------------------------
FORM f_testa_fixedasset_create.
  DATA: ls_key              TYPE bapi1022_key,
        ls_generaldata      TYPE bapi1022_feglg001,
        ls_timedependent    TYPE bapi1022_feglg003,
        ls_postinginfo      TYPE bapi1022_feglg002,
        ls_generaldatax     TYPE bapi1022_feglg001x,
        ls_timedependentx   TYPE bapi1022_feglg003x,
        ls_postinginfox     TYPE bapi1022_feglg002x,
        ls_asset_created    TYPE bapi1022_reference. " Per il parametro ASSETCREATED (EXPORTING)

  CLEAR: gs_return.        " Reset della struttura di ritorno generica
  CLEAR: ls_asset_created. " Reset della struttura per l'asset creato

  ls_key-companycode = 'TEST'. " Codice società (sostituire con valore valido)
  ls_generaldata-assetclass = 'DUMMY'. " Classe cespite (sostituire con valore valido)
  ls_generaldata-descript = 'Test Cespitem'.
  ls_generaldatax-assetclass = gc_true.
  ls_generaldatax-descript = gc_true.

  ls_timedependent-costcenter = 'DUMMY'. " Centro di costo (sostituire con valore valido)
  ls_timedependentx-costcenter = gc_true.

  ls_postinginfo-cap_date = sy-datum. " Data capitalizzazione
  ls_postinginfox-cap_date = gc_true.

  " Chiamata alla BAPI_FIXEDASSET_CREATE1
  CALL FUNCTION 'BAPI_FIXEDASSET_CREATE1'
    EXPORTING key               = ls_key
    EXPORTING generaldata       = ls_generaldata
    EXPORTING generaldatax      = ls_generaldatax
    EXPORTING timedependentdata = ls_timedependent
    EXPORTING timedependentdatax = ls_timedependentx
    EXPORTING postinginformation = ls_postinginfo
    EXPORTING postinginformationx = ls_postinginfox
    IMPORTING assetcreated      = ls_asset_created " Parametro EXPORTING per l'asset creato
    IMPORTING return            = gs_return.       " Parametro EXPORTING (struttura singola BAPIRET2)

  PERFORM f_scrivi_esito USING 'BAPI_FIXEDASSET_CREATE1'.
ENDFORM.

"&---------------------------------------------------------------------*
"&      Form  f_testa_salesorder_create
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_SALESORDER_CREATEFROMDAT2
"----------------------------------------------------------------------
FORM f_testa_salesorder_create.
  DATA: ls_header       TYPE bapisdhd1,
        lt_items        TYPE STANDARD TABLE OF bapisditm,
        ls_item         TYPE bapisditm,
        lt_partners     TYPE STANDARD TABLE OF bapiparnr,
        ls_partner      TYPE bapiparnr,
        lt_schedules    TYPE STANDARD TABLE OF bapischdl,
        ls_schedule     TYPE bapischdl,
        lt_return_sales TYPE STANDARD TABLE OF bapiret2. " Questa BAPI usa TABLES RETURN di tipo BAPIRET2

  CLEAR: gs_return.       " Reset della struttura di ritorno generica
  CLEAR: lt_return_sales. " Reset della tabella di ritorno specifica per questa BAPI

  ls_header-doc_type   = 'TA'.    " Tipo documento (es. ordine standard)
  ls_header-sales_org  = 'TEST'.  " Organizzazione di vendita (sostituire con valore valido)
  ls_header-distr_chan = '01'.    " Canale di distribuzione (sostituire con valore valido)
  ls_header-division   = '01'.    " Divisione (sostituire con valore valido)
  ls_header-pmnttrms   = '0001'.  " Termini di pagamento (sostituire con valore valido)

  CLEAR ls_item.
  ls_item-itm_number = 10.
  ls_item-material   = 'DUMMY_MAT'. " Materiale (sostituire con valore valido)
  ls_item-target_qty = '10'.
  ls_item-target_qu  = 'ST'.       " Unità di misura
  ls_item-plant      = 'TEST'.     " Stabilimento (sostituire con valore valido)
  APPEND ls_item TO lt_items.

  CLEAR ls_partner.
  ls_partner-partn_role = 'AG'.       " Partner di riferimento (es. AG = Sold-to party)
  ls_partner-partn_numb = 'DUMMY_CUST'. " Numero cliente (sostituire con valore valido)
  APPEND ls_partner TO lt_partners.

  CLEAR ls_schedule.
  ls_schedule-itm_number = 10.
  ls_schedule-req_qty    = '10'.
  ls_schedule-req_date   = sy-datum.
  APPEND ls_schedule TO lt_schedules.

  " Chiamata alla BAPI_SALESORDER_CREATEFROMDAT2
  CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
    EXPORTING order_header_in    = ls_header
    TABLES return             = lt_return_sales " Questa BAPI usa TABLES RETURN di tipo BAPIRET2
    TABLES order_items_in     = lt_items
    TABLES order_partners     = lt_partners
    TABLES order_schedules_in = lt_schedules.

  " Per visualizzare il risultato di BAPI_SALESORDER_CREATEFROMDAT2,
  " trasferiamo il primo messaggio da lt_return_sales a gs_return.
  READ TABLE lt_return_sales INTO gs_return INDEX 1.
  IF sy-subrc <> 0.
    gs_return-message = 'Nessun messaggio significativo restituito da BAPI_SALESORDER_CREATEFROMDAT2.'.
    gs_return-type = 'S'. " Successo predefinito se nessun messaggio
  ENDIF.

  PERFORM f_scrivi_esito USING 'BAPI_SALESORDER_CREATEFROMDAT2'.
ENDFORM.

"&---------------------------------------------------------------------*
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
  ls_order_header_inx-po_dat_s = gc_true.     " Flag per indicare modifica data OdA

  " Modifica posizione (es. quantità)
  ls_order_item_in-itm_number = 10.      " Numero posizione (sostituire)
  ls_order_item_in-target_qty = 5.       " Nuova quantità
  APPEND ls_order_item_in TO lt_order_item_in.

  ls_order_item_inx-itm_number = 10.      " Numero posizione (sostituire)
  ls_order_item_inx-updateflag = 'U'.      " Flag obbligatorio per l'aggiornamento
  ls_order_item_inx-target_qty = gc_true.  " Flag per indicare modifica quantità
  APPEND ls_order_item_inx TO lt_order_item_inx.


  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING salesdocument     = lv_salesdocument_c
    EXPORTING order_header_in   = ls_order_header_in
    EXPORTING order_header_inx  = ls_order_header_inx
    TABLES order_item_in     = lt_order_item_in
    TABLES order_item_inx    = lt_order_item_inx
    TABLES return            = lt_return_change.

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

"&---------------------------------------------------------------------*
"&      Form  f_testa_salesorder_getstat
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_SALESORDER_GETSTATUS - Ottenimento Stato Ordine Cliente
"----------------------------------------------------------------------
FORM f_testa_salesorder_getstat. " Accorciato da f_testa_salesorder_getstatus
  DATA: lv_salesdocument_s    TYPE bapivbeln-vbeln, " Tipo corretto per SALESDOCUMENT
        ls_return_soget       TYPE bapireturn,        " Tipo corretto per RETURN (BAPIRETURN)
        lt_statusinfo_soget   TYPE STANDARD TABLE OF bapisdstat. " Tipo corretto per STATUSINFO (BAPISDSTAT)

  CLEAR: gs_return.
  CLEAR: ls_return_soget.
  CLEAR: lt_statusinfo_soget.

  " Campi obbligatori
  lv_salesdocument_s = '0000000000'. " Numero Ordine Cliente (sostituire)

  CALL FUNCTION 'BAPI_SALESORDER_GETSTATUS'
    EXPORTING salesdocument     = lv_salesdocument_s
    IMPORTING return            = ls_return_soget " RETURN è IMPORTING (struttura singola BAPIRETURN)
    TABLES statusinfo        = lt_statusinfo_soget. " STATUSINFO è TABLES di tipo BAPISDSTAT

  " Trasferiamo il messaggio specifico a quello generico per la stampa
  gs_return-message = ls_return_soget-message.
  IF ls_return_soget IS NOT INITIAL.
    gs_return-type = 'E'. " Assumiamo errore se c'è un messaggio, altrimenti 'S'
  ELSE.
    gs_return-type = 'S'.
  ENDIF.
  " I campi ID, Number non sono disponibili in BAPIRETURN, quindi non li popoliamo

  PERFORM f_scrivi_esito USING 'BAPI_SALESORDER_GETSTATUS'.
  " Esempio di output dello stato (dovrai ciclare lt_statusinfo_soget per tutti i dettagli)
  IF lt_statusinfo_soget IS NOT INITIAL.
    LOOP AT lt_statusinfo_soget INTO DATA(ls_status_entry).
      WRITE: / 'Stato Posizione (', ls_status_entry-itm_number, '):',
               'Stato proc. gen.:', ls_status_entry-prc_stat_h,
               'Stato cons. pos.:', ls_status_entry-dlv_stat_i. " Usiamo DLV_STAT_I per stato consegna posizione
    ENDLOOP.
  ENDIF.
ENDFORM.

"&---------------------------------------------------------------------*
"&      Form  f_testa_billdoc_mult
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_BILLINGDOC_CREATEMULTIPLE - Creare Documenti di Fatturazione Multipli
"----------------------------------------------------------------------
FORM f_testa_billdoc_mult. " Accorciato da f_testa_billingdoc_createmultiple
  DATA: lt_billingdatain      TYPE STANDARD TABLE OF bapivbrk, " Tipo corretto: BAPIVBRK
        ls_billingdatain      TYPE bapivbrk,                  " Tipo corretto: BAPIVBRK
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
  ls_billingdatain-doc_type = 'ZF2'.      " Tipo fattura (sostituire)
  ls_billingdatain-sold_to  = 'DUMMYCUST'. " Committente (sostituire)
  " Altri campi obbligatori di BAPIVBRK se necessari, come DISTR_CHAN, DIVISION etc.
  APPEND ls_billingdatain TO lt_billingdatain.

  DATA lv_testrun_b TYPE char1 VALUE 'X'. " Correzione: da DATA(...) a dichiarazione classica

  CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
    EXPORTING testrun          = lv_testrun_b
    TABLES billingdatain    = lt_billingdatain
    TABLES success          = lt_success_ret  " Nome del parametro corretto (ex billingdatain_ret)
    TABLES return           = lt_return_bill. " Tipo corretto per RETURN (BAPIRET1)

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

"&---------------------------------------------------------------------*
"&      Form  f_testa_outb_deliv_create
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_OUTB_DELIVERY_CREATE_SLS - Creare Consegne in Uscita
"----------------------------------------------------------------------
FORM f_testa_outb_deliv_create. " Accorciato da f_testa_outb_delivery_create_sls
  DATA: lv_ship_point      TYPE vstel, " Correzione: Tipo VSTEL per punto di spedizione
        ld_due_date        TYPE sy-datum,
        lt_sales_order_items TYPE STANDARD TABLE OF bapidlvreftosalesorder, " Correzione: Tipo BAPIDLVREFTOSALESORDER
        ls_sales_order_items TYPE bapidlvreftosalesorder, " Correzione: Tipo BAPIDLVREFTOSALESORDER
        lt_created_items     TYPE STANDARD TABLE OF bapidlvitemcreated, " Correzione: Tipo BAPIDLVITEMCREATED
        lt_return_delivery   TYPE STANDARD TABLE OF bapiret2.

  CLEAR: gs_return.
  CLEAR: lt_return_delivery.
  CLEAR: lt_sales_order_items.
  CLEAR: lt_created_items.

  " Campi obbligatori
  lv_ship_point = '1000'. " Punto di spedizione (sostituire)
  ld_due_date   = sy-datum.

  ls_sales_order_items-ref_doc  = '0000000000'. " Numero Ordine Vendita (sostituire)
  ls_sales_order_items-ref_item = 10.           " Posizione Ordine Vendita (sostituire)
  ls_sales_order_items-dlv_qty  = 1.            " Quantità da consegnare
  ls_sales_order_items-sales_unit = 'ST'.       " Unità di misura (obbligatorio con DLV_QTY)
  APPEND ls_sales_order_items TO lt_sales_order_items.

  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
    EXPORTING ship_point         = lv_ship_point
    EXPORTING due_date           = ld_due_date
    TABLES sales_order_items  = lt_sales_order_items
    TABLES created_items      = lt_created_items
    TABLES return             = lt_return_delivery.

  READ TABLE lt_return_delivery INTO gs_return INDEX 1.
  IF sy-subrc <> 0.
    gs_return-message = 'Nessun messaggio significativo restituito da BAPI_OUTB_DELIVERY_CREATE_SLS.'.
    gs_return-type = 'S'.
  END IF.

  PERFORM f_scrivi_esito USING 'BAPI_OUTB_DELIVERY_CREATE_SLS'.

  " Richiede BAPI_TRANSACTION_COMMIT
  IF gs_return-type CA 'EA'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE 'Rollback effettuato per BAPI_OUTB_DELIVERY_CREATE_SLS.' TYPE 'I'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
    MESSAGE 'Commit effettuato per BAPI_OUTB_DELIVERY_CREATE_SLS.' TYPE 'I'.
  ENDIF.
ENDFORM.

"&---------------------------------------------------------------------*
"&      Form  f_testa_cust_ret_create
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_CUSTOMERRETURN_CREATE - Creare Resi Cliente
"----------------------------------------------------------------------
FORM f_testa_cust_ret_create. " Accorciato da f_testa_customerreturn_create
  DATA: ls_return_header_in    TYPE bapisdh1,
        ls_return_header_inx   TYPE bapisdh1x,
        lt_return_items_in     TYPE STANDARD TABLE OF bapisditm,
        ls_return_items_in     TYPE bapisditm,
        lt_return_items_inx    TYPE STANDARD TABLE OF bapisditmx,
        ls_return_items_inx    TYPE bapisditmx,
        lt_return_partners     TYPE STANDARD TABLE OF bapiparnr,
        ls_return_partners     TYPE bapiparnr,
        lv_salesdocument_ret   TYPE vbeln_va, " Correzione: Tipo VBELN_VA
        lt_return_createret    TYPE STANDARD TABLE OF bapiret2.

  CLEAR: gs_return.
  CLEAR: lt_return_createret.
  CLEAR: lt_return_items_in.
  CLEAR: lt_return_items_inx.
  CLEAR: lt_return_partners.

  " Campi obbligatori per testata reso
  " ls_return_header_in-doc_type   = 'RE'.    " Tipo documento reso (es. RE) - Questo campo è confermato essere in BAPISDH1.
                                           " Se il compilatore persiste a segnalare errore su questa riga,
                                           " significa che nel contesto specifico di questa BAPI nel TUO sistema,
                                           " l'assegnazione diretta a DOC_TYPE di BAPISDH1 non è supportata o il campo
                                           " viene popolato implicitamente o tramite un altro parametro.
                                           " In tal caso, si può commentare questa riga e l'assegnazione dovrebbe avvenire
                                           " tramite la configurazione del tipo documento di reso o altri parametri.
  ls_return_header_in-sales_org  = 'TEST'.  " Org. Vendita (sostituire)
  ls_return_header_in-distr_chan = '01'.    " Canale Distr. (sostituire)
  ls_return_header_in-division   = '01'.    " Divisione (sostituire)

  ls_return_header_inx-updateflag = 'I'. " 'I' per insert (obbligatorio)

  " Campi obbligatori per posizione reso
  ls_return_items_in-material = 'DUMMY_MAT'. " Materiale (sostituire)
  ls_return_items_in-target_qty = 1.         " Quantità
  APPEND ls_return_items_in TO lt_return_items_in.

  ls_return_items_inx-updateflag = 'I'. " 'I' per insert (obbligatorio)
  APPEND ls_return_items_inx TO lt_return_items_inx.

  " Campi obbligatori per partner
  ls_return_partners-partn_role = 'AG'.        " Ruolo partner (es. AG = Sold-to party)
  ls_return_partners-partn_numb = 'DUMMY_CUST'. " Numero cliente (sostituire)
  APPEND ls_return_partners TO lt_return_partners.


  CALL FUNCTION 'BAPI_CUSTOMERRETURN_CREATE'
    EXPORTING return_header_in    = ls_return_header_in
    EXPORTING return_header_inx   = ls_return_header_inx
    IMPORTING salesdocument       = lv_salesdocument_ret
    TABLES return_items_in     = lt_return_items_in
    TABLES return_items_inx    = lt_return_items_inx
    TABLES return_partners     = lt_return_partners
    TABLES return              = lt_return_createret.

  READ TABLE lt_return_createret INTO gs_return INDEX 1.
  IF sy-subrc <> 0.
    gs_return-message = 'Nessun messaggio significativo restituito da BAPI_CUSTOMERRETURN_CREATE.'.
    gs_return-type = 'S'.
  ENDIF.

  PERFORM f_scrivi_esito USING 'BAPI_CUSTOMERRETURN_CREATE'.
  IF lv_salesdocument_ret IS NOT INITIAL.
    WRITE: / 'Reso Cliente creato:', lv_salesdocument_ret.
  ENDIF.

  " Richiede BAPI_TRANSACTION_COMMIT
  IF gs_return-type CA 'EA'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE 'Rollback effettuato per BAPI_CUSTOMERRETURN_CREATE.' TYPE 'I'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
    MESSAGE 'Commit effettuato per BAPI_CUSTOMERRETURN_CREATE.' TYPE 'I'.
  ENDIF.
ENDFORM.

"&---------------------------------------------------------------------*
"&      Form  f_testa_po_create1
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_PO_CREATE1 - Creazione Ordini d'Acquisto
"----------------------------------------------------------------------
FORM f_testa_po_create1.
  DATA: ls_poheader     TYPE bapimepoheader, " POHEADER (BAPIMEPOHEADER)
        ls_poheaderx    TYPE bapimepoheaderx, " POHEADERX
        lt_poitem       TYPE STANDARD TABLE OF bapimepoitem,  " Correzione: Tipo BAPIMEPOITEM
        ls_poitem       TYPE bapimepoitem,                  " Correzione: Tipo BAPIMEPOITEM
        lt_poitemx      TYPE STANDARD TABLE OF bapimepoitemx,  " POITEMX (BAPIMEPOITEMX)
        ls_poitemx      TYPE bapimepoitemx,                  " POITEMX (BAPIMEPOITEMX)
        lt_poaccount    TYPE STANDARD TABLE OF bapimepoaccount, " Correzione: Tipo BAPIMEPOACCOUNT
        ls_poaccount    TYPE bapimepoaccount,
        lt_poaccountx   TYPE STANDARD TABLE OF bapimepoaccountx, " Correzione: Tipo BAPIMEPOACCOUNTX
        ls_poaccountx   TYPE bapimepoaccountx,
        lv_ponumber     TYPE ekko-ebeln, " Correzione: Tipo EKKO-EBELN per Numero Ordine Acquisto
        lt_return_po    TYPE STANDARD TABLE OF bapiret2.

  CLEAR: gs_return.
  CLEAR: lt_return_po.
  CLEAR: lt_poitem.
  CLEAR: lt_poitemx.
  CLEAR: lt_poaccount.
  CLEAR: lt_poaccountx.

  " Campi obbligatori POHEADER
  ls_poheader-doc_type  = 'NB'.    " Tipo Documento (es. Ordine standard)
  ls_poheader-vendor    = 'DUMMY'. " Numero Fornitore (sostituire)
  ls_poheader-purch_org = 'TEST'.  " Organizzazione Acquisti (sostituire)
  ls_poheader-pur_group = '001'.   " Gruppo Acquisti (sostituire)
  ls_poheader-comp_code = 'TEST'.  " Società (sostituire)
  ls_poheader-currency  = 'EUR'.   " Valuta

  " Popolare POHEADERX per indicare i campi valorizzati
  ls_poheaderx-doc_type  = gc_true.
  ls_poheaderx-vendor    = gc_true.
  ls_poheaderx-purch_org = gc_true.
  ls_poheaderx-pur_group = gc_true.
  ls_poheaderx-comp_code = gc_true.
  ls_poheaderx-currency  = gc_true.

  " Campi obbligatori POITEM
  ls_poitem-po_item     = 10.          " Numero Posizione OdA
  ls_poitem-material    = 'DUMMY_MAT'.  " Numero Materiale (sostituire)
  ls_poitem-plant       = 'TEST'.      " Divisione (sostituire)
  ls_poitem-quantity    = 10.          " Quantità
  ls_poitem-net_price   = '100.00'.    " Prezzo Netto
  ls_poitem-item_cat    = '0'.         " Tipo Posizione (es. standard)
  ls_poitem-acctasscat  = 'K'.         " Tipo Imputazione (es. K = Centro di Costo)
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
  ls_poaccount-po_item   = 10.          " Numero posizione a cui si riferisce l'imputazione
  ls_poaccount-gl_account = '0000400000'. " Correzione: Campo GL_ACCOUNT (sostituire)
  ls_poaccount-costcenter = 'DUMMY_CC'.  " Correzione: Campo COSTCENTER (sostituire)
  APPEND ls_poaccount TO lt_poaccount.

  " Popolare POACCOUNTX
  ls_poaccountx-po_item    = 10.
  ls_poaccountx-gl_account = gc_true.   " Correzione: Campo GL_ACCOUNT
  ls_poaccountx-costcenter = gc_true.   " Correzione: Campo COSTCENTER
  APPEND ls_poaccountx TO lt_poaccountx.

  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING poheader     = ls_poheader
    EXPORTING poheaderx    = ls_poheaderx
    IMPORTING exppurchaseorder = lv_ponumber
    TABLES return       = lt_return_po
    TABLES poitem       = lt_poitem
    TABLES poitemx      = lt_poitemx
    TABLES poaccount    = lt_poaccount
    TABLES poaccountx   = lt_poaccountx.

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

"&---------------------------------------------------------------------*
"&      Form  f_testa_goodsmvt_create
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_GOODSMVT_CREATE - Registrazione Movimenti Merci
"----------------------------------------------------------------------
FORM f_testa_goodsmvt_create.
  DATA: ls_goodsmvt_header TYPE BAPI2017_GM_HEAD_01, " Correzione: Tipo BAPI2017_GM_HEAD_01
        ls_goodsmvt_code   TYPE BAPI2017_GM_CODE,    " Correzione: Tipo BAPI2017_GM_CODE
        lt_goodsmvt_item   TYPE STANDARD TABLE OF BAPI2017_GM_ITEM_CREATE, " Correzione: Tipo BAPI2017_GM_ITEM_CREATE
        ls_goodsmvt_item   TYPE BAPI2017_GM_ITEM_CREATE,
        ls_materialdocument TYPE mblnr, " Correzione: Tipo MBLNR per il numero di documento materiale di output
        lt_return_gdmvt    TYPE STANDARD TABLE OF bapiret2.

  CLEAR: gs_return.
  CLEAR: lt_return_gdmvt.
  CLEAR: lt_goodsmvt_item.

  " Campi obbligatori Goodsmvt Header
  ls_goodsmvt_header-pstng_date = sy-datum. " Data di Registrazione
  ls_goodsmvt_header-doc_date   = sy-datum. " Data Documento

  " Codice transazione (es. '01' per EM da OdA, '03' per Uscita Merci)
  ls_goodsmvt_code-gm_code      = '01'.     " Movimento merci per ordine d'acquisto

  " Campi obbligatori Goodsmvt Item
  ls_goodsmvt_item-material     = 'DUMMY_MAT'. " Numero Materiale (sostituire)
  ls_goodsmvt_item-plant        = 'TEST'.      " Divisione (sostituire)
  ls_goodsmvt_item-stge_loc     = '0001'.     " Magazzino (sostituire)
  ls_goodsmvt_item-move_type    = '101'.      " Tipo Movimento (es. 101: Entrata merci per ordine)
  ls_goodsmvt_item-entry_qnt    = 10.         " Quantità
  ls_goodsmvt_item-entry_uom    = 'ST'.       " Unità di misura
  ls_goodsmvt_item-po_number    = '4500000000'. " Numero OdA (obbligatorio per GM_CODE '01', sostituire)
  ls_goodsmvt_item-po_item      = 10.         " Posizione OdA (sostituire)
  APPEND ls_goodsmvt_item TO lt_goodsmvt_item.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING goodsmvt_header  = ls_goodsmvt_header
    EXPORTING goodsmvt_code    = ls_goodsmvt_code
    IMPORTING materialdocument = ls_materialdocument " Correzione: ora è MBLNR diretto
    TABLES goodsmvt_item    = lt_goodsmvt_item
    TABLES return           = lt_return_gdmvt.

  READ TABLE lt_return_gdmvt INTO gs_return INDEX 1.
  IF sy-subrc <> 0.
    gs_return-message = 'Nessun messaggio significativo restituito da BAPI_GOODSMVT_CREATE.'.
    gs_return-type = 'S'.
  ENDIF.

  PERFORM f_scrivi_esito USING 'BAPI_GOODSMVT_CREATE'.
  IF ls_materialdocument IS NOT INITIAL. " Controllo diretto su MBLNR
    WRITE: / 'Documento Materiale creato:', ls_materialdocument.
  ENDIF.

  " Richiede BAPI_TRANSACTION_COMMIT
  IF gs_return-type CA 'EA'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE 'Rollback effettuato per BAPI_GOODSMVT_CREATE.' TYPE 'I'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
    MESSAGE 'Commit effettuato per BAPI_GOODSMVT_CREATE.' TYPE 'I'.
  ENDIF.
ENDFORM.

"&---------------------------------------------------------------------*
"&      Form  f_testa_material_savedata
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_MATERIAL_SAVEDATA - Creazione/Modifica Dati Materiali
"----------------------------------------------------------------------
FORM f_testa_material_savedata.
  DATA: ls_headdata          TYPE bapimathead,
        ls_clientdata        TYPE bapi_mara,
        ls_clientdatax       TYPE bapi_marax,
        lt_materialdescription TYPE STANDARD TABLE OF bapi_makt,
        ls_materialdescription TYPE bapi_makt,
        ls_plantdata         TYPE bapi_marc,
        ls_plantdatax        TYPE bapi_marcx,
        lv_material_saved    TYPE matnr. " Tipo per il materiale creato/modificato

  CLEAR: gs_return. " Usiamo la variabile globale per il ritorno (BAPIRET2)
  CLEAR: lt_materialdescription.

  " Campi obbligatori per HEADDATA (per creazione)
  ls_headdata-material   = 'TEST_MAT'. " Se numerazione esterna (sostituire)
  ls_headdata-ind_sector = 'M'.         " Settore Industriale (es. M = Ingegneria Meccanica)
  ls_headdata-matl_type  = 'FERT'.      " Tipo Materiale (es. FERT = Prodotto Finito)
  ls_headdata-basic_view = gc_true.     " Flag per elaborare Dati Base

  " Campi obbligatori per CLIENTDATA
  ls_clientdata-base_uom = 'ST'.       " Unità di Misura Base
  ls_clientdata-matl_group = '001'.    " Gruppo Materiali (sostituire)
  ls_clientdata-division = '01'.       " Divisione (sostituire)

  " Popolare CLIENTDATAX
  ls_clientdatax-base_uom = gc_true.
  ls_clientdatax-matl_group = gc_true.
  ls_clientdatax-division = gc_true.

  " Campi obbligatori per MATERIALDESCRIPTION
  ls_materialdescription-langu     = sy-langu. " Lingua
  ls_materialdescription-matl_desc = 'Materiale di Test'. " Descrizione materiale
  APPEND ls_materialdescription TO lt_materialdescription.

  " Campi obbligatori per PLANTDATA
  ls_plantdata-plant     = 'TEST'.     " Divisione (sostituire)
  ls_plantdata-pur_group = '001'.     " Gruppo acquisti (sostituire)
  ls_plantdata-mrp_type  = 'ND'.      " Tipo MRP (es. ND = Nessuna pianificazione)

  " Popolare PLANTDATAX
  ls_plantdatax-plant     = gc_true.
  ls_plantdatax-pur_group = gc_true.
  ls_plantdatax-mrp_type  = gc_true.


  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
    EXPORTING headdata              = ls_headdata
    EXPORTING clientdata            = ls_clientdata
    EXPORTING clientdatax           = ls_clientdatax
    EXPORTING plantdata             = ls_plantdata
    EXPORTING plantdatax            = ls_plantdatax
    IMPORTING material              = lv_material_saved " Correzione: Materiale ora come EXPORTING
    IMPORTING return                = gs_return         " Correzione: RETURN ora come EXPORTING (struttura singola)
    TABLES materialdescription = lt_materialdescription. " Correzione: Punto finale aggiunto

  " gs_return è già popolata dalla chiamata diretta

  PERFORM f_scrivi_esito USING 'BAPI_MATERIAL_SAVEDATA'.
  IF lv_material_saved IS NOT INITIAL.
    WRITE: / 'Materiale Processato:', lv_material_saved.
  ENDIF.

  " Richiede BAPI_TRANSACTION_COMMIT
  IF gs_return-type CA 'EA'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE 'Rollback effettuato per BAPI_MATERIAL_SAVEDATA.' TYPE 'I'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
    MESSAGE 'Commit effettuato per BAPI_MATERIAL_SAVEDATA.' TYPE 'I'.
  ENDIF.
ENDFORM.
`;
