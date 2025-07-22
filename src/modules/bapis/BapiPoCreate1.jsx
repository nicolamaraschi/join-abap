export const bapi = {
  "name": "BAPI_PO_CREATE1",
  "description": "Crea un Ordine d'Acquisto.",
  "details": [],
  "content": `
FORM f_test_bapi_crea_oda.
  " Scopo: Creare un Ordine d'Acquisto.
  DATA: ls_testata_oda   TYPE bapimepoheader,
        ls_testata_odax  TYPE bapimepoheaderx,
        lv_num_oda       TYPE bapimepoheader-po_number,
        lt_ritorno       TYPE STANDARD TABLE OF BAPIRET2,
        lt_posizioni     TYPE STANDARD TABLE OF bapimepoitem,
        ls_posizione     TYPE bapimepoitem,
        lt_posizionix    TYPE STANDARD TABLE OF bapimepoitemx,
        ls_posizionex    TYPE bapimepoitemx,
        lt_imputazioni   TYPE STANDARD TABLE OF bapimepoaccount,
        ls_imputazione   TYPE bapimepoaccount,
        lt_imputazionix  TYPE STANDARD TABLE OF bapimepoaccountx,
        ls_imputazionex  TYPE bapimepoaccountx,
        lt_schedulazioni TYPE STANDARD TABLE OF bapimeposchedule,
        ls_schedulazione TYPE bapimeposchedule,
        lt_schedulazionix TYPE STANDARD TABLE OF bapimeposchedulx,
        ls_schedulazionex TYPE bapimeposchedulx,
        lv_messaggio     TYPE string,
        lv_errore        TYPE abap_bool.

  WRITE: / 'Test BAPI: BAPI_PO_CREATE1'.

  " 1. Dati di Testata (SOSTITUIRE con dati validi)
  ls_testata_oda-comp_code  = '1000'.
  ls_testata_oda-doc_type   = 'NB'.
  ls_testata_oda-vendor     = '0000100000'.
  ls_testata_oda-purch_org  = '1000'.
  ls_testata_oda-pur_group  = '001'.
  ls_testata_odax-comp_code = 'X'.
  ls_testata_odax-doc_type  = 'X'.
  ls_testata_odax-vendor    = 'X'.
  ls_testata_odax-purch_org = 'X'.
  ls_testata_odax-pur_group = 'X'.

  " 2. Dati di Posizione
  ls_posizione-po_item   = '00010'.
  ls_posizione-material  = 'MATERIALE_01'.
  ls_posizione-plant     = '1000'.
  ls_posizione-quantity  = '5'.
  ls_posizione-po_unit   = 'PZ'.
  APPEND ls_posizione TO lt_posizioni.
  ls_posizionex-po_item   = '00010'.
  ls_posizionex-po_itemx  = 'X'.
  ls_posizionex-material  = 'X'.
  ls_posizionex-plant     = 'X'.
  ls_posizionex-quantity  = 'X'.
  ls_posizionex-po_unit   = 'X'.
  APPEND ls_posizionex TO lt_posizionix.

  " 3. Dati di Imputazione
  ls_imputazione-po_item    = '00010'.
  ls_imputazione-costcenter = 'CENTRO_COSTO_1'.
  APPEND ls_imputazione TO lt_imputazioni.
  ls_imputazionex-po_item    = '00010'.
  ls_imputazionex-po_itemx   = 'X'.
  ls_imputazionex-costcenter = 'X'.
  APPEND ls_imputazionex TO lt_imputazionix.

  " 4. Dati di Schedulazione
  ls_schedulazione-po_item    = '00010'.
  ls_schedulazione-sched_line = '0001'.
  ls_schedulazione-delivery_date = sy-datum + 14.
  ls_schedulazione-quantity   = '5'.
  APPEND ls_schedulazione TO lt_schedulazioni.
  ls_schedulazionex-po_item    = '00010'.
  ls_schedulazionex-sched_line = '0001'.
  ls_schedulazionex-po_itemx   = 'X'.
  ls_schedulazionex-delivery_date = 'X'.
  ls_schedulazionex-quantity   = 'X'.
  APPEND ls_schedulazionex TO lt_schedulazionix.

  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader        = ls_testata_oda
      poheaderx       = ls_testata_odax
    IMPORTING
      exppurchaseorder = lv_num_oda
    TABLES
      return          = lt_ritorno
      poitem          = lt_posizioni
      poitemx         = lt_posizionix
      poaccount       = lt_imputazioni
      poaccountx      = lt_imputazionix
      poschedule      = lt_schedulazioni
      poschedulex     = lt_schedulazionix.

  IF lv_errore = abap_true.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    WRITE: / 'Errore creazione ODA:', lv_messaggio.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
    WRITE: / 'Ordine dacquisto creato:', lv_num_oda.
  ENDIF.
ENDFORM.
`
};
