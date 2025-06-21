export const bapi = {
  "name": "BAPI_ALM_ORDER_MAINTAIN",
  "description": "BAPI per creare e modificare ordini di manutenzione (PM) e ordini di servizio (CS).",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori (per Creazione)",
      "structures": [
        { "name": "IT_METHODS", "type": "Tabella di BAPI_ALM_ORDER_METHOD", "fields": [
          { "name": "REFNUMBER", "desc": "Numero di riferimento per collegare il metodo ai dati.", "mandatory": true },
          { "name": "OBJECTTYPE", "desc": "Tipo Oggetto (es. 'HEADER', 'OPERATION', 'SAVE').", "mandatory": true },
          { "name": "METHOD", "desc": "Metodo (es. 'CREATE', 'CHANGE', 'SAVE').", "mandatory": true },
          { "name": "OBJECTKEY", "desc": "Chiave oggetto (es. numero ordine a 12 cifre per modifiche).", "mandatory": true }
        ]},
        { "name": "IT_HEADER", "type": "Tabella di BAPI_ALM_ORDER_HEADER_I", "fields": [
          { "name": "REFNUMBER", "desc": "Riferimento a IT_METHODS.", "mandatory": true },
          { "name": "ORDER_TYPE", "desc": "Tipo Ordine (es. 'PM01').", "mandatory": true },
          { "name": "PLANPLANT", "desc": "Divisione di pianificazione manutenzione.", "mandatory": true },
          { "name": "EQUIPMENT", "desc": "Equipment di riferimento.", "mandatory": false },
          { "name": "FUNCT_LOC", "desc": "Ubicazione Tecnica di riferimento.", "mandatory": false },
          { "name": "SHORT_TEXT", "desc": "Descrizione breve dell'ordine.", "mandatory": true },
          { "name": "MN_WK_CTR", "desc": "Centro di Lavoro Principale.", "mandatory": true }
        ]},
        { "name": "IT_OPERATION", "type": "Tabella di BAPI_ALM_ORDER_OPERATION_I", "fields": [
          { "name": "REFNUMBER", "desc": "Riferimento a IT_METHODS.", "mandatory": true },
          { "name": "ACTIVITY", "desc": "Numero operazione (es. '0010').", "mandatory": true },
          { "name": "CONTROL_KEY", "desc": "Chiave di controllo (es. 'PM01').", "mandatory": true },
          { "name": "WORK_CNTR", "desc": "Centro di lavoro dell'operazione.", "mandatory": true },
          { "name": "DESCRIPTION", "desc": "Descrizione dell'operazione.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "Questa è una BAPI complessa. La tabella IT_METHODS orchestra tutte le azioni. Un'azione con METHOD = 'SAVE' è richiesta per persistere i dati. È indispensabile eseguire BAPI_TRANSACTION_COMMIT dopo la chiamata." }
  ],
  "content": `"----------------------------------------------------------------------
" 6. BAPI: BAPI_ALM_ORDER_MAINTAIN
"----------------------------------------------------------------------
  WRITE: / 'Esecuzione Test 6: BAPI_ALM_ORDER_MAINTAIN'.
  ULINE.

  " Scenario: Modifica di un'operazione di un ordine di manutenzione.
  " Esempio: modifica del centro di lavoro.

  " Dichiarazione variabili LOCALI
  DATA: lt_metodi_alm            TYPE STANDARD TABLE OF bapi_alm_order_method,
        " *** CORREZIONE 2: Tipo tabella corretto per compatibilità con la BAPI.
        lt_dati_operazione_alm   TYPE STANDARD TABLE OF bapi_alm_order_operation,
        lt_update_operazione_alm TYPE STANDARD TABLE OF bapi_alm_order_operation_up,
        lt_messaggi_alm          TYPE STANDARD TABLE OF bapiret2.

  " 1. Definizione dei metodi da eseguire
  APPEND VALUE #(
      refnumber  = 1
      objecttype = 'OPERATION'
      method     = 'CHANGE'
      objectkey  = '0000000000010010' " Esempio: Ordine 000000000001, Operazione 0010
  ) TO lt_metodi_alm.

  APPEND VALUE #(
      refnumber = 1 " Riferito allo stesso gruppo di dati
      method    = 'SAVE'
  ) TO lt_metodi_alm.


  " 2. Dati della nuova operazione (IT_OPERATION)
  APPEND VALUE #(
      "orderid    = '000000000001' " Numero Ordine
      activity   = '0010'         " Numero Operazione
      work_cntr  = 'PWCC1'        " Nuovo centro di lavoro
      pers_no    = '00000001'     " Nuovo responsabile
  ) TO lt_dati_operazione_alm.

  " 3. Indicatori di quali campi aggiornare (IT_OPERATION_UP)
  APPEND VALUE #(
      " orderid    = '000000000001'
      activity   = '0010'
      work_cntr  = lc_vero " Flag per aggiornare il centro di lavoro
      pers_no    = lc_vero " Flag per aggiornare il responsabile
  ) TO lt_update_operazione_alm.

  " Chiamata alla BAPI
  CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
    TABLES
      it_methods      = lt_metodi_alm
      it_operation    = lt_dati_operazione_alm
      it_operation_up = lt_update_operazione_alm
      return          = lt_messaggi_alm.

  " Gestione esito BAPI
  DATA lv_successo_alm TYPE abap_bool VALUE lc_vero.
  LOOP AT lt_messaggi_alm INTO DATA(ls_msg_alm) WHERE type CA 'EA'.
    lv_successo_alm = lc_falso.
    EXIT.
  ENDLOOP.

  " Gestione COMMIT / ROLLBACK
  IF lv_successo_alm = lc_falso.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE 'Modifica ordine di manutenzione fallita. Eseguito ROLLBACK.' TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    " Eseguo il COMMIT.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    MESSAGE 'Ordine di manutenzione modificato con successo.' TYPE 'S'.
  ENDIF.

  " Visualizzazione messaggi
  IF lt_messaggi_alm IS NOT INITIAL.
    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING
        it_message = lt_messaggi_alm.
  ENDIF.

  NEW-LINE.
  NEW-LINE.
`
};
