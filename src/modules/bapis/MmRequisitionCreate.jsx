export const bapi = {
  "name": "BAPI_REQUISITION_CREATE",
  "description": "Crea una richiesta d'acquisto (RdA).",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori",
      "structures": [
        { "name": "REQUISITION_ITEMS", "type": "Tabella di BAPIEBANC", "fields": [
          { "name": "DOC_TYPE", "desc": "Tipo documento RdA.", "mandatory": true },
          { "name": "MATERIAL", "desc": "Numero materiale (o SHORT_TEXT per RdA di testo).", "mandatory": false },
          { "name": "PLANT", "desc": "Divisione.", "mandatory": true },
          { "name": "QUANTITY", "desc": "Quantità richiesta.", "mandatory": true },
          { "name": "DELIV_DATE", "desc": "Data di consegna richiesta.", "mandatory": true },
          { "name": "PUR_GROUP", "desc": "Gruppo acquisti.", "mandatory": true }
        ]},
        { "name": "REQUISITION_ACCOUNT_ASSIGNMENT", "type": "Tabella di BAPIEBANKN", "fields": [
          { "name": "PREQ_ITEM", "desc": "Numero posizione RdA a cui si riferisce l'imputazione.", "mandatory": true },
          { "name": "G_L_ACCT", "desc": "Conto Co.Ge.", "mandatory": true },
          { "name": "COST_CTR", "desc": "Centro di costo.", "mandatory": false }
        ]}
      ]
    },
    { "title": "Note", "content": "Se la posizione ha un tipo di imputazione (es. 'K' per centro di costo), la tabella REQUISITION_ACCOUNT_ASSIGNMENT deve essere compilata. Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": `"--------------------------------------------------------------------
  " 3. LOGICA BAPI_PLANNEDORDER_CREATE / BAPI_REQUISITION_CREATE
  "--------------------------------------------------------------------
  " Questo blocco simula la creazione di ordini pianificati o richieste
  " d'acquisto basate su dati di input.

  " Tipi di dato per la terza logica
  TYPES: BEGIN OF ty_zpf,
           matnr TYPE matnr,
           beskz TYPE beskz,
           meins TYPE meins,
           m1q   TYPE menge_d, m2q TYPE menge_d, m3q TYPE menge_d,
           m4q   TYPE menge_d, m5q TYPE menge_d, m6q TYPE menge_d,
           m7q   TYPE menge_d, m8q TYPE menge_d, m9q TYPE menge_d,
           m10q  TYPE menge_d, m11q TYPE menge_d, m12q TYPE menge_d,
         END OF ty_zpf.
  TYPES: BEGIN OF ty_perio,
           nr   TYPE numc2,
           data TYPE datum,
         END OF ty_perio.

  " Dati locali per la terza logica
  DATA: gt_zpf      TYPE STANDARD TABLE OF ty_zpf.
  DATA: gt_perio    TYPE STANDARD TABLE OF ty_perio.
  DATA: ls_zpf      LIKE LINE OF gt_zpf.
  DATA: ls_perio    LIKE LINE OF gt_perio.
  DATA: lv_cnt      TYPE numc2.
  DATA: lv_fld(80)  TYPE c.
  DATA: lv_error    TYPE abap_bool.
  DATA: lv_plnum    TYPE plaf-plnum.
  DATA: ls_hdplnd   TYPE bapiplaf_i1.
  DATA: ls_ret_plan TYPE bapireturn1.
  DATA: lt_msg_all  TYPE TABLE OF bapiret2.
  DATA: ls_msg      TYPE bapiret2.
  DATA: lt_ret_req  TYPE TABLE OF bapireturn.
  DATA: ls_ret_req  TYPE bapireturn. " *** CORREZIONE: Area di lavoro per lt_ret_req ***
  DATA: ls_reqitm   TYPE bapiebanc.
  DATA: lt_reqitm   TYPE TABLE OF bapiebanc.

  FIELD-SYMBOLS: <fs_qty> TYPE any.

  " --- INIZIO: Popolamento dati di test (DA SOSTITUIRE CON LA TUA LOGICA) ---
  " In un programma reale, questi dati verrebbero da una SELECT o da input.
  DO 12 TIMES.
    ls_perio-nr   = sy-index.
    ls_perio-data = sy-datum + ( sy-index * 30 ). " Una data per ogni mese
    APPEND ls_perio TO gt_perio.
  ENDDO.

  " Esempio 1: Materiale da produrre internamente (crea Ordine Pianificato)
  ls_zpf-matnr = 'R-B101'. " Stesso materiale di prima
  ls_zpf-beskz = 'E'.      " Produzione interna
  ls_zpf-meins = 'ST'.
  ls_zpf-m1q   = 10.
  ls_zpf-m2q   = 20.
  APPEND ls_zpf TO gt_zpf.

  " Esempio 2: Materiale da acquistare (crea Richiesta d'Acquisto)
  CLEAR ls_zpf.
  ls_zpf-matnr = 'MATERIALE_ACQUISTO'. " Sostituire con un materiale valido!
  ls_zpf-beskz = 'F'.                " Approvvigionamento esterno
  ls_zpf-meins = 'ST'.
  ls_zpf-m3q   = 50.
  APPEND ls_zpf TO gt_zpf.
  " --- FINE: Popolamento dati di test ---

  ULINE.
  WRITE: / '*** 3. Inizio Creazione Ordini Pianificati / Richieste Acquisto ***'.
  ULINE.

  LOOP AT gt_zpf INTO ls_zpf.
    lv_cnt = 1.
    WHILE lv_cnt <= 12.
      " Assegnazione dinamica della quantità del mese
      lv_fld = |LS_ZPF-M{ lv_cnt }Q|.
      ASSIGN (lv_fld) TO <fs_qty>.

      " Cerca la data corrispondente al periodo
      READ TABLE gt_perio INTO ls_perio WITH KEY nr = lv_cnt.
      IF sy-subrc <> 0.
        ADD 1 TO lv_cnt.
        CONTINUE. " Periodo non trovato
      ENDIF.

      ADD 1 TO lv_cnt.
      IF <fs_qty> IS NOT ASSIGNED OR <fs_qty> = 0.
        CONTINUE. " Salta se la quantità è zero
      ENDIF.

      CASE ls_zpf-beskz.
        WHEN 'E'. "--- Ordine pianificato ---
          CLEAR: ls_hdplnd, ls_ret_plan, lv_plnum.
          ls_hdplnd-material        = ls_zpf-matnr.
          ls_hdplnd-plan_plant      = ls_ord_create_data-plant. " Usa la stessa divisione di prima
          ls_hdplnd-total_plord_qty = <fs_qty>.
          ls_hdplnd-order_fin_date  = ls_perio-data.
          ls_hdplnd-firming_ind     = 'X'.
          ls_hdplnd-base_uom        = ls_zpf-meins.
          ls_hdplnd-det_schedule    = 'X'.

          CALL FUNCTION 'BAPI_PLANNEDORDER_CREATE'
            EXPORTING
              headerdata   = ls_hdplnd
            IMPORTING
              return       = ls_ret_plan
              plannedorder = lv_plnum.

          IF ls_ret_plan-type CA 'AEX'.
            lv_error = gc_true.
            MOVE-CORRESPONDING ls_ret_plan TO ls_msg.
          ELSE.
            ls_msg-type    = 'S'.
            ls_msg-message = |Creato ordine pianificato { lv_plnum }|.
          ENDIF.
          APPEND ls_msg TO lt_msg_all.

        WHEN 'F'. "--- Richiesta d'acquisto ---
          REFRESH: lt_reqitm, lt_ret_req.
          CLEAR ls_reqitm.
          ls_reqitm-doc_type  = 'NB'.
          ls_reqitm-preq_date = ls_perio-data.
          ls_reqitm-material  = ls_zpf-matnr.
          ls_reqitm-plant     = ls_ord_create_data-plant.
          ls_reqitm-quantity  = <fs_qty>.
          ls_reqitm-unit      = ls_zpf-meins.
          APPEND ls_reqitm TO lt_reqitm.

          CALL FUNCTION 'BAPI_REQUISITION_CREATE'
            EXPORTING
              skip_items_with_error = 'X'
            TABLES
              requisition_items     = lt_reqitm
              return                = lt_ret_req.

          " *** CORREZIONE: Utilizzo dell'area di lavoro ls_ret_req per LOOP e READ TABLE. ***
          LOOP AT lt_ret_req INTO ls_ret_req WHERE type CA 'AEX'.
            lv_error = gc_true.
            MOVE-CORRESPONDING ls_ret_req TO ls_msg.
            APPEND ls_msg TO lt_msg_all.
            EXIT. " Esce al primo errore grave
          ENDLOOP.
          IF sy-subrc <> 0. " Se non ci sono stati errori
            READ TABLE lt_ret_req INTO ls_ret_req WITH KEY type = 'S'.
            IF sy-subrc = 0.
               MOVE-CORRESPONDING ls_ret_req TO ls_msg.
               APPEND ls_msg TO lt_msg_all.
            ENDIF.
          ENDIF.
      ENDCASE.

      " Esegui COMMIT o ROLLBACK per ogni documento creato
      IF lv_error = gc_true.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        WRITE: / |ERRORE: Rollback eseguito per materiale { ls_zpf-matnr }|.
        CLEAR lv_error. " Reset per il prossimo ciclo
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
      ENDIF.

    ENDWHILE.
  ENDLOOP.

  " Visualizza tutti i messaggi raccolti
  IF lt_msg_all IS NOT INITIAL.
    ULINE.
    WRITE: / '*** Riepilogo messaggi da Creazione Ord. Pianificati / RdA ***'.
    LOOP AT lt_msg_all INTO ls_msg.
       WRITE: / 'Tipo:', ls_msg-type, 'ID:', ls_msg-id, 'Nr:', ls_msg-number.
       WRITE: '  Messaggio:', ls_msg-message.
    ENDLOOP.
    ULINE.
  ENDIF.
`
};
