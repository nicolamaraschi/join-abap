export const bapi = {
  "name": "BAPI_GOODSMVT_CANCEL",
  "description": "Utilizzata per stornare/annullare un documento materiale precedentemente registrato (es. prelievo errato).",
  "details": [
    {
      "title": "Parametri di Importazione Chiave",
      "structures": [
        { "name": "Import", "type": "Singoli", "fields": [
          { "name": "MATERIALDOCUMENT", "desc": "Numero del Documento Materiale da stornare.", "mandatory": true },
          { "name": "MATDOCUMENTYEAR", "desc": "Anno del Documento Materiale.", "mandatory": true },
          { "name": "GOODSMVT_CODE", "desc": "Codice transazione per lo storno (es. MBST).", "mandatory": false }
        ]}
      ]
    },
    { "title": "Note", "content": "Crea un documento di storno con un tipo movimento inverso. Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": `"----------------------------------------------------------------------
" 5. BAPI: BAPI_GOODSMVT_CANCEL
"----------------------------------------------------------------------
  WRITE: / 'Esecuzione Test 5: BAPI_GOODSMVT_CANCEL'.
  ULINE.

  " Scenario: Annullamento di un documento materiale.
  " I valori sono esempi hardcoded.

  " Dichiarazione variabili LOCALI
  DATA: lv_doc_mat_da_annullare   TYPE bapi2017_gm_head_ret-mat_doc,
        lv_anno_doc_mat_annullare TYPE bapi2017_gm_head_ret-doc_year,
        ls_testa_ritorno_annullo  TYPE bapi2017_gm_head_ret,
        lt_messaggi_annullo       TYPE STANDARD TABLE OF bapiret2.

  " Popolamento dei parametri di input
  lv_doc_mat_da_annullare   = '4900000001'.
  lv_anno_doc_mat_annullare = CONV gjahr( '2025' ).

  " Chiamata alla BAPI
  CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
    EXPORTING
      materialdocument    = lv_doc_mat_da_annullare
      matdocumentyear     = lv_anno_doc_mat_annullare
      goodsmvt_pstng_date = sy-datum  " *** CORREZIONE 1: Nome parametro corretto (era pstng_date)
    IMPORTING
      goodsmvt_headret    = ls_testa_ritorno_annullo
    TABLES
      return              = lt_messaggi_annullo.

  " Gestione esito BAPI
  DATA: lv_successo_annullo  TYPE abap_bool,
        lv_testo_msg_annullo TYPE string.

  lv_successo_annullo = lc_vero.
  LOOP AT lt_messaggi_annullo INTO DATA(ls_riga_msg_ann) WHERE type CA 'EA'.
    lv_successo_annullo = lc_falso.
    lv_testo_msg_annullo = ls_riga_msg_ann-message.
    EXIT.
  ENDLOOP.

  " Il successo è confermato solo se viene restituito un nuovo numero documento
  IF lv_successo_annullo = lc_vero AND ls_testa_ritorno_annullo IS INITIAL.
    lv_successo_annullo = lc_falso.
  ENDIF.

  " Gestione COMMIT / ROLLBACK
  IF lv_successo_annullo = lc_falso.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE 'Annullamento movimento merci fallito. Eseguito ROLLBACK.' TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    " Eseguo il COMMIT.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    DATA(lv_msg_succ_annullo) = |Mov. merci annullato. Doc. annullamento: { ls_testa_ritorno_annullo-mat_doc } / { ls_testa_ritorno_annullo-doc_year }|.
    MESSAGE lv_msg_succ_annullo TYPE 'S'.
  ENDIF.

  " Visualizzazione messaggi
  IF lt_messaggi_annullo IS NOT INITIAL.
    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING
        it_message = lt_messaggi_annullo.
  ENDIF.

  NEW-LINE.
  NEW-LINE.
`
};
