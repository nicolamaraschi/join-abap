export const bapi = {
  "name": "BAPI_GOODSMVT_CREATE",
  "description": "BAPI universale per la registrazione di movimenti merci, usata in PP per prelievi (es. TMOV 261) ed entrate merci (es. TMOV 101).",
  "details": [
    {
      "title": "Parametri di Importazione Chiave",
      "structures": [
        { "name": "GOODSMVT_HEADER", "type": "Struttura", "fields": [
          { "name": "PSTNG_DATE", "desc": "Data di Registrazione.", "mandatory": true },
          { "name": "DOC_DATE", "desc": "Data Documento.", "mandatory": true }
        ]},
        { "name": "GOODSMVT_CODE", "type": "Singolo", "fields": [
          { "name": "GM_CODE", "desc": "Codice che identifica la transazione (es. '02' per entrata merci da ordine, equiv. MB31).", "mandatory": true }
        ]},
        { "name": "GOODSMVT_ITEM", "type": "Tabella", "fields": [
          { "name": "MATERIAL", "desc": "Numero Materiale.", "mandatory": true },
          { "name": "PLANT", "desc": "Divisione.", "mandatory": true },
          { "name": "STGE_LOC", "desc": "Ubicazione.", "mandatory": true },
          { "name": "MOVE_TYPE", "desc": "Tipo Movimento (es. '101', '261').", "mandatory": true },
          { "name": "ENTRY_QNT", "desc": "Quantità.", "mandatory": true },
          { "name": "ORDERID", "desc": "Numero Ordine di Produzione di riferimento.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "È fondamentale popolare i campi corretti in base allo scenario. La BAPI è molto potente ma richiede precisione. Richiede BAPI_TRANSACTION_COMMIT per finalizzare il movimento." }
  ],
  "content": `"----------------------------------------------------------------------
" 4. BAPI: BAPI_GOODSMVT_CREATE
"----------------------------------------------------------------------
  WRITE: / 'Esecuzione Test 4: BAPI_GOODSMVT_CREATE'.
  ULINE.

  " Scenario: Creazione di un movimento merci (giroconto).
  DATA: ls_testa_mov_merci         TYPE bapi2017_gm_head_01,
        ls_codice_mov_merci      TYPE bapi2017_gm_code,
        lt_posizioni_mov_merci   TYPE bapi2017_gm_item_create_t,
        ls_testa_ritorno_mov_merci TYPE bapi2017_gm_head_ret,
        lt_messaggi_mov_merci    TYPE STANDARD TABLE OF bapiret2.

  " Popolamento dati di testata
  ls_testa_mov_merci-pstng_date = sy-datum.
  ls_testa_mov_merci-doc_date   = sy-datum.
  ls_testa_mov_merci-pr_uname   = sy-uname.

  " Popolamento del codice movimento
  ls_codice_mov_merci-gm_code = '04'. " Giroconto

  " Popolamento dati di posizione
  APPEND VALUE #(
    material     = 'P-101'
    plant        = '1000'
    stge_loc     = '0001'
    batch        = 'BATCH01'
    move_type    = '311'
    entry_qnt    = 10
    entry_uom    = 'PC'
    move_plant   = '1000'
    move_stloc   = '0002' " Magazzino destinazione
    move_batch   = 'BATCH01'
  ) TO lt_posizioni_mov_merci.

  " Chiamata alla BAPI
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header = ls_testa_mov_merci
      goodsmvt_code   = ls_codice_mov_merci
    IMPORTING
      goodsmvt_headret = ls_testa_ritorno_mov_merci
    TABLES
      goodsmvt_item   = lt_posizioni_mov_merci
      return          = lt_messaggi_mov_merci.

  " Gestione esito BAPI
  DATA: lv_successo_mov_merci  TYPE abap_bool,
        lv_testo_msg_mov_merci TYPE string.

  lv_successo_mov_merci = lc_vero.
  LOOP AT lt_messaggi_mov_merci INTO DATA(ls_riga_msg_mov) WHERE type CA 'EA'.
    lv_successo_mov_merci = lc_falso.
    lv_testo_msg_mov_merci = ls_riga_msg_mov-message.
    EXIT.
  ENDLOOP.

  " Il successo è confermato solo se viene restituito un numero documento
  IF lv_successo_mov_merci = lc_vero AND ls_testa_ritorno_mov_merci IS INITIAL.
    lv_successo_mov_merci = lc_falso.
  ENDIF.

  " Gestione COMMIT / ROLLBACK
  IF lv_successo_mov_merci = lc_falso.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE 'Creazione movimento merci fallita. Eseguito ROLLBACK.' TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    " Eseguo il COMMIT. Eventuali errori di aggiornamento non vengono
    " riportati in sy-subrc ma vanno controllati in SM13.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    DATA(lv_msg_succ_mov) = |Mov. merci creato: { ls_testa_ritorno_mov_merci-mat_doc } / { ls_testa_ritorno_mov_merci-doc_year }|.
    MESSAGE lv_msg_succ_mov TYPE 'S'.
  ENDIF.

  " Visualizzazione messaggi
  IF lt_messaggi_mov_merci IS NOT INITIAL.
    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING
        it_message = lt_messaggi_mov_merci.
  ENDIF.
`
};
