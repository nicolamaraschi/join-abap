export const bapi = {
  "name": "BAPI_GOODSMVT_CREATE",
  "description": "Interfaccia versatile per registrare tutti i tipi di movimenti merci.",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori",
      "structures": [
        { "name": "GOODSMVT_HEADER", "type": "BAPIGMHD01", "fields": [
          { "name": "PSTNG_DATE", "desc": "Data di Registrazione.", "mandatory": true },
          { "name": "DOC_DATE", "desc": "Data Documento.", "mandatory": true }
        ]},
        { "name": "GOODSMVT_CODE", "type": "Singolo", "fields": [
          { "name": "GM_CODE", "desc": "Codice transazione (es. '01' per EM da OdA, '03' per Uscita Merci).", "mandatory": true }
        ]},
        { "name": "GOODSMVT_ITEM", "type": "Tabella di BAPIGMITEMCREATE", "fields": [
          { "name": "MATERIAL", "desc": "Numero Materiale.", "mandatory": true },
          { "name": "PLANT", "desc": "Divisione.", "mandatory": true },
          { "name": "STGE_LOC", "desc": "Magazzino.", "mandatory": true },
          { "name": "MOVE_TYPE", "desc": "Tipo Movimento (es. 101, 201, 261).", "mandatory": true },
          { "name": "ENTRY_QNT", "desc": "Quantità.", "mandatory": true },
          { "name": "ENTRY_UOM", "desc": "Unità di misura.", "mandatory": true },
          { "name": "PO_NUMBER", "desc": "Numero OdA (per GM_CODE '01').", "mandatory": false },
          { "name": "PO_ITEM", "desc": "Posizione OdA (per GM_CODE '01').", "mandatory": false },
          { "name": "COSTCENTER", "desc": "Centro di Costo (per TMOV es. '201').", "mandatory": false },
          { "name": "ORDERID", "desc": "Ordine (per TMOV es. '261').", "mandatory": false }
        ]}
      ]
    },
    { "title": "Note", "content": "I campi obbligatori nella tabella GOODSMVT_ITEM dipendono criticamente dal valore di GOODSMVT_CODE. Ad esempio, PO_NUMBER è obbligatorio per GM_CODE '01'. Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": `"&---------------------------------------------------------------------*
"&      Form  f_testa_goodsmvt_create
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_GOODSMVT_CREATE - Registrazione Movimenti Merci
"----------------------------------------------------------------------
FORM f_testa_goodsmvt_create.
  DATA: ls_goodsmvt_header TYPE BAPI2017_GM_HEAD_01,      " Correzione: Tipo BAPI2017_GM_HEAD_01
        ls_goodsmvt_code   TYPE BAPI2017_GM_CODE,        " Correzione: Tipo BAPI2017_GM_CODE
        lt_goodsmvt_item   TYPE STANDARD TABLE OF BAPI2017_GM_ITEM_CREATE, " Correzione: Tipo BAPI2017_GM_ITEM_CREATE
        ls_goodsmvt_item   TYPE BAPI2017_GM_ITEM_CREATE,
        ls_materialdocument TYPE mblnr,                   " Correzione: Tipo MBLNR per il numero di documento materiale di output
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
  ls_goodsmvt_item-plant        = 'TEST'.     " Divisione (sostituire)
  ls_goodsmvt_item-stge_loc     = '0001'.     " Magazzino (sostituire)
  ls_goodsmvt_item-move_type    = '101'.      " Tipo Movimento (es. 101: Entrata merci per ordine)
  ls_goodsmvt_item-entry_qnt    = 10.         " Quantità
  ls_goodsmvt_item-entry_uom    = 'ST'.      " Unità di misura
  ls_goodsmvt_item-po_number    = '4500000000'. " Numero OdA (obbligatorio per GM_CODE '01', sostituire)
  ls_goodsmvt_item-po_item      = 10.         " Posizione OdA (sostituire)
  APPEND ls_goodsmvt_item TO lt_goodsmvt_item.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header = ls_goodsmvt_header
      goodsmvt_code   = ls_goodsmvt_code
    IMPORTING
      materialdocument = ls_materialdocument " Correzione: ora è MBLNR diretto
    TABLES
      goodsmvt_item   = lt_goodsmvt_item
      return          = lt_return_gdmvt.

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
ENDFORM.`
};
