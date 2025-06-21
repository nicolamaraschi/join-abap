export const bapi = {
  "name": "BAPI_MATERIAL_SAVEDATA",
  "description": "Utilizzata per creare, estendere o modificare i dati dei materiali.",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori (per Creazione)",
      "structures": [
        { "name": "HEADDATA", "type": "BAPIMATHEAD", "fields": [
          { "name": "MATERIAL", "desc": "Numero materiale (se numerazione esterna).", "mandatory": false },
          { "name": "IND_SECTOR", "desc": "Settore Industriale.", "mandatory": true },
          { "name": "MATL_TYPE", "desc": "Tipo Materiale.", "mandatory": true },
          { "name": "BASIC_VIEW", "desc": "Flag ('X') per elaborare i Dati Base.", "mandatory": true }
        ]},
        { "name": "CLIENTDATA", "type": "BAPI_MARA", "fields": [
          { "name": "BASE_UOM", "desc": "Unità di Misura Base.", "mandatory": true },
          { "name": "MATL_GROUP", "desc": "Gruppo Materiali.", "mandatory": true },
          { "name": "DIVISION", "desc": "Divisione.", "mandatory": false }
        ]},
        { "name": "MATERIALDESCRIPTION", "type": "Tabella di BAPI_MAKT", "fields": [
          { "name": "LANGU", "desc": "Lingua della descrizione.", "mandatory": true },
          { "name": "MATL_DESC", "desc": "Descrizione materiale.", "mandatory": true }
        ]},
        { "name": "PLANTDATA", "type": "BAPI_MARC", "fields": [
          { "name": "PLANT", "desc": "Divisione da creare/estendere.", "mandatory": true },
          { "name": "PUR_GROUP", "desc": "Gruppo acquisti (per la vista acquisti).", "mandatory": false },
          { "name": "MRP_TYPE", "desc": "Tipo MRP (per la vista pianificazione).", "mandatory": false }
        ]}
      ]
    },
    { "title": "Note", "content": "Per ogni struttura dati (CLIENTDATA, PLANTDATA, etc.) è obbligatorio compilare la corrispondente struttura 'X' (CLIENTDATAX, PLANTDATAX, etc.) per indicare quali campi sono stati valorizzati e devono essere elaborati. Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": `"&---------------------------------------------------------------------*
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
        lv_material_input    TYPE matnr. " Usiamo per l'input del materiale

  CLEAR: gs_return.
  CLEAR: lt_materialdescription.

  " Conversione del nome del materiale per l'input (come nello snippet fornito)
  " Nota: Poiché non abbiamo i valori di us_pnodid-pname e uv_matnrpre,
  " useremo un valore fisso per il test, simile a quanto avevi già.
  lv_material_input = 'TEST_MAT_BAPI'. " Sostituire con il nome materiale desiderato

  CALL FUNCTION 'CONVERSION_EXIT_MATN2_INPUT'
    EXPORTING
      input         = lv_material_input
    IMPORTING
      output        = lv_material_input
    EXCEPTIONS
      OTHERS        = 1.
  " Controllo del valore di ritorno dopo CALL FUNCTION
  IF sy-subrc <> 0.
    MESSAGE 'Errore durante la conversione del numero materiale.' TYPE 'E'.
    EXIT. " Esce dalla FORM in caso di errore
  ENDIF.

  " Popola HEADDATA con il materiale da creare/modificare
  ls_headdata-material_long  = lv_material_input.
  ls_headdata-ind_sector     = 'M'.         " Settore Industriale (es. M = Ingegneria Meccanica)
  ls_headdata-matl_type      = 'FERT'.      " Tipo Materiale (es. FERT = Prodotto Finito)
  ls_headdata-basic_view     = gc_true.     " Flag per elaborare Dati Base

  " Campi obbligatori per CLIENTDATA
  ls_clientdata-base_uom   = 'ST'.      " Unità di Misura Base
  ls_clientdata-matl_group = '001'.     " Gruppo Materiali (sostituire)
  ls_clientdata-division   = '01'.      " Divisione (sostituire)

  " Popolare CLIENTDATAX
  ls_clientdatax-base_uom   = gc_true.
  ls_clientdatax-matl_group = gc_true.
  ls_clientdatax-division   = gc_true.

  " Campi obbligatori per MATERIALDESCRIPTION
  ls_materialdescription-langu     = sy-langu. " Lingua
  ls_materialdescription-matl_desc = 'Materiale di Test BAPI SAVEDATA'. " Descrizione materiale
  APPEND ls_materialdescription TO lt_materialdescription.

  " Campi obbligatori per PLANTDATA
  ls_plantdata-plant     = 'TEST'.     " Divisione (sostituire)
  ls_plantdata-pur_group = '001'.      " Gruppo acquisti (sostituire)
  ls_plantdata-mrp_type  = 'ND'.       " Tipo MRP (es. ND = Nessuna pianificazione)

  " Popolare PLANTDATAX
  ls_plantdatax-plant     = gc_true.
  ls_plantdatax-pur_group = gc_true.
  ls_plantdatax-mrp_type  = gc_true.

  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
    EXPORTING
      headdata           = ls_headdata
      clientdata         = ls_clientdata
      clientdatax        = ls_clientdatax
      plantdata          = ls_plantdata
      plantdatax         = ls_plantdatax
    IMPORTING
      return             = gs_return      " RETURN è un EXPORTING (struttura singola)
    TABLES
      materialdescription = lt_materialdescription.

  PERFORM f_scrivi_esito USING 'BAPI_MATERIAL_SAVEDATA'.
  " Se la creazione ha successo e il materiale è stato assegnato, il numero sarà nel messaggio di ritorno.
  " Per il test, possiamo semplicemente stampare il valore di input.
  WRITE: / 'Tentato processo per Materiale:', lv_material_input.


  " Richiede BAPI_TRANSACTION_COMMIT
  IF gs_return-type CA 'EA'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE 'Rollback effettuato per BAPI_MATERIAL_SAVEDATA.' TYPE 'I'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
    MESSAGE 'Commit effettuato per BAPI_MATERIAL_SAVEDATA.' TYPE 'I'.
  ENDIF.
ENDFORM.
`
};
