export const bapi = {
  "name": "BAPI_FIXEDASSET_CREATE1",
  "description": "Crea dati anagrafici cespiti.",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori",
      "structures": [
        { "name": "KEY", "type": "BAPI1022_KEY", "fields": [
          { "name": "COMPANYCODE", "desc": "Società.", "mandatory": true }
        ]},
        { "name": "GENERALDATA", "type": "BAPI1022_FEGLG001", "fields": [
          { "name": "ASSETCLASS", "desc": "Classe di immobilizzazione.", "mandatory": true },
          { "name": "DESCRIPT", "desc": "Descrizione del cespite.", "mandatory": true },
          { "name": "INVENT_NO", "desc": "Numero di inventario.", "mandatory": false },
          { "name": "MAIN_NTEXT", "desc": "Descrizione lunga del cespite.", "mandatory": false }
        ]},
        { "name": "TIMEDEPENDENTDATA", "type": "BAPI1022_FEGLG002", "fields": [
          { "name": "COSTCENTER", "desc": "Centro di costo a cui è assegnato il cespite.", "mandatory": true }
        ]},
        { "name": "POSTINGINFORMATION", "type": "BAPI1022_FEGLG003", "fields": [
          { "name": "CAP_DATE", "desc": "Data di capitalizzazione.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "È necessario utilizzare le strutture 'X' corrispondenti (es. GENERALDATAX, TIMEDEPENDENTDATAX) per indicare quali campi si stanno passando. Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": `"&---------------------------------------------------------------------*
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

  CLEAR: gs_return.          " Reset della struttura di ritorno generica
  CLEAR: ls_asset_created.   " Reset della struttura per l'asset creato

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
    EXPORTING
      key                 = ls_key
      generaldata         = ls_generaldata
      generaldatax        = ls_generaldatax
      timedependentdata   = ls_timedependent
      timedependentdatax  = ls_timedependentx
      postinginformation  = ls_postinginfo
      postinginformationx = ls_postinginfox
    IMPORTING
      assetcreated        = ls_asset_created " Parametro EXPORTING per l'asset creato
      return              = gs_return.       " Parametro EXPORTING (struttura singola BAPIRET2)

  PERFORM f_scrivi_esito USING 'BAPI_FIXEDASSET_CREATE1'.
ENDFORM.
`
};
