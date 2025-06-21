export const bapi = {
  "name": "BAPI_BANK_CREATE",
  "description": "Crea dati anagrafici banca.",
  "details": [
    {
      "title": "Parametri di Importazione Obbligatori",
      "structures": [
        { "name": "Import Parameters", "type": "Singoli", "fields": [
          { "name": "BANK_CTRY", "desc": "Paese della banca (chiave).", "mandatory": true },
          { "name": "BANK_KEY", "desc": "Chiave banca (es. ABI/CAB o SWIFT).", "mandatory": true }
        ]},
        { "name": "BANK_ADDRESS", "type": "BAPIADDRESS", "fields": [
          { "name": "BANK_NAME", "desc": "Nome della banca.", "mandatory": true },
          { "name": "STREET", "desc": "Via e numero civico.", "mandatory": true },
          { "name": "CITY", "desc": "Città.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "Richiede BAPI_TRANSACTION_COMMIT per il salvataggio." }
  ],
  "content": `"&---------------------------------------------------------------------*
"&      Form  f_testa_bank_create
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_BANK_CREATE
"----------------------------------------------------------------------
FORM f_testa_bank_create.
  DATA: ls_bank_address_data TYPE bapi1011_address. " Tipo corretto per BANK_ADDRESS

  CLEAR: gs_return.            " Reset della struttura di ritorno generica
  CLEAR: ls_bank_address_data. " Reset struttura indirizzo principale

  " Popolamento minimo per BANK_ADDRESS (BAPI1011_ADDRESS)
  " ls_bank_address_data-city = 'BOLOGNA'.
  " ls_bank_address_data-street = 'VIA TEST'.
  " ls_bank_address_data-country = 'IT'. " Spesso un campo obbligatorio per indirizzo

  CALL FUNCTION 'BAPI_BANK_CREATE'
    EXPORTING
      bank_ctry     = 'IT'      " Codice paese banca
      bank_key      = 'DUMMYKEY' " ID banca (es. ABI o codice di routing, sostituire)
      bank_address  = ls_bank_address_data " Parametro obbligatorio di tipo BAPI1011_ADDRESS
    IMPORTING
      return        = gs_return. " Il parametro RETURN è EXPORTING (struttura singola BAPIRET2)

  PERFORM f_scrivi_esito USING 'BAPI_BANK_CREATE'.
ENDFORM.
`
};
