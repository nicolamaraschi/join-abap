export const bapi = {
  "name": "BAPI_AR_ACC_GETOPENITEMS",
  "description": "Recupera le partite aperte per un conto cliente a una data chiave.",
  "details": [
    {
      "title": "Parametri di Importazione Obbligatori",
      "structures": [
        { "name": "Import Parameters", "type": "Singoli", "fields": [
          { "name": "CUSTOMER", "desc": "Numero del cliente.", "mandatory": true },
          { "name": "COMPANYCODE", "desc": "Codice della società.", "mandatory": true },
          { "name": "KEYDATE", "desc": "Data di riferimento per la selezione.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "Il parametro NOTEDITEMS (opzionale) può essere impostato a 'X' per includere le partite pro-memoria." }
  ],
  "content": `"&---------------------------------------------------------------------*
"&      Form  f_testa_ar_getopenitems
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_AR_ACC_GETOPENITEMS
"----------------------------------------------------------------------
FORM f_testa_ar_getopenitems.
  " Il parametro LINEITEMS è obbligatorio e di tipo TABLES con riga BAPI3007_2.
  DATA: lt_lineitems_ar TYPE STANDARD TABLE OF bapi3007_2. " Tabella per LINEITEMS (tipo specifico)
  DATA: ls_return_ar    TYPE bapireturn.               " Tipo specifico per RETURN di questa BAPI

  CLEAR: gs_return.          " Reset della struttura di ritorno generica per stampa
  CLEAR: lt_lineitems_ar.    " Reset della tabella LINEITEMS
  CLEAR: ls_return_ar.       " Reset della struttura di ritorno specifica

  CALL FUNCTION 'BAPI_AR_ACC_GETOPENITEMS'
    EXPORTING
      companycode = 'TEST'  " Sostituire con un codice società valido
      customer    = 'DUMMY' " Sostituire con un cliente valido
      keydate     = sy-datum
    IMPORTING
      return      = ls_return_ar " Il parametro RETURN è IMPORTING (struttura singola BAPIRETURN)
    TABLES
      lineitems   = lt_lineitems_ar. " Parametro TABLES obbligatorio (tipo BAPI3007_2)

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
ENDFORM.`
};
