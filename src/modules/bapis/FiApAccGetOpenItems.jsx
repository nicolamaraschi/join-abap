export const bapi = {
  "name": "BAPI_AP_ACC_GETOPENITEMS",
  "description": "Recupera le partite aperte per un conto fornitore a una data chiave.",
  "details": [
    {
      "title": "Parametri di Importazione Obbligatori",
      "structures": [
        { "name": "Import Parameters", "type": "Singoli", "fields": [
          { "name": "VENDOR", "desc": "Numero del fornitore.", "mandatory": true },
          { "name": "COMPANYCODE", "desc": "Codice della società.", "mandatory": true },
          { "name": "KEYDATE", "desc": "Data di riferimento per la selezione.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "Il parametro NOTEDITEMS (opzionale) può essere impostato a 'X' per includere le partite pro-memoria." }
  ],
  "content": `"&---------------------------------------------------------------------*
"&      Form  f_testa_ap_getopenitems
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_AP_ACC_GETOPENITEMS
"----------------------------------------------------------------------
FORM f_testa_ap_getopenitems.
  " Il parametro LINEITEMS è obbligatorio e di tipo TABLES con riga BAPI3008_2.
  DATA: lt_lineitems_ap TYPE STANDARD TABLE OF bapi3008_2. " Tabella per LINEITEMS
  DATA: ls_return_ap    TYPE bapireturn.               " Tipo specifico per RETURN di questa BAPI

  CLEAR: gs_return.          " Reset della struttura di ritorno generica per stampa
  CLEAR: lt_lineitems_ap.    " Reset della tabella LINEITEMS
  CLEAR: ls_return_ap.       " Reset della struttura di ritorno specifica

  CALL FUNCTION 'BAPI_AP_ACC_GETOPENITEMS'
    EXPORTING
      companycode = 'TEST'  " Sostituire con un codice società valido
      vendor      = 'DUMMY' " Sostituire con un fornitore valido
      keydate     = sy-datum
    IMPORTING
      return      = ls_return_ap " Il parametro RETURN è IMPORTING (struttura singola BAPIRETURN)
    TABLES
      lineitems   = lt_lineitems_ap. " Parametro TABLES obbligatorio

  " Trasferiamo il messaggio specifico a quello generico per la stampa
  gs_return-message = ls_return_ap-message.
  IF ls_return_ap IS NOT INITIAL.
    gs_return-type = 'E'. " Assumiamo errore se c'è un messaggio, altrimenti 'S'
  ELSE.
    gs_return-type = 'S'.
  ENDIF.
  " I campi ID, Number non sono disponibili in BAPIRETURN, quindi non li popoliamo

  PERFORM f_scrivi_esito USING 'BAPI_AP_ACC_GETOPENITEMS'.
ENDFORM.`
};
