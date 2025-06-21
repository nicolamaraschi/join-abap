export const bapi = {
  "name": "BAPI_SALESORDER_GETSTATUS",
  "description": "Ottenimento Stato Ordine Cliente.",
  "details": [
    {
      "title": "Parametri di Importazione Obbligatori",
      "structures": [
        { "name": "Import Parameters", "type": "Singoli", "fields": [
          { "name": "SALESDOCUMENT", "desc": "Numero del documento di vendita di cui si vuole lo stato.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "Restituisce lo stato generale e di ogni posizione (es. stato di consegna, stato di fatturazione)." }
  ],
  "content": `"&---------------------------------------------------------------------*
"&      Form  f_testa_salesorder_getstat
"&---------------------------------------------------------------------*
" Testa la BAPI BAPI_SALESORDER_GETSTATUS - Ottenimento Stato Ordine Cliente
"----------------------------------------------------------------------
FORM f_testa_salesorder_getstat. " Accorciato da f_testa_salesorder_getstatus
  DATA: lv_salesdocument_s    TYPE bapivbeln-vbeln, " Tipo corretto per SALESDOCUMENT
        ls_return_soget     TYPE bapireturn,      " Tipo corretto per RETURN (BAPIRETURN)
        lt_statusinfo_soget   TYPE STANDARD TABLE OF bapisdstat. " Tipo corretto per STATUSINFO (BAPISDSTAT)

  CLEAR: gs_return.
  CLEAR: ls_return_soget.
  CLEAR: lt_statusinfo_soget.

  " Campi obbligatori
  lv_salesdocument_s = '0000000000'. " Numero Ordine Cliente (sostituire)

  CALL FUNCTION 'BAPI_SALESORDER_GETSTATUS'
    EXPORTING
      salesdocument     = lv_salesdocument_s
    IMPORTING
      return            = ls_return_soget " RETURN è EXPORTING di tipo BAPIRETURN
    TABLES
      statusinfo        = lt_statusinfo_soget. " STATUSINFO è TABLES di tipo BAPISDSTAT

  " Trasferiamo il messaggio specifico a quello generico per la stampa
  gs_return-message = ls_return_soget-message.
  IF ls_return_soget IS NOT INITIAL.
    gs_return-type = 'E'. " Assumiamo errore se c'è un messaggio, altrimenti 'S'
  ELSE.
    gs_return-type = 'S'.
  ENDIF.
  " I campi ID, Number non sono disponibili in BAPIRETURN, quindi non li popoliamo

  PERFORM f_scrivi_esito USING 'BAPI_SALESORDER_GETSTATUS'.
  " Esempio di output dello stato (dovrai ciclare lt_statusinfo_soget per tutti i dettagli)
  IF lt_statusinfo_soget IS NOT INITIAL.
    LOOP AT lt_statusinfo_soget INTO DATA(ls_status_entry).
      WRITE: / 'Stato Posizione (', ls_status_entry-itm_number, '):',
               'Stato proc. gen.:', ls_status_entry-prc_stat_h,
               'Stato cons. pos.:', ls_status_entry-dlv_stat_i. " Usiamo DLV_STAT_I per stato consegna posizione
    ENDLOOP.
  ENDIF.
ENDFORM.
`
};
