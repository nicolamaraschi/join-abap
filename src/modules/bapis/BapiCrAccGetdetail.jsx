export const bapi = {
  "name": "BAPI_CR_ACC_GETDETAIL",
  "description": "Ottiene dettagli sul limite di credito di un cliente.",
  "details": [],
  "content": `
FORM f_test_bapi_leggi_del.
  " Scopo: Ottenere dettagli sul limite di credito di un cliente.
  DATA: ls_ritorno TYPE bapireturn1.

  WRITE: / 'Test BAPI: BAPI_CR_ACC_GETDETAIL'.

  CALL FUNCTION 'BAPI_CR_ACC_GETDETAIL'
    EXPORTING
      customer            = '0000000001' " SOSTITUIRE
      creditcontrolarea   = '1000'       " SOSTITUIRE
    IMPORTING
      credit_account_detail = gs_dettaglio_conto_credito
      return                = ls_ritorno.

  IF gs_dettaglio_conto_credito IS NOT INITIAL.
    WRITE: / 'Dati Credito per Cliente trovati:'.
    WRITE: / 'Limite di Credito:', gs_dettaglio_conto_credito-cred_limit.
    WRITE: / 'Rischio:', gs_dettaglio_conto_credito-risk_categ.
    WRITE: / 'Valuta:', gs_dettaglio_conto_credito-currency.
  ENDIF.
ENDFORM.
`
};
