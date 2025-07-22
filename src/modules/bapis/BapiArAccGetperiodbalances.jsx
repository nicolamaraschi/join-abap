export const bapi = {
  "name": "BAPI_AR_ACC_GETPERIODBALANCES",
  "description": "Ottiene i saldi periodici per un conto cliente.",
  "details": [],
  "content": `
FORM f_test_bapi_leggi_sa.
  " Scopo: Ottenere i saldi periodici per un conto cliente.
  DATA: ls_saldo_attuale TYPE bapi3007_9,
        lt_saldi_deb     TYPE STANDARD TABLE OF bapi3007_7,
        ls_saldo_deb     TYPE bapi3007_7,
        ls_ritorno       TYPE bapireturn,
        lt_saldi_speciali TYPE STANDARD TABLE OF bapi3007_a.

  WRITE: / 'Test BAPI: BAPI_AR_ACC_GETPERIODBALANCES'.
CALL FUNCTION 'BAPI_AR_ACC_GETPERIODBALANCES'
  EXPORTING
    customer                 = '0000000001'
    companycode              = '1000'
  IMPORTING
    actual_balance           = ls_saldo_attuale
    return                   = ls_ritorno
  TABLES
    debitor_balances         = lt_saldi_deb
    debitor_special_balances = lt_saldi_speciali. " <-- Aggiunto

  IF lt_saldi_deb IS NOT INITIAL.
    WRITE: / 'Saldi Periodici per Cliente:'.
    LOOP AT lt_saldi_deb INTO ls_saldo_deb.
      "WRITE: / ls_saldo_deb-monat, ls_saldo_deb-soll_long, ls_saldo_deb-haben_long.
    ENDLOOP.
  ENDIF.
ENDFORM.
`
};
