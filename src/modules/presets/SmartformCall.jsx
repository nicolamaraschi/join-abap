export const content = `*----------------------------------------------------------------------*
* Immagine 1: Chiamata al Function Module per ottenere il nome       *
*----------------------------------------------------------------------*
DATA: p_form TYPE tdsfname VALUE 'NOME_SMARTFORM_MAIUSCOLO',
      fm_name TYPE rs38l_fnam.

DATA: ssfctrlop TYPE ssfctrlop,
      ssfcompop TYPE ssfcompop.

* print data
CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
  EXPORTING
    formname           = p_form "GLI PASSO IL NOME DELLA STAMPA
  IMPORTING
    fm_name            = fm_name "OTTENGO IL CODICE DELLA FUNZIONE DI STAMPA
  EXCEPTIONS
    no_form            = 1
    no_function_module = 2
    OTHERS             = 3.

IF sy-subrc <> 0.
* error handling
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  EXIT.
ENDIF.


* Destination
ssfcompop-tddest   = 'LP01'.  " stampante
ssfcompop-tdimmed  = 'X'.     " stampa immediata
ssfcompop-tdnewid  = 'X'.     " nuovo id spool
ssfcompop-tddelete = 'X'.     " cancella spool

* Options
ssfctrlop-device    = 'PRINTER'. " tipo stampante
ssfctrlop-no_dialog = 'X'.       " no popup
ssfctrlop-preview   = 'X'.       " preview
ssfctrlop-langu     = 'I'.       " Lingua (Italiano)

" Chiamare il modulo di stampa
CALL FUNCTION fm_name
  EXPORTING
    control_parameters         = ssfctrlop    "SETTINGS FATTI SOPRA
    output_options             = ssfcompop    "SETTINGS FATTI SOPRA
  IMPORTING
    job_output_info            =             " Qui andrebbe una struttura per raccogliere info sul job
  EXCEPTIONS
    formatting_error           = 1
    internal_error             = 2
    send_error                 = 3
    user_canceled              = 4
    OTHERS                     = 5.

IF sy-subrc <> 0.
* Implement suitable error handling here
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.`;