export const content = `"CODICE POPUP

DATA: 
        lv_popup_title    TYPE string,          " Titolo del popup
        lv_text_for_popup TYPE string,          " Testo completo per TEXT_QUESTION
        lv_answer         TYPE c LENGTH 1,      " Risposta dell'utente
        lv_text_btn1        TYPE c LENGTH 60,
        lv_text_btn2        TYPE c LENGTH 60,
        lv_quickinfo_btn1   TYPE text132.

    " COSTRUISCO COSA METTERE NEL POPUP
    CLEAR lv_text_for_popup.
    CONCATENATE 'Materiale:' iv_matnr
            INTO lv_text_for_popup SEPARATED BY space.

    CONCATENATE lv_text_for_popup cl_abap_char_utilities=>newline
                'Ultima E.M. (BWART 101) per ODA:' lv_ebeln '/' lv_ebelp
            INTO lv_text_for_popup SEPARATED BY space.

    CONCATENATE lv_text_for_popup cl_abap_char_utilities=>newline
                'Doc. Materiale:' lv_belnr_mat 'del' lv_budat_char
            INTO lv_text_for_popup SEPARATED BY space.

    CONCATENATE lv_text_for_popup cl_abap_char_utilities=>newline cl_abap_char_utilities=>newline
                'Vuoi visualizzare i dettagli dell''Ordine d''Acquisto?'
            INTO lv_text_for_popup SEPARATED BY space.

    " Definizione testi per i bottoni e quickinfo
    " È consigliabile usare elementi di testo per la traducibilità, es. 'Visualizza ODA'(001)
    lv_popup_title = 'Ultimo ODA Trovato (Entrata Merci)'.
    lv_text_btn1 = 'Visualizza ODA'. " o TEXT-001 se definito
    lv_text_btn2 = 'Annulla'.       " o TEXT-002 se definito
    lv_quickinfo_btn1 = 'Visualizza dettagli Ordine d''Acquisto'. " o TEXT-003 se definito

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = lv_popup_title
        text_question         = lv_text_for_popup    " Testo consolidato
        text_button_1         = lv_text_btn1         " Testo per il primo bottone
        text_button_2         = lv_text_btn2         " Testo per il secondo bottone
        default_button        = '1'                  " Pre-seleziona il bottone 1
        display_cancel_button = SPACE                " Non mostrare il bottone "Cancel" di sistema, usiamo i nostri 2
        popup_type            = 'ICON_INFORMATION'   " Tipo di icona da visualizzare
        iv_quickinfo_button_1 = lv_quickinfo_btn1    " Quickinfo per il bottone 1 (opzionale)
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        text_not_found        = 1                    " Se gli elementi di testo per i bottoni non sono trovati
        OTHERS                = 2.

    IF sy-subrc <> 0. "  Errore  nella  chiamata  a  POPUP_TO_CONFIRM
      IF sy-subrc = 1.
        MESSAGE 'errore:  testo  non  trovato  ' TYPE 'E'.
      ELSE.
        MESSAGE 'errore:  errore  generico  ' TYPE 'E'.
      ENDIF.
    ENDIF.

    IF lv_answer = '1'.
      "  L'utente  ha  confermato,  visualizza  l'ODA
      IF lv_ebeln IS NOT INITIAL.

          SET PARAMETER ID 'BES' FIELD lv_ebeln.
          SET PARAMETER ID 'BSP' FIELD lv_ebelp.
          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ELSE.
        MESSAGE 'Numero  Ordine  d  Acquisto  non  disponibile  per  la  visualizzazione' TYPE 'E'.
      ENDIF.
    ENDIF.`;