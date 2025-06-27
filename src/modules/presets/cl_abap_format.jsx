export const content = `
REPORT z_doc_abap_moderno.

"************************************************************************
"* Programma: Z_DOC_ABAP_MODERNO
"* Data:      23.06.2025
"* Descrizione:
"* Questo report serve come prototipo e documentazione per diverse
"* funzionalità e sintassi moderne di ABAP.
"* È stato creato come un singolo file (monoreport) senza l'uso di
"* INCLUDE, come da richiesta specifica.
"* VERSIONE 6: Aggiunta la dimostrazione dell'opzione WIDTH.
"************************************************************************

"----------------------------------------------------------------------
" SEZIONE: DICHIARAZIONI GLOBALI
"----------------------------------------------------------------------
TABLES: vbak, sflight. " Necessario per l'uso di LIKE e per la SELECT

" Tipi per l'esempio con la SELECT e calcolo
TYPES: BEGIN OF ty_volo_calcolato,
         carrid     TYPE s_carr_id,
         connid     TYPE s_conn_id,
         price      TYPE s_price,
         seatsocc_b TYPE s_seatsocc,
         seatsocc_f TYPE s_seatsocc,
         paymentsum TYPE s_price, " Campo calcolato
       END OF ty_volo_calcolato.

" Tipi per l'esempio con FOR
TYPES: BEGIN OF ty_nave,
         tknum TYPE tknum,
         nome  TYPE ernam,
         citta TYPE ort01,
         rotta TYPE route,
       END OF ty_nave.

TYPES: ty_navi      TYPE SORTED TABLE OF ty_nave WITH UNIQUE KEY tknum.
TYPES: ty_elenco_citta TYPE STANDARD TABLE OF ort01 WITH EMPTY KEY.

" Tipi per l'esempio con VALUE (sintassi robusta)
TYPES: BEGIN OF ty_riga_valore,
         valore TYPE i,
       END OF ty_riga_valore.
TYPES: t_tab_valori TYPE STANDARD TABLE OF ty_riga_valore WITH DEFAULT KEY.


"----------------------------------------------------------------------
" SEZIONE: SCHERMATA DI SELEZIONE
"----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blocco1 WITH FRAME TITLE TEXT-001.
  " Esempio di Search Help elementare. L'oggetto 'ZSH' deve essere
  " creato manualmente nella transazione SE11.
  PARAMETERS: p_ordven LIKE vbak-vbeln MATCHCODE OBJECT zsh.
SELECTION-SCREEN END OF BLOCK blocco1.


"----------------------------------------------------------------------
" EVENTO: START-OF-SELECTION
"----------------------------------------------------------------------
START-OF-SELECTION.

  "--------------------------------------------------------------------
  " Esempio 1: Formattazione dell'output con String Templates
  "--------------------------------------------------------------------
  ULINE.
  WRITE / 'FORMATTAZIONE OUTPUT'.

  WRITE / |{ 'testo allineato a sinistra' WIDTH = 80 ALIGN = LEFT }|.
  WRITE / |{ 'testo allineato al centro' WIDTH = 80 ALIGN = CENTER }|.
  WRITE / |{ 'testo allineato a destra'  WIDTH = 80 ALIGN = RIGHT }|.

  SKIP.
  WRITE / 'Casi di testo con CL_ABAP_FORMAT:'.
  WRITE / |{ 'SAPnuts' CASE = (cl_abap_format=>c_raw)   }|. " Non modificato
  WRITE / |{ 'SAPnuts' CASE = (cl_abap_format=>c_upper) }|. " MAIUSCOLO
  WRITE / |{ 'SAPnuts' CASE = (cl_abap_format=>c_lower) }|. " minuscolo

  "--------------------------------------------------------------------
  " Esempio 2: Conversione ALPHA
  "--------------------------------------------------------------------
  ULINE.
  WRITE:/ 'Conversione ALPHA'.

  DATA(lv_numero) = '0000012345'.
  WRITE: / 'Numero originale:', lv_numero.
  WRITE / |Dopo ALPHA = OUT: { lv_numero ALPHA = OUT }|.

  "--------------------------------------------------------------------
  " Esempio 3: Formattazione DATA
  "--------------------------------------------------------------------
  ULINE.
  WRITE:/ 'Formattazione DATA'.

  DATA(lv_data) = sy-datum.
  WRITE / |Formato ISO: { lv_data DATE = ISO }|.
  WRITE / |Formato UTENTE: { lv_data DATE = USER }|.
  WRITE / |Formato LINGUA: { lv_data DATE = ENVIRONMENT }|.

  "--------------------------------------------------------------------
  " Esempio 4: Calcoli in una query SQL
  "--------------------------------------------------------------------
  ULINE.
  WRITE:/ 'Calcoli in SELECT'.

  DATA lt_voli_calcolati TYPE STANDARD TABLE OF ty_volo_calcolato.

  CONSTANTS: lc_carrid TYPE s_carr_id VALUE 'UA',
             lc_connid TYPE s_conn_id VALUE '941'.

  SELECT carrid, connid, price, seatsocc_b, seatsocc_f,
         ( ( seatsocc_b + seatsocc_f ) * price ) AS paymentsum
    FROM sflight
    WHERE carrid = @lc_carrid
      AND connid = @lc_connid
    INTO TABLE @lt_voli_calcolati.

  IF sy-subrc = 0.
    WRITE / 'Risultati del calcolo sui voli:'.
    LOOP AT lt_voli_calcolati INTO DATA(ls_volo_calcolato).
      WRITE: / ls_volo_calcolato-carrid,
               ls_volo_calcolato-connid,
               ls_volo_calcolato-paymentsum.
    ENDLOOP.
  ENDIF.

  "--------------------------------------------------------------------
  " Esempio 5: Costruttore VALUE per tabelle interne
  "--------------------------------------------------------------------
  ULINE.
  WRITE:/ 'Costruttore VALUE'.

  DATA(lt_tabella_valori) = VALUE t_tab_valori(
                                  ( valore = 10 )
                                  ( valore = 20 )
                                  ( valore = 30 )
                               ).

  WRITE / 'Contenuto tabella creata con VALUE (sintassi robusta):'.
  LOOP AT lt_tabella_valori INTO DATA(ls_valore).
    WRITE / ls_valore-valore.
  ENDLOOP.


  "--------------------------------------------------------------------
  " Esempio 6: Costruttore FOR per tabelle interne
  "--------------------------------------------------------------------
  ULINE.
  WRITE:/ 'Costruttore FOR'.

  DATA gt_navi TYPE ty_navi.

  gt_navi = VALUE #(
    ( tknum = '001' nome = 'ROSSI' citta = 'AMBURGO' rotta = 'R0001' )
    ( tknum = '002' nome = 'VERDI' citta = 'BREMA'   rotta = 'R0002' )
    ( tknum = '003' nome = 'BIANCHI' citta = 'GENOVA'  rotta = 'R0001' )
  ).

  DATA(gt_citta) = VALUE ty_elenco_citta( FOR ls_nave IN gt_navi
                                          WHERE ( rotta = 'R0001' )
                                          ( ls_nave-citta ) ).

  IF gt_citta IS NOT INITIAL.
    WRITE / 'Città estratte con FOR (rotta R0001):'.
    LOOP AT gt_citta INTO DATA(lv_citta).
      WRITE / lv_citta.
    ENDLOOP.
  ELSE.
    WRITE / 'Nessuna città trovata per la rotta R0001.'.
  ENDIF.


  "--------------------------------------------------------------------
  " Esempio 7: Operatore di concatenazione &&
  "--------------------------------------------------------------------
  ULINE.
  WRITE:/ 'Operatore di concatenazione &&'.

  DATA: lv_var1 TYPE char30,
        lv_var2 TYPE char30,
        lv_var3 TYPE char30.
  DATA: lv_risultato TYPE string.

  lv_var1 = 'Costruire'.
  lv_var2 = 'una'.
  lv_var3 = 'Stringa'.

  lv_risultato = lv_var1 && ' ' && lv_var2 && ' ' && lv_var3 && '.'.

  WRITE: / 'Stringa concatenata:', lv_risultato.

  "--------------------------------------------------------------------
  " Esempio 8: Chiamata a metodo di un oggetto (Mismatch)
  "--------------------------------------------------------------------
  ULINE.
  WRITE:/ 'Chiamata a metodo di un oggetto (Spiegazione)'.
  WRITE / 'Vedi i commenti nel codice per la spiegazione dell''errore "Mismatch".'.


  "--------------------------------------------------------------------
  " Esempio 9: Lettura tramite chiave secondaria
  "--------------------------------------------------------------------
  ULINE.
  WRITE:/ 'Lettura tramite Chiave Secondaria'.

  DATA: lt_mara TYPE HASHED TABLE OF mara
          WITH UNIQUE KEY matnr
          WITH NON-UNIQUE SORTED KEY chiave_ordinata COMPONENTS bismt.
  DATA: lv_bismt TYPE bismt.

  SELECT * FROM mara INTO TABLE @lt_mara UP TO 50 ROWS.

  IF lt_mara IS NOT INITIAL.
    LOOP AT lt_mara INTO DATA(ls_mara_temp).
      lv_bismt = ls_mara_temp-bismt.
      EXIT.
    ENDLOOP.

    IF lv_bismt IS NOT INITIAL.
      WRITE: / 'Ricerca di un record con Bismt (chiave secondaria) =', lv_bismt.

      READ TABLE lt_mara INTO DATA(ls_mara)
                           WITH KEY chiave_ordinata COMPONENTS bismt = lv_bismt.

      IF sy-subrc = 0.
        WRITE: / 'Record trovato:', ls_mara-matnr.
      ELSE.
        WRITE / 'Nessun record trovato con questa chiave secondaria.'.
      ENDIF.
    ELSE.
      WRITE / 'Il campo Bismt nel primo record era vuoto, impossibile testare la ricerca.'.
    ENDIF.
  ELSE.
    WRITE / 'La tabella MARA è vuota, impossibile eseguire il test.'.
  ENDIF.

  "--------------------------------------------------------------------
  " Esempio 10: Funzioni Predicative (READ TABLE ... WITH KEY)
  "--------------------------------------------------------------------
  ULINE.
  WRITE:/ 'Funzioni Predicative (READ TABLE)'.

  DATA: lt_schedule TYPE STANDARD TABLE OF spfli.

  SELECT * FROM spfli INTO TABLE @lt_schedule.

  WRITE / 'Ricerca del volo AA-0017...'.
  READ TABLE lt_schedule TRANSPORTING NO FIELDS
                         WITH KEY carrid = 'AA'
                                  connid = '0017'.

  IF sy-subrc = 0.
    WRITE: / 'Record trovato alla riga (sy-tabix):', sy-tabix.
  ELSE.
    WRITE:/ 'La tabella non contiene il record cercato.'.
  ENDIF.

  "--------------------------------------------------------------------
  " Esempio 11: Formattazione Larghezza (WIDTH) in String Templates
  "--------------------------------------------------------------------
  ULINE.
  WRITE:/ 'Opzione di formattazione WIDTH'.

  " Esempio A: Garantire una lunghezza fissa per una stringa.
  " Il risultato occuperà 15 caratteri, con 'Testo' all'inizio
  " e 10 spazi vuoti a seguire (padding a destra).
  DATA(lv_string_width) = |{ 'Testo' WIDTH = 15 }|.
  WRITE / |Esempio A (tra apici per vedere gli spazi): '{ lv_string_width }'|.

  " Esempio B: WIDTH viene ignorato se troppo piccolo.
  " La stringa 'Esempio Lungo' ha 13 caratteri. WIDTH = 10 viene ignorato
  " perché il contenuto è più lungo della larghezza specificata.
  DATA(lv_string_ignored) = |{ 'Esempio Lungo' WIDTH = 10 }|.
  WRITE / |Esempio B (WIDTH ignorato): '{ lv_string_ignored }'|.



`;