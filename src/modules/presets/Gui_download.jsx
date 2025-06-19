export const content = `REPORT zdemo_gui_download_template.

"**********************************************************************
"* *
"* DICHIARAZIONI GLOBALI E TIPI DI DATI                     *
"* *
"**********************************************************************

TYPES: BEGIN OF ty_dati_vendita,
         vbeln  TYPE vbak-vbeln, " Numero Documento di vendita
         erdat  TYPE vbak-erdat, " Data di creazione
         auart  TYPE vbak-auart, " Tipo documento di vendita
         posnr  TYPE vbap-posnr, " Numero posizione dell'ordine
         matnr  TYPE vbap-matnr, " Codice materiale
         kwmeng TYPE vbap-kwmeng," Quantità ordinata nella posizione
       END OF ty_dati_vendita.

DATA: gt_dati_vendita TYPE STANDARD TABLE OF ty_dati_vendita,
      gs_dati_vendita TYPE ty_dati_vendita.

"**********************************************************************
"* *
"* SCHERMATA DI SELEZIONE                            *
"* *
"**********************************************************************

" Schermata di selezione rimossa.


"**********************************************************************
"* *
"* LOGICA PRINCIPALE DEL PROGRAMMA                     *
"* *
"**********************************************************************

START-OF-SELECTION.
  PERFORM f_seleziona_dati.

  IF gt_dati_vendita IS NOT INITIAL.
    PERFORM f_esporta_con_gui_download.
  ELSE.
    MESSAGE 'Nessun dato di vendita trovato nel sistema.' TYPE 'I'.
  ENDIF.


"**********************************************************************
"* *
"* DEFINIZIONE DELLE FORM                       *
"* *
"**********************************************************************

"----------------------------------------------------------------------
" FORM f_seleziona_dati
"----------------------------------------------------------------------
FORM f_seleziona_dati.
  " La query viene limitata ai primi 100 record trovati tramite
  " l'aggiunta 'UP TO 100 ROWS' per motivi di sicurezza e performance.
  SELECT vbak~vbeln, vbak~erdat, vbak~auart,
         vbap~posnr, vbap~matnr, vbap~kwmeng
    FROM vbak
    INNER JOIN vbap ON vbak~vbeln = vbap~vbeln
    UP TO 100 ROWS
    INTO TABLE @gt_dati_vendita.

  IF sy-subrc <> 0 AND sy-subrc <> 4. " sy-subrc = 4 è ok con UP TO...
    MESSAGE 'Errore durante la selezione dei dati dal database.' TYPE 'E'.
  ENDIF.
ENDFORM.

"----------------------------------------------------------------------
" FORM f_esporta_con_gui_download
"----------------------------------------------------------------------
FORM f_esporta_con_gui_download.
  DATA: lt_dati_per_download  TYPE STANDARD TABLE OF string,
        lv_riga_stringa       TYPE string,
        lv_intestazione       TYPE string,
        lv_nome_file_proposto TYPE string,
        lv_lunghezza_file     TYPE i,
        lv_quantita_char(17)  TYPE c.

  CONSTANTS: gc_separatore TYPE c VALUE cl_abap_char_utilities=>horizontal_tab.

  " 1. Costruisce la riga di intestazione
  CONCATENATE 'Numero Documento' 'Data Creazione' 'Tipo Documento'
              'Posizione' 'Materiale' 'Quantità Ordinata'
         INTO lv_intestazione SEPARATED BY gc_separatore.
  APPEND lv_intestazione TO lt_dati_per_download.

  " 2. Converte la tabella dati in una tabella di stringhe
  LOOP AT gt_dati_vendita INTO gs_dati_vendita.
    CLEAR: lv_riga_stringa, lv_quantita_char.
    WRITE gs_dati_vendita-kwmeng TO lv_quantita_char.
    CONDENSE lv_quantita_char.
    CONCATENATE gs_dati_vendita-vbeln gs_dati_vendita-erdat
                gs_dati_vendita-auart gs_dati_vendita-posnr
                gs_dati_vendita-matnr lv_quantita_char
           INTO lv_riga_stringa SEPARATED BY gc_separatore.
    CONDENSE lv_riga_stringa.
    APPEND lv_riga_stringa TO lt_dati_per_download.
  ENDLOOP.

  " 3. Propone un nome per il file
  lv_nome_file_proposto = 'ReportVendite.xls'.

  " 4. Chiama la funzione di download con tutti i parametri documentati
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
*"    BIN_FILESIZE                =
"     Commento: Da usare solo con filetype 'BIN'. Specifica la dimensione
"               del file binario da scaricare.

      filename                    = lv_nome_file_proposto
"     Commento: OBBLIGATORIO. Stringa contenente il percorso e il nome del
"               file da creare sul PC locale. Se si fornisce solo il nome,
"               verrà aperta una finestra di dialogo per la scelta del percorso.

      filetype                    = 'ASC'
"     Commento: Tipo di file da scaricare. Valori comuni:
"               'ASC': Testo ASCII. I campi di una riga vengono concatenati.
"                      È il tipo che usiamo noi, avendo preparato una tabella di stringhe.
"               'BIN': Binario. La tabella dati deve essere di tipo XSTRING.
"               'DAT': Formato dati adattabile. Scarica la tabella strutturata
"                      e usa il separatore di default (TAB).

*"    APPEND                      = ' '
"     Commento: Se impostato a 'X', i dati vengono aggiunti in coda a un
"               file esistente invece di sovrascriverlo.

*"    WRITE_FIELD_SEPARATOR       = ' '
"     Commento: Se impostato a 'X' e filetype è 'ASC' o 'DAT', inserisce
"               un tabulatore tra le colonne di una tabella dati STRUTTURATA.

*"    CONFIRM_OVERWRITE           = ' '
"     Commento: Se impostato a 'X', chiede esplicitamente conferma all'utente
"               se il file specificato in 'filename' esiste già.

*"    CODEPAGE                    = ' '
"     Commento: Specifica la codifica di caratteri del file di output.
"               Esempio: '4110' per UTF-8. Se lasciato vuoto, usa la codifica
"               di default del frontend.

*"    WRITE_BOM                   = ' '
"     Commento: Se 'X', scrive un "Byte Order Mark" all'inizio del file.
"               È importante per i file Unicode (es. UTF-8) per permettere
"               ai programmi come Excel di interpretarli correttamente.

*"    SHOW_TRANSFER_STATUS        = ABAP_TRUE
"     Commento: Se impostato a ABAP_TRUE ('X'), mostra una barra di avanzamento
"               durante il download, utile per file di grandi dimensioni.

    IMPORTING
      filelength                  = lv_lunghezza_file
"     Commento: Ritorna la dimensione in byte del file creato.
"               La variabile (es. lv_lunghezza_file) deve essere di tipo I.

    TABLES
      data_tab                    = lt_dati_per_download
"     Commento: OBBLIGATORIO. È la tabella interna che contiene i dati
"               da scaricare. Nel nostro caso, è una tabella di stringhe.

*"    FIELDNAMES                  =
"     Commento: Usato insieme a filetype 'DAT' o 'ASC' con WRITE_FIELD_SEPARATOR.
"               È una tabella di stringhe contenente i nomi delle intestazioni
"               di colonna da scrivere come prima riga del file.

    EXCEPTIONS
      file_write_error          = 1
      no_batch                  = 2
      gui_refuse_filetransfer   = 3
      invalid_type              = 4
      no_authority              = 5
      unknown_error             = 6
      header_not_allowed        = 7
      separator_not_allowed     = 8
      filesize_not_allowed      = 9
      header_too_long           = 10
      dp_error_create           = 11
      dp_error_send             = 12
      dp_error_write            = 13
      unknown_dp_error          = 14
      access_denied             = 15
      dp_out_of_memory          = 16
      disk_full                 = 17
      dp_timeout                = 18
      file_not_found            = 19
      dataprovider_exception    = 20
      control_flush_error       = 21
      OTHERS                    = 22.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    WRITE: / 'Download di', sy-dbcnt, 'record di test completato. Lunghezza file:', lv_lunghezza_file, 'byte.'.
  ENDIF.
ENDFORM.
`;
