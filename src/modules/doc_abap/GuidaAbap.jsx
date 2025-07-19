export const content = `
# Guida SAP ABAP - Tipi di Dati e Dichiarazioni
==============================================

## Sezione 1: Tipi di Dati di Base
----------------------------------

In ABAP, i tipi di dati di base sono predefiniti e utilizzati per dichiarare variabili che contengono singoli valori. I tipi più comuni includono:

### 1.1. Tipi Numerici
* **I**: Intero a 4 byte. Utilizzato per numeri interi.
* **F**: Numero in virgola mobile (Float). Utilizzato per numeri con decimali.

### 1.2. Tipi Testuali
* **STRING**: Stringa di lunghezza variabile.
* **C**: Carattere. Stringa di lunghezza fissa, definita al momento della dichiarazione.

### 1.3. Tipi Temporali
* **D**: Data (formato YYYYMMDD).
* **T**: Ora (formato HHMMSS).

### 1.4. Tipi Binari
* **XSTRING**: Stringa di byte di lunghezza variabile.

### Esempio di Dichiarazione dei Tipi Base
\`abap
DATA gv_intero TYPE i.
DATA gv_prezzo TYPE f.
DATA gv_testo TYPE string.
DATA gv_codice TYPE c LENGTH 10.
DATA gv_data TYPE d.
DATA gv_ora TYPE t.
DATA gv_raw_data TYPE xstring.
\`

---

## Sezione 2: Dichiarazione delle Variabili
--------------------------------------------

La dichiarazione delle variabili in ABAP avviene utilizzando la parola chiave DATA. È buona pratica assegnare un nome mnemonico e specificare sempre il tipo di dato.

### 2.1. Sintassi di Base
\`abap
DATA nome_variabile TYPE tipo_dato.
\`

### 2.2. Dichiarazione In Linea (ABAP 7.40+)
È anche possibile dichiarare variabili in linea (inline declaration) a partire da ABAP 7.40, il che rende il codice più compatto e leggibile.

**Esempio con inline declaration:**
\`abap
DATA(lo_oggetto) = new zcl_mia_classe( ).
\`

### 2.3. Esempi Pratici di Dichiarazione
\`abap
" Dichiarazione classica
DATA gv_contatore TYPE i.
DATA gv_messaggio TYPE string.

" Assegnazione di un valore iniziale
DATA gv_status TYPE c LENGTH 1 VALUE 'A'.

" Dichiarazione di una struttura
DATA: BEGIN OF gs_indirizzo,
        via TYPE string,
        citta TYPE string,
        cap TYPE c LENGTH 5,
      END OF gs_indirizzo.

" Dichiarazione di una tabella interna
DATA gt_utenti TYPE TABLE OF string.

" Dichiarazione in linea (ABAP 7.40+)
SELECT * FROM mara INTO TABLE @DATA(lt_materiali).
\`

## Sezione 3: Strutture (Work Areas)

-----

Una struttura è un gruppo di campi correlati, trattati come una singola unità. È usata per contenere una singola riga di una tabella o un insieme logico di dati.

### 3.1. Definizione di Tipi Strutturati

È una best practice definire prima un tipo di dato strutturato usando TYPES e poi dichiarare una variabile (work area) di quel tipo.

\`abap
" Definizione di un tipo per i dati anagrafici
TYPES: BEGIN OF ty_s_anagrafica,
         id          TYPE i,
         nome        TYPE string,
         cognome     TYPE string,
         data_nascita TYPE d,
       END OF ty_s_anagrafica.

" Dichiarazione di una work area (struttura) basata sul tipo
DATA ls_dati_utente TYPE ty_s_anagrafica.

" Accesso e valorizzazione dei campi
ls_dati_utente-nome = 'Mario'.
ls_dati_utente-cognome = 'Rossi'.
\`

### 3.2. Dichiarazione basata sul Dizionario Dati (SE11)

Il modo più comune e robusto per dichiarare una struttura è basarsi su una tabella o struttura esistente nel Dizionario ABAP.

\`abap
" Dichiara una work area con la stessa struttura della tabella KNA1
DATA ls_cliente TYPE kna1.
\`

## Sezione 4: Tabelle Interne

-----

Una tabella interna è un oggetto che contiene una collezione di righe con la stessa struttura. Sono l'elemento principale per l'elaborazione di dati in massa in ABAP.

### 4.1. Tipi di Tabelle

  * **STANDARD TABLE**: La tabella più generica. Le righe non sono ordinate in modo intrinseco. L'accesso può avvenire tramite un indice numerico o una chiave (con performance non ottimali su tabelle grandi).
  * **SORTED TABLE**: Le righe sono sempre mantenute ordinate in base a una chiave univoca o non univoca. L'accesso tramite la chiave è molto veloce grazie alla ricerca binaria.
  * **HASHED TABLE**: Le righe non hanno un ordine. L'accesso avviene tramite un algoritmo di hash sulla chiave, che deve essere **univoca**. Questo garantisce il tempo di accesso più rapido possibile, indipendente dal numero di righe. **Regola di ambiente**: l'accesso è permesso solo tramite chiave (WITH KEY), non tramite indice numerico.

### 4.2. Esempio di Dichiarazione e Uso

\`abap
" 1. Definizione di un tipo per la tabella interna
TYPES: ty_t_anagrafica TYPE STANDARD TABLE OF ty_s_anagrafica WITH NON-UNIQUE KEY id.

" 2. Dichiarazione della tabella interna
DATA lt_elenco_utenti TYPE ty_t_anagrafica.

" 3. Dichiarazione di una work area per il loop
DATA ls_utente TYPE ty_s_anagrafica.

" 4. Aggiunta di una riga alla tabella
ls_utente-id = 1.
ls_utente-nome = 'Mario'.
APPEND ls_utente TO lt_elenco_utenti.

" 5. Elaborazione con un LOOP
LOOP AT lt_elenco_utenti INTO ls_utente.
  WRITE: / ls_utente-id, ls_utente-nome.
ENDLOOP.
\`

### 4.3. Regole di Loop con Field-Symbol

L'uso di ASSIGNING con un field-symbol è più performante perché evita di copiare i dati in una work area. **Regola di ambiente**: Non usare il field-symbol nella clausola WHERE di un LOOP. Il filtro deve essere fatto con IF o CHECK all'interno del loop.

\`abap
FIELD-SYMBOLS: <ls_utente> TYPE ty_s_anagrafica.

" Sintassi ERRATA ❌
" LOOP AT lt_elenco_utenti ASSIGNING <ls_utente> WHERE id > 10.

" Sintassi CORRETTA ✅
LOOP AT lt_elenco_utenti ASSIGNING <ls_utente>.
  CHECK <ls_utente>-id > 10. " Oppure IF...
  " ... La logica qui viene eseguita solo per le righe che soddisfano la condizione
ENDLOOP.
\`


## Sezione 5: Selezione Dati con Open SQL

-----

La selezione dei dati dal database deve seguire regole di sintassi molto rigide in questo ambiente.

### 5.1. Regole di Sintassi Fondamentali

  * **Separatore a Virgola**: È obbligatorio usare la virgola (,) per separare i campi nella SELECT list.
  * **INTO alla Fine**: La clausola INTO (o APPENDING) deve sempre essere posizionata **alla fine** dell'istruzione SELECT.
  * **UP TO n ROWS dopo INTO**: La clausola UP TO n ROWS, se usata, deve trovarsi **dopo** la clausola INTO.
  * **JOIN vs FOR ALL ENTRIES**: Privilegiare JOIN se si selezionano campi da tutte le tabelle coinvolte. Usare FOR ALL ENTRIES se la tabella driver serve solo per il filtro (ricordandosi di verificare che non sia vuota prima della SELECT).

### 5.2. Esempi di Sintassi

\`abap
" Sintassi ERRATA ❌
SELECT campo1 campo2                " Manca la virgola
  INTO TABLE @DATA(lt_tabella)     " INTO è in posizione errata
  UP TO 10 ROWS                   " UP TO è in posizione errata
  FROM nome_tabella.

" Sintassi CORRETTA ✅
SELECT campo1,
       campo2,
       campo3
  FROM nome_tabella
  WHERE campo_filtro = 'VALORE'
  INTO TABLE @DATA(lt_tabella) " INTO è alla fine
    UP TO 10 ROWS.           " UP TO segue INTO
\`

### 5.3. Esempio di Sintassi con JOIN

\`abap
" Questo esempio mostra come unire più tabelle seguendo le regole.
" I nomi sono generici per illustrare la struttura.

SELECT t1~campo_chiave,
       t1~campo_dati,
       t2~campo_descrizione,
       t3~altro_campo AS alias_campo
  FROM tabella1 AS t1
 INNER JOIN tabella2 AS t2 ON t1~campo_chiave = t2~campo_chiave
 INNER JOIN tabella3 AS t3 ON t1~altra_chiave = t3~altra_chiave
 WHERE t1~campo_filtro = 'A'
  INTO CORRESPONDING FIELDS OF TABLE @DATA(lt_risultato).
\`

### 5.4. Best Practice per la Selezione Dati

  * Utilizzare sempre la virgola per separare i campi nella SELECT list.
  * Posizionare la clausola INTO alla fine dell'istruzione SELECT.
  * Verificare che le tabelle driver non siano vuote prima di usare FOR ALL ENTRIES.
  * Privilegiare JOIN quando si selezionano campi da tutte le tabelle coinvolte.
  * Utilizzare alias per le tabelle per migliorare la leggibilità del codice.
  
## 5.5. FOR ALL ENTRIES - Alternativa alle JOIN

### 5.5.1. Quando Utilizzare FOR ALL ENTRIES
FOR ALL ENTRIES è utile quando:
- Le tabelle non hanno relazioni dirette per una JOIN
- Serve maggiore controllo sui filtri applicati
- Si vogliono evitare prodotti cartesiani indesiderati

**Sintassi di base:**
\`abap
SELECT campo1, campo2
  FROM tabella2
  FOR ALL ENTRIES IN lt_tabella_driver
  WHERE campo_chiave = lt_tabella_driver-campo_chiave
  INTO TABLE lt_risultato.
\`

### 5.5.2. Esempio Pratico con FOR ALL ENTRIES
\`abap
" Prima: Selezione tabella driver
SELECT vbeln, kunnr
  FROM vbak
  WHERE audat >= '20240101'
  INTO TABLE @DATA(lt_ordini).

" Controllo fondamentale: verificare che la tabella non sia vuota
IF lt_ordini IS NOT INITIAL.
  " Seconda: Selezione con FOR ALL ENTRIES
  SELECT vbeln, posnr, matnr, kwmeng
    FROM vbap
    FOR ALL ENTRIES IN lt_ordini
    WHERE vbeln = lt_ordini-vbeln
    INTO TABLE @DATA(lt_posizioni).
ENDIF.
\`

### 5.5.3. Regole Critiche per FOR ALL ENTRIES

**⚠️ CONTROLLO OBBLIGATORIO:**
\`abap
" ❌ ERRORE - Senza controllo
SELECT campo FROM tabella
  FOR ALL ENTRIES IN lt_driver
  WHERE chiave = lt_driver-chiave
  INTO TABLE lt_result.
" Se lt_driver è vuota, vengono estratti TUTTI i record!

" ✅ CORRETTO - Con controllo
IF lt_driver IS NOT INITIAL.
  SELECT campo FROM tabella
    FOR ALL ENTRIES IN lt_driver
    WHERE chiave = lt_driver-chiave
    INTO TABLE lt_result.
ENDIF.
\`

**Regole aggiuntive:**
- Non usare DISTINCT con FOR ALL ENTRIES
- I duplicati vengono automaticamente rimossi
- La tabella driver deve avere almeno un record

## 5.6. Confronto JOIN vs FOR ALL ENTRIES

### 5.6.1. Tabella Comparativa

| Aspetto | JOIN | FOR ALL ENTRIES |
|---------|------|----------------|
| **Performance** | Migliore per grandi volumi | Migliore per piccoli volumi |
| **Flessibilità** | Limitata dalle relazioni | Maggiore libertà nei filtri |
| **Complessità** | Sintassi più semplice | Richiede controlli aggiuntivi |
| **Duplicati** | Possibili prodotti cartesiani | Eliminazione automatica |
| **Controlli** | Nessuno richiesto | Controllo tabella vuota obbligatorio |

### 5.6.2. Quando Scegliere JOIN
\`abap
" ✅ Usa JOIN quando:
" - Relazioni dirette tra tabelle
" - Serve tutti i dati da tutte le tabelle
" - Volume dati elevato
" - Struttura query semplice

SELECT v~vbeln,
       v~audat,
       p~posnr,
       p~matnr
  FROM vbak AS v
  INNER JOIN vbap AS p ON v~vbeln = p~vbeln
  WHERE v~audat >= '20240101'
 INTO CORRESPONDING FIELDS OF TABLE @DATA(lt_ordini_completi).
\`

### 5.6.3. Quando Scegliere FOR ALL ENTRIES
\`abap
" ✅ Usa FOR ALL ENTRIES quando:
" - Logica di filtro complessa sulla tabella driver
" - Non servono tutti i campi da tutte le tabelle
" - Controllo granulare sui dati estratti

" Prima estrazione con logica complessa
SELECT vbeln, kunnr
  FROM vbak
  WHERE audat BETWEEN '20240101' AND '20241231'
    AND vkorg = '1000'
    AND vtweg IN ('10', '20')
    AND spart = '00'
  INTO TABLE @DATA(lt_ordini_filtrati).

IF lt_ordini_filtrati IS NOT INITIAL.
  " Seconda estrazione solo per gli ordini che passano i filtri
  SELECT kunnr, name1, ort01
    FROM kna1
    FOR ALL ENTRIES IN lt_ordini_filtrati
    WHERE kunnr = lt_ordini_filtrati-kunnr
    INTO TABLE @DATA(lt_clienti).
ENDIF.
\`

## 5.7. Ottimizzazione delle Query

### 5.7.1. Indici Database
\`abap
" ✅ OTTIMIZZATO - Usa campi dell'indice primario o secondario
SELECT vbeln, audat, kunnr
  FROM vbak
  WHERE vbeln IN s_vbeln     " Campo chiave primaria
  INTO TABLE @DATA(lt_ordini).

" ❌ NON OTTIMIZZATO - Campo senza indice
SELECT vbeln, audat, kunnr
  FROM vbak
  WHERE bstnk = 'ABC123'     " Campo senza indice
  INTO TABLE @DATA(lt_ordini_lenti).
\`

### 5.7.2. Limitazione Record
\`abap
" Usa UP TO per limitare i record estratti
SELECT vbeln, audat, kunnr
  FROM vbak
  WHERE audat >= '20240101'
  INTO CORRESPONDING FIELDS OF TABLE @DATA(lt_ordini)
  UP TO 1000 ROWS.

" Per paginazione
DATA: lv_offset TYPE i VALUE 0,
      lv_limit  TYPE i VALUE 100.

SELECT vbeln, audat, kunnr
  FROM vbak
  WHERE audat >= '20240101'
  ORDER BY vbeln
  INTO CORRESPONDING FIELDS OF TABLE@DATA(lt_pagina)
  OFFSET @lv_offset ROWS
  FETCH NEXT @lv_limit ROWS ONLY.
\`

### 5.7.3. Proiezione Campi
\`abap
" ✅ CORRETTO - Solo campi necessari
SELECT vbeln, audat
  FROM vbak
  WHERE kunnr = '1000'
  INTO CORRESPONDING FIELDS OF TABLE @DATA(lt_essenziale).

" ❌ EVITARE - SELECT * quando non necessario
SELECT *
  FROM vbak
  WHERE kunnr = '1000'
  INTO CORRESPONDING FIELDS OF TABLE @DATA(lt_tutto).  " Spreco di risorse
\`

## 5.8. Gestione Errori nelle SELECT

### 5.8.1. Controllo SY-SUBRC
\`abap
SELECT SINGLE vbeln, audat, kunnr
  FROM vbak
  WHERE vbeln = '1000000001'
  INTO @DATA(ls_ordine).

IF sy-subrc = 0.
  " Record trovato
  MESSAGE 'Ordine trovato' TYPE 'S'.
ELSE.
  " Record non trovato
  MESSAGE 'Ordine non esistente' TYPE 'E'.
ENDIF.
\`

### 5.8.2. Controllo Tabelle Vuote
\`abap
SELECT vbeln, audat, kunnr
  FROM vbak
  WHERE audat >= '20240101'
  INTO TABLE @DATA(lt_ordini).

IF sy-subrc = 0 AND lines( lt_ordini ) > 0.
  " Elaborazione dati
  LOOP AT lt_ordini INTO DATA(ls_ordine).
    " Processamento record
  ENDLOOP.
ELSE.
  MESSAGE 'Nessun ordine trovato per il periodo' TYPE 'I'.
ENDIF.
\`

## 5.9. Esempio Completo: Query Complessa Multi-Tabella

\`abap
" Definizione struttura risultato
TYPES: BEGIN OF ty_ordine_dettaglio,
         numero_ordine    TYPE vbak-vbeln,
         data_ordine      TYPE vbak-audat,
         tipo_ordine      TYPE vbak-auart,
         cliente          TYPE vbak-kunnr,
         nome_cliente     TYPE kna1-name1,
         città_cliente    TYPE kna1-ort01,
         posizione        TYPE vbap-posnr,
         materiale        TYPE vbap-matnr,
         descrizione      TYPE makt-maktx,
         quantità         TYPE vbap-kwmeng,
         valore           TYPE vbap-netwr,
         stato_ordine     TYPE vbuk-wbstk,
       END OF ty_ordine_dettaglio.

DATA: lt_ordini_completi TYPE TABLE OF ty_ordine_dettaglio.

" Query ottimizzata con JOIN multiple
SELECT v~vbeln AS numero_ordine,
       v~audat AS data_ordine,
       v~auart AS tipo_ordine,
       v~kunnr AS cliente,
       k~name1 AS nome_cliente,
       k~ort01 AS città_cliente,
       p~posnr AS posizione,
       p~matnr AS materiale,
       m~maktx AS descrizione,
       p~kwmeng AS quantità,
       p~netwr AS valore,
       s~wbstk AS stato_ordine
  FROM vbak AS v
  INNER JOIN kna1 AS k ON v~kunnr = k~kunnr
  INNER JOIN vbap AS p ON v~vbeln = p~vbeln
  LEFT OUTER JOIN makt AS m ON p~matnr = m~matnr
                           AND m~spras = @sy-langu
  LEFT OUTER JOIN vbuk AS s ON v~vbeln = s~vbeln
  WHERE v~audat BETWEEN '20240101' AND '20241231'
    AND v~vkorg = '1000'
    AND v~vtweg IN ('10', '20')
  ORDER BY v~vbeln, p~posnr
  INTO CORRESPONDING FIELDS OF TABLE lt_ordini_completi.

" Controllo risultato
IF sy-subrc = 0.
  MESSAGE |Estratti { lines( lt_ordini_completi ) } record| TYPE 'S'.
  
  " Elaborazione risultati
  LOOP AT lt_ordini_completi INTO DATA(ls_dettaglio).
    " Logica di business sui dati estratti
    WRITE: / ls_dettaglio-numero_ordine,
           ls_dettaglio-nome_cliente,
           ls_dettaglio-quantità.
  ENDLOOP.
  
ELSE.
  MESSAGE 'Nessun dato trovato per i criteri specificati' TYPE 'I'.
ENDIF.
\`

## 5.10. Checklist Best Practice per le SELECT

### ✅ Da Fare SEMPRE:
-  Usare alias per tutte le tabelle in JOIN
-  Qualificare i campi con tabella~campo nelle JOIN  
-  Controllare sy-subrc dopo le SELECT
-  Verificare tabelle vuote prima di FOR ALL ENTRIES
-  Usare INTO CORRESPONDING per mapping flessibile
-  Limitare i campi selezionati al necessario
-  Aggiungere UP TO per limitare record
-  Formattare il codice per leggibilità

### ❌ Da EVITARE:
-  SELECT * quando non necessario
-  JOIN senza qualificatori di campo
-  FOR ALL ENTRIES senza controllo tabella vuota
-  Query senza indici appropriati
-  Nomi di campo ambigui
-  Mancanza controllo errori
-  WHERE dinamiche non validate
-----
## Sezione 6: Programmazione Dinamica (RTTI)

-----

L'ambiente di sviluppo supporta la programmazione dinamica tramite RTTI (Run-Time Type Information), che permette di creare e manipolare dati la cui struttura non è nota a compile-time.

### 6.1. Creazione Dati Dinamica

È possibile creare oggetti dato (variabili, strutture, tabelle) la cui struttura è definita a runtime.

\`abap
DATA: lv_nome_tabella TYPE string,
      lr_tabella_dati TYPE REF TO data.
FIELD-SYMBOLS: <lt_tabella_dinamica> TYPE STANDARD TABLE.

lv_nome_tabella = 'KNA1'. " Il nome può arrivare da un parametro

" Crea una tabella interna il cui tipo è definito dal contenuto di lv_nome_tabella
CREATE DATA lr_tabella_dati TYPE TABLE OF (lv_nome_tabella).

" Assegna la tabella dati a un field-symbol per poterla usare
ASSIGN lr_tabella_dati->* TO <lt_tabella_dinamica>.

" Ora <lt_tabella_dinamica> è una tabella interna con la struttura di KNA1
" e può essere usata in una SELECT
SELECT *
  FROM (lv_nome_tabella)
  INTO TABLE @<lt_tabella_dinamica>
    UP TO 20 ROWS.
\`

### 6.2. Accesso Dinamico a Componenti

È possibile accedere ai campi di una struttura dinamicamente, senza conoscerne il nome a compile-time, usando i field-symbols.

\`abap
FIELD-SYMBOLS: <ls_struttura>    TYPE any,
               <lv_valore_campo> TYPE any.
DATA: lv_nome_campo TYPE string.

" Assumiamo che <ls_struttura> stia puntando a una work area (es. in un LOOP)
lv_nome_campo = 'NOME_DEL_CAMPO_DA_LEGGERE'.

ASSIGN COMPONENT lv_nome_campo OF STRUCTURE <ls_struttura> TO <lv_valore_campo>.
IF sy-subrc = 0.
  " Ora <lv_valore_campo> punta al valore del campo richiesto
  WRITE: / <lv_valore_campo>.
ENDIF.
\`

### 6.3. WHERE Dinamica in una SELECT

La clausola WHERE può essere costruita dinamicamente in una tabella interna di stringhe.

\`abap
DATA: lt_condizioni_where TYPE TABLE OF string,
      lv_condizione       TYPE string.

lv_condizione = campo_filtro = 'A'.
APPEND lv_condizione TO lt_condizioni_where.

lv_condizione = AND altro_campo > 100.
APPEND lv_condizione TO lt_condizioni_where.

SELECT campo1, campo2
  FROM nome_tabella
  WHERE (lt_condizioni_where) " Le parentesi sono obbligatorie
  INTO TABLE @DATA(lt_dati).
\`

-----
## Sezione 7: Schermata di Selezione (Selection-Screen)

-----

La SELECTION-SCREEN è l'interfaccia standard di un report ABAP, che permette all'utente di inserire i criteri di selezione prima dell'esecuzione.
**Regola di ambiente**: i nomi degli elementi della selection-screen (PARAMETERS, SELECT-OPTIONS, etc.) non possono superare gli **8 caratteri**.

### 7.1. PARAMETERS

Un PARAMETER definisce un singolo campo di input.

\`abap
" Input per un singolo valore, obbligatorio
PARAMETERS p_bukrs TYPE bukrs OBLIGATORY.

" Input per una data, con valore di default la data odierna
PARAMETERS p_data TYPE d DEFAULT sy-datum.

" Checkbox
PARAMETERS p_test AS CHECKBOX.

" Radio button
PARAMETERS: p_rad1 RADIOBUTTON GROUP grp1,
            p_rad2 RADIOBUTTON GROUP grp1 DEFAULT 'X'.
  \`

### 7.2. SELECT-OPTIONS

Un SELECT-OPTIONS definisce un intervallo di selezione complesso (da-a, valori singoli, esclusioni) per un campo.

\`abap
" Dichiara la tabella del dizionario per poter usare FOR
TABLES: kna1.

" Crea un intervallo di selezione per il codice cliente
SELECT-OPTIONS so_kunnr FOR kna1-kunnr.
\`

Il SELECT-OPTIONS crea una tabella interna (con testata) con i campi SIGN, OPTION, LOW e HIGH, che può essere usata direttamente nella clausola WHERE di una SELECT.

### 7.3. Elementi Grafici e Blocchi

È possibile organizzare la schermata per renderla più leggibile.

\`abap
SELECTION-SCREEN BEGIN OF BLOCK blocco1 WITH FRAME TITLE TEXT-001.
  PARAMETERS p_campo1 TYPE c LENGTH 10.
SELECTION-SCREEN END OF BLOCK blocco1.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(20) TEXT-002 FOR FIELD p_campo2.
  PARAMETERS p_campo2 TYPE i.
SELECTION-SCREEN END OF LINE.

" Pulsante sulla barra dell'applicazione
SELECTION-SCREEN PUSHBUTTON /10(20) btn_proc USER-COMMAND ucomm_proc.
\`

-----

## Sezione 8: Gestione Messaggi ed Errori

-----

La comunicazione con l'utente avviene tramite l'istruzione MESSAGE. La gestione degli errori si basa principalmente sul controllo della variabile di sistema sy-subrc.

### 8.1. L'istruzione MESSAGE

Visualizza un messaggio all'utente in una finestra di dialogo o nella status bar.
**Regola di ambiente**: Usare esclusivamente l'istruzione MESSAGE. Non usare classi di eccezione (TRY...CATCH) se non per contesti specifici OO come CL_SALV_TABLE.

### 8.2. Tipi di Messaggio

  * **'S' (Success)**: Messaggio di successo nella status bar. L'elaborazione continua.
  * **'I' (Information)**: Messaggio informativo in un popup. L'elaborazione continua dopo la conferma dell'utente.
  * **'W' (Warning)**: Messaggio di avviso in un popup. L'elaborazione continua dopo la conferma.
  * **'E' (Error)**: Messaggio di errore. L'elaborazione si interrompe. L'utente deve correggere l'input.
  * **'A' (Abort)**: Messaggio di interruzione. L'elaborazione termina e si torna alla schermata iniziale.
  * **'X' (eXit/Dump)**: Genera un short dump. Da usare solo per debug in situazioni eccezionali.

<!-- end list -->

\`abap
MESSAGE 'Operazione completata con successo.' TYPE 'S'.
MESSAGE 'Dati non validi, correggere.' TYPE 'E'.
\`

### 8.3. Messaggi con Variabili e Transazioni

È possibile inserire fino a 4 variabili in un messaggio usando WITH. I messaggi dovrebbero essere definiti nella transazione SE91 (Message Class).

\`abap
" Esempio di messaggio da una classe di messaggi 'ZMSG' numero 001
" con testo: "Documento &1 per la società &2 non trovato."
MESSAGE e001(zmsg) WITH '100001' '1000'.

" Messaggio che, se cliccato (in certi contesti), avvia una transazione
" SET PARAMETER ID 'BES' FIELD lv_ebeln.
" CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
" Questo pattern è spesso usato dopo un messaggio informativo.
\`

### 8.4. Gestione di sy-subrc

Dopo la maggior parte delle istruzioni (chiamate a FUNCTION, SELECT, READ TABLE, etc.), la variabile di sistema sy-subrc viene impostata.

  * **sy-subrc = 0**: L'operazione è andata a buon fine.
  * **sy-subrc <> 0** (di solito 4 o 8): L'operazione non è andata a buon fine o non ha trovato risultati.

<!-- end list -->

\`abap
READ TABLE lt_elenco_utenti WITH KEY id = 10 INTO ls_utente.
IF sy-subrc = 0.
  " Riga trovata, procedi con l'elaborazione
ELSE.
  " Riga non trovata, gestisci l'errore
  MESSAGE 'Utente con ID 10 non trovato.' TYPE 'E'.
ENDIF.
\`

-----

## Sezione 9: Modularizzazione del Codice

-----

Per creare codice pulito, manutenibile e riutilizzabile, è obbligatorio modularizzare la logica.

### 9.1. Struttura del Programma con INCLUDE

Un report eseguibile deve essere suddiviso in file INCLUDE per separare le diverse parti del codice.

  * **Programma Principale (Z_NOME_REPORT)**: Contiene solo le istruzioni INCLUDE.
  * **Z_NOME_REPORT_TOP**: Contiene TUTTE le dichiarazioni globali: TABLES, TYPES, DATA, CONSTANTS, FIELD-SYMBOLS e la SELECTION-SCREEN.
  * **Z_NOME_REPORT_F01** (e successivi): Contengono le implementazioni dei sottoprogrammi (FORM).

### 9.2. Sottoprogrammi (FORM / ENDFORM)

## Le FORM sono blocchi di codice riutilizzabili all'interno di un programma.

\`abap
" Chiamata al sottoprogramma
PERFORM f_calcola_totale
  USING
    lv_valore1
    lv_valore2
  CHANGING
    lv_risultato.

" Definizione del sottoprogramma
FORM f_calcola_totale
  USING
    iv_valore1 TYPE i
    iv_valore2 TYPE i
  CHANGING
    cv_totale  TYPE i.

  cv_totale = iv_valore1 + iv_valore2.

ENDFORM.
\`

## Definizione di un sottoprogramma

\`abap
FORM nome_sottoprogramma
  [USING parametri_input] "VALORI SOLO LETTURA
  [CHANGING parametri_input_output] " VALORI IN LETTURA E SCRITTURA
  [TABLES parametri_tabella]. "PASSAGGIO TABELLA INTERNA IN LETTURA E SCRITTURA

  " Logica del sottoprogramma
  
ENDFORM.
\`


## Chiamata di un sottoprogramma

\`abap
PERFORM nome_sottoprogramma
  [USING valori_input] "VALORI SOLO LETTURA
  [CHANGING valori_input_output] " VALORI IN LETTURA E SCRITTURA
  [TABLES tabelle].
\`


## Tipi di passaggio parametri
## 1. USING - Parametri di input LETTURA NON SCRITTURA
## I parametri USING vengono passati per val LETTURA E SCRITTURA

\`abap
" Esempio completo
DATA: lv_numero1 TYPE i VALUE 10,
      lv_numero2 TYPE i VALUE 20,
      lv_risultato TYPE i.

" Chiamata
PERFORM calcola_somma
  USING lv_numero1
        lv_numero2
  CHANGING lv_risultato.

WRITE: / 'Risultato:', lv_risultato.

" Definizione
FORM calcola_somma
  USING iv_num1 TYPE i
        iv_num2 TYPE i
  CHANGING cv_risultato TYPE i.
  
  cv_risultato = iv_num1 + iv_num2.
  " iv_num1 e iv_num2 sono di sola lettura
  
ENDFORM.
\`

## 2. CHANGING - Parametri di input/output (pass by reference)
## I parametri CHANGING vengono passati per riferimento e possono essere sia letti che modificati all'interno della FORM.

\`abap
DATA: lv_valore TYPE i VALUE 100.

" Il valore verrà modificato dalla FORM
PERFORM raddoppia_valore
  CHANGING lv_valore.

WRITE: / 'Valore raddoppiato:', lv_valore. " Output: 200

FORM raddoppia_valore
  CHANGING cv_valore TYPE i.
  
  cv_valore = cv_valore * 2.
  " cv_valore può essere sia letto che modificato
  
ENDFORM.
\`

## 3. TABLES - Parametri per tabelle interne
## Il parametro TABLES viene utilizzato per passare tabelle interne al sottoprogramma.

\`abap
" Definizione di una struttura e tabella
TYPES: BEGIN OF ty_prodotto,
         id    TYPE i,
         nome  TYPE string,
         prezzo TYPE p DECIMALS 2,
       END OF ty_prodotto.

DATA: lt_prodotti TYPE TABLE OF ty_prodotto,
      ls_prodotto TYPE ty_prodotto.

" Popolamento tabella di esempio
ls_prodotto-id = 1.
ls_prodotto-nome = 'Laptop'.
ls_prodotto-prezzo = '999.99'.
APPEND ls_prodotto TO lt_prodotti.

ls_prodotto-id = 2.
ls_prodotto-nome = 'Mouse'.
ls_prodotto-prezzo = '25.50'.
APPEND ls_prodotto TO lt_prodotti.

" Chiamata con tabella
PERFORM elabora_prodotti
  TABLES lt_prodotti.

" Definizione che elabora la tabella
FORM elabora_prodotti
  TABLES pt_prodotti STRUCTURE ty_prodotto.
  
  DATA: ls_prodotto_local TYPE ty_prodotto.
  
  LOOP AT pt_prodotti INTO ls_prodotto_local.
    " Applicare uno sconto del 10%
    ls_prodotto_local-prezzo = ls_prodotto_local-prezzo * '0.9'.
    MODIFY pt_prodotti FROM ls_prodotto_local.
    
    WRITE: / 'Prodotto:', ls_prodotto_local-nome, 
           'Prezzo scontato:', ls_prodotto_local-prezzo.
  ENDLOOP.
  
ENDFORM.
\`
### 9.3. PERFORM Dinamica

È possibile chiamare una FORM il cui nome è definito a runtime in una variabile.
**Regola di ambiente**: La sintassi deve essere esplicita con IN PROGRAM.

\`abap
DATA: lv_nome_form TYPE string,
      lv_programma TYPE sy-repid.

lv_nome_form = 'F_SOTTOPROGRAMMA_DINAMICO'.
lv_programma = sy-repid.

PERFORM (lv_nome_form) IN PROGRAM (lv_programma).

" La FORM chiamata deve contenere il pragma per sopprimere l'avviso
" FORM f_sottoprogramma_dinamico.
"   "#EC CALLED
"   WRITE: / 'Chiamata dinamica eseguita'.
" ENDFORM.
\`

-----

## Sezione 10: Controllo di Flusso e Logica Condizionale

-----

Questi costrutti dirigono il flusso di esecuzione del programma.

### 10.1. IF / ELSEIF / ELSE

Il costrutto di base per le decisioni.

\`abap
IF lv_valore > 100.
  WRITE: / 'Valore alto'.
ELSEIF lv_valore > 50.
  WRITE: / 'Valore medio'.
ELSE.
  WRITE: / 'Valore basso'.
ENDIF.
\`

### 10.2. CASE

Utile quando si deve valutare una singola variabile contro più valori possibili.

\`abap
CASE lv_tipo_documento.
  WHEN 'A'.
    PERFORM f_gestisci_ordine.
  WHEN 'B'.
    PERFORM f_gestisci_offerta.
  WHEN OTHERS.
    MESSAGE 'Tipo documento non valido.' TYPE 'E'.
ENDCASE.
\`

### 10.3. Operatori Condizionali (Supporto Misto)

L'ambiente ha un supporto misto per la sintassi moderna.

  * **COND e SWITCH**: Sono supportati e possono essere usati per assegnazioni condizionali compatte.
  * **String Templates (|...| o &&)**: **NON supportati**. Usare CONCATENATE.

<!-- end list -->

\`abap
" Esempio con COND (supportato)
lv_descrizione_stato = COND string( WHEN lv_stato = 'A' THEN 'Attivo'
                                     WHEN lv_stato = 'C' THEN 'Chiuso'
                                     ELSE 'Sconosciuto' ).

" Esempio con CONCATENATE (obbligatorio al posto dei template)
CONCATENATE 'Il cliente' ls_cliente-name1 'vive a' ls_cliente-ort01
  INTO lv_messaggio SEPARATED BY space.
\`


# Guida Completa alle Keyword ABAP

## Dichiarazioni e Tipi di Dati

### DATA - Dichiarazione di Variabili
Utilizzata per dichiarare variabili locali o globali nel programma.

\`abap
DATA: gv_nome TYPE string,
      gv_eta TYPE i,
      gv_prezzo TYPE p DECIMALS 2.
\`

### TYPES - Definizione di Tipi Personalizzati
Permette di definire tipi di dati personalizzati per riutilizzarli nel programma.

\`abap
TYPES: BEGIN OF ty_cliente,
         id TYPE i,
         nome TYPE string,
         email TYPE string,
       END OF ty_cliente.

DATA: ls_cliente TYPE ty_cliente.
\`

### CONSTANTS - Dichiarazione di Costanti
Definisce valori costanti che non possono essere modificati durante l'esecuzione.

\`abap
CONSTANTS: gc_max_righe TYPE i VALUE 100,
          gc_stato_attivo TYPE c VALUE 'A'.
\`

### FIELD-SYMBOLS - Puntatori Dinamici
Utilizzati per accedere dinamicamente ai dati senza copiarli.

\`abap
FIELD-SYMBOLS: <fs_riga> TYPE any,
               <fs_campo> TYPE any.

ASSIGN gv_variabile TO <fs_riga>.
\`

## Strutture di Controllo

### IF/ELSE/ENDIF - Condizioni
Struttura condizionale per eseguire codice basato su condizioni.

\`abap
IF gv_eta >= 18.
  WRITE: 'Maggiorenne'.
ELSEIF gv_eta >= 13.
  WRITE: 'Adolescente'.
ELSE.
  WRITE: 'Minorenne'.
ENDIF.
\`

### CASE/WHEN/ENDCASE - Selezione Multipla
Struttura per gestire multiple condizioni basate su un singolo valore.

\`abap
CASE gv_stato.
  WHEN 'A'.
    WRITE: 'Attivo'.
  WHEN 'I'.
    WRITE: 'Inattivo'.
  WHEN OTHERS.
    WRITE: 'Stato sconosciuto'.
ENDCASE.
\`

### LOOP/ENDLOOP - Cicli
Utilizzato per iterare su tabelle interne o eseguire cicli.

\`abap
LOOP AT gt_clienti INTO gs_cliente.
  WRITE: gs_cliente-nome.
ENDLOOP.

" Ciclo con condizione
LOOP AT gt_clienti INTO gs_cliente WHERE eta > 18.
  WRITE: gs_cliente-nome.
ENDLOOP.
\`

### WHILE/ENDWHILE - Ciclo Condizionale
Esegue un blocco di codice finché una condizione rimane vera.

\`abap
WHILE gv_contatore < 10.
  WRITE: gv_contatore.
  gv_contatore = gv_contatore + 1.
ENDWHILE.
\`

### DO/ENDDO - Ciclo Determinato
Esegue un blocco di codice un numero specifico di volte.

\`abap
DO 5 TIMES.
  WRITE: 'Iterazione', sy-index.
ENDDO.
\`

## Operazioni su Database

### SELECT - Selezione Dati
Utilizzata per leggere dati dalle tabelle del database.

\`abap
SELECT * FROM mara INTO TABLE gt_materiali
  WHERE matnr IN s_matnr.

" Selezione con campi specifici
SELECT matnr, maktx FROM mara INTO TABLE gt_materiali
  WHERE spras = 'IT'.
\`

### INSERT - Inserimento Dati
Inserisce nuovi record nelle tabelle del database.

\`abap
INSERT ztabella FROM gs_record.

" Inserimento da tabella interna
INSERT ztabella FROM TABLE gt_records.
\`

### UPDATE - Aggiornamento Dati
Modifica record esistenti nel database.

\`abap
UPDATE ztabella SET campo1 = 'valore'
  WHERE id = gv_id.

" Aggiornamento da struttura
UPDATE ztabella FROM gs_record.
\`

### DELETE - Cancellazione Dati
Rimuove record dal database.

\`abap
DELETE FROM ztabella WHERE id = gv_id.

" Cancellazione da tabella interna
DELETE ztabella FROM TABLE gt_records.
\`

### MODIFY - Inserimento/Aggiornamento
Inserisce o aggiorna record basandosi sulla chiave primaria.

\`abap
MODIFY ztabella FROM gs_record.
\`

## Operazioni su Tabelle Interne

### APPEND - Aggiunta Righe
Aggiunge una nuova riga alla fine di una tabella interna.

\`abap
gs_cliente-id = 1.
gs_cliente-nome = 'Mario Rossi'.
APPEND gs_cliente TO gt_clienti.
\`

### CLEAR - Pulizia Variabili
Pulisce il contenuto di variabili o work area.

\`abap
CLEAR: gs_cliente, gv_contatore.
\`

### REFRESH - Pulizia Tabelle
Rimuove tutte le righe da una tabella interna.

\`abap
REFRESH gt_clienti.
\`

### READ TABLE - Lettura Righe
Legge una riga specifica da una tabella interna.

\`abap
READ TABLE gt_clienti INTO gs_cliente WITH KEY id = 1.
IF sy-subrc = 0.
  WRITE: gs_cliente-nome.
ENDIF.
\`

### SORT - Ordinamento
Ordina una tabella interna per uno o più campi.

\`abap
SORT gt_clienti BY nome ASCENDING eta DESCENDING.
\`

### COLLECT - Raccolta con Somma
Raccoglie righe con chiave uguale sommando i campi numerici.

\`abap
gs_totale-reparto = 'IT'.
gs_totale-importo = 1000.
COLLECT gs_totale INTO gt_totali.
\`

## Sottoprogrammi e Funzioni

### FORM/ENDFORM - Subroutine
Definisce sottoprogrammi riutilizzabili.

\`abap
FORM calcola_totale USING p_prezzo TYPE p
                         p_quantita TYPE i
                   CHANGING p_totale TYPE p.
  p_totale = p_prezzo * p_quantita.
ENDFORM.
\`

### PERFORM - Chiamata Subroutine
Esegue una subroutine precedentemente definita.

\`abap
PERFORM calcola_totale USING gv_prezzo gv_quantita
                      CHANGING gv_totale.
\`

### FUNCTION/ENDFUNCTION - Moduli Funzione
Definisce moduli funzione riutilizzabili.

\`abap
FUNCTION z_calcola_sconto.
  IMPORTING iv_prezzo TYPE p
           iv_percentuale TYPE p
  RETURNING VALUE(rv_sconto) TYPE p.
  
  rv_sconto = iv_prezzo * iv_percentuale / 100.
ENDFUNCTION.
\`

### CALL FUNCTION - Chiamata Funzione
Chiama un modulo funzione.

\`abap
CALL FUNCTION 'Z_CALCOLA_SCONTO'
  EXPORTING
    iv_prezzo = gv_prezzo
    iv_percentuale = 10
  IMPORTING
    rv_sconto = gv_sconto.
\`

## Programmazione Orientata agli Oggetti

### CLASS/ENDCLASS - Definizione Classe
Definisce una classe con attributi e metodi.

\`abap
CLASS zcl_cliente DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING iv_nome TYPE string,
             get_nome RETURNING VALUE(rv_nome) TYPE string.
  PRIVATE SECTION.
    DATA: gv_nome TYPE string.
ENDCLASS.
\`

### METHOD/ENDMETHOD - Definizione Metodo
Definisce l'implementazione di un metodo.

\`abap
CLASS zcl_cliente IMPLEMENTATION.
  METHOD constructor.
    gv_nome = iv_nome.
  ENDMETHOD.
  
  METHOD get_nome.
    rv_nome = gv_nome.
  ENDMETHOD.
ENDCLASS.
\`

### CREATE OBJECT - Creazione Istanza
Crea una nuova istanza di una classe.

\`abap
DATA: lo_cliente TYPE REF TO zcl_cliente.
CREATE OBJECT lo_cliente
  EXPORTING
    iv_nome = 'Mario Rossi'.
\`

## Gestione Errori

### TRY/CATCH/ENDTRY - Gestione Eccezioni
Gestisce le eccezioni in modo strutturato.

\`abap
TRY.
    " Codice che può generare eccezioni
    gv_risultato = 10 / gv_divisore.
  CATCH cx_sy_zerodivide.
    MESSAGE 'Divisione per zero' TYPE 'E'.
  CATCH cx_root INTO DATA(lo_error).
    MESSAGE lo_error->get_text( ) TYPE 'E'.
ENDTRY.
\`

### RAISE - Sollevamento Eccezioni
Solleva un'eccezione personalizzata.

\`abap
IF gv_valore < 0.
  RAISE EXCEPTION TYPE zcx_valore_negativo.
ENDIF.
\`

## Operazioni su Stringhe

### CONCATENATE - Concatenazione
Unisce più stringhe in una sola.

\`abap
CONCATENATE gv_nome gv_cognome INTO gv_nome_completo
  SEPARATED BY space.
\`

### SPLIT - Divisione Stringhe
Divide una stringa in parti basandosi su un separatore.

\`abap
SPLIT gv_nome_completo AT space INTO gv_nome gv_cognome.
\`

### REPLACE - Sostituzione
Sostituisce parti di una stringa con nuovo testo.

\`abap
REPLACE ALL OCCURRENCES OF 'vecchio' IN gv_testo WITH 'nuovo'.
\`

### FIND - Ricerca
Cerca una sottostringa all'interno di una stringa.

\`abap
FIND 'ABAP' IN gv_testo.
IF sy-subrc = 0.
  WRITE: 'Testo trovato'.
ENDIF.
\`

## Operazioni Matematiche

### ADD - Addizione
Aggiunge un valore a una variabile.

\`abap
ADD 10 TO gv_totale.
" Equivalente a: gv_totale = gv_totale + 10.
\`

### SUBTRACT - Sottrazione
Sottrae un valore da una variabile.

\`abap
SUBTRACT 5 FROM gv_totale.
" Equivalente a: gv_totale = gv_totale - 5.
\`

### MULTIPLY - Moltiplicazione
Moltiplica una variabile per un valore.

\`abap
MULTIPLY gv_prezzo BY gv_quantita.
" Equivalente a: gv_prezzo = gv_prezzo * gv_quantita.
\`

### DIVIDE - Divisione
Divide una variabile per un valore.

\`abap
DIVIDE gv_totale BY gv_contatore.
" Equivalente a: gv_totale = gv_totale / gv_contatore.
\`

## Input/Output

### WRITE - Scrittura Output
Scrive dati nella lista di output.

\`abap
WRITE: 'Nome:', gv_nome,
       'Età:', gv_eta.
\`

### MESSAGE - Messaggi
Mostra messaggi informativi, di avvertimento o di errore.

\`abap
MESSAGE 'Operazione completata' TYPE 'S'.
MESSAGE 'Attenzione: valore alto' TYPE 'W'.
MESSAGE 'Errore nella validazione' TYPE 'E'.
\`

## Parametri e Selezioni

### PARAMETERS - Parametri Input
Definisce parametri di input per il programma.

\`abap
PARAMETERS: p_matnr TYPE mara-matnr,
           p_werks TYPE marc-werks.
\`

### SELECT-OPTIONS - Intervalli di Selezione
Definisce range di valori per la selezione.

\`abap
SELECT-OPTIONS: s_matnr FOR mara-matnr,
               s_erdat FOR mara-erdat.
\`

### SELECTION-SCREEN - Schermata di Selezione
Configura la schermata di selezione del programma.

\`abap
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS p_test TYPE c.
SELECTION-SCREEN END OF BLOCK b1.
\`

## Controllo del Flusso

### EXIT - Uscita
Esce dal blocco di codice corrente.

\`abap
LOOP AT gt_clienti INTO gs_cliente.
  IF gs_cliente-eta < 18.
    EXIT.  " Esce dal loop
  ENDIF.
ENDLOOP.
\`

### CONTINUE - Continua
Salta alla prossima iterazione del ciclo.

\`abap
LOOP AT gt_clienti INTO gs_cliente.
  IF gs_cliente-stato = 'I'.
    CONTINUE.  " Salta questa iterazione
  ENDIF.
  " Elabora solo clienti attivi
ENDLOOP.
\`

### RETURN - Ritorno
Termina l'esecuzione del sottoprogramma corrente.

\`abap
FORM elabora_cliente.
  IF gs_cliente IS INITIAL.
    RETURN.  " Esce dalla subroutine
  ENDIF.
  " Elabora il cliente
ENDFORM.
\`

## Transazioni e Commit

### COMMIT WORK - Conferma Transazione
Conferma permanentemente le modifiche al database.

\`abap
INSERT ztabella FROM gs_record.
COMMIT WORK.
\`

### ROLLBACK WORK - Annulla Transazione
Annulla tutte le modifiche non confermate.

\`abap
INSERT ztabella FROM gs_record.
IF sy-subrc <> 0.
  ROLLBACK WORK.
ENDIF.
\`

## Tipi di Dati Predefiniti

### STRING - Stringa Variabile
Tipo di dato per stringhe di lunghezza variabile.

\`abap
DATA: gv_testo TYPE string.
gv_testo = 'Questa è una stringa di lunghezza variabile'.
\`

### I - Intero
Tipo di dato per numeri interi.

\`abap
DATA: gv_numero TYPE i.
gv_numero = 42.
\`

### F - Floating Point
Tipo di dato per numeri in virgola mobile.

\`abap
DATA: gv_decimale TYPE f.
gv_decimale = '3.14159'.
\`

### C - Carattere
Tipo di dato per stringhe di lunghezza fissa.

\`abap
DATA: gv_codice TYPE c LENGTH 10.
gv_codice = 'ABC123'.
\`

### D - Data
Tipo di dato per date nel formato YYYYMMDD.

\`abap
DATA: gv_data TYPE d.
gv_data = sy-datum.
\`

### T - Tempo
Tipo di dato per orari nel formato HHMMSS.

\`abap
DATA: gv_ora TYPE t.
gv_ora = sy-uzeit.
\`



### TABLES - Dichiarazione Tabelle Database
Dichiara una work area per una tabella del database, permettendo l'accesso diretto ai suoi campi.

\`abap
TABLES: mara, marc, makt.

" Ora si possono usare direttamente i campi
mara-matnr = 'MAT001'.
SELECT SINGLE * FROM mara WHERE matnr = mara-matnr.
\`

### STATICS - Variabili Statiche
Dichiara variabili che mantengono il loro valore tra le chiamate del sottoprogramma.

\`abap
FORM conta_chiamate.
  STATICS: gv_contatore TYPE i.
  gv_contatore = gv_contatore + 1.
  WRITE: 'Chiamata numero:', gv_contatore.
ENDFORM.
\`

### RANGES - Definizione Range
Crea una tabella interna per definire intervalli di valori.

\`abap
RANGES: r_matnr FOR mara-matnr.
r_matnr-sign = 'I'.
r_matnr-option = 'EQ'.
r_matnr-low = 'MAT001'.
APPEND r_matnr.
\`

### WORK - Area di Lavoro
Definisce un'area di lavoro per operazioni temporanee.

\`abap
WORK: wa_cliente LIKE LINE OF gt_clienti.
READ TABLE gt_clienti INTO wa_cliente INDEX 1.
\`

## Istruzioni di Controllo del Programma

### INITIALIZATION - Inizializzazione
Evento eseguito all'avvio del programma, prima della schermata di selezione.

\`abap
INITIALIZATION.
  p_werks = '1000'.
  p_datum = sy-datum.
  MESSAGE 'Programma inizializzato' TYPE 'I'.
\`

### START-OF-SELECTION - Inizio Selezione
Evento principale di elaborazione del programma.

\`abap
START-OF-SELECTION.
  PERFORM elabora_dati.
  PERFORM crea_output.
\`

### END-OF-SELECTION - Fine Selezione
Evento eseguito alla fine dell'elaborazione principale.

\`abap
END-OF-SELECTION.
  WRITE: 'Elaborazione completata'.
  WRITE: 'Totale record elaborati:', gv_contatore.
\`

### TOP-OF-PAGE - Intestazione Pagina
Evento eseguito all'inizio di ogni pagina di output.

\`abap
TOP-OF-PAGE.
  WRITE: 'Report Materiali'.
  WRITE: 'Data:', sy-datum.
  ULINE.
\`

### END-OF-PAGE - Fine Pagina
Evento eseguito alla fine di ogni pagina di output.

\`abap
END-OF-PAGE.
  WRITE: 'Pagina:', sy-pagno.
\`

## Operazioni su Dati

### MOVE - Spostamento Dati
Copia il contenuto di una variabile in un'altra.

\`abap
MOVE gv_nome TO gv_nome_copia.
" Equivalente a: gv_nome_copia = gv_nome.

MOVE-CORRESPONDING gs_cliente TO gs_fornitore.
\`

### ASSIGN - Assegnazione Dinamica
Assegna dinamicamente un campo a un field-symbol.

\`abap
FIELD-SYMBOLS: <fs_campo> TYPE any.
ASSIGN gv_variabile TO <fs_campo>.
IF <fs_campo> IS ASSIGNED.
  <fs_campo> = 'Nuovo valore'.
ENDIF.
\`

### UNASSIGN - Rimozione Assegnazione
Rimuove l'assegnazione di un field-symbol.

\`abap
UNASSIGN <fs_campo>.
\`

### PACK - Compattazione
Converte un numero in formato packed.

\`abap
DATA: gv_numero TYPE n LENGTH 10,
      gv_packed TYPE p LENGTH 5.
gv_numero = '1234567890'.
PACK gv_numero TO gv_packed.
\`

### UNPACK - Decompattazione
Converte un numero dal formato packed.

\`abap
DATA: gv_numero TYPE n LENGTH 10,
      gv_packed TYPE p LENGTH 5.
UNPACK gv_packed TO gv_numero.
\`

## Operazioni su Stringhe Avanzate

### OVERLAY - Sovrapposizione
Sovrappone caratteri di una stringa su un'altra.

\`abap
DATA: gv_stringa1 TYPE string VALUE 'A*C*E',
      gv_stringa2 TYPE string VALUE '*B*D*'.
OVERLAY gv_stringa1 WITH gv_stringa2.
" Risultato: 'ABCDE'
\`

### SHIFT - Spostamento Caratteri
Sposta i caratteri di una stringa verso sinistra o destra.

\`abap
DATA: gv_testo TYPE string VALUE '  ABAP  '.
SHIFT gv_testo LEFT DELETING LEADING space.
" Risultato: 'ABAP  '
SHIFT gv_testo RIGHT BY 2 PLACES.
" Risultato: '  ABAP'
\`

### TRANSLATE - Traduzione Caratteri
Converte caratteri in maiuscolo o minuscolo.

\`abap
DATA: gv_testo TYPE string VALUE 'Hello World'.
TRANSLATE gv_testo TO UPPER CASE.
" Risultato: 'HELLO WORLD'
TRANSLATE gv_testo TO LOWER CASE.
" Risultato: 'hello world'
\`

### CONDENSE - Compattazione Spazi
Rimuove spazi multipli e spazi iniziali/finali.

\`abap
DATA: gv_testo TYPE string VALUE '  Hello   World  '.
CONDENSE gv_testo.
" Risultato: 'Hello World'
CONDENSE gv_testo NO-GAPS.
" Risultato: 'HelloWorld'
\`

## Controllo Flusso Avanzato

### SKIP - Salto Righe
Salta un numero specificato di righe nell'output.

\`abap
WRITE: 'Prima riga'.
SKIP 2.
WRITE: 'Riga dopo due spazi'.
\`

### NEW-LINE - Nuova Riga
Forza una nuova riga nell'output.

\`abap
WRITE: 'Testo sulla prima riga'.
NEW-LINE.
WRITE: 'Testo sulla seconda riga'.
\`

### NEW-PAGE - Nuova Pagina
Forza una nuova pagina nell'output.

\`abap
WRITE: 'Contenuto prima pagina'.
NEW-PAGE.
WRITE: 'Contenuto seconda pagina'.
\`

### ULINE - Linea di Sottolineatura
Disegna una linea di sottolineatura nell'output.

\`abap
WRITE: 'Intestazione'.
ULINE.
WRITE: 'Contenuto sotto la linea'.
\`

### RESERVE - Riserva Righe
Riserva un numero di righe nella pagina corrente.

\`abap
RESERVE 5 LINES.
WRITE: 'Questo testo avrà 5 righe riservate'.
\`

## Operazioni Database Avanzate

### FETCH - Recupero Cursore
Recupera righe da un cursore aperto.

\`abap
DATA: gt_materiali TYPE TABLE OF mara.
SELECT * FROM mara INTO TABLE gt_materiali
  PACKAGE SIZE 100.
\`

### OPEN CURSOR - Apertura Cursore
Apre un cursore per leggere dati in blocchi.

\`abap
DATA: cursor TYPE cursor.
OPEN CURSOR cursor FOR SELECT * FROM mara.
\`

### CLOSE CURSOR - Chiusura Cursore
Chiude un cursore precedentemente aperto.

\`abap
CLOSE CURSOR cursor.
\`

### EXPORT - Esportazione Memoria
Esporta dati in memoria cluster.

\`abap
EXPORT gt_clienti TO MEMORY ID 'CLIENTI'.
\`

### IMPORT - Importazione Memoria
Importa dati dalla memoria cluster.

\`abap
IMPORT gt_clienti FROM MEMORY ID 'CLIENTI'.
\`

### FREE MEMORY - Liberazione Memoria
Libera la memoria cluster.

\`abap
FREE MEMORY ID 'CLIENTI'.
\`

## Gestione Transazioni

### SUBMIT - Esecuzione Programma
Esegue un altro programma ABAP.

\`abap
SUBMIT zprogram
  WITH p_matnr = 'MAT001'
  WITH s_werks IN r_werks
  AND RETURN.
\`

### LEAVE - Uscita Programma
Esce dal programma corrente.

\`abap
LEAVE PROGRAM.
" Oppure per tornare a una transazione
LEAVE TO TRANSACTION 'SE80'.
\`

### SET PARAMETER - Impostazione Parametri
Imposta parametri SAP persistenti.

\`abap
SET PARAMETER ID 'MAT' FIELD gv_matnr.
\`

### GET PARAMETER - Recupero Parametri
Recupera parametri SAP precedentemente impostati.

\`abap
GET PARAMETER ID 'MAT' FIELD gv_matnr.
\`

## Gestione Autorizzazioni

### AUTHORITY-CHECK - Controllo Autorizzazioni
Verifica le autorizzazioni dell'utente.

\`abap
AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
  ID 'ACTVT' FIELD '03'
  ID 'WERKS' FIELD p_werks.
IF sy-subrc <> 0.
  MESSAGE 'Non autorizzato' TYPE 'E'.
ENDIF.
\`

## Operazioni su File

### OPEN DATASET - Apertura File
Apre un file per lettura o scrittura.

\`abap
DATA: gv_file TYPE string VALUE '/tmp/output.txt'.
OPEN DATASET gv_file FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
\`

### TRANSFER - Trasferimento File
Scrive dati in un file.

\`abap
TRANSFER gv_riga TO gv_file.
\`

### CLOSE DATASET - Chiusura File
Chiude un file precedentemente aperto.

\`abap
CLOSE DATASET gv_file.
\`

## Gestione Memoria e Performance

### LOAD-OF-PROGRAM - Caricamento Programma
Evento eseguito al caricamento del programma in memoria.

\`abap
LOAD-OF-PROGRAM.
  " Inizializzazioni globali
  PERFORM init_global_data.
\`

### HIDE - Nascondi Valori
Nasconde valori per l'interactive reporting.

\`abap
LOOP AT gt_clienti INTO gs_cliente.
  WRITE: gs_cliente-id, gs_cliente-nome.
  HIDE: gs_cliente-id.
ENDLOOP.
\`

### GET - Recupero Valori Nascosti
Recupera valori precedentemente nascosti.

\`abap
AT LINE-SELECTION.
  GET gs_cliente-id.
  " Elabora il cliente selezionato
\`

## Operazioni Speciali

### GENERATE - Generazione Dinamica
Genera programmi o subroutine dinamicamente.

\`abap
DATA: gt_source TYPE TABLE OF string.
APPEND 'WRITE: ''Hello Dynamic World''.' TO gt_source.
GENERATE SUBROUTINE POOL gt_source NAME gv_program.
\`

### SYNTAX-CHECK - Controllo Sintassi
Verifica la sintassi di codice generato dinamicamente.

\`abap
SYNTAX-CHECK FOR gt_source.
IF sy-subrc = 0.
  WRITE: 'Sintassi corretta'.
ENDIF.
\`

### DESCRIBE - Descrizione Oggetti
Ottiene informazioni su variabili e strutture.

\`abap
DATA: gv_tipo TYPE c,
      gv_lunghezza TYPE i.
DESCRIBE FIELD gv_campo TYPE gv_tipo LENGTH gv_lunghezza.
\`

### CHECKSUM - Calcolo Checksum
Calcola il checksum di una stringa.

\`abap
DATA: gv_checksum TYPE i.
CHECKSUM gv_testo INTO gv_checksum.
\`

## Gestione Eccezioni Classiche

### CATCH SYSTEM-EXCEPTIONS - Cattura Eccezioni Sistema
Cattura eccezioni di sistema nel codice legacy.

\`abap
CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 4.
  gv_risultato = gv_numero1 / gv_numero2.
ENDCATCH.
IF sy-subrc = 4.
  WRITE: 'Errore aritmetico'.
ENDIF.
\`

### REJECT - Rifiuto Record
Rifiuta un record durante l'elaborazione di eventi.

\`abap
GET mara.
IF mara-mtart = 'OBSOLETE'.
  REJECT.
ENDIF.
\`

Questa documentazione copre tutte le keyword ABAP che erano state omesse dalla prima versione, fornendo per ciascuna una spiegazione dettagliata e esempi pratici di utilizzo.

`;