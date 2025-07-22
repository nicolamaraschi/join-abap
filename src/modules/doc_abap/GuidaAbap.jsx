import IndexedContent from '../../components/IndexedContent.jsx';

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
SELECT campo1 campo2 
  UP TO 10 ROWS             "ERRORE: UP TO DEVE STARE SOTTO INTO TABLE
  INTO TABLE @DATA(lt_tabella)                     
  FROM nome_tabella.

" Sintassi CORRETTA ✅
SELECT campo1,
       campo2,
       campo3
  FROM nome_tabella
  WHERE campo_filtro = 'VALORE'
  INTO TABLE @DATA(lt_tabella) 
    UP TO 10 ROWS.           " UP TO segue INTO TABLE
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


### 5.2.1. Lettura con Funzioni Aggregate
L'esempio che seguirà determina il numero totale dei record nella tabella spfli, come pure il valore più alto e il più basso del campo distanza tra l'aeroporto di partenza e quello di arrivo. Questo tipo di SELECT fa uso di funzioni che vengono denominate aggregate. Queste funzioni sono: MIN (minimo), MAX (massimo), COUNT (conta), AVG (media), SUM (somma, ma solo per campi numerici). A differenza delle altre SELECT, in questa non occorre assolutamente mettere il parametro ENDSELECT.

\`abap
REPORT z_aggregate_example.

TABLES: spfli.

DATA: max_distance LIKE spfli-distance,
      min_distance LIKE spfli-distance,
      counter      TYPE i.

START-OF-SELECTION.
  SELECT MAX( distance ),
         MIN( distance ),
         COUNT(*)
    FROM spfli
    INTO (max_distance, min_distance, counter).

  WRITE: / 'Distanza Massima:', max_distance,
         / 'Distanza Minima:', min_distance,
         / 'Numero di Voli:', counter.
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

### TYPES - Definizione di Tipi Personalizzati (RIGA)
Permette di definire tipi di dati personalizzati per riutilizzarli nel programma.

\`abap
TYPES: BEGIN OF ty_cliente,
         id TYPE i,
         nome TYPE string,
         email TYPE string,
       END OF ty_cliente.

DATA: ls_cliente TYPE ty_cliente.
\`

### TYPE STANDARD TABLE OF- Definizione PRPRIO UNA TABELLA INTERNA

\`abap
" --- PASSO 1: Definire il MODELLO per un singolo dato (una struttura) ---
" TYPES: È l'istruzione per creare un NUOVO TIPO di dato.
"        Qui creiamo un "modello" chiamato 'ty_s_prodotto' che raggruppa
"        tre campi. Non stiamo creando una variabile, solo la definizione.
TYPES: BEGIN OF ty_s_prodotto,
         id      TYPE i,
         nome    TYPE string,
         prezzo  TYPE p DECIMALS 2,
       END OF ty_s_prodotto.


" --- PASSO 2: Definire il MODELLO per una TABELLA di quel dato ---
" Usiamo di nuovo TYPES per creare un altro modello, questa volta per una tabella.
" TYPE STANDARD TABLE OF: Specifica che 'ty_t_prodotti' è un tipo "tabella standard"
"                         le cui righe avranno la forma del modello 'ty_s_prodotto'.
TYPES ty_t_prodotti TYPE STANDARD TABLE OF ty_s_prodotto.


" --- PASSO 3: Creare la VARIABILE vera e propria ---
" DATA: È l'istruzione che finalmente crea in memoria una variabile.
"       'lt_catalogo_prodotti' è la nostra tabella, creata usando il
"       modello di tabella 'ty_t_prodotti' definito prima.
DATA lt_catalogo_prodotti TYPE ty_t_prodotti.
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

Ecco tutti gli operatori di relazione legati all'istruzione di controllo IF in ABAP:
#### Operatori di confronto:

* **EQ (Equal)** - uguale a
\`abap
IF status EQ 'A'.
  WRITE: 'Stato Attivo'.
ENDIF.
\`
* **NE (Not Equal)** - diverso da
\`abap
IF status NE 'I'.
  WRITE: 'Stato non Inattivo'.
ENDIF.
\`
* **GT (Greater Than)** - maggiore di
\`abap
IF contatore GT 10.
  WRITE: 'Superato il limite'.
ENDIF.
\`
* **GE (Greater or Equal)** - maggiore di o uguale a
\`abap
IF punteggio GE 100.
  WRITE: 'Punteggio massimo raggiunto'.
ENDIF.
\`
* **LT (Less Than)** - minore di
\`abap
IF livello LT 5.
  WRITE: 'Livello base'.
ENDIF.
\`
* **LE (Less or Equal)** - minore di o uguale a
\`abap
IF tentativi LE 3.
  WRITE: 'Hai ancora tentativi'.
ENDIF.
\`

#### Operatori di range:

* **BETWEEN** - compreso tra due valori (f1 AND f2)
\`abap
IF numero BETWEEN 1 AND 100.
  WRITE: 'Numero nel range'.
ENDIF.
\`

#### Operatori per stringhe:

* **CO (Contains Only)** - contiene solo i caratteri specificati
\`abap
IF stringa CO 'ABCDE'.
  WRITE: 'La stringa contiene solo caratteri da A a E'.
ENDIF.
\`
* **CN (Contains Not only)** - non contiene solo i caratteri specificati
\`abap
IF stringa CN '0123456789'.
  WRITE: 'La stringa non è puramente numerica'.
ENDIF.
\`
* **CA (Contains Any)** - contiene almeno uno dei caratteri specificati
\`abap
IF stringa CA '!?#'.
  WRITE: 'La stringa contiene caratteri speciali'.
ENDIF.
\`
* **NA (Not Any)** - non contiene nessuno dei caratteri specificati
\`abap
IF stringa NA ' '.
  WRITE: 'La stringa non contiene spazi'.
ENDIF.
\`
* **CS (Contains String)** - contiene la stringa specificata
\`abap
IF testo CS 'SAP'.
  WRITE: 'Trovata la parola SAP'.
ENDIF.
\`
* **NS (Not String)** - non contiene la stringa specificata
\`abap
IF testo NS 'obsoleto'.
  WRITE: 'Il testo è aggiornato'.
ENDIF.
\`
* **CP (Contains Pattern)** - corrisponde al pattern (con * e +)
\`abap
IF codice_postale CP '#####'.
  WRITE: 'Formato CAP valido'.
ENDIF.
\`
* **NP (No Pattern)** - non corrisponde al pattern
\`abap
IF telefono NP '+##-##########'.
  WRITE: 'Formato telefono non valido'.
ENDIF.
\`

#### Operatori per valori iniziali:

* **IS INITIAL** - è vuoto/iniziale
\`abap
IF variabile IS INITIAL.
  WRITE: 'La variabile non è stata valorizzata'.
ENDIF.
\`
* **IS NOT INITIAL** - non è vuoto/iniziale
\`abap
IF tabella IS NOT INITIAL.
  WRITE: 'La tabella contiene dati'.
ENDIF.
\`

#### Operatori per valori NULL:

* **IS NULL** - è nullo (per riferimenti oggetti)
\`abap
IF oggetto IS NULL.
  WRITE: 'Oggetto non istanziato'.
ENDIF.
\`
* **IS NOT NULL** - non è nullo
\`abap
IF oggetto IS NOT NULL.
  WRITE: 'Oggetto istanziato'.
ENDIF.
\`

#### Operatori per classi/interfacce:

* **IS INSTANCE OF** - è un'istanza della classe/interfaccia
\`abap
IF lo_oggetto IS INSTANCE OF zcl_mia_classe.
  WRITE: 'L\'oggetto è una istanza della classe ZCL_MIA_CLASSE'.
ENDIF.
\`
* **IS NOT INSTANCE OF** - non è un'istanza della classe/interfaccia
\`abap
IF lo_oggetto IS NOT INSTANCE OF zif_mia_interfaccia.
  WRITE: 'L\'oggetto non implementa l\'interfaccia ZIF_MIA_INTERFACCIA'.
ENDIF.
\`

#### Operatori logici per combinare condizioni:

* **AND** - e logico
\`abap
IF status EQ 'A' AND tipo EQ 'X'.
  WRITE: 'Condizione complessa verificata'.
ENDIF.
\`
* **OR** - o logico
\`abap
IF priorita EQ 1 OR urgenza EQ 'ALTA'.
  WRITE: 'Elemento prioritario'.
ENDIF.
\`
* **NOT** - negazione logica
\`abap
IF NOT ( status EQ 'C' ).
  WRITE: 'Lo stato non è Chiuso'.
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

#### Lettura Standard
\`abap
READ TABLE gt_clienti INTO gs_cliente WITH KEY id = 1.
IF sy-subrc = 0.
  WRITE: gs_cliente-nome.
ENDIF.
\`

#### Lettura con Binary Search (Tabella Ordinata)
\`abap
" Più veloce per tabelle grandi già ordinate
SORT gt_clienti BY nome.
READ TABLE gt_clienti INTO gs_cliente WITH KEY nome = 'Mario' BINARY SEARCH.
\`

#### Lettura con Field-Symbol (Più Efficiente)
\`abap
READ TABLE gt_clienti ASSIGNING <fs_cliente> WITH KEY id = 1.
IF sy-subrc = 0.
  <fs_cliente>-eta = <fs_cliente>-eta + 1.  " Modifica diretta
ENDIF.
\`

#### Lettura con Reference
\`abap
READ TABLE gt_clienti REFERENCE INTO lr_cliente WITH KEY id = 1.
IF sy-subrc = 0.
  lr_cliente->eta = lr_cliente->eta + 1.
ENDIF.
\`

#### Lettura Selettiva di Campi (TRANSPORTING)
\`abap
" Legge solo i campi specificati per migliore performance
READ TABLE gt_clienti INTO gs_cliente WITH KEY id = 1 
     TRANSPORTING nome eta.
\`

#### Lettura per Indice
\`abap
READ TABLE gt_clienti INTO gs_cliente INDEX 1.
\`

### SORT - Ordinamento
Ordina una tabella interna per uno o più campi.

#### Ordinamento Base
\`abap
SORT gt_clienti BY nome ASCENDING eta DESCENDING.
\`

#### Ordinamento con Criteri Multipli
\`abap
SORT gt_clienti BY citta ASCENDING 
                   nome ASCENDING 
                   eta DESCENDING.
\`

#### Ordinamento Stabile
\`abap
" Mantiene l'ordine originale per elementi uguali
SORT gt_clienti STABLE BY nome.
\`

### COLLECT - Raccolta con Somma
Raccoglie righe con chiave uguale sommando i campi numerici.

#### Utilizzo Base
\`abap
gs_totale-reparto = 'IT'.
gs_totale-importo = 1000.
COLLECT gs_totale INTO gt_totali.

gs_totale-reparto = 'IT'.    " Stessa chiave
gs_totale-importo = 500.     " Verrà sommato: 1000 + 500 = 1500
COLLECT gs_totale INTO gt_totali.
\`

#### Esempio Pratico con Loop
\`abap
LOOP AT gt_vendite INTO gs_vendita.
  gs_riepilogo-cliente = gs_vendita-cliente.
  gs_riepilogo-totale = gs_vendita-importo.
  COLLECT gs_riepilogo INTO gt_riepilogo.
ENDLOOP.
\`

### MODIFY - Modifica Righe
Modifica righe esistenti in una tabella interna.

#### Modifica per Indice
\`abap
gs_cliente-nome = 'Nuovo Nome'.
MODIFY gt_clienti FROM gs_cliente INDEX 1.
\`

#### Modifica con Condizione WHERE
\`abap
gs_cliente-attivo = 'X'.
MODIFY gt_clienti FROM gs_cliente WHERE eta > 65.
\`

#### Modifica con Field-Symbol
\`abap
LOOP AT gt_clienti ASSIGNING <fs_cliente>.
  <fs_cliente>-eta = <fs_cliente>-eta + 1.
  MODIFY gt_clienti FROM <fs_cliente> INDEX sy-tabix.
ENDLOOP.
\`

### DELETE - Cancellazione Righe
Rimuove righe da una tabella interna.

#### Cancellazione per Indice
\`abap
DELETE gt_clienti INDEX 1.
\`

#### Cancellazione con Condizione WHERE
\`abap
DELETE gt_clienti WHERE eta > 65 AND attivo = ' '.
\`

#### Rimozione Duplicati Adiacenti
\`abap
SORT gt_clienti BY nome.
DELETE ADJACENT DUPLICATES FROM gt_clienti COMPARING nome.
\`

#### Rimozione Duplicati per Tutti i Campi
\`abap
DELETE ADJACENT DUPLICATES FROM gt_clienti.
\`

### INSERT - Inserimento Righe
Inserisce righe in posizioni specifiche.

#### Inserimento Singola Riga
\`abap
INSERT gs_cliente INTO gt_clienti INDEX 1.
\`

#### Inserimento Multiple Righe
\`abap
INSERT LINES OF gt_nuovi_clienti INTO gt_clienti INDEX 1.
\`

### APPEND - Aggiunta Righe
Aggiunge righe alla fine della tabella.

#### Aggiunta Singola
\`abap
APPEND gs_cliente TO gt_clienti.
\`

#### Aggiunta Multiple
\`abap
APPEND LINES OF gt_altri_clienti TO gt_clienti.
\`

### LOOP - Iterazione Avanzata
Cicli con controlli e condizioni.

#### Loop con Condizione WHERE
\`abap
LOOP AT gt_clienti INTO gs_cliente WHERE eta > 18 AND citta = 'Milano'.
  WRITE: / gs_cliente-nome, gs_cliente-eta.
ENDLOOP.
\`

#### Loop con Controllo Indice
\`abap
LOOP AT gt_clienti INTO gs_cliente.
  IF sy-tabix = 1.
    WRITE: / 'Primo cliente:', gs_cliente-nome.
  ELSEIF sy-tabix = lines( gt_clienti ).
    WRITE: / 'Ultimo cliente:', gs_cliente-nome.
  ENDIF.
ENDLOOP.
\`

#### Loop da Indice a Indice
\`abap
LOOP AT gt_clienti INTO gs_cliente FROM 5 TO 10.
  WRITE: / gs_cliente-nome.
ENDLOOP.
\`

### DESCRIBE TABLE - Informazioni Tabella
Ottiene informazioni sulla struttura della tabella.

#### Conteggio Righe
\`abap
DESCRIBE TABLE gt_clienti LINES lv_lines.
WRITE: / 'Numero righe:', lv_lines.
\`

#### Informazioni Complete
\`abap
DESCRIBE TABLE gt_clienti LINES lv_lines KIND lv_kind.
" lv_kind: 'T' = Standard Table, 'S' = Sorted Table, 'H' = Hashed Table
\`

### CLEAR vs REFRESH vs FREE
Differenti modi per pulire le tabelle.

#### CLEAR - Pulisce Header Line
\`abap
CLEAR gt_clienti.      " Pulisce solo la work area (se esiste)
CLEAR gs_cliente.      " Pulisce la struttura
\`

#### REFRESH - Pulisce Contenuto Tabella
\`abap
REFRESH gt_clienti.    " Rimuove tutte le righe, mantiene memoria allocata
\`

#### FREE - Libera Memoria
\`abap
FREE gt_clienti.       " Rimuove righe e libera completamente la memoria
\`

### Operazioni Set-Oriented (Performance Ottimale)
Operazioni che agiscono sull'intera tabella contemporaneamente.

#### Operazioni Condizionali Multiple
\`abap
" Invece di loop, usa operazioni dirette
DELETE gt_clienti WHERE eta < 18 OR eta > 65.
MODIFY gt_clienti FROM gs_default WHERE nome IS INITIAL.
\`

#### Confronto Performance
\`abap
" ❌ Lento - Loop individuale
LOOP AT gt_clienti INTO gs_cliente.
  IF gs_cliente-eta > 65.
    DELETE gt_clienti INDEX sy-tabix.
  ENDIF.
ENDLOOP.

" ✅ Veloce - Operazione set-oriented
DELETE gt_clienti WHERE eta > 65.
\`

### FOR - Espressioni di Costruzione Tabelle
Iteratore per costruire tabelle basato su altre tabelle o range, simile ai list comprehensions.

\`abap
DATA(gt_nomi) = VALUE string_table( FOR ls_cliente IN gt_clienti 
                                   WHERE ( eta > 18 )
                                   ( ls_cliente-nome ) ).
\`


### FOR - Espressioni di Costruzione Tabelle
Iteratore per costruire tabelle basato su altre tabelle o range, simile ai list comprehensions.

\`abap
DATA(gt_nomi) = VALUE string_table( FOR ls_cliente IN gt_clienti 
                                    WHERE ( eta > 18 )
                                    ( ls_cliente-nome ) ).
\`

### REDUCE - Aggregazioni
Operatore per calcolare un singolo valore aggregato iterando su una tabella (somme, conteggi, concatenazioni).

\`abap
DATA(lv_totale) = REDUCE i( INIT sum = 0 
                           FOR ls_ordine IN gt_ordini 
                           NEXT sum = sum + ls_ordine-importo ).
\`

### FILTER - Filtraggio Tabelle
Costruisce una nuova tabella contenente solo le righe che soddisfano una condizione specifica.

\`abap
DATA(gt_adulti) = FILTER #( gt_clienti WHERE eta > 18 ).
\`

### VALUE #( ) - Costruttori di Tabella
Sintassi moderna per inizializzare tabelle interne con valori specifici in modo dichiarativo.

\`abap
DATA(gt_colori) = VALUE string_table( ( 'Rosso' ) ( 'Verde' ) ( 'Blu' ) ).
\`

### COND - Operatore Ternario
Espressione condizionale inline che restituisce un valore basato su una condizione (equivalente a if-then-else compatto).

\`abap
DATA(lv_stato) = COND string( WHEN lv_eta >= 18 THEN 'Adulto' ELSE 'Minore' ).
\`

### CORRESPONDING - Mapping Automatico
Copia automaticamente campi con nomi corrispondenti tra strutture diverse, facilitando il mapping dei dati.

\`abap
gs_output = CORRESPONDING #( gs_input MAPPING nuovo_campo = vecchio_campo ).
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
  IMPORTING  " IMPORTING: definisce i parametri di INPUT della funzione
          iv_prezzo TYPE p
          iv_percentuale TYPE p
  RETURNING  " RETURNING: definisce il valore di RITORNO della funzione
          VALUE(rv_sconto) TYPE p.
  
  rv_sconto = iv_prezzo * iv_percentuale / 100.
ENDFUNCTION.
\`

### CALL FUNCTION - Chiamata Funzione
Chiama un modulo funzione.

\`abap
CALL FUNCTION 'Z_CALCOLA_SCONTO'

  " EXPORTING: Invia dati (variabili, strutture, tabelle) in SOLA LETTURA dal programma alla funzione.
  EXPORTING
    iv_prezzo_base      = gv_prezzo_iniziale
    iv_percentuale      = 10

  " IMPORTING: Riceve i risultati (variabili, strutture, tabelle) calcolati dalla funzione e li salva nelle variabili del programma.
  IMPORTING
    ev_sconto_finale    = gv_sconto_calcolato

  " CHANGING: Passa dati (variabili, strutture, tabelle) che la funzione può sia leggere che MODIFICARE. Le modifiche sono visibili nel programma chiamante.
  CHANGING
    cv_log_messaggio    = gv_messaggio_log

  " TABLES: Sintassi legacy per passare tabelle interne che la funzione può leggere e MODIFICARE. Funziona come un parametro in CHANGING.
  TABLES
    t_regole_speciali   = lt_regole_sconto

  " EXCEPTIONS: Gestisce specifici errori controllati (eccezioni) che possono essere sollevati dalla funzione, impostando la variabile di sistema sy-subrc.
  EXCEPTIONS
    percentuale_invalida = 1
    prezzo_negativo      = 2
    OTHERS               = 3.

" Dopo questa chiamata, si controlla il valore di sy-subrc per vedere se si è verificata un'eccezione.
" sy-subrc = 0 significa che la chiamata è andata a buon fine.
" sy-subrc = 1, 2, 3... significa che si è verificato l'errore corrispondente.

\`

## Programmazione Orientata agli Oggetti

### CLASS/ENDCLASS - Definizione Classe
Definisce una classe con attributi e metodi.

\`abap
" CLASS ... DEFINITION: Inizia la definizione (il "progetto") di una nuova classe.
" Una classe è un modello per creare oggetti, raggruppando dati (attributi) e funzionalità (metodi).
CLASS zcl_cliente DEFINITION.

  " PUBLIC SECTION: Dichiara i componenti (attributi, tipi e metodi) che sono visibili e utilizzabili dall'esterno della classe.
  " Qualsiasi programma che crea un'istanza (un oggetto) di questa classe può accedere a questi componenti pubblici.
  PUBLIC SECTION.

    " TYPES: Permette di definire tipi di dati personalizzati, come strutture e tabelle, che saranno parte della classe.
    TYPES:
      " BEGIN OF ... END OF: Definisce una struttura, che è un raggruppamento logico di campi correlati.
      " ty_s_indirizzo: È il nome della nostra struttura per rappresentare un singolo indirizzo.
      BEGIN OF ty_s_indirizzo,
        via    TYPE string,
        citta  TYPE string,
        cap    TYPE string,
      END OF ty_s_indirizzo,

      " TYPE TABLE OF: Definisce un tipo di dato "tabella" basato su una struttura o un tipo elementare.
      " ty_t_indirizzi: È il nome del nostro tipo di tabella, che può contenere più righe di tipo ty_s_indirizzo.
      ty_t_indirizzi TYPE STANDARD TABLE OF ty_s_indirizzo WITH EMPTY KEY.

    " METHODS: Inizia la dichiarazione dei metodi, che rappresentano le azioni che l'oggetto può compiere.
    METHODS:
      " constructor: È un metodo speciale chiamato automaticamente UNA SOLA VOLTA quando si crea un nuovo oggetto
      "              con l'istruzione CREATE OBJECT. Il suo scopo principale è inizializzare gli attributi dell'oggetto.
      " IMPORTING iv_nome: Definisce un parametro di input obbligatorio per il costruttore.
      constructor IMPORTING iv_nome TYPE string,

      " get_nome: È un metodo pubblico che permette di leggere il valore di un attributo privato in modo controllato.
      " RETURNING VALUE(rv_nome): Specifica che il metodo restituirà un valore (in questo caso una stringa) al chiamante.
      get_nome RETURNING VALUE(rv_nome) TYPE string,

      " add_indirizzo: Metodo per aggiungere un nuovo indirizzo alla lista privata del cliente.
      " IMPORTING is_indirizzo: Accetta in input una struttura di tipo ty_s_indirizzo, da noi definita sopra.
      add_indirizzo IMPORTING is_indirizzo TYPE ty_s_indirizzo.

  " PRIVATE SECTION: Dichiara i componenti accessibili SOLO dall'interno della classe stessa (cioè, dai metodi di questa classe).
  " Questo principio, chiamato incapsulamento, serve a proteggere i dati e a garantire che vengano gestiti solo tramite i metodi pubblici.
  PRIVATE SECTION.

    " DATA: Dichiara gli attributi (le variabili) della classe che ne definiscono lo stato.
    " gv_nome: È un attributo privato che memorizza il nome del cliente.
    DATA: gv_nome TYPE string.

    " gt_indirizzi: Attributo privato che usa il nostro tipo di tabella personalizzato per memorizzare una lista di indirizzi per il cliente.
    DATA: gt_indirizzi TYPE ty_t_indirizzi.

ENDCLASS. " ENDCLASS: Termina la definizione della classe.

\`

### METHOD/ENDMETHOD - Definizione Metodo
Definisce l'implementazione di un metodo.

\`abap
" CLASS ... IMPLEMENTATION: Inizia il blocco di codice che contiene la logica effettiva dei metodi dichiarati nella DEFINITION.
CLASS zcl_cliente IMPLEMENTATION.

  " METHOD ... ENDMETHOD: Delimita il codice di un singolo metodo.
  " constructor: L'implementazione del costruttore.
  METHOD constructor.
    " me-> : È un puntatore speciale che si riferisce all'istanza corrente dell'oggetto.
    "        Si usa per accedere agli attributi e ai metodi della stessa classe.
    " me->gv_nome = iv_nome: Assegna il valore del parametro di input 'iv_nome' all'attributo privato 'gv_nome' dell'oggetto.
    me->gv_nome = iv_nome.
  ENDMETHOD.

  " get_nome: L'implementazione del metodo che restituisce il nome.
  METHOD get_nome.
    " rv_nome = me->gv_nome: Assegna il valore dell'attributo privato 'gv_nome' al parametro di ritorno 'rv_nome'.
    rv_nome = me->gv_nome.
  ENDMETHOD.

  " add_indirizzo: L'implementazione del metodo che aggiunge un indirizzo.
  METHOD add_indirizzo.
    " APPEND is_indirizzo TO me->gt_indirizzi: Aggiunge la struttura 'is_indirizzo' (ricevuta come parametro)
    "                                         come nuova riga nella tabella interna privata 'gt_indirizzi'.
    APPEND is_indirizzo TO me->gt_indirizzi.
  ENDMETHOD.

ENDCLASS. " ENDCLASS: Termina l'implementazione della classe.

\`

### CREATE OBJECT - Creazione Istanza
Crea una nuova istanza di una classe.

\`abap
CREATE OBJECT go_cliente
  EXPORTING
    iv_nome = 'Mario Rossi'.


" Popola la struttura con i dati di un indirizzo.
ls_indirizzo-via   = 'Via Roma 10'.
ls_indirizzo-citta = 'Milano'.
ls_indirizzo-cap   = '20121'.

" -> : È l'operatore per chiamare un metodo di un'istanza (oggetto).
" go_cliente->add_indirizzo(...): Chiama il metodo pubblico 'add_indirizzo' dell'oggetto a cui punta 'go_cliente',
"                                passando la struttura 'ls_indirizzo' come parametro.
go_cliente->add_indirizzo( ls_indirizzo ).


" Chiama il metodo pubblico 'get_nome' dell'oggetto. Il metodo restituisce il valore dell'attributo privato
" 'gv_nome', che viene salvato nella variabile locale 'lv_nome_cliente'.
lv_nome_cliente = go_cliente->get_nome( ).


" A questo punto, lv_nome_cliente contiene 'Mario Rossi'.
WRITE: / 'Nome del cliente creato:', lv_nome_cliente.
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
 WRITE: / 'Chiamata numero:', gv_contatore.
ENDFORM.

FORM gestione_cache USING p_chiave TYPE string
                   CHANGING p_risultato TYPE string.
 STATICS: gt_cache TYPE TABLE OF string,
          gv_inizializzato TYPE abap_bool.
 
 " Inizializzazione una sola volta
 IF gv_inizializzato = abap_false.
   APPEND 'CACHE_ITEM_1' TO gt_cache.
   APPEND 'CACHE_ITEM_2' TO gt_cache.
   gv_inizializzato = abap_true.
   WRITE: / 'Cache inizializzata'.
 ENDIF.
 
 " La cache persiste tra le chiamate
 READ TABLE gt_cache INTO p_risultato WITH KEY table_line = p_chiave.
 IF sy-subrc = 0.
   WRITE: / 'Trovato in cache:', p_risultato.
 ELSE.
   WRITE: / 'Non trovato in cache per:', p_chiave.
 ENDIF.
ENDFORM.

" Esempio di chiamate multiple
START-OF-SELECTION.
 PERFORM conta_chiamate.     " Output: Chiamata numero: 1
 PERFORM conta_chiamate.     " Output: Chiamata numero: 2
 PERFORM conta_chiamate.     " Output: Chiamata numero: 3
 
 DATA: lv_risultato TYPE string.
 PERFORM gestione_cache USING 'CACHE_ITEM_1' CHANGING lv_risultato.  " Cache inizializzata + Trovato
 PERFORM gestione_cache USING 'CACHE_ITEM_2' CHANGING lv_risultato.  " Solo: Trovato
 PERFORM gestione_cache USING 'NON_ESISTE' CHANGING lv_risultato.    " Non trovato
\`

### RANGES - Definizione Range
Crea tabelle di selezione per condizioni multiple simili a SELECT-OPTIONS.

\`abap
" Definizione del range
RANGES: r_matnr FOR mara-matnr.

" Valore singolo INCLUSO (EQUALS)
r_matnr-sign = 'I'.
r_matnr-option = 'EQ'.
r_matnr-low = 'MAT001'.
APPEND r_matnr.

" Intervallo INCLUSO (BETWEEN)
r_matnr-sign = 'I'.
r_matnr-option = 'BT'.
r_matnr-low = 'MAT100'.
r_matnr-high = 'MAT200'.
APPEND r_matnr.

" Valore singolo ESCLUSO
r_matnr-sign = 'E'.
r_matnr-option = 'EQ'.
r_matnr-low = 'MAT999'.
APPEND r_matnr.

" Pattern ESCLUSO (tutti tranne quelli che iniziano con TEMP)
r_matnr-sign = 'E'.
r_matnr-option = 'CP'.
r_matnr-low = 'TEMP*'.
APPEND r_matnr.

" Uso nella SELECT con range
SELECT * FROM mara INTO TABLE gt_materiali
 WHERE matnr IN r_matnr.

" SELECT equivalente SENZA range (molto più complessa)
SELECT * FROM mara INTO TABLE gt_materiali
 WHERE ( matnr = 'MAT001' OR 
         ( matnr BETWEEN 'MAT100' AND 'MAT200' ) )
   AND matnr <> 'MAT999'
   AND matnr NOT LIKE 'TEMP%'.
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

### Apertura di un File
Per aprire un file si usa l'istruzione OPEN DATASET seguita dal nome del file.
Vediamo un esempio:
\`abap
PARAMETERS filename(128) DEFAULT '/usr/tmp/testfile.dat'
           LOWER CASE.
DATA msg_text(50).

OPEN DATASET filename FOR OUTPUT IN TEXT MODE
     MESSAGE msg_text.
IF sy-subrc NE 0.
  WRITE: 'File cannot be opened for reason:', msg_text.
ENDIF.
\`
Qui, l'utente finale deve immettere il nome del file nella schermata di selezione che si presenta sul video, per mezzo dell'istruzione PARAMETERS, il cui attributo LOWER CASE indica che deve essere scritto con caratteri minuscoli. Il comando OPEN DATASET crea questo file se è inesistente, altrimenti il contenuto del file verrà sovrascritto dai nuovi dati.
Se vogliamo solamente aggiungere nuovi dati ad un file già esistente, occorrerà aprirlo con il comando OPEN DATASET seguito da FOR APPENDING di modo che i nuovi dati verranno aggiunti a quelli già presenti nel file.
L'opzione IN TEXT MODE dice al sistema che i dati sul file sono linee di testo, in contrapposizione a questo c'è il BINARY MODE, che viene usato anche quello principalmente per i dati binari e non per le righe di testo. Per i dati trasferiti in modalità testo, ogni record termina con un carattere di fine riga.

### Trasferimento dati ad un File
A questo punto i dati vengono inviati al file tramite l'istruzione TRANSFER.
\`abap
LOOP AT all_customers.
  TRANSFER all_customers-name TO filename.
ENDLOOP.
\`
In questo modo tutti i nomi dei clienti verranno trasferiti dalla tabella "all_customers" nel file il cui nome è contenuto nella variabile filename.
Un possibile problema è che durante il trasferimento può accadere che non ci sia sufficiente spazio sul disco dove è stato dichiarato il file.
È anche possibile trasferire tutti i campi all'interno del file in un unico trasferimento:
\`abap
LOOP AT all_customers.
  TRANSFER all_customers TO filename.
ENDLOOP.
\`
A questo punto tutti i campi della tabella vengono trasferiti nel file e il programma che leggerà questo file dovrà sapere come interpretare i records.
Di default tutti i records vengono scritti con la concatenazione dei rispettivi campi.

### Chiusura di un File
Anche se il sistema chiude automaticamente tutti i file alla fine del programma, per una buona programmazione è consigliabile chiudere esplicitamente i files all'interno del programma. Questo viene fatto con l'istruzione:
\`abap
CLOSE DATASET filename.
\`
Questa istruzione chiude il file e assicura che tutti i buffer interni vengano svuotati dopo ogni operazione di transfer o prima della chiusura del file.
L'istruzione DELETE DATASET elimina il file dal sistema, per cancellare completamente il file.

### Lettura di un File
Fino ad ora abbiamo solo visto come si scrive su un file, ora vediamo come leggere il contenuto di un file o più files. Si usa l'istruzione READ DATASET, come si vede nell'esempio che segue:
\`abap
PARAMETERS filename(40) 
           DEFAULT '/usr/tmp/testfile.dat'.
DATA msg_text(50).
DATA all_customer_names LIKE customers-name OCCURS 100
     WITH HEADER LINE.

OPEN DATASET filename FOR INPUT IN TEXT MODE
     MESSAGE msg_text.
IF sy-subrc NE 0.
  WRITE: 'File cannot be opened for reason:', msg_text.
ENDIF.

DO.
  READ DATASET filename INTO all_customer_names.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.
  APPEND all_customer_names.
ENDDO.

LOOP AT all_customer_names.
  WRITE all_customer_names.
ENDLOOP.
\`
In questo esempio il file viene aperto per l'operazione di lettura (FOR INPUT). Fare attenzione che il file debba essere aperto FOR INPUT. Il sistema restituirà un codice di ritorno diverso da zero se il file non viene trovato.
Il contenuto del file, invece, viene letto con tutti i nomi dei clienti e li carica su di una tabella interna. Viene usato un ciclo di DO. Poiché non sappiamo in anticipo quanti record sono presenti nel file, onde evitare di rimanere bloccati dal ciclo, dobbiamo testare il campo di sistema sy-subrc dopo ogni lettura: se il valore è diverso da zero vuol dire che non ci sono più record sotto il file e quindi si deve uscire dal ciclo. A questo punto segue un secondo ciclo di LOOP per stampare il contenuto della tabella interna che rappresenta il file.
Per utilizzare il programma più velocemente, utilizzare il tasto F4 dalla videata dell'Object Browser, e cercare la directory desiderata nella PARAMETERS.
La modifica da fare al programma è cambiare il tipo del record:
\`abap
DATA all_customers LIKE customers OCCURS 100
     WITH HEADER LINE.

READ DATASET filename INTO all_customers.
IF sy-subrc NE 0.
  EXIT.
ENDIF.
APPEND all_customers.
\`
In questo modo possiamo leggere e memorizzare tutti i campi del record customer.

# System Fields ABAP/4 - Campi di Sistema

Di seguito sono riportati i principali system fields dell'ABAP/4:

| SYSTEM FIELD | SIGNIFICATO |
|--------------|-------------|
| sy-batch | Processo attivo in background |
| sy-colno | Colonna corrente |
| sy-cpage | Numero di pagina |
| sy-cprog | Programma principale |
| sy-cucol | Posizione cursore (colonna) |
| sy-curow | Posizione cursore (riga) |
| sy-datum | Data di sistema |
| sy-dayst | Data e ora dell'ultimo salvataggio |
| sy-dbcnt | Numero di elementi con operazioni DB |
| sy-dbsys | Database system |
| sy-dyngr | Screen group |
| sy-dynnr | Numero della schermata corrente |
| sy-fdpos | Posizione di una stringa |
| sy-index | Indice di un ciclo loop |
| sy-langu | Lingua di logon |
| sy-linct | Numero di linee listate |
| sy-linno | Linea attuale in lista |
| sy-linsz | Ampiezza riga corrente |
| sy-lsind | Numero di stacked list |
| sy-mandt | Numero del client |
| sy-msgid | Identificativo del messaggio |
| sy-msgno | Numero del messaggio |
| sy-msgty | Tipo di messaggio (e, i, w,...) |
| sy-msgv1 | Variabile del messaggio |
| sy-msgv2 | Variabile del messaggio |
| sy-msgv3 | Variabile del messaggio |
| sy-msgv4 | Variabile del messaggio |
| sy-opsys | Sistema operativo |
| sy-pagno | Numero di pagina |
| sy-pfkey | Stato della GUI attuale |
| sy-pfstatus | Stato di definizione della GUI |
| sy-repid | Nome del report |
| sy-saprl | Release R/3 |
| sy-scols | Numero di caratteri di una riga |
| sy-subrc | Codice di ritorno |
| sy-sysid | ID sistema R/3 |
| sy-tabix | Indice di tabella |
| sy-tcode | Codice di transazione |
| sy-tvar0 | Variabile di testo |
| sy-tvar1 | Variabile di testo |
| sy-tvar2 | Variabile di testo |
| sy-tvar3 | Variabile di testo |
| sy-tvar4 | Variabile di testo |
| sy-tvar5 | Variabile di testo |
| sy-tvar6 | Variabile di testo |
| sy-tvar7 | Variabile di testo |
| sy-tvar8 | Variabile di testo |
| sy-tvar9 | Variabile di testo |
| sy-tzone | Fuso orario GMT |
| sy-ucomm | Codice pressione tasto funzione |
| sy-uline | Sottolineatura |
| sy-uname | User |
| sy-uzeit | Ora di sistema |
| sy-vline | Linea verticale |
| sy-winco | Colonna del cursore in una finestra |
| sy-winro | Posizione cursore nella finestra |

## Note sui System Fields più utilizzati

### Campi di controllo del flusso
- **sy-subrc**: Codice di ritorno più importante, indica il successo (0) o fallimento (≠0) di un'operazione
- **sy-index**: Contatore automatico nei cicli DO e WHILE
- **sy-tabix**: Indice della riga corrente nelle operazioni su tabelle interne

### Campi di sistema e ambiente
- **sy-datum**: Data corrente del sistema (formato YYYYMMDD)
- **sy-uzeit**: Ora corrente del sistema (formato HHMMSS)
- **sy-uname**: Nome dell'utente collegato
- **sy-mandt**: Client/mandante corrente
- **sy-langu**: Lingua di login dell'utente

### Campi per messaggi
- **sy-msgty**: Tipo di messaggio (E=Error, W=Warning, I=Information, S=Success)
- **sy-msgid**: Classe di messaggio
- **sy-msgno**: Numero del messaggio
- **sy-msgv1-sy-msgv4**: Variabili per parametrizzare i messaggi

### Campi per database
- **sy-dbcnt**: Numero di righe elaborate dall'ultima operazione di database
- **sy-dbsys**: Tipo di sistema di database utilizzato




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

const GuidaAbap = () => {
  return <IndexedContent text={content} />;
};

export default GuidaAbap;
