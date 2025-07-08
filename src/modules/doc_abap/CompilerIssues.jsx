export const content = `
Guida alle Specificità del Compilatore ABAP
============================================

**Obiettivo:** Fornire una guida di riferimento chiara e diretta sulle specificità e le limitazioni dell'ambiente di sviluppo ABAP, per garantire che tutto il codice sia funzionante, manutenibile e conforme agli standard richiesti.

---

## Parte 1: La "Carta d'Identità" del Compilatore
L'ambiente per cui genererai codice ha le seguenti caratteristiche fondamentali, che devono essere rispettate in ogni sviluppo.

### 1.1 Versione del Linguaggio (Pre-7.51)
La prova definitiva è la totale assenza della sintassi \`BEGIN OF ENUM\`. Il compilatore non riconosce la parola chiave, obbligando a simulare gli ENUM tramite classi locali o costanti, se necessario.

### 1.2 Architettura S/4HANA Ibrida
Il sistema possiede il nuovo "cuore" digitale di S/4HANA (es. tabelle \`ACDOCA\` e \`MATDOC\`), ma per molti processi aziendali chiave (SD, MM, MRP, Credito, Output) si affida ancora all'architettura e alle tabelle del mondo SAP ECC classico.

### 1.3 Sintassi Open SQL Classica
È obbligatorio usare la virgola (,) per separare i campi in una \`SELECT\`. La sintassi più moderna con gli spazi non è supportata. Tuttavia, il sistema supporta costrutti complessi come \`LEFT JOIN\`, aggregazioni (\`SUM\`), e raggruppamenti (\`GROUP BY\`).

### 1.4 Dichiarazioni Inline (\`@DATA\`): Supporto Parziale
La sintassi \`@DATA(...)\` è valida solo in alcuni contesti semplici. Non è supportata in contesti più complessi come \`LOOP AT ... INTO @DATA(...)\` o in \`SELECT\` con \`JOIN\`.

### 1.5 Sintassi Moderna (post 7.40): Supporto Misto
Operatori come \`COND\` e \`SWITCH\` sono compatibili. Funzionalità più avanzate come i "String Templates" (\`|...|\` o \`&&\`) non sono supportate.

### 1.6 Object-Oriented ABAP di Base
Il costrutto \`TRY...CATCH\` è pienamente funzionante. Tuttavia, la libreria di classi di eccezione standard (\`CX_...\`) è incompleta.

### 1.7 Chiamate Dinamiche
Le chiamate \`PERFORM\` dinamiche richiedono obbligatoriamente la sintassi esplicita \`IN PROGRAM sy-repid\`.

### 1.8 SELECTION-SCREEN Rigorosamente Classica
I nomi degli elementi del selection-screen non possono superare gli 8 caratteri.

---

## Parte 2: Regole e Requisiti Fondamentali

### 2.1 Lingua e Nomenclatura (Italiano Mandatorio)
- **Commenti:** Solo in italiano, usando \`"\` (doppio apice).
- **Nomenclatura:** Nomi di variabili, costanti, tipi, etc., in italiano e descrittivi.
- **Convenzioni:** Prefissi standard (\`p_\`, \`so_\`, \`lt_\`, \`ls_\`, \`gv_\`, \`gc_\`).
- **Lunghezza Nomi:** Massimo 8 caratteri per \`SELECTION-SCREEN\`, 30 per tutto il resto.

### 2.2 Struttura del Programma (Modulare con INCLUDE)
Genera sempre un programma principale che richiama altri \`INCLUDE\` per separare le responsabilità:
1.  **INCLUDE TOP:** Per tutte le dichiarazioni di dati globali (\`TYPES\`, \`DATA\`, \`CONSTANTS\`, \`TABLES\`, \`SELECTION-SCREEN\`).
2.  **INCLUDE Logica:** Almeno un \`INCLUDE\` per la logica applicativa principale, implementata tramite \`FORM\`.

---

## Parte 3: Sintassi ABAP e Specificità

### 3.1 Sintassi Proibite
> L'uso dei seguenti costrutti causerà errori di compilazione e deve essere evitato:
> - Costruttore \`VALUE type(...)\`
> - Tipi \`MESH\`
> - Tipi \`ENUM\`
> - \`MESSAGE ... WITH\`
> - String Templates \`|...|\` o \`&&\`

### 3.2 Regole Specifiche
- **WRITE multiplo:** Obbligatorio l'uso dei due punti (\`:\`).
- **Tabelle HASHED:** Accesso solo con \`WITH KEY\`, non tramite indice.
- **LOOP AT ... ASSIGNING:** Non usare il field-symbol nella clausola \`WHERE\`.

---

## Parte 4: Architettura del Database (S/4HANA Ibrido)
#### 📋 Tabella 4.1: Mappatura Tabelle Centrali per Modulo
| Modulo | Tabella Centrale | Note |
|---|---|---|
| **FI/CO** | \`ACDOCA\` | Prioritizzare sempre la lettura da questa tabella. |
| **MM-IM** | \`MATDOC\` | \`MSEG\` e \`MARD\` sono ancora tabelle fisiche. |
| **SD** | \`VBUK\` / \`VBUP\` | La "Semplificazione SD" NON è attiva. |
| **PP (MRP)** | Tabelle classiche | Utilizza il MRP classico, non MRP Live. |
| **Output** | \`NAST\` | Utilizza la gestione output classica. |

---

## Parte 5: Gestione delle Incertezze
**Ricorda sempre**: se non hai la certezza assoluta su collegamenti, campi o funzionamento di BAPI/FM, **NON INVENTARE**. È mandatorio fermarsi e chiedere chiarimenti al team funzionale o a un analista senior.

---

## Parte 6: Pattern di Codice

### 6.1 Chiamata BAPI Generica
\`\`\`abap
" DEFINIZIONE VARIABILI
DATA: lt_return_bapi TYPE STANDARD TABLE OF bapiret2,
      ls_return_bapi TYPE bapiret2.

" CHIAMATA BAPI
CALL FUNCTION 'NOME_BAPI_DA_USARE'
  TABLES
    return = lt_return_bapi.

" ANALISI RETURN
LOOP AT lt_return_bapi INTO ls_return_bapi WHERE type CA 'EA'.
  " Gestione errore
ENDLOOP.
\`\`\`
**Azione Cruciale:** Analizzare sempre la tabella \`return\` per verificare la presenza di messaggi di tipo 'E' (Error) o 'A' (Abort). In base al risultato, eseguire esplicitamente il commit o il rollback della transazione.
\`\`\`abap
" COMMIT / ROLLBACK
IF sy-subrc = 0.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
ELSE.
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
ENDIF.
\`\`\`

### 6.2 Stampa SmartForm (Dinamica)
\`\`\`abap
DATA: p_form TYPE tdsfname VALUE 'NOME_SMARTFORM',
      fm_name TYPE rs38l_fnam.

CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
  EXPORTING
    formname = p_form
  IMPORTING
    fm_name  = fm_name.

IF sy-subrc = 0.
  CALL FUNCTION fm_name
    EXPORTING
      control_parameters = ...
      output_options     = ...
    EXCEPTIONS
      ...
ENDIF.
\`\`\`

### 6.3 Invio Email con Allegato (BCS)
\`\`\`abap
FORM f_send_email_generic.
  DATA: lo_bcs TYPE REF TO cl_bcs.
  TRY.
      lo_bcs = cl_bcs=>create_persistent( ).
      " ... creazione documento e aggiunta allegato ...
      lo_bcs->send( ).
      COMMIT WORK.
    CATCH cx_bcs.
      " Gestione errore
  ENDTRY.
ENDFORM.
\`\`\`

### 6.4 Lettura File Excel
\`\`\`abap
" 1. Parametro per il percorso
PARAMETERS: p_file TYPE rlgrap-filename.

" 2. F4 Help
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME' IMPORTING file_name = p_file.

" 3. Lettura dati
CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
  EXPORTING
    i_filename = p_file
  TABLES
    i_tab_raw_data = DATA(lt_raw_data).
\`\`\`
`;