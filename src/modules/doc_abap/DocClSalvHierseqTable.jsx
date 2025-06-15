export const content = `
Una Guida Definitiva per Sviluppatori a CL_SALV_HIERSEQ_TABLE
==============================================================

Parte 1: Concetti Fondamentali e Implementazione di Base
---------------------------------------------------------
Questa sezione stabilisce lo scopo principale e i meccanismi fondamentali della classe CL_SALV_HIERSEQ_TABLE.

### Sezione 1.1: Il Modello di Dati Gerarchico-Sequenziale

La classe CL_SALV_HIERSEQ_TABLE è una delle tre classi ALV principali all'interno del SALV Object Model, progettata per il rendering di una lista **gerarchico-sequenziale**. Questa struttura è rigorosamente limitata a **due livelli gerarchici**: un livello di intestazione (master) e un livello di dettaglio (slave).

L'implementazione richiede due tabelle interne separate (es. SCARR per il master, SFLIGHT per il dettaglio). Il concetto centrale che lega questi due livelli è la **"chiave di binding"** (binding key), una relazione di tipo foreign-key che lo sviluppatore deve definire esplicitamente.

### Sezione 1.2: Istanziazione tramite il Metodo FACTORY

Come per tutte le classi SALV, il punto di ingresso è il metodo statico \`FACTORY\`. Questo metodo istanzia la classe ALV e definisce le tabelle dati che forniranno la struttura.

\`\`\`abap
REPORT z_hierseq_basic_demo.

DATA:
  gt_parent  TYPE TABLE OF scarr,
  gt_child   TYPE TABLE OF sflight,
  gr_table   TYPE REF TO cl_salv_hierseq_table,
  lt_binding TYPE salv_t_hierseq_binding,
  ls_binding TYPE salv_s_hierseq_binding.

START-OF-SELECTION.
  " §1: Selezionare i dati per entrambi i livelli
  SELECT * FROM scarr INTO TABLE gt_parent.
  SELECT * FROM sflight INTO TABLE gt_child.

  " §2: Creare le informazioni di binding tra master e slave
  ls_binding-master = 'CARRID'.
  ls_binding-slave  = 'CARRID'.
  APPEND ls_binding TO lt_binding.

  " §3: Creare l'istanza ALV tramite il metodo FACTORY
  TRY.
      cl_salv_hierseq_table=>factory(
        EXPORTING
          t_binding_level1_level2 = lt_binding
        IMPORTING
          r_hierseq               = gr_table
        CHANGING
          t_table_level1          = gt_parent
          t_table_level2          = gt_child ).
    CATCH cx_salv_data_error.
      MESSAGE 'Error during ALV instantiation.' TYPE 'E'.
  ENDTRY.

  " §4: Visualizzare la tabella
  gr_table->display( ).
\`\`\`

#### Tabella 1: Parametri del Metodo CL_SALV_HIERSEQ_TABLE=>FACTORY

| Nome Parametro              | Tipo Parametro | Tipo di Dati                  | Descrizione                                                                 |
|-----------------------------|----------------|-------------------------------|-----------------------------------------------------------------------------|
| t_binding_level1_level2     | EXPORTING      | salv_t_hierseq_binding        | Tabella che definisce le relazioni di chiave esterna tra i due livelli.     |
| r_hierseq                   | IMPORTING      | REF TO cl_salv_hierseq_table  | Restituisce il riferimento all'oggetto ALV istanziato.                      |
| t_table_level1              | CHANGING       | STANDARD TABLE                | La tabella interna che contiene i dati per il livello di intestazione (master).|
| t_table_level2              | CHANGING       | STANDARD TABLE                | La tabella interna che contiene i dati per il livello di dettaglio (slave). |

---

Parte 2: Configurazione e Personalizzazione dell'ALV
------------------------------------------------------

### Sezione 2.1: Gestione Completa delle Colonne

La gestione delle colonne segue il modello universale del SALV. Si ottiene l'oggetto collezione tramite \`gr_hierseq->get_columns( level )\` (dove level è 1 per master, 2 per slave) e poi si accede alla singola colonna con \`lr_columns->get_column( 'NOME_COLONNA' )\`.

Un problema comune è il riposizionamento di una colonna chiave DDIC con \`set_column_position()\`. Per risolvere, si deve prima chiamare \`lr_column->set_key( if_salv_c_bool_sap=>false )\` per istruire il framework a non trattare più quella colonna come fissa.

### Sezione 2.4: Implementazione della Funzionalità di Espansione/Compressione

Per abilitare questa funzione, è necessario:
1.  Aggiungere una colonna "trigger" (es. \`expcol TYPE c\`) alla tabella dati master.
2.  Istruire il framework ALV a usare questa colonna: \`gr_columns->set_expand_column( 'EXPCOL' )\`.

Per espandere tutti i nodi all'avvio:
\`\`\`abap
DATA(gr_level) = gr_table->get_level( 1 ).
gr_level->set_items_expanded( abap_true ).
\`\`\`

---

Parte 3: Interattività Avanzata e Gestione Dinamica dei Dati
--------------------------------------------------------------

### Sezione 3.1: Guida Pratica alla Gestione degli Eventi

Si ottiene l'oggetto evento con \`gr_table->get_event()\` e si registrano i gestori con \`SET HANDLER\`.
* **DOUBLE_CLICK** e **LINK_CLICK**: per interazioni sulle celle.
* **ON_ADDED_FUNCTION**: per pulsanti personalizzati.

### Sezione 3.2: Aggiornamento Dinamico dei Dati

Il metodo \`gr_hierseq->refresh()\` forza il framework a rileggere i dati dalle tabelle interne e a ridisegnare la griglia. Per sostituire completamente i dati e il binding a runtime, si usa il metodo \`SET_DATA\`.

---

Parte 4: Limitazioni Critiche e Sfide di Implementazione Comuni
------------------------------------------------------------------
Questa sezione affronta le questioni non documentate che causano problemi agli sviluppatori.

### Sezione 4.1: L'Anomalia dello Stato di Selezione all'Espansione/Compressione

**Limitazione Fondamentale:** Quando un utente espande o comprime un nodo master, **tutte le selezioni di riga effettuate in precedenza vengono perse**. Questo è un comportamento sistemico del framework. La classe \`CL_SALV_HIERSEQ_TABLE\` non è adatta per scenari interattivi complessi che richiedono il mantenimento dello stato di selezione durante l'espansione. Per questi requisiti, è necessario usare \`CL_SALV_TREE\`.

### Sezione 4.2: Il Vincolo di Modificabilità delle Celle

**Limitazione Fondamentale:** I campi modificabili (diversi dalle checkbox) **non sono supportati** utilizzando l'ALV Object Model (classi SALV). Se una griglia gerarchica modificabile è un requisito, si devono usare tecnologie come \`CL_GUI_ALV_GRID\`.

### Sezione 4.3: La Limitazione dell'Integrazione in un Container

**Limitazione Fondamentale:** A differenza di \`CL_SALV_TABLE\` e \`CL_SALV_TREE\`, il metodo \`FACTORY\` di \`CL_SALV_HIERSEQ_TABLE\` **non ha un parametro per un container**. Di conseguenza, questa classe può essere utilizzata **esclusivamente per visualizzazioni a schermo intero**.
`;
