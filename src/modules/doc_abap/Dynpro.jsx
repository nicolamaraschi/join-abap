// src/modules/doc_abap/Dynpro.jsx
// QUESTO È IL CODICE CORRETTO E DEFINITIVO.
// Sostituisci l'intero contenuto del file con questo.
// Ora esporta solo una stringa, come gli altri documenti, risolvendo l'errore.

export const content = `
# Guida al Popup Dynpro: Approccio Ibrido

**Obiettivo:** Creare un popup funzionale e professionale, definendo i pulsanti principali sul layout e mantenendo la flessibilità di un titolo e una toolbar personalizzati tramite codice.

---

### Fase 1: Il Programma ABAP (Il "Cervello")

Questa fase definisce la logica di base: le variabili globali per lo scambio di dati e i moduli PBO (Process Before Output) e PAI (Process After Input).

#### 1.1 Dati Globali

\`\`\`abap
DATA: gv_dato_da_passare TYPE char50,   " Variabile per impostare il titolo dinamicamente
      gv_dato_da_ricevere TYPE char10.    " Variabile per ricevere il valore dall'input sul Dynpro
\`\`\`

#### 1.2 FORM per la Chiamata del Dynpro

\`\`\`abap
FORM f_chiama_popup_dynpro.
  CLEAR gv_dato_da_ricevere. " Pulisce il campo di ricezione prima di mostrare il popup
  CALL SCREEN '0100' STARTING AT 10 5
                     ENDING AT 80 15. " Lancia il Dynpro come finestra modale
ENDFORM.
\`\`\`

#### 1.3 Modulo PBO (Prepara lo Schermo)

Questo modulo viene eseguito prima che il Dynpro venga visualizzato.

\`\`\`abap
MODULE pbo_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'TITLE_0100' WITH gv_dato_da_passare.
ENDMODULE.
\`\`\`

#### 1.4 Modulo PAI (Reagisce all'Interazione Utente)

Questo modulo gestisce le azioni dell'utente, come la pressione di un pulsante.

\`\`\`abap
MODULE pai_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'CONFERMA'.
      " Qui va inserita la logica per processare gv_dato_da_ricevere
      MESSAGE 'Dato ricevuto:' && gv_dato_da_ricevere TYPE 'S'.
      LEAVE TO SCREEN 0. " Chiude il popup
    WHEN 'ESCI' OR 'ANNULLA'.
      LEAVE TO SCREEN 0. " Chiude il popup
  ENDCASE.
ENDMODULE.
\`\`\`

---

### Fase 2: Il Dynpro (Screen Painter in SE80)

Questa è la guida dettagliata per modificare e collegare gli elementi del Dynpro direttamente da SE80.

#### 2.1 Navigazione Iniziale in SE80
1.  Apri la transazione **SE80**.
2.  Dal menu a tendina, seleziona **"Programma"** e inserisci il nome del tuo Z-program.
3.  Nell'albero degli oggetti a sinistra, apri la cartella **"Schermate"** (Screens).
4.  Fai doppio click sul numero di schermo (es. **0100**).
5.  Assicurati di essere sulla scheda **"Layout"** per aprire l'editor visuale (Screen Painter).

#### 2.2 Collegamento degli Elementi nel Layout

**A) Collegare il Campo di Input alla Variabile ABAP**
* Nel Layout, fai doppio click sul campo di input.
* Nella finestra delle proprietà, trova il campo **"Nome"** (nel riquadro "Attributi del dizionario").
* **Azione Cruciale:** Inserisci il nome esatto della variabile globale ABAP che riceverà il valore: \`GV_DATO_DA_RICEVERE\`.

**B) Assegnare i Codici Funzione (FctCode) ai Pulsanti**
* Nel Layout, fai doppio click sul pulsante "Conferma".
* Nella finestra delle proprietà, trova il campo **"Cod. funzione"** (FctCode).
* **Azione Cruciale:** Scrivi il codice che userai nel \`CASE\` del modulo PAI. Per l'esempio: \`CONFERMA\`.
* Ripeti l'operazione per il pulsante "Annulla", inserendo nel campo "Cod. funzione" il valore \`ESCI\`.

**C) Impostare il Flow Logic**
* Sempre dalla schermata del Dynpro 0100, vai alla scheda **"Flow Logic"**.
* Verifica che la logica di processo sia definita correttamente:

\`\`\`abap
PROCESS BEFORE OUTPUT.
  MODULE pbo_0100.

PROCESS AFTER INPUT.
  MODULE pai_0100.
\`\`\`

---

### Fase 3: Oggetti di Contorno (GUI Status e Titolo)

Questi oggetti, creati con la transazione SE41, definiscono la barra degli strumenti e il titolo del popup.

* **GUI Status (STATUS_0100):** Definisci qui i codici funzione (es. \`CONFERMA\`, \`ESCI\`) se vuoi che appaiano nella toolbar standard, oltre che come pulsanti sul Dynpro.
* **Titolo (TITLE_0100):** Crea un titolo con un placeholder (\`&1\`) che verrà sostituito dinamicamente dalla variabile \`gv_dato_da_passare\` grazie all'istruzione \`SET TITLEBAR ... WITH ...\`.

---

### Promemoria Finale: L'ATTIVAZIONE!

**Ricorda sempre di attivare tutti gli oggetti dopo ogni modifica.** Il modo più sicuro è farlo da SE80, cliccando con il tasto destro sul nome del programma principale e selezionando **Attiva**. Questo attiverà il programma e tutti i suoi componenti associati (Dynpro, GUI Status, etc.).
`;
