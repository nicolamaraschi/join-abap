// src/modules/doc_abap/Dynpro.jsx
export const dynproDoc = {
    id: 'ABAP_POPUP_DYNPRO',
    title: 'Guida Definitiva: Popup Dynpro (Approccio Ibrido con Navigazione Dettagliata)',
    content: `Obiettivo: Creare un popup funzionale e professionale, con i pulsanti principali definiti sul layout e con la flessibilità di un titolo e una toolbar personalizzati.

Fase 1: Il Programma ABAP (Il "Cervello")
(Questa fase rimane invariata, con la dichiarazione delle variabili globali e la definizione dei moduli PBO e PAI)

1. Dati Globali:

\`\`\`ABAP
DATA: gv_dato_da_passare TYPE char50,   " Per il titolo
      gv_dato_da_ricevere TYPE char10.    " Per il valore dall'input
\`\`\`
2. FORM che Chiama il Dynpro:

\`\`\`ABAP
FORM f_chiama_popup_dynpro.
  CLEAR gv_dato_da_ricevere.
  CALL SCREEN '0100'.
ENDFORM.
\`\`\`
3. Modulo PBO (Prepara lo Schermo):

\`\`\`ABAP
MODULE pbo_0100 OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'TITLE_0100' WITH gv_dato_da_passare.
ENDMODULE.
\`\`\`
4. Modulo PAI (Reagisce ai Pulsanti):

\`\`\`ABAP
MODULE pai_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'CONFERMA'.
      " ... logica ...
    WHEN 'ESCI'.
      " ... logica ...
  ENDCASE.
ENDMODULE.
\`\`\`
Fase 2: Il Dynpro (SE80 - Dove Trovare e Impostare i Collegamenti)
Questa è la guida dettagliata che hai chiesto per modificare il Dynpro partendo da SE80.

Navigazione Iniziale:

* Apri la transazione SE80.
* Dal menu a tendina, seleziona "Programma" e inserisci il nome del tuo programma. Premi Invio.
* Nell'albero degli oggetti a sinistra, apri la cartella "Schermate" (Screens).
* Fai doppio click sul tuo numero di schermo: 0100.
* Nella parte destra dello schermo, assicurati di essere sulla scheda "Layout". Si aprirà l'editor visuale (il Screen Painter).

Ora che sei nel Layout, ecco come collegare gli elementi:

A) Collegare il Campo di Input (il tuo "textbar") alla Variabile ABAP
* Nel Layout, fai doppio click sul campo dove l'utente deve inserire il valore.
* Si aprirà la finestra delle "Proprietà del campo".
* Trova il campo di input etichettato "Nome" (di solito si trova nel riquadro "Attributi del dizionario").
* Azione Cruciale: Qui devi scrivere il nome esatto della tua variabile globale ABAP che deve ricevere il valore. Per il nostro esempio: gv_dato_da_ricevere.

B) Assegnare i Codici Funzione ai Pulsanti
* Sempre nel Layout, fai doppio click sul tuo pulsante "Conferma".
* Nella finestra delle "Proprietà del campo", guarda il riquadro "Attributi generali".
* Trova il campo di input etichettato "Cod. funzione" (FctCode).
* Azione Cruciale: Qui devi scrivere il codice che userai nel CASE del tuo modulo PAI. Per il nostro esempio: CONFERMA.
* Clicca OK. Ora ripeti per l'altro pulsante: fai doppio click sul pulsante "Annulla" e nel campo "Cod. funzione" scrivi ESCI.

C) Impostare il Flow Logic
* Sempre dalla schermata del Dynpro 0100, vai alla scheda "Flow Logic".
* Assicurati che il testo sia questo:
\`\`\`ABAP
PROCESS BEFORE OUTPUT.
  MODULE pbo_0100.

PROCESS AFTER INPUT.
  MODULE pai_0100.
\`\`\`
Fase 3: Oggetti di Contorno (SE41, Titolo e Status)
(Questa fase rimane invariata, si creano il TITLE_0100 e lo STATUS_0100 come descritto in precedenza)

Promemoria Finale: L'ATTIVAZIONE!
Ricorda sempre di attivare tutti gli oggetti dopo ogni modifica. Il modo più sicuro è da SE80, cliccando con il destro sul nome del programma e scegliendo Attiva.
`
};