export const bapi = {
  "name": "EAM_TASKLIST_CREATE",
  "description": "Creazione di liste di cicli operativi (task list) per la manutenzione.",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori (inferiti)",
      "structures": [
        { "name": "Task list header data", "type": "Struttura di testata", "fields": [
          { "name": "PLNTY", "desc": "Tipo Lista Cicli ('A' Equipment, 'T' U.T.).", "mandatory": true },
          { "name": "KTEXT", "desc": "Descrizione Lista Cicli.", "mandatory": true },
          { "name": "WERKS", "desc": "Divisione di pianificazione.", "mandatory": true },
          { "name": "VERWE", "desc": "Impiego del ciclo operativo (es. '4' Manutenzione).", "mandatory": true },
          { "name": "STATU", "desc": "Stato del ciclo (es. '4' Rilasciato).", "mandatory": true }
        ]},
        { "name": "Operations", "type": "Tabella di operazioni", "fields": [
          { "name": "VORNR", "desc": "Numero operazione (es. '0010').", "mandatory": true },
          { "name": "ARBPL", "desc": "Centro di Lavoro.", "mandatory": true },
          { "name": "STEUS", "desc": "Chiave di Controllo.", "mandatory": true },
          { "name": "LTXA1", "desc": "Descrizione dell'operazione.", "mandatory": true },
          { "name": "ARBEI", "desc": "Lavoro/Durata dell'operazione.", "mandatory": true },
          { "name": "ARBEH", "desc": "Unità di misura del lavoro (es. 'H').", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "Questo FM è usato negli scenari di migrazione dati. L'analisi diretta dei suoi parametri nel sistema SAP è raccomandata. Richiede un commit della transazione." }
  ],
  "content": ``
};
