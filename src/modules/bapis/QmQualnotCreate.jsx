export const bapi = {
  "name": "BAPI_QUALNOT_CREATE",
  "description": "Crea una notifica sulla qualità (avviso QM).",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori",
      "structures": [
        { "name": "NOTIF_TYPE", "type": "Singolo", "fields": [
          { "name": "NOTIF_TYPE", "desc": "Tipo di notifica (es. 'F2' per Reclamo fornitore).", "mandatory": true }
        ]},
        { "name": "NOTIFHEADER", "type": "BAPI2078_NOTHDRI", "fields": [
          { "name": "SHORT_TEXT", "desc": "Testo breve della notifica.", "mandatory": true },
          { "name": "PRIORITY", "desc": "Priorità della notifica.", "mandatory": false }
        ]}
      ]
    },
    { "title": "Note", "content": "Dopo la creazione, si possono aggiungere posizioni, cause e attività con BAPI_QUALNOT_ADD_DATA. Richiede BAPI_TRANSACTION_COMMIT per salvare la notifica." }
  ],
  "content": ``
};
