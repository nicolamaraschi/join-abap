export const bapi = {
  "name": "BAPI_BANK_CREATE",
  "description": "Crea dati anagrafici banca.",
  "details": [
    {
      "title": "Parametri di Importazione Obbligatori",
      "structures": [
        { "name": "Import Parameters", "type": "Singoli", "fields": [
          { "name": "BANK_CTRY", "desc": "Paese della banca (chiave).", "mandatory": true },
          { "name": "BANK_KEY", "desc": "Chiave banca (es. ABI/CAB o SWIFT).", "mandatory": true }
        ]},
        { "name": "BANK_ADDRESS", "type": "BAPIADDRESS", "fields": [
          { "name": "BANK_NAME", "desc": "Nome della banca.", "mandatory": true },
          { "name": "STREET", "desc": "Via e numero civico.", "mandatory": true },
          { "name": "CITY", "desc": "Città.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "Richiede BAPI_TRANSACTION_COMMIT per il salvataggio." }
  ],
  "content": ``
};
