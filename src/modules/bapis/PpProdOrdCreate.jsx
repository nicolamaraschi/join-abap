export const bapi = {
  "name": "BAPI_PRODORD_CREATE",
  "description": "Utilizzata per la creazione di ordini di produzione.",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori",
      "structures": [
        { "name": "ORDERDATA", "type": "BAPI_PP_ORDER_CREATE", "fields": [
          { "name": "MATERIAL", "desc": "Materiale di testata per l'ordine.", "mandatory": true },
          { "name": "PLANT", "desc": "Divisione di produzione.", "mandatory": true },
          { "name": "ORDER_TYPE", "desc": "Tipo ordine.", "mandatory": true },
          { "name": "QUANTITY", "desc": "Quantità totale dell'ordine.", "mandatory": true },
          { "name": "BASIC_START_DATE", "desc": "Data di inizio base.", "mandatory": true },
          { "name": "BASIC_END_DATE", "desc": "Data di fine base.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "L'obbligatorietà delle date dipende dalla configurazione del tipo di schedulazione. Richiede BAPI_TRANSACTION_COMMIT per il salvataggio." }
  ],
  "content": ``
};
