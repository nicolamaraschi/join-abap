export const bapi = {
  "name": "BAPI_PRODORD_RELEASE",
  "description": "Rilascia uno o più ordini di produzione, rendendoli pronti per l'esecuzione (es. movimenti merci, conferme).",
  "details": [
    {
      "title": "Parametri di Importazione Chiave",
      "structures": [
        { "name": "Import", "type": "Singolo o Tabella", "fields": [
          { "name": "ORDER_NUMBER", "desc": "Numero Ordine di Produzione per rilascio singolo.", "mandatory": false },
          { "name": "ORDERS", "desc": "Tabella di Chiavi Ordine (BAPI_ORDER_KEY) per rilascio multiplo.", "mandatory": false }
        ]}
      ]
    },
    { "title": "Note", "content": "È necessario fornire o un singolo numero d'ordine o una tabella di ordini. Il rilascio è un passo cruciale per l'esecuzione dell'ordine. Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": ``
};
