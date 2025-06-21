export const bapi = {
  "name": "BAPI_REQUISITION_CREATE",
  "description": "Crea una richiesta d'acquisto (RdA).",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori",
      "structures": [
        { "name": "REQUISITION_ITEMS", "type": "Tabella di BAPIEBANC", "fields": [
          { "name": "DOC_TYPE", "desc": "Tipo documento RdA.", "mandatory": true },
          { "name": "MATERIAL", "desc": "Numero materiale (o SHORT_TEXT per RdA di testo).", "mandatory": false },
          { "name": "PLANT", "desc": "Divisione.", "mandatory": true },
          { "name": "QUANTITY", "desc": "Quantità richiesta.", "mandatory": true },
          { "name": "DELIV_DATE", "desc": "Data di consegna richiesta.", "mandatory": true },
          { "name": "PUR_GROUP", "desc": "Gruppo acquisti.", "mandatory": true }
        ]},
        { "name": "REQUISITION_ACCOUNT_ASSIGNMENT", "type": "Tabella di BAPIEBANKN", "fields": [
          { "name": "PREQ_ITEM", "desc": "Numero posizione RdA a cui si riferisce l'imputazione.", "mandatory": true },
          { "name": "G_L_ACCT", "desc": "Conto Co.Ge.", "mandatory": true },
          { "name": "COST_CTR", "desc": "Centro di costo.", "mandatory": false }
        ]}
      ]
    },
    { "title": "Note", "content": "Se la posizione ha un tipo di imputazione (es. 'K' per centro di costo), la tabella REQUISITION_ACCOUNT_ASSIGNMENT deve essere compilata. Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": ``
};
