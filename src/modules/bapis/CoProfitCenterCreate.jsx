export const bapi = {
  "name": "BAPI_PROFITCENTER_CREATE",
  "description": "Creare Centro di Profitto.",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori",
      "structures": [
        { "name": "CONTROLLINGAREA", "type": "Singolo", "fields": [
          { "name": "CONTROLLINGAREA", "desc": "Area di controlling.", "mandatory": true }
        ]},
        { "name": "PROFITCENTER", "type": "Singolo", "fields": [
          { "name": "PROFITCENTER", "desc": "Codice del nuovo centro di profitto.", "mandatory": true }
        ]},
        { "name": "VALIDFROM", "type": "Singolo", "fields": [
          { "name": "VALIDFROM", "desc": "Data inizio validità.", "mandatory": true }
        ]},
        { "name": "BASICDATA", "type": "BAPI0015_1", "fields": [
          { "name": "PRCTR_NAME", "desc": "Nome del centro di profitto.", "mandatory": true },
          { "name": "IN_CHARGE", "desc": "Responsabile.", "mandatory": true },
          { "name": "PRCTR_HIER_GRP", "desc": "Nodo della gerarchia standard.", "mandatory": true }
        ]},
        { "name": "COMPANYCODEASSIGNMENT", "type": "Tabella di BAPI0015_4", "fields": [
          { "name": "COMP_CODE", "desc": "Società da assegnare al centro di profitto.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "Richiede l'assegnazione ad almeno una società. Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": ``
};
