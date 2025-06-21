export const bapi = {
  "name": "BAPI_PROJECT_MAINTAIN",
  "description": "BAPI principale per creare e modificare progetti completi (definizioni, elementi WBS, attività di rete).",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori (per Creazione)",
      "structures": [
        { "name": "I_METHOD_PROJECT", "type": "Tabella di BAPI_METHOD_PROJECT", "fields": [
          { "name": "REFNUMBER", "desc": "Numero di riferimento per l'oggetto (es. '1').", "mandatory": true },
          { "name": "OBJECTTYPE", "desc": "Tipo Oggetto (es. 'PROJECTDEFINITION', 'WBSELEMENT').", "mandatory": true },
          { "name": "METHOD", "desc": "Metodo (es. 'CREATE').", "mandatory": true },
          { "name": "OBJECTKEY", "desc": "Chiave oggetto (es. il codice del progetto/WBS).", "mandatory": true }
        ]},
        { "name": "I_PROJECT_DEFINITION", "type": "Tabella di BAPI_PROJECT_DEFINITION_I", "fields": [
          { "name": "PROJECT_DEFINITION", "desc": "Codice della definizione del progetto.", "mandatory": true },
          { "name": "DESCRIPTION", "desc": "Descrizione del progetto.", "mandatory": true },
          { "name": "PROJ_PROFILE", "desc": "Profilo del progetto.", "mandatory": true },
          { "name": "COMP_CODE", "desc": "Società.", "mandatory": true }
        ]},
        { "name": "I_WBS_ELEMENT", "type": "Tabella di BAPI_WBS_ELEMENT_I", "fields": [
          { "name": "WBS_ELEMENT", "desc": "Codice dell'elemento WBS.", "mandatory": true },
          { "name": "DESCRIPTION", "desc": "Descrizione del WBS.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "Simile a BAPI_ALM_ORDER_MAINTAIN, è una BAPI molto complessa che agisce come un controller. La tabella I_METHOD_PROJECT orchestra tutte le operazioni sugli oggetti del progetto. Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": ``
};
