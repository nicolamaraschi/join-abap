export const bapi = {
  "name": "BAPI_INSPLOT_CREATE",
  "description": "Crea manualmente un lotto di controllo in Quality Management.",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori",
      "structures": [
        { "name": "HEAD_DATA", "type": "BAPI2045L_HEAD1", "fields": [
          { "name": "MATERIAL", "desc": "Materiale per cui creare il lotto di controllo.", "mandatory": true },
          { "name": "PLANT", "desc": "Divisione.", "mandatory": true },
          { "name": "INSPLOT_TYPE", "desc": "Origine lotto di controllo (es. '89' per Manuale).", "mandatory": true },
          { "name": "INSPLOT_QTY", "desc": "Quantità del lotto di controllo.", "mandatory": true },
          { "name": "INSPLOT_UN", "desc": "Unità di misura della quantità.", "mandatory": true }
        ]},
        { "name": "TESTRUN", "type": "Singolo", "fields": [
          { "name": "TESTRUN", "desc": "Se 'X', esegue una simulazione.", "mandatory": false }
        ]}
      ]
    },
    { "title": "Note", "content": "Usata per creare lotti di controllo non generati automaticamente da altri processi (es. MM, PP). Richiede BAPI_TRANSACTION_COMMIT." }
  ],
  "content": ``
};
