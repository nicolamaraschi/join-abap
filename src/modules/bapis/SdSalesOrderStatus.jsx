export const bapi = {
  "name": "BAPI_SALESORDER_GETSTATUS",
  "description": "Ottenimento Stato Ordine Cliente.",
  "details": [
    {
      "title": "Parametri di Importazione Obbligatori",
      "structures": [
        { "name": "Import Parameters", "type": "Singoli", "fields": [
          { "name": "SALESDOCUMENT", "desc": "Numero del documento di vendita di cui si vuole lo stato.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "Restituisce lo stato generale e di ogni posizione (es. stato di consegna, stato di fatturazione)." }
  ],
  "content": ``
};
