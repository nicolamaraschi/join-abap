// src/modules/CdsData.jsx

export const cdsData = {
    // Puoi raggruppare le viste CDS per modulo funzionale, come per le BAPI
    "VDM": [ // Virtual Data Model, una possibile categoria
      {
        "name": "I_SalesOrderItem",
        "description": "Vista CDS per i dati delle posizioni degli Ordini di Vendita.",
        "details": [
          {
            "title": "Annotazioni Chiave",
            "content": "Questa vista è ottimizzata per l'analisi e l'estrazione dati in tempo reale."
          },
          {
            "title": "Entità Principali",
            "structures": [
              { "name": "Sorgenti Dati", "fields": [
                { "name": "VBAK", "desc": "Testata documento di vendita." },
                { "name": "VBAP", "desc": "Posizione documento di vendita." },
                { "name": "VBFA", "desc": "Flusso documenti di vendita." }
              ]}
            ]
          }
        ]
      },
      {
        "name": "I_PurchaseOrderItemAPI01",
        "description": "Vista CDS per le posizioni degli Ordini d'Acquisto (API).",
        "details": [
          {
            "title": "Utilizzo",
            "content": "Ideale per l'integrazione con sistemi esterni per il monitoraggio degli acquisti."
          }
        ]
      }
    ],
    "Documentazione": [ // Una categoria per le CDS custom
       {
        "name": "conoscenza cds",
        "description": "Documentazione cds",
        "details": []
      }
    ]
  };