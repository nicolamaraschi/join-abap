export const bapi = {
  "name": "BAPI_RESERVATION_CREATE1",
  "description": "Utilizzata per la creazione di prenotazioni di materiali.",
  "details": [
    {
      "title": "Parametri di Importazione e Campi Obbligatori (inferiti)",
      "structures": [
        { "name": "RESERVATIONHEADER", "type": "BAPI2093_RES_HEAD", "fields": [
          { "name": "REQUIREMENT_DATE", "desc": "Data Fabbisogno.", "mandatory": true },
          { "name": "MOVEMENT_TYPE", "desc": "Tipo Movimento (es. '201').", "mandatory": true },
          { "name": "GOODS_RECIPIENT", "desc": "Richiedente Merce.", "mandatory": true },
          { "name": "COSTCENTER", "desc": "Centro di Costo (se TMOV lo richiede).", "mandatory": false },
          { "name": "ORDERID", "desc": "Ordine (se TMOV lo richiede).", "mandatory": false }
        ]},
        { "name": "RESERVATIONITEMS", "type": "Tabella di BAPI2093_RES_ITEM", "fields": [
          { "name": "MATERIAL", "desc": "Numero Materiale.", "mandatory": true },
          { "name": "PLANT", "desc": "Divisione.", "mandatory": true },
          { "name": "STGE_LOC", "desc": "Magazzino di prelievo.", "mandatory": true },
          { "name": "QUANTITY", "desc": "Quantità richiesta.", "mandatory": true },
          { "name": "ENTRY_UOM", "desc": "Unità di misura.", "mandatory": true }
        ]}
      ]
    },
    { "title": "Note", "content": "I campi di imputazione (es. COSTCENTER, ORDERID) sono condizionali e dipendono dal tipo di movimento specificato. È indispensabile una chiamata a BAPI_TRANSACTION_COMMIT per salvare la prenotazione." }
  ],
  "content": ``
};
