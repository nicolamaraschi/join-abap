export const bapiData = {
    "FI": [
      {
        "name": "BAPI_ACC_DOCUMENT_POST",
        "description": "Questa BAPI è ampiamente utilizzata per la registrazione di diverse tipologie di documenti contabili, inclusi quelli relativi a conti Co.Ge., fornitori e clienti.",
        "details": [
          {
            "title": "Parametri di Importazione e Campi Obbligatori/Fondamentali",
            "structures": [
              { "name": "DOCUMENTHEADER", "type": "BAPIACHE09", "fields": [
                { "name": "BUS_ACT", "desc": "Transazione Contabile (es. 'RFBU').", "mandatory": true },
                { "name": "USERNAME", "desc": "Utente che esegue la registrazione.", "mandatory": true },
                { "name": "COMP_CODE", "desc": "Società.", "mandatory": true },
                { "name": "DOC_DATE", "desc": "Data del documento originale.", "mandatory": true },
                { "name": "PSTNG_DATE", "desc": "Data di registrazione.", "mandatory": true },
                { "name": "DOC_TYPE", "desc": "Classifica il documento contabile (es. 'SA').", "mandatory": true },
                { "name": "REF_DOC_NO", "desc": "Numero del documento esterno di riferimento.", "mandatory": false }
              ]},
              { "name": "ACCOUNTGL", "type": "Tabella di BAPIACGL09", "fields": [
                { "name": "ITEMNO_ACC", "desc": "Numero progressivo della riga.", "mandatory": true },
                { "name": "GL_ACCOUNT", "desc": "Conto Co.Ge. da movimentare.", "mandatory": true },
                { "name": "COSTCENTER", "desc": "Centro di Costo (se il conto lo richiede).", "mandatory": false },
                { "name": "PROFIT_CTR", "desc": "Profit Center (se il conto lo richiede).", "mandatory": false },
                { "name": "ITEMNO_TAX", "desc": "Collega la riga di costo/ricavo alla riga IVA.", "mandatory": false }
              ]},
              { "name": "ACCOUNTPAYABLE", "type": "Tabella di BAPIACAP09", "fields": [
                { "name": "ITEMNO_ACC", "desc": "Numero progressivo della riga.", "mandatory": true },
                { "name": "VENDOR_NO", "desc": "Codice del fornitore.", "mandatory": true },
                { "name": "BLINE_DATE", "desc": "Data base per calcolo scadenza.", "mandatory": false }
              ]},
              { "name": "ACCOUNTRECEIVABLE", "type": "Tabella di BAPIACAR09", "fields": [
                { "name": "ITEMNO_ACC", "desc": "Numero progressivo della riga.", "mandatory": true },
                { "name": "CUSTOMER", "desc": "Codice del cliente.", "mandatory": true },
                { "name": "BLINE_DATE", "desc": "Data base per calcolo scadenza.", "mandatory": false }
              ]},
              { "name": "ACCOUNTTAX", "type": "Tabella di BAPIACTX09", "fields": [
                { "name": "ITEMNO_ACC", "desc": "Numero progressivo della riga IVA.", "mandatory": true },
                { "name": "GL_ACCOUNT", "desc": "Conto Co.Ge. per la registrazione IVA.", "mandatory": true },
                { "name": "TAX_CODE", "desc": "Codice IVA.", "mandatory": true },
                { "name": "COND_KEY", "desc": "Chiave condizione per l'IVA.", "mandatory": false },
                { "name": "TAXJURCODE", "desc": "Codice giurisdizione fiscale (se usato).", "mandatory": false }
              ]},
              { "name": "CURRENCYAMOUNT", "type": "Tabella di BAPIACCR09", "fields": [
                { "name": "ITEMNO_ACC", "desc": "Collega l'importo alla riga del documento.", "mandatory": true },
                { "name": "CURR_TYPE", "desc": "Tipo di valuta (es. '00', '10').", "mandatory": true },
                { "name": "CURRENCY", "desc": "La valuta dell'importo.", "mandatory": true },
                { "name": "AMT_DOCCUR", "desc": "L'importo nella valuta del documento (positivo per Dare, negativo per Avere).", "mandatory": true },
                { "name": "AMT_BASE", "desc": "Base imponibile IVA (per le righe IVA).", "mandatory": false }
              ]}
            ]
          },
          { "title": "Note", "content": "È best practice utilizzare BAPI_ACC_DOCUMENT_CHECK prima della registrazione per validare i dati. È indispensabile eseguire BAPI_TRANSACTION_COMMIT per salvare permanentemente il documento. L'obbligatorietà di molti campi dipende fortemente dallo scenario contabile e dalla configurazione del sistema." }
        ]
      },
      {
        "name": "BAPI_AP_ACC_GETOPENITEMS",
        "description": "Recupera le partite aperte per un conto fornitore a una data chiave.",
        "details": [
          {
            "title": "Parametri di Importazione Obbligatori",
            "structures": [
              { "name": "Import Parameters", "type": "Singoli", "fields": [
                { "name": "VENDOR", "desc": "Numero del fornitore.", "mandatory": true },
                { "name": "COMPANYCODE", "desc": "Codice della società.", "mandatory": true },
                { "name": "KEYDATE", "desc": "Data di riferimento per la selezione.", "mandatory": true }
              ]}
            ]
          },
          { "title": "Note", "content": "Il parametro NOTEDITEMS (opzionale) può essere impostato a 'X' per includere le partite pro-memoria." }
        ]
      },
      {
        "name": "BAPI_AR_ACC_GETOPENITEMS",
        "description": "Recupera le partite aperte per un conto cliente a una data chiave.",
        "details": [
          {
            "title": "Parametri di Importazione Obbligatori",
            "structures": [
              { "name": "Import Parameters", "type": "Singoli", "fields": [
                { "name": "CUSTOMER", "desc": "Numero del cliente.", "mandatory": true },
                { "name": "COMPANYCODE", "desc": "Codice della società.", "mandatory": true },
                { "name": "KEYDATE", "desc": "Data di riferimento per la selezione.", "mandatory": true }
              ]}
            ]
          },
          { "title": "Note", "content": "Il parametro NOTEDITEMS (opzionale) può essere impostato a 'X' per includere le partite pro-memoria." }
        ]
      },
      {
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
        ]
      },
      {
        "name": "BAPI_FIXEDASSET_CREATE1",
        "description": "Crea dati anagrafici cespiti.",
        "details": [
          {
            "title": "Parametri di Importazione e Campi Obbligatori",
            "structures": [
              { "name": "KEY", "type": "BAPI1022_KEY", "fields": [
                { "name": "COMPANYCODE", "desc": "Società.", "mandatory": true }
              ]},
              { "name": "GENERALDATA", "type": "BAPI1022_FEGLG001", "fields": [
                { "name": "ASSETCLASS", "desc": "Classe di immobilizzazione.", "mandatory": true },
                { "name": "DESCRIPT", "desc": "Descrizione del cespite.", "mandatory": true },
                { "name": "INVENT_NO", "desc": "Numero di inventario.", "mandatory": false },
                { "name": "MAIN_NTEXT", "desc": "Descrizione lunga del cespite.", "mandatory": false }
              ]},
              { "name": "TIMEDEPENDENTDATA", "type": "BAPI1022_FEGLG002", "fields": [
                { "name": "COSTCENTER", "desc": "Centro di costo a cui è assegnato il cespite.", "mandatory": true }
              ]},
              { "name": "POSTINGINFORMATION", "type": "BAPI1022_FEGLG003", "fields": [
                { "name": "CAP_DATE", "desc": "Data di capitalizzazione.", "mandatory": true }
              ]}
            ]
          },
          { "title": "Note", "content": "È necessario utilizzare le strutture 'X' corrispondenti (es. GENERALDATAX, TIMEDEPENDENTDATAX) per indicare quali campi si stanno passando. Richiede BAPI_TRANSACTION_COMMIT." }
        ]
      }
    ],
    "SD": [
    {
      "name": "BAPI_SALESORDER_CREATEFROMDAT2",
      "description": "Utilizzata per la creazione di ordini di vendita, richieste di offerta e quotazioni.",
      "details": [
        {
          "title": "Parametri di Importazione e Campi Obbligatori",
          "structures": [
            { "name": "LOGIC_SWITCH", "type": "BAPISDLS", "fields": [
              { "name": "PRICING", "desc": "Controlla la determinazione prezzi (es. 'B' per nuova determinazione).", "mandatory": false },
              { "name": "ATP_WRKMOD", "desc": "Controlla la verifica ATP.", "mandatory": false }
            ]},
            { "name": "ORDER_HEADER_IN", "type": "BAPISDHD1", "fields": [
              { "name": "DOC_TYPE", "desc": "Tipo Documento di Vendita (es. 'TA').", "mandatory": true },
              { "name": "SALES_ORG", "desc": "Organizzazione Commerciale.", "mandatory": true },
              { "name": "DISTR_CHAN", "desc": "Canale di Distribuzione.", "mandatory": true },
              { "name": "DIVISION", "desc": "Divisione.", "mandatory": true },
              { "name": "PMNTTRMS", "desc": "Condizioni di Pagamento.", "mandatory": true },
              { "name": "REQ_DATE_H", "desc": "Data richiesta consegna (a livello testata).", "mandatory": false }
            ]},
            { "name": "ORDER_ITEMS_IN", "type": "Tabella di BAPISDITM", "fields": [
              { "name": "ITM_NUMBER", "desc": "Numero Posizione del documento SD.", "mandatory": true },
              { "name": "MATERIAL", "desc": "Numero Materiale.", "mandatory": true },
              { "name": "TARGET_QTY", "desc": "Quantità Obiettivo.", "mandatory": true },
              { "name": "TARGET_QU", "desc": "Unità di Misura Obiettivo.", "mandatory": true },
              { "name": "PLANT", "desc": "Divisione fornitrice.", "mandatory": true }
            ]},
            { "name": "ORDER_PARTNERS", "type": "Tabella di BAPIPARNR", "fields": [
              { "name": "PARTN_ROLE", "desc": "Funzione Partner (es. 'AG' per Committente, 'WE' per Destinatario).", "mandatory": true },
              { "name": "PARTN_NUMB", "desc": "Numero Cliente/Partner.", "mandatory": true }
            ]},
            { "name": "ORDER_SCHEDULES_IN", "type": "Tabella di BAPISCHDL", "fields": [
              { "name": "ITM_NUMBER", "desc": "Numero Posizione a cui si riferisce la schedulazione.", "mandatory": true },
              { "name": "REQ_QTY", "desc": "Quantità Richiesta per la data specificata.", "mandatory": true },
              { "name": "REQ_DATE", "desc": "Data Richiesta Consegna per la quantità specificata.", "mandatory": true }
            ]}
          ]
        },
        { "title": "Note", "content": "I campi organizzativi (SALES_ORG, DISTR_CHAN, DIVISION) sono fondamentali. Almeno un partner con funzione 'AG' (Committente) è tipicamente richiesto. È necessario eseguire BAPI_TRANSACTION_COMMIT per salvare l'ordine." }
      ]
    },
    {
      "name": "BAPI_SALESORDER_CHANGE",
      "description": "Modificare Ordine Cliente.",
      "details": [
        {
          "title": "Parametri di Importazione e Campi Obbligatori",
          "structures": [
            { "name": "Import Parameters", "type": "Singoli", "fields": [
              { "name": "SALESDOCUMENT", "desc": "Numero del documento di vendita da modificare.", "mandatory": true }
            ]},
            { "name": "ORDER_HEADER_IN", "type": "BAPISDH1", "fields": [
              { "name": "PO_DAT_S", "desc": "Nuova data dell'ordine d'acquisto del cliente.", "mandatory": false }
            ]},
            { "name": "ORDER_HEADER_INX", "type": "BAPISDH1X", "fields": [
              { "name": "UPDATEFLAG", "desc": "Flag di aggiornamento per la testata (impostare a 'U').", "mandatory": true },
              { "name": "PO_DAT_S", "desc": "Flag per indicare la modifica della data OdA cliente (impostare a 'X').", "mandatory": false }
            ]},
            { "name": "ORDER_ITEM_IN", "type": "Tabella di BAPISDITM", "fields": [
              { "name": "ITM_NUMBER", "desc": "Numero posizione da modificare.", "mandatory": true },
              { "name": "TARGET_QTY", "desc": "Nuova quantità di posizione.", "mandatory": false }
            ]},
            { "name": "ORDER_ITEM_INX", "type": "Tabella di BAPISDITMX", "fields": [
              { "name": "ITM_NUMBER", "desc": "Numero posizione da modificare.", "mandatory": true },
              { "name": "UPDATEFLAG", "desc": "Flag di aggiornamento per la posizione (impostare a 'U').", "mandatory": true },
              { "name": "TARGET_QTY", "desc": "Flag per indicare la modifica della quantità (impostare a 'X').", "mandatory": false }
            ]}
          ]
        },
        { "title": "Note", "content": "L'uso delle strutture 'X' è fondamentale per indicare quali campi si intende modificare. Richiede BAPI_TRANSACTION_COMMIT." }
      ]
    },
    {
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
        ]
    },
    {
      "name": "BAPI_BILLINGDOC_CREATEMULTIPLE",
      "description": "Creare Documenti di Fatturazione Multipli.",
      "details": [
        {
          "title": "Parametri di Importazione e Campi Obbligatori",
          "structures": [
            { "name": "BILLINGDATAIN", "type": "Tabella di BAPIVFKK", "fields": [
              { "name": "REF_DOC", "desc": "Numero del documento di riferimento da fatturare (es. ordine o consegna).", "mandatory": true },
              { "name": "REF_ITEM", "desc": "Numero posizione del documento di riferimento.", "mandatory": false }
            ]},
            { "name": "TESTRUN", "type": "Singolo", "fields": [
              { "name": "TESTRUN", "desc": "Se 'X', esegue una simulazione senza creare documenti.", "mandatory": false }
            ]}
          ]
        },
        { "title": "Note", "content": "Questa BAPI è usata per fatturare documenti esistenti (es. consegne). Per la creazione di fatture da dati esterni, usare BAPI_BILLINGDOC_CREATEFROMDATA. Richiede BAPI_TRANSACTION_COMMIT." }
      ]
    },
    {
      "name": "BAPI_OUTB_DELIVERY_CREATE_SLS",
      "description": "Utilizzata per creare consegne in uscita con riferimento a posizioni di ordini di vendita.",
      "details": [
        {
          "title": "Parametri di Importazione e Campi Obbligatori",
          "structures": [
            { "name": "SHIP_POINT", "type": "Singolo", "fields": [
              { "name": "SHIP_POINT", "desc": "Punto di spedizione. Sebbene il sistema possa determinarlo, specificarlo fornisce un controllo esplicito.", "mandatory": false }
            ]},
            { "name": "DUE_DATE", "type": "Singolo", "fields": [
              { "name": "DUE_DATE", "desc": "Data di creazione della consegna (spesso SY-DATUM, la data corrente).", "mandatory": true }
            ]},
            { "name": "SALES_ORDER_ITEMS", "type": "Tabella", "fields": [
              { "name": "REF_DOC", "desc": "Numero dell'ordine di vendita di riferimento.", "mandatory": true },
              { "name": "REF_ITEM", "desc": "Numero della posizione dell'ordine di vendita di riferimento.", "mandatory": true },
              { "name": "DLV_QTY", "desc": "Quantità da consegnare.", "mandatory": true }
            ]}
          ]
        },
        { "title": "Note", "content": "Non è progettata per la consegna di massa e richiede un COMMIT WORK." }
      ]
    },
    {
      "name": "BAPI_CUSTOMERRETURN_CREATE",
      "description": "Utilizzata per creare documenti di vendita di reso cliente.",
      "details": [
        {
          "title": "Strutture di Importazione Chiave e Campi Obbligatori",
          "structures": [
            { "name": "RETURN_HEADER_IN", "type": "Struttura", "fields": [
              { "name": "DOC_TYPE", "desc": "Dati di testata dell'ordine di reso (es. tipo ordine, area di vendita, cliente).", "mandatory": true }
            ]},
            { "name": "RETURN_HEADER_INX", "type": "Struttura", "fields": [
              { "name": "UPDATEFLAG", "desc": "Flag di aggiornamento per i dati di testata ('I' per insert, 'U' per update).", "mandatory": true }
            ]},
            { "name": "RETURN_ITEMS_IN", "type": "Tabella", "fields": [
              { "name": "MATERIAL", "desc": "Dati di posizione (materiale, quantità, etc.).", "mandatory": true }
            ]},
            { "name": "RETURN_ITEMS_INX", "type": "Tabella", "fields": [
              { "name": "UPDATEFLAG", "desc": "Flag di aggiornamento per i dati di posizione.", "mandatory": true }
            ]},
            { "name": "RETURN_PARTNERS", "type": "Tabella", "fields": [
              { "name": "PARTN_ROLE", "desc": "Informazioni sui partner del reso.", "mandatory": true }
            ]}
          ]
        },
        { "title": "Note", "content": "Confermare i campi obbligatori con la documentazione SE37. Richiede BAPI_TRANSACTION_COMMIT." }
      ]
    }
    ],
    "MM": [
      {
        "name": "BAPI_PO_CREATE1",
        "description": "Permette la creazione di ordini d'acquisto (OdA).",
        "details": [
          {
            "title": "Parametri di Importazione e Campi Obbligatori",
            "structures": [
              { "name": "POHEADER", "type": "BAPIMEPOHEADER", "fields": [
                { "name": "DOC_TYPE", "desc": "Tipo Documento.", "mandatory": true },
                { "name": "VENDOR", "desc": "Numero Fornitore.", "mandatory": true },
                { "name": "PURCH_ORG", "desc": "Organizzazione Acquisti.", "mandatory": true },
                { "name": "PUR_GROUP", "desc": "Gruppo Acquisti.", "mandatory": true },
                { "name": "COMP_CODE", "desc": "Società.", "mandatory": true },
                { "name": "CURRENCY", "desc": "Valuta.", "mandatory": false }
              ]},
              { "name": "POITEM", "type": "Tabella di BAPIMEPOITEM", "fields": [
                { "name": "PO_ITEM", "desc": "Numero Posizione OdA.", "mandatory": true },
                { "name": "MATERIAL", "desc": "Numero Materiale.", "mandatory": false },
                { "name": "PLANT", "desc": "Divisione.", "mandatory": true },
                { "name": "QUANTITY", "desc": "Quantità.", "mandatory": true },
                { "name": "NET_PRICE", "desc": "Prezzo Netto.", "mandatory": true },
                { "name": "ITEM_CAT", "desc": "Tipo Posizione.", "mandatory": false },
                { "name": "ACCTASSCAT", "desc": "Tipo Imputazione.", "mandatory": false }
              ]},
              { "name": "POACCOUNT", "type": "Tabella di BAPIMEPOACCOUNT", "fields": [
                { "name": "PO_ITEM", "desc": "Numero posizione a cui si riferisce l'imputazione.", "mandatory": true },
                { "name": "G_L_ACCT", "desc": "Conto Co.Ge.", "mandatory": true },
                { "name": "COSTCENTER", "desc": "Centro di Costo (se imputazione 'K').", "mandatory": false }
              ]}
            ]
          },
          { "title": "Note", "content": "L'uso delle strutture 'X' (es. POHEADERX, POITEMX, POACCOUNTX) è critico per indicare quali campi si stanno fornendo. Omettere un flag 'X' farà sì che il valore corrispondente venga ignorato. Richiede BAPI_TRANSACTION_COMMIT." }
        ]
      },
      {
        "name": "BAPI_GOODSMVT_CREATE",
        "description": "Interfaccia versatile per registrare tutti i tipi di movimenti merci.",
        "details": [
          {
            "title": "Parametri di Importazione e Campi Obbligatori",
            "structures": [
              { "name": "GOODSMVT_HEADER", "type": "BAPIGMHD01", "fields": [
                { "name": "PSTNG_DATE", "desc": "Data di Registrazione.", "mandatory": true },
                { "name": "DOC_DATE", "desc": "Data Documento.", "mandatory": true }
              ]},
              { "name": "GOODSMVT_CODE", "type": "Singolo", "fields": [
                { "name": "GM_CODE", "desc": "Codice transazione (es. '01' per EM da OdA, '03' per Uscita Merci).", "mandatory": true }
              ]},
              { "name": "GOODSMVT_ITEM", "type": "Tabella di BAPIGMITEMCREATE", "fields": [
                { "name": "MATERIAL", "desc": "Numero Materiale.", "mandatory": true },
                { "name": "PLANT", "desc": "Divisione.", "mandatory": true },
                { "name": "STGE_LOC", "desc": "Magazzino.", "mandatory": true },
                { "name": "MOVE_TYPE", "desc": "Tipo Movimento (es. 101, 201, 261).", "mandatory": true },
                { "name": "ENTRY_QNT", "desc": "Quantità.", "mandatory": true },
                { "name": "ENTRY_UOM", "desc": "Unità di misura.", "mandatory": true },
                { "name": "PO_NUMBER", "desc": "Numero OdA (per GM_CODE '01').", "mandatory": false },
                { "name": "PO_ITEM", "desc": "Posizione OdA (per GM_CODE '01').", "mandatory": false },
                { "name": "COSTCENTER", "desc": "Centro di Costo (per TMOV es. '201').", "mandatory": false },
                { "name": "ORDERID", "desc": "Ordine (per TMOV es. '261').", "mandatory": false }
              ]}
            ]
          },
          { "title": "Note", "content": "I campi obbligatori nella tabella GOODSMVT_ITEM dipendono criticamente dal valore di GOODSMVT_CODE. Ad esempio, PO_NUMBER è obbligatorio per GM_CODE '01'. Richiede BAPI_TRANSACTION_COMMIT." }
        ]
      },
      {
        "name": "BAPI_MATERIAL_SAVEDATA",
        "description": "Utilizzata per creare, estendere o modificare i dati dei materiali.",
        "details": [
          {
            "title": "Parametri di Importazione e Campi Obbligatori (per Creazione)",
            "structures": [
              { "name": "HEADDATA", "type": "BAPIMATHEAD", "fields": [
                { "name": "MATERIAL", "desc": "Numero materiale (se numerazione esterna).", "mandatory": false },
                { "name": "IND_SECTOR", "desc": "Settore Industriale.", "mandatory": true },
                { "name": "MATL_TYPE", "desc": "Tipo Materiale.", "mandatory": true },
                { "name": "BASIC_VIEW", "desc": "Flag ('X') per elaborare i Dati Base.", "mandatory": true }
              ]},
              { "name": "CLIENTDATA", "type": "BAPI_MARA", "fields": [
                { "name": "BASE_UOM", "desc": "Unità di Misura Base.", "mandatory": true },
                { "name": "MATL_GROUP", "desc": "Gruppo Materiali.", "mandatory": true },
                { "name": "DIVISION", "desc": "Divisione.", "mandatory": false }
              ]},
              { "name": "MATERIALDESCRIPTION", "type": "Tabella di BAPI_MAKT", "fields": [
                { "name": "LANGU", "desc": "Lingua della descrizione.", "mandatory": true },
                { "name": "MATL_DESC", "desc": "Descrizione materiale.", "mandatory": true }
              ]},
              { "name": "PLANTDATA", "type": "BAPI_MARC", "fields": [
                { "name": "PLANT", "desc": "Divisione da creare/estendere.", "mandatory": true },
                { "name": "PUR_GROUP", "desc": "Gruppo acquisti (per la vista acquisti).", "mandatory": false },
                { "name": "MRP_TYPE", "desc": "Tipo MRP (per la vista pianificazione).", "mandatory": false }
              ]}
            ]
          },
          { "title": "Note", "content": "Per ogni struttura dati (CLIENTDATA, PLANTDATA, etc.) è obbligatorio compilare la corrispondente struttura 'X' (CLIENTDATAX, PLANTDATAX, etc.) per indicare quali campi sono stati valorizzati e devono essere elaborati. Richiede BAPI_TRANSACTION_COMMIT." }
        ]
      },
      {
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
        ]
      },
      {
        "name": "BAPI_INCOMINGINVOICE_CREATE",
        "description": "Registrare Fattura Fornitore (Logistica - MIRO).",
        "details": [
          {
            "title": "Parametri di Importazione e Campi Obbligatori",
            "structures": [
              { "name": "HEADERDATA", "type": "BAPI_INCINV_CREATE_HEADER", "fields": [
                { "name": "INVOICE_IND", "desc": "Flag: 'X' per Fattura, ' ' per Nota di Credito.", "mandatory": true },
                { "name": "DOC_DATE", "desc": "Data del documento.", "mandatory": true },
                { "name": "PSTNG_DATE", "desc": "Data di registrazione.", "mandatory": true },
                { "name": "COMP_CODE", "desc": "Società.", "mandatory": true },
                { "name": "GROSS_AMOUNT", "desc": "Importo totale lordo della fattura.", "mandatory": true },
                { "name": "CURRENCY", "desc": "Valuta.", "mandatory": true }
              ]},
              { "name": "ITEMDATA", "type": "Tabella di BAPI_INCINV_CREATE_ITEM", "fields": [
                { "name": "INVOICE_DOC_ITEM", "desc": "Numero progressivo della riga fattura.", "mandatory": true },
                { "name": "PO_NUMBER", "desc": "Numero dell'ordine d'acquisto di riferimento.", "mandatory": true },
                { "name": "PO_ITEM", "desc": "Numero posizione dell'OdA.", "mandatory": true },
                { "name": "ITEM_AMOUNT", "desc": "Importo della posizione.", "mandatory": true },
                { "name": "QUANTITY", "desc": "Quantità fatturata.", "mandatory": true },
                { "name": "PO_UNIT", "desc": "Unità di misura della quantità.", "mandatory": true }
              ]}
            ]
          },
          { "title": "Note", "content": "Per la fatturazione basata su entrata merci, è necessario fornire i riferimenti al documento di movimento merci (REF_DOC, REF_DOC_YEAR, REF_DOC_IT). Richiede BAPI_TRANSACTION_COMMIT." }
        ]
      },
      {
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
        ]
      }
    ],
    "PP": [
    {
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
      ]
    },
    {
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
      ]
    },
    {
      "name": "BAPI_PRODORD_COMPLETE_TECH",
      "description": "Imposta lo stato di un ordine di produzione su 'Tecnicamente Completo' (TECO).",
      "details": [
        {
          "title": "Parametri di Importazione Chiave",
          "structures": [
            { "name": "Import", "type": "Singolo o Tabella", "fields": [
              { "name": "ORDER_NUMBER", "desc": "Numero Ordine di Produzione per chiusura singola.", "mandatory": false },
              { "name": "ORDERS", "desc": "Tabella di Chiavi Ordine (BAPI_ORDER_KEY) per chiusura multipla.", "mandatory": false }
            ]}
          ]
        },
        { "title": "Note", "content": "Indica che l'ordine è terminato dal punto di vista logistico. Richiede BAPI_TRANSACTION_COMMIT." }
      ]
    },
    {
      "name": "BAPI_GOODSMVT_CREATE",
      "description": "BAPI universale per la registrazione di movimenti merci, usata in PP per prelievi (es. TMOV 261) ed entrate merci (es. TMOV 101).",
      "details": [
        {
          "title": "Parametri di Importazione Chiave",
          "structures": [
            { "name": "GOODSMVT_HEADER", "type": "Struttura", "fields": [
              { "name": "PSTNG_DATE", "desc": "Data di Registrazione.", "mandatory": true },
              { "name": "DOC_DATE", "desc": "Data Documento.", "mandatory": true }
            ]},
            { "name": "GOODSMVT_CODE", "type": "Singolo", "fields": [
              { "name": "GM_CODE", "desc": "Codice che identifica la transazione (es. '02' per entrata merci da ordine, equiv. MB31).", "mandatory": true }
            ]},
            { "name": "GOODSMVT_ITEM", "type": "Tabella", "fields": [
              { "name": "MATERIAL", "desc": "Numero Materiale.", "mandatory": true },
              { "name": "PLANT", "desc": "Divisione.", "mandatory": true },
              { "name": "STGE_LOC", "desc": "Ubicazione.", "mandatory": true },
              { "name": "MOVE_TYPE", "desc": "Tipo Movimento (es. '101', '261').", "mandatory": true },
              { "name": "ENTRY_QNT", "desc": "Quantità.", "mandatory": true },
              { "name": "ORDERID", "desc": "Numero Ordine di Produzione di riferimento.", "mandatory": true }
            ]}
          ]
        },
        { "title": "Note", "content": "È fondamentale popolare i campi corretti in base allo scenario. La BAPI è molto potente ma richiede precisione. Richiede BAPI_TRANSACTION_COMMIT per finalizzare il movimento." }
      ]
    },
    {
      "name": "BAPI_GOODSMVT_CANCEL",
      "description": "Utilizzata per stornare/annullare un documento materiale precedentemente registrato (es. prelievo errato).",
      "details": [
        {
          "title": "Parametri di Importazione Chiave",
          "structures": [
            { "name": "Import", "type": "Singoli", "fields": [
              { "name": "MATERIALDOCUMENT", "desc": "Numero del Documento Materiale da stornare.", "mandatory": true },
              { "name": "MATDOCUMENTYEAR", "desc": "Anno del Documento Materiale.", "mandatory": true },
              { "name": "GOODSMVT_CODE", "desc": "Codice transazione per lo storno (es. MBST).", "mandatory": false }
            ]}
          ]
        },
        { "title": "Note", "content": "Crea un documento di storno con un tipo movimento inverso. Richiede BAPI_TRANSACTION_COMMIT." }
      ]
    },
    {
      "name": "BAPI_PLANNEDORDER_CREATE",
      "description": "Crea ordini pianificati, che sono precursori degli ordini di produzione o delle richieste di acquisto.",
      "details": [
        {
          "title": "Parametri di Importazione Esemplari",
          "structures": [
            { "name": "HEADERDATA", "type": "BAPI_PLNDORD_CREATE_HEADER", "fields": [
              { "name": "MATERIAL", "desc": "Materiale da pianificare.", "mandatory": true },
              { "name": "PLANT", "desc": "Divisione di pianificazione.", "mandatory": true },
              { "name": "PLANNING_PLANT", "desc": "Divisione di produzione.", "mandatory": true },
              { "name": "ORDER_TYPE", "desc": "Tipo ordine pianificato.", "mandatory": true },
              { "name": "TOTAL_QUANTITY", "desc": "Quantità da pianificare.", "mandatory": true },
              { "name": "START_DATE", "desc": "Data di inizio schedulata.", "mandatory": true },
              { "name": "END_DATE", "desc": "Data di fine schedulata.", "mandatory": true }
            ]}
          ]
        },
        { "title": "Note", "content": "Esistono anche BAPI per modificare (BAPI_PLANNEDORDER_CHANGE) e cancellare (BAPI_PLANNEDORDER_DELETE) gli ordini pianificati. Richiede BAPI_TRANSACTION_COMMIT." }
      ]
    }
  ],
    "PM": [
      {
        "name": "BAPI_ALM_ORDER_MAINTAIN",
        "description": "BAPI per creare e modificare ordini di manutenzione (PM) e ordini di servizio (CS).",
        "details": [
          {
            "title": "Parametri di Importazione e Campi Obbligatori (per Creazione)",
            "structures": [
              { "name": "IT_METHODS", "type": "Tabella di BAPI_ALM_ORDER_METHOD", "fields": [
                { "name": "REFNUMBER", "desc": "Numero di riferimento per collegare il metodo ai dati.", "mandatory": true },
                { "name": "OBJECTTYPE", "desc": "Tipo Oggetto (es. 'HEADER', 'OPERATION', 'SAVE').", "mandatory": true },
                { "name": "METHOD", "desc": "Metodo (es. 'CREATE', 'CHANGE', 'SAVE').", "mandatory": true },
                { "name": "OBJECTKEY", "desc": "Chiave oggetto (es. numero ordine a 12 cifre per modifiche).", "mandatory": true }
              ]},
              { "name": "IT_HEADER", "type": "Tabella di BAPI_ALM_ORDER_HEADER_I", "fields": [
                { "name": "REFNUMBER", "desc": "Riferimento a IT_METHODS.", "mandatory": true },
                { "name": "ORDER_TYPE", "desc": "Tipo Ordine (es. 'PM01').", "mandatory": true },
                { "name": "PLANPLANT", "desc": "Divisione di pianificazione manutenzione.", "mandatory": true },
                { "name": "EQUIPMENT", "desc": "Equipment di riferimento.", "mandatory": false },
                { "name": "FUNCT_LOC", "desc": "Ubicazione Tecnica di riferimento.", "mandatory": false },
                { "name": "SHORT_TEXT", "desc": "Descrizione breve dell'ordine.", "mandatory": true },
                { "name": "MN_WK_CTR", "desc": "Centro di Lavoro Principale.", "mandatory": true }
              ]},
              { "name": "IT_OPERATION", "type": "Tabella di BAPI_ALM_ORDER_OPERATION_I", "fields": [
                { "name": "REFNUMBER", "desc": "Riferimento a IT_METHODS.", "mandatory": true },
                { "name": "ACTIVITY", "desc": "Numero operazione (es. '0010').", "mandatory": true },
                { "name": "CONTROL_KEY", "desc": "Chiave di controllo (es. 'PM01').", "mandatory": true },
                { "name": "WORK_CNTR", "desc": "Centro di lavoro dell'operazione.", "mandatory": true },
                { "name": "DESCRIPTION", "desc": "Descrizione dell'operazione.", "mandatory": true }
              ]}
            ]
          },
          { "title": "Note", "content": "Questa è una BAPI complessa. La tabella IT_METHODS orchestra tutte le azioni. Un'azione con METHOD = 'SAVE' è richiesta per persistere i dati. È indispensabile eseguire BAPI_TRANSACTION_COMMIT dopo la chiamata." }
        ]
      },
      {
        "name": "BAPI_EQUI_CREATE",
        "description": "Creazione di nuovi equipment (oggetti tecnici).",
        "details": [
          {
            "title": "Parametri di Importazione e Campi Obbligatori",
            "structures": [
              { "name": "DATA_GENERAL", "type": "BAPI_ITOB", "fields": [
                { "name": "EQUICATGRY", "desc": "Categoria Equipment (es. 'M' per Macchine). Cruciale.", "mandatory": true },
                { "name": "DESCRIPT", "desc": "Descrizione dell'equipment.", "mandatory": true },
                { "name": "MAINTPLANT", "desc": "Divisione di manutenzione.", "mandatory": true },
                { "name": "OBJECTTYPE", "desc": "Tipo Oggetto Tecnico.", "mandatory": true }
              ]},
              { "name": "DATA_SPECIFIC", "type": "BAPI_ITOB_EQ_ONLY", "fields": [
                { "name": "MANUFACTURER", "desc": "Costruttore.", "mandatory": false },
                { "name": "MODEL_NO", "desc": "Numero Modello.", "mandatory": false },
                { "name": "CONSTYEAR", "desc": "Anno di Costruzione.", "mandatory": false },
                { "name": "START_UP_DATE", "desc": "Data di Messa in Servizio.", "mandatory": false }
              ]}
            ]
          },
          { "title": "Note", "content": "La Categoria Equipment (EQUICATGRY) è fondamentale perché determina la logica e i campi disponibili/obbligatori. Richiede BAPI_TRANSACTION_COMMIT per salvare l'equipment." }
        ]
      },
      {
        "name": "EAM_TASKLIST_CREATE",
        "description": "Creazione di liste di cicli operativi (task list) per la manutenzione.",
        "details": [
          {
            "title": "Parametri di Importazione e Campi Obbligatori (inferiti)",
            "structures": [
              { "name": "Task list header data", "type": "Struttura di testata", "fields": [
                { "name": "PLNTY", "desc": "Tipo Lista Cicli ('A' Equipment, 'T' U.T.).", "mandatory": true },
                { "name": "KTEXT", "desc": "Descrizione Lista Cicli.", "mandatory": true },
                { "name": "WERKS", "desc": "Divisione di pianificazione.", "mandatory": true },
                { "name": "VERWE", "desc": "Impiego del ciclo operativo (es. '4' Manutenzione).", "mandatory": true },
                { "name": "STATU", "desc": "Stato del ciclo (es. '4' Rilasciato).", "mandatory": true }
              ]},
              { "name": "Operations", "type": "Tabella di operazioni", "fields": [
                { "name": "VORNR", "desc": "Numero operazione (es. '0010').", "mandatory": true },
                { "name": "ARBPL", "desc": "Centro di Lavoro.", "mandatory": true },
                { "name": "STEUS", "desc": "Chiave di Controllo.", "mandatory": true },
                { "name": "LTXA1", "desc": "Descrizione dell'operazione.", "mandatory": true },
                { "name": "ARBEI", "desc": "Lavoro/Durata dell'operazione.", "mandatory": true },
                { "name": "ARBEH", "desc": "Unità di misura del lavoro (es. 'H').", "mandatory": true }
              ]}
            ]
          },
          { "title": "Note", "content": "Questo FM è usato negli scenari di migrazione dati. L'analisi diretta dei suoi parametri nel sistema SAP è raccomandata. Richiede un commit della transazione." }
        ]
      }
    ],
    "CO": [
      {
        "name": "BAPI_COSTCENTER_CREATEMULTIPLE",
        "description": "Creare Centri di Costo (multipli).",
        "details": [
          {
            "title": "Parametri di Importazione e Campi Obbligatori",
            "structures": [
              { "name": "CONTROLLINGAREA", "type": "Singolo", "fields": [
                { "name": "CONTROLLINGAREA", "desc": "Area di controlling in cui creare i centri di costo.", "mandatory": true }
              ]},
              { "name": "COSTCENTERLIST", "type": "Tabella di BAPI0012_CCLIST1", "fields": [
                { "name": "COSTCENTER", "desc": "Codice del nuovo centro di costo.", "mandatory": true },
                { "name": "VALID_FROM", "desc": "Data inizio validità.", "mandatory": true },
                { "name": "NAME", "desc": "Nome del centro di costo.", "mandatory": true },
                { "name": "DESCRIPT", "desc": "Descrizione del centro di costo.", "mandatory": true },
                { "name": "PERSON_IN_CHARGE", "desc": "Responsabile.", "mandatory": true },
                { "name": "COSTCTR_HIER_GRP", "desc": "Nodo della gerarchia standard.", "mandatory": true },
                { "name": "COMP_CODE", "desc": "Società.", "mandatory": true }
              ]}
            ]
          },
          { "title": "Note", "content": "Permette la creazione massiva di centri di costo. Richiede BAPI_TRANSACTION_COMMIT." }
        ]
      },
      {
        "name": "BAPI_INTERNALORDER_CREATE",
        "description": "Creare Ordine Interno.",
        "details": [
          {
            "title": "Parametri di Importazione e Campi Obbligatori",
            "structures": [
              { "name": "ORDER_TYPE", "type": "Singolo", "fields": [
                { "name": "ORDER_TYPE", "desc": "Tipo di ordine interno.", "mandatory": true }
              ]},
              { "name": "MASTER_DATA", "type": "BAPI2075_2", "fields": [
                { "name": "CO_AREA", "desc": "Area di controlling.", "mandatory": true },
                { "name": "OBJECT_CLASS", "desc": "Classe oggetto (es. 'OC' per Spese Generali).", "mandatory": true },
                { "name": "SHORT_TEXT", "desc": "Descrizione breve.", "mandatory": true }
              ]}
            ]
          },
          { "title": "Note", "content": "Per la modifica, usare BAPI_INTERNALORDER_CHANGE. Per ottenere i dettagli, BAPI_INTERNALORDER_GETDETAIL. Richiede BAPI_TRANSACTION_COMMIT." }
        ]
      },
      {
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
        ]
      }
    ],
    "BP": [
      {
        "name": "BAPI_BUPA_CREATE_FROM_DATA",
        "description": "BAPI centrale in S/4HANA per creare un Business Partner.",
        "details": [
          {
            "title": "Parametri di Importazione e Campi Obbligatori",
            "structures": [
              { "name": "Singoli", "type": "Parametri", "fields": [
                { "name": "PARTNERCATEGORY", "desc": "Categoria BP ('1' Persona, '2' Organizzazione, '3' Gruppo).", "mandatory": true },
                { "name": "PARTNERGROUP", "desc": "Raggruppamento (controlla numerazione e viste).", "mandatory": true }
              ]},
              { "name": "CENTRALDATAORGANIZATION", "type": "BAPIBUSISM000_CENTRAL", "fields": [
                { "name": "NAME_ORG1", "desc": "Nome dell'organizzazione (se categoria '2').", "mandatory": true },
                { "name": "SEARCHTERM1", "desc": "Termine di ricerca.", "mandatory": false }
              ]},
              { "name": "CENTRALDATAPERSON", "type": "BAPIBUSISM000_CENTRAL_PERSON", "fields": [
                { "name": "FIRSTNAME", "desc": "Nome (se categoria '1').", "mandatory": true },
                { "name": "LASTNAME", "desc": "Cognome (se categoria '1').", "mandatory": true },
                { "name": "BIRTHDATE", "desc": "Data di nascita.", "mandatory": false }
              ]},
              { "name": "ADDRESSDATA", "type": "BAPIBUSISM000_ADDRESS", "fields": [
                { "name": "COUNTRY", "desc": "Nazione.", "mandatory": true },
                { "name": "CITY", "desc": "Città.", "mandatory": true },
                { "name": "STREET", "desc": "Via e numero civico.", "mandatory": true },
                { "name": "POSTL_COD1", "desc": "Codice di Avviamento Postale.", "mandatory": true },
                { "name": "LANGU", "desc": "Lingua.", "mandatory": false }
              ]}
            ]
          },
          { "title": "Note", "content": "Questa BAPI crea i dati centrali del BP. Per aggiungere ruoli (es. cliente, fornitore) e dati dipendenti (dati società, dati vendite), sono necessarie chiamate successive ad altre BAPI (es. BAPI_BUPA_ROLE_ADD_2). La configurazione del Raggruppamento (PARTNERGROUP) è cruciale. Richiede BAPI_TRANSACTION_COMMIT." }
        ]
      }
    ],
    "PS": [
      {
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
        ]
      }
    ],
    "QM": [
      {
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
        ]
      },
      {
        "name": "BAPI_QUALNOT_CREATE",
        "description": "Crea una notifica sulla qualità (avviso QM).",
        "details": [
          {
            "title": "Parametri di Importazione e Campi Obbligatori",
            "structures": [
              { "name": "NOTIF_TYPE", "type": "Singolo", "fields": [
                { "name": "NOTIF_TYPE", "desc": "Tipo di notifica (es. 'F2' per Reclamo fornitore).", "mandatory": true }
              ]},
              { "name": "NOTIFHEADER", "type": "BAPI2078_NOTHDRI", "fields": [
                { "name": "SHORT_TEXT", "desc": "Testo breve della notifica.", "mandatory": true },
                { "name": "PRIORITY", "desc": "Priorità della notifica.", "mandatory": false }
              ]}
            ]
          },
          { "title": "Note", "content": "Dopo la creazione, si possono aggiungere posizioni, cause e attività con BAPI_QUALNOT_ADD_DATA. Richiede BAPI_TRANSACTION_COMMIT per salvare la notifica." }
        ]
      }
    ]
  }

