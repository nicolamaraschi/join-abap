// src/modules/BadiData.jsx

export const badiData = 
    {
        "FI": [
            {
                "name": "BADI_FIN_ACDOC_EXT_VALIDATION(NON C'è)",
                "description": "Validazione moderna e flessibile delle registrazioni contabili direttamente sul Universal Journal.",
                "module": "FI",
                "transactions": ["Post General Journal Entries", "Upload G/J Entries"],
                "methods": [
                    {
                        "name": "CHECK_DATA",
                        "parameters": "IM_ACDOCA TYPE ACDOCA",
                        "description": "Metodo per implementare logiche di validazione per le registrazioni contabili prima della registrazione."
                    }
                ]
            },
            {
                "name": "BADI_FIN_ACDOC_EXT_SUBSTITUTION(NON C'è)",
                "description": "Implementa logiche di sostituzione per le registrazioni contabili direttamente sul Universal Journal, garantendo coerenza e integrità dei dati.",
                "module": "FI",
                "transactions": ["Post General Journal Entries", "Upload G/J Entries"],
                "methods": [
                    {
                        "name": "SUBSTITUTE_DATA",
                        "parameters": "CH_ACDOCA TYPE ACDOCA",
                        "description": "Metodo per implementare logiche di sostituzione sui dati del Universal Journal."
                    }
                ]
            },
            {
                "name": "BADI_FGL_POSTING_VALIDATE(NON C'è)",
                "description": "BADI potente per la validazione al momento della registrazione nella contabilità generale.",
                "module": "FI",
                "transactions": ["FB01", "Post General Journal Entries"],
                "methods": [
                    {
                        "name": "VALIDATE_POSTING",
                        "parameters": "IM_DATA TYPE FGL_S_POSTING_DATA",
                        "description": "Metodo per eseguire controlli di validazione sulla registrazione contabile."
                    }
                ]
            },
            {
                "name": "BADI_FGL_POSTING_SUBSTITUTE(NON C'è)",
                "description": "BADI potente per la sostituzione al momento della registrazione nella contabilità generale.",
                "module": "FI",
                "transactions": ["FB01", "Post General Journal Entries"],
                "methods": [
                    {
                        "name": "SUBSTITUTE_POSTING",
                        "parameters": "CH_DATA TYPE FGL_S_POSTING_DATA",
                        "description": "Metodo per sostituire i dati della registrazione contabile."
                    }
                ]
            },
            {
                "name": "FI_HEADER_SUB_1300(C'è)",
                "description": "Un BAdI classico per aggiungere un subscreen personalizzato all'intestazione delle transazioni di registrazione FI (es. FB01, FB50, FB60).",
                "module": "FI",
                "transactions": ["FB01", "FB50", "FB60"],
                "methods": [
                    {
                        "name": "PBO_1300",
                        "parameters": "",
                        "description": "Process Before Output del subscreen personalizzato."
                    },
                    {
                        "name": "PAI_1300",
                        "parameters": "",
                        "description": "Process After Input del subscreen personalizzato."
                    }
                ]
            },
            {
                "name": "BADI_FDCB_SUBSTITUTION(NON C'è)",
                "description": "Cruciale per definire logiche complesse richiamabili dall'app Fiori 'Manage Substitution/Validation Rules'.",
                "module": "FI",
                "transactions": ["Manage Substitution/Validation Rules"],
                "methods": [
                    {
                        "name": "DETERMINE_SUBSTITUTION",
                        "parameters": "IS_CONTEXT TYPE ..., ET_SUBSTITUTIONS TYPE ...",
                        "description": "Metodo per definire la logica di sostituzione."
                    }
                ]
            },
            {
                "name": "UKM_R3_ACTIVATE(C'è)",
                "description": "BAdI critico per attivare l'integrazione tra FI-AR e SAP Credit Management (FSCM).",
                "module": "FI",
                "transactions": ["VA01 (creazione ordine di vendita)", "FSCM Integration"],
                "methods": [
                    {
                        "name": "ACTIVATE_CREDIT_CHECK",
                        "parameters": "IM_SALES_ORDER TYPE ...",
                        "description": "Metodo per attivare i controlli di credito in base alla logica personalizzata."
                    }
                ]
            },
            {
                "name": "UKM_CREDIT_INFO(C'è)",
                "description": "Implementazione di logiche personalizzate per il calcolo dello score e del limite di credito in FSCM.",
                "module": "FI",
                "transactions": ["Manage Business Partner Master Data", "Manage Credit Accounts"],
                "methods": [
                    {
                        "name": "GET_CREDIT_SCORE",
                        "parameters": "IS_BUSINESS_PARTNER TYPE ...",
                        "description": "Metodo per calcolare lo score di credito."
                    },
                    {
                        "name": "GET_CREDIT_LIMIT",
                        "parameters": "IS_BUSINESS_PARTNER TYPE ...",
                        "description": "Metodo per determinare il limite di credito."
                    }
                ]
            },
            {
                "name": "FDM_AR_DEF_CASE_TYPE(C'è)",
                "description": "Definizione di valori di default per la creazione di Dispute Case dalla contabilità clienti.",
                "module": "FI",
                "transactions": ["Create Dispute Case"],
                "methods": [
                    {
                        "name": "GET_DEFAULT_CASE_TYPE",
                        "parameters": "EX_CASE_TYPE TYPE ...",
                        "description": "Metodo per definire il tipo di caso di default."
                    }
                ]
            },
            {
                "name": "FDM_AR_UI_FIELDSTATUS(NON C'è)",
                "description": "Controlla le proprietà dei campi nella schermata di creazione di Dispute Case.",
                "module": "FI",
                "transactions": ["Create Dispute Case"],
                "methods": [
                    {
                        "name": "SET_FIELD_STATUS",
                        "parameters": "CT_FIELD_STATUS TYPE ...",
                        "description": "Metodo per impostare lo stato dei campi (es. nascosto, obbligatorio)."
                    }
                ]
            },
            {
                "name": "BADI_EBPP_CCP_DATA(C'è)",
                "description": "Utilizzato nelle soluzioni di pagamento per i clienti per gestire campi e metodi di pagamento personalizzati.",
                "module": "FI",
                "transactions": ["Customer Payments (Biller Direct)"],
                "methods": [
                    {
                        "name": "GET_CUSTOM_FIELDS",
                        "parameters": "ET_FIELDS TYPE ...",
                        "description": "Metodo per recuperare campi personalizzati per i pagamenti."
                    }
                ]
            },
            {
                "name": "FCC_NOTIFICATION_CUSTOMIZE(NON C'è)",
                "description": "Personalizzazione delle notifiche email inviate dal Financial Closing Cockpit.",
                "module": "FI",
                "transactions": ["Financial Closing Cockpit (FCLOCO)"],
                "methods": [
                    {
                        "name": "CUSTOMIZE_EMAIL_NOTIFICATION",
                        "parameters": "IM_ACTIVITY TYPE ..., CH_RECIPIENTS TYPE ...",
                        "description": "Metodo per modificare destinatari o corpo dell'email."
                    }
                ]
            },
            {
                "name": "FCC_USER_GROUP_CUSTOM(NON C'è)",
                "description": "Consente la creazione di logiche personalizzate per raggruppare gli utenti per l'assegnazione delle attività nel Financial Closing Cockpit.",
                "module": "FI",
                "transactions": ["Financial Closing Cockpit (FCLOCO)"],
                "methods": [
                    {
                        "name": "GET_CUSTOM_USER_GROUP",
                        "parameters": "IM_ACTIVITY TYPE ..., ET_USERS TYPE ...",
                        "description": "Metodo per definire gruppi di utenti personalizzati."
                    }
                ]
            },
            {
                "name": "FCC_REFERENCE_DATE_CUSTOM(NON C'è)",
                "description": "Utilizzato per definire logiche personalizzate per il calcolo della data di riferimento per le attività nella pianificazione della chiusura.",
                "module": "FI",
                "transactions": ["Financial Closing Cockpit (FCLOCO)"],
                "methods": [
                    {
                        "name": "CALCULATE_REFERENCE_DATE",
                        "parameters": "IM_ACTIVITY TYPE ..., EX_REFERENCE_DATE TYPE DATE",
                        "description": "Metodo per calcolare la data di riferimento."
                    }
                ]
            },
            {
                "name": "AC_DOCUMENT( C'è)",
                "description": "BAdI moderno che può essere utilizzato all'interno del framework delle Sostituzioni (GGB1) per implementare logiche complesse in un ambiente object-oriented.",
                "module": "FI",
                "transactions": ["Tutte le registrazioni FI"],
                "methods": []
            },
            {
                "name": "INVOICE_UPDATE(C'è)",
                "description": "BAdI che permette di influenzare i dati del documento contabile generato dalla registrazione di una fattura logistica (MIRO).",
                "module": "FI",
                "transactions": ["MIRO"],
                "methods": []
            }
        ],
        "CO": [
            {
                "name": "WORKORDER_GOODSMVT(C'è)",
                "description": "Manipolazione e validazione dei movimenti merci (entrata merci, backflush) durante la conferma di produzione.",
                "module": "CO",
                "transactions": ["CO11N", "Confirm Production Order Operation"],
                "methods": [
                    {
                        "name": "GOODS_RECEIPT",
                        "parameters": "IM_GOODS_RECEIPT TYPE ...",
                        "description": "Metodo per manipolare i dati di entrata merci del prodotto finito."
                    },
                    {
                        "name": "BACKFLUSH",
                        "parameters": "IM_BACKFLUSH_DATA TYPE ...",
                        "description": "Metodo per manipolare i dati di backflushing dei componenti."
                    },
                    {
                        "name": "CHECK_GOODS_MOVEMENT",
                        "parameters": "IM_DATA TYPE ...",
                        "description": "Metodo generico per validare i movimenti merci."
                    }
                ]
            },
            {
                "name": "WORKORDER_CONFIRM_CUST_SUBSCR(C'è)",
                "description": "Aggiunge un subscreen personalizzato alle transazioni di conferma a schermata unica (CO11N/COR6N).",
                "module": "CO",
                "transactions": ["CO11N", "COR6N"],
                "methods": [
                    {
                        "name": "PBO_SUBSCREEN",
                        "parameters": "",
                        "description": "Process Before Output del subscreen personalizzato."
                    },
                    {
                        "name": "PAI_SUBSCREEN",
                        "parameters": "",
                        "description": "Process After Input del subscreen personalizzato."
                    }
                ]
            },
            {
                "name": "PPH_SUPPLY_DEMAND_LIST(C'è)",
                "description": "BAdI AMDP per modificare, aggiungere o escludere elementi di domanda/offerta durante l'esecuzione di MRP Live.",
                "module": "CO",
                "transactions": ["MRP Live", "MD04 (Stock/Requirements List)"],
                "methods": [
                    {
                        "name": "MODIFY_SUPPLY_DEMAND",
                        "parameters": "CT_SUPPLY_DEMAND TYPE ...",
                        "description": "Metodo eseguito sul database HANA per modificare gli elementi MRP."
                    }
                ]
            },
            {
                "name": "COPA_MARKET_SEGMENT_MAINT(NON C'è)",
                "description": "Estensione della logica di derivazione e validazione per le caratteristiche del segmento di profittabilità (CO-PA).",
                "module": "CO",
                "transactions": ["Processi che generano registrazioni CO-PA"],
                "methods": [
                    {
                        "name": "DERIVE_MARKET_SEGMENT",
                        "parameters": "IM_DATA TYPE ...",
                        "description": "Metodo per derivare i segmenti di mercato."
                    },
                    {
                        "name": "VALIDATE_MARKET_SEGMENT",
                        "parameters": "IM_DATA TYPE ...",
                        "description": "Metodo per validare i segmenti di mercato."
                    }
                ]
            },
            {
                "name": "UJ_CUSTOM_LOGIC(NON C'è)",
                "description": "Esecuzione di logiche di calcolo complesse in ABAP per SAP BPC, con prestazioni superiori allo script.",
                "module": "CO",
                "transactions": ["SAP Business Planning and Consolidation (BPC)"],
                "methods": [
                    {
                        "name": "EXECUTE",
                        "parameters": "IM_DATA TYPE ...",
                        "description": "Metodo principale per eseguire la logica personalizzata."
                    }
                ]
            },
            {
                "name": "UJR_WRITE_BACK(NON C'è)",
                "description": "Attivato quando i dati vengono salvati nel database in SAP BPC.",
                "module": "CO",
                "transactions": ["SAP Business Planning and Consolidation (BPC)"],
                "methods": [
                    {
                        "name": "WRITE_BACK_DATA",
                        "parameters": "IM_DATA TYPE ...",
                        "description": "Metodo per intercettare e manipolare i dati prima del write-back."
                    }
                ]
            }
        ],
        "SD": [
            {
                "name": "SD_SLS_MODIFY_HEAD(C'è)",
                "description": "BAdI di nuova generazione per modificare i dati di testata dei documenti di vendita.",
                "module": "SD",
                "transactions": ["VA01", "VA02", "Manage Sales Orders"],
                "methods": [
                    {
                        "name": "MODIFY_SALES_DOCUMENT_HEADER",
                        "parameters": "CH_SALES_DOCUMENT_HEADER TYPE ...",
                        "description": "Metodo per alterare programmaticamente i dati dell'intestazione dell'ordine di vendita."
                    }
                ]
            },
            {
                "name": "SD_SLS_MODIFY_ITEM(C'è)",
                "description": "Modifica programmatica dei dati di posizione di un documento di vendita prima del salvataggio.",
                "module": "SD",
                "transactions": ["VA01", "VA02", "Manage Sales Orders"],
                "methods": [
                    {
                        "name": "MODIFY_SALES_DOCUMENT_ITEM",
                        "parameters": "CH_SALES_DOCUMENT_ITEM TYPE ...",
                        "description": "Metodo per alterare programmaticamente i dati della posizione dell'ordine di vendita."
                    }
                ]
            },
            {
                "name": "BADI_SD_SALES_BASIC(C'è)",
                "description": "BAdI classico e ampiamente utilizzato per controlli fondamentali o per definire dati di base nell'ordine di vendita.",
                "module": "SD",
                "transactions": ["VA01", "VA02"],
                "methods": [
                    {
                        "name": "CHECK_SALES_DOCUMENT",
                        "parameters": "IM_VBAK TYPE VBAK",
                        "description": "Metodo per eseguire controlli di base sull'ordine di vendita."
                    }
                ]
            },
            {
                "name": "BADI_SD_APM_SET_APPROVAL_REASON(NON C'è)",
                "description": "Cruciale per il workflow flessibile in S/4HANA, consente di impostare una logica personalizzata per l'approvazione del documento di vendita.",
                "module": "SD",
                "transactions": ["VA01", "VA02", "Manage Sales Orders"],
                "methods": [
                    {
                        "name": "SET_APPROVAL_REASON",
                        "parameters": "IM_SALES_DOCUMENT TYPE ..., EX_APPROVAL_REASON TYPE ...",
                        "description": "Metodo per determinare se il documento richiede approvazione."
                    }
                ]
            },
            {
                "name": "BADI_SLS_HEAD_SCR_CUS(C'è)",
                "description": "Equivalente moderno per aggiungere tab/schermate personalizzate a livello di testata del documento di vendita.",
                "module": "SD",
                "transactions": ["VA01", "VA02", "VA03"],
                "methods": [
                    {
                        "name": "PBO_CUSTOM_SCREEN",
                        "parameters": "",
                        "description": "Process Before Output per la schermata personalizzata in testata."
                    }
                ]
            },
            {
                "name": "BADI_SLS_ITEM_SCR_CUS(C'è)",
                "description": "Equivalente moderno per aggiungere tab/schermate personalizzate a livello di posizione del documento di vendita.",
                "module": "SD",
                "transactions": ["VA01", "VA02", "VA03"],
                "methods": [
                    {
                        "name": "PBO_CUSTOM_SCREEN",
                        "parameters": "",
                        "description": "Process Before Output per la schermata personalizzata in posizione."
                    }
                ]
            },
            {
                "name": "BADI_SD_BILLING(NON C'è)",
                "description": "BAdI chiave per influenzare il processo di fatturazione, consentendo modifiche ai dati del documento di fatturazione prima che venga creato.",
                "module": "SD",
                "transactions": ["VF01", "VF02", "Create Billing Documents"],
                "methods": [
                    {
                        "name": "MODIFY_BILLING_DOCUMENT",
                        "parameters": "CH_VBRK TYPE VBRK, CH_VBRP TYPE VBRP_T",
                        "description": "Metodo per modificare i dati del documento di fatturazione."
                    }
                ]
            },
            {
                "name": "BADI_SD_OUTPUT_EMAIL_SETTINGS(NON C'è)",
                "description": "BAdI di Gestione Output per determinare dinamicamente i destinatari delle email, modificare il corpo delle email o cambiare i nomi degli allegati.",
                "module": "SD",
                "transactions": ["Output Management (BRF+)"],
                "methods": [
                    {
                        "name": "DETERMINE_RECIPIENTS",
                        "parameters": "IM_CONTEXT TYPE ..., ET_RECIPIENTS TYPE ...",
                        "description": "Metodo per determinare i destinatari delle email."
                    }
                ]
            },
            {
                "name": "BADI_SD_SALES_ITEM(C'è)",
                "description": "BAdI a uso multiplo che viene eseguito durante l'elaborazione della posizione. Utile per implementare logiche di validazione aggiuntive o modifiche ai dati di posizione.",
                "module": "SD",
                "transactions": ["VA01", "VA02"],
                "methods": []
            },
            {
                "name": "LE_SHP_DELIVERY_PROC(C'è)",
                "description": "BAdI centrale per il processo di consegna. I suoi metodi permettono di intervenire in vari punti del processo (creazione, modifica, salvataggio).",
                "module": "SD",
                "transactions": ["VL01N", "VL02N"],
                "methods": []
            }
        ],
        "MM": [
            {
                "name": "MB_MIGO_BADI(C'è)",
                "description": "BAdI primario per MIGO, versatile con metodi per aggiungere tab/subscreen personalizzati, controllare i dati e eseguire logiche durante la registrazione.",
                "module": "MM",
                "transactions": ["MIGO"],
                "methods": [
                    {
                        "name": "PBO_DETAIL",
                        "parameters": "IM_DETAIL_DATA TYPE ...",
                        "description": "Process Before Output per i tab/subscreen personalizzati."
                    },
                    {
                        "name": "PAI_DETAIL",
                        "parameters": "IM_DETAIL_DATA TYPE ...",
                        "description": "Process After Input per i tab/subscreen personalizzati."
                    },
                    {
                        "name": "CHECK_ITEM",
                        "parameters": "IM_ITEM_DATA TYPE ...",
                        "description": "Controlla i dati di posizione."
                    },
                    {
                        "name": "CHECK_HEADER",
                        "parameters": "IM_HEADER_DATA TYPE ...",
                        "description": "Controlla i dati di testata."
                    },
                    {
                        "name": "POST_DOCUMENT",
                        "parameters": "IM_DOCUMENT_DATA TYPE ...",
                        "description": "Esegue logiche durante la registrazione del documento materiale."
                    }
                ]
            },
            {
                "name": "MB_DOCUMENT_BADI(C'è)",
                "description": "Opera a un livello più fondamentale, chiamato ogni volta che viene creato un documento materiale, indipendentemente dalla transazione.",
                "module": "MM",
                "transactions": ["MIGO", "BAPI", "Tutti i processi di movimento merci"],
                "methods": [
                    {
                        "name": "MB_DOCUMENT_BEFORE_UPDATE",
                        "parameters": "IM_MKPF TYPE MKPF, IT_MSEG TYPE MSEG_T",
                        "description": "Chiamato prima del commit del database, ideale per validazioni finali."
                    },
                    {
                        "name": "MB_DOCUMENT_UPDATE",
                        "parameters": "IM_MKPF TYPE MKPF, IT_MSEG TYPE MSEG_T",
                        "description": "Chiamato durante l'update task, ideale per l'aggiornamento di tabelle Z."
                    }
                ]
            },
            {
                "name": "MB_CHECK_LINE_BADI(C'è)",
                "description": "BAdI specifico per eseguire controlli su una singola riga di posizione in MIGO.",
                "module": "MM",
                "transactions": ["MIGO"],
                "methods": [
                    {
                        "name": "CHECK_ITEM_DATA",
                        "parameters": "IM_ITEM TYPE ...",
                        "description": "Controlli su una singola riga di posizione."
                    }
                ]
            },
            {
                "name": "MM_PUR_S4_PR_CHECK(C'è)",
                "description": "BAdI chiave di S/4HANA Cloud (disponibile anche on-premise) per eseguire controlli sulle posizioni delle richieste d'acquisto.",
                "module": "MM",
                "transactions": ["Manage Purchase Requisitions"],
                "methods": [
                    {
                        "name": "CHECK_PURCHASE_REQUISITION_ITEM",
                        "parameters": "IM_ITEM TYPE ...",
                        "description": "Metodo per validare le posizioni delle richieste d'acquisto."
                    }
                ]
            },
            {
                "name": "MM_PUR_S4_PR_FLDCNTRL_SIMPLE(C'è)",
                "description": "BAdI moderno per controllare le proprietà dei campi (es. nascosto, solo visualizzazione) nelle schermate della richiesta d'acquisto.",
                "module": "MM",
                "transactions": ["Manage Purchase Requisitions"],
                "methods": [
                    {
                        "name": "SET_FIELD_PROPERTIES",
                        "parameters": "IM_FIELD_NAME TYPE ..., CH_FIELD_PROPERTY TYPE ...",
                        "description": "Metodo per impostare le proprietà dei campi."
                    }
                ]
            },
            {
                "name": "ME_PROCESS_PO_CUST(C'è)",
                "description": "BAdI classico molto potente e ampiamente utilizzato per gli ordini d'acquisto (ME21N, ME22N), permette la modifica e la validazione di quasi ogni dato.",
                "module": "MM",
                "transactions": ["ME21N", "ME22N", "Manage Purchase Orders"],
                "methods": [
                    {
                        "name": "PROCESS_HEADER",
                        "parameters": "IM_HEADER TYPE REF TO IF_PURCHASE_ORDER_MM",
                        "description": "Processa i dati dell'intestazione dell'ordine d'acquisto."
                    },
                    {
                        "name": "PROCESS_ITEM",
                        "parameters": "IM_ITEM TYPE REF TO IF_PURCHASE_ORDER_ITEM_MM",
                        "description": "Processa i dati della posizione dell'ordine d'acquisto."
                    },
                    {
                        "name": "PROCESS_SCHEDULE",
                        "parameters": "IM_SCHEDULE TYPE REF TO IF_PURCHASE_ORDER_SCHEDULE_MM",
                        "description": "Processa i dati di schedulazione dell'ordine d'acquisto."
                    },
                    {
                        "name": "CHECK",
                        "parameters": "",
                        "description": "Esegue controlli complessivi sull'ordine d'acquisto."
                    },
                    {
                        "name": "POST",
                        "parameters": "",
                        "description": "Esegue logica dopo il salvataggio dell'ordine d'acquisto."
                    }
                ]
            },
            {
                "name": "ME_GUI_PO_CUST(C'è)",
                "description": "BAdI classico per aggiungere tab/subscreen personalizzati alla transazione dell'ordine d'acquisto.",
                "module": "MM",
                "transactions": ["ME21N", "ME22N"],
                "methods": [
                    {
                        "name": "PBO_CUSTOM_TAB",
                        "parameters": "",
                        "description": "Process Before Output per il tab personalizzato."
                    },
                    {
                        "name": "PAI_CUSTOM_TAB",
                        "parameters": "",
                        "description": "Process After Input per il tab personalizzato."
                    }
                ]
            },
            {
                "name": "MRM_HEADER_CHECK(C'è)",
                "description": "BAdI per eseguire controlli sui dati di testata di una fattura logistica (MIRO).",
                "module": "MM",
                "transactions": ["MIRO"],
                "methods": [
                    {
                        "name": "CHECK_HEADER_DATA",
                        "parameters": "IM_RBKPF TYPE RBKPF",
                        "description": "Controlla i dati dell'intestazione della fattura."
                    }
                ]
            },
            {
                "name": "MRM_ITEM_CHECK(NON C'è)",
                "description": "BAdI per eseguire controlli sui dati di posizione di una fattura logistica (MIRO).",
                "module": "MM",
                "transactions": ["MIRO"],
                "methods": [
                    {
                        "name": "CHECK_ITEM_DATA",
                        "parameters": "IM_RSEG TYPE RSEG",
                        "description": "Controlla i dati di posizione della fattura."
                    }
                ]
            },
            {
                "name": "BADI_MATERIAL_CHECK(C'è)",
                "description": "BAdI a uso multiplo per eseguire validazioni avanzate sui dati del materiale al momento del salvataggio.",
                "module": "MM",
                "transactions": ["MM01", "MM02"],
                "methods": []
            },
            {
                "name": "ME_PROCESS_REQ_CUST(C'è)",
                "description": "Il BAdI moderno per la gestione della logica di business nelle richieste d'acquisto 'Enjoy'.",
                "module": "MM",
                "transactions": ["ME51N", "ME52N"],
                "methods": []
            }
        ],
        "HR": [
            {
                "name": "HRPAD00INFTY(C'è)",
                "description": "BADI generica per infotipi HR, utilizzata per estensioni sui dati degli infotipi.",
                "module": "HR",
                "transactions": ["PA30", "PA40"],
                "methods": [
                    {
                        "name": "AFTER_INPUT",
                        "parameters": "P_INFOTY_TAB TYPE STANDARD TABLE, P_NEW_RECORD TYPE PRELP",
                        "description": "Metodo richiamato dopo l'input di un infotipo, per validazioni o modifiche."
                    },
                    {
                        "name": "BEFORE_OUTPUT",
                        "parameters": "P_INFOTY_TAB TYPE STANDARD TABLE, P_OLD_RECORD TYPE PRELP",
                        "description": "Metodo richiamato prima dell'output di un infotipo, per visualizzazione."
                    }
                ]
            }
        ],
        "PP": [
            {
                "name": "WORKORDER_UPDATE(C'è)",
                "description": "Punto di estensione centrale per la gestione degli ordini di produzione/processo, invocata in ogni fase critica del ciclo di vita dell'ordine.",
                "module": "PP",
                "transactions": ["CO01", "CO02", "COR1", "COR2"],
                "methods": [
                    {
                        "name": "INITIALIZE",
                        "parameters": "",
                        "description": "Eseguito all'inizio della creazione o dell'importazione di un ordine, per pre-popolare campi custom o valori di default."
                    },
                    {
                        "name": "AT_RELEASE",
                        "parameters": "IS_HEADER_DIALOG TYPE CO_S_HDR_DIALOG, I_AKTYP TYPE CO_AKTYP",
                        "description": "Eseguito quando l'utente tenta di rilasciare l'ordine; ideale per validazioni critiche che devono impedire il rilascio."
                    },
                    {
                        "name": "AT_SAVE",
                        "parameters": "",
                        "description": "Eseguito durante il salvataggio in modalità dialogo, prima che la transazione venga passata al processo di aggiornamento. Adatto per validazioni finali."
                    },
                    {
                        "name": "BEFORE_UPDATE",
                        "parameters": "",
                        "description": "Eseguito immediatamente prima della chiamata al modulo di aggiornamento (update task). Ultima possibilità di modificare i dati in modo sincrono."
                    },
                    {
                        "name": "IN_UPDATE",
                        "parameters": "",
                        "description": "Eseguito all'interno del processo di aggiornamento (update task). Asincrono. Scopo unico: aggiornare tabelle custom (Z-tables) in modo transazionalmente sicuro."
                    }
                ]
            },
            {
                "name": "WORKORDER_CONFIRM(C'è)",
                "description": "BAdI per la gestione e la validazione della logica di business legata all'oggetto 'conferma' (dati di testata, quantità, tempi).",
                "module": "PP",
                "transactions": ["CO11N", "COR6N", "CO15"],
                "methods": [
                    {
                        "name": "AT_SAVE",
                        "parameters": "IS_CONFIRMATION TYPE CAUFVD",
                        "description": "Metodo più importante, eseguito prima del salvataggio della conferma. Ideale per controlli di validazione finali (es. data di registrazione, coerenza quantità). Richiede RAISE ERROR_WITH_MESSAGE per bloccare."
                    },
                    {
                        "name": "AT_CANCEL_CHECK",
                        "parameters": "",
                        "description": "Invocato durante lo storno di una conferma (es. CO13). Permette di impedire storni se sono già state eseguite azioni successive."
                    }
                ]
            },
            {
                "name": "WORKORDER_GOODSMVT(C'è)",
                "description": "BAdI focalizzata esclusivamente sulla manipolazione e validazione dei movimenti merci associati alla conferma di produzione.",
                "module": "PP",
                "transactions": ["CO11N", "COR6N", "MIGO"],
                "methods": [
                    {
                        "name": "GM_SCREEN_LINE_CHECK",
                        "parameters": "",
                        "description": "Si attiva dinamicamente ogni volta che un utente modifica manualmente una riga nella griglia dei movimenti merci della CO11N. Ideale per validazione lotti/componenti."
                    },
                    {
                        "name": "BACKFLUSH",
                        "parameters": "",
                        "description": "Permette di intervenire sulla logica di consumo automatico dei componenti (backflush), ad esempio escludendo scarti."
                    },
                    {
                        "name": "GOODS_RECEIPT",
                        "parameters": "",
                        "description": "Consente di manipolare i dati relativi all'entrata merci automatica del prodotto finito (es. assegnare lotto, magazzino custom)."
                    }
                ]
            },
            {
                "name": "PPH_SUPPLY_DEMAND_LIST(C'è)",
                "description": "BAdI AMDP strategica e unificata per MRP Live. La sua logica è in SQLScript ed è eseguita su HANA, influenzando la pianificazione dei fabbisogni.",
                "module": "PP",
                "transactions": ["MD01N (MRP Live)", "MD04 (Stock/Requirements List)"],
                "methods": [
                    {
                        "name": "MODIFY_SUPPLY_DEMAND_LIST",
                        "parameters": "CT_SUPPLYDEMANDITEMLIST TYPE HDB_MRP_ITEM_LIST",
                        "description": "Metodo eseguito in SQLScript per modificare, eliminare o aggiungere elementi di domanda/offerta MRP in memoria."
                    }
                ]
            },
            {
                "name": "MD_PLDORD_CHANGE",
                "description": "BAdI che permette di modificare gli ordini pianificati appena creati dall'MRP.",
                "module": "PP",
                "transactions": ["MD01", "MD02", "MD03"],
                "methods": []
            },
            {
                "name": "MD_PURREQ_CHANGE",
                "description": "Analogo al precedente, ma per modificare le richieste d'acquisto generate dall'MRP.",
                "module": "PP",
                "transactions": ["MD01", "MD02", "MD03"],
                "methods": []
            },
            {
                "name": "MD_MODIFY_SOURCE",
                "description": "BAdI cruciale per influenzare la determinazione della fonte di approvvigionamento (source determination).",
                "module": "PP",
                "transactions": ["MD01", "MD02", "MD03"],
                "methods": []
            },
            {
                "name": "MD_ADD_ELEMENTS",
                "description": "Permette di aggiungere elementi di pianificazione fittizi alla lista stock/fabbisogni (MD04).",
                "module": "PP",
                "transactions": ["MD04"],
                "methods": []
            }
        ]
    };
    