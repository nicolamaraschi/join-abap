export const fiData = `
### Modulo FI (Financial Accounting)

#### FI :
##### Master data
**SKA1 (Accounts)**
* Chiavi Primarie: \`MANDT(MANDT), KTOPL(KTOPL), SAKNR(SAKNR)\`
* Descrizione: Anagrafica Conti Co.Ge. a livello di piano dei conti.
* Possibili Join:
  * **SKB1** (G/L account master (company code)): su \`SAKNR\`
  * **SKAT** (G/L account master record (chart of accounts: description)): su \`KTOPL, SAKNR\`

**KNA1 (General Data in Customer Master)**
* Chiavi Primarie: \`MANDT(MANDT), KUNNR(KUNNR)\`
* Descrizione: Anagrafica Clienti, Dati Generali.
* Possibili Join:
  * **KNB1** (Customer master (company code)): su \`KUNNR\`
  * **KNVP** (Customer Master Partner Functions): su \`KUNNR\`
  * **KNVV** (Customer Master Sales Data): su \`KUNNR\`

**LFA1 (Vendor master (general section))**
* Chiavi Primarie: \`MANDT(MANDT), LIFNR(LIFNR)\`
* Descrizione: Anagrafica Fornitori, Dati Generali.
* Possibili Join:
  * **LFB1** (Vendor master (company code)): su \`LIFNR\`
  * **LFM1** (Vendor master record purchasing organization data): su \`LIFNR\`
  * **LFBK** (Vendor master (bank details)): su \`LIFNR\`

**BNKA (Bank master record)**
* Chiavi Primarie: \`MANDT(MANDT), BANKS(BANKS), BANKL(BANKL)\`
* Descrizione: Anagrafica delle banche.
* Possibili Join:
  * Nessun join all'interno di questa sottocategoria.

##### Accounting documents // indices
**BKPF (Accounting documents)**
* Chiavi Primarie: \`MANDT(MANDT), BUKRS(BUKRS), BELNR(BELNR_D), GJAHR(GJAHR)\`
* Descrizione: Testata dei documenti contabili.
* Possibili Join:
  * **BSEG** (item level): su \`BUKRS, BELNR, GJAHR\`

**BSEG (item level)**
* Chiavi Primarie: \`MANDT(MANDT), BUKRS(BUKRS), BELNR(BELNR_D), GJAHR(GJAHR), BUZEI(BUZEI)\`
* Descrizione: Posizioni dei documenti contabili. Nota: In S/4HANA, ACDOCA è la tabella primaria per le voci.
* Possibili Join:
  * **BKPF** (Accounting documents): su \`BUKRS, BELNR, GJAHR\`
  * **BSIS** (Secondary index for G/L accounts): su \`BUKRS, BELNR, GJAHR, BUZEI\`
  * **BSID** (Secondary index for customers): su \`BUKRS, BELNR, GJAHR, BUZEI\`
  * **BSIK** (Secondary index for vendors): su \`BUKRS, BELNR, GJAHR, BUZEI\`
  * **BSEC** (One-time account data document segment): su \`BUKRS, BELNR, GJAHR, BUZEI\`
  * **BSET** (Tax data document segment): su \`BUKRS, BELNR, GJAHR, BUZEI\`

**BSID (Accounting: Secondary index for customers)**
* Chiavi Primarie: \`MANDT(MANDT), BUKRS(BUKRS), KUNNR(KUNNR), UMSKS(UMSKS), UMSKZ(UMSKZ), AUGDT(AUGDT), AUGBL(AUGBL), ZUONR(ZUONR_I), GJAHR(GJAHR), BELNR(BELNR_D), BUZEI(BUZEI)\`
* Descrizione: Vista Indice per le partite aperte dei clienti. Essendo una vista, non ha chiavi primarie proprie ma è usata per la ricerca.

**BSIK (Accounting: Secondary index for vendors)**
* Chiavi Primarie: \`MANDT(MANDT), BUKRS(BUKRS), LIFNR(LIFNR), UMSKS(UMSKS), UMSKZ(UMSKZ), AUGDT(AUGDT), AUGBL(AUGBL), ZUONR(ZUONR_I), GJAHR(GJAHR), BELNR(BELNR_D), BUZEI(BUZEI)\`
* Descrizione: Vista Indice per le partite aperte dei fornitori. Essendo una vista, non ha chiavi primarie proprie ma è usata per la ricerca.

**BSIM (Secondary Index, Documents for Material)**
* Chiavi Primarie: \`MANDT(MANDT), MATNR(MATNR), BWKEY(BWKEY), BWTAR(BWTAR_D), BELNR(BELNR_D), GJAHR(GJAHR), BUZEI(BUZEI)\`
* Descrizione: Indice secondario dei documenti per materiale.
* Possibili Join:
  * **BKPF** (Accounting documents): su \`BELNR, GJAHR\`

**BSIP (Index for vendor validation of double documents)**
* Chiavi Primarie: \`MANDT(MANDT), BUKRS(BUKRS), LIFNR(LIFNR), WAERS(WAERS), BLDAT(BLDAT), XBLNR(XBLNR), WRBTR(WRBTR), BELNR(BELNR_D), GJAHR(GJAHR), BUZEI(BUZEI)\`
* Descrizione: Indice per la validazione di fatture fornitore duplicate.
* Possibili Join:
  * **BKPF** (Accounting documents): su \`BUKRS, BELNR_D=BELNR, GJAHR_D=GJAHR\`

**BSIS (Accounting: Secondary index for G/L accounts)**
* Chiavi Primarie: \`MANDT(MANDT), BUKRS(BUKRS), HKONT(HKONT), AUGDT(AUGDT), AUGBL(AUGBL), ZUONR(ZUONR_I), GJAHR(GJAHR), BELNR(BELNR_D), BUZEI(BUZEI)\`
* Descrizione: Vista Indice per le partite aperte dei conti Co.Ge. Essendo una vista, non ha chiavi primarie proprie ma è usata per la ricerca.

**BSAD (Accounting: Index for customers (cleared items))**
* Chiavi Primarie: \`MANDT(MANDT), BUKRS(BUKRS), KUNNR(KUNNR), UMSKS(UMSKS), UMSKZ(UMSKZ), AUGDT(AUGDT), AUGBL(AUGBL), ZUONR(ZUONR_I), GJAHR(GJAHR), BELNR(BELNR_D), BUZEI(BUZEI)\`
* Descrizione: Vista Indice per le partite pareggiate (chiuse) dei clienti. Essendo una vista, non ha chiavi primarie proprie ma è usata per la ricerca.

**BSAK (Accounting: Index for vendors (cleared items))**
* Chiavi Primarie: \`MANDT(MANDT), BUKRS(BUKRS), LIFNR(LIFNR), UMSKS(UMSKS), UMSKZ(UMSKZ), AUGDT(AUGDT), AUGBL(AUGBL), ZUONR(ZUONR_I), GJAHR(GJAHR), BELNR(BELNR_D), BUZEI(BUZEI)\`
* Descrizione: Vista Indice per le partite pareggiate (chiuse) dei fornitori. Essendo una vista, non ha chiavi primarie proprie ma è usata per la ricerca.

**BSAS (Accounting: Index for G/L accounts (cleared items))**
* Chiavi Primarie: \`MANDT(MANDT), BUKRS(BUKRS), HKONT(HKONT), AUGDT(AUGDT), AUGBL(AUGBL), ZUONR(ZUONR_I), GJAHR(GJAHR), BELNR(BELNR_D), BUZEI(BUZEI)\`
* Descrizione: Vista Indice per le partite pareggiate (chiuse) dei conti Co.Ge. Essendo una vista, non ha chiavi primarie proprie ma è usata per la ricerca.

##### Payment run
**REGUH (Settlement data from payment program)**
* Chiavi Primarie: \`MANDT(MANDT), LAUFD(LAUFD), LAUFI(LAUFI), XVORL(XVORL), ZBUKR(ZBUKR), LIFNR(LIFNR), KUNNR(KUNNR), EMPFG(EMPFG), VBLNR(VBLNR_V)\`
* Descrizione: Dati di testata della proposta di pagamento.
* Possibili Join:
  * **REGUP** (Processed items from payment program): su \`LAUFD, LAUFI, XVORL\`

**REGUP (Processed items from payment program)**
* Chiavi Primarie: \`MANDT(MANDT), LAUFD(LAUFD), LAUFI(LAUFI), XVORL(XVORL), ZBUKR(ZBUKR), LIFNR(LIFNR), KUNNR(KUNNR), BUKRS(BUKRS), BELNR(BELNR_D), GJAHR(GJAHR), BUZEI(BUZEI)\`
* Descrizione: Posizioni (documenti) elaborate dal programma di pagamento.
* Possibili Join:
  * **REGUH** (Settlement data): su \`LAUFD, LAUFI, XVORL\`
`;