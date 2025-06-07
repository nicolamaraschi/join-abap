export const fiData = `
### Modulo FI (Financial Accounting)

#### FI :
##### Master data
**SKA1 (Accounts)**
* Chiavi Primarie: \`MANDT, KTOPL, SAKNR\`
* Descrizione: Anagrafica Conti Co.Ge. a livello di piano dei conti.
* Possibili Join:
  * **SKB1** (G/L account master (company code)): su \`MANDT, KTOPL, SAKNR\`
  * **SKAT** (G/L account master record (texts)): su \`MANDT, KTOPL, SAKNR, SPRAS\`
  * **T004** (Chart of accounts): su \`MANDT, KTOPL\`
  * **BSEG** (Accounting Document Segment): su \`MANDT, SAKNR=HKONT\`

**BNKA (Bank master record)**
* Chiavi Primarie: \`MANDT, BANKS, BANKL\`
* Descrizione: Anagrafica delle banche.
* Possibili Join:
  * **LFBK** (Vendor bank details): su \`MANDT, BANKS, BANKL\`
  * **KNBK** (Customer bank details): su \`MANDT, BANKS, BANKL\`

##### Accounting documents // indices
**BKPF (Accounting documents)**
* Chiavi Primarie: \`MANDT, BUKRS, BELNR, GJAHR\`
* Descrizione: Testata dei documenti contabili.
* Possibili Join:
  * **BSEG** (Item level): su \`MANDT, BUKRS, BELNR, GJAHR\`
  * **ACDOCA** (Universal Journal): su \`MANDT=RCLNT, BUKRS=RBUKRS, BELNR=BELNR, GJAHR=GJAHR\`
  * **T001** (Company Codes): su \`MANDT, BUKRS\`
  * **T003** (Document Types): su \`MANDT, BLART\`

**BSEG (item level)**
* Chiavi Primarie: \`MANDT, BUKRS, BELNR, GJAHR, BUZEI\`
* Descrizione: Posizioni dei documenti contabili. Nota: In S/4HANA, ACDOCA è la tabella primaria per le voci.
* Possibili Join:
  * **BKPF** (Accounting documents): su \`MANDT, BUKRS, BELNR, GJAHR\`
  * **BSIS** (Secondary index for G/L accounts): su \`MANDT, BUKRS, BELNR, GJAHR, BUZEI\`
  * **BSID** (Secondary index for customers): su \`MANDT, BUKRS, BELNR, GJAHR, BUZEI\`
  * **BSIK** (Secondary index for vendors): su \`MANDT, BUKRS, BELNR, GJAHR, BUZEI\`
  * **SKA1** (Accounts): su \`MANDT, HKONT=SAKNR\`

**BSID (Accounting: Secondary index for customers)**
* Chiavi Primarie: \`MANDT, BUKRS, KUNNR, AUGDT, AUGBL, ...\`
* Descrizione: Indice secondario per le partite aperte dei clienti.
* Possibili Join:
  * **BKPF** (Accounting documents): su \`MANDT, BUKRS, BELNR, GJAHR\`
  * **KNA1** (Customer Master): su \`MANDT, KUNNR\`

**BSIK (Accounting: Secondary index for vendors)**
* Chiavi Primarie: \`MANDT, BUKRS, LIFNR, AUGDT, AUGBL, ...\`
* Descrizione: Indice secondario per le partite aperte dei fornitori.
* Possibili Join:
  * **BKPF** (Accounting documents): su \`MANDT, BUKRS, BELNR, GJAHR\`
  * **LFA1** (Vendor Master): su \`MANDT, LIFNR\`

**BSIM (Secondary Index, Documents for Material)**
* Chiavi Primarie: \`MANDT, MATNR, BWKEY, BWTAR, ...\`
* Descrizione: Indice secondario dei documenti per materiale.
* Possibili Join:
  * **MARA** (Material Master): su \`MANDT, MATNR\`
  * **BKPF** (Accounting documents): su \`MANDT, BELNR, GJAHR\`

**BSIP (Index for vendor validation of double documents)**
* Chiavi Primarie: \`MANDT, LIFNR, WAERS, XBLNR, ...\`
* Descrizione: Indice per la validazione di fatture fornitore duplicate.
* Possibili Join:
  * **LFA1** (Vendor Master): su \`MANDT, LIFNR\`
  * **BKPF** (Accounting documents): su \`MANDT, BUKRS, BELNR_D=BELNR, GJAHR_D=GJAHR\`

**BSIS (Accounting: Secondary index for G/L accounts)**
* Chiavi Primarie: \`MANDT, BUKRS, HKONT, AUGDT, AUGBL, ...\`
* Descrizione: Indice secondario per le partite aperte dei conti Co.Ge.
* Possibili Join:
  * **BKPF** (Accounting documents): su \`MANDT, BUKRS, BELNR, GJAHR\`
  * **SKA1** (Accounts): su \`MANDT, HKONT=SAKNR\`

**BSAD (Accounting: Index for customers (cleared items))**
* Chiavi Primarie: \`MANDT, BUKRS, KUNNR, AUGDT, AUGBL, ...\`
* Descrizione: Indice secondario per le partite pareggiate (chiuse) dei clienti.
* Possibili Join:
  * **BKPF** (Accounting documents): su \`MANDT, BUKRS, BELNR, GJAHR\`
  * **KNA1** (Customer Master): su \`MANDT, KUNNR\`

**BSAK (Accounting: Index for vendors (cleared items))**
* Chiavi Primarie: \`MANDT, BUKRS, LIFNR, AUGDT, AUGBL, ...\`
* Descrizione: Indice secondario per le partite pareggiate (chiuse) dei fornitori.
* Possibili Join:
  * **BKPF** (Accounting documents): su \`MANDT, BUKRS, BELNR, GJAHR\`
  * **LFA1** (Vendor Master): su \`MANDT, LIFNR\`

**BSAS (Accounting: Index for G/L accounts (cleared items))**
* Chiavi Primarie: \`MANDT, BUKRS, HKONT, AUGDT, AUGBL, ...\`
* Descrizione: Indice secondario per le partite pareggiate (chiuse) dei conti Co.Ge.
* Possibili Join:
  * **BKPF** (Accounting documents): su \`MANDT, BUKRS, BELNR, GJAHR\`
  * **SKA1** (Accounts): su \`MANDT, HKONT=SAKNR\`

##### Payment run
**REGUH (Settlement data from payment program)**
* Chiavi Primarie: \`MANDT, LAUFD, LAUFI, XVORL, ...\`
* Descrizione: Dati di testata della proposta di pagamento.
* Possibili Join:
  * **REGUP** (Processed items from payment program): su \`MANDT, LAUFD, LAUFI, XVORL\`
  * **LFA1** (Vendor Master): su \`MANDT, LIFNR\`
  * **KNA1** (Customer Master): su \`MANDT, KUNNR\`

**REGUP (Processed items from payment program)**
* Chiavi Primarie: \`MANDT, LAUFD, LAUFI, XVORL, BUKRS, BELNR, ...\`
* Descrizione: Posizioni (documenti) elaborate dal programma di pagamento.
* Possibili Join:
  * **REGUH** (Settlement data): su \`MANDT, LAUFD, LAUFI, XVORL\`
  * **BKPF** (Accounting documents): su \`MANDT, BUKRS, BELNR, GJAHR\`
`;
