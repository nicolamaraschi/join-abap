export const fiData = `
### Modulo FI (Financial Accounting)

#### FI :
##### Master data
**SKA1 (Accounts)**
* Chiavi Primarie: \`KTOPL, SAKNR\`
* Descrizione: Anagrafica Conti Co.Ge. a livello di piano dei conti.
* Possibili Join:
  * Nessun join all'interno di questa sottocategoria.

**BNKA (Bank master record)**
* Chiavi Primarie: \`BANKS, BANKL\`
* Descrizione: Anagrafica delle banche.
* Possibili Join:
  * Nessun join all'interno di questa sottocategoria.

##### Accounting documents // indices
**BKPF (Accounting documents)**
* Chiavi Primarie: \`BUKRS, BELNR, GJAHR\`
* Descrizione: Testata dei documenti contabili.
* Possibili Join:
  * **BSEG** (item level): su \`BUKRS, BELNR, GJAHR\`

**BSEG (item level)**
* Chiavi Primarie: \`BUKRS, BELNR, GJAHR, BUZEI\`
* Descrizione: Posizioni dei documenti contabili. Nota: In S/4HANA, ACDOCA è la tabella primaria per le voci.
* Possibili Join:
  * **BKPF** (Accounting documents): su \`BUKRS, BELNR, GJAHR\`
  * **BSIS** (Secondary index for G/L accounts): su \`BUKRS, BELNR, GJAHR, BUZEI\`
  * **BSID** (Secondary index for customers): su \`BUKRS, BELNR, GJAHR, BUZEI\`
  * **BSIK** (Secondary index for vendors): su \`BUKRS, BELNR, GJAHR, BUZEI\`

**BSID (Accounting: Secondary index for customers)**
* Descrizione: Vista Indice per le partite aperte dei clienti. Essendo una vista, non ha chiavi primarie proprie ma è usata per la ricerca.

**BSIK (Accounting: Secondary index for vendors)**
* Descrizione: Vista Indice per le partite aperte dei fornitori. Essendo una vista, non ha chiavi primarie proprie ma è usata per la ricerca.

**BSIM (Secondary Index, Documents for Material)**
* Chiavi Primarie: \`MATNR, BWKEY, BWTAR, BELNR, GJAHR, BUZEI\`
* Descrizione: Indice secondario dei documenti per materiale.
* Possibili Join:
  * **BKPF** (Accounting documents): su \`BELNR, GJAHR\`

**BSIP (Index for vendor validation of double documents)**
* Chiavi Primarie: \`BUKRS, LIFNR, WAERS, BLDAT, XBLNR, WRBTR, BELNR, GJAHR, BUZEI\`
* Descrizione: Indice per la validazione di fatture fornitore duplicate.
* Possibili Join:
  * **BKPF** (Accounting documents): su \`BUKRS, BELNR_D=BELNR, GJAHR_D=GJAHR\`

**BSIS (Accounting: Secondary index for G/L accounts)**
* Descrizione: Vista Indice per le partite aperte dei conti Co.Ge. Essendo una vista, non ha chiavi primarie proprie ma è usata per la ricerca.

**BSAD (Accounting: Index for customers (cleared items))**
* Descrizione: Vista Indice per le partite pareggiate (chiuse) dei clienti. Essendo una vista, non ha chiavi primarie proprie ma è usata per la ricerca.

**BSAK (Accounting: Index for vendors (cleared items))**
* Descrizione: Vista Indice per le partite pareggiate (chiuse) dei fornitori. Essendo una vista, non ha chiavi primarie proprie ma è usata per la ricerca.

**BSAS (Accounting: Index for G/L accounts (cleared items))**
* Descrizione: Vista Indice per le partite pareggiate (chiuse) dei conti Co.Ge. Essendo una vista, non ha chiavi primarie proprie ma è usata per la ricerca.

##### Payment run
**REGUH (Settlement data from payment program)**
* Chiavi Primarie: \`LAUFD, LAUFI, XVORL, ZBUKR, LIFNR, KUNNR, EMPFG, VBLNR\`
* Descrizione: Dati di testata della proposta di pagamento.
* Possibili Join:
  * **REGUP** (Processed items from payment program): su \`LAUFD, LAUFI, XVORL\`

**REGUP (Processed items from payment program)**
* Chiavi Primarie: \`LAUFD, LAUFI, XVORL, ZBUKR, LIFNR, KUNNR, EMPFG, VBLNR, BUKRS, BELNR, GJAHR, BUZEI\`
* Descrizione: Posizioni (documenti) elaborate dal programma di pagamento.
* Possibili Join:
  * **REGUH** (Settlement data): su \`LAUFD, LAUFI, XVORL\`
`;