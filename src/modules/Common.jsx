export const commonData = `
### Modulo Common (Tabelle Comuni)

#### Tabelle Comuni e Intermodulari
**ADRC (Address Management: Central Address Table)**
* Chiavi Primarie: \`CLIENT, ADDRNUMBER\`
* Descrizione: Tabella centrale degli indirizzi.
* Possibili Join:
  * **KNA1** (Customer Master): su \`CLIENT=MANDT, ADDRNUMBER=ADRNR\`
  * **LFA1** (Vendor Master): su \`CLIENT=MANDT, ADDRNUMBER=ADRNR\`
  * **T001W** (Plants): su \`CLIENT=MANDT, ADDRNUMBER=ADRNR\`
  * **T001** (Company Codes): su \`CLIENT=MANDT, ADDRNUMBER=ADRNR\`

**BNKA (Bank Master Data)**
* Chiavi Primarie: \`MANDT, BANKS, BANKL\`
* Descrizione: Anagrafica banche.
* Possibili Join:
  * **KNBK** (Customer Bank Details): su \`MANDT, BANKS, BANKL\`
  * **LFBK** (Vendor Bank Details): su \`MANDT, BANKS, BANKL\`

**T006 (Units of Measurement)**
* Chiavi Primarie: \`MANDT, MSEHI\`
* Possibili Join:
  * **MARM** (Units of Measure for Material): su \`MANDT, MEINH=MSEHI\`
  * **MSEG** (Material Document Segment): su \`MANDT, ERFME=MSEHI\` (Unità di misura inserimento)
  * **EKPO** (Purchasing Document Item): su \`MANDT, MEINS=MSEHI\` (Unità di misura ordine)

**T006A (Units of Measurement Texts)**
* Chiavi Primarie: \`MANDT, SPRAS, MSEHI\`
* Possibili Join:
  * **T006** (Units of Measurement): su \`MANDT, MSEHI\`

**REGUH (Settlement data from payment program)**
* Chiavi Primarie: \`MANDT, LAUFD, LAUFI, XVORL, KOART, KONTO, BUKRS, VBLNR\`
* Descrizione: Dati di regolamento del programma di pagamento.
* Possibili Join:
  * **PAYR** (Payment Medium File): su \`MANDT, LAUFD, LAUFI\`
  * **BKPF** (Accounting Document Header): su \`MANDT, BUKRS, VBLNR=BELNR\` (VBLNR è il documento pagato)
  * **LFA1** (Vendor Master): su \`MANDT, KONTO=LIFNR\` (se KOART='K')
  * **KNA1** (Customer Master): su \`MANDT, KONTO=KUNNR\` (se KOART='D')
`;
