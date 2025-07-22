export const cdsPresetsData = [
  {
    id: 'CDS_BASIC_VIEW_ENTITY',
    title: 'Vista CDS di Base (Basic View Entity)',
    description: 'Una vista di base che seleziona campi da una singola tabella sorgente (sflight) utilizzando la sintassi moderna DEFINE VIEW ENTITY. ✈️',
    content: `
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View for Flights'
@Metadata.allowExtensions: true
define view entity ZFLIGHT_BASIC_VIEW
  as select from sflight
{
      @Search.defaultSearchElement: true
  key carrid                as CarrID,
  key connid                as ConnectionID,
  key fldate                as FlightDate,
      currency              as Currency,
      planetype             as PlaneType,
      seatsmax              as SeatsMax,
      seatsocc              as SeatsOccupied,
      // Esempio di calcolo aritmetico
      (seatsmax - seatsocc) as SeatsAvailable
}
// Esempio di clausola WHERE
where
      carrid like '%A%'
  and connid > '0010'
`
  },
  {
    id: 'CDS_JOIN_EXAMPLE',
    title: 'Vista CDS con Join',
    description: 'Esempio di vista che combina più tabelle (vbak, vbap, kna1, makt) usando INNER JOIN per arricchire i dati degli ordini di vendita. 📄',
    content: `
@AbapCatalog.sqlViewName: 'ZV_DEMO_ES02'
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS con Join Esempio'
define view ZCDS_DEMO_ES02 as select
 from vbak  join vbap   on vbap.vbeln = vbak.vbeln
      inner join kna1   on kna1.kunnr = vbak.kunnr
      inner join makt   on makt.matnr = vbap.matnr
                        and makt.spras = 'I'
 {
    key vbak.vbeln,
    vbak.kunnr,
    kna1.name1,
    key vbap.posnr,
    vbap.matnr,
    makt.maktx,
    vbap.netwr
};
`
  },
  {
    id: 'CDS_ASSOCIATION_EXAMPLE',
    title: 'Vista CDS con Associazione',
    description: 'Esempio di vista con un\'associazione, il metodo moderno e performante per collegare entità. I dati dell\'associazione vengono letti solo se richiesti (lazy loading). 🔗',
    content: `
@AbapCatalog.sqlViewName: 'ZV_ORD_HDR_ES09'
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS con Associazione Esempio'
define view ZCDS_DEMO_ES09
  as select from vbak
  // L'associazione collega la testata (vbak) alle sue posizioni
  association [1..*] to ZCDS_SALESORDER_ITM_ES09 as _OrderItems
    on $projection.vbeln = _OrderItems.vbeln
{
  key vbeln,
      erdat,
      auart,
      netwr,
      waerk,
      // L'associazione è esposta per permettere ai consumatori
      // di accedere ai suoi campi (es. _OrderItems.matnr)
      _OrderItems
};
`
  },
  {
    id: 'CDS_WITH_PARAMETERS',
    title: 'Vista CDS con Parametri',
    description: 'Una vista che accetta parametri in input per filtrare i dati a livello di database, rendendola più flessibile e riutilizzabile. ⚙️',
    content: `
@AbapCatalog.sqlViewName: 'ZV_DEMO_ES03'
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS con Parametri Esempio'
define view ZCDS_DEMO_ES03
   with parameters p_AUART : auart,
                    p_vkorg : vkorg,
                    p_disc  : int4
as select from vbak
  inner join vbap on vbak.vbeln = vbap.vbeln
{
    key vbak.vbeln as vbeln,
    vbap.netwr as netwr,
    // Parametro usato in un calcolo
    (vbap.netwr * :p_disc) / 100 as disc_result
}
// Parametri usati nella clausola WHERE
where vbak.auart = :p_AUART and vbak.vkorg = $parameters.p_vkorg;
`
  },
  {
    id: 'CDS_CASE_EXPRESSION',
    title: 'Vista CDS con Espressione CASE',
    description: 'Esempio di utilizzo dell\'espressione CASE per implementare logica condizionale, come la divisione sicura per evitare errori di runtime. 🤔',
    content: `
@AbapCatalog.sqlViewName: 'Z_ITEM_PRICE'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS con CASE Esempio'
define view entity ZI_GC_SALESORTDER_ITEM as select from vbap as OrderItem
{
  key OrderItem.vbeln as OrderItem,
  key OrderItem.posnr as Item,
      OrderItem.kwmeng as ItemQuan,
      OrderItem.netwr as ItemValue,
      OrderItem.waerk as ItemCurr,
      // Divisione sicura: calcola il prezzo unitario
      // solo se la quantità è diversa da zero.
      case
        when cast(OrderItem.kwmeng as abap.dec(20,2)) = 0 then 0
        else cast(OrderItem.netwr as abap.dec(20,2)) / cast(OrderItem.kwmeng as abap.dec(20,2))
      end as UnitPrice
};
`
  },
  {
    id: 'CDS_AGGREGATE_FUNCTIONS',
    title: 'Vista CDS con Funzioni di Aggregazione',
    description: 'Una vista che usa funzioni di aggregazione come SUM e COUNT per raggruppare e calcolare totali. Richiede la clausola GROUP BY. 📊',
    content: `
@AbapCatalog.sqlViewName: 'ZV_DEMO_ES04'
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS con Aggregazioni Esempio'
define view ZCDS_DEMO_ES04
  as select from mchb
    inner join mara on mchb.matnr = mara.matnr
{
    key mara.matnr,
        mara.matkl,
    // Funzione di aggregazione SUM
    sum(mchb.clabs) as total_CLABS_SUM,
    // Funzione di aggregazione COUNT
    count( distinct mara.matnr) as MATNR_count
}
// Obbligatorio quando si usano funzioni di aggregazione
group by
  mara.matkl,
  mara.matnr;
`
  },
  {
    id: 'CDS_STRING_FUNCTIONS',
    title: 'Vista CDS con Funzioni di Stringa',
    description: 'Esempio di utilizzo di funzioni di stringa come CONCAT, SUBSTRING e LENGTH per manipolare e formattare i dati di testo. 📝',
    content: `
@AbapCatalog.sqlViewName: 'ZV_DEMO_ES05'
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'String Functions in ABAP CDS Views'
define view ZCDS_DEMO_ES05
  as select from kna1 {
    key kna1.kunnr as kunnr,
    // Concatena name1 e name2
    CONCAT( kna1.name1, kna1.name2 ) as full_name01,
    // Concatena con uno spazio
    CONCAT_WITH_SPACE( kna1.name1, kna1.name2, 4 ) as full_name02,
    // Estrae una sottostringa
    SUBSTRING( kna1.name1, 2, 10) as full_name03,
    // Lunghezza della stringa
    LENGTH( kna1.name1 ) as name_length,
    // Rimuove i caratteri 'L' a sinistra
    LTRIM( kna1.name1, 'L') as name_lt
};
`
  },
  {
    id: 'CDS_CURRENCY_CONVERSION',
    title: 'Vista CDS con Conversione Valuta',
    description: 'Esempio della potente funzione integrata CURRENCY_CONVERSION, che converte un importo in un\'altra valuta in base ai tassi presenti nel sistema. 💶',
    content: `
@AbapCatalog.sqlViewName: 'ZV_DEMO_ES10C'
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS con Conversione Valuta'
define view ZCDS_DEMO_ES10C as select from sflight
{
  key carrid as AirlineCode,
  key connid as ConnectionNumber,
  key fldate as FlightDate,
      price as Price,
      currency as Currency,
      @Semantics.amount.currencyCode: 'Currency'
      currency_conversion(
        amount             => price,
        source_currency    => currency,
        target_currency    => cast('EUR' as abap.cuky),
        exchange_rate_date => fldate
      ) as PriceInEUR
};
`
  },
  {
    id: 'CDS_UNIT_CONVERSION',
    title: 'Vista CDS con Conversione Unità di Misura',
    description: 'Esempio della funzione integrata UNIT_CONVERSION, utilizzata per convertire valori tra diverse unità di misura. 📏',
    content: `
@AbapCatalog.sqlViewName: 'ZV_DEMO_ES10U'
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'CDS con Conversione Unità'
define view ZCDS_DEMO_ES10U
 as select from spfli
 {
  key carrid,
  key connid,
      @Semantics.quantity.unitOfMeasure: 'distid'
      unit_conversion(
        quantity    => distance,
        source_unit => distid,
        target_unit => cast('MI' as abap.unit)
      ) as distance,
      @Semantics.unitOfMeasure: true
      cast('MI' as abap.unit) as distid
};
`
  },
  {
    id: 'CDS_TABLE_FUNCTION',
    title: 'Vista a Funzione di Tabella (Table Function)',
    description: 'Definizione di una Table Function. La logica non è in SQL ma in un metodo di una classe ABAP (AMDP), utile per scenari complessi. 🧑‍💻',
    content: `
@EndUserText.label: 'CDS Table Function Esempio'
define table function ZCDS_DEMO_ES07
  with parameters
    // Può ricevere parametri complessi come una stringa
    // che rappresenta una select-option
    sel_opt : abap.char( 1000 )
  returns {
    mandt : mandt;
    matnr : matnr;
    maktx : maktx;
    spras : spras;
  }
  implemented by method Zcl_adt_es07=>get_material;
`
  },
  {
    id: 'CDS_VDM_CONSUMPTION_ODATA',
    title: 'Vista di Consumo VDM (con OData)',
    description: 'Illustra una vista di Consumo (C_) pronta per l\'uso in una UI. Include annotazioni UI (@UI) e viene esposta come servizio OData. 🚀',
    content: `
@EndUserText.label: 'Manufacturing Order Operations Status'
@AbapCatalog.sqlViewName: 'Z_IMFGORDSTAT'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@VDM.viewType: #CONSUMPTION
@OData.publish: true
define view Z_I_MFGORDEROBJPGSTATUS
  as select distinct from I_StatusObjectActiveStatus as _StatusObject
    inner join I_MfgOrderStatus as _MfgOrderStatus
      on _MfgOrderStatus.ManufacturingObject = _StatusObject.StatusObject
{
      @UI.hidden: true
  key _MfgOrderStatus.ManufacturingOrder,
  key _StatusObject.StatusCode,

      @UI.lineItem: [{position:10}]
      _StatusObject.StatusShortName,

      @UI.lineItem: [{position:20}]
      _StatusObject.StatusName
}
where
  _StatusObject._StatusCode.StatusIsHidden = '';
`
  },
  {
    id: 'CDS_PATH_EXPRESSION',
    title: 'Vista CDS con Path Expression',
    description: 'Dimostra come navigare tra le associazioni usando "path expressions" per accedere a campi di viste collegate senza bisogno di join espliciti. 🗺️',
    content: `
@EndUserText.label: 'Status Text via Path Expression'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define view Z_I_MFGORDEROBJPGSTATUS
  as select from I_StatusObjectActiveStatus as _StatusObject
  // Questa associazione (_StatusCode) a sua volta contiene altre associazioni
  association [0..1] to I_StatusCode as _StatusCode on $projection.StatusCode = _StatusCode.StatusCode
{
  key _StatusObject.StatusObject,
      _StatusObject.StatusCode,

      // Accesso al campo IsUserStatus tramite il percorso:
      // _StatusObject -> _StatusCode -> _StatusCodeText -> IsUserStatus
      // Notare il filtro per la lingua all'interno del percorso.
      @UI.lineItem: [{position:10}]
      cast( case _StatusObject._StatusCode._StatusCodeText[1:Language=$session.system_language].IsUserStatus
         when 'X' then 'Stato Utente'
         else 'Stato di Sistema'
         end as j_txt30 ) as StatusType
}
`
  },
  {
    id: 'CDS_VIEW_EXTENSION',
    title: 'Vista CDS Estendibile',
    description: 'Mostra come definire una vista estendibile con `@Metadata.allowExtensions: true` e come creare una vista di estensione per aggiungere campi custom. ➕',
    content: `
-- 1. Vista principale che permette estensioni
@AbapCatalog.sqlViewName: 'I_SALESORD_EXT'
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Ordine di Vendita Estendibile'
@Metadata.allowExtensions: true
define view I_SalesOrder_Extendable as select from vbak
{
  key vbeln,
      auart,
      netwr,
      waerk
      // L'estensione aggiungerà qui i suoi campi
};

-- 2. Vista di estensione per aggiungere campi custom
@AbapCatalog.sqlViewName: 'ZE_SALESORD_EXT'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Estensione per Ordini di Vendita'
@Metadata.allowExtensions: false
extend view I_SalesOrder_Extendable with Z_E_SalesOrder_Ext
{
  vbak.vkorg,
  vbak.vtweg
  // Qui potrebbero esserci campi da tabelle custom,
  // aggiunti tramite JOIN
};
`
  }
];