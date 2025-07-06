export const cdsPresetsData = [
  {
    id: 'CDS_BASIC_VIEW',
    title: 'Basic CDS View',
    content: `
@AbapCatalog.sqlViewName: 'ZCDS_BASIC_VIEW'
@AbapCatalog.compiler.compareWithVersion: 'EQ'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Basic CDS View Example'
define view ZCDS_BASIC_VIEW as select from scarr {
  key carrid,
  carrname,
  currcode
};
`
  },
  {
    id: 'CDS_JOIN_EXAMPLE',
    title: 'CDS View with Join',
    content: `
@AbapCatalog.sqlViewName: 'ZCDS_JOIN_VIEW'
@AbapCatalog.compiler.compareWithVersion: 'EQ'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View with Join Example'
define view ZCDS_JOIN_VIEW as select from scarr
  inner join spfli on scarr.carrid = spfli.carrid
  {
    key scarr.carrid,
    scarr.carrname,
    spfli.connid,
    spfli.cityfrom,
    spfli.cityto
  };
`
  },
  {
    id: 'CDS_ASSOCIATION_EXAMPLE',
    title: 'CDS View with Association',
    content: `
@AbapCatalog.sqlViewName: 'ZCDS_ASSOC_VIEW'
@AbapCatalog.compiler.compareWithVersion: 'EQ'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View with Association Example'
define view ZCDS_ASSOC_VIEW as select from scarr {
  key carrid,
  carrname,
  _Connection : redirected to spfli
} association [0..*] to spfli as _Connection on $projection.carrid = _Connection.carrid;
`
  }
];
