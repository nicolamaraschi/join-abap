import { content as excelToAbapContent } from './presets/ExcelToAbap.jsx';
import { content as bapiHandlerContent } from './presets/BapiHandler.jsx';
import { content as popupConfirmContent } from './presets/PopupConfirm.jsx';
import { content as smartformCallContent } from './presets/SmartformCall.jsx';
import { content as emailBcsContent } from './presets/EmailBcs.jsx';
import { content as alvSalvTableContent } from './presets/AlvSalvTable.jsx';
import { content as hierarchicalAlvContent } from './presets/HierarchicalAlv.jsx';
import { content as reuseAlvLegacyContent } from './presets/ReuseAlvLegacy.jsx';
import { content as gui_download } from './presets/Gui_download.jsx';  // 1. Aggiungi questa riga
import { content as cl_gui_alv_grid } from './presets/cl_gui_alv_grid.jsx';
export const codePresets = [
  {
    id: 'EXCEL_TO_ABAP',
    title: 'Lettura File Excel e Mappatura in Tabella ABAP usando function ASML_EXCEL_TO_INTERNAL_TABLE',
    content: excelToAbapContent
  },
  {
    id: 'BAPI_GENERIC_HANDLER',
    title: 'Gestione Generica Chiamata BAPI',
    content: bapiHandlerContent
  },
  {
    id: 'POPUP_CONFIRM',
    title: 'Creazione Popup di Conferma (POPUP_TO_CONFIRM)',
    content: popupConfirmContent
  },
  {
    id: 'SMARTFORM_CALL',
    title: 'Chiamata a Smart Form (SSF_FUNCTION_MODULE_NAME)',
    content: smartformCallContent
  },
  {
    id: 'EMAIL_BCS',
    title: 'Invio Email con Allegato (Classe CL_BCS)',
    content: emailBcsContent
  },
  {
    id: 'ALV_SALV_TABLE',
    title: 'ALV Report con CL_SALV_TABLE e Gestione Eventi',
    content: alvSalvTableContent
  },
  {
    id: 'HIERARCHICAL_ALV_REPORT',
    title: 'cl_salv_hierseq_table',
    content: hierarchicalAlvContent
  },
  // 2. Aggiungi questo nuovo blocco alla fine dell'array
  {
    id: 'REUSE_ALV_LEGACY',
    title: 'reuse alv merge filedcatalog merge e reuse alv gris display',
    content: reuseAlvLegacyContent
  },
  {
    id: 'Gui_download',
    title: 'uso della funzione Gui_download per mettere dati sul excel',
    content: gui_download
  },
  {
    id: 'Cl_gui_alv_grid',
    title: 'classe per alv Cl_gui_alv_grid',
    content: cl_gui_alv_grid
  }
];