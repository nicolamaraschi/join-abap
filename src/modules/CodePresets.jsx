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
import { content as cl_gui_alv_tree } from './presets/cl_gui_alv_tree.jsx';
import { content as cl_salv_gui_table_ida } from './presets/cl_salv_gui_table_ida.jsx';
import { content as cl_abap_format } from './presets/cl_abap_format.jsx';
import { content as popup_get_value } from './presets/popup_get_value.jsx';
import { content as popup_with_table_display } from './presets/popup_with_table_display.jsx';
import { content as cl_gui_alv_grid2 } from './presets/cl_gui_alv_grid2.jsx';
import { content as reuse_alv_popup_to_select } from './presets/reuse_alv_popup_to_select.jsx';
import { SplitAlv } from './presets/SplitAlv.jsx';

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
  },
  {
    id: 'cl_gui_alv_tree',
    title: 'classe per alv cl_gui_alv_tree',
    content: cl_gui_alv_tree
  },
  {
    id: 'cl_salv_gui_table_ida',
    title: 'classe per alv cl_salv_gui_table_ida',
    content: cl_salv_gui_table_ida
  },
  {
    id: 'cl_abap_format',
    title: 'classe per alv cl_abap_format',
    content: cl_abap_format
  },
  {
    id: 'popup_get_value',
    title: 'motodo per popup_get_value',
    content: popup_get_value
  },
  {
    id: 'popup_with_table_display',
    title: 'metodo popup_with_table_display',
    content: popup_with_table_display
  },
  {
    id: 'cl_gui_alv_grid2',
    title: 'cl_gui_alv_grid ma in una dympro',
    content: cl_gui_alv_grid2
  },
  {
    id: 'reuse_alv_popup_to_select',
    title: 'reuse_alv_popup_to_select ossia alv dentro pop up senza dympro',
    content: reuse_alv_popup_to_select
  },
  {
    id: 'SPLIT_ALV',
    title: 'ALV con Split Screen (due ALV uno sopra l\'altro)',
    content: SplitAlv
  }
];




