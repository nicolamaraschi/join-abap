import { content as excelToAbapContent } from './presets/ExcelToAbap.jsx';
import { content as bapiHandlerContent } from './presets/BapiHandler.jsx';
import { content as popupConfirmContent } from './presets/PopupConfirm.jsx';
import { content as smartformCallContent } from './presets/SmartformCall.jsx';
import { content as emailBcsContent } from './presets/EmailBcs.jsx';
import { content as alvSalvTableContent } from './presets/AlvSalvTable.jsx';

export const codePresets = [
  {
    id: 'EXCEL_TO_ABAP',
    title: 'Lettura File Excel e Mappatura in Tabella ABAP',
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
  }
];