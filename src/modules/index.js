import { coData } from './CO.jsx';
import { commonData } from './Common.jsx';
import { fiData } from './FI.jsx';
import { mmData } from './MM.jsx';
import { pmData } from './PM.jsx';
import { ppData } from './PP.jsx';
import { psData } from './PS.jsx';
import { qmData } from './QM.jsx';
import { sdData } from './SD.jsx';

// Combiniamo tutte le stringhe dei dati in una sola
export const sapDataRaw = [
  fiData,
  coData,
  sdData,
  mmData,
  ppData,
  pmData,
  psData,
  qmData,
  commonData
].join('\\n');