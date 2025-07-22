import { coData } from './CO.jsx';
import { fiData } from './FI.jsx';
import { mmData } from './MM.jsx';
import { pmData } from './PM.jsx';
import { ppData } from './PP.jsx';
import { psData } from './PS.jsx';
import { qmData } from './QM.jsx';
import { sdData } from './SD.jsx';
import { SplitAlv } from './presets/SplitAlv.jsx';

// Esportiamo la variabile con il nome corretto: tableDataRaw
export const tableDataRaw = [
  fiData,
  coData,
  sdData,
  mmData,
  ppData,
  pmData,
  psData,
  qmData,
  SplitAlv,
].join('\n');