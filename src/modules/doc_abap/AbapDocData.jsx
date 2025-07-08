// src/modules/doc_abap/AbapDocData.jsx
// COPIA E INCOLLA QUESTO CODICE. NON MODIFICARE NULLA.

import { content as docClDocumentBcsContent } from './DocClDocumentBcs.jsx';
import { content as docClSalvTableContent } from './DocClSalvTable.jsx';
import { content as docClSalvHierseqTableContent } from './DocClSalvHierseqTable.jsx';
import { content as docReuseAlvContent } from './DocReuseAlv.jsx';
import { content as docAbapEvolutionContent } from './DocAbapEvolution.jsx';
import { content as compilerIssuesContent } from './CompilerIssues.jsx';

// IMPORT CORRETTO PER DYNPRO: USA LE PARENTESI GRAFFE
import { content as docdympro } from './Dynpro.jsx';
import { content as docbabi} from './Bapi.jsx';

export const abapDocData = [
  {
    id: 'COMPILER_ISSUES',
    title: 'Problemi Compilatore ABAP',
    content: compilerIssuesContent,
  },
  {
    id: 'DOC_ABAP_EVOLUTION',
    title: 'Guida: Evoluzione ABAP da ECC a S/4HANA',
    content: docAbapEvolutionContent,
  },
  {
    id: 'DOC_REUSE_ALV',
    title: 'Guida Tecnica: REUSE_ALV (Classico)',
    content: docReuseAlvContent,
  },
  {
    id: 'DOC_CL_DOCUMENT_BCS',
    title: 'Guida Tecnica: CL_DOCUMENT_BCS',
    content: docClDocumentBcsContent,
  },
  {
    id: 'DOC_CL_SALV_TABLE',
    title: 'Guida Tecnica: CL_SALV_TABLE',
    content: docClSalvTableContent,
  },
  {
    id: 'DOC_CL_SALV_HIERSEQ_TABLE',
    title: 'Guida Tecnica: CL_SALV_HIERSEQ_TABLE',
    content: docClSalvHierseqTableContent,
  },
  {
    id: 'ABAP_POPUP_DYNPRO',
    title: 'Guida Definitiva: Popup Dynpro (Approccio Ibrido con Navigazione Dettagliata)',
    content: docdympro,
  },
  {
    id: 'bapi',
    title: 'Bapi funzionanti)',
    content: docbabi,
  }
];