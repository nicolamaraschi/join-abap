// src/modules/doc_abap/AbapDocData.jsx
import { content as docClDocumentBcsContent } from './DocClDocumentBcs.jsx';
import { content as docClSalvTableContent } from './DocClSalvTable.jsx';
import { content as docClSalvHierseqTableContent } from './DocClSalvHierseqTable.jsx';
import { content as docReuseAlvContent } from './DocReuseAlv.jsx';
import { content as docAbapEvolutionContent } from './DocAbapEvolution.jsx';
import { content as compilerIssuesContent } from './CompilerIssues.jsx';
import { content as docdympro } from './Dynpro.jsx';
import { content as docbabi} from './Bapi.jsx';
import GuidaAbap from './GuidaAbap.jsx';

export const abapDocData = [
  {
    id: 'GUIDA_ABAP_MARKDOWN',
    title: 'Guida Introduttiva ABAP',
    content: <GuidaAbap />,
  },
  {
    id: 'COMPILER_ISSUES',
    title: 'Problemi Compilatore ABAP',
    content: compilerIssuesContent,
    isMarkdown: true // Aggiunto il flag
  },
  {
    id: 'DOC_ABAP_EVOLUTION',
    title: 'Guida: Evoluzione ABAP da ECC a S/4HANA',
    content: docAbapEvolutionContent,
    isMarkdown: true // Aggiunto il flag
  },
  {
    id: 'DOC_REUSE_ALV',
    title: 'Guida Tecnica: REUSE_ALV (Classico)',
    content: docReuseAlvContent,
    isMarkdown: true // Aggiunto il flag
  },
  {
    id: 'DOC_CL_DOCUMENT_BCS',
    title: 'Guida Tecnica: CL_DOCUMENT_BCS',
    content: docClDocumentBcsContent,
    isMarkdown: true // Aggiunto il flag
  },
  {
    id: 'DOC_CL_SALV_TABLE',
    title: 'Guida Tecnica: CL_SALV_TABLE',
    content: docClSalvTableContent,
    isMarkdown: true // Aggiunto il flag
  },
  {
    id: 'DOC_CL_SALV_HIERSEQ_TABLE',
    title: 'Guida Tecnica: CL_SALV_HIERSEQ_TABLE',
    content: docClSalvHierseqTableContent,
    isMarkdown: true // Aggiunto il flag
  },
  {
    id: 'ABAP_POPUP_DYNPRO',
    title: 'Guida Definitiva: Popup Dynpro (Approccio Ibrido con Navigazione Dettagliata)',
    content: docdympro,
    isMarkdown: true // Aggiunto il flag
  },
  {
    id: 'bapi',
    title: 'Bapi funzionanti)',
    content: docbabi,
    isMarkdown: true // Aggiunto il flag
  }
];
