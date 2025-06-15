import { content as docClDocumentBcsContent } from './DocClDocumentBcs.jsx';
import { content as docClSalvTableContent } from './DocClSalvTable.jsx';
import { content as docClSalvHierseqTableContent } from './DocClSalvHierseqTable.jsx';
import { content as docReuseAlvContent } from './DocReuseAlv.jsx';
import { content as docAbapEvolutionContent } from './DocAbapEvolution.jsx'; // <-- Import del nuovo file

export const abapDocData = [
  {
    id: 'DOC_ABAP_EVOLUTION', // <-- Nuovo argomento
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
  }
];
