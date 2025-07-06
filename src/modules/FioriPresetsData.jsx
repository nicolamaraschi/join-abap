// src/modules/FioriPresetsData.jsx
import { content as listReportContent } from './presets/fiori/FioriListReportView.jsx';
import { content as objectPageContent } from './presets/fiori/FioriObjectPageView.jsx';
import { content as controllerContent } from './presets/fiori/FioriController.jsx';

export const fioriPresetsData = [
  {
    id: 'FIORI_LIST_REPORT_OBJECT_PAGE',
    title: 'Fiori Elements: List Report con Object Page',
    description: 'Questo preset combina una List Report e una Object Page, il pattern più comune in Fiori Elements. La List Report usa una SmartTable per visualizzare una collezione di entità, mentre la Object Page mostra i dettagli di una singola entità selezionata.',
    snippets: [
      {
        title: 'ListReport.view.xml',
        code: listReportContent
      },
      {
        title: 'ObjectPage.view.xml',
        code: objectPageContent
      },
      {
        title: 'Base.controller.js',
        code: controllerContent
      }
    ]
  }
];