// src/components/MainContent.jsx
import React from 'react';
import WelcomeView from './WelcomeView.jsx';
import TableDetailsView from './TableDetailsView.jsx';
import BapiDetailsView from './BapiDetailsView.jsx';
import PresetDetailsView from './PresetDetailsView.jsx';
import ReportView from './ReportView.jsx';
import AbapDocDetailsView from './AbapDocDetailsView.jsx';
import CdsDocumentationView from './CdsDocumentationView.jsx';
import BadiDetailsView from './BadiDetailsView.jsx';
// Importa i nuovi componenti per Smartforms e Adobe Forms
import SmartformDetailsView from './SmartformDetailsView.jsx';

import AdobeformDetailsView from './AdobeformDetailsView.jsx';

const MainContent = ({
    viewMode,
    selectedTable,
    selectedBapi,
    selectedCds,
    selectedCdsPreset, // Aggiunto
    selectedPreset,
    selectedFioriPreset,
    selectedAbapDoc,
    selectedBadi,
    selectedSmartform,
    selectedAdobeform,
    transactionData,
    allTables,
    onSelectTable,
    cdsSubMode, // Aggiunto
}) => {
    const renderContent = () => {
        switch (viewMode) {
            case 'TABLES':
                return selectedTable
                    ? <TableDetailsView table={selectedTable} onSelectTable={onSelectTable} />
                    : <WelcomeView allTables={allTables} />;
            case 'BAPIS':
                return selectedBapi
                    ? <BapiDetailsView bapi={selectedBapi} />
                    : <WelcomeView isBapiMode={true} allTables={allTables} />;
            case 'PRESETS':
                return selectedPreset
                    ? <PresetDetailsView preset={selectedPreset} />
                    : <WelcomeView isPresetMode={true} />;
            case 'FIORI_PRESETS':
                return selectedFioriPreset
                    ? <PresetDetailsView preset={selectedFioriPreset} />
                    : <WelcomeView isPresetMode={true} />;
            case 'CDS':
                if (cdsSubMode === 'docs') {
                    return selectedCds
                        ? <CdsDocumentationView cdsDoc={selectedCds} />
                        : <WelcomeView isCdsMode={true} />;
                } else if (cdsSubMode === 'presets') {
                    return selectedCdsPreset
                        ? <PresetDetailsView preset={selectedCdsPreset} />
                        : <WelcomeView isCdsPresetMode={true} />;
                }
                return <WelcomeView />;
            case 'ABAP_DOC':
                if (selectedAbapDoc) {
                    return <AbapDocDetailsView doc={selectedAbapDoc} />;
                } else if (window.location.pathname.includes('/sap-abap-guide/')) {
                    return <SapAbapGuideDetailsView />;
                } else {
                    return <WelcomeView isAbapDocMode={true} />;
                }
            case 'BADIS':
                return selectedBadi
                    ? <BadiDetailsView badi={selectedBadi} />
                    : <WelcomeView isBadiMode={true} />;
            case 'SMARTFORMS':
                return selectedSmartform
                    ? <SmartformDetailsView smartform={selectedSmartform} />
                    : <WelcomeView isSmartformMode={true} />;
            case 'ADOBEFORMS':
                return selectedAdobeform
                    ? <AdobeformDetailsView adobeform={selectedAdobeform} />
                    : <WelcomeView isAdobeformMode={true} />;
            case 'TRANSACTIONS':
                return <ReportView data={transactionData} />;
            default:
                return <WelcomeView allTables={allTables} />;
        }
    };
    return (<main className="main-content">{renderContent()}</main>);
};

export default MainContent;