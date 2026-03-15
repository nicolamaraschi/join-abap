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
    onViewModeSelect,
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
    transactionSubMode,
    setTransactionSubMode,
}) => {
    const renderContent = () => {
        switch (viewMode) {
            case 'TABLES':
                return selectedTable
                    ? <TableDetailsView table={selectedTable} onSelectTable={onSelectTable} />
                    : <WelcomeView allTables={allTables} onSelectMode={onViewModeSelect} />;
            case 'BAPIS':
                return selectedBapi
                    ? <BapiDetailsView bapi={selectedBapi} />
                    : <WelcomeView isBapiMode={true} allTables={allTables} onSelectMode={onViewModeSelect} />;
            case 'PRESETS':
                return selectedPreset
                    ? <PresetDetailsView preset={selectedPreset} />
                    : <WelcomeView isPresetMode={true} onSelectMode={onViewModeSelect} />;
            case 'FIORI_PRESETS':
                return selectedFioriPreset
                    ? <PresetDetailsView preset={selectedFioriPreset} />
                    : <WelcomeView isPresetMode={true} onSelectMode={onViewModeSelect} />;
            case 'CDS':
                if (cdsSubMode === 'docs') {
                    return selectedCds
                        ? <CdsDocumentationView cdsDoc={selectedCds} />
                        : <WelcomeView isCdsMode={true} onSelectMode={onViewModeSelect} />;
                } else if (cdsSubMode === 'presets') {
                    return selectedCdsPreset
                        ? <PresetDetailsView preset={selectedCdsPreset} />
                        : <WelcomeView isCdsPresetMode={true} onSelectMode={onViewModeSelect} />;
                }
                return <WelcomeView onSelectMode={onViewModeSelect} />;
            case 'ABAP_DOC':
                if (selectedAbapDoc) {
                    return <AbapDocDetailsView doc={selectedAbapDoc} />;
                } else if (window.location.pathname.includes('/sap-abap-guide/')) {
                    return <SapAbapGuideDetailsView />;
                } else {
                    return <WelcomeView isAbapDocMode={true} onSelectMode={onViewModeSelect} />;
                }
            case 'BADIS':
                return selectedBadi
                    ? <BadiDetailsView badi={selectedBadi} />
                    : <WelcomeView isBadiMode={true} onSelectMode={onViewModeSelect} />;
            case 'SMARTFORMS':
                return selectedSmartform
                    ? <SmartformDetailsView smartform={selectedSmartform} />
                    : <WelcomeView isSmartformMode={true} onSelectMode={onViewModeSelect} />;
            case 'ADOBEFORMS':
                return selectedAdobeform
                    ? <AdobeformDetailsView adobeform={selectedAdobeform} />
                    : <WelcomeView isAdobeformMode={true} onSelectMode={onViewModeSelect} />;
            case 'TRANSACTIONS':
                return <ReportView data={transactionData} activeView={transactionSubMode} onViewChange={setTransactionSubMode} />;
            default:
                return <WelcomeView allTables={allTables} onSelectMode={onViewModeSelect} />;
        }
    };
    return (<main className="main-content">{renderContent()}</main>);
};

export default MainContent;