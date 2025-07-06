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
    selectedCds, // Assicurati di ricevere selectedCds come prop
    selectedPreset,
    selectedFioriPreset, // Aggiunto
    selectedAbapDoc,
    selectedBadi,
    selectedSmartform, // Nuovo prop per Smartform
    selectedAdobeform, // Nuovo prop per Adobe Form
    transactionData,
    allTables,
    onSelectTable
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
            case 'FIORI_PRESETS': // Aggiunto
                return selectedFioriPreset
                    ? <PresetDetailsView preset={selectedFioriPreset} />
                    : <WelcomeView isPresetMode={true} />;
            case 'CDS': // <--- Modificato da 'CDS_DOCS' a 'CDS'
                return <CdsDocumentationView cdsDoc={selectedCds} />; // <--- Passa selectedCds a CdsDocumentationView
            case 'ABAP_DOC':
                return selectedAbapDoc
                    ? <AbapDocDetailsView doc={selectedAbapDoc} />
                    : <WelcomeView isAbapDocMode={true} />;
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