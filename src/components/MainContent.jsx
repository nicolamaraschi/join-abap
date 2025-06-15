import React from 'react';
import WelcomeView from './WelcomeView.jsx';
import TableDetailsView from './TableDetailsView.jsx';
import BapiDetailsView from './BapiDetailsView.jsx';
import PresetDetailsView from './PresetDetailsView.jsx';
import ReportView from './ReportView.jsx'; // Import ReportView
import AbapDocViewer from './AbapDocViewer.jsx'; // Import the new ABAP documentation viewer

const MainContent = ({ viewMode, selectedTable, selectedBapi, selectedPreset, transactionData, allTables, onSelectTable }) => {
    const renderContent = () => {
        switch(viewMode) {
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
            case 'TRANSACTIONS':
                return <ReportView data={transactionData} />;
            case 'ABAP_DOCS': // Nuovo caso per la documentazione ABAP
                // Assumiamo che per ora ci sia solo la guida CL_SALV_TABLE.
                // In futuro, si potrebbe passare un prop o usare un altro meccanismo per selezionare documenti specifici.
                return <AbapDocViewer docId="cl_salv_table_guide" />;
            default:
                return <WelcomeView allTables={allTables} />;
        }
    };
    return (<main className="main-content">{renderContent()}</main>);
};

export default MainContent;
