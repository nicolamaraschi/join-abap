import React from 'react';
import WelcomeView from './WelcomeView.jsx';
import TableDetailsView from './TableDetailsView.jsx';
import BapiDetailsView from './BapiDetailsView.jsx';
import PresetDetailsView from './PresetDetailsView.jsx';
import ReportView from './ReportView.jsx'; // Import ReportView

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
            default:
                return <WelcomeView allTables={allTables} />;
        }
    };
    return (<main className="main-content">{renderContent()}</main>);
};

export default MainContent;