// FILE: src/components/MainContent.jsx
import React from 'react';
import WelcomeView from './WelcomeView.jsx';
import TableDetailsView from './TableDetailsView.jsx';
import BapiDetailsView from './BapiDetailsView.jsx';

const MainContent = ({ viewMode, selectedTable, selectedBapi, allTables, onSelectTable }) => {
    const renderContent = () => {
        if (viewMode === 'BAPIS') {
            return selectedBapi ? <BapiDetailsView bapi={selectedBapi} /> : <WelcomeView allTables={allTables} isBapiMode={true} />;
        }
        return selectedTable ? <TableDetailsView table={selectedTable} onSelectTable={onSelectTable} /> : <WelcomeView allTables={allTables} isBapiMode={false} />;
    };
    return (<main className="main-content">{renderContent()}</main>);
};

export default MainContent;