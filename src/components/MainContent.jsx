import React from 'react';
import WelcomeView from './WelcomeView.jsx';
import TableDetailsView from './TableDetailsView.jsx';

const MainContent = ({ selectedTable, allTables, onSelectTable }) => (
    <main className="main-content">
        {selectedTable ? <TableDetailsView table={selectedTable} onSelectTable={onSelectTable} /> : <WelcomeView allTables={allTables} />}
    </main>
);

export default MainContent;