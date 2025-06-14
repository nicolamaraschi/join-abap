import React from 'react';
import BapiList from './BapiList.jsx';
import TableList from './TableList.jsx';
import PresetList from './PresetList.jsx';
import TransactionNavList from './TransactionNavList.jsx'; // Importa il nuovo componente

const Sidebar = ({ viewMode, searchTerm, onSearchChange, tables, bapis, presets, transactionModules, onSelectItem }) => {
    
    const getPlaceholder = () => {
        switch(viewMode) {
            case 'TABLES': return "Cerca tabella...";
            case 'BAPIS': return "Cerca BAPI...";
            case 'PRESETS': return "Cerca preset...";
            case 'TRANSACTIONS': return "Navigazione Report Transazioni"; // Testo per la modalità transazioni
            default: return "Cerca...";
        }
    };

    const renderList = () => {
        switch (viewMode) {
            case 'TABLES':
                return <TableList tables={tables} onSelectTable={onSelectItem} />;
            case 'BAPIS':
                return <BapiList bapis={bapis} onSelectBapi={onSelectItem} />;
            case 'PRESETS':
                return <PresetList presets={presets} onSelectPreset={onSelectItem} />;
            case 'TRANSACTIONS':
                // Renderizza la lista di navigazione per le transazioni
                return <TransactionNavList modules={transactionModules} />;
            default:
                return null;
        }
    };

    return (
        <aside className="sidebar">
            <div className="search-container">
                <input 
                    type="text" 
                    placeholder={getPlaceholder()} 
                    className="search-input" 
                    value={searchTerm} 
                    onChange={(e) => onSearchChange(e.target.value)}
                    disabled={viewMode === 'TRANSACTIONS'} // Disabilita la ricerca in questa modalità
                />
                <svg xmlns="http://www.w3.org/2000/svg" className="search-icon" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
                </svg>
            </div>
            
            {renderList()}
        </aside>
    );
};

export default Sidebar;
