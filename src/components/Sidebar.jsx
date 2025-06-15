// FILE: src/components/Sidebar.jsx
import React from 'react';
import BapiList from './BapiList.jsx';
import TableList from './TableList.jsx';
import PresetList from './PresetList.jsx';
// 1. Aggiungi l'importazione per il nuovo componente di navigazione
import AbapDocNavList from './AbapDocNavList.jsx';

const Sidebar = ({ viewMode, searchTerm, onSearchChange, tables, bapis, presets, onSelectItem }) => {
    
    const getPlaceholder = () => {
        switch(viewMode) {
            case 'TABLES': return "Cerca tabella...";
            case 'BAPIS': return "Cerca BAPI...";
            case 'PRESETS': return "Cerca preset...";
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