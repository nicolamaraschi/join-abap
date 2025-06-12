// FILE: src/components/Sidebar.jsx
import React from 'react';
import BapiList from './BapiList.jsx';
import TableList from './TableList.jsx';

const Sidebar = ({ viewMode, searchTerm, onSearchChange, tables, bapis, onSelectTable, onSelectBapi }) => {
    return (
        <aside className="sidebar">
            <div className="search-container">
                <input 
                    type="text" 
                    placeholder={viewMode === 'TABLES' ? "Cerca tabella..." : "Cerca BAPI..."} 
                    className="search-input" 
                    value={searchTerm} 
                    onChange={(e) => onSearchChange(e.target.value)} 
                />
                <svg xmlns="http://www.w3.org/2000/svg" className="search-icon" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
                </svg>
            </div>
            
            {viewMode === 'TABLES' 
                ? <TableList tables={tables} onSelectTable={onSelectTable} /> 
                : <BapiList bapis={bapis} onSelectBapi={onSelectBapi} />
            }
        </aside>
    );
};

export default Sidebar;