import React, { useMemo } from 'react';

const TableList = ({ tables, onSelectTable }) => {
    const groupedTables = useMemo(() => {
        return tables.reduce((acc, table) => {
            const subModule = table.subModule || 'Generale';
            if (!acc[subModule]) {
                acc[subModule] = [];
            }
            acc[subModule].push(table);
            return acc;
        }, {});
    }, [tables]);

    const subModules = Object.keys(groupedTables);

    if (tables.length === 0) return <div className="text-slate-500 p-4 text-center">Nessuna tabella trovata.</div>;

    return (
        <div>
            {subModules.map(subModule => (
                <div key={subModule}>
                    <h3 className="submodule-header">{subModule}</h3>
                    <ul className="table-list">
                        {groupedTables[subModule].map(table => (
                            <li key={table.name} onClick={() => onSelectTable(table.name)} className="table-list-item">
                                <div>
                                    <span className="table-name">{table.name}</span>
                                    <span className="table-description">{table.description || 'N/A'}</span>
                                </div>
                                <span className="table-module-badge">{table.module}</span>
                            </li>
                        ))}
                    </ul>
                </div>
            ))}
        </div>
    );
};


const Sidebar = ({ searchTerm, onSearchChange, tables, onSelectTable }) => {
    return (
        <aside className="sidebar">
            <div className="search-container">
                <input 
                    type="text" 
                    placeholder="Cerca una tabella..." 
                    className="search-input" 
                    value={searchTerm} 
                    onChange={(e) => onSearchChange(e.target.value)} 
                />
                <svg xmlns="http://www.w3.org/2000/svg" className="search-icon" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
                </svg>
            </div>
            <TableList tables={tables} onSelectTable={onSelectTable} />
        </aside>
    );
};

export default Sidebar;