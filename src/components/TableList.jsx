// FILE: src/components/TableList.jsx
import React, { useMemo } from 'react';

const TableList = ({ tables, onSelectTable, selectedTable }) => {
    const groupedTables = useMemo(() => {
        return tables.reduce((acc, table) => {
            const subModule = table.subModule || 'Generale';
            if (!acc[subModule]) acc[subModule] = [];
            acc[subModule].push(table);
            return acc;
        }, {});
    }, [tables]);

    const subModules = Object.keys(groupedTables);
    if (tables.length === 0) return <div style={{textAlign: 'center', color: '#64748b', padding: '1rem'}}>Nessun elemento trovato.</div>;

    return (
        <div>
            {subModules.map(subModule => (
                <div key={subModule}>
                    <h3 className="submodule-header">{subModule}</h3>
                    <ul className="table-list">
                        {groupedTables[subModule].map(table => (
                            <li key={table.name} onClick={() => onSelectTable(table.name)} className={`table-list-item ${table.name === selectedTable ? 'active' : ''}`}>
                                <div>
                                    <p className="table-name">{table.name}</p>
                                    <p className="table-description">{table.description || 'N/A'}</p>
                                </div>
                            </li>
                        ))}
                    </ul>
                </div>
            ))}
        </div>
    );
};

export default TableList;