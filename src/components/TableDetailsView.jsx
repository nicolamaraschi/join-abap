// FILE: src/components/TableDetailsView.jsx
import React from 'react';
import RelatedTableCard from './RelatedTableCard.jsx';

const TableDetailsView = ({ table, onSelectTable }) => {
    if (!table) return null;
    return (
        <div className="details-view">
            <div className="details-header-card"><div style={{display: 'flex', flexDirection: 'column', gap: '1rem'}}>
                <div><h2>{table.name}</h2><p>{table.description || 'Nessuna descrizione disponibile.'}</p></div>
                <div style={{textAlign: 'left'}}><h3>🔑 Chiavi Primarie</h3><p className="keys">{table.primaryKeys?.length > 0 ? table.primaryKeys.join(', ') : 'Non definite'}</p></div>
            </div></div>
            <div><h3 style={{fontSize: '1.5rem', fontWeight: '600', marginBottom: '1rem'}}>Tabelle Correlate</h3>
                {table.joins?.length > 0 ? (<div className="related-tables-grid">
                    {table.joins.map((join, index) => <RelatedTableCard key={`${join.table.name}-${index}`} join={join} onSelectTable={onSelectTable} />)}
                </div>) : <p>Nessun join definito secondo le regole correnti.</p>}
            </div>
        </div>
    );
};

export default TableDetailsView;