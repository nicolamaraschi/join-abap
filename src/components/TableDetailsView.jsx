import React from 'react';
import RelatedTableCard from './RelatedTableCard.jsx';

const TableDetailsView = ({ table, onSelectTable }) => {
    if (!table) return null;
    const sortedJoins = [...(table.joins || [])].sort((a, b) => a.table.name.localeCompare(b.table.name));
    return (
        <div className="details-view">
            <div className="details-header-card">
                 <div style={{display: 'flex', flexDirection: 'column', gap: '1rem'}}>
                    <div>
                        <h2>{table.name}</h2>
                        <p>{table.description || 'Nessuna descrizione disponibile.'}</p>
                    </div>
                    <div style={{textAlign: 'left'}}>
                        <h3>🔑 Chiavi Primarie</h3>
                        <p className="keys">{table.primaryKeys?.length > 0 ? table.primaryKeys.join(', ') : 'Non definite'}</p>
                    </div>
                </div>
            </div>
            <div>
                <h3 style={{fontSize: '1.5rem', fontWeight: '600', marginBottom: '1rem'}}>Tabelle Correlate</h3>
                {sortedJoins.length > 0 ? (
                    <div className="related-tables-grid">
                        {sortedJoins.map((join, index) => <RelatedTableCard key={`${join.table.name}-${index}`} join={join} onSelectTable={onSelectTable} />)}
                    </div>
                ) : <p>Nessun join documentato per questa tabella.</p>}
            </div>
        </div>
    );
};

export default TableDetailsView;