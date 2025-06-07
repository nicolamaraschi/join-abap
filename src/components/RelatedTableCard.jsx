import React from 'react';

const RelatedTableCard = ({ join, onSelectTable }) => {
    const { table: targetTable, on: joinOn } = join;
    const isSelectable = targetTable && targetTable.name !== "Non trovato/definito";
    const cardClass = `related-card ${isSelectable ? 'selectable' : ''}`;
    return (
        <div onClick={() => isSelectable && onSelectTable(targetTable.name)} className={cardClass}>
            <div style={{display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '0.5rem'}}>
                <h4 className="table-name" style={{fontSize: '1.125rem'}}>{targetTable.name || 'N/D'}</h4>
                <span className="table-module-badge">{targetTable.module || 'N/A'}</span>
            </div>
            <p className="table-description" style={{height: '2rem', overflow: 'hidden'}}>{targetTable.description || 'Nessuna descrizione.'}</p>
            <div style={{borderTop: '1px solid #e2e8f0', paddingTop: '0.5rem'}}>
                <p style={{fontSize: '0.75rem', fontWeight: '600', color: '#475569'}}>🔗 Join su:</p>
                <p style={{fontSize: '0.75rem', fontFamily: 'monospace', color: '#64748b', wordBreak: 'break-all'}}>{joinOn.join(', ')}</p>
            </div>
        </div>
    );
};

export default RelatedTableCard;