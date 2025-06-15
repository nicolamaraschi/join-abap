import React from 'react';

const AbapDocList = ({ docs, onSelectDoc }) => {
    if (!docs || docs.length === 0) {
        return <div style={{textAlign: 'center', color: '#64748b', padding: '1rem'}}>Nessun documento in questa sezione.</div>;
    }

    return (
        <ul className="table-list">
            {docs.map(doc => (
                <li key={doc.id} onClick={() => onSelectDoc(doc.id)} className="table-list-item">
                    <div>
                        <p className="table-name">{doc.title}</p>
                    </div>
                </li>
            ))}
        </ul>
    );
};

export default AbapDocList;