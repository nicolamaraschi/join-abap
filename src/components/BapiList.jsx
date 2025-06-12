// FILE: src/components/BapiList.jsx
import React from 'react';

const BapiList = ({ bapis, onSelectBapi }) => {
    if (!bapis || bapis.length === 0) return <div style={{textAlign: 'center', color: '#64748b', padding: '1rem'}}>Nessun elemento trovato.</div>;

    return (
        <ul className="table-list">
            {bapis.map(bapi => (
                <li key={bapi.name} onClick={() => onSelectBapi(bapi.name)} className="table-list-item">
                    <div>
                        <p className="table-name">{bapi.name}</p>
                        <p className="table-description">{bapi.description}</p>
                    </div>
                </li>
            ))}
        </ul>
    );
};

export default BapiList;