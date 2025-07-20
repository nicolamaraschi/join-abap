// src/components/SmartformList.jsx
import React from 'react';
import './List.css'; // Re-usa il CSS generico per le liste

const SmartformList = ({ smartforms, onSelectSmartform, selectedSmartform }) => {
    return (
        <div className="list-container">
            {smartforms.length > 0 ? (
                <ul className="item-list">
                    {groupedSmartforms[module].map(sf => (
                            <li key={sf.name} onClick={() => onSelectSmartform(sf.name)} className={`table-list-item ${sf.name === selectedSmartform ? 'active' : ''}`}>
                                <div>
                                    <p className="table-name">{sf.name}</p>
                                    <p className="table-description">{sf.description}</p>
                                </div>
                            </li>
                        ))}
                </ul>
            ) : (
                <p className="no-results">Nessuno Smartform trovato per la ricerca corrente.</p>
            )}
        </div>
    );
};

export default SmartformList;