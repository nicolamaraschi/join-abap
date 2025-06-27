// src/components/SmartformList.jsx
import React from 'react';
import './List.css'; // Re-usa il CSS generico per le liste

const SmartformList = ({ smartforms, onSelectSmartform }) => {
    return (
        <div className="list-container">
            {smartforms.length > 0 ? (
                <ul className="item-list">
                    {smartforms.map((sf) => (
                        <li key={sf.name} className="item-list-item" onClick={() => onSelectSmartform(sf.name)}>
                            <div className="item-title">{sf.name}</div>
                            <div className="item-description">{sf.description}</div>
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