// src/components/AdobeformList.jsx
import React from 'react';
import './List.css'; // Re-usa il CSS generico per le liste

const AdobeformList = ({ adobeforms, onSelectAdobeform, selectedAdobeform }) => {
    return (
        <div className="list-container">
            {adobeforms.length > 0 ? (
                <ul className="item-list">
                    {adobeforms.map((af) => (
                        <li key={af.name} className={`item-list-item ${af.name === selectedAdobeform ? 'active' : ''}`} onClick={() => onSelectAdobeform(af.name)}>
                            <div className="item-title">{af.name}</div>
                            <div className="item-description">{af.description}</div>
                        </li>
                    ))}
                </ul>
            ) : (
                <p className="no-results">Nessun Adobe Form trovato per la ricerca corrente.</p>
            )}
        </div>
    );
};

export default AdobeformList;