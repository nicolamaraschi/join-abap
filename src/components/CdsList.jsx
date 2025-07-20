// src/components/CdsList.jsx
import React from 'react';
import './List.css'; // Re-usa il CSS generico per le liste

const CdsList = ({ cdsDocs, onSelectCdsDoc, selectedCdsDoc }) => {
    if (!cdsDocs || cdsDocs.length === 0) {
        return <p className="no-results">Nessuna documentazione CDS trovata.</p>;
    }

    return (
        <div className="list-container">
            <ul className="item-list">
                {cdsDocs.map((doc) => (
                    <li key={doc.name} className={`item-list-item ${doc.name === selectedCdsDoc ? 'active' : ''}`} onClick={() => onSelectCdsDoc(doc.name)}>
                        <div className="item-title">{doc.name}</div>
                        <div className="item-description">{doc.description}</div>
                    </li>
                ))}
            </ul>
        </div>
    );
};

export default CdsList;