// src/components/BadiList.jsx
import React from 'react';
import './List.css'; // Assicurati che questo file esista o cambia il percorso

const BadiList = ({ badis, onSelectBadi }) => {
    return (
        <div className="list-container">
            {badis.length > 0 ? (
                <ul className="item-list">
                    {badis.map((badi) => (
                        <li key={badi.name} className="item-list-item" onClick={() => onSelectBadi(badi.name)}>
                            <div className="item-title">{badi.name}</div>
                            <div className="item-description">{badi.description}</div>
                        </li>
                    ))}
                </ul>
            ) : (
                <p className="no-results">Nessuna BADI trovata per la ricerca corrente.</p>
            )}
        </div>
    );
};

export default BadiList;