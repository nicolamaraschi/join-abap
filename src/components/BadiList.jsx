// src/components/BadiList.jsx
import React from 'react';
import './List.css'; // Assicurati che questo file esista o cambia il percorso

const BadiList = ({ badis, onSelectBadi, selectedBadi }) => {
    // Raggruppa le BADI per modulo
    const groupedBadis = badis.reduce((acc, badi) => {
        const { module } = badi;
        if (!acc[module]) {
            acc[module] = [];
        }
        acc[module].push(badi);
        return acc;
    }, {});

    return (
        <div className="list-container">
            {badis.length > 0 ? (
                <ul className="item-list">
                    {Object.keys(groupedBadis).map(module => (
                        <div key={module}>
                            <h3 className="module-title">{module}</h3>
                            {groupedBadis[module].map(badi => (
                                <li key={badi.name} onClick={() => onSelectBadi(badi.name)} className={`table-list-item ${badi.name === selectedBadi ? 'active' : ''}`}>
                                    <div>
                                        <p className="table-name">{badi.name}</p>
                                        <p className="table-description">{badi.description}</p>
                                    </div>
                                </li>
                            ))}
                        </div>
                    ))}
                </ul>
            ) : (
                <p className="no-results">Nessuna BADI trovata per la ricerca corrente.</p>
            )}
        </div>
    );
};

export default BadiList;