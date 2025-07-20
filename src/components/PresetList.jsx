// FILE: src/components/PresetList.jsx
import React from 'react';

const PresetList = ({ presets, onSelectPreset, selectedPreset }) => {
    // Se non ci sono preset o la lista è vuota, mostra un messaggio.
    if (!presets || presets.length === 0) {
        return <div style={{textAlign: 'center', color: '#64748b', padding: '1rem'}}>Nessun preset trovato.</div>;
    }

    return (
        <ul className="table-list">
            {presets.map(preset => (
                <li key={preset.id} onClick={() => onSelectPreset(preset.id)} className={`table-list-item ${preset.id === selectedPreset ? 'active' : ''}`}>
                    <div>
                        <p className="table-name">{preset.title}</p>
                        <p className="table-description">{preset.description}</p>
                    </div>
                </li>
            ))}
        </ul>
    );
};

export default PresetList;