// FILE: src/components/PresetList.jsx
import React from 'react';

const PresetList = ({ presets, onSelectPreset }) => {
    // Se non ci sono preset o la lista è vuota, mostra un messaggio.
    if (!presets || presets.length === 0) {
        return <div style={{textAlign: 'center', color: '#64748b', padding: '1rem'}}>Nessun preset trovato.</div>;
    }

    return (
        <ul className="table-list">
            {presets.map(preset => (
                <li key={preset.id} onClick={() => onSelectPreset(preset.id)} className="table-list-item">
                    <div>
                        <p className="table-name">{preset.title}</p>
                    </div>
                </li>
            ))}
        </ul>
    );
};

export default PresetList;