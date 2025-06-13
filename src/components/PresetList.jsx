// FILE: src/components/PresetDetailsView.jsx
import React, { useState } from 'react';
import AbapCode from './AbapCode.jsx';

const PresetDetailsView = ({ preset }) => {
    const [copyButtonText, setCopyButtonText] = useState('Copia Codice');

    if (!preset) return null;

    const handleCopy = () => {
        navigator.clipboard.writeText(preset.content).then(() => {
            setCopyButtonText('Copiato!');
            setTimeout(() => setCopyButtonText('Copia Codice'), 2000);
        }, () => {
            setCopyButtonText('Errore!');
            setTimeout(() => setCopyButtonText('Copia Codice'), 2000);
        });
    };

    return (
        <div className="details-view">
            <div className="details-header-card" style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', flexWrap: 'wrap', gap: '1rem' }}>
                <h2 style={{margin: 0}}>{preset.title}</h2>
                <button onClick={handleCopy} className="copy-button">
                    {copyButtonText}
                </button>
            </div>

            <div className="preset-code-container">
                <AbapCode code={preset.content} />
            </div>
        </div>
    );
};

export default PresetDetailsView;