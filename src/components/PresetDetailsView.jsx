import React, { useState } from 'react';
import AbapCode from './AbapCode.jsx';

const PresetDetailsView = ({ preset }) => {
    const [copyButtonText, setCopyButtonText] = useState('Copia Codice');

    if (!preset) return null;

    // Funzione di copia aggiornata e più robusta
    const handleCopy = () => {
        // Crea un textarea temporaneo
        const textArea = document.createElement('textarea');
        textArea.value = preset.content;

        // Rendi il textarea invisibile
        textArea.style.position = 'fixed';
        textArea.style.top = 0;
        textArea.style.left = 0;
        textArea.style.width = '2em';
        textArea.style.height = '2em';
        textArea.style.padding = 0;
        textArea.style.border = 'none';
        textArea.style.outline = 'none';
        textArea.style.boxShadow = 'none';
        textArea.style.background = 'transparent';

        document.body.appendChild(textArea);
        textArea.focus();
        textArea.select();

        try {
            document.execCommand('copy');
            setCopyButtonText('Copiato!');
        } catch (err) {
            setCopyButtonText('Errore!');
            console.error('Errore durante la copia del codice', err);
        }

        // Rimuovi il textarea temporaneo
        document.body.removeChild(textArea);

        // Resetta il testo del pulsante dopo 2 secondi
        setTimeout(() => setCopyButtonText('Copia Codice'), 2000);
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
