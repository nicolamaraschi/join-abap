import React, { useState } from 'react';
import AbapCode from './AbapCode.jsx';

const PresetDetailsView = ({ preset }) => {
    const [copyButtonText, setCopyButtonText] = useState('Copia Codice');

    if (!preset) return null;

    // La funzione di copia ora gestisce sia stringhe che array di codice
    const handleCopy = () => {
        let codeToCopy = '';

        if (typeof preset.content === 'string') {
            codeToCopy = preset.content;
        } else if (Array.isArray(preset.content)) {
            // Se è un array, unisce tutti i blocchi di codice separandoli con un commento
            codeToCopy = preset.content.map(item => 
                `*--------------------------------------------------\n* Esempio: ${item.title}\n*--------------------------------------------------\n\n${item.code}`
            ).join('\n\n\n');
        }

        if (!codeToCopy) return;

        const textArea = document.createElement('textarea');
        textArea.value = codeToCopy;
        textArea.style.position = 'fixed';
        textArea.style.top = '-9999px';
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
        document.body.removeChild(textArea);
        setTimeout(() => setCopyButtonText('Copia Codice'), 2000);
    };

    // Funzione per renderizzare il contenuto in modo flessibile
    const renderContent = () => {
        // Se il contenuto è un array, mappa ogni elemento
        if (Array.isArray(preset.content)) {
            return preset.content.map((item, index) => (
                <div key={index} className="code-example-block" style={{ marginBottom: '2rem' }}>
                    <h4 style={{ fontSize: '1.2rem', fontWeight: '600', marginBottom: '1rem', borderBottom: '1px solid #e5e7eb', paddingBottom: '0.5rem' }}>
                        {item.title}
                    </h4>
                    <div className="preset-code-container">
                        <AbapCode code={item.code} />
                    </div>
                </div>
            ));
        }
        
        // Altrimenti, se è una stringa, renderizza come prima
        if (typeof preset.content === 'string') {
            return (
                <div className="preset-code-container">
                    <AbapCode code={preset.content} />
                </div>
            );
        }

        return <p>Nessun contenuto da visualizzare.</p>;
    };

    return (
        <div className="details-view">
            <div className="details-header-card" style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', flexWrap: 'wrap', gap: '1rem' }}>
                <h2 style={{margin: 0}}>{preset.title}</h2>
                <button onClick={handleCopy} className="copy-button">
                    {copyButtonText}
                </button>
            </div>
            
            {renderContent()}

        </div>
    );
};

export default PresetDetailsView;