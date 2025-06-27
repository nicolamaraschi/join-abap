// src/components/SmartformDetailsView.jsx
import React from 'react';
import MarkdownRenderer from './MarkdownRenderer.jsx'; // Re-usa il tuo componente per il markdown
import './DetailsView.css'; // Re-usa il CSS generico per le viste di dettaglio

const SmartformDetailsView = ({ smartform }) => {
    if (!smartform) {
        return <div className="details-view-placeholder">Seleziona uno Smartform dalla lista per visualizzarne i dettagli.</div>;
    }

    return (
        <div className="details-view-container">
            <h1 className="details-view-title">{smartform.name}</h1>
            <p className="details-view-description">{smartform.description}</p>

            <div className="details-view-section">
                <h2>Modulo:</h2>
                <p>{smartform.module}</p>
            </div>

            {smartform.transactions && smartform.transactions.length > 0 && (
                <div className="details-view-section">
                    <h2>Transazioni Associate:</h2>
                    <ul>
                        {smartform.transactions.map((t, index) => (
                            <li key={index}>{t}</li>
                        ))}
                    </ul>
                </div>
            )}

            {smartform.purpose && (
                <div className="details-view-section">
                    <h2>Scopo:</h2>
                    <p>{smartform.purpose}</p>
                </div>
            )}

            {/* Renderizza le sezioni di documentazione */}
            {smartform.sections && smartform.sections.length > 0 && (
                <div className="details-view-sections">
                    {smartform.sections.map((section, index) => (
                        <div key={index} className="details-view-section">
                            <h2 id={`section-${index}`}>{section.title}</h2> {/* Aggiunto ID per potenziale navigazione */}
                            {/* Applica la classe 'prose' qui per la formattazione del markdown */}
                            <div className="report-content prose" style={{maxWidth: '100%'}}>
                                <MarkdownRenderer text={section.content} />
                            </div>
                        </div>
                    ))}
                </div>
            )}

            {/* Puoi aggiungere qui altri campi specifici per gli Smartforms */}
        </div>
    );
};

export default SmartformDetailsView;