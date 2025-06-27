// src/components/AdobeformDetailsView.jsx
import React from 'react';
import MarkdownRenderer from './MarkdownRenderer.jsx'; // Importa il componente MarkdownRenderer
import './DetailsView.css'; // Re-usa il CSS generico per le viste di dettaglio

const AdobeformDetailsView = ({ adobeform }) => {
    if (!adobeform) {
        return <div className="details-view-placeholder">Seleziona un Adobe Form dalla lista per visualizzarne i dettagli.</div>;
    }

    return (
        <div className="details-view-container">
            <h1 className="details-view-title">{adobeform.name}</h1>
            <p className="details-view-description">{adobeform.description}</p>

            <div className="details-view-section">
                <h2>Modulo:</h2>
                <p>{adobeform.module}</p>
            </div>

            {adobeform.transactions && adobeform.transactions.length > 0 && (
                <div className="details-view-section">
                    <h2>Transazioni Associate:</h2>
                    <ul>
                        {adobeform.transactions.map((t, index) => (
                            <li key={index}>{t}</li>
                        ))}
                    </ul>
                </div>
            )}

            {adobeform.purpose && (
                <div className="details-view-section">
                    <h2>Scopo:</h2>
                    <p>{adobeform.purpose}</p>
                </div>
            )}

            {/* Renderizza le sezioni di documentazione */}
            {adobeform.sections && adobeform.sections.length > 0 && (
                <div className="details-view-sections">
                    {adobeform.sections.map((section, index) => (
                        <div key={index} className="details-view-section">
                            <h2 id={`adobeform-section-${index}`}>{section.title}</h2> {/* ID per potenziale navigazione */}
                            {/* Applica la classe 'prose' qui per la formattazione del markdown */}
                            <div className="report-content prose" style={{maxWidth: '100%'}}>
                                <MarkdownRenderer text={section.content} />
                            </div>
                        </div>
                    ))}
                </div>
            )}
        </div>
    );
};

export default AdobeformDetailsView;