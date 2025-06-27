// src/components/CdsDocumentationView.jsx
import React from 'react';
import MarkdownRenderer from './MarkdownRenderer.jsx'; // Importa il componente MarkdownRenderer
import './DetailsView.css'; // Re-usa il CSS generico per le viste di dettaglio

const CdsDocumentationView = ({ cdsDoc }) => { // Ora riceve 'cdsDoc' come prop
    if (!cdsDoc) {
        return <div className="details-view-placeholder">Seleziona un documento CDS dalla lista per visualizzarne i dettagli.</div>;
    }

    return (
        <div className="details-view-container">
            <h1 className="details-view-title">{cdsDoc.name}</h1>
            <p className="details-view-description">{cdsDoc.description}</p>

            {cdsDoc.module && (
                <div className="details-view-section">
                    <h2>Modulo:</h2>
                    <p>{cdsDoc.module}</p>
                </div>
            )}

            {cdsDoc.transactions && cdsDoc.transactions.length > 0 && (
                <div className="details-view-section">
                    <h2>Transazioni Rilevanti:</h2>
                    <ul>
                        {cdsDoc.transactions.map((t, index) => (
                            <li key={index}>{t}</li>
                        ))}
                    </ul>
                </div>
            )}

            {/* Renderizza le sezioni di documentazione */}
            {cdsDoc.sections && cdsDoc.sections.length > 0 && (
                <div className="details-view-sections">
                    {cdsDoc.sections.map((section, index) => (
                        <div key={index} className="details-view-section">
                            <h2 id={`cds-section-${index}`}>{section.title}</h2> {/* ID per potenziale navigazione */}
                            {/* Applica la classe 'prose' per la formattazione del markdown */}
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

export default CdsDocumentationView;