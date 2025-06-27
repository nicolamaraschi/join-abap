// src/components/BadiDetailsView.jsx
import React from 'react';
import AbapCode from './AbapCode.jsx'; // Re-usa il tuo componente per il codice ABAP
import './DetailsView.css'; // Assicurati che questo file esista o cambia il percorso

const BadiDetailsView = ({ badi }) => {
    if (!badi) {
        return <div className="details-view-placeholder">Seleziona una BADI dalla lista per visualizzarne i dettagli.</div>;
    }

    return (
        <div className="details-view-container">
            <h1 className="details-view-title">{badi.name}</h1>
            <p className="details-view-description">{badi.description}</p>

            <div className="details-view-section">
                <h2>Modulo:</h2>
                <p>{badi.module}</p>
            </div>

            {badi.transactions && badi.transactions.length > 0 && (
                <div className="details-view-section">
                    <h2>Transazioni Associate:</h2>
                    <ul>
                        {badi.transactions.map((t, index) => (
                            <li key={index}>{t}</li>
                        ))}
                    </ul>
                </div>
            )}

            {badi.methods && badi.methods.length > 0 && (
                <div className="details-view-section">
                    <h2>Metodi:</h2>
                    {badi.methods.map((method, index) => (
                        <div key={index} className="method-details">
                            <h3>{method.name}</h3>
                            {method.parameters && <p><strong>Parametri:</strong> {method.parameters}</p>}
                            {method.description && <p>{method.description}</p>}
                            {method.codeExample && (
                                <div className="code-example">
                                    <h4>Esempio di Codice:</h4>
                                    <AbapCode code={method.codeExample} />
                                </div>
                            )}
                        </div>
                    ))}
                </div>
            )}
        </div>
    );
};

export default BadiDetailsView;