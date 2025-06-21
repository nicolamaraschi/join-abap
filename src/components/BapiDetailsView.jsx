// FILE: src/components/BapiDetailsView.jsx
import React from 'react';
import AbapCode from './AbapCode.jsx'; // Importa il componente per il codice

const BapiDetailsView = ({ bapi }) => {
    if (!bapi) return null;
    return (
        <div className="details-view">
            {/* Sezione Intestazione (invariata) */}
            <div className="details-header-card"><h2>{bapi.name}</h2><p>{bapi.description}</p></div>
            
            {/* Sezione Dettagli Parametri (invariata) */}
            <div>{bapi.details.map((detail, index) => (
                <div key={index} className="mb-6">
                    <h3 className="text-xl font-semibold mb-2 text-slate-700">{detail.title}</h3>
                    <div className="text-slate-600 bg-white p-4 rounded-lg shadow-sm border border-slate-200">
                        {detail.content && <p className="mb-4">{detail.content}</p>}
                        {detail.structures && detail.structures.map((struct, sIndex) => (
                            <div key={sIndex} className="mb-4">
                                <h4 className="font-semibold text-slate-800">{struct.name} <span className="font-normal text-slate-500 text-sm">{struct.type}</span></h4>
                                <ul className="list-disc pl-5 mt-2 space-y-2">
                                    {struct.fields.map((field, fIndex) => (
                                        <li key={fIndex}>
                                            <code className="bg-slate-100 p-1 rounded text-sm text-slate-800">{field.name}</code>
                                            <span className="text-slate-600 ml-2">{field.desc}</span>
                                            {field.mandatory && <strong className="text-red-500 ml-2">(Obbligatorio)</strong>}
                                        </li>
                                    ))}
                                </ul>
                            </div>
                        ))}
                    </div>
                </div>
            ))}</div>

            {/* NUOVA SEZIONE: Visualizzazione Codice ABAP */}
            {bapi.content && bapi.content.trim() !== '' && (
                 <div className="mt-6">
                    <h3 className="text-xl font-semibold mb-2 text-slate-700">Esempio di Utilizzo</h3>
                    <div className="preset-code-container">
                        <AbapCode code={bapi.content} />
                    </div>
                </div>
            )}
        </div>
    );
};

export default BapiDetailsView;