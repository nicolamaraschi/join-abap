import React, { useState } from 'react';
import './ReportView.css';

const ReportView = ({ data }) => {
    const [view, setView] = useState('fiori'); // 'fiori' o 'gui'

    // Controllo di sicurezza per assicurarsi che i dati e le loro proprietà esistano
    if (!data || !data.fiori || !data.gui) {
        return <div className="details-view"><p>Dati delle transazioni non disponibili o in formato non corretto.</p></div>;
    }

    const renderFioriView = () => (
        <>
            <div className="report-introduction">
                <h1>{data.fiori.introduction.title}</h1>
                <p>{data.fiori.introduction.content}</p>
            </div>
            {data.fiori.modules.map(module => (
                <section key={`fiori-${module.id}`} id={`module-section-${module.id}`} className="module-section">
                    <h2>{module.name}</h2>
                    <p className="module-context">{module.context}</p>
                    {module.submodules.map(submodule => (
                        <div key={submodule.id} className="submodule-section">
                            <h3>{submodule.name}</h3>
                            <div className="table-container">
                                <table>
                                    <thead>
                                        <tr>
                                            <th>Processo</th>
                                            <th>Nome App Fiori</th>
                                            <th>App ID</th>
                                            <th>T-Code Classico</th>
                                            <th>Scopo e Utilizzo</th>
                                        </tr>
                                    </thead>
                                    <tbody>
                                        {submodule.transactions.map((tx, index) => (
                                            <tr key={index}>
                                                <td><strong>{tx.process}</strong></td>
                                                <td>{tx.appName}</td>
                                                <td>{tx.appId}</td>
                                                <td>{tx.tCode}</td>
                                                <td>{tx.purpose}</td>
                                            </tr>
                                        ))}
                                    </tbody>
                                </table>
                            </div>
                        </div>
                    ))}
                </section>
            ))}
        </>
    );

    const renderGuiView = () => (
        <>
            <div className="report-introduction">
                <h1>{data.gui.introduction.title}</h1>
                <p>{data.gui.introduction.content}</p>
            </div>
             <div className="module-section">
                <h2>{data.gui.architecturalChanges.title}</h2>
                {data.gui.architecturalChanges.points.map(point => (
                     <div key={point.title} className="submodule-section">
                        <h3>{point.title}</h3>
                        <p className="module-context">{point.content}</p>
                    </div>
                ))}
            </div>
            {data.gui.modules.map(module => (
                <section key={`gui-${module.id}`} id={`module-section-${module.id}`} className="module-section">
                    <h2>{module.name}</h2>
                    <p className="module-context">{module.introduction}</p>
                     <div className="table-container">
                        <table>
                            <thead>
                                <tr>
                                    <th>T-Code</th>
                                    <th>Descrizione</th>
                                    <th>Stato in S/4HANA</th>
                                    <th>Successore / Note Chiave</th>
                                </tr>
                            </thead>
                            <tbody>
                                {module.transactions.map((tx, index) => (
                                    <tr key={index}>
                                        <td><strong>{tx.tCode}</strong></td>
                                        <td>{tx.description}</td>
                                        <td>{tx.status}</td>
                                        <td>{tx.notes}</td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    </div>
                </section>
            ))}
        </>
    );

    return (
        <div className="details-view">
            <div className="view-selector">
                <button onClick={() => setView('fiori')} className={view === 'fiori' ? 'active' : ''}>
                    App Fiori
                </button>
                <button onClick={() => setView('gui')} className={view === 'gui' ? 'active' : ''}>
                    Transazioni GUI
                </button>
            </div>
            <div className="report-content prose">
                {view === 'fiori' ? renderFioriView() : renderGuiView()}
            </div>
        </div>
    );
};

export default ReportView;
