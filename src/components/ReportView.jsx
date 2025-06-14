import React from 'react';

const TransactionView = ({ data }) => {
    if (!data) return null;

    return (
        <div className="details-view">
            <div className="report-content prose">
                <div className="report-introduction">
                    <h1>{data.introduction.title}</h1>
                    <p>{data.introduction.content}</p>
                </div>

                {data.modules.map(module => (
                    <section key={module.id} className="module-section">
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
            </div>
        </div>
    );
};

export default TransactionView;