// src/components/WelcomeView.jsx
import React from 'react'; // Rimosse le importazioni di Chart e hooks correlati a chart

const WelcomeView = ({ allTables = [], isBapiMode, isPresetMode, isCdsMode }) => {
    // Rimosso chartRef, chartInstance, chartData e l'useEffect correlato ai grafici

    const getWelcomeMessage = () => {
        if (isPresetMode) return "Seleziona un preset di codice dalla lista per visualizzarlo e copiarlo.";
        if (isCdsMode) return "Esplora la documentazione completa sulle Core Data Services di SAP.";
        if (isBapiMode) return "Seleziona un modulo per visualizzare le BAPI disponibili.";
        // Messaggio di benvenuto aggiornato dato che il grafico non c'è più
        return "Seleziona un modulo e una sottocategoria per visualizzare le tabelle o esplora le immagini qui sotto.";
    };

    return (
        <div className="welcome-view">
             <div style={{maxWidth: '56rem', margin: 'auto', width: '100%'}}>
                <h2 style={{fontSize: '1.875rem', fontWeight: '700', marginBottom: '0.5rem'}}>Benvenuto nell'Explorer</h2>
                <p style={{fontSize: '1.125rem', color: '#64748b', marginBottom: '1.5rem'}}>
                    {getWelcomeMessage()}
                </p>

                {/* Contenitore principale per le immagini */}
                <div style={{backgroundColor: '#f8fafc', padding: '1.5rem', borderRadius: '0.5rem', boxShadow: '0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)', minHeight: '200px', display: 'flex', flexDirection: 'column', alignItems: 'center', justifyContent: 'center'}}>

                    {/* Sezione per le immagini - la mostriamo sempre a meno che non sia una modalità specifica */}
                    {(!isBapiMode && !isPresetMode && !isCdsMode) && (
                        <div style={{display: 'flex', flexWrap: 'wrap', justifyContent: 'center', gap: '1.5rem', width: '100%', padding: '1rem'}}>
                            {/* Immagini dalla cartella public - Ingrandite e con ombra migliorata */}
                            <img src="/abap2.png" alt="ABAP 2" style={{maxWidth: '250px', height: 'auto', borderRadius: '8px', boxShadow: '0 4px 8px rgba(0,0,0,0.1)'}} />
                            <img src="/bapi.jpg" alt="BAPI" style={{maxWidth: '250px', height: 'auto', borderRadius: '8px', boxShadow: '0 4px 8px rgba(0,0,0,0.1)'}} /> 
                            <img src="/join2.png" alt="Join 2" style={{maxWidth: '250px', height: 'auto', borderRadius: '8px', boxShadow: '0 4px 8px rgba(0,0,0,0.1)'}} />
                            <img src="/logo3.png" alt="Logo 3" style={{maxWidth: '180px', height: 'auto', borderRadius: '8px', boxShadow: '0 4px 8px rgba(0,0,0,0.1)'}} />
                            <img src="/transazioni2.png" alt="Transazioni 2" style={{maxWidth: '250px', height: 'auto', borderRadius: '8px', boxShadow: '0 4px 8px rgba(0,0,0,0.1)'}} />
                            <img src="/abap.jpg" alt="ABAP" style={{maxWidth: '250px', height: 'auto', borderRadius: '8px', boxShadow: '0 4px 8px rgba(0,0,0,0.1)'}} />
                            <img src="/join.jpg" alt="Join" style={{maxWidth: '250px', height: 'auto', borderRadius: '8px', boxShadow: '0 4px 8px rgba(0,0,0,0.1)'}} />
                            <img src="/transazioni.jpg" alt="Transazioni" style={{maxWidth: '250px', height: 'auto', borderRadius: '8px', boxShadow: '0 4px 8px rgba(0,0,0,0.1)'}} />

                            <p style={{fontSize: '0.875rem', color: '#64748b', marginTop: '1rem', width: '100%', textAlign: 'center'}}>Immagini dalla cartella public</p>
                        </div>
                    )}
                    {/* Se non siamo in modalità specifiche e allTables è vuota (e quindi non ci sono immagini per le tabelle) */}
                    {(!isBapiMode && !isPresetMode && !isCdsMode && allTables.length === 0) && (
                        <p style={{color: '#64748b'}}>Nessuna tabella disponibile. Visualizza le immagini di benvenuto.</p>
                    )}
                </div>
            </div>
        </div>
    );
};

export default WelcomeView;