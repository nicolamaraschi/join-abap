import React from 'react';

const WelcomeView = ({ 
    allTables = [], 
    isBapiMode, 
    isPresetMode, 
    isCdsMode, 
    isCdsPresetMode,
    isAbapDocMode, 
    isBadiMode, 
    isSmartformMode, 
    isAdobeformMode,
    isFioriPresetMode,
    onSelectMode 
}) => {
    
    const getWelcomeMessage = () => {
        if (isPresetMode) return "Seleziona un preset di codice dalla lista per visualizzarlo e copiarlo.";
        if (isCdsMode) return "Esplora la documentazione completa sulle Core Data Services di SAP.";
        if (isCdsPresetMode) return "Esempi pratici di codice e pattern per le CDS Views.";
        if (isBapiMode) return "Seleziona un modulo per visualizzare le BAPI disponibili ufficiali SAP.";
        if (isAbapDocMode) return "Approfondisci la tua conoscenza di ABAP con la nostra guida tecnica completa.";
        if (isBadiMode) return "Esplora i Business Add-Ins (BADI) disponibili per estendere il sistema.";
        if (isSmartformMode) return "Consulta la documentazione e gli esempi per SAP Smartforms.";
        if (isAdobeformMode) return "Documentazione dettagliata per lo sviluppo di Adobe Forms in SAP.";
        if (isFioriPresetMode) return "Snippet pronti all'uso per lo sviluppo di interfacce Fiori.";
        return "Benvenuto nel tuo portale di esplorazione SAP. Scegli una funzione qui sotto per iniziare.";
    };

    const sectionImages = {
        bapi: '/welcome_bapi.png',
        cds: '/welcome_cds.png',
        abap: '/welcome_abap.png',
        transactions: '/welcome_transactions.png',
        fiori: '/welcome_fiori.png',
        tables: '/welcome_tables.png',
        smartforms: '/welcome_smartforms.png',
        adobeforms: '/welcome_adobeforms.png',
        badi: '/welcome_badi.png',
        generic: '/logo3.png'
    };

    const dashboardCards = [
        { id: 'TABLES', title: 'Tabelle & Joins', description: 'Esplora il dizionario dati e le relazioni SAP.', img: sectionImages.tables, color: '#2563eb' },
        { id: 'BAPIS', title: 'BAPI Explorer', description: 'Business API per integrazione e automazione.', img: sectionImages.bapi, color: '#059669' },
        { id: 'CDS', title: 'CDS Views', description: 'Modellazione dati moderna in S/4HANA.', img: sectionImages.cds, color: '#7c3aed' },
        { id: 'TRANSACTIONS', title: 'Transazioni SAP', description: 'Guida rapida alle transazioni per modulo.', img: sectionImages.transactions, color: '#d97706' },
        { id: 'ABAP_DOC', title: 'ABAP Knowledge', description: 'Best practice e guida allo sviluppo ABAP.', img: sectionImages.abap, color: '#db2777' },
        { id: 'FIORI_PRESETS', title: 'Fiori Elements', description: 'Esempi pronti per interfacce Fiori.', img: sectionImages.fiori, color: '#0891b2' }
    ];

    const isMainWelcome = !isBapiMode && !isPresetMode && !isCdsMode && !isCdsPresetMode && !isAbapDocMode && !isBadiMode && !isSmartformMode && !isAdobeformMode && !isFioriPresetMode;

    // Determina l'immagine della sezione corrente
    let currentSectionImg = sectionImages.generic;
    if (isBapiMode) currentSectionImg = sectionImages.bapi;
    else if (isBadiMode) currentSectionImg = sectionImages.badi;
    else if (isCdsMode || isCdsPresetMode) currentSectionImg = sectionImages.cds;
    else if (isAbapDocMode) currentSectionImg = sectionImages.abap;
    else if (isPresetMode) currentSectionImg = sectionImages.abap; // Fallback to abap for generic presets
    else if (isFioriPresetMode) currentSectionImg = sectionImages.fiori;
    else if (isSmartformMode) currentSectionImg = sectionImages.smartforms;
    else if (isAdobeformMode) currentSectionImg = sectionImages.adobeforms;

    return (
        <div className="welcome-view" style={{ padding: '2rem', animation: 'fadeIn 0.5s ease-out' }}>
            <div style={{ maxWidth: '72rem', margin: 'auto', width: '100%' }}>
                
                <div style={{ textAlign: 'center', marginBottom: '3rem' }}>
                    <h2 style={{ fontSize: '2.5rem', fontWeight: '800', color: '#1e293b', marginBottom: '1rem' }}>
                        {isMainWelcome ? "Benvenuto nell'Explorer" : "Esplora sezione"}
                    </h2>
                    <p style={{ fontSize: '1.25rem', color: '#64748b', maxWidth: '42rem', margin: 'auto' }}>
                        {getWelcomeMessage()}
                    </p>
                </div>

                {isMainWelcome ? (
                    /* DASHBOARD HOME PAGE */
                    <div style={{ display: 'grid', gridTemplateColumns: 'repeat(auto-fill, minmax(300px, 1fr))', gap: '2rem' }}>
                        {dashboardCards.map((card) => (
                            <div 
                                key={card.id}
                                onClick={() => onSelectMode(card.id)}
                                style={{
                                    backgroundColor: 'white',
                                    borderRadius: '1.25rem',
                                    overflow: 'hidden',
                                    boxShadow: '0 4px 6px -1px rgba(0, 0, 0, 0.1)',
                                    transition: 'all 0.3s ease',
                                    cursor: 'pointer',
                                    border: '1px solid #e2e8f0',
                                    display: 'flex',
                                    flexDirection: 'column'
                                }}
                                onMouseEnter={(e) => {
                                    e.currentTarget.style.transform = 'translateY(-8px)';
                                    e.currentTarget.style.borderColor = card.color;
                                }}
                                onMouseLeave={(e) => {
                                    e.currentTarget.style.transform = 'translateY(0)';
                                    e.currentTarget.style.borderColor = '#e2e8f0';
                                }}
                            >
                                <div style={{ height: '180px', overflow: 'hidden' }}>
                                    <img src={card.img} alt={card.title} style={{ width: '100%', height: '100%', objectFit: 'cover' }} />
                                </div>
                                <div style={{ padding: '1.5rem', flexGrow: 1 }}>
                                    <h3 style={{ fontSize: '1.25rem', fontWeight: '700', color: '#1e293b', marginBottom: '0.5rem' }}>{card.title}</h3>
                                    <p style={{ fontSize: '0.875rem', color: '#64748b' }}>{card.description}</p>
                                </div>
                            </div>
                        ))}
                    </div>
                ) : (
                    /* SPECIFIC SECTION VIEW - SHOW ONLY THE SECTION IMAGE */
                    <div style={{ textAlign: 'center', marginTop: '2rem' }}>
                        <div style={{
                            display: 'inline-block',
                            backgroundColor: 'white',
                            padding: '1rem',
                            borderRadius: '2rem',
                            boxShadow: '0 20px 25px -5px rgba(0, 0, 0, 0.1)',
                            border: '1px solid #e2e8f0'
                        }}>
                            <img 
                                src={currentSectionImg} 
                                alt="Section Preview" 
                                style={{ 
                                    maxWidth: '100%', 
                                    maxHeight: '450px', 
                                    borderRadius: '1.25rem',
                                    display: 'block',
                                    objectFit: 'contain'
                                }} 
                            />
                        </div>
                        <div style={{ marginTop: '2.5rem', color: '#64748b' }}>
                            <p style={{ fontSize: '1.1rem', fontWeight: '500' }}>
                                ← Usa il menu a sinistra per iniziare l'esplorazione
                            </p>
                            <button 
                                onClick={() => onSelectMode('TABLES')}
                                style={{
                                    marginTop: '1.5rem',
                                    padding: '0.6rem 1.2rem',
                                    borderRadius: '0.75rem',
                                    border: '1px solid #cbd5e1',
                                    background: 'white',
                                    cursor: 'pointer',
                                    fontWeight: '600',
                                    color: '#475569',
                                    transition: 'all 0.2s'
                                }}
                                onMouseEnter={(e) => e.target.style.backgroundColor = '#f8fafc'}
                                onMouseLeave={(e) => e.target.style.backgroundColor = 'white'}
                            >
                                Torna alla Dashboard
                            </button>
                        </div>
                    </div>
                )}
            </div>
        </div>
    );
};

export default WelcomeView;