import React from 'react';
import './Navbar.css'; // Se hai un file CSS specifico per Navbar, mantienilo

const Navbar = ({
    viewMode,
    onViewModeSelect,
    modules,
    moduleNames,
    currentModule,
    onModuleSelect,
    subgroups,
    selectedSubgroup,
    onSubgroupSelect
}) => {

    // Gestisce il cambio del modulo dal menu a tendina
    const handleModuleChange = (e) => {
        onModuleSelect(e.target.value);
    };

    // Gestisce il cambio del sottogruppo dal menu a tendina
    const handleSubgroupChange = (e) => {
        onSubgroupSelect(e.target.value);
    };

    return (
        <header className="navbar-container">
            {/* 1. BARRA PRINCIPALE: SEMPRE VISIBILE */}
            <nav className="navbar">
                <div className="navbar-brand">
                    SAP Explorer
                </div>
                <div className="main-nav-filters">
                    <button onClick={() => onViewModeSelect('TABLES')} className={viewMode === 'TABLES' ? 'active' : 'inactive'}>
                        Tabelle
                    </button>
                    <button onClick={() => onViewModeSelect('BAPIS')} className={viewMode === 'BAPIS' ? 'active' : 'inactive'}>
                        BAPI
                    </button>
                    <button onClick={() => onViewModeSelect('CDS')} className={viewMode === 'CDS' ? 'active' : 'inactive'}>
                        CDS Views
                    </button>
                    <button onClick={() => onViewModeSelect('TRANSACTIONS')} className={viewMode === 'TRANSACTIONS' ? 'active' : 'inactive'}>
                        Transazioni
                    </button>
                    <button onClick={() => onViewModeSelect('PRESETS')} className={viewMode === 'PRESETS' ? 'active' : 'inactive'}>
                        Preset Codice
                    </button>
                    <button onClick={() => onViewModeSelect('ABAP_DOC')} className={viewMode === 'ABAP_DOC' ? 'active' : 'inactive'}>
                        ABAP Doc
                    </button>
                    <button
                        className={`nav-button ${viewMode === 'BADIS' ? 'active' : ''}`}
                        onClick={() => onViewModeSelect('BADIS')}
                    >
                        BADI
                    </button>
                    {/* NUOVI PULSANTI PER SMARTFORMS E ADOBE FORMS */}
                    <button onClick={() => onViewModeSelect('SMARTFORMS')} className={viewMode === 'SMARTFORMS' ? 'active' : 'inactive'}>
                        Smartforms
                    </button>
                    <button onClick={() => onViewModeSelect('ADOBEFORMS')} className={viewMode === 'ADOBEFORMS' ? 'active' : 'inactive'}>
                        Adobe Forms
                    </button>
                    <button onClick={() => onViewModeSelect('FIORI_PRESETS')} className={viewMode === 'FIORI_PRESETS' ? 'active' : 'inactive'}>
                        Preset Fiori
                    </button>
                </div>
            </nav>

            {/* 2. BARRA DEI FILTRI: APPARE CONTESTUALMENTE */}
            {(viewMode === 'TABLES' || viewMode === 'BAPIS' || viewMode === 'BADIS') && (
                <div className="filter-bar">
                    <div className="filter-section">
                        <span className="filter-label">Modulo:</span>
                        <div className="pill-container">
                            {(modules || []).map(moduleKey => (
                                <button
                                    key={moduleKey}
                                    className={`pill-button ${currentModule === moduleKey ? 'active' : ''}`}
                                    onClick={() => onModuleSelect(moduleKey)}
                                >
                                    {moduleNames[moduleKey] || moduleKey}
                                </button>
                            ))}
                        </div>
                    </div>

                    {/* Filtro per Sottogruppo (solo per le Tabelle e CDS Views) */}
                    {(viewMode === 'TABLES' || viewMode === 'CDS') && subgroups && subgroups.length > 0 && currentModule !== 'All' && (
                        <div className="filter-section subgroup-row">
                            <span className="filter-label">Sottogruppo:</span>
                            <div className="pill-container">
                                <button
                                    className={`pill-button small ${selectedSubgroup === 'All' ? 'active' : ''}`}
                                    onClick={() => onSubgroupSelect('All')}
                                >
                                    Tutti
                                </button>
                                {subgroups.map(subgroup => (
                                    <button
                                        key={subgroup}
                                        className={`pill-button small ${selectedSubgroup === subgroup ? 'active' : ''}`}
                                        onClick={() => onSubgroupSelect(subgroup)}
                                    >
                                        {subgroup}
                                    </button>
                                ))}
                            </div>
                        </div>
                    )}
                </div>
            )}
        </header>
    );
};

export default Navbar;