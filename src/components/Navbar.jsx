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
            {/* AGGIORNA LA CONDIZIONE PER INCLUDERE SMARTFORMS E ADOBE FORMS */}
            {(viewMode === 'TABLES' || viewMode === 'BAPIS' || viewMode === 'CDS' || viewMode === 'ABAP_DOC' || viewMode === 'BADIS' || viewMode === 'SMARTFORMS' || viewMode === 'ADOBEFORMS' || viewMode === 'FIORI_PRESETS') && (
                <div className="filter-bar">
                    {/* Filtro per Modulo */}
                    <div className="filter-group">
                        <label htmlFor="module-select">Modulo</label>
                        <select
                            id="module-select"
                            value={currentModule}
                            onChange={handleModuleChange}
                            className="filter-select"
                            disabled={viewMode === 'ABAP_DOC'} // ABAP_DOC è l'unico che non ha moduli nel dropdown
                        >
                            {(modules || []).map(moduleKey => (
                                <option key={moduleKey} value={moduleKey}>
                                    {moduleNames[moduleKey] || moduleKey}
                                </option>
                            ))}
                        </select>
                    </div>

                    {/* Filtro per Sottogruppo (solo per le Tabelle e CDS Views) */}
                    {(viewMode === 'TABLES' || viewMode === 'CDS') && subgroups && subgroups.length > 0 && (
                        <div className="filter-group">
                            <label htmlFor="subgroup-select">Sottogruppo</label>
                            <select id="subgroup-select" value={selectedSubgroup} onChange={handleSubgroupChange} className="filter-select" disabled={currentModule === 'All'}>
                                <option value="All">Tutti</option>
                                {subgroups.map(subgroup => (
                                    <option key={subgroup} value={subgroup}>
                                        {subgroup}
                                    </option>
                                ))}
                            </select>
                        </div>
                    )}
                </div>
            )}
        </header>
    );
};

export default Navbar;