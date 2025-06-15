// src/components/Navbar.jsx
import React from 'react';

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
    const handleModuleChange = (e) => {
        onModuleSelect(e.target.value);
    };

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
                    <button onClick={() => onViewModeSelect('TRANSACTIONS')} className={viewMode === 'TRANSACTIONS' ? 'active' : 'inactive'}>
                        Transazioni
                    </button>
                    <button onClick={() => onViewModeSelect('ABAP_DOCS')} className={viewMode === 'ABAP_DOCS' ? 'active' : 'inactive'}>
                        Doc. ABAP
                    </button>
                    <button onClick={() => onViewModeSelect('PRESETS')} className={viewMode === 'PRESETS' ? 'active' : 'inactive'}>
                        Preset Codice
                    </button>
                </div>
            </nav>

            {/* 2. BARRA DEI FILTRI: APPARE CONTESTUALMENTE */}
            {(viewMode === 'TABLES' || viewMode === 'BAPIS') && (
                <div className="filter-bar">
                    {/* Filtro per Modulo */}
                    <div className="filter-group">
                        <label htmlFor="module-select">Modulo</label>
                        <select id="module-select" value={currentModule} onChange={handleModuleChange} className="filter-select">
                            {(modules || []).map(moduleKey => (
                                <option key={moduleKey} value={moduleKey}>
                                    {moduleNames[moduleKey] || moduleKey}
                                </option>
                            ))}
                        </select>
                    </div>

                    {/* Filtro per Sottogruppo (solo per le Tabelle) */}
                    {viewMode === 'TABLES' && subgroups && subgroups.length > 0 && (
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