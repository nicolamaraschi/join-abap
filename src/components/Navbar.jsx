import React, { useEffect } from 'react';

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
    // Gestisce il padding del body in base alla presenza della filter-bar
    useEffect(() => {
        // Aggiungi classe quando la filter-bar non è visibile
        if (viewMode !== 'TABLES' && viewMode !== 'BAPIS') {
            document.body.classList.add('navbar-simple');
        } else {
            document.body.classList.remove('navbar-simple');
        }
        
        // Cleanup quando il componente viene smontato
        return () => {
            document.body.classList.remove('navbar-simple');
        };
    }, [viewMode]);

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
                    <button onClick={() => onViewModeSelect('TRANSACTIONS')} className={viewMode === 'TRANSACTIONS' ? 'active' : 'inactive'}>
                        Transazioni
                    </button>
                    <button onClick={() => onViewModeSelect('PRESETS')} className={viewMode === 'PRESETS' ? 'active' : 'inactive'}>
                        Preset Codice
                    </button>
                </div>
            </nav>

            {/* 2. BARRA DEI FILTRI CON NUOVO STILE "AURORA" */}
            {(viewMode === 'TABLES' || viewMode === 'BAPIS') && (
                <div className="filter-bar">
                    {/* Filtro per Modulo */}
                    <div className="filter-group-aurora">
                        <select id="module-select" value={currentModule} onChange={handleModuleChange} className="filter-select-aurora">
                            {(modules || []).map(moduleKey => (
                                <option key={moduleKey} value={moduleKey}>
                                    {moduleNames[moduleKey] || moduleKey}
                                </option>
                            ))}
                        </select>
                        <label htmlFor="module-select">Modulo</label>
                    </div>

                    {/* Filtro per Sottogruppo (solo per le Tabelle) */}
                    {viewMode === 'TABLES' && subgroups && subgroups.length > 0 && (
                        <div className="filter-group-aurora">
                            <select id="subgroup-select" value={selectedSubgroup} onChange={handleSubgroupChange} className="filter-select-aurora" disabled={currentModule === 'All'}>
                                <option value="All">Tutti</option>
                                {subgroups.map(subgroup => (
                                    <option key={subgroup} value={subgroup}>
                                        {subgroup}
                                    </option>
                                ))}
                            </select>
                            <label htmlFor="subgroup-select">Sottogruppo</label>
                        </div>
                    )}
                </div>
            )}
        </header>
    );
};

export default Navbar;