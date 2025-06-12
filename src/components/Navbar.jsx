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
    return (
        <header className="navbar-container">
            {/* 1. BARRA PRINCIPALE: Switch tra Tabelle e BAPI */}
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
                </div>
            </nav>

            {/* 2. BARRA SECONDARIA: Selezione del Modulo (FI, SD, etc.) */}
            <nav className="sub-navbar">
                {(modules || []).map(moduleKey => (
                    <button
                        key={moduleKey}
                        onClick={() => onModuleSelect(moduleKey)}
                        className={currentModule === moduleKey ? 'active' : ''}
                    >
                        {moduleNames[moduleKey] || moduleKey}
                    </button>
                ))}
            </nav>
            
            {/* 3. BARRA TERZIARIA: Selezione del Sottogruppo (solo per le tabelle) */}
            {viewMode === 'TABLES' && subgroups && subgroups.length > 1 && (
                <nav className="sub-navbar" style={{ backgroundColor: '#fff', borderTop: '1px solid #e2e8f0' }}>
                    <button 
                        onClick={() => onSubgroupSelect('All')}
                        className={selectedSubgroup === 'All' ? 'active' : ''}
                    >
                        Tutti
                    </button>
                    {subgroups.map(subgroup => (
                        <button
                            key={subgroup}
                            onClick={() => onSubgroupSelect(subgroup)}
                            className={selectedSubgroup === subgroup ? 'active' : ''}
                        >
                            {subgroup}
                        </button>
                    ))}
                </nav>
            )}
        </header>
    );
};

export default Navbar;