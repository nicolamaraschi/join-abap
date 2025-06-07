import React from 'react';

const Navbar = ({ modules, currentModule, onModuleSelect }) => {
    return (
        <nav className="navbar">
            <div className="navbar-brand">
                SAP Schema Explorer
            </div>
            <div className="navbar-filters">
                {modules.map(module => (
                    <button 
                        key={module} 
                        onClick={() => onModuleSelect(module)} 
                        className={currentModule === module ? 'active' : 'inactive'}
                    >
                        {module}
                    </button>
                ))}
            </div>
        </nav>
    );
};

export default Navbar;