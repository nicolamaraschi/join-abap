import React from 'react';

const TransactionNavList = ({ modules }) => {

    const handleNavigation = (e, moduleId) => {
        e.preventDefault();
        // Trova l'elemento della sezione a cui navigare
        const element = document.getElementById(`module-section-${moduleId}`);
        if (element) {
            // Usa il metodo moderno e affidabile per scorrere fino all'elemento
            element.scrollIntoView({
                behavior: "smooth",
                block: "start"
            });
        }
    };

    if (!modules || modules.length === 0) {
        return <div style={{textAlign: 'center', color: '#64748b', padding: '1rem'}}>Nessun modulo da navigare.</div>;
    }

    return (
        <ul className="transaction-nav-list">
            <li className="submodule-header" style={{ marginTop: 0 }}>Naviga Report</li>
            {modules.map(module => (
                <li key={module.id}>
                    <a 
                        href={`#module-section-${module.id}`} 
                        onClick={(e) => handleNavigation(e, module.id)}
                        className="transaction-nav-link"
                    >
                        {module.name}
                    </a>
                </li>
            ))}
        </ul>
    );
};

export default TransactionNavList;