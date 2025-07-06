import React from 'react';

const TransactionNavList = ({ modules }) => {

    const handleNavigation = (e, transactionId) => {
        e.preventDefault();
        // Trova l'elemento della sezione a cui navigare
        const element = document.getElementById(`transaction-section-${transactionId}`);
        if (element) {
            // Usa il metodo moderno e affidabile per scorrere fino all'elemento
            element.scrollIntoView({
                behavior: "smooth",
                block: "start"
            });
        }
    };

    if (!modules || modules.length === 0) {
        return <div style={{textAlign: 'center', color: '#64748b', padding: '1rem'}}>Nessuna transazione trovata.</div>;
    }

    return (
        <ul className="transaction-nav-list">
            <li className="submodule-header" style={{ marginTop: 0 }}>Transazioni Trovate</li>
            {modules.map(transaction => (
                <li key={transaction.id}>
                    <a 
                        href={`#transaction-section-${transaction.id}`} 
                        onClick={(e) => handleNavigation(e, transaction.id)}
                        className="transaction-nav-link"
                    >
                        {transaction.name}
                    </a>
                </li>
            ))}
        </ul>
    );
};

export default TransactionNavList;