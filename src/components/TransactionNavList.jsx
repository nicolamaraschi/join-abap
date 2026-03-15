import React from 'react';

const TransactionNavList = ({ modules, onSelect }) => {

    const handleNavigation = (e, transaction) => {
        e.preventDefault();
        if (onSelect) {
            onSelect(transaction);
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
                        onClick={(e) => handleNavigation(e, transaction)}
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