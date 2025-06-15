// src/components/AbapDocNavList.jsx
import React from 'react';

const AbapDocNavList = ({ sections }) => {

    const handleNavigation = (e, sectionId) => {
        e.preventDefault();
        const element = document.getElementById(`section-${sectionId}`);
        if (element) {
            element.scrollIntoView({
                behavior: "smooth",
                block: "start"
            });
        }
    };

    if (!sections || sections.length === 0) {
        return <div style={{textAlign: 'center', color: '#64748b', padding: '1rem'}}>Nessuna sezione.</div>;
    }

    return (
        <ul className="transaction-nav-list">
            <li className="submodule-header" style={{ marginTop: 0 }}>Naviga Documentazione</li>
            {sections.map(section => (
                <li key={section.id}>
                    <a
                        href={`#section-${section.id}`}
                        onClick={(e) => handleNavigation(e, section.id)}
                        className="transaction-nav-link"
                    >
                        {section.title}
                    </a>
                </li>
            ))}
        </ul>
    );
};

export default AbapDocNavList;