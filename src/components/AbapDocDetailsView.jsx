import React from 'react';
import MarkdownRenderer from './MarkdownRenderer.jsx';

const AbapDocDetailsView = ({ doc }) => {
    if (!doc) return null;

    // Stile per aggiungere spazio in alto per la navbar e la search bar fissa
    const viewStyle = {
        paddingTop: '144px', // 64px (Navbar) + 80px (SearchBar)
    };

    // Se il contenuto è una stringa (e ha il flag), usa il renderer Markdown.
    if (typeof doc.content === 'string' && doc.isMarkdown) {
        return (
            <div className="details-view" style={viewStyle}>
                <div className="report-content prose" style={{ maxWidth: '100%' }}>
                    <MarkdownRenderer text={doc.content} />
                </div>
            </div>
        );
    }

    // Altrimenti, se il contenuto è già un elemento React (come per GuidaAbap),
    // renderizzalo direttamente.
    if (React.isValidElement(doc.content)) {
        return (
            <div className="details-view" style={viewStyle}>
                <div className="report-content prose" style={{ maxWidth: '100%' }}>
                    {doc.content}
                </div>
            </div>
        );
    }
    
    // Fallback per il comportamento precedente (contenuto come componente)
    const DocContent = doc.content;
    return (
        <div className="details-view" style={viewStyle}>
            <div className="report-content prose" style={{ maxWidth: '100%' }}>
                <DocContent />
            </div>
        </div>
    );
};

export default AbapDocDetailsView;
