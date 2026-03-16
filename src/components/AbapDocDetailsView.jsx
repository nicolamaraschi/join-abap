import React from 'react';
import IndexedContent from './IndexedContent.jsx';

const AbapDocDetailsView = ({ doc }) => {
    if (!doc) return null;

    // Rimosso il padding fisso orribile, ora il layout è gestito correttamente
    const viewStyle = {
        padding: '2rem',
    };

    // Se il contenuto è una stringa (e ha il flag), usa il renderer Markdown.
    if (typeof doc.content === 'string' && doc.isMarkdown) {
        return (
            <div className="details-view" style={viewStyle}>
                <IndexedContent text={doc.content} />
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
