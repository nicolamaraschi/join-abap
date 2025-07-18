import React from 'react';
import MarkdownRenderer from './MarkdownRenderer.jsx';

const AbapDocDetailsView = ({ doc }) => {
    if (!doc) return null;

    // Se il documento ha il flag isMarkdown, usa il renderer
    if (doc.isMarkdown) {
        return (
            <div className="details-view">
                <div className="report-content prose" style={{ maxWidth: '100%' }}>
                    <MarkdownRenderer text={doc.content} />
                </div>
            </div>
        );
    }

    // Altrimenti, renderizza il contenuto come componente React (comportamento precedente)
    const DocContent = doc.content;
    return (
        <div className="details-view">
            <div className="report-content prose" style={{ maxWidth: '100%' }}>
                <DocContent />
            </div>
        </div>
    );
};

export default AbapDocDetailsView;
