import React from 'react';
import MarkdownRenderer from './MarkdownRenderer.jsx'; // Usiamo il nuovo componente

const AbapDocDetailsView = ({ doc }) => {
    if (!doc) return null;

    return (
        <div className="details-view">
            {/* Usiamo la classe "prose" per dare una bella formattazione di base al testo,
              tipica della documentazione.
            */}
            <div className="report-content prose" style={{maxWidth: '100%'}}>
                <MarkdownRenderer text={doc.content} />
            </div>
        </div>
    );
};

export default AbapDocDetailsView;