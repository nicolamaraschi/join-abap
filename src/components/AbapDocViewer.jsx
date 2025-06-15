import React from 'react';
import ClSalvTableDoc from '../modules/doc_abap/ClSalvTableDoc';

const AbapDocViewer = ({ docId }) => {
    switch (docId) {
        case 'cl_salv_table_guide':
            return <ClSalvTableDoc />;
        // Aggiungi altri casi per future documentazioni ABAP
        default:
            return (
                <div className="p-6 text-center text-gray-600">
                    Seleziona una documentazione ABAP dalla sidebar.
                </div>
            );
    }
};

export default AbapDocViewer;
