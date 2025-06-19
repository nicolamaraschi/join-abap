import React, { useState, useEffect } from 'react';
import BapiList from './BapiList.jsx';
import TableList from './TableList.jsx';
import PresetList from './PresetList.jsx';
import TransactionNavList from './TransactionNavList.jsx';
import AbapDocList from './AbapDocList.jsx';
import './Sidebar.css'; // se usi file CSS separato

const Sidebar = ({ viewMode, searchTerm, onSearchChange, tables, bapis, presets, abapDocs, transactionModules, onSelectItem }) => {
  const [isMobileOpen, setIsMobileOpen] = useState(false);
  const [isMobile, setIsMobile] = useState(false);

  // Detect if we are on mobile screen
  useEffect(() => {
    const checkMobile = () => setIsMobile(window.innerWidth <= 768);
    checkMobile();
    window.addEventListener('resize', checkMobile);
    return () => window.removeEventListener('resize', checkMobile);
  }, []);

  const getPlaceholder = () => {
    switch(viewMode) {
      case 'TABLES': return "Cerca tabella...";
      case 'BAPIS': return "Cerca BAPI...";
      case 'PRESETS': return "Cerca preset...";
      case 'ABAP_DOC': return "Cerca documentazione...";
      case 'TRANSACTIONS': return "Navigazione Report Transazioni";
      case 'CDS_DOCS': return "Documentazione CDS"; // AGGIUNGI QUESTO
      default: return "Cerca...";
    }
  };

  const renderList = () => {
    switch (viewMode) {
      case 'TABLES':
        return <TableList tables={tables} onSelectTable={onSelectItem} />;
      case 'BAPIS':
        return <BapiList bapis={bapis} onSelectBapi={onSelectItem} />;
        case 'CDS_DOCS': // AGGIUNGI QUESTO
        return <div style={{textAlign: 'center', color: '#64748b', padding: '1rem'}}>Naviga la documentazione a destra.</div>;
      case 'PRESETS':
        return <PresetList presets={presets} onSelectPreset={onSelectItem} />;
      case 'ABAP_DOC':
        return <AbapDocList docs={abapDocs} onSelectDoc={onSelectItem} />;
      case 'TRANSACTIONS':
        return <TransactionNavList modules={transactionModules} />;
      default:
        return null;
    }
  };

  return (
    <>
      {/* Hamburger menu solo su mobile */}
      {isMobile && (
        <button className="hamburger" onClick={() => setIsMobileOpen(true)}>
          ☰
        </button>
      )}

      <aside className={`sidebar ${isMobile ? (isMobileOpen ? 'open' : 'closed') : ''}`}>
        {isMobile && (
          <button className="close-btn" onClick={() => setIsMobileOpen(false)}>
            ×
          </button>
        )}

        <div className="search-container">
          <input 
            type="text" 
            placeholder={getPlaceholder()} 
            className="search-input" 
            value={searchTerm} 
            onChange={(e) => onSearchChange(e.target.value)}
            disabled={viewMode === 'TRANSACTIONS'}
          />
          <svg xmlns="http://www.w3.org/2000/svg" className="search-icon" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
          </svg>
        </div>

        {renderList()}
      </aside>
    </>
  );
};

export default Sidebar;
