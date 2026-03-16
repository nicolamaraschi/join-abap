// src/components/Sidebar.jsx
import React, { useState, useEffect } from 'react';
import BapiList from './BapiList.jsx';
import TableList from './TableList.jsx';
import PresetList from './PresetList.jsx';
import TransactionNavList from './TransactionNavList.jsx';
import AbapDocList from './AbapDocList.jsx';
import BadiList from './BadiList.jsx';
// Importa i nuovi componenti lista per Smartforms e Adobe Forms
import SmartformList from './SmartformList.jsx';
import AdobeformList from './AdobeformList.jsx';
import CdsList from './CdsList.jsx'; // <--- Importa CdsList
import './Sidebar.css'; // se usi file CSS separato
const Sidebar = ({
  viewMode,
  searchTerm,
  onSearchChange,
  tables,
  bapis,
  cdsViews,
  presets,
  fioriPresets,
  abapDocs,
  badis,
  smartforms,
  adobeforms,
  cdsPresets,
  transactionModules,
  onSelectItem,
  selectedItemName,
  cdsSubMode,
  onCdsSubModeChange,
  onTransactionSelect,
  onViewModeSelect,
  isOpen,
  onClose,
}) => {
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
      case 'CDS': return "Cerca in CDS...";
      case 'BADIS': return "Cerca BADI...";
      case 'SMARTFORMS': return "Cerca Smartform...";
      case 'ADOBEFORMS': return "Cerca Adobe Form...";
      case 'FIORI_PRESETS': return "Cerca preset Fiori...";
      default: return "Cerca...";
    }
  };

  const renderList = () => {
    switch (viewMode) {
      case 'TABLES':
        return <TableList tables={tables} onSelectTable={onSelectItem} selectedTable={selectedItemName} />;
      case 'BAPIS':
        return <BapiList bapis={bapis} onSelectBapi={onSelectItem} selectedBapi={selectedItemName} />;
      case 'CDS':
        return (
          <>
            <div className="cds-view-selector">
              <button
                onClick={() => onCdsSubModeChange('docs')}
                className={cdsSubMode === 'docs' ? 'active' : ''}
              >
                Documentazione
              </button>
              <button
                onClick={() => onCdsSubModeChange('presets')}
                className={cdsSubMode === 'presets' ? 'active' : ''}
              >
                Preset Codice
              </button>
            </div>
            {cdsSubMode === 'docs' ? (
              <CdsList cdsDocs={cdsViews} onSelectCdsDoc={onSelectItem} selectedCdsDoc={selectedItemName} />
            ) : (
              <PresetList presets={cdsPresets} onSelectPreset={onSelectItem} selectedPreset={selectedItemName} />
            )}
          </>
        );
      case 'PRESETS':
        return <PresetList presets={presets} onSelectPreset={onSelectItem} selectedPreset={selectedItemName} />;
      case 'FIORI_PRESETS':
        return <PresetList presets={fioriPresets} onSelectPreset={onSelectItem} selectedPreset={selectedItemName} />;
      case 'ABAP_DOC':
        return <AbapDocList docs={abapDocs} onSelectDoc={onSelectItem} selectedDoc={selectedItemName} />;
      case 'BADIS':
        return <BadiList badis={badis} onSelectBadi={onSelectItem} selectedBadi={selectedItemName} />;
      case 'SMARTFORMS':
        return <SmartformList smartforms={smartforms} onSelectSmartform={onSelectItem} selectedSmartform={selectedItemName} />;
      case 'ADOBEFORMS':
        return <AdobeformList adobeforms={adobeforms} onSelectAdobeform={onSelectItem} selectedAdobeform={selectedItemName} />;
      case 'TRANSACTIONS':
        return <TransactionNavList modules={transactionModules} onSelect={onTransactionSelect} />;
      default:
        return null;
    }
  };

  return (
    <>
      {/* Overlay su mobile */}
      {isMobile && <div className={`mobile-overlay ${isOpen ? 'active' : ''}`} onClick={onClose}></div>}

      <aside className={`sidebar ${isMobile ? (isOpen ? 'open' : 'closed') : ''}`}>
        {isMobile && (
          <button className="close-btn" onClick={onClose} aria-label="Close Sidebar">
            ×
          </button>
        )}

        <div className="sidebar-content">
          {isMobile && (
            <div className="mobile-mode-selector">
              <div className="sidebar-section-title">NAVIGAZIONE</div>
              <div className="mode-pills-grid">
                {[
                  { id: 'TABLES', label: 'Tabelle', icon: <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2"><path d="M3 3h18v18H3zM3 9h18M3 15h18M9 3v18M15 3v18"/></svg> },
                  { id: 'BAPIS', label: 'BAPI', icon: <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2"><path d="M22 12h-4l-3 9L9 3l-3 9H2"/></svg> },
                  { id: 'CDS', label: 'CDS', icon: <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2"><path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"/></svg> },
                  { id: 'TRANSACTIONS', label: 'Transazioni', icon: <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2"><path d="M12 20V10M18 20V4M6 20v-4"/></svg> },
                  { id: 'PRESETS', label: 'Preset', icon: <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2"><path d="M16 18l6-6-6-6M8 6l-6 6 6 6M12 4v16"/></svg> },
                  { id: 'ABAP_DOC', label: 'Doc', icon: <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2"><path d="M4 19.5A2.5 2.5 0 0 1 6.5 17H20M4 4.5A2.5 2.5 0 0 1 6.5 2H20v20H6.5a2.5 2.5 0 0 1-2.5-2.5v-15Z"/></svg> },
                  { id: 'BADIS', label: 'BADI', icon: <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2"><path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"/><polyline points="3.27 6.96 12 12.01 20.73 6.96"/><line x1="12" y1="22.08" x2="12" y2="12"/></svg> },
                  { id: 'SMARTFORMS', label: 'Smartforms', icon: <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2"><path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"/><polyline points="14 2 14 8 20 8"/><line x1="16" y1="13" x2="8" y2="13"/><line x1="16" y1="17" x2="8" y2="17"/><polyline points="10 9 9 9 8 9"/></svg> },
                  { id: 'ADOBEFORMS', label: 'Adobe', icon: <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2"><path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"/><polyline points="14 2 14 8 20 8"/><path d="M9 15h3a2 2 0 0 0 0-4H9v4Z"/><path d="m11 15 3 3"/></svg> },
                  { id: 'FIORI_PRESETS', label: 'Fiori', icon: <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2"><path d="M13 2L3 14h9l-1 8 10-12h-9l1-8z"/></svg> }
                ].map(mode => (
                  <button 
                    key={mode.id}
                    className={`mode-pill ${viewMode === mode.id ? 'active' : ''}`}
                    onClick={() => {
                      onViewModeSelect(mode.id);
                      if (isMobile) onClose();
                    }}
                  >
                    {mode.icon}
                    <span>{mode.label}</span>
                  </button>
                ))}
              </div>
            </div>
          )}

          <div className="search-container">
            <input
              type="text"
              placeholder={getPlaceholder()}
              className="search-input"
              value={searchTerm}
              onChange={(e) => onSearchChange(e.target.value)}
            />
            <svg xmlns="http://www.w3.org/2000/svg" className="search-icon" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
            </svg>
          </div>

          <div className="sidebar-section-title">{viewMode === 'TRANSACTIONS' ? 'MODULI' : 'ELEMENTI'}</div>
          {renderList()}
        </div>
      </aside>
    </>
  );
};

export default Sidebar;