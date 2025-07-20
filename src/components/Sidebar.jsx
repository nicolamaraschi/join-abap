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
  cdsViews, // Assicurati che questa prop sia ricevuta (conterrà il documento overview da App.jsx)
  presets,
  fioriPresets, // Aggiunto
  abapDocs,
  badis,
  smartforms,
  adobeforms,
  cdsPresets, // Aggiunto
  transactionModules,
  onSelectItem,
  selectedItemName, // Aggiunto
  cdsSubMode, // Aggiunto
  onCdsSubModeChange, // Aggiunto
}) => {
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
      case 'CDS': return "Cerca in CDS..."; // Aggiornato placeholder
      case 'BADIS': return "Cerca BADI...";
      case 'SMARTFORMS': return "Cerca Smartform...";
      case 'ADOBEFORMS': return "Cerca Adobe Form...";
      case 'FIORI_PRESETS': return "Cerca preset Fiori..."; // Aggiunto
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
      case 'FIORI_PRESETS': // Aggiunto
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

        <div className="sidebar-content">
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

          {renderList()}
        </div>
      </aside>
    </>
  );
};

export default Sidebar;