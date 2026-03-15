import React from 'react';

const SearchBar = ({ searchQuery, onSearchChange, results, onResultClick }) => {
  const [isFocused, setIsFocused] = React.useState(false);

  const showResults = isFocused && searchQuery.length > 0;

  return (
    <div className="search-bar-wrapper" style={{
      marginBottom: '2rem',
      position: 'relative',
      zIndex: 100,
    }}>
      <div 
        className={`search-input-container ${isFocused ? 'focused' : ''}`}
        onFocus={() => setIsFocused(true)}
        onBlur={() => setTimeout(() => setIsFocused(false), 200)}
        style={{
          display: 'flex',
          alignItems: 'center',
          backgroundColor: 'white',
          borderRadius: '12px',
          padding: '4px 16px',
          border: '1px solid #e2e8f0',
          transition: 'all 0.3s cubic-bezier(0.4, 0, 0.2, 1)',
          boxShadow: isFocused ? '0 10px 25px -5px rgba(59, 130, 246, 0.1), 0 8px 10px -6px rgba(59, 130, 246, 0.1)' : '0 1px 2px 0 rgba(0, 0, 0, 0.05)',
          maxWidth: '800px',
          margin: '0 auto',
          position: 'relative'
        }}
      >
        <svg 
          width="20" 
          height="20" 
          viewBox="0 0 24 24" 
          fill="none" 
          stroke={isFocused ? "#3b82f6" : "#94a3b8"} 
          strokeWidth="2.5" 
          strokeLinecap="round" 
          strokeLinejoin="round"
          style={{ transition: 'stroke 0.3s ease' }}
        >
          <circle cx="11" cy="11" r="8"></circle>
          <line x1="21" y1="21" x2="16.65" y2="16.65"></line>
        </svg>
        
        <input
          type="text"
          value={searchQuery}
          onChange={onSearchChange}
          placeholder="Cerca un argomento o una sezione..."
          style={{
            flex: 1,
            border: 'none',
            padding: '12px 12px',
            fontSize: '1rem',
            outline: 'none',
            color: '#1e293b',
            backgroundColor: 'transparent',
            fontWeight: '400'
          }}
        />
        
        {searchQuery && (
          <button 
            onClick={() => onSearchChange({ target: { value: '' } })}
            style={{
              padding: '4px',
              borderRadius: '50%',
              border: 'none',
              background: '#f1f5f9',
              color: '#64748b',
              cursor: 'pointer',
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center'
            }}
          >
            <svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="3">
              <line x1="18" y1="6" x2="6" y2="18"></line>
              <line x1="6" y1="6" x2="18" y2="18"></line>
            </svg>
          </button>
        )}

        {/* Dropdown dei risultati */}
        {showResults && (
          <div className="search-results-dropdown" style={{
            position: 'absolute',
            top: 'calc(100% + 8px)',
            left: 0,
            right: 0,
            backgroundColor: 'white',
            borderRadius: '12px',
            boxShadow: '0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04)',
            border: '1px solid #e2e8f0',
            overflow: 'hidden',
            maxHeight: '400px',
            overflowY: 'auto',
          }}>
            <ul style={{ listStyle: 'none', padding: 0, margin: 0 }}>
              {results.length > 0 ? (
                results.map((result) => (
                  <li
                    key={result.id}
                    onClick={() => {
                      onResultClick(result.id);
                      setIsFocused(false);
                    }}
                    className="search-result-item"
                    style={{
                      padding: '12px 16px',
                      cursor: 'pointer',
                      borderBottom: '1px solid #f1f5f9',
                      transition: 'all 0.2s ease',
                      display: 'flex',
                      flexDirection: 'column',
                      gap: '4px'
                    }}
                  >
                    <div style={{ fontWeight: '600', color: '#0f172a', fontSize: '0.938rem' }}>
                      {result.title}
                    </div>
                    {result.description && (
                      <div style={{ fontSize: '0.813rem', color: '#64748b' }}>
                        {result.description}
                      </div>
                    )}
                  </li>
                ))
              ) : (
                <li style={{ padding: '20px', color: '#94a3b8', textAlign: 'center', fontSize: '0.875rem' }}>
                  Nessun argomento trovato per "{searchQuery}"
                </li>
              )}
            </ul>
          </div>
        )}
      </div>

      <style dangerouslySetInnerHTML={{ __html: `
        .search-input-container.focused {
          border-color: #3b82f6 !important;
          box-shadow: 0 0 0 4px rgba(59, 130, 246, 0.1) !important;
        }
        .search-result-item:hover {
          background-color: #f8fafc !important;
          padding-left: 20px !important;
        }
        .search-result-item:last-child {
          border-bottom: none;
        }
      `}} />
    </div>
  );
};

export default SearchBar;