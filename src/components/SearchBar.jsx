import React from 'react';

const SearchBar = ({ searchQuery, onSearchChange, results, onResultClick }) => {
  const [isFocused, setIsFocused] = React.useState(false);

  const showResults = isFocused && searchQuery.length > 0;

  return (
    <div 
      style={{
        position: 'sticky',
        top: '40px', // Si posiziona sotto la navbar (64px è l'altezza della navbar)
        left: 0,
        right: 0,
        zIndex: 999, // Un po' meno della navbar per non sovrapporsi
        backgroundColor: 'white',
        boxShadow: showResults ? '0 4px 20px rgba(0,0,0,0.15)' : '0 2px 10px rgba(0,0,0,0.1)',
        padding: '1rem',
        transition: 'box-shadow 0.2s ease',
        borderBottom: '1px solid #e2e8f0',
      }}
    >
      <div 
        style={{ position: 'relative', maxWidth: '800px', margin: '0 auto' }}
        onFocus={() => setIsFocused(true)}
        onBlur={() => setTimeout(() => setIsFocused(false), 200)}
      >
        <input
          type="text"
          value={searchQuery}
          onChange={onSearchChange}
          placeholder="Cerca una sezione..."
          style={{
            width: '100%',
            padding: '0.75rem 1rem',
            fontSize: '1rem',
            border: isFocused ? '2px solid #3b82f6' : '1px solid #ccc',
            borderRadius: '8px',
            outline: 'none',
            transition: 'border-color 0.2s ease, box-shadow 0.2s ease',
            boxShadow: isFocused ? '0 0 0 3px rgba(59, 130, 246, 0.1)' : 'none',
            backgroundColor: 'white',
          }}
        />
        
        {/* Dropdown dei risultati */}
        {showResults && (
          <div style={{
            position: 'absolute',
            top: '100%',
            left: 0,
            right: 0,
            backgroundColor: 'white',
            border: '1px solid #ddd',
            borderTop: 'none',
            borderRadius: '0 0 8px 8px',
            boxShadow: '0 8px 25px rgba(0,0,0,0.15)',
            zIndex: 1001,
            maxHeight: '400px',
            overflowY: 'auto',
          }}>
            <ul style={{
              listStyle: 'none',
              padding: 0,
              margin: 0,
            }}>
              {results.length > 0 ? (
                results.map((result) => (
                  <li
                    key={result.id}
                    onClick={() => {
                      onResultClick(result.id);
                      setIsFocused(false); // Chiude i risultati dopo il click
                    }}
                    style={{
                      padding: '0.75rem 1rem',
                      cursor: 'pointer',
                      borderBottom: '1px solid #eee',
                      transition: 'background-color 0.15s ease',
                    }}
                    className="search-result-item"
                  >
                    <div style={{ fontWeight: '500', color: '#374151' }}>
                      {result.title}
                    </div>
                    {result.description && (
                      <div style={{ 
                        fontSize: '0.875rem', 
                        color: '#6b7280', 
                        marginTop: '0.25rem' 
                      }}>
                        {result.description}
                      </div>
                    )}
                  </li>
                ))
              ) : (
                <li style={{ 
                  padding: '1rem', 
                  color: '#6b7280',
                  textAlign: 'center',
                  fontStyle: 'italic'
                }}>
                  Nessun risultato trovato
                </li>
              )}
            </ul>
          </div>
        )}
      </div>
      
      <style>{`
        .search-result-item:hover {
          background-color: #f8fafc !important;
        }
        .search-result-item:last-child {
          border-bottom: none;
        }
        
        /* Media query per mobile */
        @media (max-width: 768px) {
          .search-result-item {
            padding: 1rem !important;
          }
        }
      `}</style>
    </div>
  );
};

export default SearchBar;