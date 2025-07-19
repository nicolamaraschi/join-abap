import React from 'react';

const SearchBar = ({ searchQuery, onSearchChange, results, onResultClick }) => {
  const [isFocused, setIsFocused] = React.useState(false);

  const showResults = isFocused && searchQuery.length > 0;

  return (
    <div 
      style={{
        position: 'fixed',
        top: '64px', // Posizionato sotto la navbar principale
        left: 0,
        right: 0,
        zIndex: 1000,
        backgroundColor: 'white',
        boxShadow: '0 2px 10px rgba(0,0,0,0.1)',
        padding: '1rem',
      }}
      onFocus={() => setIsFocused(true)}
      onBlur={() => setTimeout(() => setIsFocused(false), 200)} // Aggiungi un piccolo ritardo per permettere il click
    >
      <input
        type="text"
        value={searchQuery}
        onChange={onSearchChange}
        placeholder="Cerca una sezione..."
        style={{
          width: '100%',
          padding: '0.75rem',
          fontSize: '1rem',
          border: '1px solid #ccc',
          borderRadius: '8px',
        }}
      />
      {showResults && (
        <ul style={{
          position: 'absolute',
          top: '100%',
          left: '1rem',
          right: '1rem',
          backgroundColor: 'white',
          border: '1px solid #ddd',
          borderRadius: '0 0 8px 8px',
          boxShadow: '0 4px 8px rgba(0,0,0,0.1)',
          listStyle: 'none',
          padding: 0,
          margin: 0,
          maxHeight: '400px',
          overflowY: 'auto',
        }}>
          {results.length > 0 ? (
            results.map((result) => (
              <li
                key={result.id}
                onClick={() => onResultClick(result.id)}
                style={{
                  padding: '0.75rem 1rem',
                  cursor: 'pointer',
                  borderBottom: '1px solid #eee',
                }}
                className="search-result-item"
              >
                {result.title}
              </li>
            ))
          ) : (
            <li style={{ padding: '0.75rem 1rem', color: '#888' }}>
              Nessun risultato
            </li>
          )}
        </ul>
      )}
      <style>{`
        .search-result-item:hover {
          background-color: #f0f0f0;
        }
        .search-result-item:last-child {
          border-bottom: none;
        }
      `}</style>
    </div>
  );
};

export default SearchBar;
