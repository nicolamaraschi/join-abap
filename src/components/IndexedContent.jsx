import React, { useState, useMemo } from 'react';
import MarkdownRenderer from './MarkdownRenderer';
import SearchBar from './SearchBar';

/**
 * Un componente che analizza un testo Markdown, genera un indice e una barra di ricerca
 * e renderizza tutto insieme al contenuto.
 * @param {{text: string}} props - Il testo Markdown da processare.
 */
const IndexedContent = ({ text }) => {
  const [searchQuery, setSearchQuery] = useState('');

  // Estrae e memoizza i titoli per non ricalcolarli ad ogni render
  const headings = useMemo(() => {
    if (!text) return [];
    
    const headings = [];
    const lines = text.split('\n');
    const headingRegex = /^(##|###)\s(.+)/;

    const createSlug = (text) => {
      return text
        .toLowerCase()
        .replace(/[^\w\s-]/g, '')
        .trim()
        .replace(/\s+/g, '-')
        .replace(/-+/g, '-');
    };

    for (const line of lines) {
      const match = line.match(headingRegex);
      if (match) {
        const level = match[1].length;
        const title = match[2].trim();
        const id = createSlug(title);
        headings.push({ level, title, id });
      }
    }
    return headings;
  }, [text]);

  // Filtra i risultati della ricerca
  const filteredResults = useMemo(() => {
    if (!searchQuery) return [];
    return headings.filter(h => 
      h.title.toLowerCase().includes(searchQuery.toLowerCase())
    );
  }, [searchQuery, headings]);

  const handleSearchChange = (e) => {
    setSearchQuery(e.target.value);
  };

  const handleResultClick = (id) => {
    document.getElementById(id)?.scrollIntoView({
      behavior: 'smooth',
      block: 'start'
    });
    setSearchQuery(''); // Pulisce la ricerca dopo il click
  };

  const handleStaticLinkClick = (e, id) => {
    e.preventDefault();
    document.getElementById(id)?.scrollIntoView({
      behavior: 'smooth',
      block: 'start'
    });
  };

  return (
    <div>
    <div style={{
      position: 'sticky',
      top: '-2rem', // Compensa il padding: 2rem di AbapDocDetailsView
      zIndex: 50,
      backgroundColor: 'rgba(241, 245, 249, 0.9)', // Sfondo slate-100 con trasparenza
      padding: '1.25rem 0',
      margin: '0 -2rem 1.5rem -2rem', // Sposta lateralmente per coprire tutto il contenitore
      borderBottom: '1px solid #e2e8f0',
      backdropFilter: 'blur(10px)',
      boxShadow: '0 4px 6px -1px rgba(0, 0, 0, 0.05)'
    }}>
      <div style={{ padding: '0 2rem' }}> {/* Ripristina il padding interno per la barra */}
        <SearchBar
          searchQuery={searchQuery}
          onSearchChange={handleSearchChange}
          results={filteredResults}
          onResultClick={handleResultClick}
        />
      </div>
    </div>

      {/* Sezione Indice Statico - Restilizzata per un look Premium */}
      {headings.length > 0 && (
        <div style={{
          padding: '1.5rem 2rem',
          marginBottom: '2.5rem',
          backgroundColor: '#f8fafc',
          border: '1px solid #e2e8f0',
          borderRadius: '16px',
          boxShadow: '0 4px 6px -1px rgba(0, 0, 0, 0.05), 0 2px 4px -1px rgba(0, 0, 0, 0.06)',
          position: 'relative',
          overflow: 'hidden'
        }}>
          <div style={{
            position: 'absolute',
            top: 0,
            left: 0,
            width: '4px',
            height: '100%',
            backgroundColor: '#3b82f6'
          }} />
          
          <h2 style={{
            fontSize: '1.25rem',
            fontWeight: '700',
            marginBottom: '1.25rem',
            color: '#1e293b',
            display: 'flex',
            alignItems: 'center',
            gap: '10px'
          }}>
            <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
              <line x1="8" y1="6" x2="21" y2="6"></line>
              <line x1="8" y1="12" x2="21" y2="12"></line>
              <line x1="8" y1="18" x2="21" y2="18"></line>
              <line x1="3" y1="6" x2="3.01" y2="6"></line>
              <line x1="3" y1="12" x2="3.01" y2="12"></line>
              <line x1="3" y1="18" x2="3.01" y2="18"></line>
            </svg>
            Indice della Guida
          </h2>
          
          <ul style={{ 
            listStyle: 'none', 
            padding: 0, 
            margin: 0,
            display: 'grid',
            gridTemplateColumns: 'repeat(auto-fill, minmax(300px, 1fr))',
            gap: '0.75rem'
          }}>
            {headings.map((heading, index) => (
              <li key={index} style={{
                marginLeft: heading.level === 3 ? '1.5rem' : '0',
                paddingLeft: heading.level === 3 ? '1rem' : '0',
                borderLeft: heading.level === 3 ? '1px solid #e2e8f0' : 'none'
              }}>
                <a 
                  href={`#${heading.id}`} 
                  className="toc-link"
                  onClick={(e) => handleStaticLinkClick(e, heading.id)}
                  style={{
                    color: heading.level === 2 ? '#334155' : '#64748b',
                    textDecoration: 'none',
                    fontSize: heading.level === 2 ? '0.95rem' : '0.875rem',
                    fontWeight: heading.level === 2 ? '600' : '400',
                    transition: 'all 0.2s ease',
                    display: 'block',
                    padding: '4px 8px',
                    borderRadius: '6px'
                  }}
                >
                  {heading.title}
                </a>
              </li>
            ))}
          </ul>
          
          <style dangerouslySetInnerHTML={{ __html: `
            .toc-link:hover {
              background-color: #eff6ff;
              color: #2563eb !important;
              padding-left: 12px !important;
            }
          `}} />
        </div>
      )}
      
      {/* Contenuto renderizzato */}
      <MarkdownRenderer text={text} />
    </div>
  );
};

export default IndexedContent;
