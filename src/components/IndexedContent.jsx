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
      {/* Barra di ricerca */}
      <SearchBar
        searchQuery={searchQuery}
        onSearchChange={handleSearchChange}
        results={filteredResults}
        onResultClick={handleResultClick}
      />

      {/* Sezione Indice Statico (invariata) */}
      {headings.length > 0 && (
        <div className="p-6 mb-8 bg-slate-50 border border-slate-200 rounded-lg shadow-sm">
          <h2 className="text-xl font-bold mb-4 text-slate-800 border-b pb-2">Indice della Guida</h2>
          <ul className="space-y-2 mt-3">
            {headings.map((heading, index) => (
              <li key={index} className={heading.level === 3 ? 'ml-5' : 'font-semibold'}>
                <a 
                  href={`#${heading.id}`} 
                  className="text-blue-600 hover:text-blue-800 hover:underline transition-colors duration-200"
                  onClick={(e) => handleStaticLinkClick(e, heading.id)}
                >
                  {heading.title}
                </a>
              </li>
            ))}
          </ul>
        </div>
      )}
      
      {/* Contenuto renderizzato */}
      <MarkdownRenderer text={text} />
    </div>
  );
};

export default IndexedContent;
