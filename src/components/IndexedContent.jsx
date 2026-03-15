import React, { useState, useMemo } from 'react';
import MarkdownRenderer from './MarkdownRenderer';
import SearchBar from './SearchBar';

/**
 * Un componente che analizza un testo Markdown, genera un indice e una barra di ricerca
 * e renderizza tutto insieme al contenuto.
 */
const IndexedContent = ({ text }) => {
  const [searchQuery, setSearchQuery] = useState('');

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
    const element = document.getElementById(id);
    if (element) {
      const offset = 100; // Offset per la barra sticky
      const bodyRect = document.body.getBoundingClientRect().top;
      const elementRect = element.getBoundingClientRect().top;
      const elementPosition = elementRect - bodyRect;
      const offsetPosition = elementPosition - offset;

      window.scrollTo({
        top: offsetPosition,
        behavior: 'smooth'
      });
    }
    setSearchQuery('');
  };

  const handleStaticLinkClick = (e, id) => {
    e.preventDefault();
    const element = document.getElementById(id);
    if (element) {
      element.scrollIntoView({
        behavior: 'smooth',
        block: 'start'
      });
    }
  };

  return (
    <div className="indexed-content">
      {/* Search Header - Sticky */}
      <div className="sticky-search-header">
        <div className="search-wrapper">
          <SearchBar
            searchQuery={searchQuery}
            onSearchChange={handleSearchChange}
            results={filteredResults}
            onResultClick={handleResultClick}
          />
        </div>
      </div>

      <div className="content-container">
        {/* Indice della Guida */}
        {headings.length > 0 && (
          <div className="toc-container">
            <div className="toc-accent" />
            <h2 className="toc-title">
              <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
                <line x1="8" y1="6" x2="21" y2="6"></line>
                <line x1="8" y1="12" x2="21" y2="12"></line>
                <line x1="8" y1="18" x2="21" y2="18"></line>
                <path d="M3 6h.01M3 12h.01M3 18h.01"></path>
              </svg>
              Indice della Guida
            </h2>
            
            <ul className="toc-list">
              {headings.map((heading, index) => (
                <li key={index} className={`toc-item level-${heading.level}`}>
                  <a 
                    href={`#${heading.id}`} 
                    className="toc-link"
                    onClick={(e) => handleStaticLinkClick(e, heading.id)}
                  >
                    {heading.title}
                  </a>
                </li>
              ))}
            </ul>
          </div>
        )}
        
        <MarkdownRenderer text={text} />
      </div>

      <style dangerouslySetInnerHTML={{ __html: `
        .indexed-content {
          position: relative;
        }

        .sticky-search-header {
          position: sticky;
          top: -2rem;
          left: 0;
          right: 0;
          z-index: 100;
          margin: -2rem -2rem 2rem -2rem;
          padding: 1rem 2rem;
          background: rgba(255, 255, 255, 0.8);
          backdrop-filter: blur(12px);
          -webkit-backdrop-filter: blur(12px);
          border-bottom: 1px solid rgba(226, 232, 240, 0.8);
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.05);
          transition: all 0.3s ease;
        }

        .search-wrapper {
          max-width: 800px;
          margin: 0 auto;
        }

        .content-container {
          max-width: 1000px;
          margin: 0 auto;
        }

        .toc-container {
          padding: 1.5rem 2rem;
          margin-bottom: 3rem;
          background: #ffffff;
          border: 1px solid #e2e8f0;
          border-radius: 12px;
          box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.02);
          position: relative;
          overflow: hidden;
        }

        .toc-accent {
          position: absolute;
          top: 0;
          left: 0;
          width: 4px;
          height: 100%;
          background: linear-gradient(to bottom, #3b82f6, #60a5fa);
        }

        .toc-title {
          font-size: 1.25rem;
          font-weight: 700;
          margin-bottom: 1.5rem;
          color: #1e293b;
          display: flex;
          align-items: center;
          gap: 12px;
        }

        .toc-list {
          list-style: none;
          padding: 0;
          margin: 0;
          display: grid;
          grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
          gap: 0.5rem 1.5rem;
        }

        .toc-item {
          transition: all 0.2s ease;
        }

        .level-3 {
          padding-left: 1.25rem;
          border-left: 1px solid #f1f5f9;
        }

        .toc-link {
          color: #475569;
          text-decoration: none;
          font-size: 0.9rem;
          display: block;
          padding: 6px 10px;
          border-radius: 6px;
          transition: all 0.2s ease;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
        }

        .toc-link:hover {
          background-color: #f0f7ff;
          color: #2563eb;
          padding-left: 14px;
        }

        .toc-item.level-2 .toc-link {
          color: #1e293b;
          font-weight: 600;
        }

        @media (max-width: 768px) {
          .toc-list {
            grid-template-columns: 1fr;
          }
          
          .sticky-search-header {
            padding: 0.75rem 1rem;
            margin: -1rem -1rem 1.5rem -1rem;
          }
        }
      `}} />
    </div>
  );
};

export default IndexedContent;
