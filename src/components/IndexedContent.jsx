import React, { useState, useMemo } from 'react';
import MarkdownRenderer from './MarkdownRenderer';
import SearchBar from './SearchBar';

/**
 * Un componente che analizza un testo Markdown, genera un indice e una barra di ricerca
 * e renderizza tutto insieme al contenuto.
 */
const IndexedContent = ({ text }) => {
  const [searchQuery, setSearchQuery] = useState('');
  const [showSubsections, setShowSubsections] = useState(true);

  const headings = useMemo(() => {
    if (!text) return [];
    
    const headings = [];
    const createSlug = (text) => {
      return text
        .toLowerCase()
        .replace(/[^\w\s-]/g, '')
        .trim()
        .replace(/\s+/g, '-')
        .replace(/-+/g, '-');
    };

    // Splittiamo il testo in base ai blocchi di codice (```) come fa MarkdownRenderer
    // per evitare di indicizzare falsi titoli all'interno del codice.
    const parts = text.split(/(```[\s\S]*?```)/gi);
    const headingRegex = /^(#{1,4})\s+(.+)/;

    parts.forEach(part => {
      // Ignora i blocchi di codice
      if (part.startsWith('```')) return;

      const lines = part.split('\n');
      for (const line of lines) {
        const match = line.match(headingRegex);
        if (match) {
          const level = match[1].length;
          const title = match[2].trim();
          const id = createSlug(title);
          headings.push({ level, title, id });
        }
      }
    });

    return headings;
  }, [text]);

  const tocHeadings = useMemo(
    () => headings.filter((heading) => heading.level <= 3),
    [headings]
  );

  const chapters = useMemo(() => {
    const groupedChapters = [];
    let currentChapter = null;
    let currentSection = null;

    tocHeadings.forEach((heading) => {
      if (heading.level === 1) {
        currentChapter = {
          ...heading,
          sections: []
        };
        groupedChapters.push(currentChapter);
        currentSection = null;
        return;
      }

      if (!currentChapter) {
        currentChapter = {
          level: 1,
          title: 'Panoramica',
          id: 'panoramica',
          sections: []
        };
        groupedChapters.push(currentChapter);
      }

      if (heading.level === 2) {
        currentSection = {
          ...heading,
          subheadings: []
        };
        currentChapter.sections.push(currentSection);
        return;
      }

      if (heading.level === 3) {
        if (!currentSection) {
          currentSection = {
            level: 2,
            title: 'Approfondimenti',
            id: `${currentChapter.id}-approfondimenti`,
            subheadings: []
          };
          currentChapter.sections.push(currentSection);
        }
        currentSection.subheadings.push(heading);
      }
    });

    return groupedChapters.filter(
      (chapter) => chapter.title || chapter.sections.length > 0
    );
  }, [tocHeadings]);

  const sectionCount = useMemo(
    () => chapters.reduce((total, chapter) => total + chapter.sections.length, 0),
    [chapters]
  );

  const subsectionCount = useMemo(
    () =>
      chapters.reduce(
        (total, chapter) =>
          total +
          chapter.sections.reduce(
            (sectionTotal, section) => sectionTotal + (section.subheadings?.length || 0),
            0
          ),
        0
      ),
    [chapters]
  );

  const filteredResults = useMemo(() => {
    if (!searchQuery) return [];
    return tocHeadings.filter(h => 
      h.title.toLowerCase().includes(searchQuery.toLowerCase())
    );
  }, [searchQuery, tocHeadings]);

  const handleSearchChange = (e) => {
    setSearchQuery(e.target.value);
  };

  const handleResultClick = (id) => {
    const element = document.getElementById(id);
    if (element) {
      element.scrollIntoView({
        behavior: 'smooth',
        block: 'start'
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

  const handleBackToIndex = () => {
    const indexElement = document.getElementById('indice-guida');
    if (indexElement) {
      indexElement.scrollIntoView({
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
        {chapters.length > 0 && (
          <div id="indice-guida" className="toc-container">
            <div className="toc-accent" />
            <div className="toc-header">
              <div>
                <h2 className="toc-title">
                  <svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
                    <line x1="8" y1="6" x2="21" y2="6"></line>
                    <line x1="8" y1="12" x2="21" y2="12"></line>
                    <line x1="8" y1="18" x2="21" y2="18"></line>
                    <path d="M3 6h.01M3 12h.01M3 18h.01"></path>
                  </svg>
                  Indice della Guida
                </h2>
                <p className="toc-description">
                  Struttura ripulita per capitoli e sezioni, con meno rumore visivo e accesso rapido ai punti davvero utili.
                </p>
              </div>

              <div className="toc-toolbar">
                <div className="toc-stats">
                  <span>{chapters.length} capitoli</span>
                  <span>{sectionCount} sezioni</span>
                  <span>{subsectionCount} sottosezioni</span>
                </div>
                <button
                  type="button"
                  className="toc-toggle-button"
                  onClick={() => setShowSubsections((value) => !value)}
                >
                  {showSubsections ? 'Vista compatta' : 'Mostra sottosezioni'}
                </button>
              </div>
            </div>

            <div className="toc-chapter-jump-list">
              {chapters.map((chapter) => (
                <a
                  key={chapter.id}
                  href={`#${chapter.id}`}
                  className="toc-chapter-jump"
                  onClick={(e) => handleStaticLinkClick(e, chapter.id)}
                >
                  {chapter.title}
                </a>
              ))}
            </div>

            <div className="toc-chapters">
              {chapters.map((chapter) => (
                <section key={chapter.id} className="toc-chapter">
                  <div className="toc-chapter-header">
                    <a
                      href={`#${chapter.id}`}
                      className="toc-chapter-title"
                      onClick={(e) => handleStaticLinkClick(e, chapter.id)}
                    >
                      {chapter.title}
                    </a>
                    <span className="toc-chapter-meta">
                      {chapter.sections.length} sezioni
                    </span>
                  </div>

                  <div className="toc-card-grid">
                    {chapter.sections.map((section) => (
                      <article key={section.id} className="toc-card">
                        <a
                          href={`#${section.id}`}
                          className="toc-card-title"
                          onClick={(e) => handleStaticLinkClick(e, section.id)}
                        >
                          {section.title}
                        </a>

                        {showSubsections && section.subheadings?.length > 0 && (
                          <div className="toc-subsection-list">
                            {section.subheadings.map((subheading) => (
                              <a
                                key={subheading.id}
                                href={`#${subheading.id}`}
                                className="toc-subsection-link"
                                onClick={(e) => handleStaticLinkClick(e, subheading.id)}
                              >
                                {subheading.title}
                              </a>
                            ))}
                          </div>
                        )}
                      </article>
                    ))}
                  </div>
                </section>
              ))}
            </div>
          </div>
        )}
        
        <MarkdownRenderer text={text} />
      </div>

      {chapters.length > 0 && (
        <button
          type="button"
          className="back-to-index-button"
          onClick={handleBackToIndex}
        >
          Torna all'indice
        </button>
      )}

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
          padding: 1.75rem 2rem 2rem;
          margin-bottom: 3rem;
          background:
            radial-gradient(circle at top right, rgba(219, 234, 254, 0.75), transparent 28%),
            linear-gradient(180deg, #ffffff 0%, #f8fbff 100%);
          border: 1px solid #dbe7f5;
          border-radius: 18px;
          box-shadow: 0 18px 40px rgba(15, 23, 42, 0.06);
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

        .toc-header {
          display: flex;
          align-items: flex-start;
          justify-content: space-between;
          gap: 1.5rem;
          margin-bottom: 1.25rem;
        }

        .toc-title {
          font-size: 1.25rem;
          font-weight: 700;
          margin-bottom: 0.5rem;
          color: #1e293b;
          display: flex;
          align-items: center;
          gap: 12px;
        }

        .toc-description {
          margin: 0;
          color: #475569;
          max-width: 720px;
          line-height: 1.6;
        }

        .toc-toolbar {
          display: flex;
          flex-direction: column;
          align-items: flex-end;
          gap: 0.75rem;
        }

        .toc-stats {
          display: flex;
          flex-wrap: wrap;
          justify-content: flex-end;
          gap: 0.5rem;
        }

        .toc-stats span {
          background: rgba(255, 255, 255, 0.92);
          border: 1px solid #dbe7f5;
          color: #334155;
          border-radius: 999px;
          padding: 0.42rem 0.8rem;
          font-size: 0.82rem;
          font-weight: 700;
        }

        .toc-toggle-button {
          border: 1px solid #bfdbfe;
          background: #eff6ff;
          color: #1d4ed8;
          border-radius: 999px;
          padding: 0.6rem 0.95rem;
          font-size: 0.88rem;
          font-weight: 700;
          cursor: pointer;
          transition: all 0.2s ease;
        }

        .toc-toggle-button:hover {
          background: #dbeafe;
          transform: translateY(-1px);
        }

        .toc-chapter-jump-list {
          display: flex;
          flex-wrap: wrap;
          gap: 0.65rem;
          margin-bottom: 1.5rem;
        }

        .toc-chapter-jump {
          text-decoration: none;
          color: #0f172a;
          background: rgba(255, 255, 255, 0.85);
          border: 1px solid #dbe7f5;
          padding: 0.55rem 0.8rem;
          border-radius: 999px;
          font-size: 0.86rem;
          font-weight: 700;
          transition: all 0.2s ease;
        }

        .toc-chapter-jump:hover {
          border-color: #93c5fd;
          background: #eff6ff;
          color: #1d4ed8;
        }

        .toc-chapters {
          display: flex;
          flex-direction: column;
          gap: 1.5rem;
        }

        .toc-chapter {
          background: rgba(255, 255, 255, 0.76);
          border: 1px solid #e2e8f0;
          border-radius: 16px;
          padding: 1.2rem;
        }

        .toc-chapter-header {
          display: flex;
          align-items: center;
          justify-content: space-between;
          gap: 1rem;
          margin-bottom: 1rem;
        }

        .toc-chapter-title {
          color: #0f172a;
          text-decoration: none;
          font-size: 1rem;
          font-weight: 800;
        }

        .toc-chapter-title:hover {
          color: #1d4ed8;
        }

        .toc-chapter-meta {
          color: #64748b;
          font-size: 0.82rem;
          font-weight: 700;
          white-space: nowrap;
        }

        .toc-card-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
          gap: 0.9rem;
        }

        .toc-card {
          background: linear-gradient(180deg, #ffffff 0%, #f8fafc 100%);
          border: 1px solid #e2e8f0;
          border-radius: 14px;
          padding: 1rem;
          min-height: 100px;
          transition: transform 0.2s ease, box-shadow 0.2s ease, border-color 0.2s ease;
        }

        .toc-card:hover {
          transform: translateY(-2px);
          border-color: #bfdbfe;
          box-shadow: 0 10px 24px rgba(37, 99, 235, 0.08);
        }

        .toc-card-title {
          display: block;
          text-decoration: none;
          color: #1e293b;
          font-size: 0.96rem;
          font-weight: 800;
          line-height: 1.45;
        }

        .toc-card-title:hover {
          color: #1d4ed8;
        }

        .toc-subsection-list {
          margin-top: 0.8rem;
          display: flex;
          flex-direction: column;
          gap: 0.45rem;
          padding-top: 0.8rem;
          border-top: 1px solid #eef2f7;
        }

        .toc-subsection-link {
          color: #475569;
          text-decoration: none;
          font-size: 0.88rem;
          line-height: 1.45;
          transition: color 0.2s ease, transform 0.2s ease;
        }

        .toc-subsection-link:hover {
          color: #1d4ed8;
          transform: translateX(2px);
        }

        .back-to-index-button {
          position: fixed;
          right: 1.5rem;
          bottom: 1.5rem;
          z-index: 120;
          border: 0;
          border-radius: 999px;
          background: linear-gradient(135deg, #2563eb, #1d4ed8);
          color: #ffffff;
          padding: 0.85rem 1.2rem;
          font-size: 0.95rem;
          font-weight: 700;
          cursor: pointer;
          box-shadow: 0 14px 30px rgba(37, 99, 235, 0.25);
          transition: transform 0.2s ease, box-shadow 0.2s ease;
        }

        .back-to-index-button:hover {
          transform: translateY(-2px);
          box-shadow: 0 18px 34px rgba(37, 99, 235, 0.32);
        }

        @media (max-width: 768px) {
          .toc-header {
            flex-direction: column;
          }

          .toc-toolbar {
            align-items: stretch;
            width: 100%;
          }

          .toc-stats {
            justify-content: flex-start;
          }
          
          .sticky-search-header {
            padding: 0.75rem 1rem;
            margin: -1rem -1rem 1.5rem -1rem;
          }

          .toc-container {
            padding: 1.2rem;
            border-radius: 14px;
          }

          .toc-card-grid {
            grid-template-columns: 1fr;
          }

          .toc-chapter-header {
            flex-direction: column;
            align-items: flex-start;
          }

          .back-to-index-button {
            right: 1rem;
            bottom: 1rem;
            padding: 0.75rem 1rem;
            font-size: 0.9rem;
          }
        }
      `}} />
    </div>
  );
};

export default IndexedContent;
