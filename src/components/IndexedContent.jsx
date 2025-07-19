import React from 'react';
import MarkdownRenderer from './MarkdownRenderer';

/**
 * Un componente che analizza un testo Markdown, genera un indice dai titoli (h2, h3)
 * e renderizza sia l'indice che il contenuto completo.
 * @param {{text: string}} props - Il testo Markdown da processare.
 */
const IndexedContent = ({ text }) => {
  
  /**
   * Estrae i titoli (h2 e h3) da una stringa Markdown.
   * @param {string} markdownText - Il testo da cui estrarre i titoli.
   * @returns {{level: number, title: string, id: string}[]} Un array di oggetti titolo.
   */
  const getHeadings = (markdownText) => {
    if (!markdownText) return [];
    
    const headings = [];
    const lines = markdownText.split('\n');
    // Regex per trovare titoli di livello 2 (##) e 3 (###)
    const headingRegex = /^(##|###)\s(.+)/;

    // Funzione per creare uno "slug" da un titolo per usarlo come ID
    const createSlug = (text) => {
      return text
        .toLowerCase()
        .replace(/[^\w\s-]/g, '') // Rimuovi caratteri non alfanumerici eccetto spazi e trattini
        .trim()
        .replace(/\s+/g, '-') // Sostituisci spazi con trattini
        .replace(/-+/g, '-'); // Rimuovi trattini multipli
    };

    for (const line of lines) {
      const match = line.match(headingRegex);
      if (match) {
        const level = match[1].length; // 2 per ##, 3 per ###
        const title = match[2].trim();
        const id = createSlug(title);
        headings.push({ level, title, id });
      }
    }
    return headings;
  };

  const headings = getHeadings(text);

  return (
    <div>
      {/* Sezione Indice (mostrata solo se ci sono titoli) */}
      {headings.length > 0 && (
        <div className="p-6 mb-8 bg-slate-50 border border-slate-200 rounded-lg shadow-sm">
          <h2 className="text-xl font-bold mb-4 text-slate-800 border-b pb-2">Indice della Guida</h2>
          <ul className="space-y-2 mt-3">
            {headings.map((heading, index) => (
              <li key={index} className={heading.level === 3 ? 'ml-5' : 'font-semibold'}>
                <a 
                  href={`#${heading.id}`} 
                  className="text-blue-600 hover:text-blue-800 hover:underline transition-colors duration-200"
                  // Aggiunge uno scroll fluido
                  onClick={(e) => {
                    e.preventDefault();
                    document.getElementById(heading.id)?.scrollIntoView({
                      behavior: 'smooth',
                      block: 'start'
                    });
                  }}
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
