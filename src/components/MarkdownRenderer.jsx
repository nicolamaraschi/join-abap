import React from 'react';
import AbapCode from './AbapCode.jsx';

const MarkdownRenderer = ({ text }) => {
  if (!text) {
    return null;
  }

  // Funzione per creare uno "slug" da un titolo per usarlo come ID
  const createSlug = (text) => {
    return text
      .toLowerCase()
      .replace(/[^\w\s-]/g, '') // Rimuovi caratteri non alfanumerici eccetto spazi e trattini
      .trim()
      .replace(/\s+/g, '-') // Sostituisci spazi con trattini
      .replace(/-+/g, '-'); // Rimuovi trattini multipli
  };

  // Funzione per processare le tabelle Markdown (migliorata)
  const processTables = (htmlContent) => {
    const tableRegex = /^((?:\|.*\|[\r\n]?)+)/gm;
    return htmlContent.replace(tableRegex, (tableBlock) => {
      const lines = tableBlock.trim().split('\n');
      if (lines.length < 2 || !lines[1].match(/^[| :\-~]+$/)) {
        return tableBlock;
      }

      let tableHtml = '<table class="markdown-table w-full border-collapse border border-gray-300 my-4">';
      const headers = lines[0].split('|').map(h => h.trim()).slice(1, -1);
      
      // Processare l'allineamento dalla riga separatrice
      const alignments = lines[1].split('|').map(cell => {
        const trimmed = cell.trim();
        if (trimmed.startsWith(':') && trimmed.endsWith(':')) return 'center';
        if (trimmed.endsWith(':')) return 'right';
        return 'left';
      }).slice(1, -1);

      tableHtml += '<thead class="bg-gray-50"><tr>';
      headers.forEach((header, i) => {
        const align = alignments[i] || 'left';
        tableHtml += `<th class="border border-gray-300 px-4 py-2 font-semibold text-${align}">${header}</th>`;
      });
      tableHtml += '</tr></thead>';

      tableHtml += '<tbody>';
      lines.slice(2).forEach(rowLine => {
        const cells = rowLine.split('|').map(c => c.trim()).slice(1, -1);
        tableHtml += '<tr class="hover:bg-gray-50">';
        for (let i = 0; i < headers.length; i++) {
          const align = alignments[i] || 'left';
          const cellContent = (cells[i] || '').replace(/`([^`]+)`/g, '<code class="bg-slate-200 text-slate-800 px-2 py-1 rounded text-sm font-mono">$1</code>');
          tableHtml += `<td class="border border-gray-300 px-4 py-2 text-${align}">${cellContent}</td>`;
        }
        tableHtml += '</tr>';
      });
      tableHtml += '</tbody></table>';
      return tableHtml;
    });
  };

  // Funzione per processare liste (MIGLIORATA per documentazione tecnica)
  const processLists = (content) => {
    // Liste non ordinate con supporto per elementi tecnici
    content = content.replace(/^(\s*)[-*+]\s+(.+)$/gm, (match, indent, text) => {
      const level = Math.floor(indent.length / 2);
      const marginClass = level > 0 ? `ml-${level * 4}` : '';
      
      // Stile speciale per elementi con pattern tecnici
      let itemClass = "list-disc ml-6 mb-2 leading-relaxed";
      if (text.match(/^[A-Z]\)\s/)) {
        itemClass = "list-none ml-6 mb-2 leading-relaxed bg-gray-50 p-2 rounded border-l-4 border-blue-400";
      }
      
      return `<li class="${itemClass} ${marginClass}">${text}</li>`;
    });

    // Liste ordinate con numerazione migliorata
    content = content.replace(/^(\s*)(\d+)\.\s+(.+)$/gm, (match, indent, num, text) => {
      const level = Math.floor(indent.length / 2);
      const marginClass = level > 0 ? `ml-${level * 4}` : '';
      return `<li class="list-decimal ml-6 ${marginClass} mb-2 leading-relaxed">${text}</li>`;
    });

    // Raggruppa le liste consecutive
    content = content.replace(/(<li class="list-disc[^>]*>.*?<\/li>(?:\s*<li class="list-[^>]*>.*?<\/li>)*)/gs, '<ul class="my-4 space-y-1">$1</ul>');
    content = content.replace(/(<li class="list-decimal[^>]*>.*?<\/li>(?:\s*<li class="list-decimal[^>]*>.*?<\/li>)*)/gs, '<ol class="my-4 space-y-1">$1</ol>');
    content = content.replace(/(<li class="list-none[^>]*>.*?<\/li>(?:\s*<li class="list-none[^>]*>.*?<\/li>)*)/gs, '<ul class="my-4 space-y-2">$1</ul>');

    return content;
  };

  // Funzione per processare link (NUOVA FUNZIONALITÀ)
  const processLinks = (content) => {
    // Link con testo [text](url)
    content = content.replace(/\[([^\]]+)\]\(([^)]+)\)/g, (match, text, url) => {
      const isInternal = url.startsWith('#');
      const target = isInternal ? '' : 'target="_blank" rel="noopener noreferrer"';
      return `<a href="${url}" class="text-blue-600 hover:text-blue-800 underline" ${target}>${text}</a>`;
    });
    
    // Link automatici
    content = content.replace(/(?<!["'])(https?:\/\/[^\s<>"]+)/g, '<a href="$1" class="text-blue-600 hover:text-blue-800 underline" target="_blank" rel="noopener noreferrer">$1</a>');
    
    return content;
  };

  // Funzione per processare blocchi di codice (MIGLIORATA)
  const processCodeBlocks = (content) => {
    // Blocchi di codice con linguaggio ```lang
    content = content.replace(/```(\w+)?\n?([\s\S]*?)```/g, (match, lang, code) => {
      const language = lang || 'text';
      const isAbap = language.toLowerCase() === 'abap';
      
      // === LA CORREZIONE È QUI ===
      // Rimuoviamo la seconda conversione (escape) che era ridondante e causava il bug.
      // La variabile 'code' contiene già il testo correttamente "escapato" dalla conversione globale.
      
      if (isAbap) {
        // Usiamo 'code' direttamente, senza ri-convertirlo.
        return `<pre class="bg-blue-50 border border-blue-200 rounded-lg p-4 overflow-x-auto my-4"><code class="language-abap text-sm font-mono text-blue-900">${code.trim()}</code></pre>`;
      }
      
      return `<pre class="bg-gray-100 border border-gray-300 rounded-lg p-4 overflow-x-auto my-4"><code class="language-${language} text-sm font-mono">${code.trim()}</code></pre>`;
    });
    
    return content;
  };

  // Funzione per processare citazioni (NUOVA FUNZIONALITÀ)
  const processBlockquotes = (content) => {
    // Lasciamo la modifica che cerca '&gt;' perché viene eseguita dopo l'escape globale.
    content = content.replace(/^&gt;\s*(.+)$/gm, '<blockquote class="border-l-4 border-blue-500 pl-4 italic my-4 text-gray-700">$1</blockquote>');
    return content;
  };

  // Funzione per processare paragrafi (MIGLIORATA)
  const processParagraphs = (content) => {
    // Dividi il contenuto in blocchi separati da righe vuote
    const blocks = content.split(/\n\s*\n/);
    
    return blocks.map(block => {
      const trimmed = block.trim();
      if (!trimmed) return '';
      
      // Non wrappare in <p> se è già un elemento HTML
      if (trimmed.match(/^<(h[1-6]|table|ul|ol|blockquote|pre|hr|div)/)) {
        return trimmed;
      }
      
      // Wrap in paragrafo
      return `<p class="mb-4 leading-relaxed">${trimmed}</p>`;
    }).join('\n\n');
  };

  // Elaborazione principale
  // Splittiamo il testo in base ai blocchi di codice (```)
  const parts = text.split(/(```[\s\S]*?```)/gi);

  const renderedParts = parts.map((part, index) => {
    // Se è un blocco di codice
    if (part.startsWith('```')) {
      // Estrai il linguaggio e il codice
      const match = part.match(/^```(\w+)?\n?([\s\S]*?)```$/i);
      const language = (match && match[1]) ? match[1].toLowerCase() : 'text'; // Default to 'text' if no language specified
      const code = (match && match[2]) ? match[2].trim() : part.replace(/^```(\w+)?\n?|```$/gi, '').trim();
      
      return (
        <div key={index} style={{ margin: '1.5rem 0' }}>
          <AbapCode code={code} language={language} />
        </div>
      );
    }

    if (part.trim() === '') {
      return null;
    }

    // Per il testo normale, applichiamo le regole Markdown
    let html = part;

    // 1. Processare le tabelle (PRIMA dell'escape, perché processTables genera HTML)
    // Salva tabelle temporaneamente
    const processedTables = [];
    html = processTables(html); // This processes raw markdown for tables
    html = html.replace(/<table[\s\S]*?<\/table>/g, (match) => {
      processedTables.push(match);
      return `__TABLE_PLACEHOLDER_${processedTables.length - 1}__`;
    });

    // 2. Escape HTML iniziale (fondamentale farlo subito per il resto del markdown)
    html = html
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;');

    // 3. Processare Titoli e Intestazioni
    html = html
      .replace(/^(Guida Tecnica.*)/gm, (match, title) => `<h1 id="${createSlug(title)}" class="text-3xl font-bold mb-6 text-gray-900 border-b-2 border-blue-500 pb-3">${title}</h1>`)
      .replace(/^# Guida al (.+)/gm, (match, title) => `<h1 id="${createSlug(title)}" class="text-3xl font-bold mb-6 text-gray-900 border-b-2 border-blue-500 pb-3">Guida al ${title}</h1>`)
      .replace(/^Sezione \d+: (.*)/gm, (match, title) => `<h2 id="${createSlug(title)}" class="text-2xl font-semibold mt-8 mb-4 pb-2 border-b border-gray-300 text-gray-800">${title}</h2>`)
      .replace(/^### Fase \d+: (.+)/gm, (match, title) => `<h2 id="${createSlug(title)}" class="text-2xl font-semibold mt-8 mb-4 pb-2 border-b border-blue-200 text-blue-800 bg-blue-50 px-4 py-2 rounded-t-lg">Fase: ${title}</h2>`)
      .replace(/^#### (.+)/gm, (match, title) => `<h3 id="${createSlug(title)}" class="text-xl font-semibold mt-6 mb-3 text-gray-800 flex items-center"><span class="bg-blue-100 text-blue-800 px-2 py-1 rounded text-sm mr-2">📋</span>${title}</h3>`)
      .replace(/^### (.+)/gm, (match, title) => `<h3 id="${createSlug(title)}" class="text-xl font-semibold mt-6 mb-3 text-gray-800">${title}</h3>`)
      .replace(/^## (.+)/gm, (match, title) => `<h2 id="${createSlug(title)}" class="text-2xl font-semibold mt-8 mb-4 pb-2 border-b border-gray-300 text-gray-800">${title}</h2>`)
      .replace(/^# (.+)/gm, (match, title) => `<h1 id="${createSlug(title)}" class="text-3xl font-bold mb-6 text-gray-900 border-b-2 border-blue-500 pb-3">${title}</h1>`);

    // 4. Processare Separatori
    html = html
      .replace(/^====+$/gm, '<hr class="my-8 border-t-2 border-blue-300 shadow-sm"/>')
      .replace(/^----+$/gm, '<hr class="my-6 border-t border-gray-300"/>')
      .replace(/^---$/gm, '<hr class="my-4 border-t border-gray-200"/>');

    // 5. Codice inline e testo formattato (MIGLIORATO per documentazione tecnica)
    html = html
      .replace(/`([^`]+)`/g, (match, code) => {
        // Ripristina i caratteri speciali nel codice inline per l'analisi,
        // ma il valore 'code' passato al tag <code> sarà già escapato.
        const restoredCode = code.replace(/&lt;/g, '<').replace(/&gt;/g, '>').replace(/&amp;/g, '&');
        
        // Stili speciali per ABAP
        if (restoredCode.match(/^<[^>]+>$/)) {
          return `<code class="bg-purple-100 text-purple-900 px-2 py-1 rounded text-sm font-mono font-semibold border border-purple-200">${code}</code>`;
        }
        if (restoredCode.match(/^[A-Z][A-Z0-9_]*$/) || restoredCode.match(/^gv_|^lv_|^gs_|^gt_/i)) {
          return `<code class="bg-blue-100 text-blue-900 px-2 py-1 rounded text-sm font-mono font-semibold border border-blue-200">${code}</code>`;
        }
        if (restoredCode.match(/^[A-Z]{2}\d{2}$|^SE\d{2}$/)) {
          return `<code class="bg-green-100 text-green-900 px-2 py-1 rounded text-sm font-mono font-semibold border border-green-200">${code}</code>`;
        }
        return `<code class="bg-slate-200 text-slate-800 px-2 py-1 rounded text-sm font-mono">${code}</code>`;
      })
      .replace(/\*\*([^*]+)\*\*/g, '<strong class="font-bold text-gray-900">$1</strong>')
      .replace(/\*([^*]+)\*/g, '<em class="italic text-gray-700">$1</em>')
      .replace(/~~([^~]+)~~/g, '<del class="line-through text-gray-500">$1</del>');

    // 6. Altri elementi (Liste, Citazioni, Link, Paragrafi)
    html = processBlockquotes(html);
    html = processLists(html);
    html = processLinks(html);
    // processCodeBlocks is no longer needed for triple backticks, inline code is handled above.
    // html = processCodeBlocks(html); 
    html = processParagraphs(html);

    // 7. Pattern speciali (assicurati che non vengano doppiamente escapati se contengono HTML)
    html = html
      .replace(/(\*\*Obiettivo:\*\*)/g, '<div class="bg-yellow-50 border-l-4 border-yellow-400 p-3 my-4"><strong class="text-yellow-800">🎯 Obiettivo:</strong>')
      .replace(/(\*\*Ricorda sempre)/g, '</div><div class="bg-red-50 border-l-4 border-red-400 p-3 my-4"><strong class="text-red-800">⚠️ Ricorda sempre')
      .replace(/(\*\*Azione Cruciale:\*\*)/g, '<strong class="bg-orange-100 text-orange-800 px-2 py-1 rounded border border-orange-300">🔥 Azione Cruciale:</strong>');

    // Ripristina tabelle
    processedTables.forEach((tableHtml, i) => {
      html = html.replace(`__TABLE_PLACEHOLDER_${i}__`, tableHtml);
    });

    return (
      <div 
        key={index} 
        className="markdown-content"
        style={{ 
          lineHeight: '1.7',
          wordWrap: 'break-word'
        }} 
        dangerouslySetInnerHTML={{ __html: html }} 
      />
    );
  });

  return (
    <div className="markdown-renderer max-w-none">
      <style jsx>{`
        .markdown-content h1, .markdown-content h2, .markdown-content h3 {
          scroll-margin-top: 6rem;
        }
        
        .markdown-content code {
          font-family: 'Courier New', Consolas, Monaco, monospace;
        }
        
        .markdown-content pre {
          font-family: 'Courier New', Consolas, Monaco, monospace;
          white-space: pre-wrap;
          word-wrap: break-word;
        }
        
        .markdown-content table {
          border-spacing: 0;
        }
        
        .markdown-content a:hover {
          text-decoration: underline;
        }
        
        .markdown-content blockquote {
          background-color: #f8fafc;
        }
        
        .markdown-content ul, .markdown-content ol {
          padding-left: 0;
        }
        
        .markdown-content li {
          margin-left: 1.5rem;
        }
        
        @media (max-width: 768px) {
          .markdown-content {
            font-size: 0.9rem;
          }
          
          .markdown-content table {
            font-size: 0.8rem;
          }
          
          .markdown-content pre {
            font-size: 0.8rem;
            padding: 0.75rem;
          }
        }
      `}</style>
      {renderedParts}
    </div>
  );
};

export default MarkdownRenderer;
