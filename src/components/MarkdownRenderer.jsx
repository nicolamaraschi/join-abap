import React, { useMemo, useCallback } from 'react';
import AbapCode from './AbapCode.jsx';

const MarkdownRenderer = ({ text }) => {
    // Memoizzazione per evitare ricalcoli inutili
    const processedContent = useMemo(() => {
        if (!text) return null;
        
        // Funzione ottimizzata per il parsing delle tabelle con supporto per allineamento
        const processTables = (htmlContent) => {
            const tableRegex = /^((?:\|.*\|[\r\n]?)+)/gm;
            return htmlContent.replace(tableRegex, (tableBlock) => {
                const lines = tableBlock.trim().split('\n').filter(line => line.trim());
                if (lines.length < 2) return tableBlock;
                
                // Parsing della riga di separazione per determinare l'allineamento
                const separatorLine = lines[1];
                if (!separatorLine.match(/^[| :\-~]+$/)) return tableBlock;
                
                const alignments = separatorLine.split('|')
                    .map(cell => cell.trim())
                    .slice(1, -1)
                    .map(cell => {
                        if (cell.startsWith(':') && cell.endsWith(':')) return 'center';
                        if (cell.endsWith(':')) return 'right';
                        return 'left';
                    });

                // Parsing delle intestazioni
                const headers = lines[0].split('|')
                    .map(h => h.trim())
                    .slice(1, -1);

                let tableHtml = '<table class="markdown-table w-full border-collapse shadow-sm rounded-lg overflow-hidden">';
                
                // Thead con styling migliorato
                tableHtml += '<thead class="bg-gradient-to-r from-blue-50 to-indigo-50"><tr>';
                headers.forEach((header, i) => {
                    const alignment = alignments[i] || 'left';
                    tableHtml += `<th class="px-6 py-4 text-${alignment} text-sm font-semibold text-gray-700 border-b border-gray-200 bg-gray-50">${header}</th>`;
                });
                tableHtml += '</tr></thead>';

                // Tbody con alternanza di colori
                tableHtml += '<tbody>';
                lines.slice(2).forEach((rowLine, rowIndex) => {
                    const cells = rowLine.split('|').map(c => c.trim()).slice(1, -1);
                    const rowClass = rowIndex % 2 === 0 ? 'bg-white' : 'bg-gray-50';
                    tableHtml += `<tr class="${rowClass} hover:bg-blue-50 transition-colors duration-200">`;
                    
                    for (let i = 0; i < headers.length; i++) {
                        const cellContent = (cells[i] || '')
                            .replace(/`([^`]+)`/g, '<code class="bg-slate-200 text-slate-800 px-2 py-1 rounded text-xs font-mono">$1</code>')
                            .replace(/\*\*(.*?)\*\*/g, '<strong class="font-semibold">$1</strong>')
                            .replace(/\*(.*?)\*/g, '<em class="italic">$1</em>');
                        
                        const alignment = alignments[i] || 'left';
                        tableHtml += `<td class="px-6 py-4 text-${alignment} text-sm text-gray-700 border-b border-gray-100">${cellContent}</td>`;
                    }
                    tableHtml += '</tr>';
                });
                tableHtml += '</tbody></table>';
                
                return `<div class="table-wrapper my-6 overflow-x-auto">${tableHtml}</div>`;
            });
        };

        // Funzione per processare liste con nesting migliorato
        const processLists = (html) => {
            // Liste non ordinate
            html = html.replace(/^(\s*)[-*+]\s(.+)$/gm, (match, indent, content) => {
                const level = Math.floor(indent.length / 2);
                const className = level === 0 ? 'ml-0' : `ml-${level * 4}`;
                return `<li class="${className} py-1 text-gray-700 leading-relaxed">• ${content}</li>`;
            });
            
            // Liste ordinate
            html = html.replace(/^(\s*)(\d+)\.\s(.+)$/gm, (match, indent, num, content) => {
                const level = Math.floor(indent.length / 2);
                const className = level === 0 ? 'ml-0' : `ml-${level * 4}`;
                return `<li class="${className} py-1 text-gray-700 leading-relaxed">${num}. ${content}</li>`;
            });
            
            return html;
        };

        // Funzione per processare link con sicurezza
        const processLinks = (html) => {
            return html.replace(/\[([^\]]+)\]\(([^)]+)\)/g, (match, text, url) => {
                // Sanitizzazione base dell'URL
                const isExternal = url.startsWith('http') || url.startsWith('https');
                const target = isExternal ? 'target="_blank" rel="noopener noreferrer"' : '';
                return `<a href="${url}" ${target} class="text-blue-600 hover:text-blue-800 underline transition-colors duration-200">${text}</a>`;
            });
        };

        // Funzione per processare citazioni
        const processBlockquotes = (html) => {
            return html.replace(/^>\s(.+)$/gm, '<blockquote class="border-l-4 border-blue-500 pl-4 py-2 my-4 bg-blue-50 text-gray-700 italic">$1</blockquote>');
        };

        // Split del contenuto per gestire blocchi di codice ABAP
        const parts = text.split(/(```(?:abap|ABAP)[\s\S]*?```)/g);

        return parts.map((part, index) => {
            // Gestione blocchi di codice ABAP
            if (part.match(/^```(?:abap|ABAP)/)) {
                const code = part.replace(/^```(?:abap|ABAP)\n?|\n?```$/g, '');
                return {
                    type: 'abap',
                    content: code,
                    index
                };
            }

            if (part.trim() === '') {
                return null;
            }
            
            // Processing avanzato del contenuto
            let html = processTables(part);
            
            // Salvataggio temporaneo delle tabelle
            const processedTables = [];
            html = html.replace(/<div class="table-wrapper[\s\S]*?<\/div>/g, (match) => {
                processedTables.push(match);
                return `__TABLE_PLACEHOLDER_${processedTables.length - 1}__`;
            });
            
            // Escaping HTML con preservazione di caratteri speciali
            html = html
                .replace(/&/g, '&amp;')
                .replace(/</g, '&lt;')
                .replace(/>/g, '&gt;');
            
            // Processing dei vari elementi markdown con styling avanzato
            html = html
                // Titoli principali con gradient
                .replace(/^(Guida Tecnica.*)/gm, '<h1 class="text-4xl font-bold mb-6 text-transparent bg-clip-text bg-gradient-to-r from-blue-600 to-purple-600">$1</h1>')
                .replace(/^(Sezione \d+:\s*)(.*)/gm, '<h2 class="text-3xl font-semibold mt-10 mb-6 pb-3 border-b-2 border-gradient-to-r from-blue-500 to-purple-500 text-gray-800"><span class="text-blue-600">$1</span>$2</h2>')
                .replace(/^###\s+(.*)/gm, '<h3 class="text-2xl font-semibold mt-8 mb-4 text-gray-700 border-l-4 border-blue-500 pl-4">$1</h3>')
                .replace(/^##\s+(.*)/gm, '<h2 class="text-3xl font-semibold mt-10 mb-6 text-gray-800">$1</h2>')
                .replace(/^#\s+(.*)/gm, '<h1 class="text-4xl font-bold mb-6 text-gray-900">$1</h1>')
                
                // Separatori con stile
                .replace(/^={4,}$/gm, '<hr class="my-8 border-0 h-1 bg-gradient-to-r from-blue-500 to-purple-500 rounded"/>')
                .replace(/^-{4,}$/gm, '<hr class="my-6 border-0 h-px bg-gray-300"/>')
                
                // Code inline con migliore styling
                .replace(/`([^`]+)`/g, '<code class="bg-gray-100 text-red-600 px-2 py-1 rounded text-sm font-mono border">$1</code>')
                
                // Testo formattato
                .replace(/\*\*\*([^*]+)\*\*\*/g, '<strong class="font-bold italic text-gray-900">$1</strong>')
                .replace(/\*\*([^*]+)\*\*/g, '<strong class="font-semibold text-gray-900">$1</strong>')
                .replace(/\*([^*]+)\*/g, '<em class="italic text-gray-700">$1</em>')
                .replace(/~~([^~]+)~~/g, '<del class="line-through text-gray-500">$1</del>')
                
                // Highlight personalizzato
                .replace(/==([^=]+)==/g, '<mark class="bg-yellow-200 px-1 rounded">$1</mark>');

            // Processing di liste, link e citazioni
            html = processLists(html);
            html = processLinks(html);
            html = processBlockquotes(html);

            // Ripristino delle tabelle
            processedTables.forEach((tableHtml, i) => {
                html = html.replace(`__TABLE_PLACEHOLDER_${i}__`, tableHtml);
            });

            return {
                type: 'html',
                content: html,
                index
            };
        }).filter(Boolean);
    }, [text]);

    // Callback per il rendering ottimizzato
    const renderPart = useCallback((part) => {
        if (part.type === 'abap') {
            return (
                <div key={part.index} className="preset-code-container my-6 shadow-lg rounded-lg overflow-hidden">
                    <div className="bg-gray-800 text-white px-4 py-2 text-sm font-semibold">
                        ABAP Code
                    </div>
                    <AbapCode code={part.content} />
                </div>
            );
        }

        return (
            <div 
                key={part.index} 
                className="prose prose-lg max-w-none"
                style={{ 
                    whiteSpace: 'pre-line', 
                    lineHeight: '1.8',
                    fontFamily: 'system-ui, -apple-system, sans-serif'
                }} 
                dangerouslySetInnerHTML={{ __html: part.content }} 
            />
        );
    }, []);

    if (!processedContent) {
        return (
            <div className="flex items-center justify-center py-8 text-gray-500">
                <div className="text-center">
                    <div className="text-lg font-medium">Nessun contenuto disponibile</div>
                    <div className="text-sm mt-1">Il testo fornito è vuoto o non valido</div>
                </div>
            </div>
        );
    }

    return (
        <div className="markdown-content space-y-4 p-6 bg-white rounded-lg shadow-sm">
            <style jsx>{`
                .markdown-table {
                    font-variant-numeric: tabular-nums;
                }
                
                .table-wrapper {
                    box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
                    border-radius: 0.5rem;
                    overflow: hidden;
                }
                
                .prose h1, .prose h2, .prose h3 {
                    scroll-margin-top: 2rem;
                }
                
                .prose blockquote {
                    font-style: italic;
                    position: relative;
                }
                
                .prose blockquote::before {
                    content: '"';
                    font-size: 4rem;
                    color: rgba(59, 130, 246, 0.3);
                    position: absolute;
                    left: -1rem;
                    top: -1rem;
                    font-family: serif;
                }
                
                @media (max-width: 768px) {
                    .table-wrapper {
                        margin: 1rem -1rem;
                        border-radius: 0;
                    }
                    
                    .markdown-table th,
                    .markdown-table td {
                        padding: 0.5rem 0.75rem !important;
                        font-size: 0.875rem;
                    }
                }
                
                .preset-code-container {
                    transition: all 0.3s ease;
                }
                
                .preset-code-container:hover {
                    transform: translateY(-2px);
                    box-shadow: 0 10px 25px rgba(0, 0, 0, 0.15);
                }
            `}</style>
            
            {processedContent.map(renderPart)}
        </div>
    );
};

export default MarkdownRenderer;