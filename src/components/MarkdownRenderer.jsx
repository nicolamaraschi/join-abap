import React from 'react';
import AbapCode from './AbapCode.jsx';

const MarkdownRenderer = ({ text }) => {
    if (!text) {
        return null;
    }

    // Funzione per processare le tabelle Markdown
    const processTables = (htmlContent) => {
        const tableRegex = /^((?:\|.*\|[\r\n]?)+)/gm;
        return htmlContent.replace(tableRegex, (tableBlock) => {
            const lines = tableBlock.trim().split('\n');
            if (lines.length < 2 || !lines[1].match(/^[| :\-~]+$/)) {
                return tableBlock;
            }

            let tableHtml = '<table class="markdown-table">';
            const headers = lines[0].split('|').map(h => h.trim()).slice(1, -1);
            tableHtml += '<thead><tr>';
            headers.forEach(header => { tableHtml += `<th>${header}</th>`; });
            tableHtml += '</tr></thead>';

            tableHtml += '<tbody>';
            lines.slice(2).forEach(rowLine => {
                const cells = rowLine.split('|').map(c => c.trim()).slice(1, -1);
                tableHtml += '<tr>';
                for (let i = 0; i < headers.length; i++) {
                    const cellContent = (cells[i] || '').replace(/`([^`]+)`/g, '<code class="bg-slate-200 text-slate-800 px-2 py-1 rounded text-sm font-mono">$1</code>');
                    tableHtml += `<td>${cellContent}</td>`;
                }
                tableHtml += '</tr>';
            });
            tableHtml += '</tbody></table>';
            
            return tableHtml;
        });
    };

    const parts = text.split(/(```abap[\s\S]*?```)/g);

    const renderedParts = parts.map((part, index) => {
        if (part.startsWith('```abap')) {
            const code = part.replace(/^```abap\n?|\n?```$/g, '');
            return (
                <div key={index} className="preset-code-container my-4">
                    <AbapCode code={code} />
                </div>
            );
        }

        if (part.trim() === '') {
            return null;
        }
        
        let html = processTables(part);
        
        const processedTables = [];
        html = html.replace(/<table[\s\S]*?<\/table>/g, (match) => {
            processedTables.push(match);
            return `__TABLE_PLACEHOLDER_${processedTables.length - 1}__`;
        });
        
        html = html
            .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            // CORREZIONE: Uso $1 per catturare solo il testo del titolo, non l'intera riga.
            .replace(/^Guida Tecnica.*/gm, (match) => `<h1 class="text-3xl font-bold mb-4">${match}</h1>`)
            .replace(/^Sezione \d+: (.*)/gm, '<h2 class="text-2xl font-semibold mt-8 mb-4 pb-2 border-b">$1</h2>')
            .replace(/^### (.*)/gm, '<h3 class="text-xl font-semibold mt-6 mb-3">$1</h3>')
            .replace(/^====*$/gm, '<hr class="my-6"/>')
            .replace(/^----*$/gm, '<hr class="my-4"/>')
            .replace(/`([^`]+)`/g, '<code class="bg-slate-200 text-slate-800 px-2 py-1 rounded text-sm font-mono">$1</code>')
            .replace(/\*\*(.*?)\*\*/g, '<strong>$1</strong>');

        processedTables.forEach((tableHtml, i) => {
            html = html.replace(`__TABLE_PLACEHOLDER_${i}__`, tableHtml);
        });

        return (
            <div 
                key={index} 
                style={{ whiteSpace: 'pre-line', lineHeight: '1.7' }} 
                dangerouslySetInnerHTML={{ __html: html }} 
            />
        );
    });

    return <div>{renderedParts}</div>;
};

export default MarkdownRenderer;