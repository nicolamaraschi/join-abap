import React from 'react';

const AbapCode = ({ code }) => {
  // Funzione che applica la colorazione della sintassi al testo ABAP
  const highlightAbap = (text) => {
    // Lista di keyword ABAP comuni (non esaustiva ma copre i casi principali)
    const keywords = [
        'ADD', 'APPEND', 'ASSIGN', 'AT', 'AUTHORITY-CHECK', 'BEGIN', 'BLOCK', 'BREAK-POINT', 'CALL', 
        'CASE', 'CATCH', 'CHANGING', 'CHECK', 'CLASS', 'CLEAR', 'COMMIT', 'CONSTANTS', 'CONTINUE', 
        'CREATE', 'DATA', 'DEFINE', 'DELETE', 'DESCRIBE', 'DO', 'ELSE', 'ELSEIF', 'ENDAT', 
        'ENDCASE', 'ENDCLASS', 'ENDDO', 'ENDFORM', 'ENDFUNCTION', 'ENDIF', 'ENDLOOP', 'ENDMETHOD', 
        'ENDMODULE', 'ENDSELECT', 'END-OF-PAGE', 'END-OF-SELECTION', 'ENDTRY', 'EVENTS', 'EXIT', 
        'EXPORT', 'EXPORTING', 'FIELD-SYMBOLS', 'FIND', 'FORM', 'FORMAT', 'FRAME', 'FREE', 'FROM', 
        'FUNCTION', 'GENERATE', 'GET', 'HIDE', 'IF', 'IMPORT', 'IMPORTING', 'INCLUDE', 'INITIALIZATION', 
        'INSERT', 'INTO', 'LEAVE', 'LIKE', 'LOAD', 'LOOP', 'MESSAGE', 'METHOD', 'MODIFY', 'MODULE', 
        'MOVE', 'NEW-LINE', 'NEW-PAGE', 'OBLIGATORY', 'OF', 'ON', 'PACK', 'PARAMETERS', 'PERFORM', 
        'PROVIDE', 'RAISE', 'RANGES', 'RECEIVE', 'REF', 'REFRESH', 'REJECT', 'REPLACE', 'REPORT', 
        'RETRY', 'RETURN', 'ROLLBACK', 'SCROLL', 'SEARCH', 'SELECT', 'SELECT-OPTIONS', 
        'SELECTION-SCREEN', 'SET', 'SHIFT', 'SKIP', 'SORT', 'SPLIT', 'START-OF-SELECTION', 'STATICS', 
        'STOP', 'SUBMIT', 'SUBTRACT', 'SUM', 'SUMMARY', 'SUPPRESS', 'TABLE', 'TABLES', 'TIMES', 
        'TITLE', 'TO', 'TRANSFER', 'TRANSLATE', 'TRY', 'TYPE', 'TYPES', 'ULINE', 'UNPACK', 'UPDATE', 
        'VALUE', 'VALUE-REQUEST', 'WAIT', 'WHEN', 'WHILE', 'WITH', 'WORK', 'WRITE'
    ];
    const keywordRegex = new RegExp(`\\b(${keywords.join('|')})\\b`, 'gi');

    const lines = text.split('\n');

    // Funzione interna per evidenziare keyword e stringhe
    const highlightPart = (part) => {
        const strings = [];
        // Sostituisce temporaneamente le stringhe con placeholder
        let tempPart = part.replace(/'[^']*'/g, (match) => {
            strings.push(`<span class="abap-string">${match}</span>`);
            return `__STRING_PLACEHOLDER_${strings.length - 1}__`;
        });
        
        // Evidenzia le keyword
        tempPart = tempPart.replace(keywordRegex, '<span class="abap-keyword">$1</span>');
        
        // Ri-sostituisce i placeholder con le stringhe evidenziate
        for (let i = 0; i < strings.length; i++) {
            tempPart = tempPart.replace(`__STRING_PLACEHOLDER_${i}__`, strings[i]);
        }
        return tempPart;
    };
    
    const highlightedLines = lines.map(line => {
        let processedLine = line.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
        
        // Gestisce i commenti a riga intera
        if (processedLine.trim().startsWith('*')) {
            return `<span class="abap-comment">${processedLine}</span>`;
        }
        
        // Gestisce i commenti inline
        const commentIndex = processedLine.indexOf('"');
        if (commentIndex !== -1) {
            const codePart = processedLine.substring(0, commentIndex);
            const commentPart = processedLine.substring(commentIndex);
            const highlightedCodePart = highlightPart(codePart);
            return `${highlightedCodePart}<span class="abap-comment">${commentPart}</span>`;
        }
        
        return highlightPart(processedLine);
    });

    return highlightedLines.join('\n');
  };
  
  const highlightedCode = highlightAbap(code);

  return (
    <pre><code dangerouslySetInnerHTML={{ __html: highlightedCode }} /></pre>
  );
};

export default AbapCode;