import React from 'react';

const AbapCode = ({ code }) => {
  // Funzione che applica la colorazione della sintassi al testo ABAP
  const highlightAbap = (text) => {
    // Lista di keyword ABAP comuni (non esaustiva ma copre i casi principali)
    const keywords = [
        // Statements and control structures
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
        'VALUE', 'VALUE-REQUEST', 'WAIT', 'WHEN', 'WHILE', 'WITH', 'WORK', 'WRITE',
        
        // Additional ABAP keywords
        'ABSTRACT', 'ACCEPTING', 'AFTER', 'ALIAS', 'ALIASES', 'ALL', 'ANALYZE', 'AND', 'ANY',
        'AS', 'ASCENDING', 'ASSERT', 'ASSOCIATE', 'ASYNCHRONOUS', 'BACK', 'BACKUP', 'BEFORE',
        'BETWEEN', 'BINARY', 'BIT', 'BLANK', 'BLOCKS', 'BOUND', 'BY', 'BYPASSING', 'CASTING',
        'CATCH', 'CENTURY', 'CHAIN', 'CLOSE', 'COLLECT', 'COMMENT', 'COMMUNICATION', 'COMPARE',
        'COMPONENTS', 'COMPUTE', 'CONCATENATE', 'CONDENSE', 'CONNECT', 'CONSIDERING', 'CONTEXT',
        'CONTROLS', 'CONVERT', 'COPY', 'CORRESPONDING', 'COUNT', 'COUNTRY', 'CURRENCY', 'CURSOR',
        'DATASET', 'DAYLIGHT', 'DECIMALS', 'DECODE', 'DEFERRED', 'DEMAND', 'DEPENDING', 'DESCENDING',
        'DESTINATION', 'DETAIL', 'DIALOG', 'DIVIDE', 'DUPLICATES', 'DYNAMIC', 'EDIT', 'EDITOR-CALL',
        'EMPTY', 'ENABLING', 'ENCODING', 'END', 'ENDCHAIN', 'ENDIAN', 'ENDPROVIDE', 'ENDWHILE',
        'ENTRIES', 'EQ', 'ERROR', 'ESCAPE', 'EXACT', 'EXCEPT', 'EXCEPTION', 'EXCEPTIONS', 'EXEC',
        'EXECUTE', 'EXPAND', 'EXPERIMENTAL', 'EXTENDED', 'EXTENSION', 'EXTRACT', 'FETCH', 'FIELDS',
        'FILE', 'FILTER', 'FINAL', 'FIRST', 'FIXED-POINT', 'FLUSH', 'FOR', 'FRACTIONAL', 'FUNCTION-POOL',
        'GE', 'GT', 'HANDLER', 'HARMLESS', 'HEADER', 'HELP-ID', 'HELP-REQUEST', 'HIGH', 'HOLD',
        'HOTSPOT', 'ICON', 'ID', 'IGNORING', 'IMMEDIATELY', 'IMPLEMENTATION', 'IMPLEMENTED', 'IMPLIED',
        'IN', 'INCREMENT', 'INDEX', 'INFOTYPES', 'INHERITING', 'INITIAL', 'INNER', 'INPUT', 'INSTANCE',
        'INSTANCES', 'INTERFACE', 'INTERFACES', 'INTERVALS', 'INVERTED-DATE', 'IS', 'ISOLATION',
        'LANGUAGE', 'LAST', 'LATE', 'LEADING', 'LEFT', 'LEFT-JUSTIFIED', 'LENGTH', 'LEVEL', 'LINES',
        'LIST', 'LISTBOX', 'LOAD-OF-PROGRAM', 'LOCAL', 'LOCALE', 'LOCK', 'LOGICAL', 'LOW', 'LOWER',
        'MARGIN', 'MASK', 'MATCH', 'MAXIMUM', 'MEMORY', 'MESH', 'MINIMUM', 'MODIF', 'MULTIPLY',
        'NESTED', 'NO', 'NO-DISPLAY', 'NO-EXTENSION', 'NO-GAP', 'NO-GAPS', 'NO-HEADING',
        'NO-SCROLLING', 'NO-SIGN', 'NO-TITLE', 'NO-TOPOFPAGE', 'NO-ZERO', 'NODE', 'NODES',
        'NON-UNIQUE', 'NOT', 'NULL', 'NUMBER', 'OBJECT', 'OCCURRENCE', 'OCCURS', 'OPEN', 'OPTION',
        'OPTIONS', 'OR', 'ORDER', 'OTHER', 'OTHERS', 'OUTPUT', 'OVERLAY', 'PACKAGE', 'PAGE',
        'PARAMETER', 'PARTIALLY', 'PATTERN', 'PERCENTAGE', 'POSITION', 'PREFERRED', 'PRESERVING',
        'PRIMARY', 'PRINT', 'PRINT-CONTROL', 'PRIVATE', 'PROCEDURE', 'PROCESS', 'PROGRAM',
        'PROPERTY', 'PROTECTED', 'PUBLIC', 'PUSHBUTTON', 'PUT', 'QUEUE-ONLY', 'QUICKINFO',
        'RADIOBUTTON', 'RAISING', 'RANGE', 'READ', 'READER', 'RECEIVING', 'REDEFINITION',
        'REDUCING', 'REFERENCE', 'REGEX', 'REMOTE', 'RENAMING', 'REPEAT', 'REPLACING', 'RESERVED',
        'RESET', 'RESPECTING', 'RESUMABLE', 'RIGHTJUSTIFIED', 'RISK', 'ROUND', 'ROWS', 'RUN',
        'SAVING', 'SCALE', 'SCAN', 'SCIENTIFIC', 'SCREEN', 'SECONDARY', 'SECTION', 'SEPARATED',
        'SHARED', 'SIGN', 'SINGLE', 'SIZE', 'SKIP-FIRST', 'SOME', 'SORTED', 'STABLE', 'STANDARD',
        'STARTING', 'STATE', 'STATEMENT', 'STRUCTURE', 'STYLE', 'SUBKEY', 'SUBSCREEN', 'SUPPLY',
        'SWITCH', 'SYMBOL', 'SYNTAX', 'SYSTEM', 'TABBED', 'TEXTPOOL', 'THEN', 'THROW', 'TIME',
        'TIMESTAMP', 'TIMEZONE', 'TITLE-LINES', 'TOP-LINES', 'TOP-OF-PAGE', 'TRAILING', 'TRANSPORTING',
        'TRUNCATE', 'TRUNCATION', 'TRYING', 'UNIQUE', 'UNIT', 'UNIX', 'UNWIND', 'UP', 'UPPER',
        'USER', 'USER-COMMAND', 'USING', 'UTF-8', 'VALID', 'VARIANT', 'VARYING', 'VERSION',
        'VIA', 'VISIBLE', 'WARNING', 'WHERE', 'WIDTH', 'WINDOW', 'WRITE-ONLY', 'ZERO', 'ZONE',
        'FIELD',
        
        // Data types
        'C', 'D', 'F', 'I', 'N', 'P', 'STRING', 'T', 'X', 'XSTRING',
        
        // Operators and logical keywords
        'CA', 'CN', 'CO', 'CP', 'CS', 'EQ', 'GE', 'GT', 'LE', 'LT', 'NA', 'NE', 'NP', 'NS',
        'ABS', 'ACOS', 'ASIN', 'ATAN', 'CEIL', 'COS', 'COSH', 'EXP', 'FLOOR', 'FRAC', 'LOG',
        'LOG10', 'ROUND', 'SIGN', 'SIN', 'SINH', 'SQRT', 'TAN', 'TANH', 'TRUNC',
        
        // Special forms and constructs
        'AUTHORITY', 'BREAK', 'CHECKPOINT', 'COMMIT', 'DISPLAY', 'EXPORT', 'FORM', 'FUNCTION',
        'INTERFACE', 'REPORT', 'ROLLBACK', 'SELECTION', 'TITLE', 'TOP-OF-PAGE', 'TRANSACTION'
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
        const placeholders = [];
        // Preserve special constructs before escaping
        let tempLine = line.replace(/<[^>]+>|->|@<[^>]+>/g, (match) => {
            placeholders.push(match);
            return `__PLACEHOLDER_${placeholders.length - 1}__`;
        });

        // General escape for the rest of the line
        let processedLine = tempLine.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');

        // Highlighting for comments and keywords
        if (processedLine.trim().startsWith('*')) {
            processedLine = `<span class="abap-comment">${processedLine}</span>`;
        } else {
            const commentIndex = processedLine.indexOf('"');
            if (commentIndex !== -1) {
                const codePart = processedLine.substring(0, commentIndex);
                const commentPart = processedLine.substring(commentIndex);
                const highlightedCodePart = highlightPart(codePart);
                processedLine = `${highlightedCodePart}<span class="abap-comment">${commentPart}</span>`;
            } else {
                processedLine = highlightPart(processedLine);
            }
        }

        // Restore placeholders with appropriate styling
        placeholders.forEach((p, index) => {
            let replacement;
            if (p.startsWith('@<')) {
                const at = '<span class="abap-keyword">@</span>';
                const symbol = p.substring(1); // The <...> part
                const escapedSymbol = symbol.replace(/</g, '&lt;').replace(/>/g, '&gt;');
                replacement = `${at}<span class="abap-field-symbol">${escapedSymbol}</span>`;
            } else if (p.startsWith('<')) {
                const escapedValue = p.replace(/</g, '&lt;').replace(/>/g, '&gt;');
                replacement = `<span class="abap-field-symbol">${escapedValue}</span>`;
            } else { // This is '->'
                replacement = p; // Put it back as is
            }
            processedLine = processedLine.replace(`__PLACEHOLDER_${index}__`, replacement);
        });

        return processedLine;
    });

    return highlightedLines.join('\n');
  };
  
  const highlightedCode = highlightAbap(code);

  return (
    <pre><code dangerouslySetInnerHTML={{ __html: highlightedCode }} /></pre>
  );
};

export default AbapCode;