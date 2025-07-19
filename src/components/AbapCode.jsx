import React from 'react';

// ==================================================================
// SEZIONE 1: FUNZIONE DI EVIDENZIAZIONE DELLA SINTASSI (CORRETTA)
// ==================================================================

/**
 * Evidenzia la sintassi di una stringa di codice ABAP.
 * @param {string} code - La stringa di codice ABAP grezzo.
 * @returns {string} - Una stringa HTML con la sintassi evidenziata.
 */
const highlightAbapSyntax = (code) => {
  if (!code) return '';

  // Pulisce il codice da eventuali escape HTML preesistenti per partire da una base pulita.
  const unescapedCode = code
    .replace(/&lt;/g, '<')
    .replace(/&gt;/g, '>')
    .replace(/&amp;/g, '&');

  const lines = unescapedCode.split('\n');

  // Funzione interna per evidenziare keyword e stringhe
  const highlightKeywordsAndStrings = (part) => {
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
    const strings = [];
    let tempPart = part.replace(/'[^']*'/g, (match) => {
        strings.push(`<span style="color: #ce9178;">${match}</span>`);
        return `__STRING_PLACEHOLDER_${strings.length - 1}__`;
    });
    tempPart = tempPart.replace(keywordRegex, '<span style="color: #569cd6; font-weight: bold;">$1</span>');
    for (let i = 0; i < strings.length; i++) {
        tempPart = tempPart.replace(`__STRING_PLACEHOLDER_${i}__`, strings[i]);
    }
    return tempPart;
  };
  
  const highlightedLines = lines.map((line) => {
    // 1. Isola TUTTI i costrutti speciali (<...>, <>, ->, =>, @<...>) E gli operatori di comparazione
    const placeholders = [];
    const specialTokensRegex = /=>|->|<>|<[^>]+>|@<[^>]+>|(?<!\w)([<>]=?)(?!\w)/g;
    let tempLine = line.replace(specialTokensRegex, (match) => {
        placeholders.push({ original: match, isOperator: /^[<>]=?$/.test(match) });
        return `__SPECIAL_PLACEHOLDER_${placeholders.length - 1}__`;
    });

    // 2. Escape HTML del resto della riga
    let processedLine = tempLine.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');

    // 3. Evidenziazione dei commenti e del codice
    if (processedLine.trim().startsWith('*')) {
        processedLine = `<span style="color: #6a9955; font-style: italic;">${processedLine}</span>`;
    } else {
        const commentIndex = processedLine.indexOf('"');
        if (commentIndex !== -1) {
            const codePart = processedLine.substring(0, commentIndex);
            const commentPart = processedLine.substring(commentIndex);
            const highlightedCodePart = highlightKeywordsAndStrings(codePart);
            processedLine = `${highlightedCodePart}<span style="color: #6a9955; font-style: italic;">${commentPart}</span>`;
        } else {
            processedLine = highlightKeywordsAndStrings(processedLine);
        }
    }

    // 4. Ripristina i placeholder con lo stile corretto
    placeholders.forEach((placeholder, index) => {
        let replacement;
        const p = placeholder.original;
        
        if (placeholder.isOperator) {
            // Gestione operatori di comparazione
            replacement = `<span style="color: #d4d4d4; font-weight: bold;">${p}</span>`;
        } else {
            // Gestione costrutti ABAP speciali
            const escapedValue = p.replace(/</g, '&lt;').replace(/>/g, '&gt;');

            if (p.startsWith('@<')) {
                const at = '<span style="color: #569cd6; font-weight: bold;">@</span>';
                const symbol = p.substring(1);
                const escapedSymbol = symbol.replace(/</g, '&lt;').replace(/>/g, '&gt;');
                replacement = `${at}<span style="color: #4ec9b0; font-weight: bold;">${escapedSymbol}</span>`;
            } else if (p === '<>' || p === '->' || p === '=>') {
                replacement = `<span style="color: #d4d4d4;">${escapedValue}</span>`;
            } else if (p.startsWith('<')) {
                replacement = `<span style="color: #4ec9b0; font-weight: bold;">${escapedValue}</span>`;
            } else {
                replacement = escapedValue; // Fallback di sicurezza
            }
        }
        
        processedLine = processedLine.replace(`__SPECIAL_PLACEHOLDER_${index}__`, replacement);
    });

    return processedLine;
  });

  return highlightedLines.join('\n');
};


// ==================================================================
// SEZIONE 2: COMPONENTE REACT
// ==================================================================

/**
 * Un componente React per visualizzare codice ABAP con syntax highlighting.
 * @param {object} props
 * @param {string} props.code - Il codice ABAP da visualizzare.
 */
const AbapCode = ({ code }) => {
  const [copied, setCopied] = React.useState(false);
  
  // Applica la funzione di evidenziazione al codice ricevuto come prop
  const highlightedCodeHtml = highlightAbapSyntax(code);

  // Funzione per copiare il codice negli appunti
  const handleCopyCode = async () => {
    try {
      await navigator.clipboard.writeText(code);
      setCopied(true);
      // Reset del feedback dopo 2 secondi
      setTimeout(() => setCopied(false), 2000);
    } catch (err) {
      console.error('Errore nel copiare il codice:', err);
      // Fallback per browser più vecchi
      const textArea = document.createElement('textarea');
      textArea.value = code;
      document.body.appendChild(textArea);
      textArea.select();
      document.execCommand('copy');
      document.body.removeChild(textArea);
      setCopied(true);
      setTimeout(() => setCopied(false), 2000);
    }
  };

  // Stile del contenitore principale
  const containerStyle = {
    position: 'relative',
    backgroundColor: '#1e1e1e',
    borderRadius: '8px',
    overflow: 'hidden',
  };

  // Stile del pulsante copy
  const copyButtonStyle = {
    position: 'absolute',
    top: '8px',
    right: '8px',
    backgroundColor: copied ? '#4CAF50' : '#333',
    color: '#fff',
    border: 'none',
    borderRadius: '4px',
    padding: '6px 10px',
    cursor: 'pointer',
    fontSize: '12px',
    display: 'flex',
    alignItems: 'center',
    gap: '4px',
    transition: 'all 0.2s ease',
    zIndex: 1,
  };

  // Stile del contenitore <pre> per un aspetto professionale (dark theme)
  const preStyle = {
    backgroundColor: '#1e1e1e',
    color: '#d4d4d4',
    padding: '1rem',
    paddingTop: '2.5rem', // Spazio per il pulsante copy
    borderRadius: '8px',
    overflowX: 'auto',
    fontFamily: "Consolas, 'Courier New', monospace",
    fontSize: '14px',
    whiteSpace: 'pre-wrap', // Permette al testo di andare a capo
    wordWrap: 'break-word', // Forza a capo le parole lunghe
    margin: 0,
  };

  // Icona copy (SVG semplice)
  const copyIcon = (
    <svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
      <rect x="9" y="9" width="13" height="13" rx="2" ry="2"></rect>
      <path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"></path>
    </svg>
  );

  // Icona check (per il feedback di successo)
  const checkIcon = (
    <svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2">
      <polyline points="20,6 9,17 4,12"></polyline>
    </svg>
  );

  return (
    <div style={containerStyle}>
      <button
        onClick={handleCopyCode}
        style={copyButtonStyle}
        title={copied ? 'Copiato!' : 'Copia codice'}
        onMouseEnter={(e) => {
          if (!copied) {
            e.target.style.backgroundColor = '#444';
          }
        }}
        onMouseLeave={(e) => {
          if (!copied) {
            e.target.style.backgroundColor = '#333';
          }
        }}
      >
        {copied ? checkIcon : copyIcon}
        {copied ? 'Copiato!' : 'Copy'}
      </button>
      <pre style={preStyle}>
        <code dangerouslySetInnerHTML={{ __html: highlightedCodeHtml }} />
      </pre>
    </div>
  );
};

export default AbapCode;