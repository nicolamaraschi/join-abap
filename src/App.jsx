import React, { useState, useMemo } from 'react';

// Importa i nuovi componenti
import Navbar from './components/Navbar.jsx';
import Sidebar from './components/Sidebar.jsx';
import MainContent from './components/MainContent.jsx'; // Assumendo che questo esista già

// Importa i dati (presumendo un file aggregatore in /modules)
import { coData } from './modules/CO.jsx';
import { fiData } from './modules/FI.jsx';
import { mmData } from './modules/MM.jsx';
import { pmData } from './modules/PM.jsx';
import { ppData } from './modules/PP.jsx';
import { psData } from './modules/PS.jsx';
import { qmData } from './modules/QM.jsx';
import { sdData } from './modules/SD.jsx';

import './App.css';

// --- LOGICA DI PARSING (inclusa nel file per semplicità) ---
const sapDataRaw = [fiData, coData, sdData, mmData, ppData, pmData, psData, qmData].join('\n');
const parseSapData = (text) => {
    const lines = text.split('\n');
    let currentModule = null;
    let currentSubModule = null;
    let currentTable = null;
    const sapData = {};
    const allTables = [];
    const moduleRegex = /^###\s*Modulo\s*(.*?)\s*\((.*?)\)/;
    const subModuleRegex = /^####\s*(.*)/;
    const tableRegex = /^\*\*(.*?)\s*(\((.*?)\))?\*\*/;
    const keysRegex = /^\*\s*Chiavi Primarie:\s*`(.*?)`/;
    const descRegex = /^\*\s*Descrizione:\s*(.*)/;
    const joinRegex = /^\s{2,}\*\s*\*\*(.*?)\*\*.*:\s*su\s*`(.*?)`/;

    for (const line of lines) {
        if (line.trim() === '') continue;
        let match;
        match = line.match(moduleRegex);
        if (match) {
            currentModule = match[2].trim();
            if (!sapData[currentModule]) sapData[currentModule] = {};
            currentSubModule = "Generale"; currentTable = null; continue;
        }
        match = line.match(subModuleRegex);
        if (match && currentModule) {
            currentSubModule = match[1].trim(); currentTable = null; continue;
        }
        match = line.match(tableRegex);
        if (match && currentModule) {
            const tableName = match[1].trim();
            const tableDesc = match[3] ? match[3].trim() : '';
            currentTable = { name: tableName, description: tableDesc, module: currentModule, subModule: currentSubModule, primaryKeys: [], joins: [] };
            sapData[currentModule][tableName] = currentTable;
            allTables.push(currentTable);
            continue;
        }
        if (currentTable) {
            match = line.match(descRegex);
            if (match) { currentTable.description = match[1].trim(); continue; }
            match = line.match(keysRegex);
            if (match) { currentTable.primaryKeys = match[1].split(',').map(k => k.trim().replace(/`/g, '')); continue; }
            match = line.match(joinRegex);
            if (match) {
                const targetTable = match[1].trim();
                const joinKeys = match[2].split(',').map(k => k.trim().replace(/`/g, ''));
                currentTable.joins.push({ table: targetTable, on: joinKeys });
            }
        }
    }
    
    Object.values(sapData).forEach(moduleContent => {
        Object.values(moduleContent).forEach(table => {
            if (table.joins) {
                const resolvedJoins = [];
                table.joins.forEach(join => {
                    let target = null;
                    for (const moduleKey in sapData) { if (sapData[moduleKey] && sapData[moduleKey][join.table]) { target = sapData[moduleKey][join.table]; break; } }
                    if (target) resolvedJoins.push({ table: target, on: join.on });
                    else resolvedJoins.push({ table: { name: join.table, description: "Non trovato/definito", module:"N/A", primaryKeys:[] }, on: join.on });
                });
                table.joins = resolvedJoins;
            }
        });
    });
    return { sapData, allTables };
};


function App() {
    const [selectedTableName, setSelectedTableName] = useState(null);
    const [searchTerm, setSearchTerm] = useState('');
    const [currentModule, setCurrentModule] = useState('All');

    const { allTables, modules, findTable } = useMemo(() => {
        const parsed = parseSapData(sapDataRaw);
        const findFunc = (tableName) => tableName ? parsed.allTables.find(t => t.name === tableName) || null : null;
        const moduleList = ['All', ...Object.keys(parsed.sapData).sort()];
        return { ...parsed, modules: moduleList, findTable: findFunc };
    }, []);

    const filteredTables = useMemo(() => {
        if (currentModule === 'All') {
            return searchTerm ? allTables.filter(table => table.name.toLowerCase().includes(searchTerm.toLowerCase())) : allTables;
        }
        return allTables.filter(table => {
            const matchesModule = table.module === currentModule;
            const matchesSearch = table.name.toLowerCase().includes(searchTerm.toLowerCase()) || (table.description && table.description.toLowerCase().includes(searchTerm.toLowerCase()));
            return matchesModule && matchesSearch;
        });
    }, [allTables, currentModule, searchTerm]);

    const handleModuleSelect = (module) => {
        setCurrentModule(module);
        setSelectedTableName(null);
        setSearchTerm('');
    };
  
    const selectedTable = useMemo(() => findTable(selectedTableName), [selectedTableName, findTable]);

    return (
        <div className="app-layout">
            <Navbar
                modules={modules}
                currentModule={currentModule}
                onModuleSelect={handleModuleSelect}
            />
            <div className="main-view">
                <Sidebar
                    searchTerm={searchTerm}
                    onSearchChange={setSearchTerm}
                    tables={filteredTables}
                    onSelectTable={setSelectedTableName}
                />
                <MainContent
                    selectedTable={selectedTable}
                    allTables={allTables}
                    onSelectTable={setSelectedTableName}
                />
            </div>
        </div>
    );
}

export default App;