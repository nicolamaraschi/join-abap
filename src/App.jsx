import React, { useState, useMemo } from 'react';

import Navbar from './components/Navbar.jsx';
import Sidebar from './components/Sidebar.jsx';
import MainContent from './components/MainContent.jsx';

import { tableDataRaw } from './modules/index.js';
import { bapiData } from './modules/BapiData.jsx';
import { codePresets } from './modules/CodePresets.jsx';
import { transactionData } from './modules/TransactionData.jsx';
// L'import di AbapDocumentationData.jsx è stato rimosso

import './App.css';

// La funzione parseTableData rimane invariata...
const parseTableData = (text) => {
    const lines = text.split('\n'); let currentModule = null; let currentSubModule = null; let currentTable = null; const sapData = {}; const allTables = []; const moduleNames = {};
    const moduleRegex = /^###\s*Modulo\s*(.*?)\s*\((.*?)\)/; const subModuleRegex = /^####\s*(.*)/; const tableRegex = /^\*\*(.*?)\s*(\(.*\))?\*\*/;
    const keysRegex = /^\*\s*Chiavi Primarie:\s*`(.*?)`/; const descRegex = /^\*\s*Descrizione:\s*(.*)/; const joinRegex = /^\s{2,}\*\s*\*\*(.*?)\*\*.*:\s*su\s*`(.*?)`/;
    for (const line of lines) {
        if (line.trim() === '') continue; let match;
        match = line.match(moduleRegex); if (match) { const fullName = match[1].trim(); const abbreviation = match[2].trim(); currentModule = abbreviation; if (!sapData[currentModule]) { sapData[currentModule] = {}; moduleNames[abbreviation] = fullName; } currentSubModule = "Generale"; currentTable = null; continue; }
        match = line.match(subModuleRegex); if (match && currentModule) { currentSubModule = match[1].trim(); currentTable = null; continue; }
        match = line.match(tableRegex); if (match && currentModule) { const tableName = match[1].trim(); const tableDesc = match[2] ? match[2].replace(/[()]/g, '').trim() : ''; currentTable = { name: tableName, description: tableDesc, module: currentModule, subModule: currentSubModule, primaryKeys: [], joins: [] }; sapData[currentModule][tableName] = currentTable; allTables.push(currentTable); continue; }
        if (currentTable) {
            match = line.match(descRegex); if (match) { currentTable.description = match[1].trim(); continue; }
            match = line.match(keysRegex); if (match) { currentTable.primaryKeys = match[1].split(',').map(k => k.trim().replace(/`/g, '')); continue; }
            match = line.match(joinRegex); if (match) { const targetTable = match[1].trim(); const joinKeys = match[2].split(',').map(k => k.trim().replace(/`/g, '')); currentTable.joins.push({ table: targetTable, on: joinKeys }); }
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
                });
                table.joins = resolvedJoins;
            }
        });
    });
    return { sapData, allTables, moduleNames };
};

function App() {
    const [viewMode, setViewMode] = useState('TABLES');
    const [currentModule, setCurrentModule] = useState('All');
    const [selectedItemName, setSelectedItemName] = useState(null);
    const [searchTerm, setSearchTerm] = useState('');
    const [selectedSubgroup, setSelectedSubgroup] = useState('All');

    const { tableData, bapiDataPrepared, presetData, staticTransactionData, abapDocSections } = useMemo(() => {
        const tables = parseTableData(tableDataRaw);
        const bapis = bapiData;
        
        // Logica esistente per tabelle, bapi, etc. ...
        const tableModuleList = ['All', ...Object.keys(tables.sapData).sort()];
        const tableDisplayNames = { 'All': 'Home', ...tables.moduleNames };
        const bapiModuleList = ['All', ...Object.keys(bapis).sort()];
        const bapiDisplayNames = { 'All': 'Tutti i Moduli', ...Object.fromEntries(Object.keys(bapis).map(abbr => [abbr, tables.moduleNames[abbr] || abbr])) };
        const subgroupsMap = {};
        Object.keys(tables.sapData).forEach(moduleKey => {
            const subgroups = new Set();
            tables.allTables.forEach(table => { if (table.module === moduleKey && table.subModule) subgroups.add(table.subModule); });
            subgroupsMap[moduleKey] = Array.from(subgroups).sort();
        });
        const findTableFunc = (name) => name ? tables.allTables.find(t => t.name === name) : null;
        const findBapiFunc = (name) => {
            if (!name) return null;
            for (const moduleKey in bapis) { const found = bapis[moduleKey].find(b => b.name === name); if (found) return found; } return null;
        };
        const presets = {
            all: codePresets,
            find: (id) => id ? codePresets.find(p => p.id === id) : null
        };
        const transactionModules = transactionData.fiori.modules.map(m => ({ id: m.id, name: m.name }));
        
        // Logica semplificata per la navigazione della documentazione
        const abapDocNavData = [
            { id: "knowledge-base", title: "Sezione 1: Knowledge Base" },
            { id: "architecture", title: "Sezione 2: Architettura ABAP" },
            { id: "alv-masterclass", title: "Sezione 3: ALV Masterclass" },
            { id: "essential-fm", title: "Sezione 4: Toolkit Essenziale" },
        ];

        return {
            tableData: { all: tables.allTables, modules: tableModuleList, names: tableDisplayNames, subgroups: subgroupsMap, find: findTableFunc },
            bapiDataPrepared: { all: bapis, modules: bapiModuleList, names: bapiDisplayNames, find: findBapiFunc },
            presetData: presets,
            staticTransactionData: { ...transactionData, navModules: transactionModules },
            abapDocSections: abapDocNavData
        };
    }, []);

    // Il resto della logica del componente App rimane invariato
    const handleViewModeSelect = (mode) => {
        setViewMode(mode);
        setSelectedItemName(null);
        setSearchTerm('');
        if (mode === 'TABLES' || mode === 'BAPIS') {
            const moduleToSelect = mode === 'BAPIS' ? bapiDataPrepared.modules[1] || 'All' : 'All';
            handleModuleSelect(moduleToSelect);
        } else {
             setCurrentModule('All');
        }
    };
    const handleModuleSelect = (module) => {
        setCurrentModule(module);
        setSelectedItemName(null);
        setSearchTerm('');
        setSelectedSubgroup('All');
    };
    const filteredTables = useMemo(() => {
        if (viewMode !== 'TABLES') return [];
        const tablesToFilter = currentModule === 'All' ? tableData.all : tableData.all.filter(table => table.module === currentModule);
        return tablesToFilter.filter(table => {
            const matchesSubgroup = selectedSubgroup === 'All' || table.subModule === selectedSubgroup;
            const term = searchTerm.toLowerCase();
            const matchesSearch = searchTerm === '' || table.name.toLowerCase().includes(term) || (table.description && table.description.toLowerCase().includes(term));
            return matchesSubgroup && matchesSearch;
        });
    }, [tableData.all, currentModule, searchTerm, selectedSubgroup, viewMode]);
    const filteredBapis = useMemo(() => {
        if (viewMode !== 'BAPIS') return [];
        let bapisToFilter = currentModule === 'All' ? Object.values(bapiDataPrepared.all).flat() : (bapiDataPrepared.all[currentModule] || []);
        if (!searchTerm) return bapisToFilter;
        return bapisToFilter.filter(b => b.name.toLowerCase().includes(searchTerm.toLowerCase()) || (b.description && b.description.toLowerCase().includes(searchTerm.toLowerCase())));
    }, [bapiDataPrepared.all, currentModule, searchTerm, viewMode]);
    const filteredPresets = useMemo(() => {
        if (viewMode !== 'PRESETS') return [];
        if (!searchTerm) return presetData.all;
        return presetData.all.filter(p => p.title.toLowerCase().includes(searchTerm.toLowerCase()));
    }, [presetData.all, searchTerm, viewMode]);

    const selectedTable = viewMode === 'TABLES' ? tableData.find(selectedItemName) : null;
    const selectedBapi = viewMode === 'BAPIS' ? bapiDataPrepared.find(selectedItemName) : null;
    const selectedPreset = viewMode === 'PRESETS' ? presetData.find(selectedItemName) : null;

    return (
        <div className="app-layout">
            <Navbar
                viewMode={viewMode}
                onViewModeSelect={handleViewModeSelect}
                modules={viewMode === 'TABLES' ? tableData.modules : bapiDataPrepared.modules}
                moduleNames={viewMode === 'TABLES' ? tableData.names : bapiDataPrepared.names}
                currentModule={currentModule}
                onModuleSelect={handleModuleSelect}
                subgroups={viewMode === 'TABLES' ? tableData.subgroups[currentModule] : null}
                selectedSubgroup={selectedSubgroup}
                onSubgroupSelect={setSelectedSubgroup}
            />
            <div className="main-view">
                <Sidebar
                    viewMode={viewMode}
                    searchTerm={searchTerm}
                    onSearchChange={setSearchTerm}
                    tables={filteredTables}
                    bapis={filteredBapis}
                    presets={filteredPresets}
                    transactionModules={staticTransactionData.navModules}
                    abapDocSections={abapDocSections}
                    onSelectItem={setSelectedItemName}
                />
                <MainContent
                    viewMode={viewMode}
                    selectedTable={selectedTable}
                    selectedBapi={selectedBapi}
                    selectedPreset={selectedPreset}
                    transactionData={staticTransactionData}
                    allTables={tableData.all}
                    onSelectTable={setSelectedItemName}
                />
            </div>
        </div>
    );
}

export default App;
