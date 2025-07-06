// src/App.jsx
import React, { useState, useMemo } from 'react';

import Navbar from './components/Navbar.jsx';
import Sidebar from './components/Sidebar.jsx';
import MainContent from './components/MainContent.jsx';

import { tableDataRaw } from './modules/index.js';
import { bapiData } from './modules/BapiData.jsx';
import { codePresets } from './modules/CodePresets.jsx';
import { transactionData } from './modules/TransactionData.jsx';
import { cdsData } from './modules/CdsData.jsx'; // cdsData ora è un array
import { abapDocData } from './modules/doc_abap/AbapDocData.jsx';
import { badiData } from './modules/BadiData.jsx';
import { smartformData } from './modules/SmartformData.jsx';
import { adobeformData } from './modules/AdobeformData.jsx';
import { fioriPresetsData } from './modules/FioriPresetsData.jsx';

import './App.css';

// Inserisci questo codice in src/App.jsx, sostituendo la vecchia funzione parseTableData

// Inserisci questo codice in src/App.jsx, sostituendo la vecchia funzione parseTableData

// Inserisci questo codice in src/App.jsx, sostituendo la funzione parseTableData esistente

const parseTableData = (text) => {
    const lines = text.split('\n');
    let currentModule = null;
    let currentSubModule = null;
    let currentTable = null;
    const sapData = {};
    const allTables = [];
    const moduleNames = {};

    const moduleRegex = /^###\s*Modulo\s*(.*?)\s*\((.*?)\)/;
    const subModuleRegex = /^####\s*(.*)/;
    const tableRegex = /^\*\*(.*?)\s*(\(.*\))?\*\*/;
    const keysRegex = /^\*\s*Chiavi Primarie:\s*`(.*?)`/;
    const descRegex = /^\*\s*Descrizione:\s*(.*)/;
    const joinRegex = /^\s{2,}\*\s*(Possibili Join:|)\s*\*\*(.*?)\*\*\s*.*:\s*su\s*`(.*?)`/;

    // FASE 1: Lettura e creazione degli oggetti tabella base
    for (const line of lines) {
        if (line.trim() === '') continue;
        let match;

        if ((match = line.match(moduleRegex))) {
            const [, fullName, abbreviation] = match;
            currentModule = abbreviation.trim();
            if (!sapData[currentModule]) {
                sapData[currentModule] = {};
                moduleNames[currentModule] = fullName.trim();
            }
            currentSubModule = "Generale";
            currentTable = null;
            continue;
        }

        if ((match = line.match(subModuleRegex))) {
            currentSubModule = match[1].trim();
            currentTable = null;
            continue;
        }

        if ((match = line.match(tableRegex))) {
            const tableName = match[1].trim();
            const tableDesc = match[2] ? match[2].replace(/[()]/g, '').trim() : '';
            currentTable = { name: tableName, description: tableDesc, module: currentModule, subModule: currentSubModule, primaryKeys: [], joins: [] };
            sapData[currentModule][tableName] = currentTable;
            allTables.push(currentTable);
            continue;
        }

        if (currentTable) {
            if ((match = line.match(descRegex))) {
                currentTable.description = match[1].trim();
                continue;
            }
            if ((match = line.match(keysRegex))) {
                currentTable.primaryKeys = match[1].split(',').map(k => k.trim().replace(/`/g, ''));
                continue;
            }
            if ((match = line.match(joinRegex))) {
                const targetTable = match[2].trim();
                const joinKeys = match[3].split(',').map(k => k.trim().replace(/`/g, ''));
                currentTable.joins.push({ table: targetTable, on: joinKeys });
            }
        }
    }

    // FASE 2: Risoluzione dei join con creazione di segnaposto
    allTables.forEach(table => {
        if (table.joins && table.joins.length > 0) {
            const resolvedJoins = [];
            table.joins.forEach(join => {
                const target = allTables.find(t => t.name === join.table);
                if (target) {
                    // Se la tabella di destinazione esiste, crea il link normale
                    resolvedJoins.push({ table: target, on: join.on });
                } else {
                    // --- NUOVA LOGICA ---
                    // Se la tabella di destinazione NON esiste, crea un oggetto segnaposto
                    const dummyTarget = {
                        name: join.table,
                        description: 'Definizione non trovata.',
                        primaryKeys: [],
                        joins: []
                    };
                    resolvedJoins.push({ table: dummyTarget, on: join.on });
                }
            });
            table.joins = resolvedJoins;
        }
    });

    return { sapData, allTables, moduleNames };
};

function App() {
    const [viewMode, setViewMode] = useState('TABLES');
    const [currentModule, setCurrentModule] = useState('All');
    const [selectedItemName, setSelectedItemName] = useState(null);
    const [searchTerm, setSearchTerm] = useState('');
    const [selectedSubgroup, setSelectedSubgroup] = useState('All');

    const {
        tableData,
        bapiDataPrepared,
        cdsDataPrepared, // cdsDataPrepared ora sarà una versione preparata dell'array flat
        presetData,
        staticTransactionData,
        abapDocPrepared,
        badiDataPrepared,
        smartformDataPrepared,
        adobeformDataPrepared,
        smartformDisplayNames,
        adobeformDisplayNames,
        cdsDocumentation, // cdsDocumentation sarà il documento overview (il primo elemento dell'array)
        findCdsFunc, // findCdsFunc sarà la funzione per cercare nell'array flat
        findTableFunc,
        findBapiFunc,
        findSmartformFunc,
        findAdobeformFunc,
        findBadiFunc,
        findAbapDocFunc,
        fioriPresetData, // Aggiunto
    } = useMemo(() => {
        const tables = parseTableData(tableDataRaw);
        const bapis = bapiData;
        const cds = cdsData; // cds è ora l'array flat
        const abapDocs = abapDocData;
        const badis = badiData;
        const smartforms = smartformData;
        const adobeforms = adobeformData;

        // cdsOverviewDoc sarà semplicemente il primo documento nell'array cds
        const cdsOverviewDoc = cds.length > 0 ? cds[0] : null;

        const tableModuleList = ['All', ...Object.keys(tables.sapData).sort()];
        const tableDisplayNames = { 'All': 'Home', ...tables.moduleNames };

        const bapiModuleList = ['All', ...Object.keys(bapis).sort()];
        const bapiDisplayNames = { 'All': 'Tutti i Moduli', ...Object.fromEntries(Object.keys(bapis).map(abbr => [abbr, tables.moduleNames[abbr] || abbr])) };

        // Queste variabili non sono più usate per CDS in questa struttura flat
        // Le manteniamo per coerenza del template ma non influiscono sul CDS ora
        const cdsModuleList = ['All'];
        const cdsDisplayNames = { 'All': 'Documentazione CDS' };

        const badiModuleList = ['All', ...Object.keys(badis).sort()];
        const badiDisplayNames = { 'All': 'Tutti i Moduli', ...Object.fromEntries(Object.keys(badis).map(abbr => [abbr, tables.moduleNames[abbr] || abbr])) };

        const smartformModuleList = ['All', ...Object.keys(smartforms).sort()];
        const sFDisplayNames = { 'All': 'Tutti i Moduli', ...Object.fromEntries(Object.keys(smartforms).map(abbr => [abbr, tables.moduleNames[abbr] || abbr])) };

        const adobeformModuleList = ['All', ...Object.keys(adobeforms).sort()];
        const aFDisplayNames = { 'All': 'Tutti i Moduli', ...Object.fromEntries(Object.keys(adobeforms).map(abbr => [abbr, tables.moduleNames[abbr] || abbr])) };


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

        // FIX: Riscrivi findCdsFunc per cercare direttamente nell'array cds
        const findCdsFunc = (name) => {
            if (!name) return null;
            return cds.find(doc => doc.name === name); // Cerca direttamente nell'array flat
        };

        const findBadiFunc = (name) => {
            if (!name) return null;
            for (const moduleKey in badis) { const found = badis[moduleKey].find(b => b.name === name); if (found) return found; } return null;
        };

        const findSmartformFunc = (name) => {
            if (!name) return null;
            for (const moduleKey in smartforms) { const found = smartforms[moduleKey].find(sf => sf.name === name); if (found) return found; } return null;
        };

        const findAdobeformFunc = (name) => {
            if (!name) return null;
            for (const moduleKey in adobeforms) { const found = adobeforms[moduleKey].find(af => af.name === name); if (found) return found; } return null;
        };


        const presets = {
            all: codePresets,
            find: (id) => id ? codePresets.find(p => p.id === id) : null
        };

        const transactionModules = transactionData.fiori.modules.map(m => ({ id: m.id, name: m.name }));
        const findAbapDocFunc = (id) => id ? abapDocs.find(d => d.id === id) : null;
        const docs = { all: abapDocs, find: findAbapDocFunc };

        const fioriPresets = {
            all: fioriPresetsData,
            find: (id) => id ? fioriPresetsData.find(p => p.id === id) : null
        };

        return {
            tableData: { all: tables.allTables, modules: tableModuleList, names: tableDisplayNames, subgroups: subgroupsMap, find: findTableFunc },
            bapiDataPrepared: { all: bapis, modules: bapiModuleList, names: bapiDisplayNames, find: findBapiFunc },
            // cdsDataPrepared ora usa direttamente l'array flat cds
            cdsDataPrepared: {
                all: cds, // cds è già l'array di documenti
                modules: cdsModuleList,
                names: cdsDisplayNames,
                find: findCdsFunc
            },
            presetData: presets,
            staticTransactionData: { ...transactionData, navModules: transactionModules },
            abapDocPrepared: docs,
            badiDataPrepared: { all: badis, modules: badiModuleList, names: badiDisplayNames, find: findBadiFunc },
            smartformDataPrepared: { all: smartforms, modules: smartformModuleList, names: sFDisplayNames, find: findSmartformFunc },
            adobeformDataPrepared: { all: adobeforms, modules: adobeformModuleList, names: aFDisplayNames, find: findAdobeformFunc },
            smartformDisplayNames: sFDisplayNames,
            adobeformDisplayNames: aFDisplayNames,
            cdsDocumentation: cdsOverviewDoc,
            findCdsFunc, // FIX: Esporta findCdsFunc
            findTableFunc,
            findBapiFunc,
            findSmartformFunc,
            findAdobeformFunc,
            findBadiFunc,
            findAbapDocFunc,
            fioriPresetData: fioriPresets, // Aggiunto
        };
    }, []);

    const handleViewModeSelect = (mode) => {
        setViewMode(mode);
        setSelectedItemName(null);
        setSearchTerm('');

        if (mode === 'TABLES' || mode === 'BAPIS' || mode === 'CDS' || mode === 'ABAP_DOC' || mode === 'BADIS' || mode === 'SMARTFORMS' || mode === 'ADOBEFORMS' || mode === 'FIORI_PRESETS') {
            let moduleToSelect = 'All';
            if (mode === 'BAPIS') moduleToSelect = 'All'; 
            else if (mode === 'ABAP_DOC') moduleToSelect = 'All';
            else if (mode === 'BADIS') moduleToSelect = 'All';
            else if (mode === 'SMARTFORMS') moduleToSelect = smartformDataPrepared.modules[1] || 'All';
            else if (mode === 'ADOBEFORMS') moduleToSelect = adobeformDataPrepared.modules[1] || 'All';
            else if (mode === 'CDS') {
                moduleToSelect = 'All'; // Nessun modulo specifico per il dropdown
                if (cdsDocumentation) {
                    setSelectedItemName(cdsDocumentation.name); // Seleziona il documento principale
                } else {
                    setSelectedItemName(null); // Nessun documento da selezionare
                }
            }
            handleModuleSelect(moduleToSelect); // Passa il modulo (anche se 'All' o [] per overview)
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

    // FIX: filteredCds deve ora gestire solo l'array flat
    const filteredCds = useMemo(() => {
        if (viewMode !== 'CDS') return [];
        let cdsToFilter = cdsDataPrepared.all; // Ora cdsDataPrepared.all è già l'array flat di documenti
        if (!searchTerm) return cdsToFilter;
        return cdsToFilter.filter(doc => doc.name.toLowerCase().includes(searchTerm.toLowerCase()) || (doc.description && doc.description.toLowerCase().includes(searchTerm.toLowerCase())));
    }, [cdsDataPrepared.all, searchTerm, viewMode]);


    const filteredBadis = useMemo(() => {
        if (viewMode !== 'BADIS') return [];
        let badisToFilter = currentModule === 'All' ? Object.values(badiDataPrepared.all).flat() : (badiDataPrepared.all[currentModule] || []);
        if (!searchTerm) return badisToFilter;
        return badisToFilter.filter(b => b.name.toLowerCase().includes(searchTerm.toLowerCase()) || (b.description && b.description.toLowerCase().includes(searchTerm.toLowerCase())));
    }, [badiDataPrepared.all, currentModule, searchTerm, viewMode]);

    const filteredSmartforms = useMemo(() => {
        if (viewMode !== 'SMARTFORMS') return [];
        let smartformsToFilter = currentModule === 'All' ? Object.values(smartformDataPrepared.all).flat() : (smartformDataPrepared.all[currentModule] || []);
        if (!searchTerm) return smartformsToFilter;
        return smartformsToFilter.filter(sf => sf.name.toLowerCase().includes(searchTerm.toLowerCase()) || (sf.description && sf.description.toLowerCase().includes(searchTerm.toLowerCase())));
    }, [smartformDataPrepared.all, currentModule, searchTerm, viewMode]);

    const filteredAdobeforms = useMemo(() => {
        if (viewMode !== 'ADOBEFORMS') return [];
        let adobeformsToFilter = currentModule === 'All' ? Object.values(adobeformDataPrepared.all).flat() : (adobeformDataPrepared.all[currentModule] || []);
        if (!searchTerm) return adobeformsToFilter;
        return adobeformsToFilter.filter(af => af.name.toLowerCase().includes(searchTerm.toLowerCase()) || (af.description && af.description.toLowerCase().includes(searchTerm.toLowerCase())));
    }, [adobeformDataPrepared.all, currentModule, searchTerm, viewMode]);


    const filteredPresets = useMemo(() => {
        if (viewMode !== 'PRESETS') return [];
        if (!searchTerm) return presetData.all;
        return presetData.all.filter(p => p.title.toLowerCase().includes(searchTerm.toLowerCase()));
    }, [presetData.all, searchTerm, viewMode]);

    const filteredFioriPresets = useMemo(() => {
        if (viewMode !== 'FIORI_PRESETS') return [];
        if (!searchTerm) return fioriPresetData.all;
        return fioriPresetData.all.filter(p => p.title.toLowerCase().includes(searchTerm.toLowerCase()));
    }, [fioriPresetData.all, searchTerm, viewMode]);

    const filteredAbapDocs = useMemo(() => {
        if (viewMode !== 'ABAP_DOC') return [];
        if (!searchTerm) return abapDocPrepared.all;
        return abapDocPrepared.all.filter(d => d.title.toLowerCase().includes(searchTerm.toLowerCase()));
    }, [abapDocPrepared.all, searchTerm, viewMode]);


    const selectedTable = viewMode === 'TABLES' ? findTableFunc(selectedItemName) : null;
    const selectedBapi = viewMode === 'BAPIS' ? findBapiFunc(selectedItemName) : null;
    const selectedCds = viewMode === 'CDS' ? findCdsFunc(selectedItemName) : null;
    const selectedPreset = viewMode === 'PRESETS' ? presetData.find(selectedItemName) : null;
    const selectedFioriPreset = viewMode === 'FIORI_PRESETS' ? fioriPresetData.find(selectedItemName) : null; // Aggiunto
    const selectedAbapDoc = viewMode === 'ABAP_DOC' ? findAbapDocFunc(selectedItemName) : null;
    const selectedBadi = viewMode === 'BADIS' ? findBadiFunc(selectedItemName) : null;
    const selectedSmartform = viewMode === 'SMARTFORMS' ? findSmartformFunc(selectedItemName) : null;
    const selectedAdobeform = viewMode === 'ADOBEFORMS' ? findAdobeformFunc(selectedItemName) : null;


    const currentModules = {
        'TABLES': tableData.modules,
        'BAPIS': bapiDataPrepared.modules,
        'CDS': ['All'],
        'ABAP_DOC': ['All'],
        'BADIS': badiDataPrepared.modules,
        'SMARTFORMS': smartformDataPrepared.modules,
        'ADOBEFORMS': adobeformDataPrepared.modules,
        'FIORI_PRESETS': ['All'], // Aggiunto
    }[viewMode] || [];

    const currentModuleNames = {
        'TABLES': tableData.names,
        'BAPIS': bapiDataPrepared.names,
        'CDS': { 'All': 'Documentazione CDS' },
        'ABAP_DOC': { 'All': 'Tutte le Docs' },
        'BADIS': badiDataPrepared.names,
        'SMARTFORMS': smartformDisplayNames,
        'ADOBEFORMS': adobeformDisplayNames,
        'FIORI_PRESETS': { 'All': 'Tutti i Preset Fiori' }, // Aggiunto
    }[viewMode] || {};

    return (
        <div className="app-layout">
            <Navbar
                viewMode={viewMode}
                onViewModeSelect={handleViewModeSelect}
                modules={currentModules}
                moduleNames={currentModuleNames}
                currentModule={currentModule}
                onModuleSelect={handleModuleSelect}
                subgroups={
                    (viewMode === 'TABLES' || viewMode === 'CDS')
                        ? (tableData.subgroups && tableData.subgroups[currentModule])
                        : null
                }
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
                    cdsViews={filteredCds}
                    presets={filteredPresets}
                    abapDocs={filteredAbapDocs}
                    badis={filteredBadis}
                    smartforms={filteredSmartforms}
                    adobeforms={filteredAdobeforms}
                    fioriPresets={filteredFioriPresets} // Aggiunto
                    transactionModules={staticTransactionData.navModules}
                    onSelectItem={setSelectedItemName}
                />
                <MainContent
                    viewMode={viewMode}
                    selectedTable={selectedTable}
                    selectedBapi={selectedBapi}
                    selectedCds={selectedCds}
                    selectedPreset={selectedPreset}
                    selectedFioriPreset={selectedFioriPreset} // Aggiunto
                    selectedAbapDoc={selectedAbapDoc}
                    selectedBadi={selectedBadi}
                    selectedSmartform={selectedSmartform}
                    selectedAdobeform={selectedAdobeform}
                    transactionData={staticTransactionData}
                    allTables={tableData.all}
                    onSelectTable={setSelectedItemName}
                />
            </div>
        </div>
    );
}

export default App;