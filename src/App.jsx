// src/App.jsx
import React, { useState, useMemo } from 'react';

import Navbar from './components/Navbar.jsx';
import Sidebar from './components/Sidebar.jsx';
import MainContent from './components/MainContent.jsx';

import { tableDataRaw } from './modules/index.js';
import { bapiData } from './modules/BapiData.jsx';
import { codePresets } from './modules/CodePresets.jsx';
import { transactionData } from './modules/TransactionData.jsx';
import { cdsData } from './modules/CdsData.jsx';
import { abapDocData } from './modules/doc_abap/AbapDocData.jsx';
import { badiData } from './modules/BadiData.jsx';
import { smartformData } from './modules/SmartformData.jsx';
import { adobeformData } from './modules/AdobeformData.jsx';
import { fioriPresetsData } from './modules/FioriPresetsData.jsx';
import { cdsPresetsData } from './modules/CdsPresetsData.jsx';

import './App.css';

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

    allTables.forEach(table => {
        if (table.joins && table.joins.length > 0) {
            const resolvedJoins = [];
            table.joins.forEach(join => {
                const target = allTables.find(t => t.name === join.table);
                if (target) {
                    resolvedJoins.push({ table: target, on: join.on });
                } else {
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
    const [cdsSubMode, setCdsSubMode] = useState('docs');

    const {
        tableData,
        bapiDataPrepared,
        cdsDataPrepared,
        presetData,
        staticTransactionData,
        abapDocPrepared,
        badiDataPrepared,
        smartformDataPrepared,
        adobeformDataPrepared,
        smartformDisplayNames,
        adobeformDisplayNames,
        cdsDocumentation,
        findCdsFunc,
        findTableFunc,
        findBapiFunc,
        findSmartformFunc,
        findAdobeformFunc,
        findBadiFunc,
        findAbapDocFunc,
        fioriPresetData,
        cdsPresetData,
    } = useMemo(() => {
        const tables = parseTableData(tableDataRaw);
        const bapis = bapiData;
        const cds = cdsData;
        const abapDocs = abapDocData;
        const badis = badiData;
        const smartforms = smartformData;
        const adobeforms = adobeformData;

        const cdsOverviewDoc = cds.length > 0 ? cds[0] : null;

        const tableModuleList = ['All', ...Object.keys(tables.sapData).sort()];
        const tableDisplayNames = { 'All': 'Home', ...tables.moduleNames };

        const bapiModuleList = ['All', ...Object.keys(bapis).sort()];
        const bapiDisplayNames = { 'All': 'Tutti i Moduli', ...Object.fromEntries(Object.keys(bapis).map(abbr => [abbr, tables.moduleNames[abbr] || abbr])) };

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
        const findCdsFunc = (name) => {
            if (!name) return null;
            return cds.find(doc => doc.name === name);
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

        const presets = { all: codePresets, find: (id) => id ? codePresets.find(p => p.id === id) : null };
        const transactionModules = [
            ...transactionData.fiori.modules.map(m => ({ id: m.id, name: m.name })),
            ...transactionData.gui.modules.map(m => ({ id: m.id, name: m.name })),
            ...transactionData.abap.modules.map(m => ({ id: m.id, name: m.name }))
        ];
        const findAbapDocFunc = (id) => id ? abapDocs.find(d => d.id === id) : null;
        const docs = { all: abapDocs, find: findAbapDocFunc };

        const fioriPresets = { all: fioriPresetsData, find: (id) => id ? fioriPresetsData.find(p => p.id === id) : null };
        const cdsPresets = { all: cdsPresetsData, find: (id) => id ? cdsPresetsData.find(p => p.id === id) : null };

        return {
            tableData: { all: tables.allTables, modules: tableModuleList, names: tableDisplayNames, subgroups: subgroupsMap, find: findTableFunc },
            bapiDataPrepared: { all: bapis, modules: bapiModuleList, names: bapiDisplayNames, find: findBapiFunc },
            cdsDataPrepared: { all: cds, modules: cdsModuleList, names: cdsDisplayNames, find: findCdsFunc },
            presetData: presets,
            staticTransactionData: { ...transactionData, navModules: transactionModules },
            abapDocPrepared: docs,
            badiDataPrepared: { all: badis, modules: badiModuleList, names: badiDisplayNames, find: findBadiFunc },
            smartformDataPrepared: { all: smartforms, modules: smartformModuleList, names: sFDisplayNames, find: findSmartformFunc },
            adobeformDataPrepared: { all: adobeforms, modules: adobeformModuleList, names: aFDisplayNames, find: findAdobeformFunc },
            smartformDisplayNames: sFDisplayNames,
            adobeformDisplayNames: aFDisplayNames,
            cdsDocumentation: cdsOverviewDoc,
            findCdsFunc,
            findTableFunc,
            findBapiFunc,
            findSmartformFunc,
            findAdobeformFunc,
            findBadiFunc,
            findAbapDocFunc,
            fioriPresetData: fioriPresets,
            cdsPresetData: cdsPresets,
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
                moduleToSelect = 'All';
                if (cdsDocumentation) {
                    setSelectedItemName(cdsDocumentation.name);
                } else {
                    setSelectedItemName(null);
                }
            }
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

    const filteredCds = useMemo(() => {
        if (viewMode !== 'CDS') return [];
        let cdsToFilter = cdsDataPrepared.all;
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

    const filteredTransactions = useMemo(() => {
        if (viewMode !== 'TRANSACTIONS') return [];
        const term = searchTerm.toLowerCase();
        let allTransactions = [];
        staticTransactionData.fiori.modules.forEach(module => {
            module.submodules.forEach(submodule => {
                submodule.transactions.forEach(tx => {
                    allTransactions.push({ ...tx, moduleId: module.id, submoduleId: submodule.id, type: 'fiori' });
                });
            });
        });
        staticTransactionData.gui.modules.forEach(module => {
            module.transactions.forEach(tx => {
                allTransactions.push({ ...tx, moduleId: module.id, type: 'gui' });
                });
            });
        staticTransactionData.abap.modules.forEach(module => {
            module.transactions.forEach(tx => {
                allTransactions.push({ ...tx, moduleId: module.id, type: 'abap' });
                });
            });
        const filtered = allTransactions.filter(tx => {
            if (!searchTerm) return true;
            if (tx.type === 'fiori') {
                return (
                    (tx.process && tx.process.toLowerCase().includes(term)) ||
                    (tx.appName && tx.appName.toLowerCase().includes(term)) ||
                    (tx.appId && tx.appId.toLowerCase().includes(term)) ||
                    (tx.tCode && tx.tCode.toLowerCase().includes(term)) ||
                    (tx.purpose && tx.purpose.toLowerCase().includes(term))
                );
            }
            return (
                (tx.tCode && tx.tCode.toLowerCase().includes(term)) ||
                (tx.description && tx.description.toLowerCase().includes(term)) ||
                (tx.notes && tx.notes.toLowerCase().includes(term))
            );
        });
        return filtered.map(tx => {
            let uniqueId;
            let displayName;
            if (tx.type === 'fiori') {
                uniqueId = `${tx.moduleId}-${tx.submoduleId}-${tx.appId || tx.tCode}`;
                displayName = tx.appName ? `${tx.appName} (${tx.appId || tx.tCode})` : `${tx.process} (${tx.tCode})`;
            } else {
                uniqueId = `${tx.moduleId}-${tx.tCode}`;
                displayName = `${tx.tCode} - ${tx.description}`;
            }
            return {
                id: uniqueId,
                name: displayName,
                originalModuleId: tx.moduleId,
                transactionDetails: tx
            };
        });
    }, [staticTransactionData, searchTerm, viewMode]);

    const selectedTable = viewMode === 'TABLES' ? findTableFunc(selectedItemName) : null;
    const selectedBapi = viewMode === 'BAPIS' ? findBapiFunc(selectedItemName) : null;
    const selectedCds = viewMode === 'CDS' && cdsSubMode === 'docs' ? findCdsFunc(selectedItemName) : null;
    const selectedCdsPreset = viewMode === 'CDS' && cdsSubMode === 'presets' ? cdsPresetData.find(selectedItemName) : null;
    const selectedPreset = viewMode === 'PRESETS' ? presetData.find(selectedItemName) : null;
    const selectedFioriPreset = viewMode === 'FIORI_PRESETS' ? fioriPresetData.find(selectedItemName) : null;
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
        'FIORI_PRESETS': ['All'],
    }[viewMode] || [];

    const currentModuleNames = {
        'TABLES': tableData.names,
        'BAPIS': bapiDataPrepared.names,
        'CDS': { 'All': 'Documentazione CDS' },
        'ABAP_DOC': { 'All': 'Tutte le Docs' },
        'BADIS': badiDataPrepared.names,
        'SMARTFORMS': smartformDisplayNames,
        'ADOBEFORMS': adobeformDisplayNames,
        'FIORI_PRESETS': { 'All': 'Tutti i Preset Fiori' },
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
                    fioriPresets={filteredFioriPresets}
                    cdsPresets={cdsPresetData.all}
                    transactionModules={filteredTransactions}
                    cdsSubMode={cdsSubMode}
                    onCdsSubModeChange={setCdsSubMode}
                    onSelectItem={setSelectedItemName}
                />
                <MainContent
                    viewMode={viewMode}
                    selectedTable={selectedTable}
                    selectedBapi={selectedBapi}
                    selectedCds={selectedCds}
                    selectedCdsPreset={selectedCdsPreset}
                    selectedPreset={selectedPreset}
                    selectedFioriPreset={selectedFioriPreset}
                    selectedAbapDoc={selectedAbapDoc}
                    selectedBadi={selectedBadi}
                    selectedSmartform={selectedSmartform}
                    selectedAdobeform={selectedAdobeform}
                    transactionData={staticTransactionData}
                    allTables={tableData.all}
                    cdsSubMode={cdsSubMode}
                    onSelectTable={setSelectedItemName}
                />
            </div>
        </div>
    );
}

export default App;
