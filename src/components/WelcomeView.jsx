import React, { useEffect, useRef, useMemo } from 'react';
import { Chart, BarController, BarElement, CategoryScale, LinearScale, Title, Tooltip, Legend } from 'chart.js';

Chart.register(BarController, BarElement, CategoryScale, LinearScale, Title, Tooltip, Legend);

const WelcomeView = ({ allTables = [], isBapiMode, isPresetMode }) => {
    const chartRef = useRef(null);
    const chartInstance = useRef(null);
    
    const chartData = useMemo(() => {
        if (!allTables) return { labels: [], data: [] };
        const moduleCounts = allTables.reduce((acc, table) => { 
            if (table.module && table.module !== 'N/A') { 
                acc[table.module] = (acc[table.module] || 0) + 1; 
            } 
            return acc; 
        }, {});
        const labels = Object.keys(moduleCounts).sort();
        const data = labels.map(label => moduleCounts[label]);
        return { labels, data };
    }, [allTables]);

    useEffect(() => {
        if (!chartRef.current || !allTables || isBapiMode || isPresetMode) return;
        
        if (chartInstance.current) {
            chartInstance.current.destroy();
        }

        const ctx = chartRef.current.getContext('2d');
        chartInstance.current = new Chart(ctx, { 
            type: 'bar', 
            data: { 
                labels: chartData.labels, 
                datasets: [{ 
                    label: 'Numero di Tabelle', 
                    data: chartData.data, 
                    backgroundColor: 'rgba(59, 130, 246, 0.7)', 
                    borderColor: 'rgb(59, 130, 246)', 
                    borderWidth: 1, 
                    borderRadius: 5 
                }] 
            }, 
            options: { 
                responsive: true, 
                maintainAspectRatio: false, 
                plugins: { legend: { display: false } }, 
                scales: { y: { beginAtZero: true } } 
            } 
        });

        return () => {
            if (chartInstance.current) {
                chartInstance.current.destroy();
            }
        };
    }, [allTables, chartData, isBapiMode, isPresetMode]);

    const getWelcomeMessage = () => {
        if (isPresetMode) return "Seleziona un preset di codice dalla lista per visualizzarlo e copiarlo.";
        if (isBapiMode) return "Seleziona un modulo per visualizzare le BAPI disponibili.";
        return "Seleziona un modulo e una sottocategoria per visualizzare le tabelle.";
    };

    return (
        <div className="welcome-view">
             <div style={{maxWidth: '56rem', margin: 'auto', width: '100%'}}>
                <h2 style={{fontSize: '1.875rem', fontWeight: '700', marginBottom: '0.5rem'}}>Benvenuto nell'Explorer</h2>
                <p style={{fontSize: '1.125rem', color: '#64748b', marginBottom: '1.5rem'}}>
                    {getWelcomeMessage()}
                </p>
                {!isBapiMode && !isPresetMode && allTables.length > 0 && (
                    <div style={{backgroundColor: 'white', padding: '1.5rem', borderRadius: '0.5rem', boxShadow: '0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)'}}>
                        <div className="chart-container"><canvas ref={chartRef}></canvas></div>
                        <p style={{fontSize: '0.875rem', color: '#64748b', marginTop: '1rem'}}>Tabelle per Modulo</p>
                    </div>
                )}
            </div>
        </div>
    );
};

export default WelcomeView;