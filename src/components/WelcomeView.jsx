import React, { useEffect, useRef, useMemo } from 'react';
import { Chart } from 'chart.js';

const WelcomeView = ({ allTables }) => {
    const chartRef = useRef(null);
    const chartInstance = useRef(null);
    const chartData = useMemo(() => {
        const moduleCounts = allTables.reduce((acc, table) => { if (table.module && table.module !== 'N/A') { acc[table.module] = (acc[table.module] || 0) + 1; } return acc; }, {});
        const labels = Object.keys(moduleCounts).sort();
        const data = labels.map(label => moduleCounts[label]);
        return { labels, data };
    }, [allTables]);

    useEffect(() => {
        if (!chartRef.current) return;
        if (chartInstance.current) chartInstance.current.destroy();
        const ctx = chartRef.current.getContext('2d');
        chartInstance.current = new Chart(ctx, {
            type: 'bar',
            data: { labels: chartData.labels, datasets: [{ label: 'Numero di Tabelle', data: chartData.data, backgroundColor: 'rgba(59, 130, 246, 0.7)', borderColor: 'rgb(59, 130, 246)', borderWidth: 1, borderRadius: 5 }] },
            options: { responsive: true, maintainAspectRatio: false, plugins: { legend: { display: false } }, scales: { y: { beginAtZero: true } } }
        });
        return () => chartInstance.current?.destroy();
    }, [chartData]);

    return (
        <div className="welcome-view">
             <div style={{maxWidth: '56rem', width: '100%'}}>
                <h2 style={{fontSize: '1.875rem', fontWeight: '700', marginBottom: '0.5rem'}}>Benvenuto nell'Esploratore di Schemi SAP</h2>
                <p style={{fontSize: '1.125rem', color: '#64748b', marginBottom: '1.5rem'}}>Seleziona un modulo o cerca una tabella per iniziare a esplorare le relazioni tra i dati.</p>
                <div style={{backgroundColor: 'white', padding: '1.5rem', borderRadius: '0.5rem', boxShadow: '0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)'}}>
                    <div className="chart-container"><canvas ref={chartRef}></canvas></div>
                    <p style={{fontSize: '0.875rem', color: '#64748b', marginTop: '1rem'}}>Grafico della distribuzione delle tabelle per modulo.</p>
                </div>
            </div>
        </div>
    );
};

export default WelcomeView;