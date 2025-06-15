import React from 'react';

const ClSalvTableDoc = () => {
    return (
        <div className="p-6 bg-white shadow-lg rounded-lg max-w-4xl mx-auto my-8 font-sans">
            <h1 className="text-4xl font-extrabold text-gray-900 mb-6 text-center">
                Guida Tecnica Completa alla Classe CL_SALV_TABLE in SAP S/4HANA
            </h1>

            {/* Sezione 1 */}
            <section className="mb-10">
                <h2 className="text-3xl font-bold text-gray-800 mb-4 border-b-2 border-indigo-500 pb-2">
                    Sezione 1: Il Modello a Oggetti SALV: Fondamenti e Istanziazione
                </h2>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        1.1. Introduzione al Framework SALV
                    </h3>
                    <p className="text-gray-700 leading-relaxed mb-4">
                        Nel contesto dell'evoluzione di SAP verso S/4HANA, la classe `CL_SALV_TABLE` rappresenta l'approccio moderno e orientato agli oggetti (Object-Oriented) per la visualizzazione di dati tabellari, posizionandosi come il successore dei moduli funzione tradizionali come `REUSE_ALV_GRID_DISPLAY`. L'adozione del modello a oggetti SALV (SAP List Viewer) offre vantaggi significativi, tra cui un'incapsulazione migliorata, una maggiore riutilizzabilità e manutenibilità del codice e, soprattutto, l'eliminazione della necessità di creare e gestire manualmente i field catalog. Il framework deduce automaticamente le proprietà delle colonne dai tipi di dati della tabella interna, semplificando notevolmente lo sviluppo.
                    </p>
                    <p className="text-gray-700 leading-relaxed">
                        È importante notare che `CL_SALV_TABLE` è la classe principale per la gestione di tabelle semplici e bidimensionali. Essa fa parte di una famiglia più ampia di classi SALV, che include `CL_SALV_HIERSEQ_TABLE` per le liste gerarchico-sequenziali e `CL_SALV_TREE` per le strutture ad albero, offrendo una soluzione per quasi ogni esigenza di reporting.
                    </p>
                </article>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        1.2. Il Pattern di Istanziazione Principale: FACTORY e DISPLAY
                    </h3>
                    <p className="text-gray-700 leading-relaxed mb-4">
                        Il processo per visualizzare un ALV di base con `CL_SALV_TABLE` si articola in due passaggi fondamentali, che costituiscono il cuore del framework.
                    </p>
                    <p className="text-gray-700 leading-relaxed mb-2">
                        **Istanziazione tramite `CL_SALV_TABLE={'>'}FACTORY`**: Questo metodo statico è il punto di ingresso universale per la creazione di un'istanza SALV. La sua firma è peculiare e può inizialmente sorprendere gli sviluppatori abituati a pattern più convenzionali. La tabella dati interna viene passata tramite un parametro `CHANGING` (`t_table`), mentre il riferimento all'oggetto ALV creato viene restituito tramite un parametro `IMPORTING` (`r_salv_table`). È una pratica di sviluppo robusta e obbligatoria racchiudere questa chiamata in un blocco `TRY...CATCH` per gestire l'eccezione `cx_salv_msg`, che viene sollevata in caso di errori durante l'istanziazione.
                    </p>
                    <p className="text-gray-700 leading-relaxed mb-4">
                        **Rendering tramite `DISPLAY`**: Una volta che l'oggetto ALV è stato creato e tutte le configurazioni desiderate sono state applicate, la chiamata al metodo d'istanza `DISPLAY` si occupa di renderizzare la griglia a schermo.
                    </p>
                    <p className="text-gray-700 leading-relaxed">
                        Un aspetto fondamentale della filosofia di progettazione di `CL_SALV_TABLE` è il suo approccio "minimalista per default". Una semplice chiamata a `FACTORY` seguita da `DISPLAY` produce una griglia dati pulita, ma priva di molte delle funzionalità standard che gli utenti si aspettano, come i pulsanti per l'ordinamento, il filtro o il salvataggio dei layout. Lo sviluppatore deve esplicitamente "attivare" queste funzionalità tramite chiamate successive, ad esempio `get_functions()={'>'}set_all(abap_true)`. Questo approccio, sebbene possa creare confusione iniziale, promuove un codice più pulito, in cui solo le funzionalità necessarie vengono caricate, e rappresenta un cambiamento deliberato rispetto ai vecchi strumenti ALV. Inoltre, il pattern basato sul metodo `FACTORY` non è un'esclusiva di `CL_SALV_TABLE`, ma un principio architetturale ricorrente in tutto il modello SALV, utilizzato anche da `CL_SALV_HIERSEQ_TABLE` e `CL_SALV_TREE`. Comprendere questo pattern facilita il passaggio ad altri strumenti del framework.
                    </p>
                </article>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        1.3. Contesti di Visualizzazione: Schermo Intero, Container e Lista
                    </h3>
                    <p className="text-gray-700 leading-relaxed mb-4">
                        Il metodo `FACTORY` permette di specificare dove e come l'ALV verrà visualizzato, offrendo tre contesti principali.
                    </p>
                    <ul className="list-disc list-inside text-gray-700 space-y-2">
                        <li>
                            <strong className="text-gray-800">Visualizzazione a Schermo Intero (Default)</strong>: È l'implementazione più semplice. Omettendo il parametro `list_display` o impostandolo a `abap_false`, l'ALV occuperà un'intera schermata generata dinamicamente.
                        </li>
                        <li>
                            <strong className="text-gray-800">Visualizzazione basata su Container</strong>: Per integrare uno o più ALV all'interno di una singola schermata (dynpro), è necessario passare un riferimento a un oggetto container (ad esempio, un'istanza di `CL_GUI_CUSTOM_CONTAINER` o `CL_GUI_SPLITTER_CONTAINER`) al parametro `r_container` del metodo `FACTORY`. Questo approccio offre la massima flessibilità per la progettazione di interfacce utente complesse.
                        </li>
                        <li>
                            <strong className="text-gray-800">Visualizzazione come Lista Classica</strong>: Impostando il parametro `list_display` a `if_salv_c_bool_sap={'>'}true`, l'output assume l'aspetto di un report ABAP classico basato su istruzioni `WRITE`, pur mantenendo i vantaggi della gestione a oggetti.
                        </li>
                    </ul>
                </article>
            </section>

            {/* Sezione 2 */}
            <section className="mb-10">
                <h2 className="text-3xl font-bold text-gray-800 mb-4 border-b-2 border-indigo-500 pb-2">
                    Sezione 2: Padroneggiare la Configurazione delle Colonne con CL_SALV_COLUMNS_TABLE
                </h2>
                <p className="text-gray-700 leading-relaxed mb-6">
                    La personalizzazione delle colonne è un'esigenza fondamentale in qualsiasi report. Il framework SALV fornisce un'interfaccia potente e granulare per questo scopo attraverso le classi `CL_SALV_COLUMNS_TABLE` e `CL_SALV_COLUMN`.
                </p>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        2.1. Accesso agli Oggetti Colonna
                    </h3>
                    <p className="text-gray-700 leading-relaxed mb-4">
                        La manipolazione delle colonne segue un processo a due passaggi:
                    </p>
                    <ol className="list-decimal list-inside text-gray-700 space-y-2">
                        <li>
                            <strong className="text-gray-800">Ottenere l'oggetto principale che gestisce tutte le colonne</strong>: `lo_columns = lo_salv={'>'}get_columns()`. Questo metodo restituisce un'istanza di `CL_SALV_COLUMNS_TABLE`.
                        </li>
                        <li>
                            <strong className="text-gray-800">Ottenere l'oggetto per una singola colonna specifica</strong>: `lo_column = lo_columns={'>'}get_column( 'NOME_COLONNA' )`. Questo metodo restituisce un'istanza di `CL_SALV_COLUMN`.
                        </li>
                    </ol>
                    <p className="text-gray-700 leading-relaxed mt-4">
                        È essenziale racchiudere la chiamata a `get_column()` in un blocco `TRY...CATCH cx_salv_not_found` per gestire elegantemente gli errori dovuti a nomi di colonna errati o non esistenti.
                    </p>
                </article>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        2.2. Controllare l'Aspetto delle Colonne: Testo, Visibilità e Posizione
                    </h3>
                    <ul className="list-disc list-inside text-gray-700 space-y-2">
                        <li>
                            <strong className="text-gray-800">Testi di Intestazione</strong>: È possibile sovrascrivere le etichette predefinite del Data Dictionary utilizzando i metodi `set_long_text`, `set_medium_text` e `set_short_text`. L'ALV sceglierà automaticamente il testo più appropriato in base alla larghezza della colonna. In alcuni casi, possono sorgere problemi con la lunghezza del testo e le traduzioni linguistiche che richiedono attenzione.
                        </li>
                        <li>
                            <strong className="text-gray-800">Visibilità</strong>: Il metodo `set_visible( abap_false )` nasconde una colonna dalla visualizzazione predefinita, ma l'utente può comunque sceglierla dalla configurazione del layout. Per nascondere completamente una colonna, anche dalla configurazione del layout, si utilizza `set_technical( abap_true )`.
                        </li>
                        <li>
                            <strong className="text-gray-800">Posizionamento</strong>: Il metodo `set_column_position` permette di riordinare programmaticamente le colonne, specificando la posizione numerica desiderata.
                        </li>
                    </ul>
                </article>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        2.3. Dimensionamento e Ottimizzazione: Un'Analisi Comparativa
                    </h3>
                    <p className="text-gray-700 leading-relaxed mb-4">
                        Il controllo della larghezza delle colonne può essere gestito in due modi principali, la cui interazione è cruciale da comprendere.
                    </p>
                    <ul className="list-disc list-inside text-gray-700 space-y-2">
                        <li>
                            <strong className="text-gray-800">Ottimizzazione Globale</strong>: La chiamata `lo_columns={'>'}set_optimize( abap_true )` adatta automaticamente la larghezza di tutte le colonne al loro contenuto.
                        </li>
                        <li>
                            <strong className="text-gray-800">Larghezza Specifica</strong>: Il metodo `lo_column={'>'}set_output_length( '20' )` imposta una larghezza fissa in caratteri per una singola colonna.
                        </li>
                    </ul>
                    <p className="text-gray-700 leading-relaxed mt-4">
                        È fondamentale notare che queste due impostazioni possono entrare in conflitto. Per garantire che `set_output_length` abbia effetto, è spesso necessario disabilitare l'ottimizzazione globale (`set_optimize( abap_false )`).
                    </p>
                </article>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        2.4. Proprietà delle Colonne Data-Centric e Interattive
                    </h3>
                    <ul className="list-disc list-inside text-gray-700 space-y-2">
                        <li>
                            <strong className="text-gray-800">Colorazione</strong>: È possibile colorare un'intera colonna staticamente con `lo_column={'>'}set_color()`. Per una colorazione più dinamica e flessibile (a livello di cella o riga), l'approccio corretto consiste nell'aggiungere una colonna speciale alla tabella dati di output (di tipo `LVC_T_SCOL`) e popolarla con le informazioni di colore per ogni riga. Successivamente, si imposta questa colonna come "color column" tramite `lo_columns={'>'}set_color_column( 'NOME_COLONNA_COLORE' )`.
                        </li>
                        <li>
                            <strong className="text-gray-800">Interattività</strong>:
                            <ul className="list-circle list-inside ml-4 text-gray-600">
                                <li>
                                    <strong className="text-gray-700">Hotspot (Hyperlink)</strong>: Impostando il tipo di cella con `set_cell_type( if_salv_c_cell_type={'>'}hotspot )`, la colonna diventa cliccabile, scatenando l'evento `LINK_CLICK`.
                                </li>
                                <li>
                                    <strong className="text-gray-700">Checkbox</strong>: Allo stesso modo, `set_cell_type( if_salv_c_cell_type={'>'}checkbox_hotspot )` crea delle checkbox modificabili, il cui stato può essere gestito tramite l'evento `LINK_CLICK`.
                                </li>
                                <li>
                                    <strong className="text-gray-700">Search Help (F4)</strong>: È possibile abilitare programmaticamente un aiuto alla ricerca (F4) per una colonna. Questo si ottiene fornendo un riferimento a un campo del DDIC con `set_ddic_reference()` e attivando la funzionalità con `set_f4( abap_true )`.
                                </li>
                            </ul>
                        </li>
                    </ul>
                </article>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        2.5. Tabella di Riferimento: Metodi di CL_SALV_COLUMN
                    </h3>
                    <p className="text-gray-700 leading-relaxed mb-4">
                        La seguente tabella riassume i metodi più comuni per la manipolazione delle colonne, fungendo da guida di riferimento rapido per gli sviluppatori.
                    </p>
                    <div className="overflow-x-auto rounded-lg shadow-md">
                        <table className="min-w-full divide-y divide-gray-200">
                            <thead className="bg-indigo-600">
                                <tr>
                                    <th scope="col" className="px-6 py-3 text-left text-xs font-medium text-white uppercase tracking-wider rounded-tl-lg">
                                        Nome Metodo
                                    </th>
                                    <th scope="col" className="px-6 py-3 text-left text-xs font-medium text-white uppercase tracking-wider">
                                        Classe
                                    </th>
                                    <th scope="col" className="px-6 py-3 text-left text-xs font-medium text-white uppercase tracking-wider">
                                        Parametri
                                    </th>
                                    <th scope="col" className="px-6 py-3 text-left text-xs font-medium text-white uppercase tracking-wider rounded-tr-lg">
                                        Descrizione e Caso d'Uso
                                    </th>
                                </tr>
                            </thead>
                            <tbody className="bg-white divide-y divide-gray-200">
                                <tr>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">`set_long_text`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">`CL_SALV_COLUMN`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">`value`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">Imposta la variante più lunga del testo di intestazione.</td>
                                </tr>
                                <tr>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">`set_visible`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">`CL_SALV_COLUMN`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">`value`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">Nasconde o mostra una colonna nella visualizzazione predefinita.</td>
                                </tr>
                                <tr>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">`set_technical`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">`CL_SALV_COLUMN`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">`value`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">Nasconde una colonna sia dalla visualizzazione che dalle impostazioni del layout.</td>
                                </tr>
                                <tr>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">`set_optimize`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">`CL_SALV_COLUMNS_TABLE`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">`value`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">Abilita/disabilita l'ottimizzazione automatica della larghezza per tutte le colonne.</td>
                                </tr>
                                <tr>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">`set_output_length`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">`CL_SALV_COLUMN`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">`value`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">Imposta una larghezza fissa in caratteri per una colonna specifica.</td>
                                </tr>
                                <tr>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">`set_color`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">`CL_SALV_COLUMN_TABLE`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">`value`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">Imposta un colore statico per un'intera colonna.</td>
                                </tr>
                                <tr>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">`set_cell_type`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">`CL_SALV_COLUMN_TABLE`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">`value`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">Cambia la visualizzazione della cella in un hotspot, pulsante o checkbox.</td>
                                </tr>
                                <tr>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">`set_ddic_reference`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">`CL_SALV_COLUMN`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">`value`</td>
                                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-700">Collega la colonna a un campo del DDIC per ereditarne le proprietà, come l'aiuto F4.</td>
                                </tr>
                            </tbody>
                        </table>
                    </div>
                </article>
            </section>

            {/* Sezione 3 */}
            <section className="mb-10">
                <h2 className="text-3xl font-bold text-gray-800 mb-4 border-b-2 border-indigo-500 pb-2">
                    Sezione 3: Implementazione delle Operazioni Standard sui Dati ALV
                </h2>
                <p className="text-gray-700 leading-relaxed mb-6">
                    `CL_SALV_TABLE` offre un accesso semplificato alle operazioni sui dati più comuni come l'ordinamento, il filtro e le aggregazioni, ma astrae deliberatamente le configurazioni più complesse. Questo rappresenta un compromesso di progettazione: la semplicità e la facilità d'uso in cambio di un controllo meno granulare sui dettagli. Comprendere questo compromesso è essenziale per scegliere lo strumento giusto per il lavoro. Per circa il 90% delle esigenze di reporting, `CL_SALV_TABLE` è sufficiente e preferibile; per scenari altamente personalizzati, `CL_GUI_ALV_GRID` rimane la scelta obbligata.
                </p>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        3.1. Ordinamento Programmatico (CL_SALV_SORTS)
                    </h3>
                    <p className="text-gray-700 leading-relaxed mb-4">
                        Per definire un ordinamento predefinito all'avvio del report, si utilizza la classe `CL_SALV_SORTS`.
                    </p>
                    <ol className="list-decimal list-inside text-gray-700 space-y-2">
                        <li>
                            <strong className="text-gray-800">Ottenere l'oggetto di ordinamento</strong>: `lo_sorts = lo_salv={'>'}get_sorts()`.
                        </li>
                        <li>
                            <strong className="text-gray-800">Aggiungere un criterio di ordinamento</strong>: `lo_sorts={'>'}add_sort( columnname = 'VBELN', sequence = if_salv_c_sort={'>'}sort_up )`. È possibile specificare l'ordinamento ascendente (`sort_up`) o discendente (`sort_down`). L'ordinamento programmatico è anche un prerequisito per abilitare i subtotali nelle aggregazioni.
                        </li>
                    </ol>
                </article>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        3.2. Filtro Programmatico (CL_SALV_FILTERS)
                    </h3>
                    <p className="text-gray-700 leading-relaxed mb-4">
                        Similmente all'ordinamento, è possibile pre-impostare dei filtri sui dati.
                    </p>
                    <ol className="list-decimal list-inside text-gray-700 space-y-2">
                        <li>
                            <strong className="text-gray-800">Ottenere l'oggetto dei filtri</strong>: `lo_filters = lo_salv={'>'}get_filters()`.
                        </li>
                        <li>
                            <strong className="text-gray-800">Aggiungere una condizione di filtro</strong> tramite il metodo `add_filter()`. Questo metodo accetta parametri che rispecchiano la logica di un `SELECT-OPTIONS`: `columnname`, `sign` (`I` per 'inclusive', `E` per 'exclusive'), `option` (`EQ`, `BT`, `GT`, etc.), `low` e `high`. Questo permette di definire facilmente filtri per valori singoli o intervalli.
                        </li>
                    </ol>
                </article>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        3.3. Aggregazioni e Calcoli (CL_SALV_AGGREGATIONS)
                    </h3>
                    <p className="text-gray-700 leading-relaxed mb-4">
                        Per calcolare totali, medie o altri valori aggregati su colonne numeriche, si utilizza la classe `CL_SALV_AGGREGATIONS`.
                    </p>
                    <ol className="list-decimal list-inside text-gray-700 space-y-2">
                        <li>
                            <strong className="text-gray-800">Ottenere l'oggetto delle aggregazioni</strong>: `lo_aggs = lo_salv={'>'}get_aggregations()`.
                        </li>
                        <li>
                            <strong className="text-gray-800">Aggiungere un'aggregazione</strong>: `lo_aggs={'>'}add_aggregation( columnname = 'NETWR', aggregation = if_salv_c_aggregation={'>'}total )`. Le costanti della classe `if_salv_c_aggregation` permettono di specificare il tipo di calcolo (totale, media, minimo, massimo).
                        </li>
                    </ol>
                    <p className="text-gray-700 leading-relaxed mt-4">
                        È di fondamentale importanza notare una limitazione critica di `CL_SALV_TABLE`: non è possibile manipolare direttamente il testo o il comportamento dei subtotali. Se questa funzionalità avanzata è un requisito, è necessario utilizzare la classe di livello inferiore `CL_GUI_ALV_GRID`.
                    </p>
                </article>
            </section>

            {/* Sezione 4 */}
            <section className="mb-10">
                <h2 className="text-3xl font-bold text-gray-800 mb-4 border-b-2 border-indigo-500 pb-2">
                    Sezione 4: Personalizzazione Avanzata dell'UI e User Experience
                </h2>
                <p className="text-gray-700 leading-relaxed mb-6">
                    Oltre alla manipolazione dei dati, `CL_SALV_TABLE` offre ampie possibilità per personalizzare l'interfaccia utente e migliorare l'esperienza dell'utente finale.
                </p>

                <article className="mb-6">
                    <h3 className="2xl font-semibold text-gray-700 mb-3">
                        4.1. Gestione della Barra degli Strumenti (CL_SALV_FUNCTIONS_LIST)
                    </h3>
                    <p className="text-gray-700 leading-relaxed mb-4">
                        La barra degli strumenti (toolbar) dell'ALV è gestita tramite la classe `CL_SALV_FUNCTIONS_LIST`.
                    </p>
                    <ol className="list-decimal list-inside text-gray-700 space-y-2">
                        <li>
                            <strong className="text-gray-800">Abilitare le Funzioni Standard</strong>: Dopo aver ottenuto l'oggetto funzioni (`lo_functions = lo_salv={'>'}get_functions()`), si può usa `set_all( abap_true )` per visualizzare tutti i pulsanti standard (filtro, ordinamento, export, etc.) o `set_default( abap_true )` per un set predefinito.
                        </li>
                        <li>
                            <strong className="text-gray-800">Aggiungere Funzioni Personalizzate</strong>: Il metodo `add_function()` permette di aggiungere pulsanti personalizzati, specificandone nome, icona, testo, tooltip e posizione.
                        </li>
                    </ol>
                    <p className="text-gray-700 leading-relaxed mt-4">
                        Un punto critico da comprendere è che il comportamento di questi metodi dipende dal contesto di visualizzazione. L'aggiunta dinamica di funzioni con `add_function` è supportata solo quando l'ALV è visualizzato in un container. Se si tenta di usarlo in modalità schermo intero, si verificherà un dump. In modalità schermo intero, l'unico modo supportato per aggiungere funzionalità personalizzate è definire uno `GUI Status` (`PF-STATUS`) e associarlo all'ALV tramite il metodo `set_screen_status`. Questa distinzione non è arbitraria ma architetturale: la variante a schermo intero è gestita da un adapter (`CL_SALV_FULLSCREEN_ADAPTER`) che si basa su uno status statico, mentre la variante in container offre un controllo più dinamico sulla propria toolbar.
                    </p>
                </article>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        4.2. Persistenza dei Layout (CL_SALV_LAYOUT)
                    </h3>
                    <p className="text-gray-700 leading-relaxed mb-4">
                        Abilitare il salvataggio dei layout da parte dell'utente è un processo a più passaggi che spesso genera confusione. La sequenza corretta è la seguente:
                    </p>
                    <ol className="list-decimal list-inside text-gray-700 space-y-2">
                        <li>
                            <strong className="text-gray-800">Ottenere l'oggetto layout</strong>: `lo_layout = lo_salv={'>'}get_layout()`.
                        </li>
                        <li>
                            <strong className="text-gray-800">Impostare la Chiave del Layout</strong>: È un passaggio obbligatorio. Senza una chiave univoca, il sistema non sa come salvare o recuperare il layout. Tipicamente si usa `lo_layout={'>'}set_key( VALUE #( report = sy-repid ) )`.
                        </li>
                        <li>
                            <strong className="text-gray-800">Rimuovere le Restrizioni di Salvataggio</strong>: Per permettere all'utente di salvare, si usa `lo_layout={'>'}set_save_restriction( if_salv_c_layout={'>'}restrict_none )`.
                        </li>
                        <li>
                            <strong className="text-gray-800">Abilitare il Layout di Default</strong>: Per rendere disponibile la checkbox "Impostazione standard" nella finestra di dialogo di salvataggio, è necessario chiamare `lo_layout={'>'}set_default( abap_true )`.
                        </li>
                        <li>
                            <strong className="text-gray-800">Impostare un Layout Iniziale</strong>: Per caricare un layout specifico all'avvio del report, si utilizza `lo_layout={'>'}set_initial_layout( 'NOME_VARIANTE' )`.
                        </li>
                    </ol>
                </article>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        4.3. Miglioramenti Visivi (CL_SALV_DISPLAY_SETTINGS)
                    </h3>
                    <p className="text-gray-700 leading-relaxed mb-4">
                        La classe `CL_SALV_DISPLAY_SETTINGS` permette di personalizzare l'aspetto generale della griglia.
                    </p>
                    <ul className="list-disc list-inside text-gray-700 space-y-2">
                        <li>
                            <strong className="text-gray-800">Intestazione della Lista</strong>: `set_list_header( 'Titolo del Mio Report' )` imposta un titolo per l'ALV.
                        </li>
                        <li>
                            <strong className="text-gray-800">Pattern a Strisce (Zebra)</strong>: `set_striped_pattern( abap_true )` migliora la leggibilità alternando il colore di sfondo delle righe.
                        </li>
                        <li>
                            <strong className="text-gray-800">Contenuti Aggiuntivi</strong>: È possibile aggiungere contenuti complessi sopra (`set_top_of_list`) e sotto (`set_end_of_list`) la griglia dati, utilizzando oggetti di tipo `CL_SALV_FORM_LAYOUT_GRID` per creare intestazioni strutturate con etichette, testi e loghi.
                        </li>
                    </ul>
                </article>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        4.4. Integrazione con GUI Status Personalizzato
                    </h3>
                    <p className="text-gray-700 leading-relaxed">
                        Come accennato, per gli ALV a schermo intero, la personalizzazione della toolbar avviene tramite un `PF-STATUS` custom, che viene assegnato con `lo_salv={'>'}set_screen_status( report = sy-repid pfstatus = 'MIO_STATUS' )`. Per esigenze molto avanzate, è possibile implementare una enhancement (modifica implicita) nella classe `CL_SALV_MODEL_BASE` per permettere l'esclusione programmatica di singole funzioni standard da un `PF-STATUS` a schermo intero, una soluzione potente ma non standard.
                    </p>
                </article>
            </section>

            {/* Sezione 5 */}
            <section className="mb-10">
                <h2 className="text-3xl font-bold text-gray-800 mb-4 border-b-2 border-indigo-500 pb-2">
                    Sezione 5: Programmazione Guidata dagli Eventi per Report Interattivi
                </h2>
                <p className="text-gray-700 leading-relaxed mb-6">
                    Il framework SALV adotta un paradigma di programmazione guidato dagli eventi per gestire le interazioni dell'utente, promuovendo una netta separazione tra la logica di visualizzazione e la logica di business.
                </p>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        5.1. Il Modello di Gestione degli Eventi SALV
                    </h3>
                    <p className="text-gray-700 leading-relaxed">
                        Il fulcro di questo modello è la classe `CL_SALV_EVENTS_TABLE`. Il pattern standard prevede la creazione di una classe locale (es. `LCL_HANDLER`) che contiene i metodi gestori di eventi. Un'istanza di questa classe viene poi registrata per ascoltare gli eventi dell'ALV tramite l'istruzione `SET HANDLER mio_handler={'>'}on_double_click FOR lo_events`.
                    </p>
                </article>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        5.2. Implementazione delle Interazioni Utente Principali
                    </h3>
                    <ul className="list-disc list-inside text-gray-700 space-y-2">
                        <li>
                            <strong className="text-gray-800">DOUBLE_CLICK</strong>: Questo evento è ideale per implementare funzionalità di drill-down. Il metodo gestore riceve come parametri la riga (`row`) e la colonna (`column`) su cui è stato effettuato il doppio clic, permettendo di leggere i dati della riga selezionata e, ad esempio, navigare a un'altra transazione con `CALL TRANSACTION`.
                        </li>
                        <li>
                            <strong className="text-gray-800">LINK_CLICK (Hotspot/Checkbox)</strong>: Questo evento a singolo clic viene scatenato quando l'utente interagisce con una colonna definita come hotspot o checkbox. La struttura del metodo gestore è identica a quella del `DOUBLE_CLICK`, ricevendo riga e colonna come parametri.
                        </li>
                    </ul>
                </article>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        5.3. Rispondere a Funzioni Personalizzate (ADDED_FUNCTION)
                    </h3>
                    <p className="text-gray-700 leading-relaxed">
                        Quando un utente preme un pulsante personalizzato aggiunto alla toolbar, viene scatenato l'evento `added_function`. Il metodo gestore riceve il nome della funzione (`e_salv_function`), permettendo di utilizzare un'istruzione `CASE` per instradare la logica corretta per ogni pulsante.
                    </p>
                </article>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        5.4. Gestione della Selezione (CL_SALV_SELECTIONS)
                    </h3>
                    <p className="text-gray-700 leading-relaxed mb-4">
                        La classe `CL_SALV_SELECTIONS` offre il controllo su come l'utente può selezionare i dati.
                    </p>
                    <ul className="list-disc list-inside text-gray-700 space-y-2">
                        <li>
                            <strong className="text-gray-800">Controllo della Modalità di Selezione</strong>: Dopo aver ottenuto l'oggetto selezioni (`lo_selections = lo_salv={'>'}get_selections()`), si può impostare la modalità tramite `set_selection_mode()`. Le opzioni includono la selezione di singole celle (`cell`), righe e colonne (`row_column`), selezione singola (`single`) o multipla (`multiple`).
                        </li>
                        <li>
                            <strong className="text-gray-800">Recupero delle Selezioni Utente</strong>: Per elaborare in blocco i record selezionati dall'utente, si utilizza il metodo `get_selected_rows()`, che restituisce una tabella con gli indici delle righe selezionate.
                        </li>
                    </ul>
                </article>
            </section>

            {/* Sezione 6 */}
            <section className="mb-10">
                <h2 className="text-3xl font-bold text-gray-800 mb-4 border-b-2 border-indigo-500 pb-2">
                    Sezione 6: Il SALV Modificabile: Guida alle Tecniche Moderne e Legacy
                </h2>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        6.1. La Filosofia di Progettazione: Perché il SALV era "Solo Visualizzazione"
                    </h3>
                    <p className="text-gray-700 leading-relaxed">
                        È fondamentale comprendere il contesto storico: `CL_SALV_TABLE` è stato originariamente progettato come uno strumento di reporting semplificato e di alto livello. La modificabilità non faceva parte del suo scopo iniziale. Questa decisione di progettazione spiega perché per anni gli sviluppatori hanno fatto ricorso a workaround complessi e non supportati per colmare questa lacuna funzionale, una domanda persistente da parte della comunità di sviluppo.
                    </p>
                </article>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        6.2. La Best Practice di S/4HANA (Release &gt;= 7.56): L'API extended_grid_api
                    </h3>
                    <p className="text-gray-700 leading-relaxed mb-4">
                        La risposta di SAP a questa esigenza è arrivata con la release 7.56, che ha introdotto un'API ufficiale per la modifica. Questo è il metodo raccomandato e supportato per creare griglie modificabili nei moderni sistemi S/4HANA. L'introduzione di questa API non è solo una nuova funzionalità, ma rappresenta un cambiamento strategico che riconosce e colma un divario funzionale di lunga data, rendendo il framework SALV più potente e versatile.
                        Il processo per creare una griglia modificabile è il seguente:
                    </p>
                    <ol className="list-decimal list-inside text-gray-700 space-y-2">
                        <li>Aggiungere pulsanti personalizzati "Modifica" e "Salva" alla toolbar tramite `add_function`.</li>
                        <li>Nel gestore eventi per il pulsante "Modifica", ottenere l'API estesa: `ls_api = o_salv={'>'}extended_grid_api()`.</li>
                        <li>Ottenere il controller di modifica: `ls_edit = ls_api={'>'}editable_restricted()`.</li>
                        <li>Utilizzare i metodi dell'oggetto `ls_edit` per rendere modificabili colonne specifiche o l'intera griglia.</li>
                        <li>Nel gestore eventi per il pulsante "Salva", recuperare i dati modificati dalla tabella interna (che è stata passata per riferimento al metodo `FACTORY`) ed eseguire la logica di aggiornamento sul database.</li>
                    </ol>
                </article>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        6.3. Workaround Legacy e Avviso sul Debito Tecnico
                    </h3>
                    <p className="text-gray-700 leading-relaxed">
                        Per completezza storica o per chi lavora su sistemi più datati, è utile conoscere le tecniche legacy, sebbene siano fortemente sconsigliate e non supportate. Questi approcci si basano sull'accesso all'istanza sottostante di `CL_GUI_ALV_GRID`, con il rischio che smettano di funzionare dopo un aggiornamento di sistema. Tecniche come l'uso del modulo funzione `GET_GLOBALS_FROM_SLVC_FULLSCR` o la gestione dell'evento `AFTER_REFRESH` dovrebbero essere considerate come artefatti storici o soluzioni di ultima istanza.
                    </p>
                </article>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        6.4. Strategie di Modifica Alternative
                    </h3>
                    <p className="text-gray-700 leading-relaxed mb-4">
                        Esistono alternative pratiche e completamente supportate alla modifica diretta dell'intera griglia.
                    </p>
                    <ul className="list-disc list-inside text-gray-700 space-y-2">
                        <li>
                            <strong className="text-gray-800">Checkbox Modificabili</strong>: Come descritto nella Sezione 2.4, è possibile definire una colonna come checkbox e aggiornare la tabella interna tramite l'evento `LINK_CLICK`. Questo è un modo semplice e robusto per gestire selezioni binarie.
                        </li>
                        <li>
                            <strong className="text-gray-800">Dialoghi Modali per la Modifica</strong>: Un pattern comune e sempre supportato consiste nell'utilizzare un doppio clic o un pulsante personalizzato per selezionare una riga e aprire una finestra di dialogo modale (un dynpro di popup) per modificare i dati di quella specifica riga. Questo approccio garantisce stabilità e manutenibilità.
                        </li>
                    </ul>
                </article>
            </section>

            {/* Sezione 7 */}
            <section className="mb-10">
                <h2 className="text-3xl font-bold text-gray-800 mb-4 border-b-2 border-indigo-500 pb-2">
                    Sezione 7: Sviluppo Robusto: Gestione delle Eccezioni e Best Practice
                </h2>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        7.1. Anticipare e Gestire le Eccezioni SALV
                    </h3>
                    <p className="text-gray-700 leading-relaxed mb-4">
                        Uno sviluppo di qualità richiede una gestione proattiva degli errori. Il framework SALV utilizza un sistema di eccezioni basato su classi. Le principali classi di eccezione da conoscere sono:
                    </p>
                    <ul className="list-disc list-inside text-gray-700 space-y-2">
                        <li>
                            <strong className="text-gray-800">CX_SALV_MSG</strong>: L'eccezione generica, utilizzata per errori generali durante l'istanziazione o la configurazione.
                        </li>
                        <li>
                            <strong className="text-gray-800">CX_SALV_NOT_FOUND</strong>: Sollevata quando si tenta di accedere a un oggetto inesistente, come una colonna con un nome errato.
                        </li>
                        <li>
                            <strong className="text-gray-800">CX_SALV_DATA_ERROR</strong>: Si verifica in genere a causa di incongruenze nei dati, come una tabella di colori per le celle non valida.
                        </li>
                        <li>
                            <strong className="text-gray-800">CX_SALV_EXISTING</strong>: Sollevata quando si tenta di aggiungere un oggetto che esiste già, come un filtro o una funzione con lo stesso nome.
                        </li>
                    </ul>
                    <p className="text-gray-700 leading-relaxed mt-4">
                        È imperativo che tutte le chiamate al metodo `FACTORY` e ad altri metodi che possono fallire (es. `get_column`, `add_filter`) siano racchiuse in blocchi `TRY...CATCH` appropriati.
                    </p>
                </article>

                <article className="mb-6">
                    <h3 className="text-2xl font-semibold text-gray-700 mb-3">
                        7.2. Raccomandazioni e Conclusione
                    </h3>
                    <p className="text-gray-700 leading-relaxed mb-4">
                        La classe `CL_SALV_TABLE` si è affermata come uno strumento potente, flessibile e moderno per il reporting in SAP S/4HANA. Offre un percorso di sviluppo rapido per la maggior parte delle esigenze, astraendo gran parte della complessità associata ai vecchi strumenti ALV.
                    </p>
                    <p className="text-gray-700 leading-relaxed mb-4">
                        La scelta tra `CL_SALV_TABLE` e la classe di livello inferiore `CL_GUI_ALV_GRID` può essere riassunta come segue:
                    </p>
                    <h4 className="text-xl font-semibold text-gray-800 mb-2">Utilizzare CL_SALV_TABLE per:</h4>
                    <ul className="list-disc list-inside text-gray-700 space-y-2 mb-4">
                        <li>La stragrande maggioranza (90%+) delle esigenze di reporting.</li>
                        <li>Sviluppo rapido e codice più pulito.</li>
                        <li>Funzionalità interattive standard (ordinamento, filtro, layout).</li>
                        <li>Griglie modificabili in modo ufficialmente supportato (su release &gt;= 7.56).</li>
                    </ul>
                    <h4 className="text-xl font-semibold text-gray-800 mb-2">Utilizzare CL_GUI_ALV_GRID per:</h4>
                    <ul className="list-disc list-inside text-gray-700 space-y-2 mb-4">
                        <li>Scenari che richiedono un controllo estremamente granulare.</li>
                        <li>Requisiti specifici non coperti da SALV, come la personalizzazione del testo dei subtotali.</li>
                        <li>Gestione di eventi complessi non disponibili nel modello SALV.</li>
                        <li>Necessità di griglie modificabili su sistemi SAP più datati.</li>
                    </ul>
                    <p className="text-gray-700 leading-relaxed">
                        In conclusione, `CL_SALV_TABLE` dovrebbe essere considerato lo strumento predefinito e preferito per tutto il nuovo sviluppo di report ALV in un ambiente SAP S/4HANA moderno.
                    </p>
                </article>
            </section>
        </div>
    );
};

export default ClSalvTableDoc;
