#!/bin/bash

# File per memorizzare il numero del commit
COUNTER_FILE=".commit_counter"

# Se il file contatore non esiste, lo inizializziamo a 0
if [ ! -f "$COUNTER_FILE" ]; then
    count=0
else
    count=$(cat "$COUNTER_FILE")
fi

# Incrementiamo il contatore
next_count=$((count + 1))
commit_message="commit stabile $next_count"

echo "Esecuzione del deploy..."
echo "Commit message: $commit_message"

# Eseguiamo i comandi Git e il deploy
# Usiamo '&&' per assicurarci che lo script si fermi se un comando fallisce
git add . && \
git commit -m "$commit_message" && \
git push && \
echo "$next_count" > "$COUNTER_FILE" && \
echo "Push su Git completato. Avvio del deploy su Vercel..." && \
vercel --prod && \
echo "Deploy completato con successo!"

