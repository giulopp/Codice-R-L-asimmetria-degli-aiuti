#L'asimmetria degli aiuti

Codice R della tesi magistrale "L'asimmetria degli aiuti: un'indagine sull'allocazione
delle risorse tra global cluster OCHA" (Università Cattolica del Sacro Cuore,
Facoltà di Economia, a.a. 2025/2026; relatore prof. Vincenzo Nardelli).

#Obiettivo
Verificare se, all'interno dello stesso piano di risposta, alcuni cluster umanitari
ricevono una copertura del fabbisogno sistematicamente più alta o più bassa di altri.

#Dati
OCHA Financial Tracking Service, "Global – Requirements and Funding Data",
Humanitarian Data Exchange: https://data.humdata.org/dataset/global-requirements-and-funding-data
File: fts_requirements_funding_globalcluster_global.csv, scaricato il 9 settembre 2026.
Per eseguire lo script, metti il file nella stessa cartella dello script.

#Passaggi dell'analisi
1. Raccordo delle categorie FTS agli 8 cluster IASC 2026; esclusione del 2026;
   finanziamenti mancanti posti a zero; copertura = fondi ricevuti / fabbisogno.
2. Statistiche descrittive e grafici (Capitolo 2).
3. Modello con effetti fissi di piano (fixest::feols), Health come riferimento,
   errori standard raggruppati per paese (Capitolo 3).
4. Stime per periodo (2000-2013, 2014-2019, 2020-2025) e per crisi brevi/lunghe,
   con test di Wald sulle interazioni.

#Software
R 4.4.2; dplyr 1.1.4; fixest [versione]; ggplot2 [versione]; maps 3.4.3.

#Limiti dei dati
La comunicazione dei finanziamenti a FTS non è obbligatoria; i fondi registrati come
"Not specified" o "Multiple clusters/sectors" sono esclusi; un finanziamento mancante
è trattato come zero. Si veda il paragrafo 4.4.1 della tesi.
