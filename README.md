## General

Ocaml project

## Content

### Descrizione: Progettazione e sviluppo di un interprete in OCaml
Il progetto prevede la progettazione e realizzazione di una semplice estensione del linguaggio didattico
funzionale presentato a lezione che permetta di manipolare insiemi. Un insieme è una collezione di valori,
non ordinati, che non contiene valori duplicati.
Un insieme può essere create a partire dall’insieme vuoto
let s = empty(t)
(* risultato è l’insieme vuoto di tipo t *)
dove t è il tipo degli elementi dell’insieme.
Alternativamente può essere creato attraverso un valore:
let s = singleton(“hello”, String)
(* risultato è un insieme di stringhe contenente la stringa “hello” *)
Infine può essere creato a partire da espressioni
let s = of(int, 3+5, succ(5))
(* risultato è l’inisme di interi che contiene i valori 7 e 6 *)
Gli insiemi sono caratterizzati da numerose operazioni di base (tra cui unione,
intersezione, differenza). Ipotizziamo inoltre di avere altre operazioni che
permettono:
1. inserire (rimuovere) un elemento da un insieme,
2. controllare se un insieme è vuoto,
3. controllare se un elemento appartiene ad un insieme,
4. controllare se un insieme è sottoinsieme di un altro insieme,
5. determinare il minimo (massimo) valore di un insieme,
Oltre a questo insieme di operazioni di base, il tipo insieme prevede un insieme
di operatori di natura “funzionale”.
• For_all(predicate, aSet) controlla se tutti gli elementi dell’insieme
soddisfano la proprietà definita dal parametro “predicate”. Il paramentro
“predicate” è una funzione che applicata ad un elemento dell’insieme
restituisce un valore booleano.
• Exists(predicate aSet) coptrolla se esiste almeno un elemento dell’insieme che soddisfa la proprietà definita dal parametro “predicate”.
• Filter(predicate, aSet) restituisce l’insieme degli elementi dell’insieme
che soddisfano la proprietà definita dal parametro “predicate”.
• Map(function, aSet) restitusce l’insieme dei valori v tali che v =
function(x) dove x appartiene a aSet.
1. Definire le regole operazionali per l’introduzione del tipo di dato set nel linguaggio didattico.
2. Definire le regole operazionali per le classi di operazione previste dal tipo di dato set3. Estendere l’interprete OCaml del linguaggio funzionale assumendo scoping statico.
4. Definire il type checker dinamico del linguaggio risultante.
5. Si verifichi la correttezza dell’interprete progettando ed eseguendo una quantità di casi di test
sufficiente a testare tutti gli operatori aggiuntivi.
6. Opzionale (per la consegna del 15 Dicembre) definire il type checker statico.
