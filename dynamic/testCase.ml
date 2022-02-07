(*Un set vuoto*)
let emptStr = eval (Empty(String)) emptyEnv;;
let emptInt = eval (Empty(Int)) emptyEnv;;
toList emptInt;;
("Crea un singleton")
let singStr = eval (Singleton(CstString("Ciao"), String)) emptyEnv;;
toList singStr;; (*[String "ciao"]*)
let singInt = eval (Singleton(CstInt(17), Int)) emptyEnv;;
toList singInt;; (*[Int 17]*)

(*Non Vengono valutati poiche' i tipi differiscono*)
let singStr = eval (Singleton(CstString("Ciao"), Int)) emptyEnv;;
let singStr = eval (Singleton(CstString("Ciao"), String)) emptyEnv;;

(*Caso n >= m*)
let evOfStr = eval (Of(String, Sum(CstInt(3), CstInt(6)), CstString("9"))) emptyEnv;;
toList evOfStr;; (*[]*)

(*Un set che inzia da un un n e finisce in m esclusi, dove n < m altrimenti restituisce un set vuoto*)
let evOfInt = eval (Of(Int, Sum(CstInt(5), CstInt(10)), CstInt(3))) emptyEnv;;
toList evOfInt;; (*[Int 10; Int 11; Int 9; Int 4; Int 5; Int 12; Int 13; Int 7; Int 8; Int 14; Int 6]*)
let evOfStr = eval (Of(String, Sum(CstInt(3), CstInt(44)), CstString("5"))) emptyEnv;;
toList evOfStr;; (*[String "14"; String "18"; ...; String 31]*)
let evOfStr = eval (Of(String, Sum(CstInt(3), CstInt(6)), CstString("3"))) emptyEnv;;
toList evOfStr;; (*[String "8"; String "5"; String "4"; String "6"; String "7"]*)
let evOfInt = eval (Of(Int, Sum(CstInt(3), CstInt(6)), CstString("4"))) emptyEnv;;
toList evOfInt;; (*[Int 5; Int 7; Int 8; Int 6]*)
let evOfStr = eval (Of(String, Sum(CstInt(3), CstInt(6)), CstInt(3))) emptyEnv;;
oList evOfStr;; (*[String "8"; String "5"; String "4"; String "6"; String "7"]*)


(*Non viene valutato*)
let evOfStr = eval (Of(String, Sum(CstInt(3), CstInt(6)), CstString("sd"))) emptyEnv;;

(*Unione di due set senza modificare gli originali*)
let newSet = eval (Union(evOfStr, evOfStr)) emptyEnv;;
toList newSet;; (*[String "8"; String "6"; String "4"; String "5"; String "7"]*)
let newSet = eval (Union(evOfInt, evOfInt)) emptyEnv;;
toList newSet;; (*[Int 5; Int 7; Int 8; Int 6]*)

(*Inserzione di un elemento con la modifica del set originario e unione di due Set*)
let insrSetInt = eval(Insert(CstInt(23), evOfInt)) emptyEnv;;
toList insrSet;; (*[Int 23; Int 5; Int 7; Int 8; Int 6]*)
let newSet = eval (Union(insrSetInt, evOfInt)) emptyEnv;;
toList newSet;; (*[Int 23; Int 5; Int 6; Int 7; Int 8]*)

let insrSetStr = eval(Insert(CstString("777"), evOfStr)) emptyEnv;;
toList insrSet;; (*[String "23"; String "777"; String "8"; String "5"; String "4"; String "6"; String "7"]*)
let newSet = eval (Union(insrSetStr, evOfStr)) emptyEnv;;
toList newSet;; (*[String "777"; String "8"; String "6"; String "4"; String "5"; String "7"]*)

(*Non viene valutato poiche' i tipi sono differenti*)
let newSet = eval (Union(insrSet, evOfInt)) emptyEnv;;

(*Intersezione tra set, restituisce un nuovo set senza modificare gli originali*)
let intersezione = eval (Inter(insrSetInt, insrSetInt)) emptyEnv;;
toList intersezione;; (*[Int 23; Int 5; Int 7; Int 8; Int 6]*)
let intersezione = eval (Inter(insrSetInt, emptInt)) emptyEnv;;
toList intersezione;; (*[]*)
let evOfInt = eval (Of(Int, Sum(CstInt(5), CstInt(9)), CstString("4"))) emptyEnv;;
let intersezione = eval (Inter(insrSetInt, evOfInt)) emptyEnv;;
toList intersezione;; (*[Int 5; Int 7; Int 8; Int 6]*)

(*Non viene valutato poiche' i tipi sono differenti*)
let intersezione = eval (Inter(insrSetInt, evOfStr)) emptyEnv;;

(*Differenza tra set senza la modifica degli originali*)
let differenza = eval (Diff(insrSetInt, evOfInt)) emptyEnv;;
toList differenza;; (*[23]*)
let evInt = eval (Of(Int, CstInt(13), CstInt(5))) emptyEnv;;
let evInt2 = eval (Of(Int, CstInt(17), CstInt(9))) emptyEnv;;
let l = eval(Diff(evInt, evInt2)) emptyEnv;;
toList l;; (*[Int 9; Int 7; Int 8; Int 6]*)

let empSet = eval (Empty(String)) emptyEnv;;
let setStr = eval (Singleton(CstString("Ciao"), String)) emptyEnv;;
eval (Insert(CstString("Arrivederci"), setStr)) emptyEnv;;
eval (Insert(CstString("Buongiorno"), setStr)) emptyEnv;;
eval (Insert(CstString("Salve"), setStr)) emptyEnv;;
toList setStr;;
let differenza = eval(Diff(empSet, setStr)) emptyEnv;;
toList differenza;; (*[]*)
let differenza = eval(Diff(setStr, empSet)) emptyEnv;;
toList differenza;; (*[String "Arrivederci"; String "Buongiorno"; String "Ciao"; String "Salve"]*)

(*Non e' valutata per i tipi diversi*)
let differenza = eval (Diff(setStr, evInt)) emptyEnv;;

(*Rimozione di un elemento dall'insieme originario*)
let elim = eval (Delete(CstString("Salve"), setStr)) emptyEnv;;
toList elim;; (*[String "Arrivederci"; String "Buongiorno"; String "Ciao"]*)
let elim = eval (Delete(CstString("Salve"), setStr)) emptyEnv;;
toList elim;; (*[String "Arrivederci"; String "Buongiorno"; String "Ciao"]*)

(*Non Verra' valutata*)
let elim = eval (Delete(CstInt(34), setStr)) emptyEnv;;

let em = eval (IsEmpty(setStr)) emptyEnv;; (*Bool false*)
let em = eval (IsEmpty(empSet)) emptyEnv;; (*Bool true*)
let inOrnot = eval (IsIn(CstString("Salve"), setStr)) emptyEnv;; (*Bool false*)
let inOrnot = eval (IsIn(CstString("Arrivederci"), setStr)) emptyEnv;; (*Bool true*)
let inOrnot = eval (IsIn(CstInt(9), evInt)) emptyEnv;; (*Bool true*)
let inOrnot = eval (IsIn(CstInt(15), evInt)) emptyEnv;; (*Bool false*)


(*Non verra' valutata*)
let inOrnot = eval (IsIn(CstInt(53), setStr)) emptyEnv;;

(*Controlla se un set e' un sottoinsieme*)
eval (Insert(CstString("Arrivederci"), empSet)) emptyEnv;;
eval (Insert(CstString("Ciao"), empSet)) emptyEnv;;
let sub = eval (IsSubset(empSet, setStr)) emptyEnv;; (*Bool true*)
eval(Delete(CstString("Arrivederci"), empSet)) emptyEnv;;
eval(Delete(CstString("Ciao"), empSet)) emptyEnv;;
toList empSet;; (*[]*)
let sub = eval (IsSubset(empSet, setStr)) emptyEnv;; (*Bool true*)
eval(Insert(CstString("Buona serata"), empSet)) emptyEnv;;
let sub = eval (IsSubset(empSet, setStr)) emptyEnv;; (*Bool false*)

(*Min e Max*)
let m = eval (Min(setStr)) emptyEnv;; (*String "Arrivederci"*)
eval(Delete(CstString("Buona serata"), empSet)) emptyEnv;;
toList empSet;;
let m = eval (Min(empSet)) emptyEnv;; (*None*)
let m = eval (Min(evInt)) emptyEnv;; (*Int 6*)

let m = eval (Max(setStr)) emptyEnv;; (*String "Ciao"*)
let m = eval (Max(evInt)) emptyEnv;; (*Int 12*)
let m = eval (Max(emptInt)) emptyEnv;; (*None*)

(*Funzione con predicato: For_all*)
let pred = Fun("x", Ifthenelse(Greater(Den("x"), CstString("Hi")), CstTrue, CstFalse));;
eval (For_all(pred, setStr)) emptyEnv;;(*Bool false*)
let pred = Fun("x", Eq(Den("x"), CstInt(15)));;
eval (For_all(pred, evInt)) emptyEnv;; (*Bool false*)
let pred = Fun("x", Greater(Den("x"), CstInt(3)));;
eval (For_all(pred, evInt)) emptyEnv;; (*Bool true*)

(*Non viene valtato*)
let pred = Fun("x", Sum(Den("x"), CstInt(4)));;
eval (For_all(pred, evInt)) emptyEnv;;
let pred = Sub(CstInt(6), Sum(CstInt(4), CstInt(4)));;
eval (For_all(pred, evInt)) emptyEnv;;

(*Funzione con predicato: Exists*)
let pred = Fun("x", Ifthenelse(Greater(Den("x"), CstString("Hi")), CstTrue, CstFalse));;
eval (Exists(pred, setStr)) emptyEnv;;(*Bool true*)
let pred = Fun("x", Eq(Den("x"), CstInt(15)));;
eval (Exists(pred, evInt)) emptyEnv;; (*Bool false*)
let pred = Fun("x", Eq(Den("x"), CstInt(9)));;
eval (Exists(pred, evInt)) emptyEnv;; (*Bool true*)
let pred = Fun("x", Greater(Den("x"), CstInt(19)));;
eval (Exists(pred, evInt)) emptyEnv;; (*Bool false*)

(*Non viene valtato*)
let pred = Fun("x", Sum(Den("x"), CstInt(4)));;
eval (Exists(pred, evInt)) emptyEnv;;
let pred = Sub(CstInt(6), Sum(CstInt(4), CstInt(4)));;
eval (Exists(pred, evInt)) emptyEnv;;

(*Funzione con predicato: Filter*)
(*Filtra il set e restituisce un nuovo insieme con elementi che soddisfanno il predicato
senza modificare il set originario*)
let pred = Fun("x", Ifthenelse(Greater(Den("x"), CstString("Hi")), CstTrue, CstFalse));;
let newSet = eval (Filter(pred, setStr)) emptyEnv;;
toList newSet;; (*[String "Salve"]*)
let pred = Fun("x", Eq(Den("x"), CstInt(15)));;
let newSet = eval (Filter(pred, evInt)) emptyEnv;;
toList newSet;; (*[]*)
let pred = Fun("x", Eq(Den("x"), CstInt(9)));;
let newSet = eval (Filter(pred, evInt)) emptyEnv;;
toList newSet;; (*[Int 9]*)
let pred = Fun("x", Greater(Den("x"), CstInt(7)));;
let newSet = eval (Filter(pred, evInt)) emptyEnv;; 
toList newSet;; (*[Int 10; Int 11; Int 9; Int 12; Int 8]*)

(*Non viene valtato*)
let pred = Fun("x", Sum(Den("x"), CstInt(4)));;
eval (Filter(pred, evInt)) emptyEnv;;
let pred = Sub(CstInt(6), Sum(CstInt(4), CstInt(4)));;
eval (Filter(pred, evInt)) emptyEnv;;

(*Funzione con una funzione: Map*)
(*Applica la funzione a tutti gli elementi senza modificare la lista originaria
e ritorna una nuova lista.*)
let pred = Fun("x", Sum(Den("x"), CstInt(4)));;
let newSet = eval (Map(pred, evInt)) emptyEnv;;
toList newSet;; (*[Int 15; Int 10; Int 11; Int 12; Int 13; Int 16; Int 14]*)
let pred = Fun("x", Concat(Den("x"), CstString("AUE")));;
let newSet = eval(Map(pred, setStr)) emptyEnv;;
toList newSet;; (*[String "SalveAUE"; String "CiaoAUE"; String "ArrivederciAUE";
 String "BuongiornoAUE"]*)