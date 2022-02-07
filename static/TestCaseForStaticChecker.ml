(*Un set vuoto*)
teval (Empty(String)) emptT;;
let emptStr = eval (Empty(String)) emptEnv;;
teval (Empty(Int)) emptT;;
let emptInt = eval (Empty(Int)) emptyEnv;;
toList emptInt;;

("Crea un singleton")
teval (Singleton(CstString("Ciao"), String)) emptT;;
let singStr = eval (Singleton(CstString("Ciao"), String)) emptyEnv;;
toList singStr;; (*[String "ciao"]*)
teval (Singleton(CstInt(17), Int)) emptT;;
let singInt = eval (Singleton(CstInt(17), Int)) emptyEnv;;
toList singInt;; (*[Int 17]*)

(*Non Vengono valutati poiche' i tipi differiscono*)
teval (Singleton(CstString("Ciao"), Int)) emptT;;
let singStr = eval (Singleton(CstString("Ciao"), Int)) emptyEnv;;
teval (Singleton(CstString("Ciao"), Int)) emptT;;
let singStr = eval (Singleton(CstString("Ciao"), Int)) emptyEnv;;

(*Caso n >= m*)
teval (Of(String, Sum(CstInt(3), CstInt(6)), CstString("9"))) emptT;;
let evOfStr = eval (Of(String, Sum(CstInt(3), CstInt(6)), CstString("9"))) emptyEnv;;
toList evOfStr;; (*[]*)

(*Un set che inzia da un un n e finisce in m esclusi, dove n < m altrimenti restituisce un set vuoto*)
teval (Of(Int, Sum(CstInt(5), CstInt(10)), CstInt(3))) emptT;;
let evOfInt = eval (Of(Int, Sum(CstInt(5), CstInt(10)), CstInt(3))) emptyEnv;;
teval(Insert(CstString("777"), evOfStr)) emptT;;
let insrSetStr = eval(Insert(CstString("777"), evOfStr)) emptyEnv;;

(*Non viene valutato poiche' i tipi sono differenti*)
teval (Union(insrSetStr, evOfInt)) emptT;;

(*Non viene valutato poiche' i tipi sono differenti*)
teval (Inter(insrSetInt, evOfStr)) emptT;;

(*Non e' valutata per i tipi diversi*)
teval (Diff(setStr, evInt)) emptT;;

teval (Singleton(CstString("Ciao"), String)) emptT;;
let setStr = eval (Singleton(CstString("Ciao"), String)) emptyEnv;;
teval (Insert(CstString("Arrivederci"), setStr)) emptT;;
eval (Insert(CstString("Arrivederci"), setStr)) emptyEnv;;
teval (Insert(CstString("Buongiorno"), setStr)) emptT;;
eval (Insert(CstString("Buongiorno"), setStr)) emptyEnv;;
teval (Insert(CstString("Salve"), setStr)) emptT;;
eval (Insert(CstString("Salve"), setStr)) emptyEnv;;

(*Non Verra' valutata*)
 teval (Delete(CstInt(34), setStr)) emptT;;


(*Non verra' valutata*)
teval (IsIn(CstInt(53), setStr)) emptT;;

(*Non viene valutato*)
let pred = Sub(CstInt(6), Sum(CstInt(4), CstInt(4)));;
teval (For_all(pred, evOfInt)) emptT;;

(*Non viene valutato*)
let pred = Sub(CstInt(6), Sum(CstInt(4), CstInt(4)));;
teval (Exists(pred, evOfInt)) emptT;;

(*Non viene valutato*)
let pred = Sub(CstInt(6), Sum(CstInt(4), CstInt(4)));;
teval (Filter(pred, evOfInt)) emptT;;

 (*Non viene valutato*)
 let pred = Sub(CstInt(6), Sum(CstInt(4), CstInt(4)));;
teval (Exists(pred, evOfInt)) emptT;;