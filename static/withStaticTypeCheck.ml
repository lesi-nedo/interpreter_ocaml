
(* The language *)
(*I tipi ammessi per il set, serve per assegnare il tipo al modulo Table*)
type k = Int of int | String of string | None
(*La creazione di un modulo attraverso functor per avere una hash funzione 
che lavori su tipi primitivi*)
module Table = Hashtbl.Make(struct
  type t = k (*k Table.t*)
  let eva ex = match ex with
    | String(n) -> Hashtbl.hash n
    | Int(n) -> Hashtbl.hash n
    | _ -> failwith("run-time error")
  let equal : 'a -> 'a -> bool = fun x y -> x = y
  let hash x = eva x 
end)
type ide = string 
(*Tipi per il set*)
type typeSet =  Int | String
let typeof e   = match e with String -> "string"
                      | Int -> "int"
                     

type exp = CstInt of int
    | CstString of string
    | CstTrue 
    | CstFalse
    | Den of ide
    | Sum of exp * exp
    | Greater of exp * exp
    | Sub of exp * exp
    | Times of exp * exp
    | Ifthenelse of exp * exp * exp
    | Eq of exp * exp
    | Let of ide * exp * exp
    | Fun of ide * exp
    | Letrec of ide * ide * exp * exp
    | Apply of exp * exp
        | Empty of typeSet
        | Singleton of exp * typeSet
        | Of of typeSet * exp * exp(*first exp can be any of the operation allowed on int, second has to be in the range succ(a+b) < a+b  *)
        | Union of evT * evT
        | Inter of evT * evT
        | Diff of evT * evT
        | Insert of exp * evT
        | Delete of exp * evT
        | IsEmpty of evT
        | IsIn of exp * evT
        | IsSubset of evT * evT
        | Min of evT
        | Max of evT
        | For_all of exp * evT
        | Exists of exp * evT
        | Filter of exp * evT
        | Map of exp * evT 
        | Concat of exp * exp
and  set =  k Table.t * typeSet 
and  evT = Int of int 
        | Bool of bool
        | String of string 
        | Set of set
        | Closure of ide * exp * evT env 
        | RecClosure of ide * ide * exp * evT env 
        | Unbound
        | None
and  'v env = (string * 'v) list
(*Prende un valore del tipo evT e restituisce il tipo del modulo*)
let toK (x : evT) : k = match x with
  | String(n) -> String(n)
  | Int(n) ->  Int(n)
  |_ -> failwith("run-time error")
(*Creazione dell'ambiente statico*)
let emptyEnv  = [ ("", Unbound)] 

(*Trasforma dal tipo k al tipo exp*)
let fromKtoExp (e : k) : exp = match e with 
  | String(n) -> CstString(n)
  | Int(n) ->  CstInt(n)
  |_ -> failwith("run-time error")
(*restituisce il tipo di un valore valutata*)
let returnTypeE (e: evT) : typeSet  = match e with 
    | Int(n) -> Int
    | String(n) -> String
    | _-> failwith("Run-time error")
(*restituisce la dimensione della tabella hash*)
let size s = match s with 
           | Set(h, t) -> Table.length h
           | _ -> failwith("Run-time error")
(*restituisce il tipo primitivo dentro al tipo k*)
let returnPrimStr (e : k) = match e with
    String(n) -> n
    | _ -> failwith("run-time error") 
let returnPrimInt (e : k) = match e with
    Int(n) -> n
    | _ -> failwith("run-time error") 

(*restituisce il tipo del Set*)
let returnType set = match set with Set(h, t) -> t
                | _ -> failwith("run-time error")
(*crea un Set vuoto con un argomento opzionale per scegliere la dimensione della tabella vuota*)
let createEmpty ?size:(n=0) (t : typeSet) = Set(Table.create n, t)
(*Crea il singleton*)
let createSing v t = Set(let hash = Table.create 1 in Table.replace hash v v; hash, t)
(*prende l'argomento dal construttore String e lo converte in int, restiuisce un errore se non e'del tipo "5"*)
let convertToInt_exn s =  try Some(int_of_string s) with Failure _ -> None
(*Crea una lista dalla tabella data*)
let toList = function Set(h, t) -> let f = fun h -> Table.fold (fun k v acc -> k :: acc ) h [] in f h
            | _ -> failwith("run-time error")
(*Restituisce l'argomento del construttore String e Int*)
let correct ex =  (match ex with String(n) -> let yes = convertToInt_exn n in
    (match yes with Some(n) -> n | None -> failwith("Not a number"))
  | Int(n) -> n
  | _ -> failwith("Impossible  to evaluate the expression."))
(*Aggiunge un elemento al Set*)
let addToSet  (s : evT) (n : k) = match s with 
  |  Set(table, t) -> Table.replace table n n ; s
  |  _ -> failwith("run-time error")
(*Creazione di un Set dato un inizio e fine: {inizio+1,...,fine-1}. Sono ammessi i tipi String, Int
ma String dev'essere un numro.*)
let ofS (t : typeSet) (n : int) (from : int) = 
  let setToRet = createEmpty ~size:(2*from) t in if from >= n-1 then setToRet
    else if typeof t = "int" then 
        let rec aux (da : int) (tab : evT) = match da = n with
          | true -> tab
          | _ -> let cons : k = Int(da) in aux (da+1) (addToSet tab cons)
        in aux (from+1) setToRet
        else let rec aux da tab = match da = n with
          | true -> setToRet
          | _ -> let cons : k = String(string_of_int da) in aux (da+1) (addToSet tab cons)
        in aux (from+1) setToRet

(*Lega all'ambiente il risultato valutato con la variabile*)
let bind (s: evT env) (i:string) (x:evT) = ( i, x ) :: s
(*Unisce un elemento al set dato*)
let merge (e: k) (t: evT)  = match t with
          |  Set(h, t) -> Table.replace h e e
          |_ -> failwith("run-time error")
(*Restituisce al chiamante la tabella*)
let returnTable s = match s with Set(h, t) -> h | _ -> failwith("run-time error")
(*Controlla se un elemento e' gia' presente nel set*)
let isThere (e: k) (t: 'a Table.t) : bool = Table.mem t e
(*Controlla nel ambiente la presenza della variabile*)
let rec lookup (s:evT env) (i:string) = match s with
    | [] ->  Unbound
    | (j,v)::sl when j = i -> v
    | _::sl -> lookup sl i

(*Funzioni per lavorare sui Int e String*)
let greater(x,y)= 
match (x, y) with
  | (Int(v), Int(w)) -> Bool(v > w)
  | (String(v), String(w)) -> Bool(v > w)
  | (_,_) -> failwith("Run-time error")

let int_eq(x,y) =   
match (x, y) with
  | (Int(v), Int(w)) -> Bool(v = w)
  | (_,_) -> failwith("run-time error ")
       
 let int_plus(x, y) = 
 match(x, y) with
  | (Int(v), Int(w)) -> Int(v + w)
  | (_,_) -> failwith("run-time error ")

let int_sub(x, y) = 
 match(x, y) with
  | (Int(v), Int(w)) -> Int(v - w)
  | (_,_) -> failwith("run-time error ")

let int_times(x, y) = 
 match(x, y) with
  | (Int(v), Int(w)) -> Int(v * w)
  | (_,_) -> failwith("run-time error ")
  (*Restituisce la grandezza piu' grande, serve per creare una nuova tabella*)
let bigSize s1 s2 = let size1 = (size s1) in let size2 = (size s2) in match size1 < size2 with 
  true -> size2 | false -> size1
(*Restituisce una tulpa, ha come primo argomento la tabella piu grande.*)
let bigger s1 s2 = match (size s1) < (size s2) with true -> (s2, s1) | false -> (s1, s2)

(*Funzione che operano sul il set*)
let union  ((s1: evT),(s2:evT)) : unit  = match s1 with 
      | Set(h, t) -> Table.iter (fun x y -> merge x s2) h
      | _ -> failwith("run-time error")
let inter s1 s2 final = Table.iter (fun x y -> match (isThere x (returnTable s2)) with | true -> Table.replace final x x | false -> ()) (returnTable s1)
let diff (s1, s2) final = Table.iter (fun x y -> match (isThere x (returnTable s2)) with | true -> () | false -> Table.replace final x x) (returnTable s1)
let fromKtoEvt (e : k) : evT = match e with String(n) -> String(n) | Int(n) -> Int(n) | _ -> failwith("Run-time error") 
let minS (t : k Table.t) :  evT = let temp  = ref None in Table.iter (fun x y -> match !temp with
      | Int(v) -> (match v > (returnPrimInt x) with true -> temp := (fromKtoEvt x) | false -> ())
      | String(v) -> (match v > (returnPrimStr x) with true -> temp := (fromKtoEvt x) | false -> ())
      | None -> temp := (fromKtoEvt x)
      | _ -> failwith("Run-time error")) t; !temp
let maxS (t : k Table.t) :  evT = let temp  = ref None in Table.iter (fun x y -> match !temp with
      | Int(v) -> (match v < (returnPrimInt x) with true -> temp := (fromKtoEvt x) | false -> ())
      | String(v) -> (match v < (returnPrimStr x) with true -> temp := (fromKtoEvt x) | false -> ())
      | None -> temp := (fromKtoEvt x)
      | _ -> failwith("Run-time error")) t; !temp


let rec eval  (e:exp) (s:evT env) : evT = (match e with
 | CstInt(n) -> Int(n)
 | CstTrue -> Bool(true)
 | CstFalse -> Bool(false)
 | CstString(n) -> String(n)
 | Greater(e1, e2) -> greater((eval e1 s), (eval e2 s))
 | Eq(e1, e2) -> int_eq((eval e1 s), (eval e2 s))
 | Times(e1,e2) -> int_times((eval e1 s), (eval e2 s))
 | Sum(e1, e2) -> int_plus((eval e1 s), (eval e2 s))
 | Sub(e1, e2) -> int_sub((eval e1 s), (eval e2 s))
 | Ifthenelse(e1,e2,e3) -> let g = eval e1 s in
                (match g with
      | Bool(true) -> eval e2 s
                        | Bool(false) -> eval e3 s
                  |_ -> failwith ("Run-time error"))
| Den(i) -> lookup s i
| Let(i, e, ebody) -> eval ebody (bind s i (eval e s))
| Fun(arg, ebody) -> Closure(arg,ebody,s)
| Letrec(f, arg, fBody, letBody) -> 
  let benv = bind (s) (f) (RecClosure(f, arg, fBody,s)) in
      eval letBody benv
| Apply(Den(f), eArg) -> 
  let fclosure = lookup s f in 
     (match fclosure with 
       | Closure(arg, fbody, fDecEnv) ->
         let aVal = eval eArg s in
   let aenv = bind fDecEnv arg aVal in 
         eval fbody aenv
       | RecClosure(f, arg, fbody, fDecEnv) ->
         let aVal = eval eArg s in
         let rEnv = bind fDecEnv f fclosure in
   let aenv = bind rEnv arg aVal in 
         eval fbody aenv
       | _ -> failwith("non functional value"))
| Apply(_,_) -> failwith("Application: not first order function") 
| Empty(t) -> createEmpty t
| Singleton(v, t) -> (let res = eval v s  in  createSing (toK res)  t) 
| Of(t, n, from) -> let top = correct (eval n s) in let start = correct (eval from s) in ofS t top start
| Union(s1, s2) -> (let final = Set(Table.create (2 * (size s1 + size s2)), returnType s1) in 
        union (s1, final);  union (s2, final); final)
| Inter(s1, s2) -> (let final = (Table.create (2 * (bigSize s1 s2))) in 
        inter s1 s2 final; Set(final, returnType s1))
| Diff(s1, s2) -> (let final = (Table.create (2 * (bigSize s1 s2 ))) in 
        diff (s1 ,s2) final; Set(final, returnType s1))
| Insert(e, set1) -> (let evF = eval e s in  addToSet set1 (toK evF))
| Delete(e, set1) -> (let evF = eval e s in Table.remove (returnTable set1) (toK evF); set1)
| IsEmpty(set1) -> (match Table.length (returnTable set1) == 0 with true -> Bool(true) | false -> Bool(false))
| IsIn(e, set1) -> (let evF = eval e s in Bool(Table.mem (returnTable set1) (toK evF)))
| IsSubset(set1, set2) -> (let x = toList set1 in match List.length x = 0 with 
         | true -> Bool(true) 
         | false -> Bool(not (List.exists (fun y -> match Table.find_opt (returnTable set2) y with
           | Some(n) -> false
           | None -> true) x)))       
| Min(set) -> minS (returnTable set)
| Max(set) ->  maxS (returnTable set)
(*Fa il controllo se un predicato restituisce un bool a run-time*)
(*Si lega il nome "f" alla Closure e si chiama la funzione su ciascuno elemento del Set*)
| For_all(e, set) -> (let evF = eval e s in let newEnv = bind s "f" evF in  
        let l = toList set in Bool((List.for_all (fun x ->  match eval (Apply(Den("f"), (fromKtoExp x))) newEnv with Bool(n) -> n | _-> failwith("The predicate does not return a bool")) l)))
| Exists(e, set) -> let evF = eval e s in (let newEnv = bind s "f" evF in
        let l = toList set in Bool((List.exists (fun x ->  match eval (Apply(Den("f"), (fromKtoExp x))) newEnv with Bool(n) -> n | _-> failwith("The predicate does not return a bool")) l)))
| Filter(e, set) -> (let evF = eval e s in (let newEnv = bind s "f" evF in let newT = Table.create (2 * (size set)) in 
        Table.iter (fun x y -> match eval (Apply(Den("f"), (fromKtoExp x))) newEnv with 
            Bool(n) -> (match n with true -> Table.replace newT x x | false -> ())
          | _-> failwith("The predicate does not return a bool")) (returnTable set); Set(newT, (returnType set))))
| Map(e, set) -> (let evF = eval e s in (let newEnv = bind s "f" evF in let newT = Table.create (2 * (size set)) in
      Table.iter (fun x y -> match eval (Apply(Den("f"), (fromKtoExp x))) newEnv with 
            Int(n) -> (Table.replace newT (toK (Int(n))) (toK (Int(n))))
          | String(n) -> (Table.replace newT (toK (String(n))) (toK (String(n))))
          | _-> failwith("The predicate does not return a value")) (returnTable set); Set(newT, (returnType set))))
| Concat(e1, e2) -> let evE1 = eval e1 s in let evE2 = eval e2 s in 
  match (evE1, evE2) with (String(n), String(f)) -> String(n ^ f)
              | (_, _) -> failwith("Not a String(n) type"))

let toListStr = function Set(h, t) -> let f = fun h -> Table.fold (fun k v acc -> match k with String(n) -> n ::acc | _-> failwith("Something went wrong") ) h [] in f h
            | _ -> failwith("run-time error")


let toListInt = function Set(h, t) -> let f = fun h -> Table.fold (fun k v acc -> match k with Int(n) -> n ::acc | _-> failwith("Something went wrong") ) h [] in f h
            | _ -> failwith("run-time error");;

(*Type checker statico*)
type tval = 
  | TInt
  | TBool
  | TString
  | TSet of tval
  | FunT of tval 
  | Unbound 
type tEnv = (string * tval) list
let emptT = [("", Unbound)]
let bind (s: tEnv) (i:string) (x:tval) = ( i, x ) :: s
let rec lookup (s:tEnv) (i:string) = match s with
    | [] ->  Unbound
    | (j,v)::sl when j = i -> v
    | _::sl -> lookup sl i

let fromTypeToEval (e : typeSet) = (match e with String -> TString | Int -> TInt)
let rec teval (e:exp) (tenv : tEnv) :tval = match e with
  | CstInt(n) -> TInt
  | CstString(n) -> TString
  | CstFalse -> TBool
  | CstTrue -> TBool
  | Den(s) -> lookup tenv s
  | Sub(e1, e2) | Eq (e1, e2) | Times(e1,e2) | Sum(e1, e2) ->
    let t1 = teval e1 tenv in 
    let t2 = teval e2 tenv in 
      (match (t1, t2) with
        | (TInt, TInt) -> TInt
        |(_,_) -> failwith ("WrongType"))
  | Greater(e1, e2) -> 
    let t1 = teval e1 tenv in 
    let t2 = teval e2 tenv in 
      (match (t1, t2) with
        | (TInt, TInt) -> TInt
        | (TString, TString) -> TString
        |(_,_) -> failwith ("WrongType"))
  | Ifthenelse(e1,e2,e3) ->
    let t1 = teval e1 tenv in
      (match t1 with
        TBool -> let t2 = teval e2 tenv in
          let t3 = teval e3 tenv in 
          (match (t2, t3) with
              (TInt, TInt) -> TInt
            | (TString, TString) -> TString
            | (TBool, TBool) -> TBool
            | (TSet(n), TSet(h)) -> (match n = h with true -> TSet(n) | false -> failwith ("WrongType"))
            | (FunT(h), FunT(e)) -> (match h = e with true -> FunT(e) | false -> failwith ("WrongType"))
            | (_,_) -> failwith ("WrongType"))
          | _-> failwith("Not a bool guard"))
  | Let(i, e, ebody) -> teval ebody (bind tenv i (teval e tenv))
  | Letrec(f, arg, fBody, letBody) -> 
    let benv = bind (tenv) (f) (teval fBody tenv) in
      teval letBody benv
  | Fun(i, e) -> 
    let t2 = teval e tenv in FunT(t2)
  | Apply(e1, e2) ->
      let f = teval e1 tenv in
        (match f with
          | FunT(t1) ->
            (if ((teval e2 tenv) = t1) then t1
              else failwith ("WrongType"))
          |_ -> failwith ("WrongType"))
  | Empty(t) -> TSet(fromTypeToEval t)
  | Singleton(v, t) -> let set1 = teval v tenv in
    (match set1 = (fromTypeToEval t) with true -> TSet(set1) | false -> failwith("WrongType"))
  | Of(t, n, from) -> let top = fromTypeToEval t in let nt = teval n tenv 
    in let fr = teval from tenv in (match (top, nt, fr) with 
        (TInt, _, _) -> TSet(top)
      | (TString, _, _) -> TSet(top)
      | (_,_,_) -> failwith("WrongType"))
  | Union(s1, s2) | Diff(s1, s2) | Inter (s1, s2) -> let typ = returnType s1 in (match typ = returnType s2 with 
      true -> TSet(fromTypeToEval typ)
    | false -> failwith("WrongType"))
  | Insert(e, set1) | Delete(e, set1) | IsIn(e, set1) -> let ev = teval e tenv in 
    (match ev = fromTypeToEval (returnType set1) with true -> TSet(ev) | false -> failwith("WrongType"))
  | IsEmpty(set1) -> TSet(fromTypeToEval(returnType set1))
  | IsSubset(set1, set2) -> let t1 = fromTypeToEval (returnType set1) in 
    (match t1 = fromTypeToEval(returnType set2) with true -> TSet(t1) | false -> failwith("WrongType"))
  | Min(set) -> TSet(fromTypeToEval (returnType set))
  | Max(set) -> TSet(fromTypeToEval (returnType set))
  | Concat(e1, e2) -> (match ((teval e1 tenv) = TString) = ((teval e2 tenv = TString)) with
      true -> TString
    | false -> failwith("WrongType"))
  | For_all(e, set) | Exists(e, set) | Filter(e, set) | Map(e, set)  -> match teval e tenv with FunT(h) -> FunT(h) | _-> failwith("Not a function");;
