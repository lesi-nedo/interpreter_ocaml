(*Il codice e' commentato nel file withStaticTypeCheck.ml. I due file variano soltanto
in che modo controllano i tipi, le funzioni e i passaggi per valutare un espressione sono identici*)
(* The language *)
type k = Int of int | String of string | None
module Table = Hashtbl.Make(struct
	type t = k
  let eva ex = match ex with
  	| String(n) -> Hashtbl.hash n
  	| Int(n) -> Hashtbl.hash n
  	| _ -> failwith("run-time error")
  let equal : 'a -> 'a -> bool = fun x y -> x = y
  let hash x = eva x 
end)
type ide = string 
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
let toK (x : evT) : k = match x with
	| String(n) -> String(n)
	| Int(n) ->  Int(n)
	|_ -> failwith("run-time error")

let emptyEnv  = [ ("", Unbound)] 

let fromKtoExp (e : k) : exp = match e with 
	| String(n) -> CstString(n)
	| Int(n) ->  CstInt(n)
	|_ -> failwith("run-time error")

let returnTypeE (e: evT) : typeSet  = match e with 
		| Int(n) -> Int
		| String(n) -> String
		| _-> failwith("Run-time error")

let size s = match s with 
				   | Set(h, t) -> Table.length h
				   | _ -> failwith("Run-time error")
let returnPrimStr (e : k) = match e with
		String(n) -> n
	  | _ -> failwith("run-time error") 
let returnPrimInt (e : k) = match e with
		Int(n) -> n
	  | _ -> failwith("run-time error") 
let returnType set = match set with Set(h, t) -> t
								| _ -> failwith("run-time error")
let createEmpty ?size:(n=0) (t : typeSet) = Set(Table.create n, t)
let createSing v t = Set(let hash = Table.create 1 in Table.replace hash v v; hash, t)
let convertToInt_exn s =  try Some(int_of_string s) with Failure _ -> None
let toList = function Set(h, t) -> let f = fun h -> Table.fold (fun k v acc -> k :: acc ) h [] in f h
					  | _ -> failwith("run-time error")
let correct ex =  (match ex with String(n) -> let yes = convertToInt_exn n in
    (match yes with Some(n) -> n | None -> failwith("Not a number"))
  | Int(n) -> n
  | _ -> failwith("Impossible  to evaluate the expression."))
let addToSet  (s : evT) (n : k) = match s with 
	|  Set(table, t) -> Table.replace table n n ; s
	|  _ -> failwith("run-time error")
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


let bind (s: evT env) (i:string) (x:evT) = ( i, x ) :: s
let merge (e: k) (t: evT)  = match t with
					|  Set(h, t) -> Table.replace h e e
					|_ -> failwith("run-time error")
let returnTable s = match s with Set(h, t) -> h | _ -> failwith("run-time error")
let isThere (e: k) (t: 'a Table.t) : bool = Table.mem t e

let rec lookup (s:evT env) (i:string) = match s with
    | [] ->  Unbound
    | (j,v)::sl when j = i -> v
    | _::sl -> lookup sl i

let typecheck (x, y) = match x with	
         | "int" -> 
             (match y with 
                         | Int(u) -> true
                         | _ -> false)

        | "bool" -> 
              (match y with 
                          | Bool(u) -> true
                          | _ -> false)
        | "string" -> 
              (match y with
                          | String(u) -> true
                          | _ -> false)
        | "set" -> 
              (match y with
                          | Set(u, s) -> true
                          | _-> false)

        | _ -> failwith ("not a valid type")


let greater(x,y)= 
match (typecheck("int", x) || typecheck("string", x), typecheck("int", y) || typecheck("string", y), x, y) with
  | (true, true, Int(v), Int(w)) -> Bool(v > w)
  | (true, true, String(v), String(w)) -> Bool(v > w)
  | (_,_,_,_) -> failwith("Run-time error")

let int_eq(x,y) =   
match (typecheck("int",x), typecheck("int",y), x, y) with
  | (true, true, Int(v), Int(w)) -> Bool(v = w)
  | (_,_,_,_) -> failwith("run-time error ")
       
 let int_plus(x, y) = 
 match(typecheck("int",x), typecheck("int",y), x, y) with
  | (true, true, Int(v), Int(w)) -> Int(v + w)
  | (_,_,_,_) -> failwith("run-time error ")

let int_sub(x, y) = 
 match(typecheck("int",x), typecheck("int",y), x, y) with
  | (true, true, Int(v), Int(w)) -> Int(v - w)
  | (_,_,_,_) -> failwith("run-time error ")

let int_times(x, y) = 
 match(typecheck("int",x), typecheck("int",y), x, y) with
  | (true, true, Int(v), Int(w)) -> Int(v * w)
  | (_,_,_,_) -> failwith("run-time error ")
let bigSize s1 s2 = let size1 = (size s1) in let size2 = (size s2) in match size1 < size2 with 
	true -> size2 | false -> size1
let bigger s1 s2 = match (size s1) < (size s2) with true -> (s2, s1) | false -> (s1, s2)

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
                (match (typecheck("bool", g), g) with
			| (true, Bool(true)) -> eval e2 s
                        | (true, Bool(false)) -> eval e3 s
                	| (_, _) -> failwith ("nonboolean guard"))
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
| Singleton(v, t) -> (let res = eval v s in let yes = typecheck (typeof t, res) || typecheck (typeof t, res) in 
		match yes with 
		  | true -> createSing (toK res)  t 
		  | _ -> failwith("not a valid type")) 
| Of(t, n, from) -> let top = correct (eval n s) in let start = correct (eval from s) in ofS t top start
| Union(s1, s2) -> (match returnType s1 = returnType s2 with true -> let final = Set(Table.create (2 * (size s1 + size s2)), returnType s1) in 
				union (s1, final);  union (s2, final); final
		  	  
		  | false -> failwith("Type mismatch"))
| Inter(s1, s2) -> (match returnType s1 = returnType s2 with true ->  let final = (Table.create (2 * (bigSize s1 s2))) in 
				inter s1 s2 final; Set(final, returnType s1)
				   | false -> failwith("Different sets types"))
| Diff(s1, s2) -> (match returnType s1 = returnType s2 with true ->  let final = (Table.create (2 * (bigSize s1 s2 ))) in 
				diff (s1 ,s2) final; Set(final, returnType s1)
				   | false -> failwith("Different sets types"))
| Insert(e, set1) -> (let evF = eval e s in  match returnTypeE evF = returnType set1 with true -> addToSet set1 (toK evF) | false -> failwith("Type mismatch"))
| Delete(e, set1) -> (let evF = eval e s in  match returnTypeE evF = returnType set1 with true -> Table.remove (returnTable set1) (toK evF); set1 | false -> failwith("Type mismatch"))
| IsEmpty(set1) -> (match Table.length (returnTable set1) == 0 with true -> Bool(true) | false -> Bool(false))
| IsIn(e, set1) -> (let evF = eval e s in match returnTypeE evF = returnType set1 with true -> Bool(Table.mem (returnTable set1) (toK evF)) | false -> failwith("Type mismatch"))
| IsSubset(set1, set2) -> (match returnType set1 = returnType set2 with 
		  | true -> (let x = toList set1 in match List.length x = 0 with 
		  	 | true -> Bool(true) 
		  	 | false -> Bool(not (List.exists (fun y -> match Table.find_opt (returnTable set2) y with
		  	   | Some(n) -> false
		  	   | None -> true) x)))
  		  | false -> failwith("Type mismatch"))
| Min(set) -> minS (returnTable set)
| Max(set) ->  maxS (returnTable set)
| For_all(e, set) -> (let evF = eval e s in match evF with 
		  Closure(arg, body, env) -> let newEnv = bind s "f" evF in  
		  	let l = toList set in Bool((List.for_all (fun x ->  match eval (Apply(Den("f"), (fromKtoExp x))) newEnv with Bool(n) -> n | _-> failwith("The predicate does not return a bool")) l))
		| _ -> failwith("Not a predicate"))
| Exists(e, set) -> (let evF = eval e s in match evF with 
		  Closure(arg, body, env) -> let newEnv = bind s "f" evF in
		  	let l = toList set in Bool((List.exists (fun x ->  match eval (Apply(Den("f"), (fromKtoExp x))) newEnv with Bool(n) -> n | _-> failwith("The predicate does not return a bool")) l))
		| _ -> failwith("Not a predicate"))
| Filter(e, set) -> (let evF = eval e s in match evF with 
		  Closure(arg, body, env) -> (let newEnv = bind s "f" evF in let newT = Table.create (2 * (size set)) in 
		  	Table.iter (fun x y -> match eval (Apply(Den("f"), (fromKtoExp x))) newEnv with 
		        Bool(n) -> (match n with true -> Table.replace newT x x | false -> ())
		      | _-> failwith("The predicate does not return a bool")) (returnTable set); Set(newT, (returnType set)))
		| _ -> failwith("Not a predicate"))
| Map(e, set) -> (let evF = eval e s in match evF with 
		  Closure(arg, body, env) -> (let newEnv = bind s "f" evF in let newT = Table.create (2 * (size set)) in
		  Table.iter (fun x y -> match eval (Apply(Den("f"), (fromKtoExp x))) newEnv with 
		        Int(n) -> (Table.replace newT (toK (Int(n))) (toK (Int(n))))
		      | String(n) -> (Table.replace newT (toK (String(n))) (toK (String(n))))
		      | _-> failwith("The predicate does not return a value")) (returnTable set); Set(newT, (returnType set)))
		| _ -> failwith("Not a predicate"))
| Concat(e1, e2) -> let evE1 = eval e1 s in let evE2 = eval e2 s in 
	match (evE1, evE2) with (String(n), String(f)) -> String(n ^ f)
						  | (_, _) -> failwith("Now a String(n) type"))

let toListStr = function Set(h, t) -> let f = fun h -> Table.fold (fun k v acc -> match k with String(n) -> n ::acc | _-> failwith("Something went wrong") ) h [] in f h
					  | _ -> failwith("run-time error")


let toListInt = function Set(h, t) -> let f = fun h -> Table.fold (fun k v acc -> match k with Int(n) -> n ::acc | _-> failwith("Something went wrong") ) h [] in f h
					  | _ -> failwith("run-time error");;
