type chr = char
type str = chr list

type transitions = chr -> state list
and state = int * bool * transitions
  (* Each state has a unique identifier *)

let count = ref (-1)
let mk b (f : transitions) : state =
  incr count;
  (!count, b, f)
let compare_s (i, _, _) (i', _, _) = compare i i'

(* Operations on sets *)
let (^@) l l' = l @ l'
let union ls = List.concat ls

let advance (c : chr) ((_, _, f) : state) : state list =
  f c
let advance_par (ss : state list) (c : chr) : state list =
  union @@ List.map (advance c) ss
let walk a str =
   List.fold_left advance_par [a] str

let matching (_, b, _) = b

let matches (a : state) (str : str) : bool =
  match str with
  | [] ->
      matching a
  | _ ->
      List.exists matching @@ walk a str

(* Merge two transition lists starting from one character. *)
let rec merge_ts (c : chr) (ts : transitions) (ts' : transitions)
    : transitions =
  match ts c with
  | [] -> ts'
  | s :: ss ->
    begin
      fun c' ->
        if c = c' then
          blah
        else

    end

let zero : state =
  mk false @@
    fun _ -> []
let one : state =
  mk true @@
    fun _ -> []
let char c : state =
  mk false @@
    function c' when c = c' -> [one] | _ -> []
let plus a b =
  mk (matching a || matching b) @@
    advance_par [a; b]
let (+.+) a b = plus a b
let maybe a = one +.+ a
let rec cat a b =
  mk (matching a && matching b) @@
    if matching a then
      fun c ->
        let a_ts = advance c a in
        let a_ts' = List.map (fun a' -> cat a' b) a_ts in
        advance_par (b :: a_ts') c
    else
      fun c -> List.map (fun a' -> cat a' b) (advance c a)
let ( *.* ) a b = cat a b
let concat l = List.fold_left cat one l

let tests = [
  matches zero [] = false;
  matches zero ['a'] = false;
  matches one [];
  matches one ['a'] = false;
  matches (char 'a') [] = false;
  matches (char 'a') ['a'];
  matches (char 'a') ['a';'a'] = false;
  matches (char 'a') ['a';'b'] = false;
  matches (char 'a' +.+ char 'b') ['a'];
  matches (char 'a' +.+ char 'b') ['b'];
  matches (char 'a' +.+ char 'b') ['a';'b'] = false;
  matches (maybe (char 'a')) [];
  matches (maybe (char 'a')) ['a'];
  matches (maybe (char 'a')) ['a';'a'] = false;
  matches (char 'a' *.* char 'b') ['a';'b'];
  matches (char 'a' *.* char 'b') ['a'] = false;
  matches (char 'a' *.* char 'b') ['a';'b';'c'] = false;
  matches (maybe (char 'a') *.* char 'a') [] = false;
  matches (maybe (char 'a') *.* char 'a') ['a'];
  matches (maybe (char 'a') *.* char 'a') ['a';'a'];
]

let rec repeat n x =
  if n <= 0 then []
  else x :: repeat (pred n) x
let n = 15
let a =
  concat (repeat n (maybe (char 'a'))) *.* concat (repeat n (char 'a'))
let str = repeat n 'a'
