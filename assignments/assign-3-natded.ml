(*
    Printing related functions
*)

let print s = print_endline s;;

let print_bool b =
    match b with
    | true  -> print "true"
    | false -> print "false"
;;

let print_int i = print (string_of_int i);;

(*
    Proposition related functions
*)

(* Proposition Data Type *)
type prop =
    | P of string
    | T | F
    | Not of prop
    | And of prop * prop
    | Or of prop * prop
    | Implies of prop * prop
;;

(*
    Set related helper functions
*)

let member x s = List.mem x s;;
let rec remove x s = List.filter (fun a -> a <> x) s;;

let rec subset s1 s2 =
    match s1 with
        [] -> true
    | h::t -> member h s2 && subset t s2
;;
let rec equalset s1 s2 = subset s1 s2 && subset s2 s1;;
let rec union s1 s2 =
    match s1 with
        [] -> s2
    | h::t ->
        if member h s2 then
            union t s2
        else
            h :: (union t s2)
;;

(*
    =====================================================
    =====================================================
*)

(*
    Sequent Data Type

    Represents: Gamma |- P
*)
type sequent = prop list * prop;;

(*
    Proof Tree Data Type

    Represents a Natural Deduction based proof tree.
*)
type prooftree =
    (* Assumption *)
    | Ass of sequent
    (* True Introduction *)
    | TI of sequent
    (* False Elimination *)
    | FE of sequent
    (* Implies Introduction *)
    | ImpI of prooftree * sequent
    (* Implies Elimination *)
    | ImpE of prooftree * prooftree * sequent
    (* And Introduction *)
    | AndI of prooftree * prooftree * sequent
    (* And Elimination Left *)
    | AndEleft of prooftree * sequent
    (* And Elimination Right *)
    | AndEright of prooftree * sequent
    (* Or Introduction Left *)
    | OrIleft of prooftree * sequent
    (* Or Introduction Right *)
    | OrIright of prooftree * sequent
    (* Or Elimination *)
    | OrE of prooftree * prooftree * prooftree * sequent
    (* Not Classical *)
    | NotClass of  prooftree * sequent
    (* Not Intutionistic *)
    | NotIntu of prooftree * sequent
;;


(*
    ht : prooftree -> int

    returns the height of a prooftree
    (counting from 0)
*)
let rec ht pft =
    match pft with

    (* Base Cases *)
    | Ass _ | TI _ | FE _       -> 0

    (* Rules involving 1 Proof Tree *)
    | ImpI (pft1, _)

    | AndEleft (pft1, _)
    | AndEright (pft1, _)

    | OrIleft (pft1, _)
    | OrIright (pft1, _)

    | NotClass (pft1, _)
    | NotIntu (pft1, _)         -> 1 + ht pft1

    (* Rules involving 2 Proof Trees *)
    | ImpE (pft1, pft2, _)
    | AndI (pft1, pft2, _)      -> 1 + max (ht pft1) (ht pft2)

    (* Rules involving 3 Proof Trees *)
    | OrE (pft1, pft2, pft3, _) -> 1 + max (max (ht pft1) (ht pft2)) (ht pft3)
;;

(*
    size : prooftree -> int

    returns the number of nodes / rules used in a prooftree
*)
let rec size pft =
    match pft with

    (* Base Cases *)
    | Ass _ | TI _ | FE _       -> 1

    (* Rules involving 1 Proof Tree *)
    | ImpI (pft1, _)

    | AndEleft (pft1, _)
    | AndEright (pft1, _)

    | OrIleft (pft1, _)
    | OrIright (pft1, _)

    | NotClass (pft1, _)
    | NotIntu (pft1, _)         -> 1 + size pft1

    (* Rules involving 2 Proof Trees *)
    | ImpE (pft1, pft2, _)
    | AndI (pft1, pft2, _)      -> 1 + size pft1 + size pft2

    (* Rules involving 3 Proof Trees *)
    | OrE (pft1, pft2, pft3, _) -> 1 + size pft1 + size pft2 + size pft3
;;

(*
    root : prooftree -> sequent

    returns the root sequent of a proof tree
*)
let root pft =
    match pft with

    (* Base cases *)
    | Ass seq | TI seq | FE seq -> seq

    (* Rules involving 1 Proof Tree *)
    | ImpI (_, seq)

    | AndEleft (_, seq)
    | AndEright (_, seq)

    | OrIleft (_, seq)
    | OrIright (_, seq)

    | NotClass (_, seq)
    | NotIntu (_, seq)          -> seq

    (* Rules involving 2 Proof Trees *)
    | ImpE (_, _, seq)
    | AndI (_, _, seq)          -> seq

    (* Rules involving 3 Proof Trees *)
    | OrE (_, _, _, seq)        -> seq
;;

(*
    wfprooftree  : prooftree -> bool

    checks whether a prooftree is indeed a well-formed proof tree
    (by the rules of natural deduction)
*)
let rec wfprooftree pft =
    match pft with

    | Ass (g, p) -> member p g

    | TI (g, p) -> p = T

    | FE (g, p) -> member F g

    | ImpI (pft1, (g, p)) ->

    wfprooftree pft1 &&
    (
        let (g1, p1) = root pft1 in

        (* the main formula is of the form expected by the rule *)
        match p with
        | Implies (q1, q2) ->

            (* the side formulas are consistent with the main formula *)
            q2 = p1

            (* the extra formulas agree as specified in each rule *)
            && equalset (q1::g) g1

        | _ -> false
    )

    | ImpE (pft1, pft2, (g, p)) ->

    wfprooftree pft1 &&
    wfprooftree pft2 &&
    (
        let (g1, p1) = root pft1 in
        let (g2, p2) = root pft2 in

        (* the main formula is of the form expected by the rule *)
        match p1 with
        | Implies (q1, q2) ->

            (* the side formulas are consistent with the main formula *)
            q1 = p2 && q2 = p

            (* the extra formulas agree as specified in each rule *)
            && equalset g g1
            && equalset g1 g2

        | _ -> false

    )

    | AndI (pft1, pft2, (g, p)) ->

    wfprooftree pft1 &&
    wfprooftree pft2 &&
    (
        let (g1, p1) = root pft1 in
        let (g2, p2) = root pft2 in

        (* the main formula is of the form expected by the rule *)
        match p with
        | And (q1, q2) ->

            (* the side formulas are consistent with the main formula *)
            q1 = p1 && q2 = p2

            (* the extra formulas agree as specified in each rule *)
            && equalset g g1
            && equalset g1 g2

        | _ -> false
    )

    | AndEleft (pft1, (g, p)) ->

    wfprooftree pft1 &&
    (
        let (g1, p1) = root pft1 in

        (* the main formula is of the form expected by the rule *)
        match p1 with
        | And (q1, q2) ->

            (* the side formulas are consistent with the main formula *)
            q1 = p

            (* the extra formulas agree as specified in each rule *)
            && equalset g g1

        | _ -> false
    )

    | AndEright (pft1, (g, p)) ->

    wfprooftree pft1 &&
    (
        let (g1, p1) = root pft1 in

        (* the main formula is of the form expected by the rule *)
        match p1 with
        | And (q1, q2) ->

            (* the side formulas are consistent with the main formula *)
            q2 = p

            (* the extra formulas agree as specified in each rule *)
            && equalset g g1

        | _ -> false
    )

    | OrIleft (pft1, (g, p)) ->

    wfprooftree pft1 &&
    (
        let (g1, p1) = root pft1 in

        (* the main formula is of the form expected by the rule *)
        match p with
        | Or (q1, q2) ->

            (* the side formulas are consistent with the main formula *)
            q1 = p1

            (* the extra formulas agree as specified in each rule *)
            && equalset g g1

        | _ -> false
    )

    | OrIright (pft1, (g, p)) ->

    wfprooftree pft1 &&
    (
        let (g1, p1) = root pft1 in

        (* the main formula is of the form expected by the rule *)
        match p with
        | Or (q1, q2) ->

            (* the side formulas are consistent with the main formula *)
            q2 = p1

            (* the extra formulas agree as specified in each rule *)
            && equalset g g1

        | _ -> false
    )

    | OrE (pft1, pft2, pft3, (g, p)) ->

    wfprooftree pft1 &&
    wfprooftree pft2 &&
    wfprooftree pft3 &&
    (
        let (g1, p1) = root pft1 in
        let (g2, p2) = root pft2 in
        let (g3, p3) = root pft3 in

        (* the main formula is of the form expected by the rule *)
        match p1 with
        | Or (q1, q2) ->

            (* the side formulas are consistent with the main formula *)
            p = p2 && p = p3

            (* the extra formulas agree as specified in each rule *)
            && equalset g g1
            && equalset (q1::g) g2
            && equalset (q2::g) g3

        | _ -> false
    )

    | NotIntu (pft1, (g, p)) ->

    wfprooftree pft1 &&
    (
        let (g1, p1) = root pft1 in

        (* the side formulas are consistent with the main formula *)
        p1 = F

        (* the extra formulas agree as specified in each rule *)
        && equalset g g1
    )

    | NotClass (pft1, (g, p)) ->

    wfprooftree pft1 &&
    (
        let (g1, p1) = root pft1 in

        (* the side formulas are consistent with the main formula *)
        p1 = F

        (* the extra formulas agree as specified in each rule *)
        && ( equalset (Not(p)::g) g1 || equalset (Implies(p, F)::g) g1 )
    )
;;

(*
    pad : prooftree -> prop list -> prooftree

    creates a new proof tree with the set of additional assumptions
    added at each node.
*)
let rec pad pft d =
    match pft with

    (* Base Cases *)
    | Ass (g, p) -> Ass (union g d, p)
    | TI (g, p) -> TI (union g d, p)
    | FE (g, p) -> FE (union g d, p)

    (* Rules involving 1 Proof Tree *)
    | ImpI (pft1, (g, p)) -> ImpI (pad pft1 d, (union g d, p))

    | AndEleft (pft1, (g, p)) -> AndEleft (pad pft1 d, (union g d, p))
    | AndEright (pft1, (g, p)) -> AndEright (pad pft1 d, (union g d, p))

    | OrIleft (pft1, (g, p)) -> OrIleft (pad pft1 d, (union g d, p))
    | OrIright (pft1, (g, p)) -> OrIright (pad pft1 d, (union g d, p))

    | NotClass (pft1, (g, p)) -> NotClass (pad pft1 d, (union g d, p))
    | NotIntu (pft1, (g, p)) -> NotIntu (pad pft1 d, (union g d, p))

    (* Rules involving 2 Proof Trees *)
    | ImpE (pft1, pft2, (g, p)) -> ImpE (pad pft1 d, pad pft2 d, (union g d, p))
    | AndI (pft1, pft2, (g, p)) -> AndI (pad pft1 d, pad pft2 d, (union g d, p))

    (* Rules involving 3 Proof Trees *)
    | OrE (pft1, pft2, pft3, (g, p)) -> OrE (pad pft1 d, pad pft2 d, pad pft3 d, (union g d, p))
;;

(*
    pare : prooftree -> prooftree

    returns a well-formed proof tree with minimal assumptions in each sequent
*)

exception NotWellFormed;;

let rec pare pft =
    match pft with

    (* Base Cases *)
    | Ass (g, p) -> Ass ([p], p)
    | TI (g, p)  -> TI  ([ ], p)
    | FE (g, p)  -> FE  ([F], p)

    | ImpI (pft1, (g, p)) ->
    (
        let pared = pare pft1 in
        let (g1, p1) = root pared in

        match p with
        | Implies (q1, q2) -> ImpI (pared, (remove q1 g1, p))

        | _                -> raise NotWellFormed
    )

    | ImpE (pft1, pft2, (g, p)) ->
        ImpE (pft1, pft2, (g, p))

    | AndI (pft1, pft2, (g, p)) ->
        AndI (pft1, pft2, (g, p))
    | AndEleft (pft1, (g, p)) ->
        AndEleft (pft1, (g, p))
    | AndEright (pft1, (g, p)) ->
        AndEright (pft1, (g, p))

    | OrIleft (pft1, (g, p)) ->
        OrIleft (pft1, (g, p))
    | OrIright (pft1, (g, p)) ->
        OrIright (pft1, (g, p))
    | OrE (pft1, pft2, pft3, (g, p)) ->
        OrE (pft1, pft2, pft3, (g, p))

    | NotClass (pft1, (g, p)) ->
        NotClass (pft1, (g, p))
    | NotIntu (pft1, (g, p)) ->
        NotIntu (pft1, (g, p))
;;


(*
    =====================================================
    =====================================================
*)

(*
    Examples of some prooftrees
*)
let g_1 = [P("a"); P("b"); P("c")]

let pft_Ass = Ass(g_1, P("b"));;
let pft_TI = TI(g_1, T);;
let pft_FE = FE(F::g_1, P "z");;

let pft_ImpI = ImpI(
    Ass(g_1, P("b")),
    ( g_1, Implies( P("a"), P("b") ) )
);;

let pft_ImpE = ImpE(
    pft_ImpI,
    Ass(g_1, P("a")),
    ( g_1, P("b") )
);;

let pft_AndI = AndI(
    Ass(g_1, P("a")),
    Ass(g_1, P("b")),
    ( g_1, And( P("a"), P("b") ) )
);;

let pft_AndEleft = AndEleft(
    pft_AndI,
    ( g_1, P("a") )
);;

let pft_AndEright = AndEright(
    pft_AndI,
    ( g_1, P("b") )
);;

let pft_OrIleft = OrIleft(
    Ass(g_1, P("a")),
    (g_1, Or( P("a"), P("b") ) )
);;

let pft_OrIright = OrIright(
    Ass(g_1, P("b")),
    (g_1, Or( P("a"), P("b") ) )
);;

let pft_OrE = OrE(
    pft_OrIleft,
    Ass(g_1, P("c")),
    Ass(g_1, P("c")),
    (g_1, P("c"))
);;

let pft_NotIntu = NotIntu(
    Ass([F], F),
    ([F], P("z"))
);;

let pft_NotClass = NotClass(
    Ass([Not(P("a")); F], F),
    ([Not(P("a")); F], P("a"))
);;

let pft_pad = Ass(
    [P("a"); P("b"); P("c"); P("d")],
    P("b")
);;

print "Is Well Formed?";;
print "";;
print_string "1 - Assumption:\t\t\t";;             print_bool (wfprooftree pft_Ass);;
print_string "2 - True Introduction:\t\t";;        print_bool (wfprooftree pft_TI);;
print_string "3 - False Elimination:\t\t";;        print_bool (wfprooftree pft_FE);;
print "";;
print_string "4 - Implies Introduction:\t";;       print_bool (wfprooftree pft_ImpI);;
print_string "5 - Implies Elimination:\t";;        print_bool (wfprooftree pft_ImpE);;
print "";;
print_string "6 - And Introduction:\t\t";;         print_bool (wfprooftree pft_AndI);;
print_string "7 - And Elimination (left):\t";;     print_bool (wfprooftree pft_AndEleft);;
print_string "8 - And Elimination (right):\t";;    print_bool (wfprooftree pft_AndEright);;
print "";;
print_string "9  - Or Introduction (left):\t";;    print_bool (wfprooftree pft_OrIleft);;
print_string "10 - Or Introduction (right):\t";;   print_bool (wfprooftree pft_OrIright);;
print_string "11 - Or Elimination:\t\t";;          print_bool (wfprooftree pft_OrE);;
print "";;
print_string "12 - Not Classical:\t\t";;           print_bool (wfprooftree pft_NotClass);;
print_string "13 - Not Intutionistic:\t\t";;       print_bool (wfprooftree pft_NotIntu);;
print "\n----\n";;

print "Pad";;
print "";;
print_string "Padded tree is correct?\t\t";;       print_bool (pft_pad = pad pft_Ass [P("d")]);;
