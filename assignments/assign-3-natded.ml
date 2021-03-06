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
let rec difference s1 s2 =
    match s1 with
        [] -> []
    | h::t ->
        if not (member h s2) then
            h :: (difference t s2)
        else
            difference t s2
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
        let pared1 = pare pft1 in
        let (g1, p1) = root pared1 in

        match p with
        | Implies (q1, q2) -> ImpI (pad pared1 [q1], (difference g1 [q1], p))
        | _                -> raise NotWellFormed
    )

    | ImpE (pft1, pft2, (g, p)) ->
    (
        let pared1 = pare pft1 in
        let (g1, p1) = root pared1 in

        let pared2 = pare pft2 in
        let (g2, p2) = root pared2 in

        ImpE (pad pared1 g2, pad pared2 g1, (union g1 g2, p))
    )

    | AndI (pft1, pft2, (g, p)) ->
    (
        let pared1 = pare pft1 in
        let (g1, p1) = root pared1 in

        let pared2 = pare pft2 in
        let (g2, p2) = root pared2 in

        AndI (pad pared1 g2, pad pared2 g1, (union g1 g2, p))
    )

    | AndEleft (pft1, (g, p)) ->
    (
        let pared1 = pare pft1 in
        let (g1, p1) = root pared1 in

        AndEleft (pared1, (g1, p))
    )

    | AndEright (pft1, (g, p)) ->
    (
        let pared1 = pare pft1 in
        let (g1, p1) = root pared1 in

        AndEright (pared1, (g1, p))
    )

    | OrIleft (pft1, (g, p)) ->
    (
        let pared1 = pare pft1 in
        let (g1, p1) = root pared1 in

        OrIleft (pared1, (g1, p))
    )

    | OrIright (pft1, (g, p)) ->
    (
        let pared1 = pare pft1 in
        let (g1, p1) = root pared1 in

        OrIright (pared1, (g1, p))
    )

    | OrE (pft1, pft2, pft3, (g, p)) ->
    (
        let pared1 = pare pft1 in
        let (g1, p1) = root pared1 in

        let pared2 = pare pft2 in
        let (g2, p2) = root pared2 in

        let pared3 = pare pft3 in
        let (g3, p3) = root pared3 in


        match p1 with

        | Or (q1, q2) ->
        (
            let pared_g2 = difference g2 [q1] in
            let pared_g3 = difference g3 [q2] in

            OrE (
                pad pared1 (union pared_g2 pared_g3),
                pad pared2 (union g1 (q1::pared_g3)),
                pad pared3 (union g1 (q2::pared_g2)),

                (union g1 (union pared_g2 pared_g3) , p)
            )
        )

        | _ -> raise NotWellFormed
    )

    | NotClass (pft1, (g, p)) ->
    (
        let pared = pare pft1 in
        let (g1, p1) = root pared in

        (* Note: Not / p -> F ? *)
        NotClass (pad pared [Not(p)], (remove (Not(p)) g1, p))
    )

    | NotIntu (pft1, (g, p)) ->
    (
        let pared = pare pft1 in
        let (g1, p1) = root pared in

        NotIntu (pared, (g1, p))
    )
;;

(*
    find_pft : prooftree list -> prop -> prooftree

    returns a proof tree from the list that can prove the conclusion
*)
let find_pft conclusion pft_list =
    List.find
        (fun pft -> let (_, r) = root pft in r = conclusion)
        pft_list


(*
    graft : prooftree -> prooftree list -> prooftree

    returns a proof tree in which all assumptions have been
    replaced with their proof trees
*)

let rec graft pft pft_list =
    match pft with

    (* Instead of this assumption return a tree that proves it. *)
    (* TODO - What if p is not present in the list? *)
    | Ass (g, p) -> find_pft p pft_list

    | TI (g, p) -> let (g1, _) = root (List.hd pft_list) in TI (g1, p)
    | FE (g, p) -> let (g1, _) = root (List.hd pft_list) in FE (g1, p)

    | AndI (pft1, pft2, (g, p)) ->
    (
        let grafted1 = graft pft1 pft_list in
        let (g1, p1) = root grafted1 in

        let grafted2 = graft pft2 pft_list in

        AndI (grafted1, grafted2, (g1, p))
    )

    | AndEleft (pft1, (g, p)) ->
    (
        let grafted1 = graft pft1 pft_list in
        let (g1, p1) = root grafted1 in

        AndEleft (grafted1, (g1, p))
    )

    | AndEright (pft1, (g, p)) ->
    (
        let grafted1 = graft pft1 pft_list in
        let (g1, p1) = root grafted1 in

        AndEright (grafted1, (g1, p))
    )

    | ImpI (pft1, (g, p)) ->
    (
        let grafted1 = graft pft1 pft_list in
        let (g1, p1) = root grafted1 in

        ImpI (grafted1, (g1, p))
    )

    | ImpE (pft1, pft2, (g, p)) ->
    (
        let grafted1 = graft pft1 pft_list in
        let (g1, p1) = root grafted1 in

        let grafted2 = graft pft2 pft_list in

        ImpE (grafted1, grafted2, (g1, p))
    )

    | OrIleft (pft1, (g, p)) ->
    (
        let grafted1 = graft pft1 pft_list in
        let (g1, p1) = root grafted1 in

        OrIleft (grafted1, (g1, p))
    )

    | OrIright (pft1, (g, p)) ->
    (
        let grafted1 = graft pft1 pft_list in
        let (g1, p1) = root grafted1 in

        OrIright (grafted1, (g1, p))
    )

    | OrE (pft1, pft2, pft3, (g, p)) ->
    (
        let grafted1 = graft pft1 pft_list in
        let (g1, p1) = root grafted1 in

        let grafted2 = graft pft2 pft_list in
        let grafted3 = graft pft2 pft_list in

        OrE (grafted1, grafted2, grafted3, (g1, p))
    )

    | NotClass (pft1, (g, p)) ->
    (
        let grafted1 = graft pft1 pft_list in
        let (g1, p1) = root grafted1 in

        NotClass (grafted1, (g1, p))
    )


    | NotIntu (pft1, (g, p)) ->
    (
        let grafted1 = graft pft1 pft_list in
        let (g1, p1) = root grafted1 in

        NotIntu (grafted1, (g1, p))
    )

(*
    normalise : prooftree -> prooftree = <fun>

    removes all occurrences of r-pairs in a given proof tree
    (i.e., where an introduction rule is followed only by an
    elimination rule of the main connective)
*)

let rec normalise pft =
    match pft with

    | AndEleft(  AndI(pft1, pft2, (g1, p1)), (g, p) ) -> pft1
    | AndEright( AndI(pft1, pft2, (g1, p1)), (g, p) ) -> pft2

    | ImpE( ImpI(pft1, (g1, p1)), pft2, (g, p) ) -> graft pft1 [pft2]

    | OrE ( OrIleft (pft1, (g1, p1)), pft2, pft3, (g, p)) -> graft pft2 [pft1]
    | OrE ( OrIright(pft1, (g1, p1)), pft2, pft3, (g, p)) -> graft pft3 [pft1]

    | _ -> pft

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

(* ################################################################################################### *)

let pft_pad = Ass(
    [P("a"); P("b"); P("c"); P("d")],
    P("b")
);;

print "Is Pad's output correct?";;
print "";;
print_string "Assumption\t\t\t";;                  print_bool (pft_pad = pad pft_Ass [P("d")]);;
print "\n----\n";;

(* ################################################################################################### *)

print "Does Pare maintain Well Formedness?";;
print "";;
print_string "1 - Assumption:\t\t\t";;             print_bool (wfprooftree pft_Ass = wfprooftree (pare pft_Ass));;
print_string "2 - True Introduction:\t\t";;        print_bool (wfprooftree pft_TI = wfprooftree (pare pft_TI));;
print_string "3 - False Elimination:\t\t";;        print_bool (wfprooftree pft_FE = wfprooftree (pare pft_FE));;
print "";;
print_string "4 - Implies Introduction:\t";;       print_bool (wfprooftree pft_ImpI = wfprooftree (pare pft_ImpI));;
print_string "5 - Implies Elimination:\t";;        print_bool (wfprooftree pft_ImpE = wfprooftree (pare pft_ImpE));;
print "";;
print_string "6 - And Introduction:\t\t";;         print_bool (wfprooftree pft_AndI = wfprooftree (pare pft_AndI));;
print_string "7 - And Elimination (left):\t";;     print_bool (wfprooftree pft_AndEleft = wfprooftree (pare pft_AndEleft));;
print_string "8 - And Elimination (right):\t";;    print_bool (wfprooftree pft_AndEright = wfprooftree (pare pft_AndEright));;
print "";;
print_string "9  - Or Introduction (left):\t";;    print_bool (wfprooftree pft_OrIleft = wfprooftree (pare pft_OrIleft));;
print_string "10 - Or Introduction (right):\t";;   print_bool (wfprooftree pft_OrIright = wfprooftree (pare pft_OrIright));;
print_string "11 - Or Elimination:\t\t";;          print_bool (wfprooftree pft_OrE = wfprooftree (pare pft_OrE));;
print "";;
print_string "12 - Not Classical:\t\t";;           print_bool (wfprooftree pft_NotClass = wfprooftree (pare pft_NotClass));;
print_string "13 - Not Intutionistic:\t\t";;       print_bool (wfprooftree pft_NotIntu = wfprooftree (pare pft_NotIntu));;
print "\n----\n";;

(* ################################################################################################### *)

let g_1 = [P "a"; P "b"];;
let a_and_b = And (P "a", P "b");;

let pft_graft_1 = AndI (
    Ass (g_1, P "a"),
    Ass (g_1, P "b"),
    (g_1, a_and_b)
);;

let pft_graft_2 = OrIleft (
    Ass (g_1, P "b"),
    (g_1, Or (P "b", P "c"))
) ;;

let pft_graft_Ass = Ass ([a_and_b], a_and_b);;

let pft_graft_AndEleft = AndEleft (
    pft_graft_Ass,
    ([a_and_b], P "a")
);;

let pft_graft_AndEleft_ans = AndEleft (
    pft_graft_1,
    (g_1, P "a")
)
let pft_list = [pft_graft_1; pft_graft_2];;

print "Is graft's output correct?";;
print_string "1 - Assumption:\t\t\t";;             print_bool (graft pft_graft_Ass pft_list = pft_graft_1);;
print_string "2 - And Introduction:\t\t";;         print_bool (graft pft_graft_AndEleft pft_list = pft_graft_AndEleft_ans);;
print "\n----\n";;

(* ################################################################################################### *)
(* Test Cases given by the TA *)

let gamma = [P "p2";P "p1";P "p3"];;
let gamma2 = [And (P "p",P "q");Or (P "p2",P "q2")];;

let test_tree1 = Ass (gamma ,P "p1") ;;
let test_tree2 = Ass (gamma ,P "p2") ;;
let test_tree3 = AndI (test_tree1,test_tree2,(gamma,(And (P "p1",P
"p2"))));;

let test_tree4 = AndEleft (test_tree3,(gamma,P "p2"));;

let test_fE    = FE (F::gamma,P "p1");;

let gamma2 = [And (P "p",P "q");Or (P "p2",P "q2")];;
let test_tree5 = Ass (gamma2,And (P "p",P "q")) ;;
let test_tree6 = ImpI (test_tree5,([Or (P "p2",P "q2")],Implies (And (P "p",P "q"),And (P "p",P "q"))));;

let gamma3 = [P "a"; P "b"];;
let tq1 = Ass (gamma3,P "a");;
let tq2 = Ass (gamma3,P "b");;
let q1 = AndI (tq1, tq2, (gamma3,(And (P "a",P "b"))));;
let q2 = OrIleft (tq2, (gamma3,(Or(P "b",P "c"))));;

let delta1 = [And (P "a",P "b")];;
let tp1 = Ass (delta1,(And (P "a",P "b")));;
let p = AndEleft (tp1,(delta1, (P "a")));;

let gamma3 = [P "a"; P "b"];;
let tq1 = Ass (gamma3,P "a");;
let tq2 = Ass (gamma3,P "b");;
let q1 = AndI (tq1, tq2, (gamma3,(And (P "a",P "b"))));;
let tree_nor = AndEleft (q1,(gamma,P "a"));;

let pad_test_tree4 = AndEleft(AndI (Ass ([P "p2"; P "p1"; P "p3"; P "p4"], P "p1"),
                               Ass ([P "p2"; P "p1"; P "p3"; P "p4"], P "p2"),
                               ([P "p2"; P "p1"; P "p3"; P "p4"], And (P "p1", P "p2"))),
                             ([P "p2"; P "p1"; P "p3"; P "p4"], P "p2"));;

let pare_test_tree6 = ImpI (Ass ([And (P "p", P "q")], And (P "p", P "q")),
                            ([], Implies (And (P "p", P "q"), And (P "p", P "q"))));;

let graft_p_q1_q2 = AndEleft (AndI (Ass ([P "a"; P "b"], P "a"),Ass ([P "a"; P "b"], P "b"),
                                    ([P "a"; P "b"], And (P "a", P "b"))),([P "a"; P "b"], P "a"));;

let normalise_tree_nor = Ass ([P "a"; P "b"], P "a");;

print "Test cases match TA's";;
print "";;
print_string "wfprooftree test_tree4\t\t";;        print_bool (wfprooftree test_tree4 = false);;
print_string "wfprooftree test_fE\t\t";;           print_bool (wfprooftree test_fE = true);;
print_string "pad test_tree4\t\t\t";;              print_bool (pad test_tree4 [P "p3";P "p4"] = pad_test_tree4);;
print_string "pare test_tree6\t\t\t";;             print_bool (pare test_tree6 = pare_test_tree6);;
print_string "graft p [q1;q2]\t\t\t";;             print_bool (graft p [q1;q2] = graft p [q1;q2]);;
print_string "normalise tree_nor\t\t";;            print_bool (normalise tree_nor = normalise_tree_nor);;
print "\n----\n";;
