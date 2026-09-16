//The following sources were used as a reference: 'Faster, Simpler Red-Black Trees' and Data/Set/RBTree.hs.
namespace RBSet

open Result

type RBSetError = EmptyNodeWasNotExpected

type Color =
    | Red
    | Black

type Tree<'T> =
    | Empty
    | Node of color: Color * left: Tree<'T> * value: 'T * right: Tree<'T>

module private Tree =
    type private Condition<'T> =
        | Done of 'T
        | ToDo of 'T

    let private blacken tree =
        match tree with
        | Node(Red, a, x, b) -> Done(Node(Black, a, x, b))
        | _ -> ToDo tree

    let private justTree resultTree =
        match resultTree with
        | Done t
        | ToDo t -> t

    let rec private blackHeight tree =
        match tree with
        | Empty -> 0
        | Node(Red, l, _, _) -> blackHeight l
        | Node(Black, l, _, _) -> 1 + (blackHeight l)


    let rec contains tree v =
        match tree with
        | Empty -> false
        | Node(_, left, value, right) ->
            if value > v then contains left v
            elif value < v then contains right v
            else true

    let private balance tree =
        match tree with
        | Node(Black, Node(Red, Node(Red, a, x, b), y, c), z, d)
        | Node(Black, Node(Red, a, x, Node(Red, b, y, c)), z, d)
        | Node(Black, a, x, Node(Red, Node(Red, b, y, c), z, d))
        | Node(Black, a, x, Node(Red, b, y, Node(Red, c, z, d))) ->
            ToDo(Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d)))
        | Node(Black, a, x, b) as n -> Done(n)
        | _ -> ToDo(tree)

    let insert tree v =

        let rec insertRec tree v =
            match tree with
            | Empty -> ToDo(Node(Red, Empty, v, Empty))
            | Node(color, left, value, right) ->
                if value > v then
                    let newLeft = insertRec left v

                    match newLeft with
                    | Done nl -> Done(Node(color, nl, value, right))
                    | ToDo nl -> balance (Node(color, nl, value, right))
                elif value < v then
                    let newRight = insertRec right v

                    match newRight with
                    | Done nr -> Done(Node(color, left, value, nr))
                    | ToDo nr -> balance (Node(color, left, value, nr))
                else
                    Done(tree)

        let newTree = insertRec tree v
        newTree |> justTree |> blacken |> justTree |> Ok

    let delete tree v =

        let balanceDel tree =
            match tree with
            | Node(color, Node(Red, Node(Red, a, x, b), y, c), z, d)
            | Node(color, Node(Red, a, x, Node(Red, b, y, c)), z, d)
            | Node(color, a, x, Node(Red, Node(Red, b, y, c), z, d))
            | Node(color, a, x, Node(Red, b, y, Node(Red, c, z, d))) ->
                Done(Node(color, Node(Black, a, x, b), y, Node(Black, c, z, d)))
            | _ -> blacken tree

        let rec eqL tree =
            resultM {
                match tree with
                | Node(color, a, x, Node(Black, b, y, c)) -> return balanceDel (Node(color, a, x, Node(Red, b, y, c)))
                | Node(color, a, x, Node(Red, b, y, c)) ->
                    let! newLeft = eqL (Node(Red, a, x, b))

                    match newLeft with
                    | Done nl -> return Done(Node(Black, nl, y, c))
                    | ToDo nl -> return ToDo(Node(Black, nl, y, c))
                | _ -> return! Error EmptyNodeWasNotExpected
            }

        let rec eqR tree =
            resultM {
                match tree with
                | Node(color, Node(Black, a, x, b), y, c) -> return balanceDel (Node(color, Node(Red, a, x, b), y, c))
                | Node(color, Node(Red, a, x, b), y, c) ->
                    let! newRight = eqR (Node(Red, b, y, c))

                    match newRight with
                    | Done nr -> return Done(Node(Black, a, x, nr))
                    | ToDo nr -> return ToDo(Node(Black, a, x, nr))
                | _ -> return! Error EmptyNodeWasNotExpected
            }

        let delCur tree =
            resultM {
                let rec delMin tree =
                    resultM {
                        match tree with
                        | Node(Red, Empty, x, b) -> return Done b, x
                        | Node(Black, Empty, x, b) -> return blacken b, x
                        | Node(color, a, x, b) ->
                            let! an, min = delMin a

                            match an with
                            | Done t -> return Done(Node(color, t, x, b)), min
                            | ToDo t ->
                                let! t' = eqL (Node(color, t, x, b))
                                return t', min
                        | _ -> return! Error EmptyNodeWasNotExpected
                    }

                match tree with
                | Node(Red, a, y, Empty) -> return Done a
                | Node(Black, a, x, Empty) -> return blacken a
                | Node(color, a, x, b) ->
                    let! bn, min = delMin b

                    match bn with
                    | Done t -> return Done(Node(color, a, min, t))
                    | ToDo t -> return! eqR (Node(color, a, min, t))
                | _ -> return! Error EmptyNodeWasNotExpected
            }

        let rec deleteRec tree v =
            resultM {
                match tree with
                | Empty -> return Done(Empty)
                | Node(color, left, value, right) ->
                    if value > v then
                        let! newLeft = deleteRec left v

                        match newLeft with
                        | Done nl -> return Done(Node(color, nl, value, right))
                        | ToDo nl -> return! eqL (Node(color, nl, value, right))
                    elif value < v then
                        let! newRight = deleteRec right v

                        match newRight with
                        | Done nr -> return Done(Node(color, left, value, nr))
                        | ToDo nr -> return! eqR (Node(color, left, value, nr))
                    else
                        return! delCur tree
            }

        resultM {
            let! t = deleteRec tree v
            return t |> justTree |> blacken |> justTree
        }

    let join t1 g t2 =

        let rec joinLT t1 g t2 targetHeight currentHeight =
            resultM {
                if targetHeight = currentHeight then
                    return Node(Red, t1, g, t2)
                else
                    match t2 with
                    | Node(Red, l, x, r) ->
                        let! newLeft = joinLT t1 g l targetHeight currentHeight
                        return Node(Red, newLeft, x, r) |> balance |> justTree
                    | Node(Black, l, x, r) ->
                        let! newLeft = joinLT t1 g l targetHeight (currentHeight - 1)
                        return Node(Black, newLeft, x, r) |> balance |> justTree
                    | _ -> return! Error EmptyNodeWasNotExpected
            }

        let rec joinRT t1 g t2 targetHeight currentHeight =
            resultM {
                if targetHeight = currentHeight then
                    return Node(Red, t1, g, t2)
                else
                    match t1 with
                    | Node(Red, l, x, r) ->
                        let! newRight = joinRT t2 g r targetHeight currentHeight
                        return Node(Red, l, x, newRight) |> balance |> justTree
                    | Node(Black, l, x, r) ->
                        let! newRight = joinRT t2 g r targetHeight (currentHeight - 1)
                        return Node(Black, l, x, newRight) |> balance |> justTree
                    | _ -> return! Error EmptyNodeWasNotExpected
            }

        let h1 = blackHeight t1
        let h2 = blackHeight t2

        resultM {
            if h1 = 0 then
                return! insert t2 g
            elif h2 = 0 then
                return! insert t1 g
            elif h1 < h2 then
                let! t = joinLT t1 g t2 h1 h2
                return t |> blacken |> justTree
            else if h1 > h2 then
                let! t = joinRT t1 g t2 h2 h1
                return t |> blacken |> justTree
            else
                return Node(Black, t1, g, t2)
        }

    let merge t1 t2 =

        let rec minimum tree =
            match tree with
            | Ok(Node(_, Empty, x, _)) -> Ok x
            | Ok(Node(_, l, _, _)) -> minimum (Ok l)
            | _ -> Error EmptyNodeWasNotExpected

        let mergeEQ t1 t2 =
            resultM {
                let! m = minimum (Ok t2)
                let! t2' = delete t2 m
                let h2' = blackHeight t2'
                let h1 = blackHeight t1

                if h1 = h2' then
                    return Node(Red, t1, m, t2')
                else
                    match t1 with
                    | Node(_, Node(Red, ll, lx, lr), x, r) ->
                        return Node(Red, Node(Black, ll, lx, lr), x, Node(Black, r, m, t2'))
                    | Node(_, l, x, Node(Red, rl, rx, rr)) ->
                        return Node(Black, Node(Red, l, x, rl), rx, Node(Red, rr, m, t2'))
                    | _ -> return Node(Black, (justTree (blacken t1)), m, t2')
            }

        let rec mergeLT t1 t2 targetHeight currentHeight =
            resultM {
                if targetHeight = currentHeight then
                    return! mergeEQ t1 t2
                else
                    match t2 with
                    | Node(Red, l, x, r) ->
                        let! newLeft = mergeLT t1 l targetHeight currentHeight
                        return Node(Red, newLeft, x, r) |> balance |> justTree
                    | Node(Black, l, x, r) ->
                        let! newLeft = mergeLT t1 l targetHeight (currentHeight - 1)
                        return Node(Red, newLeft, x, r) |> balance |> justTree
                    | _ -> return! Error EmptyNodeWasNotExpected
            }

        let rec mergeRT t1 t2 targetHeight currentHeight =
            resultM {
                if targetHeight = currentHeight then
                    return! mergeEQ t1 t2
                else
                    match t1 with
                    | Node(Red, l, x, r) ->
                        let! newRight = mergeRT r t2 targetHeight currentHeight
                        return Node(Red, l, x, newRight) |> balance |> justTree
                    | Node(Black, l, x, r) ->
                        let! newRight = mergeRT r t2 targetHeight (currentHeight - 1)
                        return Node(Red, l, x, newRight) |> balance |> justTree
                    | _ -> return! Error EmptyNodeWasNotExpected
            }

        let h1 = blackHeight t1
        let h2 = blackHeight t2

        resultM {
            if h1 = 0 then
                return t2
            else if h2 = 0 then
                return t1
            else if h1 < h2 then
                let! t = mergeLT t1 t2 h1 h2
                return t |> blacken |> justTree
            else if h1 > h2 then
                let! t = mergeRT t1 t2 h2 h1
                return t |> blacken |> justTree
            else
                let! t = mergeEQ t1 t2
                return t |> blacken |> justTree
        }

    let rec split kx tree =
        resultM {
            match tree with
            | Empty -> return Empty, Empty
            | Node(_, l, x, r) ->
                if kx < x then
                    let! lt, gt = split kx l
                    let! t = join gt x (justTree (blacken r))
                    return lt, t
                else if kx > x then
                    let! lt, gt = split kx r
                    let! t = join (justTree (blacken l)) x lt
                    return t, gt
                else
                    return justTree (blacken (l)), justTree (blacken (r))
        }

module RBSet =
    open Tree

    let empty = Empty

    let add value set = Tree.insert set value

    let delete value set = Tree.delete set value

    let contains value set = Tree.contains set value

    let rec union set1 set2 =
        resultM {
            match set1 with
            | Empty -> return set2
            | _ ->
                match set2 with
                | Empty -> return set1
                | Node(_, l, x, r) ->
                    let! l', r' = Tree.split x set1
                    let! tl = union l' l
                    let! tr = union r' r
                    return! Tree.join tl x tr
        }

    let rec intersection set1 set2 =
        resultM {
            match set1 with
            | Empty -> return Empty
            | _ ->
                match set2 with
                | Empty -> return Empty
                | Node(_, l, x, r) ->
                    let! l', r' = Tree.split x set1
                    let! tl = intersection l' l
                    let! tr = intersection r' r

                    if Tree.contains set1 x then
                        return! Tree.join tl x tr
                    else
                        return! Tree.merge tl tr
        }

    let rec difference set1 set2 =
        resultM {
            match set1 with
            | Empty -> return Empty
            | _ ->
                match set2 with
                | Empty -> return set1
                | Node(_, l, x, r) ->
                    let! l', r' = Tree.split x set1
                    let! tl = difference l' l
                    let! tr = difference r' r
                    return! Tree.merge tl tr
        }
