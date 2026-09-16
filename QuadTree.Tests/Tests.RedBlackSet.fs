module RedBlackSet.Tests

open System
open QuadTree.RBSet.RBSet
open QuadTree.RBSet
open Xunit

let rec blHeightInv tree =
    match tree with
    | Empty -> 0
    | Node(color, l, _, r) ->
        let lH = blHeightInv l
        let rH = blHeightInv r

        if lH = -1 || rH = -1 || lH <> rH then -1
        else if color = Red then lH
        else lH + 1

let rec heightInv tree =
    match tree with
    | Empty -> 0
    | Node(_, l, _, r) ->
        let lH = heightInv l
        let rH = heightInv r

        if lH = -1 || rH = -1 || (float (rH + 1) / float (lH + 1) > 2) then
            -1
        else if lH > rH then
            lH
        else
            rH

let rec blackSonsOfRed tree =
    match tree with
    | Empty -> true
    | Node(Red, Node(Red, _, _, _), _, _)
    | Node(Red, _, _, Node(Red, _, _, _)) -> false
    | Node(_, l, _, r) -> blackSonsOfRed l && blackSonsOfRed r

let rec numOfElements tree num =
    match tree with
    | Empty -> 0
    | Node(_, l, _, r) ->
        let lN = numOfElements l num
        let rN = numOfElements r num
        lN + rN + 1

[<Fact>]
let oneElement () =
    let finalTree = empty |> add 4 |> Result.bind (add 4)

    match finalTree with
    | Ok t ->
        Assert.True(contains 4 t)
        Assert.Equal(1, blHeightInv t)
        Assert.NotEqual(-1, heightInv t)
        Assert.True(blackSonsOfRed t)
        Assert.Equal(1, numOfElements t 0)
    | Error e -> Assert.True(false, sprintf "Expect Ok, but get Error: %A" e)

[<Fact>]
let insertSomeElem () =
    let finalTree =
        empty
        |> add 5
        |> Result.bind (add 9)
        |> Result.bind (add -7)
        |> Result.bind (add 89)
        |> Result.bind (add -27)
        |> Result.bind (add 13)

    match finalTree with
    | Ok t ->
        Assert.True(contains -7 t)
        Assert.Equal(2, blHeightInv t)
        Assert.NotEqual(-1, heightInv t)
        Assert.True(blackSonsOfRed t)
        Assert.Equal(6, numOfElements t 0)
    | Error e -> Assert.True(false, sprintf "Expect Ok, but get Error: %A" e)

[<Fact>]
let deleteSomeElem () =
    let finalTree =
        empty
        |> add 5
        |> Result.bind (add 9)
        |> Result.bind (add -7)
        |> Result.bind (add 89)
        |> Result.bind (add -27)
        |> Result.bind (add 13)
        |> Result.bind (delete 99)
        |> Result.bind (delete 13)

    match finalTree with
    | Ok t ->
        Assert.False(contains 13 t)
        Assert.Equal(2, blHeightInv t)
        Assert.NotEqual(-1, heightInv t)
        Assert.True(blackSonsOfRed t)
        Assert.Equal(5, numOfElements t 0)
    | Error e -> Assert.True(false, sprintf "Expect Ok, but get Error: %A" e)

[<Fact>]
let unionSets () =
    let finalTree1 =
        empty
        |> add 5
        |> Result.bind (add 9)
        |> Result.bind (add -7)
        |> Result.bind (add 89)
        |> Result.bind (add -27)
        |> Result.bind (add 13)

    let finalTree2 =
        empty
        |> add 2
        |> Result.bind (add 7)
        |> Result.bind (add 21)
        |> Result.bind (add 9)
        |> Result.bind (add 5)

    match finalTree1, finalTree2 with
    | Ok t1, Ok t2 ->
        match union t1 t2 with
        | Ok tU ->
            Assert.NotEqual(-1, heightInv tU)
            Assert.True(blackSonsOfRed tU)
            Assert.Equal(9, numOfElements tU 0)
        | Error e -> Assert.True(false, sprintf "Error in union: %A" e)
    | _ -> Assert.True(false, sprintf "Error in insert")

[<Fact>]
let intersectionSets () =
    let finalTree1 =
        empty
        |> add 5
        |> Result.bind (add 9)
        |> Result.bind (add -7)
        |> Result.bind (add 89)
        |> Result.bind (add -27)
        |> Result.bind (add 13)

    let finalTree2 =
        empty
        |> add 2
        |> Result.bind (add 7)
        |> Result.bind (add 21)
        |> Result.bind (add 9)
        |> Result.bind (add 5)

    match finalTree1, finalTree2 with
    | Ok t1, Ok t2 ->
        match intersection t1 t2 with
        | Ok tI ->
            Assert.NotEqual(-1, heightInv tI)
            Assert.True(blackSonsOfRed tI)
            Assert.Equal(2, numOfElements tI 0)
        | Error e -> Assert.True(false, sprintf "Error in intersection: %A" e)
    | _ -> Assert.True(false, sprintf "Error in insert")

[<Fact>]
let differenceSets () =
    let finalTree1 =
        empty
        |> add 5
        |> Result.bind (add 9)
        |> Result.bind (add -7)
        |> Result.bind (add 89)
        |> Result.bind (add -27)
        |> Result.bind (add 13)

    let finalTree2 =
        empty
        |> add 2
        |> Result.bind (add 7)
        |> Result.bind (add 21)
        |> Result.bind (add 9)
        |> Result.bind (add 5)

    match finalTree1, finalTree2 with
    | Ok t1, Ok t2 ->
        match difference t1 t2 with
        | Ok tD ->
            Assert.NotEqual(-1, heightInv tD)
            Assert.True(blackSonsOfRed tD)
            Assert.Equal(4, numOfElements tD 0)
        | Error e -> Assert.True(false, sprintf "Error in difference: %A" e)
    | _ -> Assert.True(false, sprintf "Error in insert")

[<Fact>]
let emptySetProperties () =
    let t = empty
    Assert.False(contains 0 t)
    Assert.Equal(0, numOfElements t 0)
    Assert.Equal(0, blHeightInv t)
    Assert.True(blackSonsOfRed t)

[<Fact>]
let largeSetInsertion () =
    let rng = Random()
    let randomValues = [ for _ in 1..1000 -> rng.Next(-10000, 10000) ]

    let treeResult =
        randomValues |> List.fold (fun acc x -> acc |> Result.bind (add x)) (Ok empty)

    match treeResult with
    | Ok tree ->
        Assert.NotEqual(-1, blHeightInv tree)
        Assert.True(blackSonsOfRed tree)

        for x in randomValues do
            Assert.True(contains x tree)
    | Error err -> Assert.True(false, sprintf "Error in insert: %A" err)

[<Fact>]
let deleteRoot () =
    let finalTree =
        empty
        |> add 5
        |> Result.bind (add 3)
        |> Result.bind (add 7)
        |> Result.bind (delete 5)

    match finalTree with
    | Ok t ->
        Assert.False(contains 5 t)
        Assert.True(contains 3 t)
        Assert.True(contains 7 t)
        Assert.NotEqual(-1, blHeightInv t)
    | _ -> Assert.True(false, sprintf "Expect Ok, but get Error")

[<Fact>]
let complexRedBlackViolations () =
    let values = [ 1..20 ]

    let treeResult =
        values |> List.fold (fun acc x -> acc |> Result.bind (add x)) (Ok empty)

    match treeResult with
    | Ok tree ->
        Assert.NotEqual(-1, blHeightInv tree)
        Assert.True(blackSonsOfRed tree)
    | Error err -> Assert.True(false, sprintf "Error in insert: %A" err)
