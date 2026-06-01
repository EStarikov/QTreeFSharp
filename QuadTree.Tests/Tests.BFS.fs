module Graph.BFS.Tests

open System
open Xunit

open Matrix
open Vector
open Common

let singleStart (n: uint64) (s: uint64) =
    Vector.CoordinateList(
        n * 1UL<Vector.dataLength>,
        [s * 1UL<Vector.index>, 1UL]
    )
    |> Vector.fromCoordinateList

let vec (n: uint64) pairs =
    Vector.CoordinateList(
        n * 1UL<Vector.dataLength>,
        pairs |> List.map (fun (i: uint64, v: uint64) -> i * 1UL<Vector.index>, v)
    )
    |> Vector.fromCoordinateList

let unsafes (n: uint64) (v: Vector.SparseVector<_>) =
    List.init (int n) (fun i -> Vector.unsafeGet v (uint64 i * 1UL<Vector.index>))

// ============== Existing test (unchanged) ==============

[<Fact>]
let ``Simple level bfs.`` () =
    let graph =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(1))),
                    Matrix.qtree.Leaf(UserValue(Some(3))),
                    Matrix.qtree.Leaf(UserValue(None))
                ),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(Some(1))),
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(2))),
                    Matrix.qtree.Leaf(UserValue(Some(3)))
                ),
                Matrix.qtree.Leaf(UserValue(None)),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(2))),
                    Matrix.qtree.Leaf(UserValue(Some(3))),
                    Matrix.qtree.Leaf(UserValue(None))
                )
            )

        let store = Matrix.Storage(4UL<storageSize>, tree)
        SparseMatrix(4UL<nrows>, 4UL<ncols>, 9UL<nvals>, store)

    let startVertices =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(1UL))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Leaf(UserValue(None))
            )

        let store = Vector.Storage(4UL<storageSize>, tree)
        SparseVector(4UL<dataLength>, 1UL<nvals>, store)

    let expected =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(0UL))), Vector.btree.Leaf(UserValue(Some(1UL)))),
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(1UL))), Vector.btree.Leaf(UserValue(Some(2UL))))
            )

        let store = Vector.Storage(4UL<storageSize>, tree)
        Ok(SparseVector(4UL<dataLength>, 4UL<nvals>, store))

    let actual = Graph.BFS.bfs_level graph startVertices

    Assert.Equal(expected, actual)

// ============== Parent BFS on same graph ==============

[<Fact>]
let ``Simple parent bfs.`` () =
    let graph =
        let tree =
            Matrix.qtree.Node(
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(1))),
                    Matrix.qtree.Leaf(UserValue(Some(3))),
                    Matrix.qtree.Leaf(UserValue(None))
                ),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(Some(1))),
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(2))),
                    Matrix.qtree.Leaf(UserValue(Some(3)))
                ),
                Matrix.qtree.Leaf(UserValue(None)),
                Matrix.qtree.Node(
                    Matrix.qtree.Leaf(UserValue(None)),
                    Matrix.qtree.Leaf(UserValue(Some(2))),
                    Matrix.qtree.Leaf(UserValue(Some(3))),
                    Matrix.qtree.Leaf(UserValue(None))
                )
            )

        let store = Matrix.Storage(4UL<storageSize>, tree)
        SparseMatrix(4UL<nrows>, 4UL<ncols>, 9UL<nvals>, store)

    let startVertices =
        let tree =
            Vector.btree.Node(
                Vector.btree.Node(Vector.btree.Leaf(UserValue(Some(1UL))), Vector.btree.Leaf(UserValue(None))),
                Vector.btree.Leaf(UserValue(None))
            )

        let store = Vector.Storage(4UL<storageSize>, tree)
        SparseVector(4UL<dataLength>, 1UL<nvals>, store)

    let expected =
        let tree =
            Vector.btree.Node(
                Vector.btree.Leaf(UserValue(Some(0UL))),
                Vector.btree.Node(
                    Vector.btree.Leaf(UserValue(Some(0UL))),
                    Vector.btree.Leaf(UserValue(Some(1UL)))
                )
            )

        let store = Vector.Storage(4UL<storageSize>, tree)
        Ok(SparseVector(4UL<dataLength>, 4UL<nvals>, store))

    let actual = Graph.BFS.bfs_parent graph startVertices

    Assert.Equal(expected, actual)

// ============== 3-node line ==============

let private line3graph =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            3UL<nrows>,
            3UL<ncols>,
            [0UL<rowindex>, 1UL<colindex>, 1UL
             1UL<rowindex>, 0UL<colindex>, 1UL
             1UL<rowindex>, 2UL<colindex>, 1UL
             2UL<rowindex>, 1UL<colindex>, 1UL]
        )
    )

[<Fact>]
let ``Level bfs 3 node line start 0`` () =
    let n = uint64 line3graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level line3graph start
    let expected = [ Some 0UL; Some 1UL; Some 2UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs 3 node line start 0`` () =
    let n = uint64 line3graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent line3graph start
    let expected = [ Some 0UL; Some 0UL; Some 1UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

[<Fact>]
let ``Level bfs 3 node line start 1`` () =
    let n = uint64 line3graph.ncols
    let start = singleStart n 1UL
    let result = Graph.BFS.bfs_level line3graph start
    let expected = [ Some 1UL; Some 0UL; Some 1UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs 3 node line start 1`` () =
    let n = uint64 line3graph.ncols
    let start = singleStart n 1UL
    let result = Graph.BFS.bfs_parent line3graph start
    let expected = [ Some 1UL; Some 1UL; Some 1UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

// ============== 5-node star (center 0) ==============

let private star5graph =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            5UL<nrows>,
            5UL<ncols>,
            [0UL<rowindex>, 1UL<colindex>, 1UL
             1UL<rowindex>, 0UL<colindex>, 1UL
             0UL<rowindex>, 2UL<colindex>, 1UL
             2UL<rowindex>, 0UL<colindex>, 1UL
             0UL<rowindex>, 3UL<colindex>, 1UL
             3UL<rowindex>, 0UL<colindex>, 1UL
             0UL<rowindex>, 4UL<colindex>, 1UL
             4UL<rowindex>, 0UL<colindex>, 1UL]
        )
    )

[<Fact>]
let ``Level bfs 5 node star start center`` () =
    let n = uint64 star5graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level star5graph start
    let expected = [ Some 0UL; Some 1UL; Some 1UL; Some 1UL; Some 1UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs 5 node star start center`` () =
    let n = uint64 star5graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent star5graph start
    let expected = [ Some 0UL; Some 0UL; Some 0UL; Some 0UL; Some 0UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

[<Fact>]
let ``Level bfs 5 node star start leaf`` () =
    let n = uint64 star5graph.ncols
    let start = singleStart n 1UL
    let result = Graph.BFS.bfs_level star5graph start
    let expected = [ Some 1UL; Some 0UL; Some 2UL; Some 2UL; Some 2UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs 5 node star start leaf`` () =
    let n = uint64 star5graph.ncols
    let start = singleStart n 1UL
    let result = Graph.BFS.bfs_parent star5graph start
    let expected = [ Some 1UL; Some 1UL; Some 0UL; Some 0UL; Some 0UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

// ============== Two components ==============

let private twoCompGraph =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            6UL<nrows>,
            6UL<ncols>,
            [0UL<rowindex>, 1UL<colindex>, 1UL
             1UL<rowindex>, 0UL<colindex>, 1UL
             1UL<rowindex>, 2UL<colindex>, 1UL
             2UL<rowindex>, 1UL<colindex>, 1UL
             0UL<rowindex>, 2UL<colindex>, 1UL
             2UL<rowindex>, 0UL<colindex>, 1UL

             3UL<rowindex>, 4UL<colindex>, 1UL
             4UL<rowindex>, 3UL<colindex>, 1UL
             4UL<rowindex>, 5UL<colindex>, 1UL
             5UL<rowindex>, 4UL<colindex>, 1UL
             3UL<rowindex>, 5UL<colindex>, 1UL
             5UL<rowindex>, 3UL<colindex>, 1UL]
        )
    )

[<Fact>]
let ``Level bfs two components start 0`` () =
    let n = uint64 twoCompGraph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level twoCompGraph start
    let expected =
        [ Some 0UL; Some 1UL; Some 1UL
          None;    None;    None ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs two components start 0`` () =
    let n = uint64 twoCompGraph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent twoCompGraph start
    let expected =
        [ Some 0UL; Some 0UL; Some 0UL
          None;    None;    None ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

// ============== Square (4-cycle) ==============

let private squareGraph =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            4UL<nrows>,
            4UL<ncols>,
            [0UL<rowindex>, 1UL<colindex>, 1UL
             1UL<rowindex>, 0UL<colindex>, 1UL
             1UL<rowindex>, 2UL<colindex>, 2UL
             2UL<rowindex>, 1UL<colindex>, 2UL
             2UL<rowindex>, 3UL<colindex>, 1UL
             3UL<rowindex>, 2UL<colindex>, 1UL
             3UL<rowindex>, 0UL<colindex>, 2UL
             0UL<rowindex>, 3UL<colindex>, 2UL]
        )
    )

[<Fact>]
let ``Level bfs square start 0`` () =
    let n = uint64 squareGraph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level squareGraph start
    let expected = [ Some 0UL; Some 1UL; Some 2UL; Some 1UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs square start 0`` () =
    let n = uint64 squareGraph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent squareGraph start
    let expected = [ Some 0UL; Some 0UL; Some 1UL; Some 0UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

// ============== 6-cycle ==============

let private cycle6graph =
    Matrix.fromCoordinateList (
        Matrix.CoordinateList(
            6UL<nrows>,
            6UL<ncols>,
            [0UL<rowindex>, 1UL<colindex>, 1UL
             1UL<rowindex>, 0UL<colindex>, 1UL
             1UL<rowindex>, 2UL<colindex>, 2UL
             2UL<rowindex>, 1UL<colindex>, 2UL
             2UL<rowindex>, 3UL<colindex>, 3UL
             3UL<rowindex>, 2UL<colindex>, 3UL
             3UL<rowindex>, 4UL<colindex>, 4UL
             4UL<rowindex>, 3UL<colindex>, 4UL
             4UL<rowindex>, 5UL<colindex>, 5UL
             5UL<rowindex>, 4UL<colindex>, 5UL
             5UL<rowindex>, 0UL<colindex>, 6UL
             0UL<rowindex>, 5UL<colindex>, 6UL]
        )
    )

[<Fact>]
let ``Level bfs 6 cycle start 0`` () =
    let n = uint64 cycle6graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_level cycle6graph start
    let expected = [ Some 0UL; Some 1UL; Some 2UL; Some 3UL; Some 2UL; Some 1UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)

[<Fact>]
let ``Parent bfs 6 cycle start 0`` () =
    let n = uint64 cycle6graph.ncols
    let start = singleStart n 0UL
    let result = Graph.BFS.bfs_parent cycle6graph start
    let expected = [ Some 0UL; Some 0UL; Some 1UL; Some 2UL; Some 5UL; Some 0UL ]
    Assert.Equal(Ok expected, Result.map (unsafes n) result)


