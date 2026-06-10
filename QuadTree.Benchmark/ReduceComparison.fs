namespace QuadTree.Benchmarks.ReduceComparison

open BenchmarkDotNet.Attributes
open QuadTree.Benchmarks.Utils

[<Config(typeof<MyConfig>)>]
type Benchmark() =

    let mutable sparseSmall = Unchecked.defaultof<Matrix.SparseMatrix<double>>
    let mutable denseSmall = Unchecked.defaultof<Matrix.SparseMatrix<double>>
    let mutable sparseMedium = Unchecked.defaultof<Matrix.SparseMatrix<double>>
    let mutable denseMedium = Unchecked.defaultof<Matrix.SparseMatrix<double>>
    let mutable sparseLarge = Unchecked.defaultof<Matrix.SparseMatrix<double>>
    let mutable denseLarge = Unchecked.defaultof<Matrix.SparseMatrix<double>>

    let add x y =
        match x, y with
        | Some a, Some b -> Some(a + b)
        | Some a, None
        | None, Some a -> Some a
        | _ -> None

    member private this.CreateSparseMatrix size density =
        let rng = System.Random(42)

        let coords =
            [ for i in 0UL .. size - 1UL do
                  for j in 0UL .. size - 1UL do
                      if rng.NextDouble() < density then
                          (i * 1UL<Matrix.rowindex>, j * 1UL<Matrix.colindex>, rng.NextDouble()) ]

        match
            Matrix.fromCoordinateList (
                Matrix.CoordinateList(size * 1UL<Matrix.nrows>, size * 1UL<Matrix.ncols>, coords)
            )
        with
        | Ok m -> m
        | Error msg -> failwith $"Failed to create matrix: {msg}"

    member private this.CreateDenseMatrix size =
        let coords =
            [ for i in 0UL .. size - 1UL do
                  for j in 0UL .. size - 1UL do
                      (i * 1UL<Matrix.rowindex>, j * 1UL<Matrix.colindex>, 1.0) ]

        match
            Matrix.fromCoordinateList (
                Matrix.CoordinateList(size * 1UL<Matrix.nrows>, size * 1UL<Matrix.ncols>, coords)
            )
        with
        | Ok m -> m
        | Error msg -> failwith $"Failed to create matrix: {msg}"

    [<GlobalSetup>]
    member this.Setup() =
        let sizeSmall = 32UL
        let sizeMedium = 256UL
        let sizeLarge = 1024UL

        sparseSmall <- this.CreateSparseMatrix sizeSmall 0.10
        denseSmall <- this.CreateDenseMatrix sizeSmall
        sparseMedium <- this.CreateSparseMatrix sizeMedium 0.05
        denseMedium <- this.CreateDenseMatrix sizeMedium
        sparseLarge <- this.CreateSparseMatrix sizeLarge 0.01
        denseLarge <- this.CreateDenseMatrix sizeLarge

    // ========== Оригинальный reduceCols ==========
    [<Benchmark>]
    member this.ReduceCols_SparseSmall() = Matrix.reduceCols add sparseSmall

    [<Benchmark>]
    member this.ReduceCols_DenseSmall() = Matrix.reduceCols add denseSmall

    [<Benchmark>]
    member this.ReduceCols_SparseMedium() = Matrix.reduceCols add sparseMedium

    [<Benchmark>]
    member this.ReduceCols_DenseMedium() = Matrix.reduceCols add denseMedium

    [<Benchmark>]
    member this.ReduceCols_SparseLarge() = Matrix.reduceCols add sparseLarge

    [<Benchmark>]
    member this.ReduceCols_DenseLarge() = Matrix.reduceCols add denseLarge

    // ========== reduceCols через транспонирование ==========
    [<Benchmark>]
    member this.ReduceColsViaTranspose_SparseSmall() =
        let transposed = Matrix.transpose sparseSmall
        Matrix.reduceRows add transposed

    [<Benchmark>]
    member this.ReduceColsViaTranspose_DenseSmall() =
        let transposed = Matrix.transpose denseSmall
        Matrix.reduceRows add transposed

    [<Benchmark>]
    member this.ReduceColsViaTranspose_SparseMedium() =
        let transposed = Matrix.transpose sparseMedium
        Matrix.reduceRows add transposed

    [<Benchmark>]
    member this.ReduceColsViaTranspose_DenseMedium() =
        let transposed = Matrix.transpose denseMedium
        Matrix.reduceRows add transposed

    [<Benchmark>]
    member this.ReduceColsViaTranspose_SparseLarge() =
        let transposed = Matrix.transpose sparseLarge
        Matrix.reduceRows add transposed

    [<Benchmark>]
    member this.ReduceColsViaTranspose_DenseLarge() =
        let transposed = Matrix.transpose denseLarge
        Matrix.reduceRows add transposed
