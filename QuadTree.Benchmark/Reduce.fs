namespace QuadTree.Benchmarks.Reduce

open BenchmarkDotNet.Attributes
open QuadTree.Benchmarks.Utils

[<Config(typeof<MyConfig>)>]
[<MemoryDiagnoser>]
type Benchmark() =

    let mutable sparseSmall = Unchecked.defaultof<Matrix.SparseMatrix<double>>
    let mutable denseUniformSmall = Unchecked.defaultof<Matrix.SparseMatrix<double>>
    let mutable denseRandomSmall = Unchecked.defaultof<Matrix.SparseMatrix<double>>

    let mutable sparseMedium = Unchecked.defaultof<Matrix.SparseMatrix<double>>
    let mutable denseUniformMedium = Unchecked.defaultof<Matrix.SparseMatrix<double>>
    let mutable denseRandomMedium = Unchecked.defaultof<Matrix.SparseMatrix<double>>

    let mutable sparseLarge = Unchecked.defaultof<Matrix.SparseMatrix<double>>
    let mutable denseUniformLarge = Unchecked.defaultof<Matrix.SparseMatrix<double>>
    let mutable denseRandomLarge = Unchecked.defaultof<Matrix.SparseMatrix<double>>

    let add x y =
        match x, y with
        | Some a, Some b -> Some(a + b)
        | Some a, None
        | None, Some a -> Some a
        | _ -> None

    let private rngSeed = 42

    member private this.CreateMatrix size generateValue =
        let rng = System.Random(rngSeed)
        let coords =
            [ for i in 0UL .. size - 1UL do
                for j in 0UL .. size - 1UL do
                    match generateValue rng i j with
                    | Some v -> (i * 1UL<Matrix.rowindex>, j * 1UL<Matrix.colindex>, v)
                    | None -> () ]
        match Matrix.fromCoordinateList (
            Matrix.CoordinateList(
                size * 1UL<Matrix.nrows>,
                size * 1UL<Matrix.ncols>,
                coords
            )
        ) with
        | Ok m -> m
        | Error msg -> failwith $"Failed to create matrix: {msg}"

    member private this.CreateSparseMatrix size density =
        let generateValue rng i j =
            if rng.NextDouble() < density then Some(rng.NextDouble()) else None
        this.CreateMatrix size generateValue

    member private this.CreateDenseUniformMatrix size =
        let generateValue _ _ _ = Some 1.0
        this.CreateMatrix size generateValue

    member private this.CreateDenseRandomMatrix size =
        let generateValue rng _ _ = Some(rng.NextDouble())
        this.CreateMatrix size generateValue

    [<GlobalSetup>]
    member this.Setup() =
        let sizeSmall = 32UL
        let sizeMedium = 256UL
        let sizeLarge = 1024UL

        sparseSmall <- this.CreateSparseMatrix sizeSmall 0.10
        denseUniformSmall <- this.CreateDenseUniformMatrix sizeSmall
        denseRandomSmall <- this.CreateDenseRandomMatrix sizeSmall

        sparseMedium <- this.CreateSparseMatrix sizeMedium 0.05
        denseUniformMedium <- this.CreateDenseUniformMatrix sizeMedium
        denseRandomMedium <- this.CreateDenseRandomMatrix sizeMedium

        sparseLarge <- this.CreateSparseMatrix sizeLarge 0.01
        denseUniformLarge <- this.CreateDenseUniformMatrix sizeLarge
        denseRandomLarge <- this.CreateDenseRandomMatrix sizeLarge

    [<Benchmark>]
    member this.ReduceRows_SparseSmall() = Matrix.reduceRows add sparseSmall

    [<Benchmark>]
    member this.ReduceCols_SparseSmall() = Matrix.reduceCols add sparseSmall

    [<Benchmark>]
    member this.ReduceRows_SparseMedium() = Matrix.reduceRows add sparseMedium

    [<Benchmark>]
    member this.ReduceCols_SparseMedium() = Matrix.reduceCols add sparseMedium

    [<Benchmark>]
    member this.ReduceRows_SparseLarge() = Matrix.reduceRows add sparseLarge

    [<Benchmark>]
    member this.ReduceCols_SparseLarge() = Matrix.reduceCols add sparseLarge


    [<Benchmark>]
    member this.ReduceRows_DenseUniformSmall() = Matrix.reduceRows add denseUniformSmall

    [<Benchmark>]
    member this.ReduceCols_DenseUniformSmall() = Matrix.reduceCols add denseUniformSmall

    [<Benchmark>]
    member this.ReduceRows_DenseUniformMedium() =
        Matrix.reduceRows add denseUniformMedium

    [<Benchmark>]
    member this.ReduceCols_DenseUniformMedium() =
        Matrix.reduceCols add denseUniformMedium

    [<Benchmark>]
    member this.ReduceRows_DenseUniformLarge() = Matrix.reduceRows add denseUniformLarge

    [<Benchmark>]
    member this.ReduceCols_DenseUniformLarge() = Matrix.reduceCols add denseUniformLarge


    [<Benchmark>]
    member this.ReduceRows_DenseRandomSmall() = Matrix.reduceRows add denseRandomSmall

    [<Benchmark>]
    member this.ReduceCols_DenseRandomSmall() = Matrix.reduceCols add denseRandomSmall

    [<Benchmark>]
    member this.ReduceRows_DenseRandomMedium() = Matrix.reduceRows add denseRandomMedium

    [<Benchmark>]
    member this.ReduceCols_DenseRandomMedium() = Matrix.reduceCols add denseRandomMedium

    [<Benchmark>]
    member this.ReduceRows_DenseRandomLarge() = Matrix.reduceRows add denseRandomLarge

    [<Benchmark>]
    member this.ReduceCols_DenseRandomLarge() = Matrix.reduceCols add denseRandomLarge
