namespace QuadTree.Benchmarks.Kronecker

open BenchmarkDotNet.Attributes
open QuadTree.Benchmarks.Utils

[<Config(typeof<MyConfig>)>]
[<MemoryDiagnoser>]
type Benchmark() =

    let mutable sparseSmall = Unchecked.defaultof<Matrix.SparseMatrix<double>>
    let mutable denseSmall = Unchecked.defaultof<Matrix.SparseMatrix<double>>
    let mutable sparseMedium = Unchecked.defaultof<Matrix.SparseMatrix<double>>
    let mutable denseMedium = Unchecked.defaultof<Matrix.SparseMatrix<double>>
    let mutable sparseLarge = Unchecked.defaultof<Matrix.SparseMatrix<double>>
    let mutable denseLarge = Unchecked.defaultof<Matrix.SparseMatrix<double>>

    let rngSeed = 42

    member private this.CreateMatrix size (generateValue: System.Random -> uint64 -> uint64 -> Option<float>) =
        let rng = System.Random(rngSeed)

        let coords =
            [ for i in 0UL .. size - 1UL do
                  for j in 0UL .. size - 1UL do
                      match generateValue rng i j with
                      | Some v -> (i * 1UL<Matrix.rowindex>, j * 1UL<Matrix.colindex>, v)
                      | None -> () ]

        match
            Matrix.fromCoordinateList (
                Matrix.CoordinateList(size * 1UL<Matrix.nrows>, size * 1UL<Matrix.ncols>, coords)
            )
        with
        | Ok m -> m
        | Error msg -> failwith $"Failed to create matrix: {msg}"

    member private this.CreateSparseMatrix size density =
        let generateValue (rng: System.Random) _ _ =
            if rng.NextDouble() < density then
                Some(rng.NextDouble())
            else
                None

        this.CreateMatrix size generateValue

    member private this.CreateDenseMatrix size =
        let generateValue _ _ _ = Some 1.0
        this.CreateMatrix size generateValue

    [<GlobalSetup>]
    member this.Setup() =
        let sizeSmall = 8UL
        let sizeMedium = 16UL
        let sizeLarge = 32UL

        sparseSmall <- this.CreateSparseMatrix sizeSmall 0.10
        denseSmall <- this.CreateDenseMatrix sizeSmall
        sparseMedium <- this.CreateSparseMatrix sizeMedium 0.05
        denseMedium <- this.CreateDenseMatrix sizeMedium
        sparseLarge <- this.CreateSparseMatrix sizeLarge 0.01
        denseLarge <- this.CreateDenseMatrix sizeLarge

    member private this.Mult a b = Some(a * b)

    [<Benchmark>]
    member this.Kronecker_SparseSmall() =
        match Matrix.kroneckerProduct sparseSmall sparseSmall this.Mult with
        | Ok res -> res
        | Error _ -> failwith "Kronecker failed"

    [<Benchmark>]
    member this.Kronecker_DenseSmall() =
        match Matrix.kroneckerProduct denseSmall denseSmall this.Mult with
        | Ok res -> res
        | Error _ -> failwith "Kronecker failed"

    [<Benchmark>]
    member this.Kronecker_SparseMedium() =
        match Matrix.kroneckerProduct sparseMedium sparseMedium this.Mult with
        | Ok res -> res
        | Error _ -> failwith "Kronecker failed"

    [<Benchmark>]
    member this.Kronecker_DenseMedium() =
        match Matrix.kroneckerProduct denseMedium denseMedium this.Mult with
        | Ok res -> res
        | Error _ -> failwith "Kronecker failed"

    [<Benchmark>]
    member this.Kronecker_SparseLarge() =
        match Matrix.kroneckerProduct sparseLarge sparseLarge this.Mult with
        | Ok res -> res
        | Error _ -> failwith "Kronecker failed"

    [<Benchmark>]
    member this.Kronecker_DenseLarge() =
        match Matrix.kroneckerProduct denseLarge denseLarge this.Mult with
        | Ok res -> res
        | Error _ -> failwith "Kronecker failed"
