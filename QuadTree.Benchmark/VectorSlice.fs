namespace QuadTree.Benchmarks.VectorSlice

open BenchmarkDotNet.Attributes
open QuadTree.Benchmarks.Utils

[<Config(typeof<MyConfig>)>]
type Benchmark() =

    let mutable sparseSmall = Unchecked.defaultof<Vector.SparseVector<double>>
    let mutable denseSmall = Unchecked.defaultof<Vector.SparseVector<double>>
    let mutable sparseMedium = Unchecked.defaultof<Vector.SparseVector<double>>
    let mutable denseMedium = Unchecked.defaultof<Vector.SparseVector<double>>
    let mutable sparseLarge = Unchecked.defaultof<Vector.SparseVector<double>>
    let mutable denseLarge = Unchecked.defaultof<Vector.SparseVector<double>>

    member private this.CreateSparseVector size density =
        let rng = System.Random(42)

        let coords =
            [ for i in 0UL .. size - 1UL do
                  if rng.NextDouble() < density then
                      (i * 1UL<Vector.index>, rng.NextDouble()) ]

        match Vector.fromCoordinateList (Vector.CoordinateList(size * 1UL<Vector.dataLength>, coords)) with
        | Ok v -> v
        | Error msg -> failwith $"Failed to create vector: {msg}"

    member private this.CreateDenseVector size =
        let coords =
            [ for i in 0UL .. size - 1UL do
                  (i * 1UL<Vector.index>, 1.0) ]

        match Vector.fromCoordinateList (Vector.CoordinateList(size * 1UL<Vector.dataLength>, coords)) with
        | Ok v -> v
        | Error msg -> failwith $"Failed to create vector: {msg}"

    [<GlobalSetup>]
    member this.Setup() =
        let sizeSmall = 32UL
        let sizeMedium = 256UL
        let sizeLarge = 1024UL

        sparseSmall <- this.CreateSparseVector sizeSmall 0.10
        denseSmall <- this.CreateDenseVector sizeSmall
        sparseMedium <- this.CreateSparseVector sizeMedium 0.05
        denseMedium <- this.CreateDenseVector sizeMedium
        sparseLarge <- this.CreateSparseVector sizeLarge 0.01
        denseLarge <- this.CreateDenseVector sizeLarge

    member private this.SliceMiddle(v: Vector.SparseVector<double>) =
        let n = int v.length
        let start = n / 4
        let last = 3 * n / 4 - 1

        match Vector.slice start last v with
        | Ok res -> res
        | Error _ -> failwith "Slice failed"

    [<Benchmark>]
    member this.Slice_SparseSmall() = this.SliceMiddle sparseSmall

    [<Benchmark>]
    member this.Slice_DenseSmall() = this.SliceMiddle denseSmall

    [<Benchmark>]
    member this.Slice_SparseMedium() = this.SliceMiddle sparseMedium

    [<Benchmark>]
    member this.Slice_DenseMedium() = this.SliceMiddle denseMedium

    [<Benchmark>]
    member this.Slice_SparseLarge() = this.SliceMiddle sparseLarge

    [<Benchmark>]
    member this.Slice_DenseLarge() = this.SliceMiddle denseLarge
