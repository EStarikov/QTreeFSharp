open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =
    let benchmarks =
        BenchmarkSwitcher
            [| typeof<QuadTree.Benchmarks.BFS.Benchmark>
               typeof<QuadTree.Benchmarks.SSSP.Benchmark>
               typeof<QuadTree.Benchmarks.Triangles.Benchmark>
               typeof<QuadTree.Benchmarks.RedBlackSet.BatchOpsBenchmark>
               typeof<QuadTree.Benchmarks.RedBlackSet.SetsBenchmark> |]

    benchmarks.Run argv |> ignore
    0
