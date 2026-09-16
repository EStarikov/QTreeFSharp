namespace QuadTree.Benchmarks.RedBlackSet

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open QuadTree.RBSet
open System.Collections.Generic

[<GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)>]
[<CategoriesColumn>]
[<HtmlExporter>]
[<MemoryDiagnoser>]
type SingleOpsBenchmark() =
    let rnd = System.Random(1234561)

    [<Params(100, 10000, 100000)>]
    [<DefaultValue>]
    val mutable public A: int

    [<DefaultValue>]
    val mutable public rndInt: int

    [<DefaultValue>]
    val mutable public setA: RBSet<int>

    [<GlobalSetup>]
    member self.Setup() =
        self.rndInt <- rnd.Next(self.A + 1, self.A + 1000)

        let dataA = Array.init self.A (fun _ -> rnd.Next())

        self.setA <-
            dataA
            |> Array.fold
                (fun (set: RBSet<int>) v ->
                    match RBSet.add v set with
                    | Ok nextSet -> nextSet
                    | Error err -> failwithf "Benchmark setup failed: %A" err)
                RBSet.empty

    [<Benchmark>]
    [<BenchmarkCategory("Adding")>]
    member self.AddingOneElement() = RBSet.add self.rndInt self.setA

    [<Benchmark>]
    [<BenchmarkCategory("Deleting")>]
    member self.DeletingOneElement() = RBSet.delete self.rndInt self.setA


[<GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)>]
[<CategoriesColumn>]
[<HtmlExporter>]
[<MemoryDiagnoser>]
type FSSetsBenchmark() =
    let rnd = System.Random(1234561)

    [<Params(1000, 10000, 100000)>]
    [<DefaultValue>]
    val mutable public A: int

    [<Params(100, 10000, 100000)>]
    [<DefaultValue>]
    val mutable public B: int

    [<DefaultValue>]
    val mutable public RedBlackSetA: RBSet<int>

    [<DefaultValue>]
    val mutable public RedBlackSetB: RBSet<int>

    [<DefaultValue>]
    val mutable public SetA: Set<int>

    [<DefaultValue>]
    val mutable public SetB: Set<int>

    [<DefaultValue>]
    val mutable public HashSetA: HashSet<int>

    [<DefaultValue>]
    val mutable public HashSetB: HashSet<int>

    [<GlobalSetup>]
    member self.Setup() =
        let dataA = Array.init self.A (fun _ -> rnd.Next())

        let dataB = Array.init self.B (fun _ -> rnd.Next())

        self.RedBlackSetA <-
            dataA
            |> Array.fold
                (fun set v ->
                    match RBSet.add v set with
                    | Ok s -> s
                    | Error e -> failwithf "%A" e)
                RBSet.empty

        self.RedBlackSetB <-
            dataB
            |> Array.fold
                (fun set v ->
                    match RBSet.add v set with
                    | Ok s -> s
                    | Error e -> failwithf "%A" e)
                RBSet.empty

        self.SetA <- dataA |> Array.fold (fun set v -> Set.add v set) Set.empty

        self.SetB <- dataB |> Array.fold (fun set v -> Set.add v set) Set.empty

        self.HashSetA <- HashSet<int>()
        dataA |> Array.iter (fun v -> self.HashSetA.Add(v) |> ignore)

        self.HashSetB <- HashSet<int>()
        dataB |> Array.iter (fun v -> self.HashSetB.Add(v) |> ignore)

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Union")>]
    member self.UnionRB() =
        RBSet.union self.RedBlackSetA self.RedBlackSetB

    [<Benchmark>]
    [<BenchmarkCategory("Union")>]
    member self.UnionFS() = Set.union self.SetA self.SetB

    [<Benchmark>]
    [<BenchmarkCategory("Union")>]
    member self.UnionHashFS() =
        let a = HashSet<int>(self.HashSetA)
        a.UnionWith(self.HashSetB)

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Intersection")>]
    member self.IntersectionRB() =
        RBSet.intersection self.RedBlackSetA self.RedBlackSetB

    [<Benchmark>]
    [<BenchmarkCategory("Intersection")>]
    member self.IntersectionFS() = Set.intersect self.SetA self.SetB

    [<Benchmark>]
    [<BenchmarkCategory("Intersection")>]
    member self.IntersectionHashFS() =
        let a = HashSet<int>(self.HashSetA)
        a.IntersectWith(self.HashSetB)

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Difference")>]
    member self.DifferenceRB() =
        RBSet.difference self.RedBlackSetA self.RedBlackSetB

    [<Benchmark>]
    [<BenchmarkCategory("Difference")>]
    member self.DifferenceFS() = Set.difference self.SetA self.SetB

    [<Benchmark>]
    [<BenchmarkCategory("Difference")>]
    member self.DifferenceHashFS() =
        let a = HashSet<int>(self.HashSetA)
        a.ExceptWith(self.HashSetB)
