open System.IO

let x = "Hello", 42 // create a tuple with 2 elements
let (a, b) = x // unpack the two elements of x by pattern matching
printfn "%s, %i" a b
printfn "%s, %i" (fst x) (snd x)

let y = 1,2,3,4
let (c,_,d,e) = y
printfn "%i, %i, %i" c d e

type Observation = {Label: string; Pixels: int[]}
type Distance = int[] * int[] -> int
type Classifier = int[] -> string
type MyClassifier = Observation[] * Distance * int[] -> string

let toObservation (csvData:string) = 
    let columns = csvData.Split(',')
    let label = columns.[0]
    let pixels = columns.[1..] |> Array.map int
    {Label = label; Pixels = pixels}

let reader path = 
    let data = File.ReadAllLines path
    data.[1..] |> Array.map toObservation

let trainingPath =  __SOURCE_DIRECTORY__ + @"..\..\Data\trainingsample.csv"
let trainingData = reader trainingPath

let manhattanDistance (pixels1,pixels2) =
    Array.zip pixels1 pixels2
    |> Array.map (fun (x,y) -> abs (x-y))
    |> Array.sum

let euclideanDistance (pixels1,pixels2) =
    Array.zip pixels1 pixels2
    |> Array.map (fun (x,y) -> pown (x-y) 2)
    |> Array.sum

let train (trainingData:Observation[]) (distance: Distance) =
    let classify (pixels:int[]) =
        trainingData
        |> Array.minBy (fun x -> distance (x.Pixels, pixels))
        |> fun x -> x.Label
    classify

let manhattanClassifier = train trainingData manhattanDistance
let euclideanClassifier = train trainingData euclideanDistance

let myClassfifier (trainingData: Observation[]) (distance: Distance) (pixels: int[]) =
    trainingData
    |> Array.minBy (fun x -> distance (x.Pixels, pixels))
    |> fun x -> x.Label 

let validationPath = __SOURCE_DIRECTORY__ + @"..\..\Data\validationsample.csv"
let validationData =  reader validationPath

let evaluate (validationData: Observation[]) (classifier: Classifier) = 
    validationData
    |> Array.averageBy (fun x -> if classifier x.Pixels = x.Label then 1.0 else 0.0)
    |> printfn "Correct: %.3f"

let myEvaluate (validationData: Observation[]) (trainingData: Observation[]) (distance: Distance) (classifier: MyClassifier) = 
    validationData
    |> Array.averageBy (fun x -> if classifier (trainingData, distance, x.Pixels) = x.Label then 1.0 else 0.0)
    |> printfn "Correct: %.3f"

printfn "Manhattan"
evaluate validationData manhattanClassifier
printfn "Euclidean"
evaluate validationData euclideanClassifier

printfn "Manhattan"
myEvaluate validationData trainingData manhattanDistance
printfn "Euclidean"
myEvaluate validationData trainingData euclideanDistance


