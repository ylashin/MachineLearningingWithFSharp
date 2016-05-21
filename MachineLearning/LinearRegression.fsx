#load "References.fsx"

open MathNet.Numerics
open MathNet.Numerics.Statistics
open MathNet.Numerics.Data.Text    
open RDotNet
open RProvider
open RProvider.``base``
open FSharp.Charting
open MathNet.Numerics.LinearAlgebra
open MachineLearning.HelperFunctions


// COOEFFICIENT CAN BE USED TO TELL US IF WE CAN US LINEAR REGRESSION FIND MODEL

let x1 = [for x in 1..1000 -> rand 1 100 ]
let y1 = x1 |> List.map (fun a -> 4. * a + 30. + rand -10 10)

List.zip x1 y1 |> FSharp.Charting.Chart.Point

//Note : correlation is almost one regardless of the magnitude of x coefficeint in the equation
let c1 = Correlation.Pearson(Array.ofList x1, Array.ofList y1)


let x2 = [for x in 1..1000 -> rand 1 100 ]
let y2 = [for x in 1..1000 -> rand 1 100 ]

List.zip x2 y2 |> FSharp.Charting.Chart.Point


let c2 = Correlation.Pearson(Array.ofList x2, Array.ofList y2)



// generate 1000 samples of   Blood Pressure = 5 * Age - 4 * Weight + 7

let numRows = 1000
let intercept = 7.0
let theta1 = 5.0
let theta2 = -4.0


let ageList = [for x in 1..numRows -> rand -100 100 ]
let weightList = [for x in 1..numRows -> rand -100 100 ]

let bloodPressureList = List.zip ageList weightList |> List.map 
                                (fun a -> theta1 * fst a + theta2 * snd a + intercept + rand -5 5)


 
RProvider.scatterplot3d.R.scatterplot3d(ageList,weightList,bloodPressureList)

// Guess the cooefficients


let computeCost (X:Matrix<double>) (y:Vector<double>) (theta:Vector<double>) =   
    let prediction = (X * theta)
    let delta = prediction - y
    let sumSquaredError = delta.ToArray() |> Array.map (fun a -> a ** 2.) |> Array.sum
    0.5 * sumSquaredError / (double y.Count)

// Above cost function has two benefits:
//  - Errors do not cancel each other
//  - It is a quatratic function in Theta which will come handy later for minimization

let oneList = [for x in 1..numRows -> double 1 ]
let X = Matrix<double>.Build.DenseOfColumnArrays(Array.ofList oneList,Array.ofList ageList,Array.ofList weightList)
let bloodPressure = Vector<double>.Build.Dense(Array.ofList bloodPressureList)

computeCost X bloodPressure (vector[7.; 5.; -4.;]) 

computeCost X bloodPressure (vector[7.; 6.; -4.;])


//assuming we magically know the intercept

let theta1List = seq {-50. .. 50.}
let theta2List = seq {-50. .. 50.}

let allCombinations =  [for t1 in theta1List do
                        for t2 in theta2List do
                        let theta = vector[intercept; t1; t2;] 
                        yield (t1,t2,computeCost X bloodPressure theta)
                        ]

//Parabola opening upward
graph3D allCombinations


// logic to minimize x2 -- > move with the slope till the minimum in SMALL steps

// logic to the derivation of minimizing the cost function in case y = theta * x



let gradientDescent (X:Matrix<double>) (y:Vector<double>)  = 
    let initialTheta = vector[0.; 0.; 0.;]
    // LEARNING RATE
    let alpha = 0.0005
    let num_iters = 10000    
    let rec loop (X:Matrix<double>) (y:Vector<double>) (theta:Vector<double>) (num_iters:int) = 
        match num_iters with
        | 0 -> theta
        | _ ->          
            let sampleSize = double y.Count        
            let thetaNew = theta - (alpha/sampleSize) * (X.Transpose() * ((X*theta)-y))            
            loop X y thetaNew (num_iters-1)  
                       
    loop X y initialTheta num_iters


// ignoring scaling feature / smarter stop criteria / categorical features
// We have gradient decent algorithm in 3 lines of code (excluding function signature and recursion stopping logic)

let solution = gradientDescent X bloodPressure


printf "%A" solution

let fullMatrix = Matrix<double>.Build.DenseOfColumnArrays(Array.ofList ageList,Array.ofList weightList,Array.ofList bloodPressureList)
                                      
DelimitedWriter.Write("d:\\matrix.csv",fullMatrix,",")  







