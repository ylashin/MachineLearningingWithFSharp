namespace MachineLearning

#I @"../packages/FSharp.Charting.0.90.13/lib/net40"
#load "../packages/FSharp.Charting.0.90.13/FSharp.Charting.fsx"
#load "../packages/FsLab.0.3.19/FsLab.fsx"
#r "../packages/MathNet.Numerics.Data.Text.3.2.0/lib/net40/MathNet.Numerics.Data.Text.dll"
#I "../packages/RProvider.1.1.20/lib/net40"
#load @"../packages/RProvider.1.1.20/RProvider.fsx"
#r @"../packages/R.NET.Community.1.6.5/lib/net40/RDotNet.dll"
#r @"../packages/R.NET.Community.FSharp.1.6.5/lib/net40/RDotNet.FSharp.dll"
#r @"../packages/RProvider.1.1.20/lib/net40/RProvider.dll"

open MathNet.Numerics
open MathNet.Numerics.Statistics
open MathNet.Numerics.Data.Text    
open XPlot.GoogleCharts
open RDotNet
open RProvider
open RProvider.``base``
open FSharp.Charting
open MathNet.Numerics.LinearAlgebra


[<AutoOpen>]
module HelperFunctions = 
    let random = new System.Random()

    let rand min max =     
        float (random.Next(min,max))

    let graph3D (allValues:(float*float*float) List) = 
        let x = allValues |> List.map (fun (a,b,c) -> a)
        let y = allValues |> List.map (fun (a,b,c) -> b)
        let z = allValues |> List.map (fun (a,b,c) -> c)

        let len = int ((float x.Length) ** 0.5)
        let mx =  R.matrix(x,len,len,true)
        let my =  R.matrix(y,len,len,true)
        let mz =  R.matrix(z,len,len,true)

        RProvider.plot3D.R.surf3D(mx,my,mz,theta=30) |> ignore

