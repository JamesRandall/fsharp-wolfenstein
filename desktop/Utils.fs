module App.Utils

open System.IO
open System.Reflection
open App.Model

let loadAsset assetName = async {
  use stream = Assembly.GetEntryAssembly().GetManifestResourceStream($"FSharpWolfenstein.Desktop.Assets.{assetName}")
  use memoryStream = new MemoryStream()
  do! stream.CopyToAsync memoryStream |> Async.AwaitTask
  return Fable.Core.JS.Constructors.ArrayBuffer.CreateWithBytes (memoryStream.GetBuffer()) |> Ok
}

let loadAssetBytes assetName =
  use stream = Assembly.GetEntryAssembly().GetManifestResourceStream($"FSharpWolfenstein.Desktop.Assets.{assetName}")
  use memoryStream = new MemoryStream()
  stream.CopyTo memoryStream
  let result = Array.zeroCreate<byte> (memoryStream.GetBuffer().Length)
  memoryStream.GetBuffer().CopyTo(result,0)
  result
  
let log (message:string) = System.Console.WriteLine message

let diagnoseCompositeAreas (compositeAreas:CompositeArea list) =
  compositeAreas
  |> List.iter(fun ca ->
    if ca.ConnectedTo |> Set.count > 1 then
      let diag = ca.ConnectedTo |> Set.map(fun i -> $"{i}") |> String.concat ","
      log $"Area: {ca.Area}, Composite: [{diag}]"
  )