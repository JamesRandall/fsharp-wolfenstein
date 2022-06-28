module App.Utils
open App.Model
open Fable.SimpleHttp
open Browser

let loadAsset assetName = async {
  let! response =
    Http.request $"assets/{assetName}"
    |> Http.method GET
    |> Http.overrideResponseType ResponseTypes.ArrayBuffer
    |> Http.send
  return
    match response.content with
    | ResponseContent.ArrayBuffer arrayBuffer ->
      Ok arrayBuffer
    | _ ->
      console.error "Unable to load asset"
      Error ""
}

let log (message:string) = console.log message

let diagnoseCompositeAreas (compositeAreas:CompositeArea list) =
  compositeAreas
  |> List.iter(fun ca ->
    if ca.ConnectedTo |> Set.count > 1 then
      let diag = ca.ConnectedTo |> Set.map(fun i -> $"{i}") |> String.concat ","
      log $"Area: {ca.Area}, Composite: [{diag}]"
  )